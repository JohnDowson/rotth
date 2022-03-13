use dashmap::DashMap;
use ropey::Rope;
use rotth::ast::{parse_no_include, TopLevel};
use rotth::lexer::lex_string;
use rotth_lsp::semantic_token::{semantic_token_from_ast, CompleteSemanticToken, LEGEND_TYPE};
use somok::Somok;
use std::fs::File;
use std::path::{Path, PathBuf};
use tokio::io::AsyncReadExt;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug, Clone)]
struct TextDocument {
    uri: Url,
    text: String,
    version: i32,
}

#[derive(Debug)]
struct Backend {
    client: Client,
    ast_map: DashMap<PathBuf, Vec<TopLevel>>,
    semantic_token_map: DashMap<PathBuf, Vec<CompleteSemanticToken>>,
    document_map: DashMap<PathBuf, Rope>,
}

impl Backend {
    async fn parse_file(
        &self,
        parent: Option<&Path>,
        path: &Path,
    ) -> tokio::io::Result<Vec<(PathBuf, PathBuf)>> {
        let path = if let Some(parent) = parent {
            parent.parent().unwrap().join(path).canonicalize()
        } else {
            path.canonicalize()
        }
        .unwrap();
        self.client
            .log_message(MessageType::INFO, format!("Parsing file {:?}", &path))
            .await;
        let text = {
            let mut src = String::new();
            let mut file = tokio::fs::File::open(&path).await?;
            file.read_to_string(&mut src).await?;
            src
        };
        let ast = self
            .parse_text(TextDocument {
                uri: Url::from_file_path(dbg! {&path}).unwrap(),
                text,
                version: 0,
            })
            .await
            .unwrap_or_default();
        let includes: Vec<(PathBuf, PathBuf)> = ast
            .iter()
            .filter_map(|i| {
                if let TopLevel::Include(inc) = &i {
                    let p = inc.path().to_owned();
                    (i.span().file, p).some()
                } else {
                    None
                }
            })
            .collect();
        self.ast_map.insert(path, ast);

        includes.okay()
    }

    async fn parse_text(&self, params: TextDocument) -> Option<Vec<TopLevel>> {
        let rope = ropey::Rope::from_str(&params.text);
        if self
            .document_map
            .insert(params.uri.to_file_path().unwrap(), rope.clone())
            .is_some()
        {
            return None;
        }
        let tokens = match lex_string(params.text, params.uri.to_file_path().unwrap()) {
            Ok(tokens) => tokens,
            Err(e) => {
                self.client
                    .log_message(MessageType::INFO, format!("{:?}", e))
                    .await;
                return None;
            } // TODO!
        };
        let ast = match parse_no_include(tokens) {
            Ok(ast) => ast,
            Err(e) => {
                let diagnostics = match e {
                    rotth::Error::Parser(errors) => errors
                        .into_iter()
                        .filter_map(|item| {
                            let (message, span) = match item.reason() {
                                chumsky::error::SimpleReason::Unclosed { span, delimiter } => {
                                    (format!("Unclosed delimiter {}", delimiter), span.clone())
                                }
                                chumsky::error::SimpleReason::Unexpected => (
                                    format!(
                                        "{}, expected {}",
                                        if item.found().is_some() {
                                            "Unexpected token in input"
                                        } else {
                                            "Unexpected end of input"
                                        },
                                        if item.expected().len() == 0 {
                                            "something else".to_string()
                                        } else {
                                            item.expected()
                                                .map(|expected| match expected {
                                                    Some(expected) => expected.to_string(),
                                                    None => "end of input".to_string(),
                                                })
                                                .collect::<Vec<_>>()
                                                .join(", ")
                                        }
                                    ),
                                    item.span(),
                                ),
                                chumsky::error::SimpleReason::Custom(msg) => {
                                    (msg.to_string(), item.span())
                                }
                            };

                            || -> Option<Diagnostic> {
                                let start_position = offset_to_position(span.start, &rope)?;
                                let end_position = offset_to_position(span.end, &rope)?;
                                Some(Diagnostic::new_simple(
                                    Range::new(start_position, end_position),
                                    message,
                                ))
                            }()
                        })
                        .collect::<Vec<_>>(),
                    rotth::Error::Redefinition(errors) => errors
                        .into_iter()
                        .filter_map(|item| {
                            let (message, span) = (
                                "This word redifines another word defined elsewhere".to_string(),
                                item.redefining_item,
                            );

                            || -> Option<Diagnostic> {
                                let start_position = offset_to_position(span.start, &rope)?;
                                let end_position = offset_to_position(span.end, &rope)?;
                                Some(Diagnostic::new_simple(
                                    Range::new(start_position, end_position),
                                    message,
                                ))
                            }()
                        })
                        .collect::<Vec<_>>(),
                    todo => {
                        self.client
                            .log_message(MessageType::INFO, format!("{:?}", todo))
                            .await;
                        return None;
                    }
                };
                self.client
                    .publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
                    .await;
                return None;
            } // TODO!
        };

        ast.some()
    }

    async fn on_change(&self, params: TextDocument) {
        let ast = self.parse_text(params.clone()).await.unwrap_or_default();

        self.semantic_token_map.insert(
            params.uri.to_file_path().unwrap(),
            semantic_token_from_ast(&ast),
        );

        let mut includes = ast
            .iter()
            .filter_map(|i| {
                if let TopLevel::Include(include) = i {
                    (i.span().file, include.path().to_owned()).some()
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        while !includes.is_empty() {
            let (parent, path) = includes.pop().unwrap();
            let extend = self.parse_file(Some(&parent), &path).await.unwrap();
            includes.extend(extend)
        }

        self.ast_map.insert(params.uri.to_file_path().unwrap(), ast);

        self.client
            .log_message(
                MessageType::INFO,
                format!("Parsed AST for {:?}", params.uri.to_string()),
            )
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "rotth-lsp".into(),
                version: Some("0.1.0".into()),
            }),
            capabilities: ServerCapabilities {
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: None,
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![DocumentFilter {
                                        language: Some("rotth".to_string()),
                                        scheme: Some("file".to_string()),
                                        pattern: None,
                                    }]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: LEGEND_TYPE.into(),
                                    token_modifiers: vec![],
                                },
                                range: None,
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                ..Default::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Rotth-LSP initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        self.client
            .log_message(MessageType::INFO, "Rotth-LSP shutting down")
            .await;
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(
                MessageType::INFO,
                format!("file opened! {:?}", params.text_document.uri.to_file_path()),
            )
            .await;
        self.on_change(TextDocument {
            uri: params.text_document.uri,
            text: params.text_document.text,
            version: params.text_document.version,
        })
        .await
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.on_change(TextDocument {
            uri: params.text_document.uri,
            text: std::mem::take(&mut params.content_changes[0].text),
            version: params.text_document.version,
        })
        .await
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let rope = if let Some(r) = self.document_map.get(&uri.to_file_path().unwrap()) {
            r
        } else {
            return Ok(None);
        };

        let position = params.text_document_position_params.position;
        let line = if let Some(l) = rope.get_line(position.line as usize) {
            l
        } else {
            return Ok(None);
        };

        let mut last_space = 0;
        let mut word_start = None;
        let mut word_end = None;
        for (i, c) in line.chars().enumerate() {
            if c.is_whitespace() {
                if word_start.is_none() {
                    last_space = i
                } else {
                    word_end = Some(i);
                    break;
                }
            } else if i == position.character as usize {
                word_start = Some(last_space + 1)
            }
        }
        let word = if let (Some(start), Some(end)) = (word_start, word_end) {
            if let Some(line) = line.as_str() {
                &line[start..end]
            } else {
                return Ok(None);
            }
        } else {
            return Ok(None);
        };

        let item = self.ast_map.iter().find_map(|r| {
            for item in r.value() {
                if item.name()? == word {
                    return item.clone().some();
                }
            }
            None
        });
        let names = dbg! {
            self
                .ast_map
                .get(&PathBuf::from("/home/johnd/pets/rotth/rotth-src/std.rh"))
                .unwrap()
                .iter().filter_map(|i| i.name()).collect::<Vec<_>>()
        };
        dbg! {names.iter().find(|n| n == &"putu")};

        let definition = item.and_then(|item| {
            let span = &item.span();
            let uri = Url::from_file_path(&span.file).unwrap();

            let rope = &*self
                .document_map
                .entry(span.file.clone())
                .or_insert(Rope::from_reader(File::open(&span.file).unwrap()).unwrap());
            let start_position = offset_to_position(span.start, rope)?;
            let end_position = offset_to_position(span.end, rope)?;

            let range = Range::new(start_position, end_position);

            Some(GotoDefinitionResponse::Scalar(Location::new(uri, range)))
        });
        Ok(definition)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.to_file_path().unwrap();
        self.client
            .log_message(MessageType::LOG, "semantic_token_full")
            .await;
        let semantic_tokens = || -> Option<Vec<SemanticToken>> {
            let mut im_complete_tokens = self.semantic_token_map.get_mut(&uri)?;
            let rope = self.document_map.get(&uri)?;

            im_complete_tokens.sort_by(|a, b| a.start.cmp(&b.start));
            let mut pre_line = 0;
            let mut pre_start = 0;
            let semantic_tokens = im_complete_tokens
                .iter()
                .filter_map(|token| {
                    let line = rope.try_byte_to_line(token.start as usize).ok()? as u32;
                    let first = rope.try_line_to_char(line as usize).ok()? as u32;
                    let start = rope.try_byte_to_char(token.start as usize).ok()? as u32 - first;
                    let delta_line = line - pre_line;
                    let delta_start = if delta_line == 0 {
                        start - pre_start
                    } else {
                        start
                    };
                    let ret = Some(SemanticToken {
                        delta_line,
                        delta_start,
                        length: token.length as u32,
                        token_type: token.token_type as u32,
                        token_modifiers_bitset: 0,
                    });
                    pre_line = line;
                    pre_start = start;
                    ret
                })
                .collect::<Vec<_>>();
            Some(semantic_tokens)
        }();
        if let Some(semantic_tokens) = semantic_tokens {
            return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: semantic_tokens,
            })));
        }
        Ok(None)
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        document_map: Default::default(),
        semantic_token_map: Default::default(),
        ast_map: Default::default(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}

fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
    let line = rope.try_char_to_line(offset).ok()?;
    let first_char = rope.try_line_to_char(line).ok()?;
    let column = offset - first_char;
    Some(Position::new(line as u32, column as u32))
}
