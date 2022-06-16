use rotth_parser::ast::*;
use somok::Either;
use spanner::Spanned;
use tower_lsp::lsp_types::SemanticTokenType;

pub const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::FUNCTION,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::STRING,
    SemanticTokenType::COMMENT,
    SemanticTokenType::NUMBER,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::TYPE,
    SemanticTokenType::REGEXP,
];

#[derive(Debug)]
pub struct CompleteSemanticToken {
    pub start: usize,
    pub length: usize,
    pub token_type: usize,
}

pub fn semantic_token_from_ast(ast: &[Spanned<TopLevel>]) -> Vec<CompleteSemanticToken> {
    let mut semantic_tokens = vec![];

    for item in ast.iter() {
        match &**item {
            TopLevel::Include(i) => {
                push_token(&i.include, &mut semantic_tokens, SemanticTokenType::KEYWORD);
                if let Some(Qualifiers { items, from }) = &i.qualifiers {
                    for item in items {
                        push_token(item, &mut semantic_tokens, SemanticTokenType::FUNCTION);
                    }
                    push_token(from, &mut semantic_tokens, SemanticTokenType::KEYWORD);
                }
                push_token(&i.path, &mut semantic_tokens, SemanticTokenType::STRING);
            }
            TopLevel::Proc(p) => {
                push_token(&p.proc, &mut semantic_tokens, SemanticTokenType::KEYWORD);
                if let Some(Spanned {
                    span: _,
                    inner:
                        Generics {
                            left_bracket,
                            tys,
                            right_bracket,
                        },
                }) = &p.generics
                {
                    push_token(
                        left_bracket,
                        &mut semantic_tokens,
                        SemanticTokenType::KEYWORD,
                    );
                    for ty in tys {
                        push_token(ty, &mut semantic_tokens, SemanticTokenType::TYPE);
                    }
                    push_token(
                        right_bracket,
                        &mut semantic_tokens,
                        SemanticTokenType::KEYWORD,
                    );
                };
                push_token(&p.name, &mut semantic_tokens, SemanticTokenType::FUNCTION);
                let signature = &p.signature;
                for ty in &signature.ins {
                    push_token(ty, &mut semantic_tokens, SemanticTokenType::TYPE)
                }
                if let Some(sep) = &signature.sep {
                    push_token(sep, &mut semantic_tokens, SemanticTokenType::KEYWORD);
                }
                if let Some(outs) = &signature.outs {
                    for ty in outs {
                        push_token(ty, &mut semantic_tokens, SemanticTokenType::TYPE)
                    }
                }
                push_token(&p.do_, &mut semantic_tokens, SemanticTokenType::KEYWORD);
                for e in &p.body {
                    push_expr_tokens(e, &mut semantic_tokens);
                }
                push_token(&p.end, &mut semantic_tokens, SemanticTokenType::KEYWORD);
            }
            TopLevel::Const(c) => {
                push_token(&c.const_, &mut semantic_tokens, SemanticTokenType::KEYWORD);
                push_token(&c.name, &mut semantic_tokens, SemanticTokenType::TYPE);
                push_token(&c.do_, &mut semantic_tokens, SemanticTokenType::KEYWORD);
                for e in &c.body {
                    push_expr_tokens(e, &mut semantic_tokens);
                }
                push_token(&c.end, &mut semantic_tokens, SemanticTokenType::KEYWORD);
            }
            TopLevel::Mem(m) => {
                push_token(&m.mem, &mut semantic_tokens, SemanticTokenType::KEYWORD);
                push_token(&m.name, &mut semantic_tokens, SemanticTokenType::TYPE);
                push_token(&m.do_, &mut semantic_tokens, SemanticTokenType::KEYWORD);
                for e in &m.body {
                    push_expr_tokens(e, &mut semantic_tokens);
                }
                push_token(&m.end, &mut semantic_tokens, SemanticTokenType::KEYWORD);
            }
            TopLevel::Var(v) => {
                push_token(&v.var, &mut semantic_tokens, SemanticTokenType::KEYWORD);
                push_token(&v.name, &mut semantic_tokens, SemanticTokenType::TYPE);
                push_token(&v.sep, &mut semantic_tokens, SemanticTokenType::KEYWORD);
                push_token(&v.ty, &mut semantic_tokens, SemanticTokenType::TYPE);
            }
            TopLevel::Struct(s) => {
                push_token(&s.struct_, &mut semantic_tokens, SemanticTokenType::KEYWORD);
                push_token(&s.name, &mut semantic_tokens, SemanticTokenType::TYPE);
                push_token(&s.do_, &mut semantic_tokens, SemanticTokenType::KEYWORD);
                for f in &s.body {
                    push_token(&f.name, &mut semantic_tokens, SemanticTokenType::PARAMETER);
                    push_token(&f.name, &mut semantic_tokens, SemanticTokenType::KEYWORD);
                    push_token(&f.ty, &mut semantic_tokens, SemanticTokenType::TYPE);
                }
                push_token(&s.end, &mut semantic_tokens, SemanticTokenType::KEYWORD);
            }
        }
    }

    semantic_tokens
}

fn push_expr_tokens(node: &Spanned<Expr>, tokens: &mut Vec<CompleteSemanticToken>) {
    match &node.inner {
        Expr::Keyword(_) => push_token(node, tokens, SemanticTokenType::KEYWORD),
        Expr::CompStop => push_token(node, tokens, SemanticTokenType::KEYWORD),
        Expr::Type(_) => push_token(node, tokens, SemanticTokenType::TYPE),
        Expr::Bind(b) => {
            push_token(&b.bind, tokens, SemanticTokenType::KEYWORD);
            for binding in &b.bindings {
                match &**binding {
                    Either::Left(_) => push_token(binding, tokens, SemanticTokenType::VARIABLE),
                    Either::Right(NameTypePair { name, sep, ty }) => {
                        push_token(name, tokens, SemanticTokenType::VARIABLE);
                        push_token(sep, tokens, SemanticTokenType::KEYWORD);
                        push_token(ty, tokens, SemanticTokenType::TYPE)
                    }
                }
            }
            push_token(&b.do_, tokens, SemanticTokenType::KEYWORD);
            for e in &b.body {
                push_expr_tokens(e, tokens)
            }
            push_token(&b.end, tokens, SemanticTokenType::KEYWORD);
        }
        Expr::While(w) => {
            push_token(&w.while_, tokens, SemanticTokenType::KEYWORD);
            for e in &w.cond {
                push_expr_tokens(e, tokens);
            }
            push_token(&w.do_, tokens, SemanticTokenType::KEYWORD);
            for e in &w.body {
                push_expr_tokens(e, tokens);
            }
            push_token(&w.end, tokens, SemanticTokenType::KEYWORD);
        }
        Expr::If(i) => {
            push_token(&i.if_, tokens, SemanticTokenType::KEYWORD);
            for e in &i.truth {
                push_expr_tokens(e, tokens);
            }
            for e in &i.lie {
                push_token(&e.else_, tokens, SemanticTokenType::KEYWORD);
                for e in &e.body {
                    push_expr_tokens(e, tokens);
                }
            }
            push_token(&i.end, tokens, SemanticTokenType::KEYWORD);
        }
        Expr::Cond(c) => {
            push_token(&c.cond, tokens, SemanticTokenType::KEYWORD);
            push_token(&c.pat, tokens, SemanticTokenType::REGEXP);
            push_token(&c.do_, tokens, SemanticTokenType::KEYWORD);
            for e in &c.body {
                push_expr_tokens(e, tokens);
            }
            for b in &c.branches {
                push_token(&b.else_, tokens, SemanticTokenType::KEYWORD);
                push_token(&b.pat, tokens, SemanticTokenType::PARAMETER);
                push_token(&b.do_, tokens, SemanticTokenType::KEYWORD);
                for e in &b.body {
                    push_expr_tokens(e, tokens);
                }
            }
            push_token(&c.end, tokens, SemanticTokenType::KEYWORD);
        }
        Expr::Cast(c) => {
            push_token(&c.cast, tokens, SemanticTokenType::KEYWORD);
            push_token(&c.ty, tokens, SemanticTokenType::TYPE);
        }
        Expr::Word(_) => push_token(node, tokens, SemanticTokenType::FUNCTION),
        Expr::Path(_) => push_token(node, tokens, SemanticTokenType::FUNCTION),
        Expr::Literal(l) => {
            let ty = match l {
                Literal::Bool(_) => SemanticTokenType::NUMBER,
                Literal::Char(_) => SemanticTokenType::STRING,
                Literal::String(_) => SemanticTokenType::STRING,
                Literal::Num(_) => SemanticTokenType::NUMBER,
            };
            push_token(node, tokens, ty);
        }
        Expr::Var(v) => {
            push_token(&v.var, tokens, SemanticTokenType::KEYWORD);
            push_token(&v.name, tokens, SemanticTokenType::PARAMETER);
            push_token(&v.sep, tokens, SemanticTokenType::KEYWORD);
            push_token(&v.ty, tokens, SemanticTokenType::TYPE);
        }
        Expr::FieldAccess(a) => {
            push_token(&a.access, tokens, SemanticTokenType::KEYWORD);
            push_token(&a.field, tokens, SemanticTokenType::PARAMETER);
        }
    }
}

fn push_token<T>(
    node: &Spanned<T>,
    tokens: &mut Vec<CompleteSemanticToken>,
    typ: SemanticTokenType,
) {
    tokens.push(CompleteSemanticToken {
        start: node.span.start,
        length: node.span.length(),
        token_type: LEGEND_TYPE.iter().position(|item| item == &typ).unwrap(),
    })
}
