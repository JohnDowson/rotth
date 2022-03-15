use rotth::{
    ast::AstKind,
    ast::{AstNode, Binding, TopLevel},
    iconst::IConst,
};
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

pub fn semantic_token_from_ast(ast: &[TopLevel]) -> Vec<CompleteSemanticToken> {
    let mut semantic_tokens = vec![];

    for item in ast.iter() {
        match item {
            TopLevel::Include(i) => {
                push_token(&i.include, &mut semantic_tokens, SemanticTokenType::KEYWORD);
                push_token(&i.path, &mut semantic_tokens, SemanticTokenType::STRING);
            }
            TopLevel::Proc(p) => {
                push_token(&p.proc, &mut semantic_tokens, SemanticTokenType::KEYWORD);
                push_token(&p.name, &mut semantic_tokens, SemanticTokenType::FUNCTION);
                let signature =
                    rotth::coerce_ast!(p.signature => REF ProcSignature || unreachable!());
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
                push_tokens_recursively(&p.body, &mut semantic_tokens);
                push_token(&p.end, &mut semantic_tokens, SemanticTokenType::KEYWORD);
            }
            TopLevel::Const(c) => {
                push_token(&c.const_, &mut semantic_tokens, SemanticTokenType::KEYWORD);
                push_token(&c.name, &mut semantic_tokens, SemanticTokenType::TYPE);
                push_token(&c.do_, &mut semantic_tokens, SemanticTokenType::KEYWORD);
                push_tokens_recursively(&c.body, &mut semantic_tokens);
                push_token(&c.end, &mut semantic_tokens, SemanticTokenType::KEYWORD);
            }
            TopLevel::Mem(m) => {
                push_token(&m.mem, &mut semantic_tokens, SemanticTokenType::KEYWORD);
                push_token(&m.name, &mut semantic_tokens, SemanticTokenType::TYPE);
                push_token(&m.do_, &mut semantic_tokens, SemanticTokenType::KEYWORD);
                push_tokens_recursively(&m.body, &mut semantic_tokens);
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
                for field in &s.body {
                    push_tokens_recursively(field, &mut semantic_tokens);
                }
                push_token(&s.end, &mut semantic_tokens, SemanticTokenType::KEYWORD);
            }
        }
    }

    semantic_tokens
}

fn push_tokens_recursively(node: &AstNode, tokens: &mut Vec<CompleteSemanticToken>) {
    match &node.ast {
        AstKind::StructField(f) => {
            push_token(&*f.name, tokens, SemanticTokenType::PARAMETER);
            push_token(&*f.sep, tokens, SemanticTokenType::KEYWORD);
            push_token(&*f.ty, tokens, SemanticTokenType::TYPE);
        }
        AstKind::KeyWord(_) => push_token(node, tokens, SemanticTokenType::KEYWORD),
        AstKind::Type(_) => push_token(node, tokens, SemanticTokenType::TYPE),
        AstKind::Separator => push_token(node, tokens, SemanticTokenType::KEYWORD),
        AstKind::Bind(b) => {
            push_token(&b.bind, tokens, SemanticTokenType::KEYWORD);
            for binding in &b.bindings {
                push_tokens_recursively(binding, tokens);
            }
            push_token(&b.do_, tokens, SemanticTokenType::KEYWORD);
            push_tokens_recursively(&b.body, tokens);
            push_token(&b.end, tokens, SemanticTokenType::KEYWORD);
        }
        AstKind::Binding(b) => {
            if let Binding::Bind { name, sep, ty } = b {
                push_token(name, tokens, SemanticTokenType::PARAMETER);
                push_token(sep, tokens, SemanticTokenType::KEYWORD);
                push_token(ty, tokens, SemanticTokenType::TYPE);
            } else {
                push_token(node, tokens, SemanticTokenType::PARAMETER);
            }
        }
        AstKind::While(w) => {
            push_token(&w.while_, tokens, SemanticTokenType::KEYWORD);
            push_tokens_recursively(&w.cond, tokens);
            push_token(&w.do_, tokens, SemanticTokenType::KEYWORD);
            push_tokens_recursively(&w.body, tokens);
            push_token(&w.end, tokens, SemanticTokenType::KEYWORD);
        }
        AstKind::If(i) => {
            push_token(&i.if_, tokens, SemanticTokenType::KEYWORD);
            push_tokens_recursively(&i.truth, tokens);
            for e in &i.lie {
                push_token(&e.else_, tokens, SemanticTokenType::KEYWORD);
                push_tokens_recursively(&e.body, tokens);
            }
            push_token(&i.end, tokens, SemanticTokenType::KEYWORD);
        }
        AstKind::Cond(c) => {
            push_token(&c.cond, tokens, SemanticTokenType::KEYWORD);
            push_token(&c.pat, tokens, SemanticTokenType::REGEXP);
            push_token(&c.do_, tokens, SemanticTokenType::KEYWORD);
            push_tokens_recursively(&c.body, tokens);
            for b in &c.branches {
                push_token(&b.else_, tokens, SemanticTokenType::KEYWORD);
                push_token(&b.pat, tokens, SemanticTokenType::PARAMETER);
                push_token(&b.do_, tokens, SemanticTokenType::KEYWORD);
                push_tokens_recursively(&b.body, tokens);
            }
            push_token(&c.end, tokens, SemanticTokenType::KEYWORD);
        }
        AstKind::Cast(c) => {
            push_token(&c.cast, tokens, SemanticTokenType::KEYWORD);
            push_token(&c.ty, tokens, SemanticTokenType::TYPE);
        }
        AstKind::Word(_) => push_token(node, tokens, SemanticTokenType::FUNCTION),
        AstKind::Path(_) => push_token(node, tokens, SemanticTokenType::STRING),
        AstKind::Literal(l) => {
            let ty = match l {
                IConst::Bool(_) => SemanticTokenType::NUMBER,
                IConst::U64(_) => SemanticTokenType::NUMBER,
                IConst::I64(_) => SemanticTokenType::NUMBER,
                IConst::Char(_) => SemanticTokenType::STRING,
                IConst::Str(_) => SemanticTokenType::STRING,
                IConst::Ptr(_) => unreachable!(),
            };
            push_token(node, tokens, ty);
        }
        AstKind::Pattern(_) => unreachable!(),
        AstKind::ProcSignature(_) => unreachable!(),
        AstKind::ConstSignature(_) => unreachable!(),
        AstKind::Body(b) => {
            for binding in b {
                push_tokens_recursively(binding, tokens);
            }
        }
        AstKind::Var(v) => {
            push_token(&v.var, tokens, SemanticTokenType::KEYWORD);
            if let Some(ret) = &v.ret {
                push_token(ret, tokens, SemanticTokenType::KEYWORD);
            }
            push_token(&v.name, tokens, SemanticTokenType::PARAMETER);
            push_token(&v.sep, tokens, SemanticTokenType::KEYWORD);
            push_token(&v.ty, tokens, SemanticTokenType::TYPE);
        }
        AstKind::Accessor => {
            push_token(node, tokens, SemanticTokenType::KEYWORD);
        }
        AstKind::FieldAccess(a) => {
            push_token(&a.access, tokens, SemanticTokenType::KEYWORD);
            push_token(&a.field, tokens, SemanticTokenType::PARAMETER);
        }
    }
}

fn push_token(node: &AstNode, tokens: &mut Vec<CompleteSemanticToken>, typ: SemanticTokenType) {
    tokens.push(CompleteSemanticToken {
        start: node.span.start,
        length: node.span.length(),
        token_type: LEGEND_TYPE.iter().position(|item| item == &typ).unwrap(),
    })
}
