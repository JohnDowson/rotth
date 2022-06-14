use rotth_parser::ast::*;
use spanner::Spanned;

#[derive(Debug)]
pub enum CompleteCompletionItem {
    Const(String),
    Mem(String),
    Proc(String),
}
/// return (need_to_continue_search, founded reference)
pub fn completion(ast: &[Spanned<TopLevel>], ident_offset: usize) -> Vec<CompleteCompletionItem> {
    let mut res = Vec::default();
    for item in ast.iter() {
        match &**item {
            TopLevel::Proc(p) => {
                if p.name.span.end < ident_offset {
                    let Spanned {
                        span: _,
                        inner: Word(name),
                    } = &p.name;
                    res.push(CompleteCompletionItem::Proc(name.to_string()));
                }
            }
            TopLevel::Const(c) => {
                if c.name.span.end < ident_offset {
                    let Spanned {
                        span: _,
                        inner: Word(name),
                    } = &c.name;
                    res.push(CompleteCompletionItem::Proc(name.to_string()));
                }
            }
            TopLevel::Mem(m) => {
                if m.name.span.end < ident_offset {
                    let Spanned {
                        span: _,
                        inner: Word(name),
                    } = &m.name;
                    res.push(CompleteCompletionItem::Proc(name.to_string()));
                }
            }
            _ => (),
        }
    }
    res
}
