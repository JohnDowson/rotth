use rotth::ast::TopLevel;

#[derive(Debug)]
pub enum CompleteCompletionItem {
    Const(String),
    Mem(String),
    Proc(String),
}
/// return (need_to_continue_search, founded reference)
pub fn completion(ast: &[TopLevel], ident_offset: usize) -> Vec<CompleteCompletionItem> {
    let mut res = Vec::default();
    for item in ast.iter() {
        if let TopLevel::Proc(p) = item {
            if p.name.span.end < ident_offset {
                let name = rotth::coerce_ast!(p.name => REF Word || unreachable!());
                res.push(CompleteCompletionItem::Proc(name.clone()));
            }
        } else if let TopLevel::Const(c) = item {
            if c.name.span.end < ident_offset {
                let name = rotth::coerce_ast!(c.name => REF Word || unreachable!());
                res.push(CompleteCompletionItem::Proc(name.clone()));
            }
        } else if let TopLevel::Mem(m) = item {
            if m.name.span.end < ident_offset {
                let name = rotth::coerce_ast!(m.name => REF Word || unreachable!());
                res.push(CompleteCompletionItem::Proc(name.clone()));
            }
        }
    }
    res
}
