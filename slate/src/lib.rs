#![feature(iterator_try_collect)]

mod ast;
mod lexer;
mod parsers;

#[test]
fn test() {
    use lexer::Token;
    use logos::Logos as _;
    let s = std::fs::read_to_string("../slate-examples/1.sr").unwrap();
    let t = Token::lexer(&*s).try_collect::<Vec<Token>>().unwrap();
    println!("{t:?}")
}
