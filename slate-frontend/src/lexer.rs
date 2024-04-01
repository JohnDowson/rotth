use internment::Intern;
use logos::{Lexer, Logos};
use smol_str::SmolStr;
use spanner::Span;
use std::path::PathBuf;

fn to_char(l: &'_ mut Lexer<'_, Token>) -> Option<char> {
    l.slice().chars().nth(1)
}

fn indent_level(l: &'_ mut Lexer<'_, Token>) -> Option<usize> {
    // skip newline
    let mut chars = l.slice().chars().skip(1);
    let Some(first) = chars.next() else {
        return Some(0);
    };
    chars.clone().all(|c| c == first).then(|| chars.count() + 1)
}

fn to_bool(l: &'_ mut Lexer<'_, Token>) -> Option<bool> {
    match l.slice() {
        "true" => Some(true),
        "false" => Some(false),
        _ => None,
    }
}

fn to_interned_string(l: &'_ mut Lexer<'_, Token>) -> Intern<String> {
    Intern::from_ref(l.slice())
}

fn to_smol_str(l: &'_ mut Lexer<'_, Token>) -> SmolStr {
    SmolStr::from(l.slice())
}

#[derive(Clone, Hash, PartialEq, Eq, Logos)]
pub enum Token {
    #[token("&")]
    OpAnd,
    #[token("*")]
    OpMul,
    #[token("/")]
    OpDiv,
    #[token("+")]
    OpPlus,
    #[token("-")]
    OpMinus,
    #[token("=")]
    OpAssign,
    #[token("==")]
    OpEq,
    #[token(".")]
    OpDot,

    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token("::")]
    DoubleColon,
    #[token(",")]
    Comma,
    #[token("=>")]
    FatArrow,

    #[regex(r"[_A-Za-z][_A-Za-z0-9]*", to_interned_string)]
    Word(Intern<String>),
    #[regex("false|true", to_bool)]
    Bool(bool),
    #[regex(r#""(?:[^"]|\\")*""#, to_smol_str)]
    String(SmolStr),
    #[regex(r"'.'", to_char)]
    Char(char),
    #[regex(r"(-?[1-9][0-9]*)|0", to_smol_str)]
    Num(SmolStr),
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,

    #[token("module")]
    KwModule,
    #[token("use")]
    KwUse,
    #[token("from")]
    KwFrom,
    #[token("return")]
    KwReturn,
    #[token("match")]
    KwMatch,
    #[token("if")]
    KwIf,
    #[token("else")]
    KwElse,
    #[token("func")]
    KwFunc,
    #[token("while")]
    KwWhile,
    #[token("lambda")]
    KwLambda,
    #[token("const")]
    KwConst,
    #[token("static")]
    KwStatic,
    #[token("let")]
    KwLet,
    #[token("struct")]
    KwStruct,
    #[token("impl")]
    KwImpl,
    #[token("for")]
    KwFor,

    #[regex(r";.*\n", logos::skip)]
    Comment,

    #[regex(r"\n( |\t)*", indent_level, priority = 2)]
    Indent(usize),

    #[regex(r"[ \t]+", logos::skip, priority = 1)]
    Whitespace,

    Error,
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::OpAnd => write!(f, "&"),
            Token::OpMul => write!(f, "*"),
            Token::OpDiv => write!(f, "/"),
            Token::OpPlus => write!(f, "+"),
            Token::OpMinus => write!(f, "-"),
            Token::OpAssign => write!(f, "="),
            Token::OpEq => write!(f, "=="),
            Token::Semicolon => write!(f, ";"),
            Token::Colon => write!(f, ":"),
            Token::DoubleColon => write!(f, "::"),
            Token::OpDot => write!(f, "."),
            Token::Comma => write!(f, ","),
            Token::FatArrow => write!(f, "=>"),

            Token::Word(word) => write!(f, "W({word})"),
            Token::Bool(b) => write!(f, "{b}"),
            Token::String(str) => write!(f, "{str:?}"),
            Token::Char(c) => write!(f, "{c:?}"),
            Token::Num(num) => write!(f, "{num}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::KwUse => write!(f, "use"),
            Token::KwModule => write!(f, "module"),
            Token::KwFrom => write!(f, "from"),
            Token::KwReturn => write!(f, "return"),
            Token::KwMatch => write!(f, "match"),
            Token::KwIf => write!(f, "if"),
            Token::KwElse => write!(f, "else"),
            Token::KwFunc => write!(f, "func"),
            Token::KwWhile => write!(f, "while"),
            Token::KwFor => write!(f, "for"),
            Token::KwLambda => write!(f, "lambda"),
            Token::KwConst => write!(f, "const"),
            Token::KwStatic => write!(f, "static"),
            Token::KwLet => write!(f, "let"),
            Token::KwStruct => write!(f, "struct"),
            Token::KwImpl => write!(f, "impl"),

            Token::Comment => write!(f, "<comment>"),
            Token::Indent(level) => write!(f, "<indent-{}>", level),
            Token::Whitespace => write!(f, "<whitespace>"),
            Token::Error => write!(f, "<error>"),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Debug>::fmt(self, f)
    }
}

pub fn lex(src: &str, path: Intern<PathBuf>) -> (Vec<(Token, Span)>, Span) {
    let tt: Vec<_> = Token::lexer(src)
        .spanned()
        .map(|(t, s)| match t {
            Ok(t) => (t, Span::new(path, s.start, s.end)),
            Err(()) => (Token::Error, Span::new(path, s.start, s.end)),
        })
        .collect();
    let eoi = tt
        .last()
        .map_or(Span::point(path, 0), |(_, s)| Span::point(path, s.end));
    (tt, eoi)
}
