use internment::Intern;
use logos::{Lexer, Logos};
use smol_str::SmolStr;
use spanner::Span;
use std::path::PathBuf;

fn to_char(l: &'_ mut Lexer<'_, Token>) -> Option<char> {
    l.slice().chars().nth(1)
}

fn to_bool(l: &'_ mut Lexer<'_, Token>) -> Option<bool> {
    match l.slice() {
        "true" => Some(true),
        "false" => Some(false),
        _ => None,
    }
}

fn to_smol_str(l: &'_ mut Lexer<'_, Token>) -> SmolStr {
    SmolStr::from(l.slice())
}

#[derive(Clone, Hash, PartialEq, Eq, Logos)]
pub enum Token {
    #[token("->", priority = 1000000)]
    FieldAccess,
    #[token("&>", priority = 1000000)]
    Ptr,
    #[token("&?&")]
    CompStop,
    #[token("@")]
    Read,
    #[token("!")]
    Write,
    #[regex(
        r"[()\{\}<>\|\\/#$%^&*\-=+?][()\{\}<>\|\\/!@#$%^&*\-=+?]?",
        to_smol_str
    )]
    Operator(SmolStr),
    #[regex(
        r"[_A-Za-z][()\{\}<>\|\\/!#$%^&*\-=+_?A-Za-z0-9]*",
        to_smol_str,
        priority = 0
    )]
    Word(SmolStr),
    #[regex("false|true", to_bool)]
    Bool(bool),
    #[regex(r#""(?:[^"]|\\")*""#, to_smol_str)]
    String(SmolStr),
    #[regex(r"'.'", to_char)]
    Char(char),
    #[regex(r"(-?[1-9][0-9]*)|0", to_smol_str)]
    Num(SmolStr),
    #[token(":")]
    SigSep,
    #[token("::")]
    PathSep,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,

    #[token("module")]
    KwModule,
    #[token("use")]
    KwUse,
    #[token("from")]
    KwFrom,
    #[token("return")]
    KwReturn,
    #[token("cond")]
    KwCond,
    #[token("if")]
    KwIf,
    #[token("else")]
    KwElse,
    #[token("proc")]
    KwProc,
    #[token("while")]
    KwWhile,
    #[token("do")]
    KwDo,
    #[token("bind")]
    KwBind,
    #[token("const")]
    KwConst,
    #[token("var")]
    KwVar,
    #[token("struct")]
    KwStruct,
    #[token("cast")]
    KwCast,
    #[token("end")]
    KwEnd,

    #[regex(r";.*\n", logos::skip)]
    Comment,

    #[regex(r"\p{Whitespace}+", logos::skip)]
    Whitespace,

    Error,
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Word(word) => write!(f, "W({word})"),
            Token::Read => write!(f, "@"),
            Token::Write => write!(f, "!"),
            Token::Operator(op) => write!(f, "O({op})"),
            Token::Bool(b) => write!(f, "{b}"),
            Token::String(str) => write!(f, "{str:?}"),
            Token::Char(c) => write!(f, "{c:?}"),
            Token::Num(num) => write!(f, "{num}"),
            Token::SigSep => write!(f, ":"),
            Token::PathSep => write!(f, "::"),
            Token::Ptr => write!(f, "&>"),
            Token::CompStop => write!(f, "&?&"),
            Token::FieldAccess => write!(f, "->"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::KwUse => write!(f, "use"),
            Token::KwModule => write!(f, "module"),
            Token::KwFrom => write!(f, "from"),
            Token::KwReturn => write!(f, "return"),
            Token::KwCond => write!(f, "cond"),
            Token::KwIf => write!(f, "if"),
            Token::KwElse => write!(f, "else"),
            Token::KwProc => write!(f, "proc"),
            Token::KwWhile => write!(f, "while"),
            Token::KwDo => write!(f, "do"),
            Token::KwBind => write!(f, "bind"),
            Token::KwConst => write!(f, "const"),
            Token::KwVar => write!(f, "var"),
            Token::KwStruct => write!(f, "struct"),
            Token::KwCast => write!(f, "cast"),
            Token::KwEnd => write!(f, "end"),
            Token::Comment => write!(f, "<comment>"),
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

pub fn lex(src: &str, path: Intern<PathBuf>) -> Vec<(Token, Span)> {
    Token::lexer(src)
        .spanned()
        .map(|(t, s)| match t {
            Ok(t) => (t, Span::new(path, s.start, s.end)),
            Err(()) => (Token::Error, Span::new(path, s.start, s.end)),
        })
        .collect()
}
