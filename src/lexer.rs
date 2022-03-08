use crate::span::Span;
use chumsky::{prelude::*, text::Character, Error};
use somok::Somok;

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Token {
    Word(String),
    KeyWord(KeyWord),
    Num(String),
    SigSep,
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Word(word) => write!(f, "{}", word),
            Self::KeyWord(keyword) => keyword.fmt(f),
            Self::Num(num) => write!(f, "{}", num),
            Self::SigSep => write!(f, ":"),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum KeyWord {
    If,
    Else,
    Proc,
    While,
    Do,
    End,
}

pub fn word_parser<C: Character, E: Error<C>>(
) -> impl Parser<C, C::Collection, Error = E> + Copy + Clone {
    const ALLOWED_NON_ALPHA: &[u8; 24] = b"(){}[]<>|\\/!@#$%^&*-=+_?";
    filter(|c: &C| {
        c.to_char().is_ascii_alphabetic() || ALLOWED_NON_ALPHA.contains(&(c.to_char() as u8))
    })
    .map(Some)
    .chain::<C, Vec<_>, _>(
        filter(|c: &C| {
            c.to_char().is_ascii_alphanumeric() || ALLOWED_NON_ALPHA.contains(&(c.to_char() as u8))
        })
        .repeated(),
    )
    .collect()
}

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char, Span>>
where
{
    let num = text::int(10).map(Token::Num);

    let word = word_parser().map(Token::Word);

    let keyword = word_parser().try_map(|i: String, s| {
        Token::KeyWord(match i.as_str() {
            "if" => KeyWord::If,
            "else" => KeyWord::Else,
            "proc" => KeyWord::Proc,
            "while" => KeyWord::While,
            "do" => KeyWord::Do,
            "end" => KeyWord::End,
            _ => return Err(Simple::custom(s, "Invalid keyword".to_string())),
        })
        .okay()
    });

    let sig_sep = just(':').map(|_| Token::SigSep);

    let token = num
        .or(keyword)
        .or(word)
        .or(sig_sep)
        .recover_with(skip_then_retry_until([]));

    let comment = just(";").then(take_until(just('\n'))).padded();

    token
        .map_with_span(|a, b| (a, b))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
}
