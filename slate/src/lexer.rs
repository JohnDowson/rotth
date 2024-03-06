use internment::Intern;
use logos::Logos;

#[derive(PartialEq, Logos)]
pub enum Token {
    #[regex(r"[ \t]+")]
    Whitespace,
    #[regex(r"[\n\f]+")]
    Newline,

    #[regex(r"[\p{XID_Start}_]\p{XID_Continue}*", |s| Intern::new(s.slice().to_string()))]
    Ident(Intern<String>),

    #[token("&")]
    Ref,
    #[token(":")]
    Sep,
    #[token("module")]
    Module,
    #[token("proc")]
    Proc,
    #[token("do")]
    Do,
    #[token("bind")]
    Bind,
    #[token("end")]
    End,
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Whitespace => write!(f, "\\w"),
            Self::Newline => write!(f, "\\n"),
            Self::Ident(i) => write!(f, "{i}"),
            Self::Ref => write!(f, "&"),
            Self::Sep => write!(f, ":"),
            Self::Module => write!(f, "module"),
            Self::Proc => write!(f, "proc"),
            Self::Do => write!(f, "do"),
            Self::Bind => write!(f, "bind"),
            Self::End => write!(f, "end"),
        }
    }
}
