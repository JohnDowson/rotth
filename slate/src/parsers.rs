use chumsky::{input::Stream, prelude::*};
use internment::Intern;
use logos::Lexer;

use crate::{
    ast::{Expr, Ident, Item, Module, Package, Proc},
    lexer::Token,
};

pub fn parser<'s>() -> impl Parser<'s, Stream<Lexer<'s, Token>>, Package> {
    item().repeated().collect::<Vec<_>>().map(|items| Package {
        root_module: Module {
            name: Ident(Intern::new("".into())),
            items: items.into_iter().map(|i| (i.name(), i)).collect(),
        },
        dependencies: Default::default(),
    })
}

fn item<'s>() -> impl Parser<'s, Stream<Lexer<'s, Token>>, Item> {
    choice((proc().map(Item::Proc),))
}

fn proc<'s>() -> impl Parser<'s, Stream<Lexer<'s, Token>>, Proc> {
    just(Ok(Token::Proc))
        .then(ident())
        .then(body())
        .then(just(Ok(Token::End)))
        .map(|(((_proc, name), _body), _end)| Proc { name })
}

fn body<'s>() -> impl Parser<'s, Stream<Lexer<'s, Token>>, Vec<Expr>> {
    recursive(|body| {
        let bind = just(Ok(Token::Bind))
            .ignore_then(ident().repeated().collect::<Vec<_>>())
            .then(body)
            .map(|a| Expr::Bind);

        bind.or(ident().map(|a| Expr::Word))
    })
}

fn ident<'s>() -> impl Parser<'s, Stream<Lexer<'s, Token>>, Ident> + Clone {
    select! {
        Ok(Token::Ident(i)) => Ident(i)
    }
}
