use std::path::PathBuf;

use clap::Parser;
use internment::Intern;

#[derive(Parser)]
struct Args {
    source: PathBuf,
}

fn main() -> std::io::Result<()> {
    let args = Args::parse();
    let src = std::fs::read_to_string(&args.source)?;
    let (tokens, eoi) = slate_frontend::lexer::lex(&src, Intern::new(args.source));
    println!("{tokens:?}");
    let ast = slate_frontend::parser::parse(tokens, eoi).unwrap();
    dbg!(ast);

    Ok(())
}
