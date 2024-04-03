use std::path::PathBuf;

use clap::Parser;
use internment::Intern;

#[derive(Parser)]
struct Args {
    source: PathBuf,
}

fn main() -> std::io::Result<()> {
    tracing_subscriber::fmt()
        .pretty()
        .with_timer(tracing_subscriber::fmt::time::uptime())
        .with_span_events(
            tracing_subscriber::fmt::format::FmtSpan::ENTER
                | tracing_subscriber::fmt::format::FmtSpan::EXIT,
        )
        .init();
    let args = Args::parse();
    let src = std::fs::read_to_string(&args.source)?;
    let (tokens, eoi) = slate_frontend::lexer::lex(&src, Intern::new(args.source));
    let ast = slate_frontend::parser::parse(tokens, eoi).unwrap();

    Ok(())
}
