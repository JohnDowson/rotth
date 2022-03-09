use chumsky::{Parser, Stream};
use clap::Parser as ClapParser;
use rotth::{
    emit, eval::eval, hir::procs, lexer::lexer, lir, span::Span, typecheck::typecheck_program,
};
use somok::Somok;
use std::{
    fs::OpenOptions,
    io::{BufWriter, Read},
    path::PathBuf,
    time::Instant,
};

#[derive(ClapParser)]
struct Args {
    #[clap(short = 'k', long)]
    dump_tokens: bool,
    #[clap(short = 'i', long)]
    dump_hir: bool,
    #[clap(short = 'l', long)]
    dump_lir: bool,
    #[clap(short = 't', long)]
    time: bool,
    #[clap(long)]
    compile: bool,
    source: PathBuf,
}

fn main() -> std::io::Result<()> {
    let args = Args::parse();
    let mut src = String::new();
    std::fs::File::open(&args.source)?.read_to_string(&mut src)?;

    let start = Instant::now();

    let tokens = match lexer().parse(Stream::from_iter(
        Span::new(src.len(), src.len()),
        src.chars().enumerate().map(|(i, c)| (c, Span::point(i))),
    )) {
        Ok(ts) => ts,
        Err(es) => {
            for e in es {
                dbg!(e);
            }
            return ().okay();
        }
    };

    let tokenized = Instant::now();
    if args.time {
        println!("Tokenized in:\t{:?}", tokenized - start)
    }

    if args.dump_tokens {
        println!("{tokens:?}");
    }

    let ast = match procs().parse(Stream::from_iter(
        tokens.last().unwrap().1,
        tokens.into_iter(),
    )) {
        Ok(ast) => ast,
        Err(es) => {
            for e in es {
                dbg!(e);
            }
            return ().okay();
        }
    };

    let parsed = Instant::now();
    if args.time {
        println!("Parsed in:\t{:?}", parsed - tokenized)
    }

    if args.dump_hir {
        println!("{ast:#?}");
    }

    let procs = typecheck_program(ast).unwrap();

    let typechecked = Instant::now();
    if args.time {
        println!("Typechecked in:\t{:?}", typechecked - parsed)
    }

    let comp = lir::Compiler::default();
    let lir = comp.compile(procs);

    let transpiled = Instant::now();
    if args.time {
        println!("Transpiled in:\t{:?}", transpiled - typechecked);
    }

    if args.dump_lir {
        for (i, op) in lir.iter().enumerate() {
            println!("{i}:\t{op:?}");
        }
    }
    if args.compile {
        let comp = emit::Compiler::default();
        comp.compile(
            lir,
            BufWriter::new(
                OpenOptions::new()
                    .create(true)
                    .write(true)
                    .open(args.source.with_extension("asm"))?,
            ),
        )?;

        let compiled = Instant::now();
        if args.time {
            println!("Compiled in:\t{:?}", compiled - transpiled);
            println!("Total:\t{:?}", compiled - start);
        }
    } else {
        println!("exitcode: {}", eval(lir).unwrap());
        let evaluated = Instant::now();
        if args.time {
            println!("Evaluated in:\t{:?}", evaluated - transpiled);
            println!("Total:\t{:?}", evaluated - start);
        }
    }

    ().okay()
}
