use clap::Parser as ClapParser;
use rotth::{emit, eval::eval, hir::parse, lexer::lex, lir, typecheck::typecheck_program, Result};
use somok::Somok;
use std::{fs::OpenOptions, io::BufWriter, path::PathBuf, time::Instant};

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

fn main() -> Result<()> {
    let args = Args::parse();

    let start = Instant::now();

    let tokens = lex(args.source.clone())?;

    let tokenized = Instant::now();
    if args.time {
        println!("Tokenized in:\t{:?}", tokenized - start)
    }

    if args.dump_tokens {
        println!("Tokens:\n");
        println!("{tokens:?}");
    }

    let ast = parse(tokens)?;

    let parsed = Instant::now();
    if args.time {
        println!("Parsed in:\t{:?}", parsed - tokenized)
    }

    if args.dump_hir {
        println!("HIR:\n");
        println!("{ast:#?}");
    }

    let procs = typecheck_program(ast)?;

    let typechecked = Instant::now();
    if args.time {
        println!("Typechecked in:\t{:?}", typechecked - parsed)
    }

    let comp = lir::Compiler::default();
    let (lir, strs, mangle_table) = comp.compile(procs);

    let transpiled = Instant::now();
    if args.time {
        println!("Transpiled in:\t{:?}", transpiled - typechecked);
    }

    if args.dump_lir {
        println!("LIR:\n");
        for (i, op) in lir.iter().enumerate() {
            println!("{i}:\t{op:?}");
        }
    }
    if args.compile {
        let comp = emit::Compiler::default();
        comp.compile(
            lir,
            &strs,
            mangle_table,
            BufWriter::new(
                OpenOptions::new()
                    .create(true)
                    .write(true)
                    .truncate(true)
                    .open(args.source.with_extension("asm"))?,
            ),
        )?;

        let compiled = Instant::now();
        if args.time {
            println!("Compiled in:\t{:?}", compiled - transpiled);
            println!("Total:\t{:?}", compiled - start);
        }
    } else {
        println!("exitcode: {:?}", eval(lir, &strs).unwrap());
        let evaluated = Instant::now();
        if args.time {
            println!("Evaluated in:\t{:?}", evaluated - transpiled);
            println!("Total:\t{:?}", evaluated - start);
        }
    }

    ().okay()
}
