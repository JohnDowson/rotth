use ariadne::{Color, FileCache, Fmt, Label, Report, ReportKind, Span};
use chumsky::error::SimpleReason;
use clap::Parser;
use rotth::{tir, typecheck, Error};
use rotth_parser::hir::Walker;
use somok::Somok;
use std::{path::PathBuf, time::Instant};

#[derive(Parser)]
struct Args {
    #[clap(short = 'k', long)]
    dump_tokens: bool,
    #[clap(short = 'a', long)]
    dump_ast: bool,
    #[clap(short = 'i', long)]
    dump_hir: bool,
    #[clap(short = 'c', long)]
    dump_tir: bool,
    #[clap(short = 'l', long)]
    dump_lir: bool,
    #[clap(short = 't', long)]
    time: bool,
    #[clap(long)]
    compile: bool,
    source: PathBuf,
}

fn main() -> std::result::Result<(), ()> {
    match compiler() {
        Ok(_) => ().okay(),
        Err(e) => {
            report_errors(e);
            ().error()
        }
    }
}

fn report_errors(e: Error) {
    let mut sources = FileCache::default();
    match e {
        Error::IO(e) => eprintln!("{}", e),
        Error::Lexer => {}
        Error::Parser(rotth_parser::ParserError(es)) => {
            for e in es {
                match e {
                    rotth_parser::Error::Parser(e) => {
                        let report =
                            Report::build(ReportKind::Error, e.span().source(), e.span().start);

                        let report = match e.reason() {
                            SimpleReason::Unexpected => report
                                .with_message(format!(
                                    "{}, expected {}",
                                    if e.found().is_some() {
                                        "Unexpected token in input"
                                    } else {
                                        "Unexpected end of input"
                                    },
                                    if e.expected().len() == 0 {
                                        "something else".to_string()
                                    } else {
                                        e.expected()
                                            .map(|expected| match expected {
                                                Some(expected) => expected.to_string(),
                                                None => "end of input".to_string(),
                                            })
                                            .collect::<Vec<_>>()
                                            .join(", ")
                                    }
                                ))
                                .with_label(
                                    Label::new(e.span())
                                        .with_message(format!(
                                            "Unexpected token {}",
                                            e.found()
                                                .map(ToString::to_string)
                                                .unwrap_or_else(|| "end of file".to_string())
                                                .fg(Color::Red)
                                        ))
                                        .with_color(Color::Red),
                                ),
                            SimpleReason::Custom(msg) => report.with_message(msg).with_label(
                                Label::new(e.span())
                                    .with_message(format!("{}", msg.fg(Color::Red)))
                                    .with_color(Color::Red),
                            ),
                            SimpleReason::Unclosed {
                                span: _,
                                delimiter: _,
                            } => todo!(),
                        };
                        report.finish().print(&mut sources).unwrap();
                    }
                    rotth_parser::Error::Redefinition(e) => {
                        let report = Report::build(
                            ReportKind::Error,
                            e.redefined_item.source(),
                            e.redefined_item.start,
                        )
                        .with_message("Duplicate word definitions")
                        .with_label(
                            Label::new(e.redefined_item)
                                .with_message("Word originally defined here...")
                                .with_color(Color::Green),
                        )
                        .with_label(
                            Label::new(e.redefining_item)
                                .with_message("redefined here")
                                .with_color(Color::Yellow),
                        );
                        report.finish().print(&mut sources).unwrap();
                    }
                    rotth_parser::Error::UnresolvedInclude(_) => todo!(),
                }
            }
        }
        Error::Typecheck(e) => {
            let report = Report::build(ReportKind::Error, e.span.source(), e.span.start)
                .with_message(e.message);

            let report = match e.kind {
                typecheck::ErrorKind::TypeMismatch { expected, actual } => report.with_label(
                    Label::new(e.span).with_message(
                        format!(
                            "Unexpected types: {} where {} expected",
                            format!("{:?}", actual).fg(Color::Green),
                            format!("{:?}", expected).fg(Color::Yellow)
                        )
                        .fg(Color::Red),
                    ),
                ),
                typecheck::ErrorKind::NotEnoughData => report.with_label(
                    Label::new(e.span).with_message("Not enough data on the stack".fg(Color::Red)),
                ),
                typecheck::ErrorKind::UnsupportedOperation => report.with_label(
                    Label::new(e.span).with_message("Not enough data on the stack".fg(Color::Red)),
                ),
                typecheck::ErrorKind::Undefined => report
                    .with_label(Label::new(e.span).with_message("Unknown word".fg(Color::Red))),
                typecheck::ErrorKind::InvalidMain => report.with_label(
                    Label::new(e.span).with_message(
                        format!("Invalid type signature for `{}`", "main".fg(Color::Yellow))
                            .fg(Color::Red),
                    ),
                ),
                typecheck::ErrorKind::InvalidWhile => {
                    report.with_label(Label::new(e.span).with_message(
                        "While body must not alter types on the stack".fg(Color::Red),
                    ))
                }
                typecheck::ErrorKind::CompStop => {
                    report.with_label(Label::new(e.span).with_message("Compilation stopped here"))
                }
                typecheck::ErrorKind::Unexpected => {
                    report.with_label(Label::new(e.span).with_message("Unexpected word"))
                }
                typecheck::ErrorKind::CallInConst => {
                    report.with_label(Label::new(e.span).with_message("Procedure call here"))
                }
                typecheck::ErrorKind::UnificationError => report.with_label(
                    Label::new(e.span).with_message("Unification error".fg(Color::Red)),
                ),
            };

            report.finish().print(&mut sources).unwrap();
        }
    }
}

fn compiler() -> Result<(), Error> {
    let args = Args::parse();

    let start = Instant::now();

    let source = Box::leak(args.source.canonicalize()?.into_boxed_path());

    let src_text = Box::leak(std::fs::read_to_string(&source)?.into_boxed_str());

    let tokens = rotth_lexer::lex(src_text, source);

    let tokenized = Instant::now();
    if args.time {
        println!("Tokenized in:\t{:?}", tokenized - start)
    }

    if args.dump_tokens {
        println!("Tokens:");
        println!("{tokens:?}");
    }

    let ast = rotth_parser::ast::parse(tokens)?;
    let ast = rotth_parser::ast::resolve_includes(ast)?;
    // let (structs, ast) = ast
    //     .into_iter()
    //     .partition::<FnvHashMap<_, _>, _>(|(_, i)| matches!(i, ast::TopLevel::Struct(_)));

    let parsed = Instant::now();
    if args.time {
        println!("Parsed in:\t{:?}", parsed - tokenized)
    }

    if args.dump_ast {
        println!("AST:");
        println!("{ast:#?}");
    }

    // let struct_index = rotth::types::define_structs(structs);

    let walker = Walker::new();
    let (hir, structs) = walker.walk_ast(ast);

    let lowered = Instant::now();
    if args.time {
        println!("Lowered in:\t{:?}", lowered - parsed)
    }

    if args.dump_hir {
        println!("HIR:");
        println!("{hir:#?}");
    }

    let tir = tir::Walker::walk(hir, structs)?;

    let typechecked = Instant::now();
    if args.time {
        println!("Typechecked in:\t{:?}", typechecked - lowered)
    }

    if args.dump_tir {
        println!("TIR:");
        println!("{tir:#?}");
    }

    // // let comp = lir::Compiler::new(struct_index);
    // // let (lir, strs, mems) = comp.compile(procs);

    // let transpiled = Instant::now();
    // if args.time {
    //     println!("Transpiled in:\t{:?}", transpiled - typechecked);
    // }

    // if args.dump_lir {
    //     println!("LIR:\n");
    //     // for (i, op) in lir.iter().enumerate() {
    //     //     println!("{i}:\t{op:?}");
    //     // }
    // }
    // if args.compile {
    //     // emit::compile(
    //     //     lir,
    //     //     &strs,
    //     //     &mems,
    //     //     BufWriter::new(
    //     //         OpenOptions::new()
    //     //             .create(true)
    //     //             .write(true)
    //     //             .truncate(true)
    //     //             .open(source.with_extension("asm"))?,
    //     //     ),
    //     // )?;

    //     let compiled = Instant::now();
    //     if args.time {
    //         println!("Compiled in:\t{:?}", compiled - transpiled);
    //         println!("Total:\t{:?}", compiled - start);
    //     }
    // } else {
    //     // println!("exitcode: {:?}", eval(lir, &strs).unwrap());
    //     let evaluated = Instant::now();
    //     if args.time {
    //         println!("Evaluated in:\t{:?}", evaluated - transpiled);
    //         println!("Total:\t{:?}", evaluated - start);
    //     }
    // }

    ().okay()
}
