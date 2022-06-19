use ariadne::{Color, FileCache, Fmt, Label, Report, ReportKind, Span};
use chumsky::error::SimpleReason;
use clap::Parser;
use rotth::{emit, lir, Error};
use rotth_analysis::{ctir, tir};
use rotth_parser::hir;
use somok::Somok;
use std::{fs::OpenOptions, io::BufWriter, path::PathBuf, time::Instant};

#[derive(Parser)]
struct Args {
    #[clap(short = 'k', long)]
    dump_tokens: bool,
    #[clap(short = 'a', long)]
    dump_ast: bool,
    #[clap(short = 'i', long)]
    dump_hir: bool,
    #[clap(short = 'r', long)]
    dump_tir: bool,
    #[clap(short = 'c', long)]
    dump_ctir: bool,
    #[clap(short = 'l', long)]
    dump_lir: bool,
    #[clap(short = 't', long)]
    time: bool,
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
            let span = if let Some(span) = e.span {
                span
            } else {
                dbg!(e);
                return;
            };
            let report =
                Report::build(ReportKind::Error, span.source(), span.start).with_message(e.message);

            let report = match e.kind {
                rotth_analysis::ErrorKind::TypeMismatch { expected, actual } => report.with_label(
                    Label::new(span).with_message(
                        format!(
                            "Unexpected types: {} where {} expected",
                            format!("{:?}", actual).fg(Color::Green),
                            format!("{:?}", expected).fg(Color::Yellow)
                        )
                        .fg(Color::Red),
                    ),
                ),
                rotth_analysis::ErrorKind::NotEnoughData => report.with_label(
                    Label::new(span).with_message("Not enough data on the stack".fg(Color::Red)),
                ),
                rotth_analysis::ErrorKind::UnsupportedOperation => report.with_label(
                    Label::new(span).with_message("Not enough data on the stack".fg(Color::Red)),
                ),
                rotth_analysis::ErrorKind::Undefined => {
                    report.with_label(Label::new(span).with_message("Unknown word".fg(Color::Red)))
                }
                rotth_analysis::ErrorKind::InvalidMain => report.with_label(
                    Label::new(span).with_message(
                        format!("Invalid type signature for `{}`", "main".fg(Color::Yellow))
                            .fg(Color::Red),
                    ),
                ),
                rotth_analysis::ErrorKind::InvalidWhile => {
                    report.with_label(Label::new(span).with_message(
                        "While body must not alter types on the stack".fg(Color::Red),
                    ))
                }
                rotth_analysis::ErrorKind::CompStop => {
                    report.with_label(Label::new(span).with_message("Compilation stopped here"))
                }
                rotth_analysis::ErrorKind::Unexpected => {
                    report.with_label(Label::new(span).with_message("Unexpected word"))
                }
                rotth_analysis::ErrorKind::CallInConst => {
                    report.with_label(Label::new(span).with_message("Procedure call here"))
                }
                rotth_analysis::ErrorKind::UnificationError(msg) => {
                    report.with_label(Label::new(span).with_message(format!(
                        "{}: {}",
                        "Unification error".fg(Color::Red),
                        msg
                    )))
                }
                rotth_analysis::ErrorKind::Concrete(c) => {
                    match c {
                        rotth_analysis::ctir::ConcreteError::IncorrectMainOutputs => report
                            .with_label(Label::new(span).with_message(format!(
                                "{}: `main` proc must have a single u64 output",
                                "IncorrectMainOutputs".fg(Color::Red)
                            ))),
                        rotth_analysis::ctir::ConcreteError::MainWithInputs => {
                            report.with_label(Label::new(span).with_message(format!(
                                "{}: `main` proc can't take inputs",
                                "MainWithInputs".fg(Color::Red)
                            )))
                        }
                        rotth_analysis::ctir::ConcreteError::GenericMain => {
                            report.with_label(Label::new(span).with_message(format!(
                                "{}: `main` proc can't be generic",
                                "GenericMain".fg(Color::Red)
                            )))
                        }
                        rotth_analysis::ctir::ConcreteError::NoEntry => {
                            report.with_label(Label::new(span).with_message(format!(
                                "{}: Entry point not found in this file",
                                "NoEntry".fg(Color::Red)
                            )))
                        }
                        rotth_analysis::ctir::ConcreteError::IncompleteProcedure(p) => report
                            .with_label(Label::new(span).with_message(format!(
                                "{}: encountered incomplete item {:?}",
                                "IncompleteProcedure".fg(Color::Red),
                                p
                            ))),
                        rotth_analysis::ctir::ConcreteError::IncompleteConst(p) => report
                            .with_label(Label::new(span).with_message(format!(
                                "{}: encountered incomplete item {:?}",
                                "IncompleteConst".fg(Color::Red),
                                p
                            ))),
                        rotth_analysis::ctir::ConcreteError::IncompleteMem(p) => {
                            report.with_label(Label::new(span).with_message(format!(
                                "{}: encountered incomplete item {:?}",
                                "IncompleteMem".fg(Color::Red),
                                p
                            )))
                        }
                        rotth_analysis::ctir::ConcreteError::IncompleteVar(p) => {
                            report.with_label(Label::new(span).with_message(format!(
                                "{}: encountered incomplete item {:?}",
                                "IncompleteVar".fg(Color::Red),
                                p
                            )))
                        }
                    }
                }
            };

            report.finish().print(&mut sources).unwrap();
        }
        Error::Concrete(e) => todo!("{e:?}"),
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
        println!("{:?}", tokens.iter().map(|(a, _)| a).collect::<Vec<_>>());
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

    let (hir, structs) = hir::Walker::walk_ast(ast);

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

    let ctir = ctir::Walker::walk(tir)?;

    let concretized = Instant::now();
    if args.time {
        println!("Concretized in:\t{:?}", concretized - typechecked)
    }

    if args.dump_ctir {
        println!("CTIR:");
        println!("{ctir:#?}");
    }

    let (lir, strings, mems) = lir::Compiler::compile(ctir);

    let transpiled = Instant::now();
    if args.time {
        println!("Transpiled in:\t{:?}", transpiled - typechecked);
    }

    if args.dump_lir {
        println!("LIR:");
        for (name, proc) in lir.iter() {
            println!("{name}:");
            println!("{proc:?}");
        }
        println!();
    }

    emit::asm::compile(
        lir,
        strings,
        mems,
        BufWriter::new(
            OpenOptions::new()
                .create(true)
                .write(true)
                .truncate(true)
                .open(source.with_extension("asm"))?,
        ),
    )?;

    let compiled = Instant::now();
    if args.time {
        println!("Compiled in:\t{:?}", compiled - transpiled);
        println!("Total:\t{:?}", compiled - start);
    }

    ().okay()
}
