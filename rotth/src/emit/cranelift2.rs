use cranelift::{codegen::isa::TargetIsa, prelude::*};
use cranelift_module::{FuncId, Linkage, Module, ModuleResult};
use cranelift_object::{ObjectBuilder, ObjectModule};
use fnv::FnvHashMap;
use itempath::ItemPathBuf;
use rotth_analysis::{
    ctir::{CProc, ConcreteProgram, Intrinsic},
    inference::ReifiedType,
    tir::TypedIr,
};
use rotth_parser::{ast::Literal, types::Primitive};
use smol_str::ToSmolStr;
use target_lexicon::Triple;

use std::sync::Arc;

use super::disasm;

fn get_isa() -> Arc<dyn TargetIsa + 'static> {
    let mut flags_builder = codegen::settings::builder();
    flags_builder.set("opt_level", "speed").unwrap();
    flags_builder.set("is_pic", "false").unwrap();
    codegen::isa::lookup(Triple::host())
        .unwrap()
        .finish(settings::Flags::new(flags_builder))
        .unwrap()
}

const TRIPLE: Triple = Triple::host();

pub fn calling_convention() -> isa::CallConv {
    isa::CallConv::triple_default(&TRIPLE)
}

pub fn compile(
    ConcreteProgram {
        procs,
        consts,
        vars,
    }: ConcreteProgram,
) -> ModuleResult<Vec<u8>> {
    let builder =
        ObjectBuilder::new(get_isa(), "", cranelift_module::default_libcall_names()).unwrap();

    let mut module = ObjectModule::new(builder);

    let mut builder_ctx = FunctionBuilderContext::new();
    let mut ctx = module.make_context();

    let mut path_to_fid = FnvHashMap::default();
    for (path, proc) in &procs {
        let mut sig = module.make_signature();

        for ty in &proc.ins {
            match ty {
                ReifiedType::Ptr(_) => sig.params.push(AbiParam::new(module.isa().pointer_type())),
                ReifiedType::Primitive(prim) => match prim {
                    Primitive::Void => todo!(),
                    Primitive::Bool | Primitive::Char | Primitive::U8 | Primitive::I8 => {
                        sig.params.push(AbiParam::new(types::I8))
                    }
                    Primitive::U16 | Primitive::I16 => sig.params.push(AbiParam::new(types::I16)),
                    Primitive::U32 | Primitive::I32 => sig.params.push(AbiParam::new(types::I32)),
                    Primitive::U64 | Primitive::I64 => sig.params.push(AbiParam::new(types::I64)),
                },
                ReifiedType::Custom(_) => todo!(),
            }
        }

        for ty in &proc.outs {
            match ty {
                ReifiedType::Ptr(_) => sig.returns.push(AbiParam::new(module.isa().pointer_type())),
                ReifiedType::Primitive(prim) => match prim {
                    Primitive::Void => todo!(),
                    Primitive::Bool | Primitive::Char | Primitive::U8 | Primitive::I8 => {
                        sig.returns.push(AbiParam::new(types::I8))
                    }
                    Primitive::U16 | Primitive::I16 => sig.returns.push(AbiParam::new(types::I16)),
                    Primitive::U32 | Primitive::I32 => sig.returns.push(AbiParam::new(types::I32)),
                    Primitive::U64 | Primitive::I64 => sig.returns.push(AbiParam::new(types::I64)),
                },
                ReifiedType::Custom(_) => todo!(),
            }
        }

        let fid = module.declare_function(&path.to_smolstr(), Linkage::Export, &sig)?;

        path_to_fid.insert(path.clone(), (fid, sig));
    }

    for (path, proc) in procs {
        compile_proc(
            path,
            proc,
            &path_to_fid,
            &mut module,
            &mut ctx,
            &mut builder_ctx,
        )?;
    }

    let object = module.finish().emit().unwrap();
    Ok(object)
}

fn compile_proc(
    path: ItemPathBuf,
    proc: CProc,
    path_to_fid: &FnvHashMap<ItemPathBuf, (FuncId, Signature)>,
    module: &mut ObjectModule,
    ctx: &mut codegen::Context,
    builder_ctx: &mut FunctionBuilderContext,
) -> ModuleResult<()> {
    module.clear_context(ctx);

    let (fid, sig) = path_to_fid[&path].clone();
    ctx.func.signature = sig;

    let callees_frefs = proc
        .callees
        .into_iter()
        .map(|(callee, (ins, outs))| {
            let (fid, _) = &path_to_fid[&callee];
            let fref = module.declare_func_in_func(*fid, &mut ctx.func);
            (callee, (fref, ins, outs))
        })
        .collect::<FnvHashMap<_, _>>();

    let mut builder = FunctionBuilder::new(&mut ctx.func, builder_ctx);
    let entry_b = builder.create_block();
    builder.seal_block(entry_b);
    builder.append_block_params_for_function_params(entry_b);
    builder.switch_to_block(entry_b);

    let mut stack: Vec<Value> = Vec::new();
    stack.extend_from_slice(builder.block_params(entry_b));

    for node in proc.body {
        match node.node {
            TypedIr::GVarUse(_) => todo!(),
            TypedIr::LVarUse(_) => todo!(),
            TypedIr::BindingUse(_) => todo!(),
            TypedIr::ConstUse(_) => todo!(),
            TypedIr::Call(path) => {
                let (fref, ins, outs) = callees_frefs[&path];
                let ins = (0..ins).map(|_| stack.pop().unwrap()).collect::<Vec<_>>();
                let call = builder.ins().call(fref, &ins);
                for &val in builder.inst_results(call) {
                    stack.push(val)
                }
            }
            TypedIr::Intrinsic(intr) => match intr {
                Intrinsic::Drop => {
                    stack.pop();
                }
                Intrinsic::Dup => {
                    stack.push(*stack.last().unwrap());
                }
                Intrinsic::Swap => {
                    let len = stack.len();
                    stack.swap(len - 1, len - 2)
                }
                Intrinsic::Over => {
                    let len = stack.len();
                    let val = stack[len - 2];
                    stack.push(val);
                }
                Intrinsic::Cast(_) => {
                    // I think this is a no-op in most cases
                }
                Intrinsic::Read(_) => todo!(),
                Intrinsic::Write(_) => todo!(),
                Intrinsic::Add => {
                    let b = stack.pop().unwrap();
                    let a = stack.pop().unwrap();
                    let res = match &node.ins[0] {
                        ReifiedType::Primitive(prim) => match prim {
                            Primitive::U64 | Primitive::U32 | Primitive::U16 | Primitive::U8 => {
                                let (res, _) = builder.ins().uadd_overflow(a, b);
                                res
                            }
                            Primitive::I64 | Primitive::I32 | Primitive::I16 | Primitive::I8 => {
                                builder.ins().iadd(a, b)
                            }
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };
                    stack.push(res);
                }
                Intrinsic::Sub => {
                    let b = stack.pop().unwrap();
                    let a = stack.pop().unwrap();
                    let res = match &node.ins[0] {
                        ReifiedType::Primitive(prim) => match prim {
                            Primitive::U64 | Primitive::U32 | Primitive::U16 | Primitive::U8 => {
                                let (res, _) = builder.ins().usub_overflow(a, b);
                                res
                            }
                            Primitive::I64 | Primitive::I32 | Primitive::I16 | Primitive::I8 => {
                                builder.ins().isub(a, b)
                            }
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };
                    stack.push(res);
                }
                Intrinsic::Divmod => todo!(),
                Intrinsic::Mul => todo!(),
                Intrinsic::Eq => todo!(),
                Intrinsic::Ne => todo!(),
                Intrinsic::Lt => todo!(),
                Intrinsic::Le => todo!(),
                Intrinsic::Gt => todo!(),
                Intrinsic::Ge => todo!(),
            },
            TypedIr::Bind(_) => todo!(),
            TypedIr::While(_) => todo!(),
            TypedIr::If(_) => todo!(),
            TypedIr::Cond(_) => todo!(),
            TypedIr::Literal(lit) => {
                let val = match lit {
                    Literal::Bool(b) => builder.ins().iconst(types::I8, b as i64),
                    Literal::Int(i) => builder.ins().iconst(types::I64, i),
                    Literal::UInt(u) => builder
                        .ins()
                        .iconst(types::I64, unsafe { std::mem::transmute::<_, i64>(u) }),
                    Literal::String(_) => todo!(),
                    Literal::Char(_) => todo!(),
                };
                stack.push(val);
            }
            TypedIr::IgnorePattern => todo!(),
            TypedIr::Return => {
                builder.ins().return_(&stack);
            }
            TypedIr::FieldAccess(_) => todo!(),
        }
    }
    builder.ins().return_(&stack);
    builder.finalize();

    module.define_function(fid, ctx)?;
    Ok(())
}
