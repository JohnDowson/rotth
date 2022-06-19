// use crate::lir::{Mangled, Op};
use fnv::FnvHashMap;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassManager,
    values::{FunctionValue, PointerValue},
};
use smol_str::SmolStr;

use crate::lir::CompiledProc;

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    pub function: &'a CompiledProc,

    variables: FnvHashMap<SmolStr, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
}
