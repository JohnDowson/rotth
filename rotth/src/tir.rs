use crate::hir;

pub enum TopLevel {
    Proc(Proc),
}

pub struct Proc {}

pub struct Walker {}

impl Walker {
    pub fn walk_hir(&mut self, item: hir::TopLevel) -> TopLevel {
        match item {
            hir::TopLevel::Proc(p) => TopLevel::Proc(self.walk_proc(p)),
            hir::TopLevel::Const(_c) => todo!(),
            hir::TopLevel::Mem(_m) => todo!(),
            hir::TopLevel::Var(_v) => todo!(),
        }
    }

    fn walk_proc(&mut self, proc: hir::Proc) -> Proc {
        todo!()
    }
}
