use slotmap::SlotMap;
use spanner::Spanned;

use crate::parser::ast::ResolvedModule;

slotmap::new_key_type! {
    pub struct FuncKey;
    pub struct ModuleKey;
}

pub struct HIR {
    modules: SlotMap<ModuleKey, (ModuleKey, usize)>,
    funcs: SlotMap<FuncKey, (ModuleKey, usize)>,
}

impl HIR {
    pub fn walk(root: ResolvedModule) -> HIR {
        let mut this = HIR {
            funcs: Default::default(),
            modules: Default::default(),
        };

        let key = this.modules.insert_with_key(|k| (k, 0));
        this.walk_module(root, key);

        this
    }

    fn walk_module(&mut self, module: ResolvedModule, key: ModuleKey) {
        for (
            i,
            Spanned {
                span: _,
                inner: module,
            },
        ) in module.modules.into_iter().enumerate()
        {
            let key = self.modules.insert((key, i));
            self.walk_module(module, key)
        }
    }
}
