use std::collections::HashMap;

use internment::Intern;

struct Package {
    root_module: Module,
    dependencies: HashMap<Ident, Package>,
}
struct Module {
    submodules: HashMap<Ident, Module>,
}

#[derive(Hash, Eq, PartialEq)]
struct ItemPath(Vec<Ident>);

#[derive(Hash, Eq, PartialEq)]
struct Ident(Intern<String>);
