use fnv::{FnvHashMap, FnvHashSet};
use rotth_parser::ast::ItemPathBuf;

use super::{Mangled, Op, Value};

#[derive(Default)]
pub struct Block {
    pub ops: Vec<Op>,
    pub parents: FnvHashSet<BlockId>,
    pub children: FnvHashSet<BlockId>,
}

impl std::fmt::Debug for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, op) in self.ops.iter().enumerate() {
            writeln!(f, "\t{i}:\t{op:?}")?;
        }
        Ok(())
    }
}

pub struct ProcBuilder {
    pub blocks: Vec<Block>,
    pub current_block: BlockId,
    pub entry: BlockId,
    pub exit: BlockId,
    pub bindings: Vec<Vec<ItemPathBuf>>,
    pub locals: FnvHashMap<ItemPathBuf, usize>,
}

impl ProcBuilder {
    pub fn new() -> Self {
        let blocks = vec![Block::default(), Block::default()];
        Self {
            blocks,
            current_block: BlockId(0),
            entry: BlockId(0),
            exit: BlockId(1),
            bindings: Default::default(),
            locals: Default::default(),
        }
    }

    pub fn new_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len());
        self.blocks.push(Block::default());
        id
    }

    fn push_child(&mut self, blk: BlockId, child: BlockId) {
        self.blocks[child.0].parents.insert(blk);
        self.blocks[blk.0].children.insert(child);
    }

    pub fn switch_block(&mut self, blk: BlockId) {
        self.current_block = blk
    }

    pub fn dbg_graph(&self, path: impl AsRef<std::path::Path>) -> std::io::Result<()> {
        use std::io::Write;
        let mut f = std::fs::File::create(path)?;
        writeln!(f, "strict digraph {{")?;

        for (c, blk) in self.blocks.iter().enumerate() {
            for &child in &blk.children {
                writeln!(f, "\t{c} -> {child:?}")?;
            }
        }

        writeln!(f, "}}")?;
        Ok(())
    }

    pub fn push(&mut self, const_: Value) {
        self.blocks[self.current_block.0].ops.push(Op::Push(const_))
    }
    pub fn push_mem(&mut self) {}
    pub fn drop(&mut self) {
        self.blocks[self.current_block.0].ops.push(Op::Drop)
    }
    pub fn dup(&mut self) {
        self.blocks[self.current_block.0].ops.push(Op::Drop)
    }
    pub fn swap(&mut self) {
        self.blocks[self.current_block.0].ops.push(Op::Swap)
    }
    pub fn over(&mut self) {
        self.blocks[self.current_block.0].ops.push(Op::Over)
    }
    pub fn bind(&mut self) {
        self.blocks[self.current_block.0].ops.push(Op::Bind)
    }
    pub fn use_binding(&mut self, binding: usize) {
        self.blocks[self.current_block.0]
            .ops
            .push(Op::UseBinding(binding))
    }
    pub fn unbind(&mut self) {
        self.blocks[self.current_block.0].ops.push(Op::Unbind)
    }
    pub fn read(&mut self, size: usize) {
        self.blocks[self.current_block.0].ops.push(Op::Read(size))
    }
    pub fn write(&mut self, size: usize) {
        self.blocks[self.current_block.0].ops.push(Op::Write(size))
    }
    pub fn push_lvar(&mut self, lvar: usize) {
        self.blocks[self.current_block.0]
            .ops
            .push(Op::PushLvar(lvar))
    }
    pub fn add(&mut self) {
        self.blocks[self.current_block.0].ops.push(Op::Add)
    }
    pub fn sub(&mut self) {
        self.blocks[self.current_block.0].ops.push(Op::Sub)
    }
    pub fn divmod(&mut self) {
        self.blocks[self.current_block.0].ops.push(Op::Divmod)
    }
    pub fn mul(&mut self) {
        self.blocks[self.current_block.0].ops.push(Op::Mul)
    }
    pub fn eq(&mut self) {
        self.blocks[self.current_block.0].ops.push(Op::Eq)
    }
    pub fn ne(&mut self) {
        self.blocks[self.current_block.0].ops.push(Op::Ne)
    }
    pub fn lt(&mut self) {
        self.blocks[self.current_block.0].ops.push(Op::Lt)
    }
    pub fn le(&mut self) {
        self.blocks[self.current_block.0].ops.push(Op::Le)
    }
    pub fn gt(&mut self) {
        self.blocks[self.current_block.0].ops.push(Op::Gt)
    }
    pub fn ge(&mut self) {
        self.blocks[self.current_block.0].ops.push(Op::Ge)
    }
    pub fn jump(&mut self, block: BlockId) {
        self.blocks[self.current_block.0].ops.push(Op::Jump(block));
        self.push_child(self.current_block, block);
    }
    pub fn branch(&mut self, truth: BlockId, lie: BlockId) {
        self.blocks[self.current_block.0]
            .ops
            .push(Op::Branch(truth, lie));
        self.push_child(self.current_block, truth);
        self.push_child(self.current_block, lie);
    }
    pub fn call(&mut self, proc: Mangled) {
        self.blocks[self.current_block.0].ops.push(Op::Call(proc))
    }
    pub fn return_(&mut self) {
        self.blocks[self.current_block.0].ops.push(Op::Return);
        self.push_child(self.current_block, self.exit);
    }
}

impl Default for ProcBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);
