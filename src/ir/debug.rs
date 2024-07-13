use rustc_hash::FxHashMap;

use super::{Block, Inst, Symbol};

/// The position of a comment relative to an entity.
pub enum CommentPos {
    /// Before the line.
    Before,
    /// At the end of the line.
    ///
    /// For functions or any other entities that can span multiple lines, the
    /// comment will be added after displaying the entity, without a
    /// newline.
    AtEnd,
    /// After the last line of the entity.
    After,
}

#[derive(Default)]
pub(super) struct CommentInfo {
    insts: FxHashMap<Inst, Vec<(CommentPos, String)>>,
    blocks: FxHashMap<Block, Vec<(CommentPos, String)>>,
    symbols: FxHashMap<Symbol, Vec<(CommentPos, String)>>,
}

impl CommentInfo {
    pub(super) fn comment_inst(&mut self, inst: Inst, pos: CommentPos, content: String) {
        self.insts.entry(inst).or_default().push((pos, content));
    }

    pub(super) fn comment_block(&mut self, block: Block, pos: CommentPos, content: String) {
        self.blocks.entry(block).or_default().push((pos, content));
    }

    pub(super) fn comment_symbol(&mut self, symbol: Symbol, pos: CommentPos, content: String) {
        self.symbols.entry(symbol).or_default().push((pos, content));
    }

    pub(super) fn get_inst_comments(&self, inst: Inst) -> Option<&[(CommentPos, String)]> {
        self.insts.get(&inst).map(|v| v.as_slice())
    }

    pub(super) fn get_block_comments(&self, block: Block) -> Option<&[(CommentPos, String)]> {
        self.blocks.get(&block).map(|v| v.as_slice())
    }

    pub(super) fn get_symbol_comments(&self, symbol: &Symbol) -> Option<&[(CommentPos, String)]> {
        self.symbols.get(symbol).map(|v| v.as_slice())
    }
}
