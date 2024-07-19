use rustc_hash::FxHashMap;

use crate::{
    collections::union_find::UnionFind,
    ir::{
        passman::{GlobalPass, LocalPass, PassResult},
        Block,
        Context,
        Func,
        Value,
    },
    utils::{cfg::CfgInfo, dominance::Dominance, loop_info::LoopContext},
};

enum InductionOp {
    Add,
    Sub,
    Mul,
    Div,
    Shl,

    Invariant,
    Unknown,
}

/// A record of an induction variable.
pub struct Indvar {
    /// The representative of thie induction variable.
    ///
    /// All induction variables are stored in a union-find data structure, this
    /// field is the parent of this induction variable.
    repr: Value,
    /// The evolving step of this induction variable.
    step: Value,
    /// The start value of this induction variable.
    start: Value,
    /// The end value of this induction variable.
    end: Value,
    /// The evolution method of this induction variable.
    op: InductionOp,
    /// The modulus of this induction variable.
    modulus: Option<Value>,
}

/// The analysis pass of scalar evolution.
#[derive(Default)]
pub struct Scev {
    loop_ctx: LoopContext<Block>,
    indvars: UnionFind<Value>,
}

pub struct ScevResult {
    indvars: Vec<Indvar>,
}

impl LocalPass for Scev {
    type Output = ScevResult;

    fn run(&mut self, ctx: &Context, func: Func) -> PassResult<Self::Output> {
        let cfg = CfgInfo::new(ctx, func);
        let dominance = Dominance::new(ctx, &cfg);

        self.loop_ctx = LoopContext::new(&cfg, &dominance);

        // TODO
        todo!()
    }
}

impl GlobalPass for Scev {
    type Output = FxHashMap<Func, ScevResult>;

    fn run(&mut self, ctx: &Context) -> PassResult<Self::Output> {
        let mut result = FxHashMap::default();
        for func in ctx.funcs() {
            result.insert(func, LocalPass::run(self, ctx, func)?);
        }
        Ok(result)
    }
}
