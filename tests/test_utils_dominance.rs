use common::{CfgBlock, CfgContext, CfgFunc};
use orzcc::utils::{cfg::CfgRegion, dominance::Dominance};

mod common;

#[test]
fn test_dominance_0() {
    let mut arena = CfgContext::default();
    let bb1 = CfgBlock::new(&mut arena);
    let bb2 = CfgBlock::new(&mut arena);
    let bb3 = CfgBlock::new(&mut arena);
    let bb4 = CfgBlock::new(&mut arena);
    let bb5 = CfgBlock::new(&mut arena);

    //       5
    //      / \
    //     /   \
    //    /     \
    //   4       3
    //   |       |
    //   1 <---- 2
    //   +------->
    //
    // Ref: Figure 2 in "A Simple, Fast Dominance Algorithm" by Cooper et al.

    bb5.add_succ(&mut arena, bb4);
    bb5.add_succ(&mut arena, bb3);

    bb4.add_succ(&mut arena, bb1);

    bb3.add_succ(&mut arena, bb2);

    bb1.add_succ(&mut arena, bb2);
    bb2.add_succ(&mut arena, bb1);

    let func = CfgFunc::new(&mut arena, bb5);
    let cfg = func.cfg_info(&arena);

    let dominance = Dominance::new(&arena, &cfg);

    assert_eq!(dominance.idom(bb5), None);
    assert_eq!(dominance.idom(bb4), Some(bb5));
    assert_eq!(dominance.idom(bb3), Some(bb5));
    assert_eq!(dominance.idom(bb2), Some(bb5));
    assert_eq!(dominance.idom(bb1), Some(bb5));

    assert_eq!(dominance.frontier(bb5), vec![]);
    assert_eq!(dominance.frontier(bb4), vec![bb1]);
    assert_eq!(dominance.frontier(bb3), vec![bb2]);
    assert_eq!(dominance.frontier(bb2), vec![bb1]);
    assert_eq!(dominance.frontier(bb1), vec![bb2]);
}
