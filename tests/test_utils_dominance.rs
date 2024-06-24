use common::{CfgBlock, CfgContext, CfgFunc};
use orzcc::utils::{cfg::CfgRegion, dominance::Dominance};

mod common;

#[test]
fn test_dominance_0() {
    let mut arena = CfgContext::default();
    let bb0 = CfgBlock::new(&mut arena);
    let bb1 = CfgBlock::new(&mut arena);
    let bb2 = CfgBlock::new(&mut arena);
    let bb3 = CfgBlock::new(&mut arena);
    let bb4 = CfgBlock::new(&mut arena);

    //       bb0
    //       / \
    //      /   \
    //    bb1   bb2
    //    /       \
    //  bb3 <---- bb4 <-+
    //   |              |
    //   |              |
    //   +--------------+
    //

    bb0.add_succ(&mut arena, bb1);
    bb0.add_succ(&mut arena, bb2);
    bb1.add_succ(&mut arena, bb3);
    bb2.add_succ(&mut arena, bb4);

    bb3.add_succ(&mut arena, bb4);
    bb4.add_succ(&mut arena, bb3);

    let region = CfgFunc::new(&mut arena, bb0);
    let cfg = region.cfg_info(&arena);

    let mut dominance = Dominance::default();

    dominance.compute(&arena, region, &cfg);

    assert_eq!(dominance.idom(bb0), None);
    assert_eq!(dominance.idom(bb1), Some(bb0));
    assert_eq!(dominance.idom(bb2), Some(bb0));
    assert_eq!(dominance.idom(bb3), Some(bb0));
    assert_eq!(dominance.idom(bb4), Some(bb0));

    assert_eq!(dominance.frontier(bb0), &[]);
    assert_eq!(dominance.frontier(bb1), &[bb3]);
    assert_eq!(dominance.frontier(bb2), &[bb4]);
    assert_eq!(dominance.frontier(bb3), &[bb4]);
    assert_eq!(dominance.frontier(bb4), &[bb3]);
}
