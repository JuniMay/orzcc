use common::{CfgBlock, CfgContext, CfgFunc};
use orzcc::utils::{cfg::CfgRegion, dominance::Dominance, loop_info::LoopContext};

mod common;

#[test]
fn test_utils_loop_info_0() {
    let mut arena = CfgContext::default();
    let bb0 = CfgBlock::new(&mut arena);
    let bb1 = CfgBlock::new(&mut arena);
    let bb2 = CfgBlock::new(&mut arena);
    let bb3 = CfgBlock::new(&mut arena);
    let bb4 = CfgBlock::new(&mut arena);

    bb0.add_succ(&mut arena, bb1);
    bb1.add_succ(&mut arena, bb2);
    // add twice to test duplicate
    bb1.add_succ(&mut arena, bb2);

    bb2.add_succ(&mut arena, bb1);
    bb2.add_succ(&mut arena, bb3);

    bb3.add_succ(&mut arena, bb0);
    bb3.add_succ(&mut arena, bb4);

    //  .--> bb0
    //  |     |
    //  | .- bb1 <--.
    //  | |   |     |
    //  | `> bb2 ---'
    //  |     |
    //  `--- bb3
    //        |
    //       bb4

    let func = CfgFunc::new(&mut arena, bb0);

    let cfg = func.cfg_info(&arena);
    let dominance = Dominance::new(&arena, &cfg);
    let loop_ctx = LoopContext::new(&cfg, &dominance);

    let loops = loop_ctx.loops();
    assert_eq!(loops.len(), 2);
    assert_eq!(
        loop_ctx.get_loop(bb0).unwrap(),
        loop_ctx.get_loop(bb3).unwrap()
    );
    assert_eq!(
        loop_ctx.get_loop(bb1).unwrap(),
        loop_ctx.get_loop(bb2).unwrap()
    );

    let outer = loop_ctx.get_loop(bb0).unwrap();
    let inner = loop_ctx.get_loop(bb1).unwrap();

    assert_eq!(outer.header(&loop_ctx), bb0);
    assert_eq!(inner.header(&loop_ctx), bb1);

    assert_eq!(outer.depth(&loop_ctx), 1);
    assert_eq!(inner.depth(&loop_ctx), 2);

    assert!(outer.is_child_of(&loop_ctx, outer));
    assert!(inner.is_child_of(&loop_ctx, outer));

    assert!(loop_ctx.is_in_loop(bb0, outer));
    assert!(loop_ctx.is_in_loop(bb1, outer));
    assert!(loop_ctx.is_in_loop(bb2, outer));
    assert!(loop_ctx.is_in_loop(bb3, outer));
    assert!(loop_ctx.is_in_loop(bb1, inner));
    assert!(loop_ctx.is_in_loop(bb2, inner));

    assert!(!loop_ctx.is_in_loop(bb4, outer));
    assert!(!loop_ctx.is_in_loop(bb4, inner));
}
