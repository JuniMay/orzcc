use common::{CfgBlock, CfgContext, CfgFunc};
use orzcc::utils::{cdg::CdgInfo, cfg::CfgRegion};

mod common;

#[test]
fn test_cfg_info_0() {
    let mut arena = CfgContext::default();
    let bb0 = CfgBlock::new(&mut arena);
    let bb1 = CfgBlock::new(&mut arena);
    let bb2 = CfgBlock::new(&mut arena);
    let bb3 = CfgBlock::new(&mut arena);
    let bb4 = CfgBlock::new(&mut arena);
    let bb5 = CfgBlock::new(&mut arena);
    let bb6 = CfgBlock::new(&mut arena);

    // bb7 unreachable
    let bb7 = CfgBlock::new(&mut arena);

    bb0.add_succ(&mut arena, bb1);
    bb0.add_succ(&mut arena, bb2);

    bb1.add_succ(&mut arena, bb3);
    bb2.add_succ(&mut arena, bb3);

    bb3.add_succ(&mut arena, bb4);
    bb0.add_succ(&mut arena, bb4);

    bb4.add_succ(&mut arena, bb5);

    bb5.add_succ(&mut arena, bb0);
    bb5.add_succ(&mut arena, bb1);
    bb5.add_succ(&mut arena, bb6);

    //   bb7     bb0 -----+ <-+
    //           / \      |   |
    //          /   \     |   |
    //   +--> bb1   bb2   |   |
    //   |      \   /     |   |
    //   |       \ /      |   |
    //   |       bb3      |   |
    //   |        |       |   |
    //   |       bb4 <----+   |
    //   |        |           |
    //   +------ bb5 ---------+
    //            |
    //           bb6
    //

    let func = CfgFunc::new(&mut arena, bb0);

    let cfg_info = func.cfg_info(&arena);

    assert_eq!(cfg_info.succs(bb0).unwrap(), [bb1, bb2, bb4]);
    assert_eq!(cfg_info.succs(bb1).unwrap(), [bb3]);
    assert_eq!(cfg_info.succs(bb2).unwrap(), [bb3]);
    assert_eq!(cfg_info.succs(bb3).unwrap(), [bb4]);
    assert_eq!(cfg_info.succs(bb4).unwrap(), [bb5]);
    assert_eq!(cfg_info.succs(bb5).unwrap(), [bb0, bb1, bb6]);
    assert_eq!(cfg_info.succs(bb6).unwrap(), []);

    assert_eq!(cfg_info.preds(bb0).unwrap(), [bb5]);
    assert_eq!(cfg_info.preds(bb1).unwrap(), [bb0, bb5]);
    assert_eq!(cfg_info.preds(bb2).unwrap(), [bb0]);
    assert_eq!(cfg_info.preds(bb3).unwrap(), [bb1, bb2]);
    assert_eq!(cfg_info.preds(bb4).unwrap(), [bb0, bb3]);
    assert_eq!(cfg_info.preds(bb5).unwrap(), [bb4]);
    assert_eq!(cfg_info.preds(bb6).unwrap(), [bb5]);

    assert_eq!(cfg_info.succs(bb7), None);
    assert_eq!(cfg_info.preds(bb7), None);
}

#[test]
fn test_cfg_info_1() {
    let mut arena = CfgContext::default();
    let bb0 = CfgBlock::new(&mut arena);
    let bb1 = CfgBlock::new(&mut arena);

    bb0.add_succ(&mut arena, bb1);
    bb0.add_succ(&mut arena, bb1);

    let func = CfgFunc::new(&mut arena, bb0);

    let cfg_info = func.cfg_info(&arena);

    assert_eq!(cfg_info.succs(bb0).unwrap(), [bb1, bb1]);
    assert_eq!(cfg_info.preds(bb1).unwrap(), [bb0, bb0]);
}

#[test]
fn test_cdg_info() {
    // ref: Moderen Compiler Implementation in C, FIGURE 19.15.

    let mut arena = CfgContext::default();
    let bb1 = CfgBlock::new(&mut arena);
    let bb2 = CfgBlock::new(&mut arena);
    let bb3 = CfgBlock::new(&mut arena);
    let bb4 = CfgBlock::new(&mut arena);
    let bb5 = CfgBlock::new(&mut arena);
    let bb6 = CfgBlock::new(&mut arena);
    let bb7 = CfgBlock::new(&mut arena);
    let exit = CfgBlock::new(&mut arena);

    bb1.add_succ(&mut arena, bb2);

    bb2.add_succ(&mut arena, bb3);
    bb2.add_succ(&mut arena, bb4);

    bb3.add_succ(&mut arena, bb5);
    bb3.add_succ(&mut arena, bb6);

    bb5.add_succ(&mut arena, bb7);

    bb6.add_succ(&mut arena, bb7);

    bb7.add_succ(&mut arena, bb2);

    bb4.add_succ(&mut arena, exit);

    let func = CfgFunc::new(&mut arena, bb1);

    let cdg = CdgInfo::<CfgBlock>::new(&arena, func);

    assert!(cdg.succs(bb2).contains(&bb3));
    assert!(cdg.succs(bb2).contains(&bb7));
    assert!(cdg.succs(bb2).contains(&bb2));

    assert!(cdg.succs(bb3).contains(&bb5));
    assert!(cdg.succs(bb3).contains(&bb6));

    assert!(cdg.succs(bb1).is_empty());
    assert!(cdg.succs(bb4).is_empty());
    assert!(cdg.succs(bb5).is_empty());
    assert!(cdg.succs(bb6).is_empty());
    assert!(cdg.succs(bb7).is_empty());
}
