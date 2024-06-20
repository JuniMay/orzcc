use common::{CfgBlock, CfgContext, CfgFunc};
use orzcc::utils::dfs::{DfsContext, Event};

mod common;

#[test]
fn test_dfs_0() {
    let mut arena = CfgContext::default();
    let bb0 = CfgBlock::new(&mut arena);
    let bb1 = CfgBlock::new(&mut arena);
    let bb2 = CfgBlock::new(&mut arena);
    let bb3 = CfgBlock::new(&mut arena);
    let bb4 = CfgBlock::new(&mut arena);
    let bb5 = CfgBlock::new(&mut arena);
    let bb6 = CfgBlock::new(&mut arena);

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

    //           bb0 -----+ <-+
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

    let mut dfs = DfsContext::<CfgBlock>::default();

    let nodes = dfs.iter(&arena, func).collect::<Vec<_>>();

    assert_eq!(
        nodes,
        vec![
            (Event::Enter, bb0),
            (Event::Enter, bb1),
            (Event::Enter, bb3),
            (Event::Enter, bb4),
            (Event::Enter, bb5),
            (Event::Enter, bb6),
            (Event::Leave, bb6),
            (Event::Leave, bb5),
            (Event::Leave, bb4),
            (Event::Leave, bb3),
            (Event::Leave, bb1),
            (Event::Enter, bb2),
            (Event::Leave, bb2),
            (Event::Enter, bb4), // because bb4 was pushed when visiting bb0
            (Event::Leave, bb0)
        ]
    );
}
