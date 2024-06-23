use orzcc::{
    collections::{linked_list::LinkedListContainerPtr, storage::ArenaPtr},
    ir::{debug::CommentPos, Block, Context, Func, IBinaryOp, Inst, Signature, Ty, Value},
    utils::{
        cfg::CfgNode,
        def_use::{Usable, User},
    },
};

#[test]
fn test_ir_def_use_0() {
    let mut ctx = Context::default();

    let int = Ty::int(&mut ctx, 32);
    let i1 = Inst::iconst(&mut ctx, 1, int);
    let i2 = Inst::iconst(&mut ctx, 2, int);

    let v1 = i1.result(&ctx, 0);
    let v2 = i2.result(&ctx, 0);

    let i3 = Inst::ibinary(&mut ctx, IBinaryOp::Add, v1, v2);

    let v3 = i3.result(&ctx, 0);

    assert_eq!(v3.ty(&ctx), int);

    assert_eq!(v1.users(&ctx), vec![i3]);
    assert_eq!(v2.users(&ctx), vec![i3]);
    assert_eq!(v3.users(&ctx), vec![]);

    i3.remove(&mut ctx);

    assert_eq!(v1.users(&ctx), vec![]);
    assert_eq!(v2.users(&ctx), vec![]);
    assert!(i3.try_deref(&ctx).is_none());
}

#[test]
fn test_ir_def_use_1() {
    let mut ctx = Context::default();

    let int = Ty::int(&mut ctx, 32);

    let i1 = Inst::iconst(&mut ctx, 1, int);
    let v1 = i1.result(&ctx, 0);

    let i3 = Inst::ibinary(&mut ctx, IBinaryOp::Add, v1, v1);
    let v3 = i3.result(&ctx, 0);

    assert_eq!(v3.ty(&ctx), int);

    assert_eq!(v1.users(&ctx), vec![i3]);
    assert_eq!(v3.users(&ctx), vec![]);

    i3.remove(&mut ctx);

    assert_eq!(v1.users(&ctx), vec![]);
    assert!(i3.try_deref(&ctx).is_none());
}

#[test]
#[should_panic]
fn test_ir_def_use_2() {
    let mut ctx = Context::default();

    let int = Ty::int(&mut ctx, 32);

    let i1 = Inst::iconst(&mut ctx, 1, int);
    let v1 = i1.result(&ctx, 0);

    let _ = Inst::ibinary(&mut ctx, IBinaryOp::Add, v1, v1);

    i1.remove(&mut ctx); // should panic because still a use in i3
}

#[test]
fn test_ir_block_def_use_0() {
    let mut ctx = Context::default();

    let int = Ty::int(&mut ctx, 32);
    let boolean = Ty::int(&mut ctx, 1);
    let float = Ty::float32(&mut ctx);
    let void = Ty::void(&mut ctx);

    // construct ir:
    // func @test() -> void {
    // ^entry:
    //     %v1 = iconst 1 : i32
    //     %v2 = iconst 2 : f32
    //
    //     %dummy_cond = iconst 1 : i1
    //     br %dummy_cond, ^bb1, ^bb2
    // ^bb1:
    //      %v3 = add %v1, %v1 : i32
    //      jump ^merge(%v3, %v2)
    // ^bb2:
    //      %v4 = mul %v1, %v1 : i32
    //      jump ^merge(%v4, %v2)
    //
    // ^merge(%p1: i32, %p2: f32):
    //      %v5 = add %p1, %p1 : i32 // make sure p1 is used
    //
    //      /* and p2 is not used */
    //
    //      ret void
    // }
    //
    let sig = Signature::new(vec![], vec![void]);
    let func = Func::new(&mut ctx, "@test", sig);

    let entry = Block::new(&mut ctx);
    let bb1 = Block::new(&mut ctx);
    let bb2 = Block::new(&mut ctx);
    let merge = Block::new(&mut ctx);

    let p1 = merge.new_param(&mut ctx, int);
    let p2 = merge.new_param(&mut ctx, float);

    func.push_back(&mut ctx, entry);
    func.push_back(&mut ctx, bb1);
    func.push_back(&mut ctx, bb2);
    func.push_back(&mut ctx, merge);

    let i1 = Inst::iconst(&mut ctx, 1, int);
    let i2 = Inst::fconst32(&mut ctx, 2.0, float);

    let v1 = i1.result(&ctx, 0);
    let v2 = i2.result(&ctx, 0);

    let i_dummy_cond = Inst::iconst(&mut ctx, 1, boolean);
    let dummy_cond = i_dummy_cond.result(&ctx, 0);

    let br = Inst::br(&mut ctx, dummy_cond, bb1, vec![], bb2, vec![]);

    assert_eq!(User::<Value>::all_uses(br, &ctx), vec![dummy_cond]);
    assert_eq!(User::<Block>::all_uses(br, &ctx), vec![bb1, bb2]);

    entry.push_back(&mut ctx, i1);
    entry.push_back(&mut ctx, i2);
    entry.push_back(&mut ctx, i_dummy_cond);
    entry.push_back(&mut ctx, br);

    let i3 = Inst::ibinary(&mut ctx, IBinaryOp::Add, v1, v1);
    let i4 = Inst::ibinary(&mut ctx, IBinaryOp::Mul, v1, v1);

    let v3 = i3.result(&ctx, 0);
    let v4 = i4.result(&ctx, 0);

    let jump1 = Inst::jump(&mut ctx, merge, vec![v3, v2]);
    let jump2 = Inst::jump(&mut ctx, merge, vec![v4, v2]);

    assert_eq!(User::<Value>::all_uses(jump1, &ctx).len(), 2);
    assert!(User::<Value>::all_uses(jump1, &ctx).contains(&v3));
    assert!(User::<Value>::all_uses(jump1, &ctx).contains(&v2));

    assert_eq!(User::<Value>::all_uses(jump2, &ctx).len(), 2);
    assert!(User::<Value>::all_uses(jump2, &ctx).contains(&v4));
    assert!(User::<Value>::all_uses(jump2, &ctx).contains(&v2));

    assert_eq!(User::<Block>::all_uses(jump1, &ctx), vec![merge]);
    assert_eq!(User::<Block>::all_uses(jump2, &ctx), vec![merge]);

    bb1.push_back(&mut ctx, i3);
    bb1.push_back(&mut ctx, jump1);

    bb2.push_back(&mut ctx, i4);
    bb2.push_back(&mut ctx, jump2);

    let i5 = Inst::ibinary(&mut ctx, IBinaryOp::Add, p1, p1);

    assert!(bb1.users(&ctx).contains(&br));
    assert!(bb2.users(&ctx).contains(&br));
    assert!(merge.users(&ctx).contains(&jump1));
    assert!(merge.users(&ctx).contains(&jump2));

    assert!(v2.users(&ctx).contains(&jump1));
    assert!(v2.users(&ctx).contains(&jump2));

    assert!(p1.users(&ctx).contains(&i5));
    assert!(p2.users(&ctx).is_empty());

    assert_eq!(merge.params(&ctx).len(), 2);
    assert_eq!(p1.idx(&ctx), 0);
    assert_eq!(p2.idx(&ctx), 1);

    merge.drop_param(&mut ctx, 1);

    assert_eq!(merge.params(&ctx).len(), 1);

    assert!(p2.try_deref(&ctx).is_none());
    assert!(v2.users(&ctx).is_empty());

    assert_eq!(entry.succs(&ctx), vec![bb1, bb2]);
    assert_eq!(bb1.succs(&ctx), vec![merge]);
}

#[test]
fn test_ir_replace_use_0() {
    let mut ctx = Context::default();

    let int = Ty::int(&mut ctx, 32);
    let i1 = Inst::iconst(&mut ctx, 1, int);
    let i2 = Inst::iconst(&mut ctx, 2, int);

    let v1 = i1.result(&ctx, 0);
    let v2 = i2.result(&ctx, 0);

    let i3 = Inst::ibinary(&mut ctx, IBinaryOp::Add, v1, v1);

    let v3 = i3.result(&ctx, 0);

    let i4 = Inst::ibinary(&mut ctx, IBinaryOp::Add, v3, v2);

    assert_eq!(v1.users(&ctx), vec![i3]);
    assert_eq!(v2.users(&ctx), vec![i4]);
    assert_eq!(v3.users(&ctx), vec![i4]);

    assert_eq!(User::<Value>::all_uses(i3, &ctx), vec![v1, v1]);
    i3.replace(&mut ctx, v1, v2);

    assert_eq!(v1.users(&ctx), vec![]);

    assert_eq!(v2.users(&ctx).len(), 2);
    assert!(v2.users(&ctx).contains(&i3));
    assert!(v2.users(&ctx).contains(&i4));

    assert_eq!(User::<Value>::all_uses(i3, &ctx), vec![v2, v2]);

    assert_eq!(v3.users(&ctx), vec![i4]);

    i4.replace(&mut ctx, v2, v1);

    assert_eq!(v2.users(&ctx), vec![i3]);
    assert_eq!(v1.users(&ctx), vec![i4]);

    assert_eq!(User::<Value>::all_uses(i4, &ctx), vec![v3, v1]);
}

#[test]
fn test_ir_display_0() {
    let mut ctx = Context::default();

    let int = Ty::int(&mut ctx, 32);
    let i1 = Inst::iconst(&mut ctx, 1, int);
    let i2 = Inst::iconst(&mut ctx, 2, int);

    let float = Ty::float32(&mut ctx);
    let f1 = Inst::fconst32(&mut ctx, 1.0f32, float);

    let v2 = i2.result(&ctx, 0);
    v2.assign_name(&mut ctx, "second");

    ctx.alloc_all_names();

    let s = format!("{}", i1.display(&ctx, true));
    assert_eq!(s, "%v0 /* 0 */ = iconst 0x00000001i32 : i32");

    let s = format!("{}", i2.display(&ctx, true));
    assert_eq!(s, "%second /* 1 */ = iconst 0x00000002i32 : i32");

    let s = format!("{}", f1.display(&ctx, true));
    let hex_1_0 = format!("0x{:08x}", f32::to_bits(1.0));
    // this is `v1` because i2 is assigned a name.
    let expected = format!("%v1 /* 2 */ = fconst {} : f32", hex_1_0);
    assert_eq!(s, expected);
}

#[test]
fn test_ir_diaplay_0() {
    let mut ctx = Context::default();

    let int = Ty::int(&mut ctx, 32);
    let boolean = Ty::int(&mut ctx, 1);
    let float = Ty::float32(&mut ctx);
    let void = Ty::void(&mut ctx);

    let sig = Signature::new(vec![], vec![void]);
    let func = Func::new(&mut ctx, "test", sig);

    func.comment(&mut ctx, CommentPos::Before, "Just a test function");
    func.comment(
        &mut ctx,
        CommentPos::AtEnd,
        "Just a test function comment at end",
    );

    func.comment(
        &mut ctx,
        CommentPos::After,
        "Just a test function comment after",
    );

    let entry = Block::new(&mut ctx);
    let bb1 = Block::new(&mut ctx);
    let bb2 = Block::new(&mut ctx);
    let merge = Block::new(&mut ctx);

    let p1 = merge.new_param(&mut ctx, int);
    let p2 = merge.new_param(&mut ctx, float);

    p1.assign_name(&mut ctx, "p1");
    p2.assign_name(&mut ctx, "p2");

    func.push_back(&mut ctx, entry);
    func.push_back(&mut ctx, bb1);
    func.push_back(&mut ctx, bb2);
    func.push_back(&mut ctx, merge);

    let i1 = Inst::iconst(&mut ctx, 1, int);
    let i2 = Inst::fconst32(&mut ctx, 2.0f32, float);

    let v1 = i1.result(&ctx, 0);
    let v2 = i2.result(&ctx, 0);

    let i_dummy_cond = Inst::iconst(&mut ctx, 1, boolean);
    let dummy_cond = i_dummy_cond.result(&ctx, 0);

    dummy_cond.assign_name(&mut ctx, "dummy_cond");

    let br = Inst::br(&mut ctx, dummy_cond, bb1, vec![], bb2, vec![]);

    entry.push_back(&mut ctx, i1);
    entry.push_back(&mut ctx, i2);
    entry.push_back(&mut ctx, i_dummy_cond);
    entry.push_back(&mut ctx, br);

    let i3 = Inst::ibinary(&mut ctx, IBinaryOp::Add, v1, v1);
    let i4 = Inst::ibinary(&mut ctx, IBinaryOp::Mul, v1, v1);

    let v3 = i3.result(&ctx, 0);
    let v4 = i4.result(&ctx, 0);

    let jump1 = Inst::jump(&mut ctx, merge, vec![v3, v2]);
    let jump2 = Inst::jump(&mut ctx, merge, vec![v4, v2]);

    bb1.push_back(&mut ctx, i3);
    bb1.push_back(&mut ctx, jump1);

    bb2.push_back(&mut ctx, i4);
    bb2.push_back(&mut ctx, jump2);

    let i5 = Inst::ibinary(&mut ctx, IBinaryOp::Add, p1, p1);
    let ret = Inst::return_(&mut ctx, vec![]);

    merge.push_back(&mut ctx, i5);
    merge.push_back(&mut ctx, ret);

    merge.assign_name(&mut ctx, "merge");

    ctx.alloc_all_names();

    let s = format!("{}", ctx.display(true));

    let expected = include_str!("ir_display/snapshot_0.orzir");

    assert_eq!(s.trim_end(), expected.trim_end());
}

// TODO: more tests on display
