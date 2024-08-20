//! The compiler executable.

use clap::{Arg, Command};
use orzcc::{
    backend::{
        reg_alloc::reg_coalescing::RegisterCoalescing,
        riscv64::{self, schedule},
        simplify_cfg::SimplifyCfg,
        LowerConfig,
    },
    ir::{
        passes::{
            adce::{Adce, ADCE},
            bool2cond::{Bool2Cond, BOOL2COND},
            branch2select::{Branch2Select, BRANCH2SELECT},
            branch_condition_sink::{BranchConditionSink, BRANCH_CONDITION_SINK},
            constant_phi::{ElimConstantPhi, ELIM_CONSTANT_PHI},
            control_flow::{
                BlockReorder,
                CfgCanonicalize,
                CfgSimplify,
                PHBlockLayout,
                SplitCriticalEdge,
                BLOCK_REORDER,
                CFG_SIMPLIFY,
                PH_BLOCK_LAYOUT,
                SPLIT_CRITICAL_EDGE,
            },
            fold::{ConstantFolding, CONSTANT_FOLDING},
            gcm::{Gcm, GCM},
            global2local::{Global2Local, GLOBAL2LOCAL},
            global_dce::{GlobalDce, GLOBAL_DCE},
            gvn::{GlobalValueNumbering, GVN},
            inline::{Inline, INLINE},
            instcombine::{
                AdvancedInstcombine,
                AggressiveInstcombine,
                Instcombine,
                ADVANCED_INSTCOMBINE,
                AGGRESSIVE_INSTCOMBINE,
                INSTCOMBINE,
            },
            legalize::{Legalize, LEGALIZE},
            loops::{
                AutoParallelize,
                DeadLoopElim,
                IndvarOffset,
                IndvarReduce,
                IndvarSimplify,
                Lcssa,
                LoopInvariantMotion,
                LoopPeel,
                LoopSimplify,
                LoopStrengthReduction,
                LoopUnroll,
                AUTO_PARALLELIZE,
                DEAD_LOOP_ELIM,
                INDVAR_OFFSET,
                INDVAR_REDUCE,
                INDVAR_SIMPLIFY,
                LOOP_INVARIANT_MOTION,
                LOOP_PEEL,
                LOOP_STRENGTH_REDUCTION,
                LOOP_UNROLL,
            },
            mem2reg::{Mem2reg, MEM2REG},
            simple_dce::{SimpleDce, SIMPLE_DCE},
            static_branch_prediction::{StaticBranchPrediction, STATIC_BRANCH_PREDICTION},
            tco::{Tco, TCO},
        },
        passman::{PassManager, Pipeline, TransformPass},
    },
};

struct CliCommand {
    /// The output assembly
    output: String,
    /// The source code
    source: String,
    /// Emitting ast
    emit_ast: Option<String>,
    /// Emitting type-checked ast
    emit_typed_ast: Option<String>,
    /// Emitting ir
    emit_ir: Option<String>,
    /// Emitting virtual-register code
    emit_vcode: Option<String>,
    /// Optimization level
    opt: u8,
    /// If aggressive optimizations are enabled
    aggressive: bool,
    /// Lower config
    lower_cfg: LowerConfig,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    #[cfg(feature = "frontend-sysy")]
    {
        use orzcc::{
            backend::{riscv64::lower::RvLowerSpec, LowerContext},
            frontend::sysy::{self, SysYParser},
        };

        let mut passman = PassManager::default();

        register_passes(&mut passman);
        let cmd = parse_args(&mut passman);

        let src = std::fs::read_to_string(&cmd.source)?;
        let src = sysy::preprocess(&src);

        let mut ast = SysYParser::new().parse(&src).unwrap();
        if let Some(emit_ast) = &cmd.emit_ast {
            std::fs::write(emit_ast, format!("{:#?}", ast))?;
        }

        ast.type_check();
        if let Some(emit_typed_ast) = &cmd.emit_typed_ast {
            std::fs::write(emit_typed_ast, format!("{:#?}", ast))?;
        }

        let mut ir = sysy::irgen(&ast, 64);

        if cmd.opt > 0 {
            let mut pipe_basic = Pipeline::default();
            {
                pipe_basic.add_pass(GLOBAL2LOCAL);
                pipe_basic.add_pass(GLOBAL_DCE);

                pipe_basic.add_pass(MEM2REG);
                pipe_basic.add_pass(ELIM_CONSTANT_PHI);
                pipe_basic.add_pass(SIMPLE_DCE);
                pipe_basic.add_pass(CFG_SIMPLIFY);

                pipe_basic.add_pass(CONSTANT_FOLDING);
                pipe_basic.add_pass(ELIM_CONSTANT_PHI);
                pipe_basic.add_pass(SIMPLE_DCE);
                pipe_basic.add_pass(CFG_SIMPLIFY);

                pipe_basic.add_pass(INSTCOMBINE);
                pipe_basic.add_pass(ELIM_CONSTANT_PHI);
                pipe_basic.add_pass(SIMPLE_DCE);
                pipe_basic.add_pass(CFG_SIMPLIFY);

                pipe_basic.add_pass(GCM);
                pipe_basic.add_pass(ELIM_CONSTANT_PHI);
                pipe_basic.add_pass(SIMPLE_DCE);
                pipe_basic.add_pass(CFG_SIMPLIFY);
                pipe_basic.add_pass(BRANCH_CONDITION_SINK);

                pipe_basic.add_pass(BRANCH2SELECT);
                pipe_basic.add_pass(ELIM_CONSTANT_PHI);
                pipe_basic.add_pass(SIMPLE_DCE);
                pipe_basic.add_pass(CFG_SIMPLIFY);
            }

            // basic + gvn, run after legalization.
            let mut pipe_gvn = Pipeline::default();
            {
                pipe_gvn.add_pass(GLOBAL2LOCAL);
                pipe_gvn.add_pass(GLOBAL_DCE);

                pipe_gvn.add_pass(MEM2REG);
                pipe_gvn.add_pass(ELIM_CONSTANT_PHI);
                pipe_gvn.add_pass(SIMPLE_DCE);
                pipe_gvn.add_pass(CFG_SIMPLIFY);

                pipe_gvn.add_pass(CONSTANT_FOLDING);
                pipe_gvn.add_pass(ELIM_CONSTANT_PHI);
                pipe_gvn.add_pass(SIMPLE_DCE);
                pipe_gvn.add_pass(CFG_SIMPLIFY);

                pipe_gvn.add_pass(INSTCOMBINE);
                pipe_gvn.add_pass(ELIM_CONSTANT_PHI);
                pipe_gvn.add_pass(SIMPLE_DCE);
                pipe_gvn.add_pass(CFG_SIMPLIFY);

                pipe_gvn.add_pass(GCM);
                pipe_gvn.add_pass(ELIM_CONSTANT_PHI);
                pipe_gvn.add_pass(SIMPLE_DCE);
                pipe_gvn.add_pass(CFG_SIMPLIFY);
                pipe_gvn.add_pass(BRANCH_CONDITION_SINK);

                pipe_gvn.add_pass(BRANCH2SELECT);
                pipe_gvn.add_pass(ELIM_CONSTANT_PHI);
                pipe_gvn.add_pass(SIMPLE_DCE);
                pipe_gvn.add_pass(CFG_SIMPLIFY);

                pipe_gvn.add_pass(GVN);
                pipe_gvn.add_pass(ELIM_CONSTANT_PHI);
                pipe_gvn.add_pass(SIMPLE_DCE);
                pipe_gvn.add_pass(CFG_SIMPLIFY);
            }

            let mut pipe_tco = Pipeline::default();
            {
                pipe_tco.add_pass(TCO);
                pipe_tco.add_pass(ELIM_CONSTANT_PHI);
                pipe_tco.add_pass(SIMPLE_DCE);
                pipe_tco.add_pass(CFG_SIMPLIFY);
            }

            let mut pipe_inline = Pipeline::default();
            {
                pipe_inline.add_pass(INLINE);
                pipe_inline.add_pass(ELIM_CONSTANT_PHI);
                pipe_inline.add_pass(SIMPLE_DCE);
                pipe_inline.add_pass(CFG_SIMPLIFY);
                pipe_inline.add_pass(GLOBAL_DCE);
            }

            let mut pipe_parallelize = Pipeline::default();
            {
                pipe_parallelize.add_pass(ELIM_CONSTANT_PHI);
                pipe_parallelize.add_pass(ADCE);
                pipe_parallelize.add_pass(CFG_SIMPLIFY);
                pipe_parallelize.add_pass(SIMPLE_DCE);
                pipe_parallelize.add_pass(AUTO_PARALLELIZE);
                pipe_parallelize.add_pass(CFG_SIMPLIFY);
                pipe_parallelize.add_pass(SIMPLE_DCE);
            }

            let mut pipe_unroll = Pipeline::default();
            {
                pipe_unroll.add_pass(LOOP_UNROLL);
                pipe_unroll.add_pass(CONSTANT_FOLDING);
                pipe_unroll.add_pass(ELIM_CONSTANT_PHI);
                pipe_unroll.add_pass(SIMPLE_DCE);
                pipe_unroll.add_pass(CFG_SIMPLIFY);
            }

            // initial pipelines, remove redundant code and simplify the control flow.
            {
                passman.run_pipeline(&mut ir, &pipe_basic, 32, 8);

                passman.run_pipeline(&mut ir, &pipe_tco, 32, 8);
                passman.run_pipeline(&mut ir, &pipe_basic, 32, 8);
            }

            // loop peeling to remove inefficient inner loop patterns.
            {
                // loop-peeling tend to eliminate the inner loops that will only be executed in
                // the first trip of outer loop.
                passman.run_transform(LOOP_PEEL, &mut ir, 1);
                // the control indvar of the inner loop will be simplified, and passed to the
                // original loop, as the new init.
                passman.run_transform(INDVAR_SIMPLIFY, &mut ir, 1);
                // there might be nested argument passing, we did not detect that in
                // `dead-loop-elim`, but just regard it as constant phi.
                passman.run_transform(ELIM_CONSTANT_PHI, &mut ir, 32);
                // remove redundant inner loops.
                passman.run_transform(DEAD_LOOP_ELIM, &mut ir, 1);
                // remove all redundant code.
                passman.run_pipeline(&mut ir, &pipe_basic, 32, 8);
                // remove unnecessary control flow.
                passman.run_transform(ADCE, &mut ir, 1);
                passman.run_pipeline(&mut ir, &pipe_basic, 32, 8);
                // licm, for partial impure function and operations.
                passman.run_transform(LOOP_INVARIANT_MOTION, &mut ir, 32);
                passman.run_pipeline(&mut ir, &pipe_basic, 32, 8);
            }

            // auto parallelize
            {
                // dump before parallelize
                // ir.alloc_all_names();
                // std::fs::write("./before.orzir", format!("{}", ir.display(true)))?;

                passman.run_pipeline(&mut ir, &pipe_parallelize, 32, 1);

                // dump after parallelize
                ir.alloc_all_names();
                // std::fs::write("./after.orzir", format!("{}",
                // ir.display(true)))?; passman.run_pipeline(&
                // mut ir, &pipe_basic, 32, 8);
            }

            // legalize to remove high level operations.
            {
                passman.run_transform(LEGALIZE, &mut ir, 1);
                passman.run_pipeline(&mut ir, &pipe_gvn, 32, 8);
            }

            // reduce the strength of operations with the loop, especially multiplication
            // with indvars.
            {
                // remove redundant induction variables.
                let iter = passman.run_transform(INDVAR_REDUCE, &mut ir, 32);
                println!("indvar-reduce iterations: {}", iter);
                passman.run_pipeline(&mut ir, &pipe_gvn, 32, 8);

                let iter = passman.run_transform(LOOP_STRENGTH_REDUCTION, &mut ir, 32);
                println!("loop strength reduction iterations: {}", iter);
                passman.run_pipeline(&mut ir, &pipe_gvn, 32, 8);
            }

            let iter = passman.run_pipeline(&mut ir, &pipe_inline, 32, 2);
            println!("pipeline inline iterations: {}", iter);

            let iter = passman.run_pipeline(&mut ir, &pipe_gvn, 32, 8);
            println!("pipeline gvn iterations: {}", iter);

            let iter = passman.run_pipeline(&mut ir, &pipe_unroll, 1, 1);
            println!("pipeline unroll iterations: {}", iter);

            // iterate several times, seeking more opportunities.
            for i in 0..4 {
                println!("Round {}", i);

                // aggressive dce is a little expensive, run once per round
                passman.run_transform(ADCE, &mut ir, 1);

                let iter = passman.run_pipeline(&mut ir, &pipe_gvn, 32, 8);
                println!("pipeline gvn iterations: {}", iter);

                let iter = passman.run_pipeline(&mut ir, &pipe_inline, 32, 8);
                println!("pipeline inline iterations: {}", iter);

                let iter = passman.run_pipeline(&mut ir, &pipe_gvn, 32, 8);
                println!("pipeline gvn iterations: {}", iter);
            }

            // optimize address generation inside loops.
            {
                // induce offset instructions that uses induction variables. This is placed here
                // because there can be complex alias problem after inducing the offset
                // instructions.
                passman.run_transform(INDVAR_OFFSET, &mut ir, 32);
                passman.run_pipeline(&mut ir, &pipe_gvn, 32, 8);

                // aggressive dce will remove redundant indvars after `indvar-offset`
                passman.run_transform(ADCE, &mut ir, 1);
                passman.run_pipeline(&mut ir, &pipe_gvn, 32, 8);
            }

            // reorder after loop unrolling.
            passman.run_transform(BLOCK_REORDER, &mut ir, 1);

            // TODO: refactor everything below.
            passman.run_transform(BOOL2COND, &mut ir, 32);
            passman.run_pipeline(&mut ir, &pipe_gvn, 32, 8);

            for i in 0..4 {
                println!("Second Round {}", i);

                passman.run_transform(ADVANCED_INSTCOMBINE, &mut ir, 32);

                let iter = passman.run_pipeline(&mut ir, &pipe_gvn, 32, 8);
                println!("pipeline gvn iterations: {}", iter);

                if cmd.aggressive {
                    passman.run_transform(AGGRESSIVE_INSTCOMBINE, &mut ir, 32);
                }

                passman.run_transform(ADCE, &mut ir, 1);

                let iter = passman.run_pipeline(&mut ir, &pipe_gvn, 32, 8);
                println!("pipeline gvn iterations: {}", iter);
            }

            // reorder
            passman.run_transform(STATIC_BRANCH_PREDICTION, &mut ir, 1);
            passman.run_transform(SIMPLE_DCE, &mut ir, 32);
            passman.run_transform(BRANCH_CONDITION_SINK, &mut ir, 1);
            passman.run_transform(BLOCK_REORDER, &mut ir, 1);
            passman.run_transform(SPLIT_CRITICAL_EDGE, &mut ir, 1);
            passman.run_transform(PH_BLOCK_LAYOUT, &mut ir, 1);
        } else {
            passman.run_transform(LEGALIZE, &mut ir, 1);
        }

        ir.alloc_all_names();

        if let Some(emit_ir) = &cmd.emit_ir {
            std::fs::write(emit_ir, format!("{}", ir.display(true)))?;
        }

        let mut lower_ctx: LowerContext<RvLowerSpec> =
            LowerContext::new(&ir, cmd.lower_cfg.clone());

        lower_ctx.mctx_mut().set_arch("rv64imafdc_zba_zbb");

        lower_ctx.lower();

        if cmd.opt > 0 {
            riscv64::run_peephole(lower_ctx.mctx_mut(), &cmd.lower_cfg, cmd.aggressive);
            SimplifyCfg::run(lower_ctx.mctx_mut(), &cmd.lower_cfg);
            RegisterCoalescing::run::<RvLowerSpec>(&mut lower_ctx, &cmd.lower_cfg);
            schedule(lower_ctx.mctx_mut(), &cmd.lower_cfg, Some(128));
        }

        if let Some(emit_vcode) = &cmd.emit_vcode {
            std::fs::write(emit_vcode, format!("{}", lower_ctx.mctx().display()))?;
        }

        if cmd.opt > 0 {
            RegisterCoalescing::run::<RvLowerSpec>(&mut lower_ctx, &cmd.lower_cfg);
        }

        lower_ctx.reg_alloc();
        lower_ctx.after_regalloc();

        if cmd.opt > 0 {
            // SimplifyCfg::tail_duplication(lower_ctx.mctx_mut(), &cmd.lower_cfg);
            riscv64::run_peephole_after_regalloc(lower_ctx.mctx_mut(), &cmd.lower_cfg);
            SimplifyCfg::ret_duplication(lower_ctx.mctx_mut(), &cmd.lower_cfg);
            schedule(lower_ctx.mctx_mut(), &cmd.lower_cfg, None);
        }

        let mctx = lower_ctx.finish();
        std::fs::write(cmd.output, format!("{}", mctx.display()))?;
    }

    Ok(())
}

fn register_passes(passman: &mut PassManager) {
    CfgCanonicalize::register(passman);
    CfgSimplify::register(passman);

    Mem2reg::register(passman);
    SimpleDce::register(passman);
    Adce::register(passman);

    ConstantFolding::register(passman);
    Instcombine::register(passman);
    AdvancedInstcombine::register(passman);
    AggressiveInstcombine::register(passman);

    ElimConstantPhi::register(passman);
    Branch2Select::register(passman);
    Bool2Cond::register(passman);

    Inline::register(passman);
    Tco::register(passman);

    Global2Local::register(passman);
    GlobalDce::register(passman);

    LoopUnroll::register(passman);
    LoopSimplify::register(passman);
    Lcssa::register(passman);
    LoopPeel::register(passman);
    IndvarSimplify::register(passman);
    DeadLoopElim::register(passman);
    LoopStrengthReduction::register(passman);
    IndvarOffset::register(passman);
    IndvarReduce::register(passman);
    LoopInvariantMotion::register(passman);

    GlobalValueNumbering::register(passman);
    Gcm::register(passman);
    BranchConditionSink::register(passman);
    StaticBranchPrediction::register(passman);
    PHBlockLayout::register(passman);

    Legalize::register(passman);
    BlockReorder::register(passman);
    SplitCriticalEdge::register(passman);
    AutoParallelize::register(passman);
}

fn cli(passman: &mut PassManager) -> Command {
    Command::new("compiler")
        .arg(
            Arg::new("output")
                .short('o')
                .required(true)
                .help("The output assembly"),
        )
        .arg(Arg::new("source").required(true).help("The source code"))
        .arg(
            Arg::new("s_flag")
                .short('S')
                .action(clap::ArgAction::Count)
                .help("Output an assembly file"),
        )
        .arg(
            Arg::new("opt")
                .short('O')
                .help("Optimization level")
                .default_value("0"),
        )
        .arg(
            Arg::new("emit-ast")
                .long("emit-ast")
                .help("Emit the AST to the specified file"),
        )
        .arg(
            Arg::new("emit-typed-ast")
                .long("emit-typed-ast")
                .help("Emit the typed AST to the specified file"),
        )
        .arg(
            Arg::new("emit-ir")
                .long("emit-ir")
                .help("Emit the IR to the specified file"),
        )
        .arg(
            Arg::new("emit-vcode")
                .long("emit-vcode")
                .help("Emit the virtual-register code to the specified file"),
        )
        .arg(
            Arg::new("no-combine-stack-adjustments")
                .long("no-combine-stack-adjustments")
                .action(clap::ArgAction::Count),
        )
        .arg(
            Arg::new("no-omit-frame-pointer")
                .long("no-omit-frame-pointer")
                .action(clap::ArgAction::Count),
        )
        .arg(
            Arg::new("aggressive")
                .long("aggressive")
                .action(clap::ArgAction::Count),
        )
        .args(passman.get_cli_args())
}

fn parse_args(passman: &mut PassManager) -> CliCommand {
    let matches = cli(passman).get_matches();
    let output = matches.get_one::<String>("output").unwrap().clone();
    let source = matches.get_one::<String>("source").unwrap().clone();
    let _s_flag = matches.get_count("s_flag") > 0; // just a placeholder
    let opt = matches
        .get_one::<String>("opt")
        .unwrap()
        .parse::<u8>()
        .unwrap();

    let emit_ast = matches.get_one::<String>("emit-ast").cloned();
    let emit_typed_ast = matches.get_one::<String>("emit-typed-ast").cloned();
    let emit_ir = matches.get_one::<String>("emit-ir").cloned();
    let emit_vcode = matches.get_one::<String>("emit-vcode").cloned();

    let omit_frame_pointer = matches.get_count("no-omit-frame-pointer") == 0;
    let combine_stack_adjustments = matches.get_count("no-combine-stack-adjustments") == 0;

    let mut passes = Vec::new();

    let transform_names = passman.gather_transform_names();
    let parameters = passman.gather_parameter_names();

    for (parameter_name, _) in parameters {
        let param = matches.get_one::<String>(&parameter_name);
        if let Some(param) = param {
            passman.set_parameter(&parameter_name, param.clone());
        }
    }

    for transform_name in transform_names {
        if matches.get_count(&transform_name) > 0 {
            passes.push(transform_name);
        }
    }

    let lower_cfg = LowerConfig {
        omit_frame_pointer,
        combine_stack_adjustments,
    };

    // let aggressive = matches.get_count("aggressive") > 0;
    let aggressive = true;

    CliCommand {
        output,
        source,
        emit_ast,
        emit_typed_ast,
        emit_ir,
        emit_vcode,
        opt,
        aggressive,
        lower_cfg,
    }
}
