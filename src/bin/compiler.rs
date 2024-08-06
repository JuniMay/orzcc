//! The compiler executable.

use clap::{Arg, Command};
use orzcc::{
    backend::{
        reg_alloc::reg_coalescing::RegisterCoalescing,
        riscv64,
        simplify_cfg::SimplifyCfg,
        LowerConfig,
    },
    ir::{
        passes::{
            adce::{Adce, ADCE},
            branch2select::{Branch2Select, BRANCH2SELECT},
            constant_phi::{ElimConstantPhi, ELIM_CONSTANT_PHI},
            control_flow::{
                BlockReorder,
                CfgCanonicalize,
                CfgSimplify,
                BLOCK_REORDER,
                CFG_SIMPLIFY,
            },
            fold::{ConstantFolding, CONSTANT_FOLDING},
            gcm::{Gcm, GCM},
            global2local::{Global2Local, GLOBAL2LOCAL},
            global_dce::{GlobalDce, GLOBAL_DCE},
            gvn::{GlobalValueNumbering, GVN},
            inline::{Inline, INLINE},
            instcombine::{InstCombine, INSTCOMBINE},
            legalize::{Legalize, LEGALIZE},
            loops::{Lcssa, LoopSimplify, LoopUnroll, LOOP_UNROLL},
            mem2reg::{Mem2reg, MEM2REG},
            simple_dce::{SimpleDce, SIMPLE_DCE},
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
            let mut pipe0 = Pipeline::default();

            pipe0.add_pass(GLOBAL2LOCAL);
            pipe0.add_pass(SIMPLE_DCE);
            pipe0.add_pass(GLOBAL_DCE);
            pipe0.add_pass(MEM2REG);
            pipe0.add_pass(SIMPLE_DCE);
            pipe0.add_pass(CFG_SIMPLIFY);
            pipe0.add_pass(CONSTANT_FOLDING);
            pipe0.add_pass(SIMPLE_DCE);
            pipe0.add_pass(INSTCOMBINE);
            pipe0.add_pass(SIMPLE_DCE);
            pipe0.add_pass(GCM);
            pipe0.add_pass(GVN);
            pipe0.add_pass(CFG_SIMPLIFY);
            pipe0.add_pass(ELIM_CONSTANT_PHI);
            pipe0.add_pass(SIMPLE_DCE);
            pipe0.add_pass(CFG_SIMPLIFY);
            pipe0.add_pass(BRANCH2SELECT);
            pipe0.add_pass(CFG_SIMPLIFY);

            let mut pipe1 = Pipeline::default();

            pipe1.add_pass(INLINE);
            pipe1.add_pass(CFG_SIMPLIFY);
            pipe1.add_pass(SIMPLE_DCE);
            pipe1.add_pass(GLOBAL_DCE);

            let mut pipe2 = Pipeline::default();

            pipe2.add_pass(LOOP_UNROLL);
            pipe2.add_pass(CONSTANT_FOLDING);
            pipe2.add_pass(CFG_SIMPLIFY);
            pipe2.add_pass(SIMPLE_DCE);

            passman.run_transform(GLOBAL2LOCAL, &mut ir, 32);
            passman.run_transform(SIMPLE_DCE, &mut ir, 32);
            passman.run_transform(GLOBAL_DCE, &mut ir, 32);
            passman.run_transform(MEM2REG, &mut ir, 32);
            passman.run_transform(SIMPLE_DCE, &mut ir, 32);
            passman.run_transform(CFG_SIMPLIFY, &mut ir, 32);
            passman.run_transform(CONSTANT_FOLDING, &mut ir, 32);
            passman.run_transform(SIMPLE_DCE, &mut ir, 32);
            passman.run_transform(INSTCOMBINE, &mut ir, 32);
            passman.run_transform(SIMPLE_DCE, &mut ir, 32);
            passman.run_transform(GCM, &mut ir, 32);
            passman.run_transform(LEGALIZE, &mut ir, 1);

            for i in 0..4 {
                println!("Round {}", i);

                let iter = passman.run_pipeline(&mut ir, &pipe0, 32, 8);
                println!("pipeline 0 iterations: {}", iter);

                let iter = passman.run_pipeline(&mut ir, &pipe1, 32, 8);
                println!("pipeline 1 iterations: {}", iter);

                let iter = passman.run_pipeline(&mut ir, &pipe2, 32, 8);
                println!("pipeline 2 iterations: {}", iter);

                passman.run_transform(ADCE, &mut ir, 1); // a little expensive, run once per round
                passman.run_transform(CFG_SIMPLIFY, &mut ir, 32);
            }

            passman.run_transform(BLOCK_REORDER, &mut ir, 1);
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
            riscv64::run_peephole(lower_ctx.mctx_mut(), &cmd.lower_cfg);
            SimplifyCfg::run(lower_ctx.mctx_mut(), &cmd.lower_cfg);
            RegisterCoalescing::run::<RvLowerSpec>(&mut lower_ctx, &cmd.lower_cfg);
        }

        if let Some(emit_vcode) = &cmd.emit_vcode {
            std::fs::write(emit_vcode, format!("{}", lower_ctx.mctx().display()))?;
        }

        lower_ctx.reg_alloc();
        lower_ctx.after_regalloc();

        if cmd.opt > 0 {
            // SimplifyCfg::tail_duplication(lower_ctx.mctx_mut(), &cmd.lower_cfg);
            riscv64::run_peephole_after_regalloc(lower_ctx.mctx_mut(), &cmd.lower_cfg);
            SimplifyCfg::ret_duplication(lower_ctx.mctx_mut(), &cmd.lower_cfg);
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
    InstCombine::register(passman);
    ElimConstantPhi::register(passman);
    Branch2Select::register(passman);

    Inline::register(passman);

    Global2Local::register(passman);
    GlobalDce::register(passman);

    LoopUnroll::register(passman);
    LoopSimplify::register(passman);
    Lcssa::register(passman);

    GlobalValueNumbering::register(passman);
    Gcm::register(passman);

    Legalize::register(passman);
    BlockReorder::register(passman);
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

    CliCommand {
        output,
        source,
        emit_ast,
        emit_typed_ast,
        emit_ir,
        emit_vcode,
        opt,
        lower_cfg,
    }
}
