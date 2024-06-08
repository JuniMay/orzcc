use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    error::Error,
    str::FromStr,
};

use thiserror::Error;

use super::{entities::FunctionData, module::Module, values::Function};

pub mod control_flow_analysis;
pub mod control_flow_canonicalization;
pub mod data_flow_analysis;
pub mod dominance_analysis;
pub mod mem2reg;
pub mod printer;
pub mod straighten;
pub mod unreachable_block_elimination;

pub trait GlobalPass {
    type Ok;

    fn run_on_module(&mut self, module: &Module) -> PassResult<Self::Ok>;
}

pub trait LocalPass {
    type Ok;

    fn run_on_function(&mut self, function: Function, data: &FunctionData) -> PassResult<Self::Ok>;
}

pub trait LocalPassMut {
    type Ok;

    /// Run the pass on function, return the payload and whether the program has
    /// changed.
    fn run_on_function(
        &mut self,
        function: Function,
        data: &mut FunctionData,
    ) -> PassResult<(Self::Ok, bool)>;
}

pub trait GlobalPassMut {
    type Ok;

    /// Run the pass on module, return the payload and whether the program has
    /// changed.
    fn run_on_module(&mut self, module: &mut Module) -> PassResult<(Self::Ok, bool)>;
}

#[derive(Debug, Error)]
pub enum PassErrorKind {
    #[error("analysis error")]
    AnalysisError,

    #[error("perparation error")]
    PreparationError,

    #[error("transformation error")]
    TransformationError,

    #[error("other error")]
    Other,
}

#[derive(Debug, Error)]
#[error("{kind} on {name}: {err}")]
pub struct PassError {
    kind: PassErrorKind,
    err: Box<dyn Error>,
    name: String,
}

type PassResult<T> = Result<T, PassError>;

impl PassError {
    pub fn analysis_error(name: String, err: Box<dyn Error>) -> Self {
        Self {
            kind: PassErrorKind::AnalysisError,
            err,
            name,
        }
    }

    pub fn preparation_error(name: String, err: Box<dyn Error>) -> Self {
        Self {
            kind: PassErrorKind::PreparationError,
            err,
            name,
        }
    }

    pub fn transformation_error(name: String, err: Box<dyn Error>) -> Self {
        Self {
            kind: PassErrorKind::TransformationError,
            err,
            name,
        }
    }

    pub fn other(name: String, err: Box<dyn Error>) -> Self {
        Self {
            kind: PassErrorKind::Other,
            err,
            name,
        }
    }
}

pub trait TransformationPass: GlobalPassMut<Ok = ()> {
    fn reset(&mut self);
}

type TransformationPassObj = Box<dyn TransformationPass>;

pub struct PassManager {
    parameters: HashMap<&'static str, String>,
    transformations: HashMap<&'static str, TransformationPassObj>,
    dependencies: HashMap<&'static str, Vec<TransformationPassObj>>,
}

impl PassManager {
    thread_local! {
        static PASS_MANAGER: RefCell<PassManager> = RefCell::new(PassManager::new());
    }

    fn new() -> Self {
        Self {
            parameters: HashMap::new(),
            transformations: HashMap::new(),
            dependencies: HashMap::new(),
        }
    }

    pub fn add_parameter(name: &'static str) {
        Self::PASS_MANAGER.with(|pm| {
            let mut pm = pm.borrow_mut();
            pm.parameters.insert(name, String::new());
        });
    }

    pub fn set_parameter(name: &str, value: String) {
        Self::PASS_MANAGER.with(|pm| {
            let mut pm = pm.borrow_mut();
            if let Some(param) = pm.parameters.get_mut(name) {
                *param = value;
            }
        });
    }

    pub fn get_parameter<T: FromStr>(name: &str) -> Option<T> {
        Self::PASS_MANAGER.with(|pm| {
            let pm = pm.borrow();
            pm.parameters.get(name).and_then(|v| v.parse().ok())
        })
    }

    pub fn get_cli_args() -> Vec<clap::Arg> {
        Self::PASS_MANAGER.with(|pm| {
            let pm = pm.borrow();
            let mut args = pm
                .parameters
                .iter()
                .map(|(name, _)| clap::Arg::new(name).long(name))
                .collect::<Vec<_>>();

            for transformation in pm.transformations.keys() {
                args.push(
                    clap::Arg::new(transformation)
                        .long(transformation)
                        .action(clap::ArgAction::Count),
                );
            }

            args
        })
    }

    pub fn register_transformation(
        name: &'static str,
        pass: TransformationPassObj,
        dependencies: Vec<TransformationPassObj>,
    ) {
        Self::PASS_MANAGER.with(|pm| {
            let mut pm = pm.borrow_mut();
            pm.transformations.insert(name, pass);
            pm.dependencies.insert(name, dependencies);
        });
    }

    pub fn get_transformation_names() -> HashSet<String> {
        Self::PASS_MANAGER.with(|pm| {
            let pm = pm.borrow();
            pm.transformations
                .keys()
                .copied()
                .map(String::from)
                .collect()
        })
    }

    pub fn get_parameter_names() -> HashSet<String> {
        Self::PASS_MANAGER.with(|pm| {
            let pm = pm.borrow();
            pm.parameters.keys().copied().map(String::from).collect()
        })
    }

    pub fn run_transformation(name: &str, module: &mut Module, max_iter: usize) -> usize {
        Self::PASS_MANAGER.with(|pm| {
            let mut pm = pm.borrow_mut();
            let mut iter = 0;
            for _ in 0..max_iter {
                iter += 1;
                let mut changed = false;
                for pass in pm.dependencies.get_mut(name).unwrap() {
                    pass.reset();
                    let (_, has_changed) = pass.run_on_module(module).unwrap();
                    changed |= has_changed;
                }
                pm.transformations.get_mut(name).unwrap().reset();
                let (_, has_changed) = pm
                    .transformations
                    .get_mut(name)
                    .unwrap()
                    .run_on_module(module)
                    .unwrap();
                changed |= has_changed;
                if !changed {
                    break;
                }
            }
            iter
        })
    }
}
