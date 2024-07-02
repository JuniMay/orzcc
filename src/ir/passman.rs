//! # Pass Manager
//!
//! The pass manager is a module that provides a consistent interface for
//! running and managing passes on a module.

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    str::FromStr,
};

use thiserror::Error;

use super::{Context, Func};

#[derive(Debug, Error)]
pub enum PassErrorKind {
    #[error("analysis error")]
    AnalysisError,

    #[error("transform error")]
    TransformError,

    #[error("other error")]
    Other,
}

#[derive(Debug, Error)]
#[error("{kind} on {pass_name}: {err}")]
pub struct PassError {
    kind: PassErrorKind,
    err: Box<dyn std::error::Error>,
    pass_name: String,
}

pub type PassResult<T> = Result<T, PassError>;

impl PassError {
    pub fn analysis_error(pass_name: impl Into<String>, err: Box<dyn std::error::Error>) -> Self {
        Self {
            kind: PassErrorKind::AnalysisError,
            err,
            pass_name: pass_name.into(),
        }
    }

    pub fn transform_error(pass_name: impl Into<String>, err: Box<dyn std::error::Error>) -> Self {
        Self {
            kind: PassErrorKind::TransformError,
            err,
            pass_name: pass_name.into(),
        }
    }

    pub fn other(pass_name: impl Into<String>, err: Box<dyn std::error::Error>) -> Self {
        Self {
            kind: PassErrorKind::Other,
            err,
            pass_name: pass_name.into(),
        }
    }
}

/// A pass that can be run on a function.
pub trait LocalPass {
    /// The output of the pass.
    type Output;

    /// Run the pass on the given function.
    fn run(&mut self, ctx: &Context, func: Func) -> PassResult<Self::Output>;
}

/// A pass that can be run on a function and modify it.
pub trait LocalPassMut {
    /// The output of the pass.
    type Output;

    /// Run the pass on the given function and maybe modify it.
    ///
    /// # Returns
    ///
    /// A tuple of the output of the pass and a boolean indicating whether the
    /// IR has been modified.
    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)>;
}

/// A pass that can be run on a Context.
pub trait GlobalPass {
    /// The output of the pass.
    type Output;

    /// Run the pass on the given Context.
    fn run(&mut self, ctx: &Context) -> PassResult<Self::Output>;
}

/// A pass that can be run on a Context and modify it.
pub trait GlobalPassMut {
    /// The output of the pass.
    type Output;

    /// Run the pass on the given Context and maybe modify it.
    ///
    /// # Returns
    ///
    /// A tuple of the output of the pass and a boolean indicating whether the
    /// IR has been modified.
    fn run(&mut self, ctx: &mut Context) -> PassResult<(Self::Output, bool)>;
}

/// The output of an analysis pass.
pub trait AnalysisOutput {}

/// The wrapper for an analysis output.
pub struct AnalysisOutputObj(pub Box<dyn AnalysisOutput>);

pub trait AnalysisPass: GlobalPass<Output = AnalysisOutputObj> {}

pub trait TransformPass: GlobalPassMut<Output = ()> {}

pub struct PassManager {
    parameters: HashMap<&'static str, String>,
    transform_passes: HashMap<&'static str, Box<dyn TransformPass>>,
    dependencies: HashMap<&'static str, Vec<Box<dyn TransformPass>>>,
}

impl PassManager {
    thread_local! {
        static PM: RefCell<PassManager> = RefCell::new(PassManager::new());
    }

    fn new() -> Self {
        Self {
            parameters: HashMap::new(),
            transform_passes: HashMap::new(),
            dependencies: HashMap::new(),
        }
    }

    pub fn add_parameter(name: &'static str) {
        Self::PM.with(|pm| {
            let mut pm = pm.borrow_mut();
            pm.parameters.insert(name, String::new());
        });
    }

    pub fn set_parameter(name: &str, value: String) {
        Self::PM.with(|pm| {
            let mut pm = pm.borrow_mut();
            if let Some(param) = pm.parameters.get_mut(name) {
                *param = value;
            }
        });
    }

    pub fn get_parameter<T: FromStr>(name: &str) -> Option<T> {
        Self::PM.with(|pm| {
            let pm = pm.borrow();
            pm.parameters.get(name).and_then(|v| v.parse().ok())
        })
    }

    pub fn get_cli_args() -> Vec<clap::Arg> {
        Self::PM.with(|pm| {
            let pm = pm.borrow();
            let mut args = pm
                .parameters
                .keys()
                .map(|name| clap::Arg::new(name).long(name))
                .collect::<Vec<_>>();

            for transform in pm.transform_passes.keys() {
                args.push(
                    clap::Arg::new(transform)
                        .long(transform)
                        .action(clap::ArgAction::Count),
                );
            }

            args
        })
    }

    pub fn get_transform_names() -> HashSet<String> {
        Self::PM.with(|pm| {
            let pm = pm.borrow();
            pm.transform_passes
                .keys()
                .copied()
                .map(String::from)
                .collect()
        })
    }

    pub fn get_parameter_names() -> HashSet<String> {
        Self::PM.with(|pm| {
            let pm = pm.borrow();
            pm.parameters.keys().copied().map(String::from).collect()
        })
    }

    pub fn register_transform(
        name: &'static str,
        pass: Box<dyn TransformPass>,
        dependencies: Vec<Box<dyn TransformPass>>,
    ) {
        Self::PM.with(|pm| {
            let mut pm = pm.borrow_mut();
            pm.transform_passes.insert(name, pass);
            pm.dependencies.insert(name, dependencies);
        });
    }

    pub fn run_transform(name: &str, ctx: &mut Context, max_iter: usize) -> usize {
        Self::PM.with(|pm| {
            let mut pm = pm.borrow_mut();
            let mut iter = 0;
            for _ in 0..max_iter {
                iter += 1;
                let mut changed = false;
                for pass in pm.dependencies.get_mut(name).unwrap() {
                    let (_, local_changed) = GlobalPassMut::run(pass.as_mut(), ctx).unwrap();
                    changed |= local_changed;
                }
                pm.transform_passes.get_mut(name).unwrap();
                let transform = pm.transform_passes.get_mut(name).unwrap();
                let (_, local_changed) = GlobalPassMut::run(transform.as_mut(), ctx).unwrap();
                changed |= local_changed;
                if !changed {
                    break;
                }
            }
            iter
        })
    }
}
