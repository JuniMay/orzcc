//! # Pass Manager
//!
//! The pass manager is a module that provides a consistent interface for
//! running and managing passes on a module.

use std::{collections::HashMap, str::FromStr};

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

pub trait TransformPass: GlobalPassMut<Output = ()> {
    fn register(passman: &mut PassManager)
    where
        Self: Sized;
}

#[derive(Default)]
pub struct PassManager {
    parameters: HashMap<String, String>,
    transforms: HashMap<String, Box<dyn TransformPass>>,
    deps: HashMap<String, Vec<Box<dyn TransformPass>>>,
}

#[derive(Default)]
pub struct Pipeline {
    passes: Vec<String>,
}

impl Pipeline {
    pub fn add_pass(&mut self, name: impl Into<String>) { self.passes.push(name.into()); }
}

impl PassManager {
    pub fn new() -> Self { Self::default() }

    pub fn add_parameter(&mut self, name: impl Into<String>) {
        self.parameters.insert(name.into(), String::new());
    }

    pub fn set_parameter<T: ToString>(&mut self, name: impl Into<String>, value: T) {
        if let Some(param) = self.parameters.get_mut(&name.into()) {
            *param = value.to_string();
        }
    }

    pub fn get_parameter<T: FromStr>(&self, name: impl AsRef<str>) -> Option<T> {
        self.parameters
            .get(name.as_ref())
            .and_then(|v| v.parse().ok())
    }

    pub fn register_transform<T: TransformPass + 'static>(
        &mut self,
        name: impl Into<String>,
        pass: T,
        deps: Vec<Box<dyn TransformPass>>,
    ) {
        let name = name.into();
        self.transforms.insert(name.clone(), Box::new(pass));
        self.deps.insert(name, deps);
    }

    pub fn gather_transform_names(&self) -> Vec<String> {
        let mut names: Vec<String> = self
            .transforms
            .keys()
            .map(|name| name.to_string())
            .collect();
        names.sort();
        names
    }

    pub fn gather_parameter_names(&self) -> Vec<String> {
        let mut names: Vec<String> = self
            .parameters
            .keys()
            .map(|name| name.to_string())
            .collect();
        names.sort();
        names
    }

    pub fn run_transform(
        &mut self,
        name: impl Into<String>,
        ctx: &mut Context,
        max_iter: usize,
    ) -> usize {
        let mut iter = 0;
        let name = name.into();
        for _ in 0..max_iter {
            iter += 1;
            let mut changed = false;
            for pass in self.deps.get_mut(&name).unwrap() {
                let (_, local_changed) = GlobalPassMut::run(pass.as_mut(), ctx).unwrap();
                changed |= local_changed;
            }
            let transform = self.transforms.get_mut(&name).unwrap();
            let (_, local_changed) = GlobalPassMut::run(transform.as_mut(), ctx).unwrap();
            changed |= local_changed;
            if !changed {
                break;
            }
        }
        iter
    }

    pub fn get_cli_args(&self) -> Vec<clap::Arg> {
        let mut args: Vec<clap::Arg> = self
            .gather_parameter_names()
            .into_iter()
            .map(|name| clap::Arg::new(&name).long(&name))
            .collect();

        args.extend(self.gather_transform_names().into_iter().map(|name| {
            clap::Arg::new(&name)
                .long(&name)
                .action(clap::ArgAction::Count)
        }));

        args
    }

    pub fn run_pipeline(
        &mut self,
        ctx: &mut Context,
        pipeline: &Pipeline,
        local_max_iter: usize,
        max_iter: usize,
    ) -> usize {
        let mut changed = true;
        let mut total_iter = 0;
        while changed {
            changed = false;
            for pass_name in &pipeline.passes {
                let iter = self.run_transform(pass_name, ctx, local_max_iter);
                total_iter += iter;
                if iter > 1 {
                    changed = true;
                }
            }

            if total_iter > max_iter {
                break;
            }
        }

        total_iter
    }
}
