//! # Pass Manager
//!
//! The pass manager is a module that provides a consistent interface for
//! running and managing passes on a module.

use std::collections::HashMap;

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
    pub fn analysis_error(pass_name: String, err: Box<dyn std::error::Error>) -> Self {
        Self {
            kind: PassErrorKind::AnalysisError,
            err,
            pass_name,
        }
    }

    pub fn transform_error(pass_name: String, err: Box<dyn std::error::Error>) -> Self {
        Self {
            kind: PassErrorKind::TransformError,
            err,
            pass_name,
        }
    }

    pub fn other(pass_name: String, err: Box<dyn std::error::Error>) -> Self {
        Self {
            kind: PassErrorKind::Other,
            err,
            pass_name,
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
    _analysis_passes: HashMap<&'static str, Box<dyn AnalysisPass>>,
    _transform_passes: HashMap<&'static str, Box<dyn TransformPass>>,
    _dependencies: HashMap<&'static str, Vec<&'static str>>,
    _analysis_cache: HashMap<&'static str, AnalysisOutputObj>,
}
