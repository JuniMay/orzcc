//! # Pass Manager
//!
//! The pass manager is a module that provides a consistent interface for
//! running and managing passes on a module.

use std::collections::HashMap;

use thiserror::Error;

use crate::ir::Context;

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
    fn run(&mut self, ctx: &mut Context) -> PassResult<Self::Output>;
}

/// The output of an analysis pass.
pub trait AnalysisOutput {}

/// The wrapper for an analysis output.
pub struct AnalysisOutputObj(Box<dyn AnalysisOutput>);

pub trait AnalysisPass: GlobalPass<Output = AnalysisOutputObj> {}

pub trait TransformPass: GlobalPassMut<Output = ()> {}

pub struct PassManager {
    analysis_passes: HashMap<&'static str, Box<dyn AnalysisPass>>,
    transform_passes: HashMap<&'static str, Box<dyn TransformPass>>,
    dependencies: HashMap<&'static str, Vec<&'static str>>,
    analysis_cache: HashMap<&'static str, AnalysisOutputObj>,
}
