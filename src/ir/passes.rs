use std::error::Error;

use thiserror::Error;

use super::{entities::FunctionData, module::Module, values::Function};

pub mod control_flow_analysis;
pub mod control_flow_normalization;
pub mod data_flow_analysis;
pub mod dominance_analysis;
pub mod mem2reg;
pub mod printer;
pub mod unreachable_block_elimination;

pub trait GlobalPass {
    type Ok;

    fn run(&mut self, module: &Module) -> PassResult<Self::Ok>;
}

pub trait LocalPass {
    type Ok;

    fn run(&mut self, function: Function, data: &FunctionData) -> PassResult<Self::Ok>;
}

pub trait LocalPassMut {
    type Ok;

    fn run(&mut self, function: Function, data: &mut FunctionData) -> PassResult<Self::Ok>;
}

pub trait GlobalPassMut {
    type Ok;

    fn run(&mut self, module: &mut Module) -> PassResult<Self::Ok>;
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

pub struct PassManager {}
