//! # Mem2reg Pass for OrzIR
//!
//! This pass will transform the IR to SSA form by promoting memory to register.

use crate::ir::{entities::FunctionData, pass::LocalPassMut, values::Function};

pub enum Mem2regError {

}

pub struct Mem2reg {
    
}

impl Mem2reg {
    
}

impl LocalPassMut for Mem2reg {
    type Ok = ();
    type Err = Mem2regError;

    fn run(&mut self, function: Function, data: &mut FunctionData) -> Result<Self::Ok, Self::Err> {
        Ok(())
    }   
}