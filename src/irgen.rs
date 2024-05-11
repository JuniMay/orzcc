use crate::{frontend::CompUnit, ir::module::Module};

pub struct IrGenContext {
    pub compunit: CompUnit,
    pub module: Module,
}

impl IrGenContext {
    pub fn new(compunit: CompUnit, module_name: impl Into<String>) -> Self {
        let module = Module::new(module_name.into());
        Self { compunit, module }
    }

    pub fn irgen(&mut self) { todo!() }

    /// Finish the irgen process and return the module.
    pub fn finish(self) -> Module { self.module }
}
