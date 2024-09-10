use brig_diagnostic::Result;
use brig_ir::Ir;

pub mod x86_linux;

pub trait CodeGenerator {
    fn new() -> Self
    where
        Self: Sized;

    fn process_graph(&mut self, graph: Ir) -> Result<()>;
}
