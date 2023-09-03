/// Target dependent data layout
#[derive(Clone, Copy, Debug)]
pub struct DataLayout {
    /// Pointer size on the platform
    pub pointer_size: usize,
}
