pub enum ComptimeVal {
    Bool(bool),
    Int(i32),
    Float(f32),
    List(Vec<ComptimeVal>),
}
