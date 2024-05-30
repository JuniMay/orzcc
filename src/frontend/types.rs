use std::{cell::RefCell, cmp, collections::HashMap, fmt, hash, rc::Rc};

/// Kind of SysY types.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SysyTypeKind {
    /// Void
    Void,
    Bool,
    Int,
    /// Float    
    Float,
    /// Array
    ///
    /// Array is a type that represents a fixed number of elements of the same
    /// type.    
    Array(Option<usize>, SysyType),
    /// Function
    ///
    /// Function is a type that takes a list of types and returns a type.
    Function(Vec<SysyType>, SysyType),
}

impl fmt::Display for SysyTypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SysyTypeKind::Void => write!(f, "void"),
            SysyTypeKind::Bool => write!(f, "bool"),
            SysyTypeKind::Int => write!(f, "int"),
            SysyTypeKind::Float => write!(f, "float"),
            SysyTypeKind::Array(size, ty) => {
                let size_str = match size {
                    Some(s) => s.to_string(),
                    None => "_".to_string(), // use _ to represent unknown size
                };
                write!(f, "[{}; {}]", ty, size_str)
            }
            SysyTypeKind::Function(params, ret) => {
                write!(
                    f,
                    "({}) -> {}",
                    params
                        .iter()
                        .map(|ty| format!("{}", ty))
                        .collect::<Vec<_>>()
                        .join(", "),
                    ret
                )
            }
        }
    }
}

#[derive(Clone, Eq)]
pub struct SysyType(Rc<SysyTypeKind>);

impl fmt::Display for SysyType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{}", self.0) }
}

impl SysyType {
    thread_local! {
        /// Pool of all types.
        static POOL: RefCell<HashMap<SysyTypeKind, SysyType>> = RefCell::new(HashMap::new());
    }

    pub fn make(kind: SysyTypeKind) -> SysyType {
        Self::POOL.with(|pool| {
            let mut pool = pool.borrow_mut();
            if let Some(ty) = pool.get(&kind) {
                ty.clone()
            } else {
                let ty = SysyType(Rc::new(kind.clone()));
                pool.insert(kind, ty.clone());
                ty
            }
        })
    }

    pub fn int() -> SysyType { SysyType::make(SysyTypeKind::Int) }

    pub fn bool() -> SysyType { SysyType::make(SysyTypeKind::Bool) }

    pub fn void() -> SysyType { SysyType::make(SysyTypeKind::Void) }

    pub fn float() -> SysyType { SysyType::make(SysyTypeKind::Float) }

    pub fn array(size: Option<usize>, ty: SysyType) -> SysyType {
        SysyType::make(SysyTypeKind::Array(size, ty))
    }

    pub fn function(params: Vec<SysyType>, ret: SysyType) -> SysyType {
        SysyType::make(SysyTypeKind::Function(params, ret))
    }

    pub fn kind(&self) -> &SysyTypeKind { &self.0 }

    pub fn bytewidth(&self) -> usize {
        match self.kind() {
            SysyTypeKind::Void => 0,
            SysyTypeKind::Bool => 1,
            SysyTypeKind::Int => 4,
            SysyTypeKind::Float => 4,
            SysyTypeKind::Array(Some(size), ty) => size * ty.bytewidth(),
            SysyTypeKind::Array(None, _) => unreachable!(),
            SysyTypeKind::Function(_, _) => unreachable!(),
        }
    }

    pub fn bitwidth(&self) -> usize {
        match self.kind() {
            SysyTypeKind::Void => 0,
            SysyTypeKind::Bool => 8,
            SysyTypeKind::Int => 32,
            SysyTypeKind::Float => 32,
            SysyTypeKind::Array(Some(size), ty) => size * ty.bitwidth(),
            SysyTypeKind::Array(None, _) => unreachable!(),
            SysyTypeKind::Function(_, _) => unreachable!(),
        }
    }

    pub fn is_int(&self) -> bool { matches!(self.kind(), SysyTypeKind::Int) }

    pub fn is_float(&self) -> bool { matches!(self.kind(), SysyTypeKind::Float) }

    pub fn is_bool(&self) -> bool { matches!(self.kind(), SysyTypeKind::Bool) }

    pub fn is_numeric(&self) -> bool { self.is_int() || self.is_float() }

    pub fn is_void(&self) -> bool { matches!(self.kind(), SysyTypeKind::Void) }

    pub fn is_function(&self) -> bool { matches!(self.kind(), SysyTypeKind::Function(_, _)) }

    pub fn is_zero_initializable(&self) -> bool { self.is_numeric() || self.is_aggregate() }

    pub fn as_array(&self) -> Option<(Option<usize>, SysyType)> {
        match self.kind() {
            SysyTypeKind::Array(size, ty) => Some((*size, ty.clone())),
            _ => None,
        }
    }

    pub fn get_array_leaf(&self) -> SysyType {
        let mut ty = self.clone();
        while let Some((_, inner)) = ty.as_array() {
            ty = inner;
        }
        ty
    }

    pub fn as_function(&self) -> Option<(Vec<SysyType>, SysyType)> {
        match self.kind() {
            SysyTypeKind::Function(params, ret) => Some((params.clone(), ret.clone())),
            _ => None,
        }
    }

    pub fn is_aggregate(&self) -> bool { self.as_array().is_some() }
}

impl hash::Hash for SysyType {
    fn hash<H: hash::Hasher>(&self, state: &mut H) { self.0.hash(state); }
}

impl fmt::Debug for SysyType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{}", self.0) }
}

impl cmp::PartialEq for SysyType {
    fn eq(&self, other: &Self) -> bool { Rc::ptr_eq(&self.0, &other.0) }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", SysyType::int()), "i32");
        let ty = SysyType::array(Some(10), SysyType::int());
        assert_eq!(format!("{}", ty), "[i32; 10]");
        assert_eq!(format!("{}", SysyType::void()), "void");
        assert_eq!(format!("{}", SysyType::float()), "float");
        let ty = SysyType::function(vec![], SysyType::void());
        assert_eq!(format!("{}", ty), "() -> void");
        let ty = SysyType::function(vec![SysyType::int()], SysyType::void());
        assert_eq!(format!("{}", ty), "(i16) -> void");
        let ty = SysyType::function(vec![SysyType::int(), SysyType::int()], SysyType::void());
        assert_eq!(format!("{}", ty), "(i16, i32) -> void");
    }

    #[test]
    fn test_size() {
        assert_eq!(SysyType::void().bytewidth(), 0);
        assert_eq!(SysyType::bool().bytewidth(), 1);
        assert_eq!(SysyType::int().bytewidth(), 4);
        assert_eq!(SysyType::int().bytewidth(), 8);
        assert_eq!(SysyType::float().bytewidth(), 4);
        assert_eq!(SysyType::array(Some(10), SysyType::int()).bytewidth(), 40);
    }
}
