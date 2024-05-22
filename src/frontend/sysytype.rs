use std::{cell::RefCell, cmp, collections::HashMap, fmt, hash, rc::Rc};


#[derive(Clone, Copy, Debug)]
pub struct DataLayout {
    pub pointer_size: usize,
}

impl Default for DataLayout {
    fn default() -> Self { Self::new() }
}

impl DataLayout {
    pub fn new() -> Self { Self { pointer_size: 8 } }
}

/// Kind of SysY types.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SyTypeKind {
    /// Void
    Void,
    /// Integer of arbitrary bits    
    Int(usize),
    /// Float    
    Float,
    /// Array
    ///
    /// Array is a type that represents a fixed number of elements of the same
    /// type.    
    Array(Option<usize>, SyType),
    /// Function
    ///
    /// Function is a type that takes a list of types and returns a type.
    Function(Vec<SyType>, SyType),
}

impl fmt::Display for SyTypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SyTypeKind::Void => write!(f, "void"),
            SyTypeKind::Int(bits) => write!(f, "i{}", bits),
            SyTypeKind::Float => write!(f, "float"),
            SyTypeKind::Array(size, ty) => {
                let size_str = match size {
                    Some(s) => s.to_string(),
                    None => "_".to_string(), // use _ to represent unknown size
                };
                write!(f, "[{}; {}]", ty, size_str)
            }
            SyTypeKind::Function(params, ret) => {
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
pub struct SyType(Rc<SyTypeKind>);

impl fmt::Display for SyType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{}", self.0) }
}

impl SyType {
    thread_local! {
        /// Pool of all types.
        static POOL: RefCell<HashMap<SyTypeKind, SyType>> = RefCell::new(HashMap::new());

        /// Pool of identified types.
        static IDENTIFIED_POOL: RefCell<HashMap<String, SyType>> = RefCell::new(HashMap::new());

        /// Data layout.
        static DATA_LAYOUT: RefCell<DataLayout> = RefCell::new(DataLayout::new());
    }

    pub fn make(kind: SyTypeKind) -> SyType {
        Self::POOL.with(|pool| {
            let mut pool = pool.borrow_mut();
            if let Some(ty) = pool.get(&kind) {
                ty.clone()
            } else {
                let ty = SyType(Rc::new(kind.clone()));
                pool.insert(kind, ty.clone());
                ty
            }
        })
    }

    pub fn int(bits: usize) -> SyType { SyType::make(SyTypeKind::Int(bits)) }

    pub fn i32_() -> SyType { SyType::int(32) }

    pub fn i1() -> SyType { SyType::int(1) }

    pub fn void() -> SyType { SyType::make(SyTypeKind::Void) }

    pub fn float() -> SyType { SyType::make(SyTypeKind::Float) }

    pub fn array(size: Option<usize>, ty: SyType) -> SyType { SyType::make(SyTypeKind::Array(size, ty)) }


    pub fn function(params: Vec<SyType>, ret: SyType) -> SyType {
        SyType::make(SyTypeKind::Function(params, ret))
    }

    pub fn kind(&self) -> &SyTypeKind { &self.0 }

    pub fn set_data_layout(data_layout: DataLayout) {
        Self::DATA_LAYOUT.with(|dl| {
            let mut dl = dl.borrow_mut();
            *dl = data_layout;
        });
    }

    pub fn bytewidth(&self) -> Option<usize> {
        match self.kind() {
            SyTypeKind::Void => Some(0),
            SyTypeKind::Int(bits) => Some(cmp::max(1, bits / 8)),
            SyTypeKind::Float => Some(4),
            SyTypeKind::Array(Some(size), ty) => Some(size * ty.bytewidth()?),
            SyTypeKind::Array(None, _) => None,
            SyTypeKind::Function(_, _) => Some(Self::DATA_LAYOUT.with(|data_layout| {
                let data_layout = data_layout.borrow();
                data_layout.pointer_size
            })),
        }
    }
    
    pub fn bitwidth(&self) -> Option<usize> {
        match self.kind() {
            SyTypeKind::Void => Some(0),
            SyTypeKind::Int(bits) => Some(*bits),
            SyTypeKind::Float => Some(32),
            SyTypeKind::Array(Some(size), ty) => Some(size * ty.bitwidth()?),
            SyTypeKind::Array(None, _) => None,
            SyTypeKind::Function(_, _) => Some(Self::DATA_LAYOUT.with(|data_layout| {
                let data_layout = data_layout.borrow();
                data_layout.pointer_size * 8
            })),
        }
    }
    

    pub fn is_int(&self) -> bool {
        matches!(self.kind(), SyTypeKind::Int(_))
    }

    pub fn is_float(&self) -> bool {
        matches!(self.kind(), SyTypeKind::Float)
    }

    pub fn is_i32(&self) -> bool { self == &SyType::i32_() }

    pub fn is_i1(&self) -> bool { self == &SyType::i1() }

    pub fn is_numeric(&self) -> bool { self.is_int() || self.is_float() }

    pub fn is_void(&self) -> bool {
        matches!(self.kind(), SyTypeKind::Void)
    }

    pub fn is_function(&self) -> bool {
        matches!(self.kind(), SyTypeKind::Function(_, _))
    }

    pub fn is_zero_initializable(&self) -> bool {
        self.is_numeric() || self.is_aggregate()
    }

    pub fn as_array(&self) -> Option<(Option<usize>, SyType)> {
        match self.kind() {
            SyTypeKind::Array(size, ty) => Some((size.clone(), ty.clone())),
            _ => None,
        }
    }    

    pub fn get_array_leaf(&self) -> SyType {
        let mut ty = self.clone();
        while let Some((_, inner)) = ty.as_array() {
            ty = inner;
        }
        ty
    }

    pub fn as_function(&self) -> Option<(Vec<SyType>, SyType)> {
        match self.kind() {
            SyTypeKind::Function(params, ret) => Some((params.clone(), ret.clone())),
            _ => None,
        }
    }

    pub fn is_aggregate(&self) -> bool { self.as_array().is_some() }
}

impl hash::Hash for SyType {
    fn hash<H: hash::Hasher>(&self, state: &mut H) { self.0.hash(state); }
}

impl fmt::Debug for SyType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{}", self.0) }
}

impl cmp::PartialEq for SyType {
    fn eq(&self, other: &Self) -> bool { Rc::ptr_eq(&self.0, &other.0) }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_equality() {
        assert_eq!(SyType::i32_(), SyType::int(32));
        assert_eq!(SyType::i1(), SyType::int(1));
        assert_ne!(SyType::i32_(), SyType::int(64));

        assert!(SyType::i32_().is_int());
        assert!(SyType::float().is_float());
        assert!(!SyType::i32_().is_float());

        assert_eq!(
            SyType::array(Some(10), SyType::int(32)),
            SyType::array(Some(10), SyType::int(32))
        );
        assert_ne!(SyType::int(32), SyType::int(64));
        assert_ne!(
            SyType::array(Some(10), SyType::int(32)),
            SyType::array(Some(20), SyType::int(32))
        );
        assert_eq!(SyType::void(), SyType::void());
        assert_eq!(SyType::float(), SyType::float());
        assert_eq!(
            SyType::function(vec![], SyType::void()),
            SyType::function(vec![], SyType::void())
        );
        assert_ne!(
            SyType::function(vec![SyType::int(16)], SyType::void()),
            SyType::function(vec![], SyType::int(32))
        );
    }

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", SyType::int(32)), "i32");
        let ty = SyType::array(Some(10), SyType::int(32));
        assert_eq!(format!("{}", ty), "[i32; 10]");
        assert_eq!(format!("{}", SyType::void()), "void");
        assert_eq!(format!("{}", SyType::float()), "float");
        let ty = SyType::function(vec![], SyType::void());
        assert_eq!(format!("{}", ty), "() -> void");
        let ty = SyType::function(vec![SyType::int(16)], SyType::void());
        assert_eq!(format!("{}", ty), "(i16) -> void");
        let ty = SyType::function(vec![SyType::int(16), SyType::int(32)], SyType::void());
        assert_eq!(format!("{}", ty), "(i16, i32) -> void");
    }

    #[test]
    fn test_size() {
        assert_eq!(SyType::void().bytewidth(), Some(0));
        assert_eq!(SyType::int(1).bytewidth(), Some(1));
        assert_eq!(SyType::int(32).bytewidth(), Some(4));
        assert_eq!(SyType::int(64).bytewidth(), Some(8));
        assert_eq!(SyType::float().bytewidth(), Some(4));
        assert_eq!(SyType::array(Some(10), SyType::int(32)).bytewidth(), Some(40));
        let ty = SyType::function(vec![], SyType::void());
        assert_eq!(ty.bytewidth(), Some(8));
        let ty = SyType::function(vec![SyType::int(32)], SyType::void());
        assert_eq!(ty.bytewidth(), Some(8));
        let ty = SyType::function(vec![SyType::int(32), SyType::int(64)], SyType::void());
        assert_eq!(ty.bytewidth(), Some(8));
    }
}