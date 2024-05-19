use std::{cell::RefCell, cmp, collections::HashMap, fmt, hash, rc::Rc};

use super::TYPE_PREFIX;

/// Target dependent data layout
#[derive(Clone, Copy, Debug)]
pub struct DataLayout {
    /// Pointer size on the platform
    pub pointer_size: usize,
}

impl Default for DataLayout {
    fn default() -> Self { Self::new() }
}

impl DataLayout {
    pub fn new() -> Self { Self { pointer_size: 8 } }
}

/// Kind of types.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    /// Void
    Void,
    /// Integer of arbitrary bits
    Int(usize),
    /// Half
    Half,
    /// Float
    Float,
    /// Double
    Double,
    /// Pointer
    Ptr,
    /// Array
    ///
    /// Array is a type that represents a fixed number of elements of the same
    /// type.
    Array(usize, Type),
    /// Function
    ///
    /// Function is a type that takes a list of types and returns a type.
    Function(Vec<Type>, Type),
    /// Struct
    ///
    /// Struct is basically a collection of types.
    Struct(Vec<Type>),
    /// An identified type
    Identified(String),
}

impl fmt::Display for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeKind::Void => write!(f, "void"),
            TypeKind::Int(bits) => write!(f, "i{}", bits),
            TypeKind::Half => write!(f, "half"),
            TypeKind::Float => write!(f, "float"),
            TypeKind::Double => write!(f, "double"),
            TypeKind::Ptr => write!(f, "ptr"),
            TypeKind::Array(size, ty) => write!(f, "[{}; {}]", ty, size),
            TypeKind::Function(params, ret) => {
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
            TypeKind::Struct(fields) => {
                write!(
                    f,
                    "{{ {} }}",
                    fields
                        .iter()
                        .map(|ty| format!("{}", ty))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            TypeKind::Identified(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Clone, Eq)]
pub struct Type(Rc<TypeKind>);

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{}", self.0) }
}

impl Type {
    thread_local! {
        /// Pool of all types.
        static POOL: RefCell<HashMap<TypeKind, Type>> = RefCell::new(HashMap::new());

        /// Pool of identified types.
        static IDENTIFIED_POOL: RefCell<HashMap<String, Type>> = RefCell::new(HashMap::new());

        /// Data layout.
        static DATA_LAYOUT: RefCell<DataLayout> = RefCell::new(DataLayout::new());
    }

    pub fn make(kind: TypeKind) -> Type {
        Self::POOL.with(|pool| {
            let mut pool = pool.borrow_mut();
            if let Some(ty) = pool.get(&kind) {
                ty.clone()
            } else {
                let ty = Type(Rc::new(kind.clone()));
                pool.insert(kind, ty.clone());
                ty
            }
        })
    }

    pub fn int(bits: usize) -> Type { Type::make(TypeKind::Int(bits)) }

    pub fn i32_() -> Type { Type::int(32) }

    pub fn i1() -> Type { Type::int(1) }

    pub fn void() -> Type { Type::make(TypeKind::Void) }

    pub fn half() -> Type { Type::make(TypeKind::Half) }

    pub fn float() -> Type { Type::make(TypeKind::Float) }

    pub fn double() -> Type { Type::make(TypeKind::Double) }

    pub fn ptr() -> Type { Type::make(TypeKind::Ptr) }

    pub fn array(size: usize, ty: Type) -> Type { Type::make(TypeKind::Array(size, ty)) }

    pub fn function(params: Vec<Type>, ret: Type) -> Type {
        Type::make(TypeKind::Function(params, ret))
    }

    pub fn struct_(fields: Vec<Type>) -> Type { Type::make(TypeKind::Struct(fields)) }

    pub fn identified(name: String) -> Type {
        let name = if !name.starts_with(TYPE_PREFIX) {
            format!("{}{}", TYPE_PREFIX, name)
        } else {
            name
        };
        Type::make(TypeKind::Identified(name))
    }

    pub fn set_identified(name: String, ty: Type) {
        Self::IDENTIFIED_POOL.with(|pool| {
            let mut pool = pool.borrow_mut();
            pool.insert(name, ty);
        });
    }

    pub fn get_identified(name: &str) -> Option<Type> {
        Self::IDENTIFIED_POOL.with(|pool| {
            let pool = pool.borrow();
            pool.get(name).cloned()
        })
    }

    pub fn kind(&self) -> &TypeKind { &self.0 }

    pub fn set_data_layout(data_layout: DataLayout) {
        Self::DATA_LAYOUT.with(|dl| {
            let mut dl = dl.borrow_mut();
            *dl = data_layout;
        });
    }

    pub fn bytewidth(&self) -> usize {
        Self::DATA_LAYOUT.with(|data_layout| {
            let data_layout = data_layout.borrow();
            match self.kind() {
                TypeKind::Void => 0,
                TypeKind::Int(bits) => cmp::max(1, bits / 8),
                TypeKind::Half => 2,
                TypeKind::Float => 4,
                TypeKind::Double => 8,
                TypeKind::Ptr => data_layout.pointer_size,
                TypeKind::Array(size, ty) => size * ty.bytewidth(),
                TypeKind::Function(_, _) => data_layout.pointer_size,
                TypeKind::Struct(fields) => fields.iter().map(|ty| ty.bytewidth()).sum(),
                TypeKind::Identified(_) => {
                    if let Some(ty) = Self::get_identified(self.to_string().as_str()) {
                        ty.bytewidth()
                    } else {
                        0
                    }
                }
            }
        })
    }

    pub fn bitwidth(&self) -> usize {
        match self.kind() {
            TypeKind::Void => 0,
            TypeKind::Int(bits) => *bits,
            TypeKind::Half => 16,
            TypeKind::Float => 32,
            TypeKind::Double => 64,
            TypeKind::Ptr => Self::DATA_LAYOUT.with(|data_layout| {
                let data_layout = data_layout.borrow();
                data_layout.pointer_size * 8
            }),
            TypeKind::Array(size, ty) => size * ty.bitwidth(),
            TypeKind::Function(_, _) => Self::DATA_LAYOUT.with(|data_layout| {
                let data_layout = data_layout.borrow();
                data_layout.pointer_size * 8
            }),
            TypeKind::Struct(fields) => fields.iter().map(|ty| ty.bitwidth()).sum(),
            TypeKind::Identified(_) => {
                if let Some(ty) = Self::get_identified(self.to_string().as_str()) {
                    ty.bitwidth()
                } else {
                    0
                }
            }
        }
    }

    pub fn is_int(&self) -> bool {
        match self.kind() {
            TypeKind::Int(_) => true,
            TypeKind::Identified(name) => {
                if let Some(ty) = Self::get_identified(name) {
                    ty.is_int()
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self.kind() {
            TypeKind::Half | TypeKind::Float | TypeKind::Double => true,
            TypeKind::Identified(name) => {
                if let Some(ty) = Self::get_identified(name) {
                    ty.is_float()
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn is_i32(&self) -> bool { self == &Type::i32_() }

    pub fn is_i1(&self) -> bool { self == &Type::i1() }

    pub fn is_numeric(&self) -> bool { self.is_int() || self.is_float() }

    pub fn is_ptr(&self) -> bool {
        match self.kind() {
            TypeKind::Ptr => true,
            TypeKind::Identified(name) => {
                if let Some(ty) = Self::get_identified(name) {
                    ty.is_ptr()
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn is_void(&self) -> bool {
        match self.kind() {
            TypeKind::Void => true,
            TypeKind::Identified(name) => {
                if let Some(ty) = Self::get_identified(name) {
                    ty.is_void()
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn is_function(&self) -> bool {
        match self.kind() {
            TypeKind::Function(_, _) => true,
            TypeKind::Identified(name) => {
                if let Some(ty) = Self::get_identified(name) {
                    ty.is_function()
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn is_zero_initializable(&self) -> bool {
        self.is_numeric() || self.is_ptr() || self.is_aggregate()
    }

    pub fn as_array(&self) -> Option<(usize, Type)> {
        match self.kind() {
            TypeKind::Array(size, ty) => Some((*size, ty.clone())),
            TypeKind::Identified(name) => {
                if let Some(ty) = Self::get_identified(name) {
                    ty.as_array()
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn get_array_leaf(&self) -> Type {
        let mut ty = self.clone();
        while let Some((_, inner)) = ty.as_array() {
            ty = inner;
        }
        ty
    }

    pub fn as_function(&self) -> Option<(Vec<Type>, Type)> {
        match self.kind() {
            TypeKind::Function(params, ret) => Some((params.clone(), ret.clone())),
            TypeKind::Identified(name) => {
                if let Some(ty) = Self::get_identified(name) {
                    ty.as_function()
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Convert the type to a struct type.
    ///
    /// If the type is not a struct type, return `None`.
    /// Note that for identified types, this function returns `None` for now.
    pub fn as_struct(&self) -> Option<Vec<Type>> {
        match self.kind() {
            TypeKind::Struct(fields) => Some(fields.clone()),
            TypeKind::Identified(name) => {
                if let Some(ty) = Self::get_identified(name) {
                    ty.as_struct()
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn is_aggregate(&self) -> bool { self.as_array().is_some() || self.as_struct().is_some() }
}

impl hash::Hash for Type {
    fn hash<H: hash::Hasher>(&self, state: &mut H) { self.0.hash(state); }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{}", self.0) }
}

impl cmp::PartialEq for Type {
    fn eq(&self, other: &Self) -> bool { Rc::ptr_eq(&self.0, &other.0) }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_equality() {
        assert_eq!(Type::i32_(), Type::int(32));
        assert_eq!(Type::i1(), Type::int(1));
        assert_ne!(Type::i32_(), Type::int(64));

        assert!(Type::i32_().is_int());
        assert!(Type::float().is_float());
        assert!(!Type::i32_().is_float());
        assert!(Type::ptr().is_ptr());

        assert_eq!(
            Type::array(10, Type::int(32)),
            Type::array(10, Type::int(32))
        );
        assert_ne!(Type::int(32), Type::int(64));
        assert_ne!(
            Type::array(10, Type::int(32)),
            Type::array(20, Type::int(32))
        );
        assert_eq!(Type::void(), Type::void());
        assert_eq!(Type::half(), Type::half());
        assert_eq!(Type::float(), Type::float());
        assert_eq!(Type::double(), Type::double());
        assert_eq!(Type::ptr(), Type::ptr());
        assert_eq!(
            Type::identified("name".to_string()),
            Type::identified("name".to_string())
        );
        assert_ne!(
            Type::identified("name".to_string()),
            Type::identified("name2".to_string())
        );
        assert_eq!(
            Type::function(vec![], Type::void()),
            Type::function(vec![], Type::void())
        );
        assert_ne!(
            Type::function(vec![Type::int(16)], Type::void()),
            Type::function(vec![], Type::int(32))
        );
        let ty1 = Type::struct_(vec![Type::int(32), Type::int(64)]);
        let ty2 = Type::struct_(vec![Type::int(32), Type::int(64)]);
        assert!(ty1.as_struct().is_some());
        assert_eq!(ty1, ty2);
    }

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", Type::int(32)), "i32");
        let ty = Type::array(10, Type::int(32));
        assert_eq!(format!("{}", ty), "[i32; 10]");
        assert_eq!(format!("{}", Type::void()), "void");
        assert_eq!(format!("{}", Type::half()), "half");
        assert_eq!(format!("{}", Type::float()), "float");
        assert_eq!(format!("{}", Type::double()), "double");
        assert_eq!(format!("{}", Type::ptr()), "ptr");
        assert_eq!(
            format!("{}", Type::identified("!name".to_string())),
            "!name"
        );
        assert_eq!(format!("{}", Type::identified("name".to_string())), "!name");
        let ty = Type::function(vec![], Type::void());
        assert_eq!(format!("{}", ty), "() -> void");
        let ty = Type::function(vec![Type::int(16)], Type::void());
        assert_eq!(format!("{}", ty), "(i16) -> void");
        let ty = Type::function(vec![Type::int(16), Type::int(32)], Type::void());
        assert_eq!(format!("{}", ty), "(i16, i32) -> void");
        let ty = Type::struct_(vec![Type::int(32), Type::int(64)]);
        assert_eq!(format!("{}", ty), "{ i32, i64 }");
    }

    #[test]
    fn test_size() {
        assert_eq!(Type::void().bytewidth(), 0);
        assert_eq!(Type::identified("name".to_string()).bytewidth(), 0);
        assert_eq!(Type::int(1).bytewidth(), 1);
        assert_eq!(Type::int(32).bytewidth(), 4);
        assert_eq!(Type::int(64).bytewidth(), 8);
        assert_eq!(Type::half().bytewidth(), 2);
        assert_eq!(Type::float().bytewidth(), 4);
        assert_eq!(Type::double().bytewidth(), 8);
        assert_eq!(Type::ptr().bytewidth(), 8);
        assert_eq!(Type::array(10, Type::int(32)).bytewidth(), 40);
        let ty = Type::function(vec![], Type::void());
        assert_eq!(ty.bytewidth(), 8);
        let ty = Type::function(vec![Type::int(32)], Type::void());
        assert_eq!(ty.bytewidth(), 8);
        let ty = Type::function(vec![Type::int(32), Type::int(64)], Type::void());
        assert_eq!(ty.bytewidth(), 8);
        let ty = Type::struct_(vec![Type::int(32), Type::int(64)]);
        assert_eq!(ty.bytewidth(), 12);
    }
}
