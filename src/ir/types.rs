use std::cell::RefCell;
use std::cmp;
use std::collections::HashMap;
use std::fmt;
use std::hash;
use std::rc::Rc;

use super::TYPE_PREFIX;

/// Target dependent data layout
#[derive(Clone, Copy, Debug)]
pub struct DataLayout {
    /// Pointer size on the platform
    pub pointer_size: usize,
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
    /// Array is a type that represents a fixed number of elements of the same type.
    Array(usize, Type),
    /// Function
    ///
    /// Function is a type that takes a list of types and returns a type.
    Function(Vec<Type>, Type),
    /// Struct
    ///
    /// Struct is basically a collection of types.
    Struct(Vec<Type>),
    /// A label
    ///
    /// Block in the IR can have the same name as operands and can be distinguished by `label` type.
    Label,
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
            TypeKind::Label => write!(f, "label"),
            TypeKind::Identified(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Clone, Eq)]
pub struct Type(Rc<TypeKind>);

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Type {
    thread_local! {
        /// Pool of all types.
        static POOL: RefCell<HashMap<TypeKind, Type>> = RefCell::new(HashMap::new());
        static IDENTIFIED_POOL: RefCell<HashMap<String, Type>> = RefCell::new(HashMap::new());
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

    pub fn mk_int(bits: usize) -> Type {
        Type::make(TypeKind::Int(bits))
    }

    pub fn mk_i32() -> Type {
        Type::mk_int(32)
    }

    pub fn mk_i1() -> Type {
        Type::mk_int(1)
    }

    pub fn mk_void() -> Type {
        Type::make(TypeKind::Void)
    }

    pub fn mk_half() -> Type {
        Type::make(TypeKind::Half)
    }

    pub fn mk_float() -> Type {
        Type::make(TypeKind::Float)
    }

    pub fn mk_double() -> Type {
        Type::make(TypeKind::Double)
    }

    pub fn mk_ptr() -> Type {
        Type::make(TypeKind::Ptr)
    }

    pub fn mk_array(size: usize, ty: Type) -> Type {
        Type::make(TypeKind::Array(size, ty))
    }

    pub fn mk_function(params: Vec<Type>, ret: Type) -> Type {
        Type::make(TypeKind::Function(params, ret))
    }

    pub fn mk_struct(fields: Vec<Type>) -> Type {
        Type::make(TypeKind::Struct(fields))
    }

    pub fn mk_label() -> Type {
        Type::make(TypeKind::Label)
    }

    pub fn mk_identified(name: String) -> Type {
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

    pub fn kind(&self) -> &TypeKind {
        &self.0
    }

    pub fn size(&self, data_layout: Option<&DataLayout>) -> usize {
        match self.kind() {
            TypeKind::Void => 0,
            TypeKind::Int(bits) => cmp::max(1, bits / 8),
            TypeKind::Half => 2,
            TypeKind::Float => 4,
            TypeKind::Double => 8,
            TypeKind::Ptr => data_layout.map_or(8, |dl| dl.pointer_size),
            TypeKind::Array(size, ty) => size * ty.size(data_layout),
            TypeKind::Function(_, _) => data_layout.map_or(0, |dl| dl.pointer_size),
            TypeKind::Struct(fields) => fields.iter().map(|ty| ty.size(data_layout)).sum(),
            TypeKind::Label => 0,
            TypeKind::Identified(_) => 0,
        }
    }

    pub fn is_int(&self) -> bool {
        matches!(self.kind(), TypeKind::Int(_))
    }

    pub fn is_float(&self) -> bool {
        matches!(
            self.kind(),
            TypeKind::Half | TypeKind::Float | TypeKind::Double
        )
    }

    pub fn is_numeric(&self) -> bool {
        self.is_int() || self.is_float()
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self.kind(), TypeKind::Ptr)
    }

    pub fn is_void(&self) -> bool {
        matches!(self.kind(), TypeKind::Void)
    }

    pub fn is_function(&self) -> bool {
        matches!(self.kind(), TypeKind::Function(_, _))
    }

    pub fn is_zero_initializable(&self) -> bool {
        !matches!(
            self.kind(),
            TypeKind::Void | TypeKind::Function(_, _) | TypeKind::Label | TypeKind::Identified(_)
        )
    }

    pub fn as_array(&self) -> Option<(usize, &Type)> {
        match self.kind() {
            TypeKind::Array(size, ty) => Some((*size, ty)),
            _ => None,
        }
    }

    /// Convert the type to a struct type.
    ///
    /// If the type is not a struct type, return `None`.
    /// Note that for identified types, this function returns `None` for now.
    pub fn as_struct(&self) -> Option<&[Type]> {
        match self.kind() {
            TypeKind::Struct(fields) => Some(fields),
            _ => None,
        }
    }

    pub fn is_aggregate(&self) -> bool {
        matches!(self.kind(), TypeKind::Array(_, _) | TypeKind::Struct(_))
    }
}

impl hash::Hash for Type {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl cmp::PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_equality() {
        assert_eq!(Type::mk_i32(), Type::mk_int(32));
        assert_eq!(Type::mk_i1(), Type::mk_int(1));
        assert_ne!(Type::mk_i32(), Type::mk_int(64));

        assert!(Type::mk_i32().is_int());
        assert!(Type::mk_float().is_float());
        assert!(!Type::mk_i32().is_float());
        assert!(Type::mk_ptr().is_ptr());

        assert_eq!(
            Type::mk_array(10, Type::mk_int(32)),
            Type::mk_array(10, Type::mk_int(32))
        );
        assert_ne!(Type::mk_int(32), Type::mk_int(64));
        assert_ne!(
            Type::mk_array(10, Type::mk_int(32)),
            Type::mk_array(20, Type::mk_int(32))
        );
        assert_eq!(Type::mk_void(), Type::mk_void());
        assert_eq!(Type::mk_half(), Type::mk_half());
        assert_eq!(Type::mk_float(), Type::mk_float());
        assert_eq!(Type::mk_double(), Type::mk_double());
        assert_eq!(Type::mk_ptr(), Type::mk_ptr());
        assert_eq!(Type::mk_label(), Type::mk_label());
        assert_eq!(
            Type::mk_identified("name".to_string()),
            Type::mk_identified("name".to_string())
        );
        assert_ne!(
            Type::mk_identified("name".to_string()),
            Type::mk_identified("name2".to_string())
        );
        assert_eq!(
            Type::mk_function(vec![], Type::mk_void()),
            Type::mk_function(vec![], Type::mk_void())
        );
        assert_ne!(
            Type::mk_function(vec![Type::mk_int(16)], Type::mk_void()),
            Type::mk_function(vec![], Type::mk_int(32))
        );
        let ty1 = Type::mk_struct(vec![Type::mk_int(32), Type::mk_int(64)]);
        let ty2 = Type::mk_struct(vec![Type::mk_int(32), Type::mk_int(64)]);
        assert!(ty1.as_struct().is_some());
        assert_eq!(ty1, ty2);
    }

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", Type::mk_int(32)), "i32");
        let ty = Type::mk_array(10, Type::mk_int(32));
        assert_eq!(format!("{}", ty), "[i32; 10]");
        assert_eq!(format!("{}", Type::mk_void()), "void");
        assert_eq!(format!("{}", Type::mk_half()), "half");
        assert_eq!(format!("{}", Type::mk_float()), "float");
        assert_eq!(format!("{}", Type::mk_double()), "double");
        assert_eq!(format!("{}", Type::mk_ptr()), "ptr");
        assert_eq!(format!("{}", Type::mk_label()), "label");
        assert_eq!(
            format!("{}", Type::mk_identified("!name".to_string())),
            "!name"
        );
        assert_eq!(
            format!("{}", Type::mk_identified("name".to_string())),
            "!name"
        );
        let ty = Type::mk_function(vec![], Type::mk_void());
        assert_eq!(format!("{}", ty), "() -> void");
        let ty = Type::mk_function(vec![Type::mk_int(16)], Type::mk_void());
        assert_eq!(format!("{}", ty), "(i16) -> void");
        let ty = Type::mk_function(vec![Type::mk_int(16), Type::mk_int(32)], Type::mk_void());
        assert_eq!(format!("{}", ty), "(i16, i32) -> void");
        let ty = Type::mk_struct(vec![Type::mk_int(32), Type::mk_int(64)]);
        assert_eq!(format!("{}", ty), "{ i32, i64 }");
    }

    #[test]
    fn test_size() {
        let dl = DataLayout { pointer_size: 8 };
        assert_eq!(Type::mk_void().size(Some(&dl)), 0);
        assert_eq!(Type::mk_label().size(Some(&dl)), 0);
        assert_eq!(Type::mk_identified("name".to_string()).size(Some(&dl)), 0);
        assert_eq!(Type::mk_int(1).size(Some(&dl)), 1);
        assert_eq!(Type::mk_int(32).size(Some(&dl)), 4);
        assert_eq!(Type::mk_int(64).size(Some(&dl)), 8);
        assert_eq!(Type::mk_half().size(Some(&dl)), 2);
        assert_eq!(Type::mk_float().size(Some(&dl)), 4);
        assert_eq!(Type::mk_double().size(Some(&dl)), 8);
        assert_eq!(Type::mk_ptr().size(Some(&dl)), 8);
        assert_eq!(Type::mk_array(10, Type::mk_int(32)).size(Some(&dl)), 40);
        let ty = Type::mk_function(vec![], Type::mk_void());
        assert_eq!(ty.size(Some(&dl)), 8);
        let ty = Type::mk_function(vec![Type::mk_int(32)], Type::mk_void());
        assert_eq!(ty.size(Some(&dl)), 8);
        let ty = Type::mk_function(vec![Type::mk_int(32), Type::mk_int(64)], Type::mk_void());
        assert_eq!(ty.size(Some(&dl)), 8);
        let ty = Type::mk_struct(vec![Type::mk_int(32), Type::mk_int(64)]);
        assert_eq!(ty.size(Some(&dl)), 12);
    }
}
