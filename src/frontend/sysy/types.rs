use std::{cell::RefCell, collections::HashMap, fmt, hash, rc::Rc};

/// The type in SysY language.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Void,
    /// The boolean type.
    ///
    /// There is no `bool` type, but the intermediate result might be boolean.
    Bool,
    /// The integer type.
    Int,
    /// The floating-point type.
    Float,
    /// We need pointer type for array-pointer decay.
    Ptr(Type),
    /// The array type.
    Array(Type, usize),
    /// The function type, with params and return type.
    ///
    /// TODO: Maybe support va_args?
    Func(Vec<Type>, Type),
}

#[derive(Clone, Eq)]
pub struct Type(Rc<TypeKind>);

impl hash::Hash for Type {
    fn hash<H: hash::Hasher>(&self, state: &mut H) { self.0.hash(state) }
}

impl PartialEq for Type {
    // just compare the pointers
    fn eq(&self, other: &Self) -> bool { Rc::ptr_eq(&self.0, &other.0) }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.0.fmt(f) }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind() {
            TypeKind::Void => write!(f, "void"),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::Int => write!(f, "int"),
            TypeKind::Float => write!(f, "float"),
            TypeKind::Ptr(t) => write!(f, "{}*", t),
            TypeKind::Array(t, len) => write!(f, "[{} x {}]", t, len),
            TypeKind::Func(params, ret) => write!(
                f,
                "{}({})",
                ret,
                params
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl Type {
    thread_local! {
        /// The pool to implement singleton.
        ///
        /// Reference: https://github.com/pku-minic/koopa/blob/master/src/ir/types.rs
        static POOL: RefCell<HashMap<TypeKind, Type>> = RefCell::new(HashMap::new());
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

    pub fn kind(&self) -> &TypeKind { &self.0 }

    pub fn void() -> Self { Self::make(TypeKind::Void) }

    pub fn bool() -> Self { Self::make(TypeKind::Bool) }

    pub fn int() -> Self { Self::make(TypeKind::Int) }

    pub fn float() -> Self { Self::make(TypeKind::Float) }

    pub fn ptr(ty: Type) -> Self { Self::make(TypeKind::Ptr(ty)) }

    pub fn array(ty: Type, len: usize) -> Self { Self::make(TypeKind::Array(ty, len)) }

    pub fn func(params: Vec<Type>, ret: Type) -> Self { Self::make(TypeKind::Func(params, ret)) }

    /// Get all the dimensions of an array
    pub fn array_dims(&self) -> Vec<usize> {
        let mut dims = Vec::new();
        let mut ty = self;

        while let TypeKind::Array(elem_ty, len) = ty.kind() {
            dims.push(*len);
            ty = elem_ty;
        }

        dims
    }

    pub fn is_int(&self) -> bool { matches!(self.kind(), TypeKind::Int) }

    pub fn is_float(&self) -> bool { matches!(self.kind(), TypeKind::Float) }

    pub fn is_bool(&self) -> bool { matches!(self.kind(), TypeKind::Bool) }

    pub fn is_aggregate(&self) -> bool { matches!(self.kind(), TypeKind::Array(..)) }

    pub fn is_ptr(&self) -> bool { matches!(self.kind(), TypeKind::Ptr(..)) }

    pub fn is_void(&self) -> bool { matches!(self.kind(), TypeKind::Void) }

    pub fn unwrap_func(&self) -> (&[Type], &Type) {
        if let TypeKind::Func(params, ret) = self.kind() {
            (params, ret)
        } else {
            panic!("unwrap_func: not a function type: {}", self);
        }
    }

    pub fn unwrap_array(&self) -> (&Type, usize) {
        if let TypeKind::Array(ty, len) = self.kind() {
            (ty, *len)
        } else {
            panic!("unwrap_array: not an array type: {}", self);
        }
    }

    pub fn inner_ty(&self) -> Option<&Type> {
        if let TypeKind::Array(ty, _) = self.kind() {
            Some(ty)
        } else if let TypeKind::Ptr(ty) = self.kind() {
            Some(ty)
        } else {
            None
        }
    }

    pub fn array_base(&self) -> &Type {
        let mut ty = self;
        while let TypeKind::Array(elem_ty, _) = ty.kind() {
            ty = elem_ty;
        }
        ty
    }

    pub fn bytewidth(&self) -> usize {
        match self.kind() {
            TypeKind::Void => 0,
            TypeKind::Bool => 1,
            TypeKind::Int => 4,
            TypeKind::Float => 4,
            TypeKind::Array(ty, len) => ty.bytewidth() * len,
            TypeKind::Ptr(_) => unreachable!(), // TODO: target dependent
            TypeKind::Func(_, _) => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_array_dims() {
        // int[4][3]
        let ty = Type::array(Type::array(Type::int(), 3), 4);
        assert_eq!(ty.array_dims(), vec![4, 3]);
    }

    #[test]
    fn test_type_display() {
        assert_eq!(Type::void().to_string(), "void");
        assert_eq!(Type::bool().to_string(), "bool");
        assert_eq!(Type::int().to_string(), "int");
        assert_eq!(Type::float().to_string(), "float");
        assert_eq!(Type::ptr(Type::int()).to_string(), "int*");
        assert_eq!(Type::array(Type::int(), 4).to_string(), "[int x 4]");
        assert_eq!(
            Type::array(Type::array(Type::int(), 3), 4).to_string(),
            "[[int x 3] x 4]"
        );
        assert_eq!(
            Type::func(vec![Type::int(), Type::float()], Type::void()).to_string(),
            "void(int, float)"
        );
    }
}
