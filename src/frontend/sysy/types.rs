/// The type in SysY language.
pub enum Type {
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
    Ptr(Box<Type>),
    /// The array type.
    Array(Box<Type>, usize),
    /// The function type, with params and return type.
    ///
    /// TODO: Maybe support va_args?
    Func(Vec<Type>, Box<Type>),
}

impl Type {
    pub fn void() -> Self { Self::Void }

    pub fn bool() -> Self { Self::Bool }

    pub fn int() -> Self { Self::Int }

    pub fn float() -> Self { Self::Float }

    pub fn ptr(ty: Type) -> Self { Self::Ptr(Box::new(ty)) }

    pub fn array(ty: Type, len: usize) -> Self { Self::Array(Box::new(ty), len) }

    pub fn func(params: Vec<Type>, ret: Type) -> Self { Self::Func(params, Box::new(ret)) }

    /// Get all the dimensions of an array
    pub fn array_dims(&self) -> Vec<usize> {
        let mut dims = Vec::new();
        let mut ty = self;

        while let Type::Array(elem_ty, len) = ty {
            dims.push(*len);
            ty = elem_ty;
        }

        dims
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
}
