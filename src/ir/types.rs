use crate::{backend::layout::DataLayout, ir::IDENTIFIER_PREFIX};
use std::{
    fmt::{self},
    rc::Rc,
};

/// Kind of types.
#[derive(Debug, Clone)]
pub enum TyKind {
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
    Array(usize, Type),
    /// Fn
    Fn(Vec<Type>, Type),
    /// Struct
    Struct(Option<String>, Vec<Type>),
    /// A label
    Label,
}

#[derive(Debug, Clone)]
pub struct Type(Rc<TyKind>);

impl fmt::Display for TyKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TyKind::Void => write!(f, "void"),
            TyKind::Int(bits) => write!(f, "i{}", bits),
            TyKind::Half => write!(f, "half"),
            TyKind::Float => write!(f, "float"),
            TyKind::Double => write!(f, "double"),
            TyKind::Ptr => write!(f, "*"),
            TyKind::Array(size, ty) => write!(f, "[{}x{}]", size, ty),
            TyKind::Fn(params, ret) => {
                write!(f, "(")?;
                let mut is_first = true;
                for param in params {
                    if !is_first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                    is_first = false;
                }
                write!(f, ") -> {}", ret)
            }
            TyKind::Struct(name, fields) => {
                if let Some(name) = name {
                    write!(f, "{}{}", IDENTIFIER_PREFIX, name)
                } else {
                    write!(f, "{{ ")?;
                    let mut is_first = true;
                    for field in fields {
                        if !is_first {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", field)?;
                        is_first = false;
                    }
                    write!(f, " }}")
                }
            }
            TyKind::Label => write!(f, "label"),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Type {
    pub fn kind(&self) -> &TyKind {
        return &*self.0;
    }

    pub fn calc_bits(&self, data_layout: &DataLayout) -> usize {
        match *self.0 {
            TyKind::Void => 0,
            TyKind::Int(size) => size,
            TyKind::Half => 16,
            TyKind::Float => 32,
            TyKind::Double => 64,
            TyKind::Ptr => data_layout.pointer_size,
            TyKind::Array(_, ref ty) => ty.calc_bits(data_layout),
            TyKind::Fn(_, _) => 64,
            TyKind::Struct(_, ref fields) => {
                let mut bits = 0;
                for field in fields {
                    bits += field.calc_bits(data_layout);
                }
                bits
            }
            TyKind::Label => 0,
        }
    }

    pub fn calc_bytes(&self, data_layout: &DataLayout) -> usize {
        self.calc_bits(data_layout) / 8
    }

    pub fn mk_void() -> Type {
        Type(Rc::new(TyKind::Void))
    }

    pub fn mk_int(size: usize) -> Type {
        Type(Rc::new(TyKind::Int(size)))
    }

    pub fn mk_half() -> Type {
        Type(Rc::new(TyKind::Half))
    }

    pub fn mk_float() -> Type {
        Type(Rc::new(TyKind::Float))
    }

    pub fn mk_double() -> Type {
        Type(Rc::new(TyKind::Double))
    }

    pub fn mk_ptr() -> Type {
        Type(Rc::new(TyKind::Ptr))
    }

    pub fn mk_fn(params: Vec<Type>, ret: Type) -> Type {
        Type(Rc::new(TyKind::Fn(params, ret)))
    }

    pub fn mk_array(size: usize, ty: Type) -> Type {
        Type(Rc::new(TyKind::Array(size, ty)))
    }

    pub fn mk_struct(name: Option<String>, fields: Vec<Type>) -> Type {
        Type(Rc::new(TyKind::Struct(name, fields)))
    }

    pub fn mk_label() -> Type {
        Type(Rc::new(TyKind::Label))
    }
}
