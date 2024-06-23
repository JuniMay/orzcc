use std::fmt;

use super::{source_loc::Span, Context};
use crate::collections::storage::{ArenaAlloc, ArenaDeref, ArenaPtr, UniqueArenaPtr};

/// The type kinds.
#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub enum TyData {
    /// A void type.
    Void,
    /// An integer type.
    ///
    /// The width (in bits) of an integer can be any positive integer under
    /// [u16::MAX].
    Integer(u16),
    /// An IEEE 754 single precision floating point number.
    Float32,
    /// An IEEE 754 double precision floating point number.
    Float64,
    /// An index type.
    ///
    /// The width of index is target-dependent.
    ///
    /// This concept is borrowed from MLIR. There are nuances between `index`
    /// and `ptr`. Pointer emphasizes that it points to a memory address and the
    /// object there, but index can also refer to offsets of addresses. So
    /// index can be more concise and maybe eliminate the requirement of the
    /// `getelementptr` operation.
    ///
    /// Question: Do we need to distinguish between `index` and `ptr`? Or do we
    /// need to introduce a `memref` type? I don't think that is necessary,
    /// because the main purpose of `index` is to represent a target-dependent
    /// integer type. The IR is low-level enough to represent memory addresses
    /// directly.
    Index,
    /// A SIMD type.
    Simd {
        /// The element type.
        elem_ty: Ty,
        /// The exponential number of elements in the SIMD type.
        ///
        /// The number of elements in a SIMD type is usually a power of 2.
        exp: u16,
    },
    /// An array type.
    Array {
        /// The element type.
        elem_ty: Ty,
        /// The number of elements in the array.
        len: usize,
    },
    /// A struct type.
    Struct {
        /// The field types.
        field_tys: Vec<Ty>,
        /// Whether the struct is packed.
        is_packed: bool,
    },
}

/// The signature of a functions.
///
/// Yes, function does not has a type, but a signature representing its
/// parameter and return types. The function type does not interact with other
/// types, so it is reasonable to make it standalone.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Signature {
    pub(super) ret: Vec<Ty>,
    pub(super) params: Vec<Ty>,

    source_span: Span,
}

pub struct DisplaySig<'a> {
    ctx: &'a Context,
    sig: &'a Signature,
}

impl<'a> fmt::Display for DisplaySig<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for (i, param) in self.sig.params.iter().enumerate() {
            write!(f, "{}", param.display(self.ctx))?;
            if i + 1 < self.sig.params.len() {
                write!(f, ", ")?;
            }
        }

        write!(f, ") -> ")?;

        if self.sig.ret.len() == 1 {
            write!(f, "{}", self.sig.ret[0].display(self.ctx))
        } else {
            write!(f, "(")?;
            for (i, ret) in self.sig.ret.iter().enumerate() {
                write!(f, "{}", ret.display(self.ctx))?;
                if i + 1 < self.sig.ret.len() {
                    write!(f, ", ")?;
                }
            }
            write!(f, ")")
        }
    }
}

impl Signature {
    pub fn new(params: Vec<Ty>, ret: Vec<Ty>) -> Signature {
        Self {
            ret,
            params,
            source_span: Span::default(),
        }
    }

    pub fn set_source_span(&mut self, span: Span) { self.source_span = span; }

    pub fn source_span(self) -> Span { self.source_span }

    pub fn display<'a>(&'a self, ctx: &'a Context) -> DisplaySig<'a> {
        DisplaySig { ctx, sig: self }
    }
}

pub struct DisplayTy<'a> {
    ctx: &'a Context,
    data: &'a TyData,
}

impl<'a> fmt::Display for DisplayTy<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.data {
            TyData::Void => write!(f, "void"),
            TyData::Integer(bits) => write!(f, "i{}", bits),
            TyData::Float32 => write!(f, "f32"),
            TyData::Float64 => write!(f, "f64"),
            TyData::Index => write!(f, "index"),
            TyData::Array { elem_ty, len } => {
                write!(f, "[{}; {}]", elem_ty.display(self.ctx), len)
            }
            TyData::Struct {
                field_tys,
                is_packed,
            } => {
                if *is_packed {
                    write!(f, "<")?;
                }
                write!(f, "{{")?;
                for (i, ty) in field_tys.iter().enumerate() {
                    write!(f, "{}", ty.display(self.ctx))?;
                    if i + 1 < field_tys.len() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")?;
                if *is_packed {
                    write!(f, ">")?;
                }
                Ok(())
            }
            TyData::Simd { elem_ty, exp } => {
                write!(f, "<{}; {}>", elem_ty.display(self.ctx), 1 << exp)
            }
        }
    }
}

impl Ty {
    pub fn display(self, ctx: &Context) -> DisplayTy<'_> {
        DisplayTy {
            ctx,
            data: self.deref(ctx),
        }
    }
}

/// The type in IR.
///
/// [Ty] is actually a wrapper of [UniqueArenaPtr] of [TyData], and can be
/// copied and hashed. The associated arena is [Context].
#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct Ty(UniqueArenaPtr<TyData>);

// [Ty] is special because it wraps [UniqueArenaPtr], so we need to implement
// it manually.

impl ArenaPtr for Ty {
    type A = Context;
    type T = TyData;

    fn try_deref(self, ctx: &Self::A) -> Option<&Self::T> { ctx.try_deref(self) }

    fn try_deref_mut(self, arena: &mut Self::A) -> Option<&mut Self::T> {
        arena.try_deref_mut(self)
    }
}

impl ArenaDeref<TyData, Ty> for Context {
    fn try_deref(&self, ptr: Ty) -> Option<&TyData> { self.tys.try_deref(ptr.0) }

    fn try_deref_mut(&mut self, ptr: Ty) -> Option<&mut TyData> { self.tys.try_deref_mut(ptr.0) }
}

impl ArenaAlloc<TyData, Ty> for Context {
    /// # Panics
    ///
    /// Panics if `alloc_with` is called, because unique hash is required for
    /// [Ty].
    ///
    /// # See Also
    ///
    /// - [UniqueArena](crate::collections::storage::UniqueArena)
    fn alloc_with<F>(&mut self, _f: F) -> Ty
    where
        F: FnOnce(Ty) -> TyData,
    {
        panic!("alloc_with should not be called for Ty")
    }

    fn alloc(&mut self, val: TyData) -> Ty { Ty(self.tys.alloc(val)) }
}

impl Ty {
    pub fn void(ctx: &mut Context) -> Self { ctx.alloc(TyData::Void) }

    pub fn int(ctx: &mut Context, bits: u16) -> Self { ctx.alloc(TyData::Integer(bits)) }

    pub fn float32(ctx: &mut Context) -> Self { ctx.alloc(TyData::Float32) }

    pub fn float64(ctx: &mut Context) -> Self { ctx.alloc(TyData::Float64) }

    pub fn index(ctx: &mut Context) -> Self { ctx.alloc(TyData::Index) }

    pub fn array(ctx: &mut Context, elem_ty: Ty, len: usize) -> Self {
        ctx.alloc(TyData::Array { elem_ty, len })
    }

    pub fn struct_(ctx: &mut Context, field_tys: Vec<Ty>, is_packed: bool) -> Self {
        ctx.alloc(TyData::Struct {
            field_tys,
            is_packed,
        })
    }

    pub fn simd(ctx: &mut Context, elem_ty: Ty, exp: u16) -> Self {
        ctx.alloc(TyData::Simd { elem_ty, exp })
    }

    /// Check if the type is integer or index.
    pub fn is_integer(&self, ctx: &Context) -> bool {
        matches!(self.deref(ctx), TyData::Integer(_))
    }

    /// Check if the type is float32 or float64.
    pub fn is_float(&self, ctx: &Context) -> bool {
        matches!(self.deref(ctx), TyData::Float32 | TyData::Float64)
    }

    pub fn is_float32(&self, ctx: &Context) -> bool { matches!(self.deref(ctx), TyData::Float32) }

    pub fn is_float64(&self, ctx: &Context) -> bool { matches!(self.deref(ctx), TyData::Float64) }

    pub fn is_index(&self, ctx: &Context) -> bool { matches!(self.deref(ctx), TyData::Index) }

    pub fn bitwidth(&self, ctx: &Context) -> Option<usize> {
        match self.deref(ctx) {
            TyData::Integer(bits) => Some(*bits as usize),
            TyData::Float32 => Some(32),
            TyData::Float64 => Some(64),
            TyData::Index => None,
            TyData::Void => None,
            TyData::Array { elem_ty, len } => elem_ty.bitwidth(ctx).map(|bw| bw * len),
            TyData::Struct { field_tys, .. } => {
                let mut bw = 0;
                for ty in field_tys {
                    if let Some(ty_bw) = ty.bitwidth(ctx) {
                        bw += ty_bw;
                    } else {
                        return None;
                    }
                }
                Some(bw)
            }
            TyData::Simd { elem_ty, exp } => elem_ty.bitwidth(ctx).map(|bw| bw * (1 << exp)),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ir::{
        ty::{Signature, Ty},
        Context,
    };

    #[test]
    fn test_ty_equality() {
        let mut ctx = Context::default();

        let void1 = Ty::void(&mut ctx);
        let void2 = Ty::void(&mut ctx);

        assert_eq!(void1, void2);

        let int1 = Ty::int(&mut ctx, 32);
        let int2 = Ty::int(&mut ctx, 32);
        let int3 = Ty::int(&mut ctx, 64);

        assert_eq!(int1, int2);
        assert_ne!(int1, int3); // 32 != 64

        let float32_1 = Ty::float32(&mut ctx);
        let float32_2 = Ty::float32(&mut ctx);
        let float64_1 = Ty::float64(&mut ctx);
        let float64_2 = Ty::float64(&mut ctx);

        assert_eq!(float32_1, float32_2);
        assert_eq!(float64_1, float64_2);

        assert_ne!(float32_1, float64_1); // float32 != float64

        let pointer1 = Ty::index(&mut ctx);
        let pointer2 = Ty::index(&mut ctx);

        assert_eq!(pointer1, pointer2);

        let array1 = Ty::array(&mut ctx, int1, 10);
        let array2 = Ty::array(&mut ctx, int1, 10);
        let array3 = Ty::array(&mut ctx, int1, 20);

        assert_eq!(array1, array2);
        assert_ne!(array1, array3); // 10 != 20

        let array_array1 = Ty::array(&mut ctx, array1, 10);
        let array_array2 = Ty::array(&mut ctx, array1, 10);
        let array_array3 = Ty::array(&mut ctx, array1, 20);
        let array_array4 = Ty::array(&mut ctx, array3, 10);

        assert_eq!(array_array1, array_array2);
        assert_ne!(array_array1, array_array3); // 10 != 20
        assert_ne!(array_array1, array_array4); // array1 != array3
    }

    #[test]
    fn test_ty_display() -> std::fmt::Result {
        let mut ctx = Context::default();
        let void = Ty::void(&mut ctx);
        let s = format!("{}", void.display(&ctx));
        assert_eq!(s, "void");

        let int32 = Ty::int(&mut ctx, 32);
        let s = format!("{}", int32.display(&ctx));
        assert_eq!(s, "i32");

        let float32 = Ty::float32(&mut ctx);
        let s = format!("{}", float32.display(&ctx));
        assert_eq!(s, "f32");

        let float64 = Ty::float64(&mut ctx);
        let s = format!("{}", float64.display(&ctx));
        assert_eq!(s, "f64");

        let pointer = Ty::index(&mut ctx);
        let s = format!("{}", pointer.display(&ctx));
        assert_eq!(s, "index");

        let array = Ty::array(&mut ctx, int32, 10);
        let s = format!("{}", array.display(&ctx));
        assert_eq!(s, "[i32; 10]");

        let array_array = Ty::array(&mut ctx, array, 10);
        let s = format!("{}", array_array.display(&ctx));
        assert_eq!(s, "[[i32; 10]; 10]");

        let struct_ty = Ty::struct_(&mut ctx, vec![int32, float32], false);
        let s = format!("{}", struct_ty.display(&ctx));
        assert_eq!(s, "{i32, f32}");

        let packed_struct_ty = Ty::struct_(&mut ctx, vec![int32, float32], true);
        let s = format!("{}", packed_struct_ty.display(&ctx));
        assert_eq!(s, "<{i32, f32}>");

        let simd0 = Ty::simd(&mut ctx, int32, 2);
        let s = format!("{}", simd0.display(&ctx));
        assert_eq!(s, "<i32; 4>");

        let simd1 = Ty::simd(&mut ctx, int32, 5);
        let s = format!("{}", simd1.display(&ctx));
        assert_eq!(s, "<i32; 32>");

        Ok(())
    }

    #[test]
    fn test_sig_display() {
        let mut ctx = Context::default();
        let int32 = Ty::int(&mut ctx, 32);
        let float32 = Ty::float32(&mut ctx);
        let void = Ty::void(&mut ctx);

        let sig = Signature::new(vec![int32, float32], vec![void]);
        let s = format!("{}", sig.display(&ctx));
        assert_eq!(s, "(i32, f32) -> void");
    }
}
