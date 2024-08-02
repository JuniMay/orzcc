use std::{fmt, hash::Hash};

use super::{source_loc::Span, Context};
use crate::collections::storage::{ArenaAlloc, ArenaDeref, ArenaPtr, UniqueArenaPtr};

/// The type kinds.
#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub enum TyData {
    /// A void type.
    Void,
    /// 1-bit integer.
    ///
    /// This is special because it is used for boolean values.
    I1,
    /// A byte integer.
    I8,
    /// A 16-bit integer.
    I16,
    /// A 32-bit integer.
    I32,
    /// A 64-bit integer.
    I64,
    /// An IEEE 754 single precision floating point number.
    Float32,
    /// An IEEE 754 double precision floating point number.
    Float64,
    /// An opaque pointer.
    Ptr,
    /// A SIMD type.
    Simd {
        /// The element type.
        elem_ty: Ty,
        /// The exponential number of elements in the SIMD type.
        ///
        /// The number of elements in a SIMD type is usually a power of 2.
        exp: u16,
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
#[derive(Debug, Clone)]
pub struct Signature {
    pub(super) ret: Vec<Ty>,
    pub(super) params: Vec<Ty>,

    /// The source span of the signature.
    ///
    /// The source span will not be compared nor hashed.
    source_span: Span,
}

impl PartialEq for Signature {
    fn eq(&self, other: &Self) -> bool { self.ret == other.ret && self.params == other.params }
}

impl Eq for Signature {}

impl Hash for Signature {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ret.hash(state);
        self.params.hash(state);
    }
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

    pub fn with_source_span(mut self, span: Span) -> Self {
        self.source_span = span;
        self
    }

    pub fn source_span(&self) -> Span { self.source_span }

    pub fn display<'a>(&'a self, ctx: &'a Context) -> DisplaySig<'a> {
        DisplaySig { ctx, sig: self }
    }

    pub fn ret_tys(&self) -> &[Ty] { &self.ret }

    pub fn param_tys(&self) -> &[Ty] { &self.params }
}

pub struct DisplayTy<'a> {
    ctx: &'a Context,
    data: &'a TyData,
}

impl<'a> fmt::Display for DisplayTy<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.data {
            TyData::Void => write!(f, "void"),
            TyData::I1 => write!(f, "i1"),
            TyData::I8 => write!(f, "i8"),
            TyData::I16 => write!(f, "i16"),
            TyData::I32 => write!(f, "i32"),
            TyData::I64 => write!(f, "i64"),
            TyData::Float32 => write!(f, "f32"),
            TyData::Float64 => write!(f, "f64"),
            TyData::Ptr => write!(f, "ptr"),
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

    pub fn int(ctx: &mut Context, bits: u16) -> Self {
        match bits {
            1 => ctx.alloc(TyData::I1),
            8 => ctx.alloc(TyData::I8),
            16 => ctx.alloc(TyData::I16),
            32 => ctx.alloc(TyData::I32),
            64 => ctx.alloc(TyData::I64),
            _ => panic!("unsupported integer bitwidth: {}", bits),
        }
    }

    pub fn float32(ctx: &mut Context) -> Self { ctx.alloc(TyData::Float32) }

    pub fn float64(ctx: &mut Context) -> Self { ctx.alloc(TyData::Float64) }

    pub fn ptr(ctx: &mut Context) -> Self { ctx.alloc(TyData::Ptr) }

    pub fn struct_(ctx: &mut Context, field_tys: Vec<Ty>, is_packed: bool) -> Self {
        ctx.alloc(TyData::Struct {
            field_tys,
            is_packed,
        })
    }

    pub fn simd(ctx: &mut Context, elem_ty: Ty, exp: u16) -> Self {
        ctx.alloc(TyData::Simd { elem_ty, exp })
    }

    /// Check if the type is integer.
    pub fn is_integer(&self, ctx: &Context) -> bool {
        matches!(
            self.deref(ctx),
            TyData::I1 | TyData::I8 | TyData::I16 | TyData::I32 | TyData::I64
        )
    }

    /// Check if the type is float32 or float64.
    pub fn is_float(&self, ctx: &Context) -> bool {
        matches!(self.deref(ctx), TyData::Float32 | TyData::Float64)
    }

    pub fn is_float32(&self, ctx: &Context) -> bool { matches!(self.deref(ctx), TyData::Float32) }

    pub fn is_float64(&self, ctx: &Context) -> bool { matches!(self.deref(ctx), TyData::Float64) }

    pub fn is_ptr(&self, ctx: &Context) -> bool { matches!(self.deref(ctx), TyData::Ptr) }

    pub fn is_void(&self, ctx: &Context) -> bool { matches!(self.deref(ctx), TyData::Void) }

    /// Get the bitwidth of the type.
    ///
    /// # Parameters
    ///
    /// - `ctx`: The context.
    ///
    /// # Panics
    ///
    /// Panics if the type is/contains void.
    pub fn bitwidth(&self, ctx: &Context) -> usize {
        match self.deref(ctx) {
            TyData::I1 => 1,
            TyData::I8 => 8,
            TyData::I16 => 16,
            TyData::I32 => 32,
            TyData::I64 => 64,
            TyData::Float32 => 32,
            TyData::Float64 => 64,
            TyData::Ptr => ctx.pointer_width() as usize,
            TyData::Struct { field_tys, .. } => {
                let mut bw = 0;
                for ty in field_tys {
                    bw += ty.bitwidth(ctx);
                }
                bw
            }
            TyData::Simd { elem_ty, exp } => elem_ty.bitwidth(ctx) * (1 << exp),
            TyData::Void => unreachable!(
                "should not get bitwidth of void, which only appears in function return type"
            ),
        }
    }

    /// Get the byte width of the type.
    ///
    /// # Parameters
    ///
    /// - `ctx`: The context.
    ///
    /// # Panics
    ///
    /// Panics if the type is/contains void.
    pub fn bytewidth(&self, ctx: &Context) -> usize { (self.bitwidth(ctx) + 7) / 8 }

    pub fn display(self, ctx: &Context) -> DisplayTy<'_> {
        DisplayTy {
            ctx,
            data: self.deref(ctx),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ir::{
        source_loc::Span,
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

        let pointer1 = Ty::ptr(&mut ctx);
        let pointer2 = Ty::ptr(&mut ctx);

        assert_eq!(pointer1, pointer2);
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

        let pointer = Ty::ptr(&mut ctx);
        let s = format!("{}", pointer.display(&ctx));
        assert_eq!(s, "ptr");

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

    #[test]
    fn test_sig_equality() {
        // test the equality under different spans
        let mut ctx = Context::default();
        let int32 = Ty::int(&mut ctx, 32);
        let float32 = Ty::float32(&mut ctx);

        let sig1 = Signature::new(vec![int32, float32], vec![])
            .with_source_span(Span::new(3.into(), 20.into()));
        let sig2 = Signature::new(vec![int32, float32], vec![])
            .with_source_span(Span::new(5.into(), 22.into()));

        assert_ne!(sig1.source_span(), sig2.source_span());
        assert_eq!(sig1, sig2);
    }
}
