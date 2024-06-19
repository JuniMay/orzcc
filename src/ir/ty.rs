use std::fmt;

use crate::{
    collections::storage::{ArenaAlloc, ArenaDeref, ArenaPtr, UniqueArenaPtr},
    ir::Context,
};

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
    Index,
    /// A SIMD type.
    Simd {
        /// The element type.
        elem_ty: Ty,
        /// The number of elements in the SIMD type.
        len: usize,
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
    ret: Ty,
    params: Vec<Ty>,
}

impl Signature {
    pub fn new(params: Vec<Ty>, ret: Ty) -> Signature { Self { ret, params } }

    pub fn display<T: fmt::Write>(&self, ctx: &Context, f: &mut T) -> fmt::Result {
        write!(f, "(")?;
        for (i, param) in self.params.iter().enumerate() {
            param.display(ctx, f)?;
            if i + 1 < self.params.len() {
                write!(f, ", ")?;
            }
        }
        write!(f, ") -> ")?;
        self.ret.display(ctx, f)
    }
}

impl Ty {
    /// Display the type.
    pub fn display<T: fmt::Write>(&self, ctx: &Context, f: &mut T) -> fmt::Result {
        match self.deref(ctx) {
            TyData::Void => write!(f, "void"),
            TyData::Integer(bits) => write!(f, "i{}", bits),
            TyData::Float32 => write!(f, "f32"),
            TyData::Float64 => write!(f, "f64"),
            TyData::Index => write!(f, "index"),
            TyData::Array { elem_ty, len } => {
                write!(f, "[")?;
                elem_ty.display(ctx, f)?;
                write!(f, "; {}]", len)
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
                    ty.display(ctx, f)?;
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
            TyData::Simd { elem_ty, len } => {
                write!(f, "<")?;
                elem_ty.display(ctx, f)?;
                write!(f, "; {}>", len)
            }
        }
    }
}

/// The type in IR.
///
/// [Ty] is actually a wrapper of [UniqueArenaPtr] of [TyKind], and can be
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
    /// - [UniqueArena]
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

    pub fn pointer(ctx: &mut Context) -> Self { ctx.alloc(TyData::Index) }

    pub fn array(ctx: &mut Context, elem_ty: Ty, len: usize) -> Self {
        ctx.alloc(TyData::Array { elem_ty, len })
    }

    pub fn struct_(ctx: &mut Context, field_tys: Vec<Ty>, is_packed: bool) -> Self {
        ctx.alloc(TyData::Struct {
            field_tys,
            is_packed,
        })
    }

    pub fn simd(ctx: &mut Context, elem_ty: Ty, len: usize) -> Self {
        ctx.alloc(TyData::Simd { elem_ty, len })
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

        let pointer1 = Ty::pointer(&mut ctx);
        let pointer2 = Ty::pointer(&mut ctx);

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
        let mut s = String::new();
        void.display(&ctx, &mut s)?;
        assert_eq!(s, "void");

        let int32 = Ty::int(&mut ctx, 32);
        s.clear();
        int32.display(&ctx, &mut s)?;
        assert_eq!(s, "i32");

        let float32 = Ty::float32(&mut ctx);
        s.clear();
        float32.display(&ctx, &mut s)?;
        assert_eq!(s, "f32");

        let float64 = Ty::float64(&mut ctx);
        s.clear();
        float64.display(&ctx, &mut s)?;
        assert_eq!(s, "f64");

        let pointer = Ty::pointer(&mut ctx);
        s.clear();
        pointer.display(&ctx, &mut s)?;
        assert_eq!(s, "index");

        let array = Ty::array(&mut ctx, int32, 10);
        s.clear();
        array.display(&ctx, &mut s)?;
        assert_eq!(s, "[i32; 10]");

        let array_array = Ty::array(&mut ctx, array, 10);
        s.clear();
        array_array.display(&ctx, &mut s)?;
        assert_eq!(s, "[[i32; 10]; 10]");

        let struct_ty = Ty::struct_(&mut ctx, vec![int32, float32], false);
        s.clear();
        struct_ty.display(&ctx, &mut s)?;
        assert_eq!(s, "{i32, f32}");

        let packed_struct_ty = Ty::struct_(&mut ctx, vec![int32, float32], true);
        s.clear();
        packed_struct_ty.display(&ctx, &mut s)?;
        assert_eq!(s, "<{i32, f32}>");

        let simd = Ty::simd(&mut ctx, int32, 4);
        s.clear();
        simd.display(&ctx, &mut s)?;
        assert_eq!(s, "<i32; 4>");

        Ok(())
    }

    #[test]
    fn test_sig_display() {
        let mut ctx = Context::default();
        let int32 = Ty::int(&mut ctx, 32);
        let float32 = Ty::float32(&mut ctx);
        let void = Ty::void(&mut ctx);

        let sig = Signature::new(vec![int32, float32], void);
        let mut s = String::new();
        sig.display(&ctx, &mut s).unwrap();
        assert_eq!(s, "(i32, f32) -> void");
    }
}
