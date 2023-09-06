// Crates that have the "proc-macro" crate type are only allowed to export
// procedural macros. So we cannot have one crate that defines procedural macros
// alongside other types of public APIs like traits and structs.
//
// For this project we are going to need a #[bitfield] macro but also a trait
// and some structs. We solve this by defining the trait and structs in this
// crate, defining the attribute macro in a separate bitfield-impl crate, and
// then re-exporting the macro from this crate so that users only have one crate
// that they need to import.
//
// From the perspective of a user of this crate, they get all the necessary APIs
// (macro, trait, struct) through the one bitfield crate.
pub use bitfield_impl::{bitfield, BitfieldSpecifier};

/// Bitfield with specifier trait.
pub trait Specifier {
    /// Number of bits in the bitfield.
    const BITS: usize;

    /// Modulo 8 of the number of bits in the bitfield.
    type Mod8;

    /// Type of the bitfield.
    type Ty;

    /// Type of the unsigned type that can hold the bitfield.
    /// It is required that `Self::Ty` is convertible to `Self::UintTy`.
    type UintTy;
}

/// Specifier trait implementations for primitive types.
impl Specifier for u8 {
    const BITS: usize = 8;
    type Mod8 = checks::NumOfBitsMod8Is<0>;
    type Ty = Self;
    type UintTy = Self;
}

impl Specifier for u16 {
    const BITS: usize = 16;
    type Mod8 = checks::NumOfBitsMod8Is<0>;
    type Ty = Self;
    type UintTy = Self;
}

impl Specifier for u32 {
    const BITS: usize = 32;
    type Mod8 = checks::NumOfBitsMod8Is<0>;
    type Ty = Self;
    type UintTy = Self;
}

impl Specifier for u64 {
    const BITS: usize = 64;
    type Mod8 = checks::NumOfBitsMod8Is<0>;
    type Ty = Self;
    type UintTy = Self;
}

impl Specifier for bool {
    const BITS: usize = 1;
    type Mod8 = checks::NumOfBitsMod8Is<1>;
    type Ty = Self;
    type UintTy = u8;
}

/// Specialization for bool bitfield.
pub mod boolean {
    /// Unsafe black magic that will only cause UB when T is `bool`,
    /// which will never be executed.
    pub unsafe trait TypeMapper<T> {
        type Identity;
    }

    unsafe impl<T> TypeMapper<T> for u8 {
        type Identity = T;
    }
    unsafe impl<T> TypeMapper<T> for u16 {
        type Identity = T;
    }
    unsafe impl<T> TypeMapper<T> for u32 {
        type Identity = T;
    }
    unsafe impl<T> TypeMapper<T> for u64 {
        type Identity = T;
    }
    unsafe impl<T> TypeMapper<T> for bool {
        type Identity = bool;
    }
}

/// Generate bitfield specifier types B1 through B64 with the `seq` macro I've
/// written before!
use seq::seq;

macro_rules! impl_bitfield_specifier {
    ($t:ty, $start:expr, $end:expr) => {
        seq!(N in $start..=$end {
            /// Bitfield specifier type.
            pub enum B~N {}
            impl Specifier for B~N {
                const BITS: usize = N;
                type Mod8 = checks::NumOfBitsMod8Is<{ N % 8 }>;
                type Ty = $t;
                type UintTy = $t;
            }
        });
    };
}

impl_bitfield_specifier!(u8, 1, 8);
impl_bitfield_specifier!(u16, 9, 16);
impl_bitfield_specifier!(u32, 17, 32);
impl_bitfield_specifier!(u64, 33, 64);

pub mod checks {
    /// Modulo 8 of the number of bits in the bitfield.
    pub enum NumOfBitsMod8Is<const N: usize> {}

    use bitfield_impl::impl_size_check_helpers;
    impl_size_check_helpers! { NumOfBitsMod8Is, 8 }

    pub trait NumOfBitsDivisibleBy8 {
        type Marker;
    }
    impl NumOfBitsDivisibleBy8 for NumOfBitsMod8Is<0> {
        type Marker = ();
    }
}
