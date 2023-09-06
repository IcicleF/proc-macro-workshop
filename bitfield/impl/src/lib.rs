use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote, quote_spanned};

use syn::spanned::Spanned;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, DeriveInput, Expr, Lit, Ident, Item, LitInt, Token, Type, Meta};

/// Generate accessors for a field in a bitfield based on its name,
/// offset, and bit width. The name should be a [`Ident`], and the
/// other two should be [`proc_macro2::TokenStream`]s that evaluate
/// to the desired values.
fn accessor(field: &Ident, ty: &Type, offset: &TokenStream2) -> TokenStream2 {
    let getter = format_ident!("get_{}", field);
    let setter = format_ident!("set_{}", field);
    quote! {
        pub fn #getter(&self) -> <#ty as bitfield::Specifier>::Ty {
            const OFFSET: usize = #offset;
            const START_BYTE: usize = OFFSET / 8;
            const END_BIT: usize = OFFSET + <#ty as bitfield::Specifier>::BITS;
            const END_BYTE: usize = (END_BIT - 1) / 8;

            let mut ret: <#ty as bitfield::Specifier>::UintTy = 0u8.into();

            if START_BYTE == END_BYTE {
                let mask = { (1 << { if END_BIT % 8 == 0 { 8 } else { END_BIT % 8 } }) - (1 << { OFFSET % 8 }) } as u8;
                ret = (((self.data[START_BYTE] & mask) >> (OFFSET % 8)) as u8).into();
            } else {
                for i in START_BYTE..=END_BYTE {
                    let data = self.data[i] as <#ty as bitfield::Specifier>::UintTy;
                    ret |= match i {
                        START_BYTE => {
                            let mask = 0b1111_1111 << (OFFSET % 8);
                            (data & mask) >> (OFFSET % 8)
                        },
                        END_BYTE => {
                            let mask = if END_BIT % 8 == 0 { 0b1111_1111 } else { (1 << (END_BIT % 8)) - 1 };
                            let data = data & mask;
                            data << ((END_BYTE * 8) - OFFSET)
                        }
                        _ => data << ((i * 8) - OFFSET),
                    };
                }
            }

            {
                use std::any::*;
                if false.type_id() == TypeId::of::<<#ty as bitfield::Specifier>::Ty>() {
                    (ret != 0).into()
                } else {
                    // Unsafe black magic that will do no harm
                    let ret: <<#ty as bitfield::Specifier>::Ty as bitfield::boolean::TypeMapper<<#ty as bitfield::Specifier>::UintTy>>::Identity = unsafe { std::mem::transmute(ret) };
                    ret.into()
                }
            }
        }

        pub fn #setter(&mut self, val: <#ty as bitfield::Specifier>::Ty) {
            const OFFSET: usize = #offset;
            const START_BYTE: usize = OFFSET / 8;
            const END_BIT: usize = OFFSET + <#ty as bitfield::Specifier>::BITS;
            const END_BYTE: usize = (END_BIT - 1) / 8;

            let mut val: <#ty as bitfield::Specifier>::UintTy = val as _;

            // Truncate `val` if it could be too large
            if <#ty as bitfield::Specifier>::BITS != std::mem::size_of::<<#ty as bitfield::Specifier>::Ty>() * 8 {
                val = val & ((1 << <#ty as bitfield::Specifier>::BITS) - 1);
            }
            if START_BYTE == END_BYTE {
                let mask = { (1 << { if END_BIT % 8 == 0 { 8 } else { END_BIT % 8 } }) - (1 << { OFFSET % 8 }) } as u8;
                self.data[START_BYTE] = (self.data[START_BYTE] & !mask) | (val << (OFFSET % 8)) as u8;
            } else {
                for i in START_BYTE..=END_BYTE {
                    match i {
                        START_BYTE => {
                            let mask = 0b1111_1111 << (OFFSET % 8);
                            let val = ((val << (OFFSET - START_BYTE * 8)) & 0b1111_1111) as u8;
                            self.data[i] = (self.data[i] & !mask) | val;
                        }
                        END_BYTE => {
                            let mask = if END_BIT % 8 == 0 { 0b1111_1111 } else { 1 << (END_BIT % 8) - 1 };
                            let val = (val >> (i * 8 - OFFSET)) as u8;
                            self.data[i] = (self.data[i] & !mask) | val;
                        }
                        _ => self.data[i] = ((val >> (i * 8 - OFFSET)) & 0b1111_1111) as u8,
                    };
                }
            }
        }
    }
}

#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let input = parse_macro_input!(input as Item);
    let Item::Struct(input) = input else {
        return syn::Error::new_spanned(input, "expected struct")
            .to_compile_error()
            .into();
    };

    // Collect necessary information
    let input_item_name = &input.ident;

    // Generate a formula that calculates the sum of bitfield widths
    let total_bits = {
        let elems = input.fields.iter().map(|f| {
            let t = &f.ty;
            quote! { <#t as bitfield::Specifier>::BITS }
        });
        quote! { #( #elems )+* }
    };

    // Generate a helper trait that holds the necessary associated constants for us
    let helper_trait_name = format_ident!("{}Helper", input_item_name);
    let helper_trait_def = quote! {
        trait #helper_trait_name {
            /// Total number of bits in the bitfield.
            const TOTAL_BITS: usize;

            /// Total number of bytes in the bitfield.
            const TOTAL_BYTES: usize;
        }
    };
    let helper_trait_impl = quote! {
        impl #helper_trait_name for #input_item_name {
            const TOTAL_BITS: usize = #total_bits;
            const TOTAL_BYTES: usize = Self::TOTAL_BITS / 8;
        }
    };

    // Generate the mod type for the input type
    let mut mod_type = quote! { bitfield::checks::NumOfBitsMod8Is<0> };
    input.fields.iter().for_each(|f| {
        let t = &f.ty;
        mod_type = quote! {
            <<#t as bitfield::Specifier>::Mod8 as bitfield::checks::ModAdd<#mod_type>>::Output
        }
    });

    // Generate the modified struct definition
    let output_struct_def = quote! {
        #[repr(C)]
        pub struct #input_item_name {
            data: [u8; <Self as #helper_trait_name>::TOTAL_BYTES],
            _marker: <#mod_type as bitfield::checks::NumOfBitsDivisibleBy8>::Marker,
        }
    };

    // Generate accessors for each field
    let mut offset = quote! { 0 };
    let mut accessors = Vec::new();
    for f in input.fields.iter() {
        let Some(ref ident) = &f.ident else {
            return syn::Error::new_spanned(&f.ident, "expected named field")
                .to_compile_error()
                .into();
        };
        let ty = &f.ty;

        accessors.push(accessor(&ident, ty, &offset));
        offset = quote! { #offset + <#ty as bitfield::Specifier>::BITS };
    }

    // Generate the impl block for the struct
    let output_struct_impl = quote! {
        #[allow(unreachable_patterns)]
        impl #input_item_name {
            pub fn new() -> Self {
                unsafe { std::mem::zeroed() }
            }

            #( #accessors )*
        }
    };

    // Collect information of `#[bits = ...]` attributes
    let mut checked_fields = Vec::new();
    for f in input.fields.iter().filter(|f| f.attrs.iter().any(|a| a.meta.path().is_ident("bits"))) {
        let attr = f.attrs.iter().find(|a| a.meta.path().is_ident("bits")).unwrap();
        let Meta::NameValue(ref nv) = attr.meta else {
            return syn::Error::new_spanned(attr, "expected #[bits = ...]")
                .to_compile_error()
                .into();
        };
        let Expr::Lit(ref expr) = nv.value else {
            return syn::Error::new_spanned(attr, "expected #[bits = <integer literal>]")
                .to_compile_error()
                .into();
        };
        let Lit::Int(ref lit) = expr.lit else {
            return syn::Error::new_spanned(attr, "expected #[bits = <integer literal>]")
                .to_compile_error()
                .into();
        };
        checked_fields.push((lit, &f.ty));
    }

    // Generate check type
    let check_type_name = format_ident!("{}Size", input_item_name);
    let check_type_def = {
        let fields = checked_fields.iter().enumerate().map(|(i, (lit, _))| {
            let field_name = format_ident!("field{}", i);
            let span = lit.span();
            quote_spanned! {span=>
                #field_name: [(); #lit],
            }
        });
        quote! {
            #[allow(dead_code)]
            struct #check_type_name {
                #( #fields )*
            }
        }
    };
    let check_type_impl = {
        let fields = checked_fields.iter().enumerate().map(|(i, (lit, ty))| {
            let field_name = format_ident!("field{}", i);
            let span = lit.span();
            quote_spanned! {span=>
                #field_name: [(); <#ty as bitfield::Specifier>::BITS],
            }
        });
        quote! {
            #[allow(dead_code)]
            impl #check_type_name {
                fn check() {
                    let _ = Self {
                        #( #fields )*
                    };
                }
            }
        }
    };

    // Generate the output
    let output = quote! {
        #output_struct_def
        #output_struct_impl
        #helper_trait_def
        #helper_trait_impl

        #check_type_def
        #check_type_impl
    };
    output.into()
}

/// Make the enum a bitfield member.
#[proc_macro_derive(BitfieldSpecifier)]
pub fn bitfield_specifier(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let input_type_name = &input.ident;

    let syn::Data::Enum(data) = &input.data else {
        return syn::Error::new_spanned(input, "expected enum")
            .to_compile_error()
            .into();
    };

    // Compute the number of bits from the number of variants
    let bits = data.variants.len().next_power_of_two().trailing_zeros() as usize;
    if (1 << bits) != data.variants.len() {
        return syn::Error::new(
            proc_macro2::Span::call_site(),
            "BitfieldSpecifier expected a number of variants which is a power of 2",
        )
        .to_compile_error()
        .into();
    }

    let bits_lit = LitInt::new(&bits.to_string(), proc_macro2::Span::call_site());
    let bits_lit_mod8 = LitInt::new(&format!("{}", bits % 8), proc_macro2::Span::call_site());
    let uint_ty = match bits {
        0..=8 => quote! { u8 },
        9..=16 => quote! { u16 },
        17..=32 => quote! { u32 },
        33..=64 => quote! { u64 },
        _ => unreachable!(),
    };

    // Generate `Specifier` impl for the type
    let specifier_impl = quote! {
        impl bitfield::Specifier for #input_type_name {
            const BITS: usize = #bits_lit;
            type Mod8 = bitfield::checks::NumOfBitsMod8Is<#bits_lit_mod8>;
            type Ty = Self;
            type UintTy = #uint_ty;
        }
    };

    // Generate `Choose` impl for the type
    let type_mapper_impl = quote! {
        unsafe impl<T> bitfield::boolean::TypeMapper<T> for #input_type_name {
            type Identity = T;
        }
    };

    // Generate `From<uint>` impl for the type
    let from_uint_variants = data.variants.iter().map(|v| {
        let ident = &v.ident;
        quote! { x if x == (Self::#ident as _) => Self::#ident }
    });
    let from_uint_impl = quote! {
        impl From<<#input_type_name as bitfield::Specifier>::UintTy> for #input_type_name {
            fn from(val: <#input_type_name as bitfield::Specifier>::UintTy) -> Self {
                match val {
                    #( #from_uint_variants, )*
                    _ => unreachable!(),
                }
            }
        }
    };

    // Generate `From<bool>` dummy impl for the type
    let from_bool_impl = quote! {
        impl From<bool> for #input_type_name {
            fn from(val: bool) -> Self {
                unreachable!()
            }
        }
    };

    // Generate helpers that detects whether the discriminant is in range
    let in_range_check_mod = format_ident!("{}_in_range_check", input_type_name);
    let in_range_check_trait_name = format_ident!("{}DiscriminantInRange", input_type_name);
    let in_range_check_struct_name = format_ident!("{}Discriminant", input_type_name);
    let in_range_check_struct_def = quote! {
        pub struct #in_range_check_struct_name<const N: usize> {}
    };
    let in_range_check_trait_def = quote! {
        pub trait #in_range_check_trait_name {
            type Output;
        }
    };
    let in_range_check_trait_impl = {
        let impls = (0..(1 << bits)).map(|i| {
            let lit = LitInt::new(&i.to_string(), proc_macro2::Span::call_site());
            quote! {
                impl #in_range_check_trait_name for #in_range_check_struct_name<#lit> {
                    type Output = ();
                }
            }
        });
        quote! {
            #( #impls )*
        }
    };

    let in_range_check_executor_name = format_ident!("{}DiscriminantCheck", input_type_name);
    let in_range_check_executor = {
        let markers = data.variants.iter().enumerate().map(|(i, v)| {
            let ident = &v.ident;
            let marker = format_ident!("marker{}", i);
            let span = v.span();
            quote_spanned! {span=>
                #marker: <#in_range_check_struct_name<{ #input_type_name::#ident as usize }> as #in_range_check_trait_name>::Output 
            }
        });
        quote! {
            struct #in_range_check_executor_name {
                #( #markers, )*
            }
        }
    };

    let output = quote! {
        #specifier_impl
        #type_mapper_impl
        #from_uint_impl
        #from_bool_impl

        #[allow(non_snake_case)]
        mod #in_range_check_mod {
            use super::*;
            #in_range_check_struct_def
            #in_range_check_trait_def
            #in_range_check_trait_impl
            #in_range_check_executor
        }
    };
    output.into()
}

#[proc_macro]
pub fn impl_size_check_helpers(input: TokenStream) -> TokenStream {
    struct Input {
        ident: Ident,
        n: usize,
    }

    impl Parse for Input {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let ident = input.parse::<Ident>()?;
            input.parse::<Token![,]>()?;
            let n = input.parse::<LitInt>()?.base10_parse()?;
            Ok(Self { ident, n })
        }
    }

    let input = parse_macro_input!(input as Input);
    let n = input.n;

    // Generate adder type
    let adder_trait_def = quote! {
        pub trait ModAdd<Rhs> {
            type Output;
        }
    };

    fn make_mod_type(ident: &Ident, m: usize) -> TokenStream2 {
        let lit = LitInt::new(&m.to_string(), ident.span());
        quote! { #ident<#lit> }
    }

    // Generate adder impl
    let mut adder_impl = Vec::new();
    for i in 0..n {
        for j in 0..n {
            let lhs = make_mod_type(&input.ident, i);
            let rhs = make_mod_type(&input.ident, j);
            let sum = make_mod_type(&input.ident, (i + j) % n);
            adder_impl.push(quote! {
                impl ModAdd<#rhs> for #lhs {
                    type Output = #sum;
                }
            });
        }
    }

    let output = quote! {
        #adder_trait_def
        #( #adder_impl )*
    };
    output.into()
}
