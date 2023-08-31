use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, parse_str, Data, Expr, GenericArgument, GenericParam, Ident, Lit, Meta,
    PathArguments, Type, TypePath, WhereClause,
};

/// Determines inferred trait bound on `CustomDebug`.
enum TraitBoundType {
    /// A normal trait bound, e.g. `T: Debug`.
    /// This indicates that the type `T` is directly used in a field of the struct.
    /// Generate a trait bound for the type.
    Normal(Ident),

    /// An associated type of a trait.
    /// This indicates that the type `T` is not directly used in a field of the struct,
    /// but used in the form of `T::AssocType` in a field of the struct.
    /// Generate a trait bound for the associated type.
    AssocType(TypePath),
}

/// Determine if the given type contains the given generic type parameter.
fn type_count_generic_occurrences(ty: &Type, generic: &Ident) -> usize {
    match ty {
        Type::Paren(tp) => type_count_generic_occurrences(&tp.elem, generic),
        Type::Path(tp) => tp
            .path
            .segments
            .iter()
            .map(|s| {
                let args = &s.arguments;
                let cur = if s.ident == generic.to_string() { 1 } else { 0 };
                cur + match args {
                    PathArguments::AngleBracketed(ab) => ab
                        .args
                        .iter()
                        .map(|arg| match arg {
                            GenericArgument::Type(ty) => {
                                type_count_generic_occurrences(ty, generic)
                            }
                            _ => 0,
                        })
                        .sum(),
                    _ => 0,
                }
            })
            .sum(),
        Type::Slice(ts) => type_count_generic_occurrences(&ts.elem, generic),
        Type::Tuple(tt) => tt
            .elems
            .iter()
            .map(|ty| type_count_generic_occurrences(ty, generic))
            .sum(),
        _ => 0,
    }
}

/// Extract associated type information from the given type and the given generic type parameter.
fn find_type_associates(ty: &Type, generic: &Ident) -> Vec<TypePath> {
    let mut assoc = Vec::new();
    match ty {
        Type::Paren(tp) => return find_type_associates(&tp.elem, generic),
        Type::Slice(ts) => return find_type_associates(&ts.elem, generic),

        // Below arms do not diverge and push to `assoc`
        Type::Path(tp) => {
            if tp.path.segments.len() == 2 && tp.path.segments[0].ident == generic.to_string() {
                return vec![tp.clone()];
            }

            tp.path.segments.iter().for_each(|s| {
                if let PathArguments::AngleBracketed(ab) = &s.arguments {
                    ab.args.iter().for_each(|arg| {
                        if let GenericArgument::Type(ty) = arg {
                            assoc.extend(find_type_associates(ty, generic));
                        }
                    });
                }
            });
        }
        Type::Tuple(tt) => tt.elems.iter().for_each(|ty| {
            let ret = find_type_associates(ty, generic);
            assoc.extend(ret);
        }),
        _ => {}
    }
    assoc
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);

    // Collect target type information
    let data_struct = match &input.data {
        Data::Struct(ds) => ds,
        _ => {
            return TokenStream::from(syn::Error::into_compile_error(syn::Error::new_spanned(
                &input,
                "only structs are supported for this `CustomDebug` derive macro",
            )))
        }
    };
    let mut manual_bounds = None;
    for attr in &input.attrs {
        if attr.path().is_ident("debug") {
            // Check attribute format sanity
            let meta = attr.parse_args::<Expr>().unwrap();
            let Expr::Assign(assign_expr) = &meta else {
                return TokenStream::from(syn::Error::into_compile_error(syn::Error::new_spanned(
                    &attr.meta,
                    "expected `debug(bound = \"...\")`",
                )));
            };
            let Expr::Path(ref lhs_path) = *(assign_expr.left) else {
                return TokenStream::from(syn::Error::into_compile_error(syn::Error::new_spanned(
                    &attr.meta,
                    "expected `debug(bound = \"...\")`",
                )));
            };
            let Expr::Lit(ref rhs_lit) = *(assign_expr.right) else {
                return TokenStream::from(syn::Error::into_compile_error(syn::Error::new_spanned(
                    meta,
                    "expected `debug(bound = \"...\")`",
                )));
            };

            // Check attribute value sanity
            if !lhs_path.path.is_ident("bound") {
                return TokenStream::from(syn::Error::into_compile_error(syn::Error::new_spanned(
                    &attr.meta,
                    "expected `debug(bound = \"...\")`",
                )));
            };
            let lit_str = match rhs_lit.lit {
                syn::Lit::Str(ref lit_str) => lit_str,
                _ => {
                    return TokenStream::from(syn::Error::into_compile_error(
                        syn::Error::new_spanned(&attr.meta, "expected `debug(bound = \"...\")`"),
                    ))
                }
            };

            manual_bounds = Some(lit_str.value());
        }
    }

    // - 1. Generic type parameters
    let generics = &input.generics;
    let mut generic_type_params = generics
        .params
        .iter()
        .filter_map(|param| match param {
            GenericParam::Type(ty) => Some((&ty.ident, false, Vec::new())),
            _ => None,
        })
        .collect::<Vec<_>>();
    let generic_types = generic_type_params
        .iter()
        .map(|(ty, _, _)| quote! { #ty, })
        .collect::<Vec<_>>();

    // - 2. Fields
    let target_type_ident = &input.ident;
    let mut member_info = Vec::new();
    for f in data_struct.fields.iter() {
        let member_ident = f.ident.as_ref().unwrap();
        let member_type = &f.ty;

        // Invoke trait bound inference only when manual bounds are not present
        if manual_bounds.is_none() {
            // Determine if the type is a PhantomData
            let is_phantom_data = match member_type {
                syn::Type::Path(tp) => {
                    let path = &tp.path;
                    path.leading_colon.is_none()
                        && path.segments.len() == 1
                        && path.segments[0].ident == "PhantomData"
                }
                _ => false,
            };

            // Determine if the type mentions any generic type parameters
            for i in 0..generic_type_params.len() {
                let (generic, has_normal, assoc_types) = &mut generic_type_params[i];
                let occurrences = type_count_generic_occurrences(member_type, &generic);
                if occurrences > 0 && !is_phantom_data {
                    // Associated type does not directly use the generic type parameter and require `Debug` on it
                    let assocs = find_type_associates(member_type, &generic);
                    assoc_types.extend(assocs);

                    if occurrences > assoc_types.len() {
                        *has_normal = true;
                    }
                }
            }
        }

        // Determine if the member has a `debug = "..."` attribute
        let mut customized_debug = None;
        for attr in &f.attrs {
            if attr.path().is_ident("debug") {
                // Check attribute format sanity
                let Meta::NameValue(nv) = &attr.meta else {
                    return TokenStream::from(syn::Error::into_compile_error(
                        syn::Error::new_spanned(&attr.meta, "expected #[debug = \"...\"]"),
                    ));
                };
                let Expr::Lit(lit) = &nv.value else {
                    return TokenStream::from(syn::Error::into_compile_error(
                        syn::Error::new_spanned(&attr.meta, "expected #[debug = \"...\"]"),
                    ));
                };
                let Lit::Str(format_string) = &lit.lit else {
                    return TokenStream::from(syn::Error::into_compile_error(
                        syn::Error::new_spanned(&attr.meta, "expected #[debug = \"...\"]"),
                    ));
                };

                customized_debug = Some(format_string.value());
            }
        }

        let elem = (member_ident, member_type, customized_debug);
        member_info.push(elem);
    }
    let member_info = member_info;

    // - 3. Summarize inferred trait bounds
    let inferred_bounded_types = if manual_bounds.is_none() {
        generic_type_params
            .into_iter()
            .map(|(generic, has_normal, assoc_types)| {
                let mut types = assoc_types
                    .into_iter()
                    .map(|at| TraitBoundType::AssocType(at))
                    .collect::<Vec<_>>();
                if has_normal {
                    types.push(TraitBoundType::Normal(generic.clone()));
                }
                types.into_iter()
            })
            .flatten()
            .collect::<Vec<_>>()
    } else {
        Vec::new()
    };

    // Generate Debug trait implementation for the input struct fields
    let debug_fields =
        member_info
            .iter()
            .map(|(ident, _, customized_debug)| match customized_debug {
                Some(format_string) => {
                    quote! {
                        .field(stringify!(#ident), &format_args!(#format_string, &self.#ident))
                    }
                }
                None => {
                    quote! {
                        .field(stringify!(#ident), &self.#ident)
                    }
                }
            });

    // Generate trait bounds for the type generics
    let inferred_trait_bounds = match manual_bounds {
        Some(bound) => {
            let bound = "where ".to_string() + &bound;
            let bound = parse_str::<WhereClause>(&bound);
            if bound.is_err() {
                return TokenStream::from(syn::Error::into_compile_error(syn::Error::new_spanned(
                    &input,
                    "failed to parse the bounds in `debug(bound = \"...\")`",
                )));
            }
            let bound = bound.unwrap();
            quote! { #bound }
        }
        None => {
            let bounds = inferred_bounded_types
                .iter()
                .map(|ty| match ty {
                    TraitBoundType::Normal(generic) => quote! { #generic: std::fmt::Debug, },
                    TraitBoundType::AssocType(assoc_type) => {
                        quote! { #assoc_type: std::fmt::Debug, }
                    }
                })
                .collect::<Vec<_>>();
            quote! { where #( #bounds )* }
        }
    };

    // Generate the Debug impl
    let debug_impl = quote! {
        impl #generics std::fmt::Debug for #target_type_ident <#( #generic_types )*>
        #inferred_trait_bounds
        {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!(#target_type_ident))
                    #( #debug_fields )*
                    .finish()
            }
        }
    };

    let output = quote! {
        #debug_impl
    };
    TokenStream::from(output)
}
