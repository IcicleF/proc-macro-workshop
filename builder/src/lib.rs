use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, Expr, GenericArgument, Ident, PathArguments, Type};

enum MemberBuildType<'a> {
    /// A normal required field.
    Normal,

    /// An optional field denoted by `Option<T>`.
    Optional(&'a Type),

    /// An appendable field denoted by `builder(each = "...")`.
    Each(String, &'a Type),
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    enum MemberBuildTypeTemp<'a> {
        Normal,
        Special(&'a Ident, &'a Type),
        Each(String, &'a Type),
    }

    impl<'a> From<MemberBuildTypeTemp<'a>> for MemberBuildType<'a> {
        fn from(temp: MemberBuildTypeTemp<'a>) -> Self {
            match temp {
                MemberBuildTypeTemp::Normal => MemberBuildType::Normal,
                MemberBuildTypeTemp::Special(ident, ty) => {
                    if ident == "Option" {
                        MemberBuildType::Optional(ty)
                    } else if ident == "Vec" {
                        MemberBuildType::Normal
                    } else {
                        unreachable!()
                    }
                }
                MemberBuildTypeTemp::Each(builder_name, ty) => {
                    MemberBuildType::Each(builder_name, ty)
                }
            }
        }
    }

    let input = parse_macro_input!(input as syn::DeriveInput);
    let data_struct = match &input.data {
        Data::Struct(ds) => ds,
        _ => {
            return TokenStream::from(syn::Error::into_compile_error(syn::Error::new_spanned(
                &input,
                "only structs are supported for the `Builder` macro",
            )))
        }
    };

    let target_type_ident = &input.ident;
    let target_type_vis = input.vis.clone();

    // Generate member info for each field
    let mut member_info = Vec::new();
    for f in data_struct.fields.iter() {
        let member_ident = f.ident.as_ref().unwrap();
        let member_type = &f.ty;
        let mut member_build_type = MemberBuildTypeTemp::Normal;

        // Determine if the member type is an Option<T> or Vec<T>
        if let Type::Path(type_path) = member_type {
            if let Some(segment) = type_path.path.segments.first() {
                if segment.ident == "Option" || segment.ident == "Vec" {
                    if let PathArguments::AngleBracketed(args) = &segment.arguments {
                        if args.args.len() == 1 {
                            if let GenericArgument::Type(ty) = args.args.first().unwrap() {
                                member_build_type =
                                    MemberBuildTypeTemp::Special(&segment.ident, ty);
                            }
                        }
                    }
                }
            }
        }

        // Determine if the member has a `builder(each = "...")` attribute
        for attr in &f.attrs {
            if attr.path().is_ident("builder") {
                // Check attribute format sanity
                let meta = attr.parse_args::<Expr>().unwrap();
                let Expr::Assign(assign_expr) = &meta else {
                    return TokenStream::from(syn::Error::into_compile_error(
                        syn::Error::new_spanned(&attr.meta, "expected `builder(each = \"...\")`"),
                    ));
                };
                let Expr::Path(ref lhs_path) = *(assign_expr.left) else {
                    return TokenStream::from(syn::Error::into_compile_error(
                        syn::Error::new_spanned(&attr.meta, "expected `builder(each = \"...\")`"),
                    ));
                };
                let Expr::Lit(ref rhs_lit) = *(assign_expr.right) else {
                    return TokenStream::from(syn::Error::into_compile_error(
                        syn::Error::new_spanned(meta, "expected `builder(each = \"...\")`"),
                    ));
                };

                // Check attribute value sanity
                if !lhs_path.path.is_ident("each") {
                    return TokenStream::from(syn::Error::into_compile_error(
                        syn::Error::new_spanned(&attr.meta, "expected `builder(each = \"...\")`"),
                    ));
                };
                let lit_str = match rhs_lit.lit {
                    syn::Lit::Str(ref lit_str) => lit_str,
                    _ => {
                        return TokenStream::from(syn::Error::into_compile_error(
                            syn::Error::new_spanned(
                                &attr.meta,
                                "expected `builder(each = \"...\")`",
                            ),
                        ))
                    }
                };

                let builder_name = lit_str.value();
                let MemberBuildTypeTemp::Special(_, ty) = member_build_type else {
                    unreachable!()
                };
                member_build_type = MemberBuildTypeTemp::Each(builder_name, ty);

                break;
            }
        }

        let elem = (
            member_ident,
            member_type,
            MemberBuildType::from(member_build_type),
        );
        member_info.push(elem);
    }
    let member_info = member_info;

    // Generate the builder type
    let builder_type_ident = format_ident!("{}Builder", target_type_ident);
    let builder_type_members =
        member_info
            .iter()
            .map(
                |(member_ident, member_type, member_build_type)| match member_build_type {
                    MemberBuildType::Optional(data_type) => quote! {
                        #member_ident: std::option::Option<#data_type>,
                    },
                    MemberBuildType::Each(_, data_type) => quote! {
                        #member_ident: std::vec::Vec<#data_type>,
                    },
                    MemberBuildType::Normal => quote! {
                        #member_ident: std::option::Option<#member_type>,
                    },
                },
            );
    let builder_type_def = quote! {
        #target_type_vis struct #builder_type_ident {
            #( #builder_type_members )*
        }
    };

    // Generate method impl on the original type to get the builder
    let builder_type_members =
        member_info.iter().map(
            |(member_ident, _, member_build_type)| match member_build_type {
                MemberBuildType::Optional(_) | MemberBuildType::Normal => quote! {
                    #member_ident: std::option::Option::None,
                },
                MemberBuildType::Each(_, _) => quote! {
                    #member_ident: std::vec::Vec::new(),
                },
            },
        );
    let builder_fn = quote! {
        impl #target_type_ident {
            pub fn builder() -> #builder_type_ident {
                #builder_type_ident {
                    #( #builder_type_members )*
                }
            }
        }
    };

    // Generate methods on the builder for setting a value of each of the struct fields
    let builder_setters =
        member_info
            .iter()
            .map(
                |(member_ident, member_type, member_build_type)| match member_build_type {
                    MemberBuildType::Optional(data_type) => quote! {
                        pub fn #member_ident(&mut self, #member_ident: #data_type) -> &mut Self {
                            self.#member_ident = std::option::Option::Some(#member_ident);
                            self
                        }
                    },
                    MemberBuildType::Each(builder_name, data_type) => {
                        let builder_ident = format_ident!("{}", builder_name);
                        quote! {
                            pub fn #builder_ident(&mut self, elem: #data_type) -> &mut Self {
                                self.#member_ident.push(elem);
                                self
                            }
                        }
                    }
                    MemberBuildType::Normal => quote! {
                        pub fn #member_ident(&mut self, #member_ident: #member_type) -> &mut Self {
                            self.#member_ident = Some(#member_ident);
                            self
                        }
                    },
                },
            );
    let builder_setter_impl = quote! {
        impl #builder_type_ident {
            #( #builder_setters )*
        }
    };

    // Generate the build method
    let builder_build_fields = member_info
        .iter()
        .map(|(member_ident, _, member_build_type)| {
            let err_msg = format!("missing field `{}`", member_ident.to_string());
            match member_build_type {
                MemberBuildType::Optional(_) | MemberBuildType::Each(_, _) => quote! {
                    #member_ident: self.#member_ident.clone(),
                },
                MemberBuildType::Normal => quote! {
                    #member_ident: self.#member_ident.clone().ok_or_else(|| #err_msg)?,
                },
            }
        });
    let builder_build = quote! {
        impl #builder_type_ident {
            pub fn build(&mut self) -> std::result::Result<#target_type_ident, std::boxed::Box<dyn std::error::Error>> {
                Ok(#target_type_ident {
                    #( #builder_build_fields )*
                })
            }
        }
    };

    // Construct the resulting token stream
    let output = quote! {
        #builder_type_def
        #builder_fn
        #builder_setter_impl
        #builder_build
    };
    TokenStream::from(output)
}
