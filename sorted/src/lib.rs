#![allow(unused_variables)]

use std::fmt::Display;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::visit_mut::VisitMut;
use syn::{parse_macro_input, Attribute, ExprMatch, Item, ItemFn, Meta, Pat, Path};

/// Check whether an `Attribute` is `sorted`.
fn is_sorted_attr(attr: &Attribute) -> bool {
    let Meta::Path(path) = &attr.meta else {
        return false;
    };
    path.is_ident("sorted")
}

/// Get the string representation of a `Path`.
fn path_to_string(path: &Path) -> String {
    let res = path
        .segments
        .iter()
        .map(|s| s.ident.to_string())
        .collect::<Vec<_>>()
        .join("::");

    if path.leading_colon.is_some() {
        "::".to_owned() + &res
    } else {
        res
    }
}

/// Delegation function that checks a enum for sortedness.
fn check_enum_sorted(args: TokenStream2, input: Item) -> Result<TokenStream2, syn::Error> {
    let input = match input {
        Item::Enum(e) => e,
        _ => {
            return Err(syn::Error::new_spanned(
                args,
                "expected enum or match expression",
            ))
        }
    };

    let variants = input.variants.iter().map(|v| &v.ident).collect::<Vec<_>>();
    for i in 1..variants.len() {
        let prev = &variants[i - 1];
        let curr = &variants[i];

        // Out-of-order
        if prev.to_string() > curr.to_string() {
            // Search for the correct position, i.e., lower_bound
            let lb = variants
                .iter()
                .enumerate()
                .filter(|(_, v)| v.to_string() > curr.to_string())
                .next()
                .unwrap()
                .1;
            return Err(syn::Error::new_spanned(
                curr,
                format!("{} should sort before {}", curr, lb),
            ));
        }
    }

    Ok(quote! { #input })
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum MatchArmType {
    Named(String),
    Wild,
}

impl Display for MatchArmType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Named(s) => write!(f, "{}", s),
            Self::Wild => write!(f, "_"),
        }
    }
}

impl PartialOrd for MatchArmType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for MatchArmType {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Self::Wild, Self::Wild) => std::cmp::Ordering::Equal,
            (Self::Wild, _) => std::cmp::Ordering::Greater,
            (_, Self::Wild) => std::cmp::Ordering::Less,
            (Self::Named(a), Self::Named(b)) => a.cmp(b),
        }
    }
}

struct MatchArm<'a> {
    mat: MatchArmType,
    span: Box<&'a dyn ToTokens>,
}

impl Display for MatchArm<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.mat.fmt(f)
    }
}

impl<'a> MatchArm<'a> {
    fn new(pat: &'a Pat) -> Result<Self, syn::Error> {
        match pat {
            Pat::Ident(ident) => Ok(Self {
                mat: MatchArmType::Named(ident.ident.to_string()),
                span: Box::new(&ident.ident),
            }),
            Pat::Path(path) => Ok(Self {
                mat: MatchArmType::Named(path_to_string(&path.path)),
                span: Box::new(&path.path),
            }),
            Pat::Struct(s) => Ok(Self {
                mat: MatchArmType::Named(path_to_string(&s.path)),
                span: Box::new(&s.path),
            }),
            Pat::TupleStruct(ts) => Ok(Self {
                mat: MatchArmType::Named(path_to_string(&ts.path)),
                span: Box::new(&ts.path),
            }),
            Pat::Wild(_) => Ok(Self {
                mat: MatchArmType::Wild,
                span: Box::new(pat),
            }),
            _ => Err(syn::Error::new_spanned(pat, "unsupported by #[sorted]")),
        }
    }
}

impl PartialEq for MatchArm<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.mat == other.mat
    }
}

impl Eq for MatchArm<'_> {}

impl Ord for MatchArm<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.mat.cmp(&other.mat)
    }
}

impl PartialOrd for MatchArm<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// Delegation function that checks a enum for sortedness.
fn check_match_sorted(expr: &ExprMatch) -> Result<(), syn::Error> {
    let mut arms = Vec::with_capacity(expr.arms.len());
    for arm in &expr.arms {
        let pat = MatchArm::new(&arm.pat)?;
        arms.push(pat);
    }

    for i in 1..arms.len() {
        let prev = &arms[i - 1];
        let curr = &arms[i];

        // Out-of-order
        if prev > curr {
            // Search for the correct position, i.e., lower_bound
            let lb = arms
                .iter()
                .enumerate()
                .filter(|(_, v)| *v > curr)
                .next()
                .unwrap()
                .1;
            return Err(syn::Error::new_spanned(
                &curr.span,
                format!("{} should sort before {}", curr, lb),
            ));
        }
    }
    Ok(())
}

struct MatchVisitor {
    errors: Vec<TokenStream2>,
}

impl MatchVisitor {
    fn new() -> Self {
        Self { errors: Vec::new() }
    }
}

impl VisitMut for MatchVisitor {
    fn visit_expr_match_mut(&mut self, i: &mut ExprMatch) {
        // Detect if there is a #[sorted] attribute
        let has_sorted = i.attrs.iter().any(|a| is_sorted_attr(a));
        if !has_sorted {
            return;
        }

        // Remove the #[sorted] attribute
        i.attrs.retain(|a| !is_sorted_attr(a));

        // Check the match arms
        let res = check_match_sorted(i);
        if let Err(e) = res {
            self.errors.push(e.to_compile_error());
        }
    }
}

/// Enable the #[sorted] attribute on `match` expressions in the function body.
#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(input as ItemFn);

    // Traverse the function body looking for match-expressions.
    let mut visitor = MatchVisitor::new();
    visitor.visit_item_fn_mut(&mut input);

    // Construct the output TokenStream
    let errors = visitor.errors;
    let output = quote! {
        #input
        #( #errors )*
    };
    output.into()
}

/// Ensure that the arms of a enum or a match expression are sorted lexicographically.
#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Item);
    match check_enum_sorted(args.into(), input.clone()) {
        Ok(ts) => ts.into(),
        Err(e) => {
            let err = e.to_compile_error();
            quote! { #input #err }.into()
        }
    }
}
