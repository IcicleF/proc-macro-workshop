#![allow(unused_imports)]

use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Group, Literal, TokenStream as TokenStream2, TokenTree};
use quote::{quote, quote_spanned};

use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{parse_macro_input, Ident, LitInt, Token};

use std::ops::Range;

/// Substitute the specified identifier in the given [`proc_macro2::TokenStream`] with the given value.
fn substitute_ident(input: TokenStream2, target: &Ident, value: usize) -> TokenStream2 {
    let input = input.into_iter().collect::<Vec<_>>();
    let mut output = Vec::with_capacity(input.len());

    let mut i = 0;
    while i < input.len() {
        let tt = &input[i];

        // Lookahead to detect `prefix~N` patterns.
        if let TokenTree::Ident(ident) = tt {
            if i + 2 < input.len() {
                if let TokenTree::Punct(pun) = &input[i + 1] {
                    if let TokenTree::Ident(placeholder) = &input[i + 2] {
                        if pun.as_char() == '~' && placeholder.to_string() == target.to_string() {
                            // Now we have the `prefix~N` pattern, further check if we have a `prefix~N_suffix` pattern.
                            if i + 4 < input.len() {
                                if let TokenTree::Punct(pun) = &input[i + 3] {
                                    if let TokenTree::Ident(suffix) = &input[i + 4] {
                                        if pun.as_char() == '~' {
                                            output.push(TokenTree::Ident(Ident::new(
                                                &format!("{}{}{}", ident, value, suffix),
                                                ident.span(),
                                            )));
                                            i += 5;
                                            continue;
                                        }
                                    }
                                }
                            }

                            // If we are here, we only have a `prefix~N` pattern.
                            output.push(TokenTree::Ident(Ident::new(
                                &format!("{}{}", ident, value),
                                ident.span(),
                            )));
                            i += 3;
                            continue;
                        }
                    }
                }
            }
        }

        output.push(match tt {
            TokenTree::Ident(ref ident) if ident.to_string() == target.to_string() => {
                TokenTree::Literal(Literal::usize_unsuffixed(value))
            }
            TokenTree::Group(ref group) => {
                let stream = group.stream();
                let stream = substitute_ident(stream, &target, value);
                TokenTree::Group(Group::new(group.delimiter(), stream))
            }
            _ => tt.clone(),
        });
        i += 1;
    }

    output.into_iter().collect()
}

/// The body of the sequence.
///
/// Body consists of disjoint parts, each part is a sequence of tokens.
/// The parts are separated by `#( ... )*` syntax.
/// Part 0 is not repeated, part 1 is repeated, part 2 is not repeated, part 3 is repeated, and so on.
/// However, if there is only one part, then the whole body should be repeated.
#[derive(Clone, Debug)]
struct Body {
    delimiter: Delimiter,
    parts: Vec<Part>,
}

impl Body {
    /// Create a new body from the given [`proc_macro2::TokenStream`].
    /// Detect the `#( ... )*` pattern and split the body into parts accordingly.
    fn new(body: TokenStream2, delimiter: Delimiter) -> Self {
        let input = body.into_iter().collect::<Vec<_>>();

        let mut parts: Vec<Part> = Vec::new();
        let mut current_part = Part::new();

        // Collect tokens into parts.
        let mut i = 0;
        while i < input.len() {
            let tt = &input[i];

            // Detect `#( ... )*` pattern.
            if let TokenTree::Punct(pun) = tt {
                if pun.as_char() == '#' {
                    if i + 2 < input.len() {
                        if let TokenTree::Group(group) = &input[i + 1] {
                            if let TokenTree::Punct(pun) = &input[i + 2] {
                                if pun.as_char() == '*' {
                                    // Here, we've detected a `#( ... )*` pattern.
                                    // Push the previous collected part.
                                    current_part.merge_tokens();
                                    parts.push(current_part);
                                    current_part = Part::new();

                                    // Push the repeated part.
                                    parts.push(Part {
                                        elems: vec![Elem::Tokens(group.stream())],
                                    });
                                    i += 3;
                                    continue;
                                }
                            }
                        }
                    }
                }
            }

            // `#( ... )*` pattern not detected, continue collecting tokens.
            match tt {
                TokenTree::Group(group) => {
                    let body = group.stream();
                    current_part.push(Elem::Body(Box::new(Body::new(body, group.delimiter()))));
                }
                _ => current_part.push(Elem::Tokens(quote! {#tt})),
            }
            i += 1;
        }

        // Push the last part.
        current_part.merge_tokens();
        parts.push(current_part);

        Self { delimiter, parts }
    }

    /// Determines if there is a `#( ... )*` pattern in the body.
    ///
    /// Criteria:
    ///   1. There are more than one parts.
    ///   2. The only part has a `#( ... )*` pattern.
    fn is_repeat_pattern_present(&self) -> bool {
        assert!(!self.parts.is_empty());
        self.parts.len() > 1 || self.parts[0].is_repeat_pattern_present()
    }

    /// Recursively substitute the given identifier in the body.
    /// Should only be called from `Part`.
    fn substitute_ident(&mut self, target: &Ident, value: usize) {
        for part in &mut self.parts {
            part.substitute_ident(target, value);
        }
    }

    /// Construct the part into a [`proc_macro2::TokenStream`].
    /// Should only be called from `Part`.
    fn as_token_stream(&self, has_delimiter: bool, seq: &Seq) -> TokenStream2 {
        let body = self
            .parts
            .iter()
            .enumerate()
            .map(|(i, part)| {
                if i % 2 == 0 {
                    part.as_token_stream(seq)
                } else {
                    seq.range()
                        .map(|idx| {
                            let mut part = part.clone();
                            part.substitute_ident(&seq.ident, idx);
                            part.as_token_stream(seq)
                        })
                        .collect()
                }
            })
            .collect();
        if has_delimiter {
            let body = Group::new(self.delimiter, body);
            quote! { #body }
        } else {
            quote! { #body }
        }
    }
}

/// A part is a series of elements.
/// Each element is either a token or a recursive body.
#[derive(Clone, Debug)]
struct Part {
    elems: Vec<Elem>,
}

impl Part {
    fn new() -> Self {
        Self { elems: Vec::new() }
    }

    fn push(&mut self, elem: Elem) {
        self.elems.push(elem);
    }

    /// Concatenate contiguous tokens into a single token stream.
    fn merge_tokens(&mut self) {
        let mut elems = Vec::with_capacity(self.elems.len());
        let mut i = 0;
        while i < self.elems.len() {
            let elem = &self.elems[i];
            if let Elem::Tokens(tokens) = elem {
                let mut tokens = tokens.clone();
                let mut j = i + 1;
                while j < self.elems.len() {
                    if let Elem::Tokens(tokens2) = &self.elems[j] {
                        tokens.extend(tokens2.clone());
                        j += 1;
                    } else {
                        break;
                    }
                }
                elems.push(Elem::Tokens(tokens.clone()));
                i = j;
            } else {
                elems.push(elem.clone());
                i += 1;
            }
        }
        self.elems = elems;
    }

    /// Determines if there is a `#( ... )*` pattern in the part.
    fn is_repeat_pattern_present(&self) -> bool {
        self.elems.iter().any(|elem| match elem {
            Elem::Body(body) => body.is_repeat_pattern_present(),
            _ => false,
        })
    }

    /// Recursively substitute the given identifier in the part.
    fn substitute_ident(&mut self, target: &Ident, value: usize) {
        let elems = self
            .elems
            .drain(..)
            .map(|elem| match elem {
                Elem::Body(mut body) => {
                    body.substitute_ident(target, value);
                    Elem::Body(body)
                }
                Elem::Tokens(tokens) => Elem::Tokens(substitute_ident(tokens, target, value)),
            })
            .collect();
        self.elems = elems;
    }

    /// Construct the part into a [`proc_macro2::TokenStream`].
    fn as_token_stream(&self, seq: &Seq) -> TokenStream2 {
        self.elems
            .iter()
            .map(|elem| match elem {
                Elem::Body(body) => body.as_token_stream(true, seq),
                Elem::Tokens(tokens) => tokens.clone(),
            })
            .collect()
    }
}

#[derive(Clone, Debug)]
enum Elem {
    /// Tokens.
    Tokens(TokenStream2),

    /// A recursive body.
    Body(Box<Body>),
}

struct Seq {
    /// The name of the sequence variable.
    ident: Ident,

    /// The start (inclusive) of the sequence.
    start: usize,

    /// The end (exclusive) of the sequence.
    end: usize,

    /// The body of the sequence.
    body: Body,
}

impl Seq {
    fn range(&self) -> Range<usize> {
        self.start..self.end
    }

    fn as_token_stream(&self) -> TokenStream2 {
        if self.body.is_repeat_pattern_present() {
            // If there is a `#( ... )*` pattern in the body, repeat the body.
            self.body.as_token_stream(false, self)
        } else {
            // Else, repeat the whole block.
            self.range()
                .map(|idx| {
                    let mut body = self.body.clone();
                    body.substitute_ident(&self.ident, idx);
                    body.as_token_stream(false, self)
                })
                .collect()
        }
    }
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse()?;
        input.parse::<Token![in]>()?;
        let start = input.parse::<LitInt>()?.base10_parse::<usize>()?;
        input.parse::<Token![..]>()?;

        // Handle `..=` syntax.
        let lookahead = input.lookahead1();
        let end = if lookahead.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            input.parse::<LitInt>()?.base10_parse::<usize>()? + 1
        } else {
            input.parse::<LitInt>()?.base10_parse::<usize>()?
        };

        let body;
        syn::braced!(body in input);

        let body = Body::new(body.parse::<TokenStream2>()?, Delimiter::Brace);
        Ok(Seq {
            ident,
            start,
            end,
            body,
        })
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let seq = parse_macro_input!(input as Seq);
    seq.as_token_stream().into()
}
