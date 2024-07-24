use proc_macro::TokenStream;
use quote::quote;
use syn::{parse::Parse, parse_macro_input, Expr};

struct VerifyToken {
    token: Expr,
    expected: Vec<Expr>,
}

impl Parse for VerifyToken {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let token = input.parse()?;

        input
            .parse::<syn::Token![,]>()
            .expect("Expected comma after token.");

        let expected = input
            .parse_terminated(Expr::parse, syn::Token![,])
            .expect("Expected a list of expected values.");
        Ok(VerifyToken {
            token,
            expected: expected.into_iter().collect(),
        })
    }
}

#[proc_macro]
/// Verifies that the provided token matches the expected tokens.
/// Throws an error and calls return if it fails.
pub fn verify_token(_input: TokenStream) -> TokenStream {
    let VerifyToken { token, expected } = parse_macro_input!(_input as VerifyToken);

    let expanded = quote! {
        if !matches!(#token.kind, #(#expected)|*) {
            let expected = vec![#(#expected.to_string()),*];
            return Err(Error::expected_token(#token.kind.to_string(), expected, #token.span));
        }
    };

    TokenStream::from(expanded)
}

#[proc_macro]
pub fn verify_ident(_input: TokenStream) -> TokenStream {
    let ident = parse_macro_input!(_input as Expr);

    let expanded = quote! {
        if !matches!(#ident.kind, TokenKind::Identifier(_)) {
            return Err(Error::expected_token(#ident.kind.to_string(), vec!["identifier".to_string()], #ident.span));
        }
    };

    TokenStream::from(expanded)
}
