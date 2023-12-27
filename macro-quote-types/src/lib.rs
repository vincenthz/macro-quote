#![no_std]
extern crate alloc;

extern crate proc_macro;

pub mod ext;

use alloc::{vec, vec::Vec};
use proc_macro::{Literal, TokenTree};

pub trait ToTokenTrees {
    fn generate(ty: Self) -> Vec<TokenTree>;
}

impl ToTokenTrees for &str {
    fn generate(ty: Self) -> Vec<TokenTree> {
        vec![Literal::string(ty).into()]
    }
}

impl ToTokenTrees for alloc::string::String {
    fn generate(ty: Self) -> Vec<TokenTree> {
        vec![Literal::string(ty.as_str()).into()]
    }
}

impl ToTokenTrees for u32 {
    fn generate(ty: Self) -> Vec<TokenTree> {
        vec![Literal::u32_suffixed(ty).into()]
    }
}

impl ToTokenTrees for proc_macro::TokenStream {
    fn generate(ty: Self) -> Vec<TokenTree> {
        ty.into_iter().collect::<Vec<_>>()
    }
}

impl ToTokenTrees for proc_macro::Literal {
    fn generate(lit: Self) -> Vec<TokenTree> {
        vec![lit.into()]
    }
}

impl ToTokenTrees for proc_macro::Ident {
    fn generate(ident: Self) -> Vec<TokenTree> {
        vec![ident.into()]
    }
}
