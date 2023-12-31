#![no_std]
extern crate alloc;

extern crate proc_macro;

pub mod ext;

use alloc::vec::Vec;
use proc_macro::{Literal, TokenTree};

pub trait ToTokenTrees {
    fn append_tt(ty: Self, out: &mut Vec<TokenTree>);

    fn generate(ty: Self) -> Vec<TokenTree>
    where
        Self: Sized,
    {
        let mut out = Vec::new();
        Self::append_tt(ty, &mut out);
        out
    }
}

impl ToTokenTrees for &str {
    fn append_tt(ty: Self, out: &mut Vec<TokenTree>) {
        out.push(Literal::string(ty).into())
    }
}

impl ToTokenTrees for alloc::string::String {
    fn append_tt(ty: Self, out: &mut Vec<TokenTree>) {
        out.push(Literal::string(ty.as_str()).into())
    }
}

impl ToTokenTrees for u32 {
    fn append_tt(ty: Self, out: &mut Vec<TokenTree>) {
        out.push(Literal::u32_suffixed(ty).into())
    }
}

impl ToTokenTrees for usize {
    fn append_tt(ty: Self, out: &mut Vec<TokenTree>) {
        out.push(Literal::usize_suffixed(ty).into())
    }
}

impl ToTokenTrees for Vec<TokenTree> {
    fn append_tt(ty: Self, out: &mut Vec<TokenTree>) {
        out.extend(ty.into_iter())
    }
}

impl ToTokenTrees for &mut Vec<TokenTree> {
    fn append_tt(ty: Self, out: &mut Vec<TokenTree>) {
        out.append(ty)
    }
}
impl ToTokenTrees for proc_macro::TokenStream {
    fn append_tt(ty: Self, out: &mut Vec<TokenTree>) {
        out.extend(ty)
    }
}

impl ToTokenTrees for proc_macro::Literal {
    fn append_tt(ty: Self, out: &mut Vec<TokenTree>) {
        out.push(ty.into())
    }
}

impl ToTokenTrees for proc_macro::Ident {
    fn append_tt(ty: Self, out: &mut Vec<TokenTree>) {
        out.push(ty.into())
    }
}
