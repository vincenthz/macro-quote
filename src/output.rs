use alloc::{string::ToString, vec::Vec};
use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

use macro_quote_types::ext::literal_kind;

/// lightweight wrapper to append easily to a (quasi) TokenStream
pub(crate) struct Output(Vec<TokenTree>);

impl Output {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    #[inline]
    pub fn push_literal(&mut self, lit: Literal) {
        self.0.push(TokenTree::from(lit))
    }

    #[inline]
    pub fn push_ident(&mut self, p: Ident) {
        self.0.push(TokenTree::from(p))
    }

    #[inline]
    pub fn push_punct(&mut self, p: Punct) {
        self.0.push(TokenTree::from(p))
    }

    #[inline]
    pub fn push_dcolon(&mut self) {
        self.0
            .push(TokenTree::from(Punct::new(':', Spacing::Joint)));
        self.0
            .push(TokenTree::from(Punct::new(':', Spacing::Alone)));
    }

    #[inline]
    pub fn push_dcolon_joint(&mut self) {
        self.0
            .push(TokenTree::from(Punct::new(':', Spacing::Joint)));
        self.0
            .push(TokenTree::from(Punct::new(':', Spacing::Joint)));
    }

    #[inline]
    pub fn push_dot(&mut self) {
        self.0
            .push(TokenTree::from(Punct::new('.', Spacing::Alone)));
    }

    #[inline]
    pub fn push_semicolon(&mut self) {
        self.0
            .push(TokenTree::from(Punct::new(';', Spacing::Alone)));
    }

    #[inline]
    pub fn push_comma(&mut self) {
        self.0
            .push(TokenTree::from(Punct::new(',', Spacing::Alone)));
    }

    pub fn ts(span: Span) -> Ident {
        Ident::new("_ts", span)
    }

    pub fn ts_inner(span: Span) -> Ident {
        Ident::new("_ts_inner", span)
    }

    pub fn push_ts(&mut self, span: Span) {
        self.push_ident(Self::ts(span))
    }

    pub fn push_let_ident_eq(&mut self, mutable: bool, ident: &Ident) {
        self.push_ident(Ident::new("let", ident.span()));
        if mutable {
            self.push_ident(Ident::new("mut", ident.span()));
        }
        self.push_ident(ident.clone());
        self.push_punct(Punct::new('=', Spacing::Alone));
    }

    pub fn push_let_some_ident_eq(&mut self, ident: &Ident) {
        self.push_ident(Ident::new("let", ident.span()));
        self.push_ident(Ident::new("Some", ident.span()));
        self.arg1(ident.span(), |inner| inner.push_ident(ident.clone()));
        self.push_punct(Punct::new('=', Spacing::Alone));
    }

    pub fn push_new_ts<F>(&mut self, f: F, span: Span, it: TokenStream, finalize: bool)
    where
        F: FnOnce(&mut Output, TokenStream),
    {
        let mut root = Output::new();
        root.push_let_ident_eq(true, &Self::ts(span.clone()));
        root.push_tokenstream_new(span.clone());
        root.push_semicolon();

        f(&mut root, it);

        if finalize {
            root.tokentree_from_iter(Self::ts(span.clone()));
        } else {
            root.push_ts(span.clone());
        }
        self.push_grp(span, Group::new(Delimiter::Brace, root.finalize()));
    }

    pub fn push_path(&mut self, span: Span, absolute: bool, fragments: &[&str]) {
        for (i, fragment) in fragments.iter().enumerate() {
            if absolute || i > 0 {
                self.push_dcolon();
            }
            self.push_ident(Ident::new(fragment, span.clone()))
        }
    }

    pub fn push_call_span_call_site(&mut self) {
        self.push_path(
            Span::call_site(),
            true,
            &["proc_macro", "Span", "call_site"],
        );
        self.arg0(Span::call_site());
    }

    /*
    pub fn bracket<F>(&mut self, gen: F)
    where
        F: FnOnce(&mut Output),
    {
        let mut inner: Output = Output::new();
        gen(&mut inner);
        self.push_grp(Group::new(Delimiter::Bracket, inner.finalize()).into());
    }
    */

    pub fn brace<F>(&mut self, span: Span, gen: F)
    where
        F: FnOnce(&mut Output),
    {
        let mut inner: Output = Output::new();
        gen(&mut inner);
        self.push_grp(span, Group::new(Delimiter::Brace, inner.finalize()).into());
    }

    pub fn arg0(&mut self, span: Span) {
        self.push_grp(span, Group::new(Delimiter::Parenthesis, TokenStream::new()));
    }

    pub fn arg1<F>(&mut self, span: Span, gen1: F)
    where
        F: FnOnce(&mut Output),
    {
        let mut inner: Output = Output::new();
        gen1(&mut inner);
        self.push_grp(
            span,
            Group::new(Delimiter::Parenthesis, inner.finalize()).into(),
        );
    }

    pub fn arg2<F, G>(&mut self, span: Span, gen1: F, gen2: G)
    where
        F: FnOnce(&mut Output),
        G: FnOnce(&mut Output),
    {
        let mut inner: Output = Output::new();
        gen1(&mut inner);
        inner.push_comma();
        gen2(&mut inner);
        self.push_grp(
            span,
            Group::new(Delimiter::Parenthesis, inner.finalize()).into(),
        );
    }

    pub fn ts_extend_one_tokentreeable<F>(&mut self, span: Span, f: F)
    where
        F: FnOnce(&mut Output),
    {
        self.push_ts(span.clone());
        self.push_dot();
        self.push_ident(Ident::new("push", span.clone()));
        self.arg1(span.clone(), |gen1| gen1.wrap_tokentree(span.clone(), f));
        self.push_semicolon();
    }

    pub fn wrap_tokentree<F>(&mut self, span: Span, f: F)
    where
        F: FnOnce(&mut Output),
    {
        self.push_path(span.clone(), true, &["proc_macro", "TokenTree", "from"]);
        self.arg1(span, f);
    }

    pub fn tokentree_from_iter(&mut self, ident: Ident) {
        self.push_path(
            ident.span(),
            true,
            &["proc_macro", "TokenStream", "from_iter"],
        );
        self.arg1(ident.span(), |inner| inner.push_ident(ident))
    }

    /// Push a call to Literal::new(string, site) into ts
    pub fn push_escaped_literal(&mut self, lit: Literal) {
        let kind = literal_kind(&lit);
        let method_name = kind.to_method_name();
        let span = lit.span();
        self.ts_extend_one_tokentreeable(span.clone(), |inner| {
            inner.push_path(span.clone(), true, &["proc_macro", "Literal", method_name]);
            inner.arg1(span, |gen1| gen1.push_literal(lit));
        })
    }

    /// Push a call to Ident::new(string, site)
    pub fn push_escaped_ident(&mut self, p: Ident) {
        self.ts_extend_one_tokentreeable(p.span(), |inner| {
            inner.push_path(p.span(), true, &["proc_macro", "Ident", "new"]);
            inner.arg2(
                p.span(),
                |gen1| {
                    gen1.push_literal(Literal::string(&p.to_string()));
                },
                |gen2| {
                    gen2.push_call_span_call_site();
                },
            );
        })
    }

    /// Push a call to Punct::new(string, site)
    pub fn push_escaped_punct(&mut self, p: Punct) {
        self.ts_extend_one_tokentreeable(p.span(), |inner| {
            inner.push_path(p.span(), true, &["proc_macro", "Punct", "new"]);
            inner.arg2(
                p.span(),
                |gen1| gen1.push_literal(Literal::character(p.as_char())),
                |gen2| match p.spacing() {
                    Spacing::Alone => {
                        gen2.push_path(p.span(), true, &["proc_macro", "Spacing", "Alone"]);
                    }
                    Spacing::Joint => {
                        gen2.push_path(p.span(), true, &["proc_macro", "Spacing", "Joint"]);
                    }
                },
            );
        })
    }

    /// Push a call to Group::new(string, site)
    pub fn push_escaped_grp<F>(&mut self, g: Group, f: F)
    where
        F: FnOnce(&mut Output, TokenStream),
    {
        let delimiter_name = match g.delimiter() {
            Delimiter::Parenthesis => "Parenthesis",
            Delimiter::Brace => "Brace",
            Delimiter::Bracket => "Bracket",
            Delimiter::None => "None",
        };
        self.push_let_ident_eq(false, &Self::ts_inner(g.span()));
        self.push_new_ts(f, g.span(), g.stream(), false);
        self.push_semicolon();

        self.ts_extend_one_tokentreeable(g.span(), |inner| {
            inner.push_path(g.span(), true, &["proc_macro", "Group", "new"]);
            inner.arg2(
                g.span(),
                |gen1| gen1.push_path(g.span(), true, &["proc_macro", "Delimiter", delimiter_name]),
                |gen2| gen2.tokentree_from_iter(Self::ts_inner(g.span())),
            );
        })
    }

    // push a call to TokenStream::new()
    pub fn push_tokenstream_new(&mut self, span: Span) {
        self.push_path(span.clone(), true, &["alloc", "vec", "Vec"]);
        self.push_dcolon_joint();
        self.push_punct(Punct::new('<', Spacing::Joint));
        self.push_path(span.clone(), true, &["proc_macro", "TokenTree"]);
        self.push_punct(Punct::new('>', Spacing::Joint));
        self.push_path(span.clone(), true, &["new"]);
        self.arg0(span);
    }

    #[inline]
    pub fn push_grp(&mut self, span: Span, mut grp: Group) {
        grp.set_span(span);
        self.0.push(TokenTree::from(grp))
    }

    #[inline]
    pub fn finalize(self) -> TokenStream {
        TokenStream::from_iter(self.0)
    }
}
