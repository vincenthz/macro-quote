#![no_std]

extern crate alloc;

mod output;

use alloc::{string::String, vec::Vec};
use proc_macro::{Delimiter, Ident, Punct, Spacing, Span, TokenStream, TokenTree};

use output::Output;

#[proc_macro]
pub fn quote(input: TokenStream) -> TokenStream {
    let mut out = Output::new();
    out.push_new_ts(stream, Span::call_site(), input, true);
    out.finalize()
}

fn stream(out: &mut Output, it: TokenStream) {
    let mut it = it.into_iter().peekable();
    while let Some(tt) = it.next() {
        match tt {
            proc_macro::TokenTree::Group(grp) => {
                out.push_escaped_grp(grp, stream);
            }
            proc_macro::TokenTree::Ident(ident) => out.push_escaped_ident(ident),
            proc_macro::TokenTree::Punct(punct) => {
                // add a way to ## to produce a single #
                if is_escape_punct(&punct) {
                    escape(punct, out, &mut it);
                } else {
                    out.push_escaped_punct(punct)
                }
            }
            proc_macro::TokenTree::Literal(literal) => out.push_escaped_literal(literal),
        }
    }
}

struct Repeat(String);

fn is_escape_punct(punct: &Punct) -> bool {
    punct.spacing() == Spacing::Alone && punct.as_char() == '#'
}

fn escape<I: Iterator<Item = TokenTree>>(
    escape_punct: Punct,
    out: &mut Output,
    it: &mut core::iter::Peekable<I>,
) {
    // if we reach end of stream, we just push the escape punct as is without doing anything else
    let Some(next) = it.next() else {
        out.push_punct(escape_punct);
        return;
    };

    match next {
        TokenTree::Group(grp) => {
            if grp.delimiter() == Delimiter::Parenthesis {
                let mut repeat = String::new();
                loop {
                    match it.peek() {
                        Some(TokenTree::Punct(p)) if p.as_char() == '*' => {
                            it.next();
                            break;
                        }
                        Some(TokenTree::Punct(p)) => {
                            if p.spacing() == Spacing::Joint {
                                repeat.push(p.as_char());
                            } else {
                                panic!("")
                            }
                            it.next();
                        }
                        _ => {
                            panic!("repeat need to terminate with a '*' character")
                        }
                    }
                }
                escape_group(out, grp.span(), grp.stream(), Repeat(repeat));
            } else {
                panic!("brace or paren group not supported in escape");
                // out.push_punct(escape_punct);
                // out.push_grp_escape(grp);
            }
        }
        TokenTree::Ident(ident) => ts_extend_ident(out, ident),
        // For punct we don't escape anything, so just produce the # and the following punct
        TokenTree::Punct(punct) => {
            out.push_escaped_punct(escape_punct);
            out.push_escaped_punct(punct);
        }
        // For literal we don't escape anything, so just produce the # and the following literal
        TokenTree::Literal(literal) => {
            out.push_escaped_punct(escape_punct);
            out.push_escaped_literal(literal);
        }
    }
}

fn ts_extend_ident(out: &mut Output, ident: Ident) {
    let span = ident.span();
    out.push_path(
        span.clone(),
        true,
        &["macro_quote_types", "ToTokenTrees", "append_tt"],
    );
    out.arg2(
        span,
        |gen1| {
            gen1.push_ident(ident);
        },
        |gen2| {
            gen2.push_punct(Punct::new('&', Spacing::Alone));
            gen2.push_ident(Ident::new("mut", span.clone()));
            gen2.push_ts(span);
        },
    );
}

fn escape_group(out: &mut Output, span: Span, it: TokenStream, repeat: Repeat) {
    let items = it.clone().into_iter().collect::<Vec<_>>();

    // Scan the variables first
    let mut escape_vars = Vec::new();
    let mut items_it = items.iter();
    while let Some(item) = items_it.next() {
        match item {
            TokenTree::Punct(punct) if is_escape_punct(punct) => match items_it.next() {
                Some(TokenTree::Ident(i)) => {
                    escape_vars.push(i.clone());
                }
                _ => {}
            },
            TokenTree::Punct(_)
            | TokenTree::Group(_)
            | TokenTree::Ident(_)
            | TokenTree::Literal(_) => {}
        }
    }

    // then build the following data structure:
    //
    // let _inner_ts = {
    //    let mut var0 = var0.into_iter();
    //    let mut var1 = var1.into_iter();
    //    ...
    //    let mut ts = TokenStream::new();
    //    loop {
    //        let Some(var0) = var0.next() else {
    //            break;
    //        }
    //        let Some(var1) = var1.next() else {
    //            break;
    //        }
    //        ...
    //        .. unescape token ..
    //        ts.extends(ToTokenTrees::generate(var0));
    //        .. unescape token ..
    //        ts.extends(ToTokenTrees::generate(var1));
    //        .. unescape token ..
    //        ...
    //    }
    //    ts
    // };
    // ts.extend(_inner_ts)
    out.push_let_ident_eq(false, &Output::ts_inner(span.clone()));
    out.brace(span.clone(), |inner| {
        for var in escape_vars.iter() {
            inner.push_let_ident_eq(true, var);
            inner.push_ident(var.clone());
            inner.push_dot();
            inner.push_ident(Ident::new("into_iter", span.clone()));
            inner.arg0(span.clone());
            inner.push_semicolon();
        }
        inner.push_let_ident_eq(true, &Output::ts(span.clone()));
        inner.push_tokenstream_new(span.clone());
        inner.push_semicolon();
        inner.push_ident(Ident::new("loop", span.clone()));
        inner.brace(span.clone(), |inner| {
            for var in escape_vars.iter() {
                inner.push_let_some_ident_eq(var);
                inner.push_ident(var.clone());
                inner.push_dot();
                inner.push_ident(Ident::new("next", span.clone()));
                inner.arg0(span.clone());
                inner.push_ident(Ident::new("else", span.clone()));
                inner.brace(span.clone(), |inner| {
                    inner.push_ident(Ident::new("break", span.clone()));
                    inner.push_semicolon();
                });
                inner.push_semicolon();
            }
            stream(inner, it);
            let len = repeat.0.chars().count();
            for (i, c) in repeat.0.chars().enumerate() {
                let spacing = if i + 1 == len {
                    Spacing::Alone
                } else {
                    Spacing::Joint
                };
                inner.push_escaped_punct(Punct::new(c, spacing));
            }
        });
        inner.push_ts(span.clone())
    });
    out.push_semicolon();
    ts_extend_ident(out, Output::ts_inner(span));
}
/*

The following proc macro call:

```
quote! {
    abc(def(123))
}
```

will generate:

```
Ident(abc) Group(Parenthesis, [
    Ident(def),
    Group(Parenthesis, [
        Literal(123)
    ])
])
```

With escaped:

```
let s = 123;
quote! {
    #s + 23;
}
```

```
Literal(123) Punct('+') Literal(23)
```
*/
