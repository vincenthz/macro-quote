#[cfg(feature = "proc-quote")]
use proc_quote::quote;

#[cfg(feature = "quote")]
use quote::quote;

#[cfg(feature = "macro-quote")]
use macro_quote::quote;

#[cfg(feature = "macro-quote")]
extern crate proc_macro;
#[cfg(feature = "macro-quote")]
extern crate alloc;

#[cfg(all(
    not(feature = "macro-quote"),
    not(feature = "proc-quote"),
    not(feature = "quote")
))]
compile_error!("no quote backend selected");

fn main() {
    let x = "abc";
    let ts = quote! {
        let x = "123";
        #x
    };

    let len = ts.into_iter().collect::<Vec<_>>().len();
    println!("{}", len);
}
