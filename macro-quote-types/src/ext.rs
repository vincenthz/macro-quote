//! This is a helper module to workaround the inherent limitation in the proc-macro APIs
//!
//! Add some API for proc-macro shortcoming:
//!
//! * (byte_character constructor unstable)[https://github.com/rust-lang/rust/issues/115268]
//! * (span inspection API)[https://github.com/rust-lang/rust/issues/54725]
//!

use alloc::{
    format,
    string::{String, ToString},
};
use proc_macro::{Literal, Span};

#[derive(Debug, Clone)]
pub enum LiteralKind {
    //Byte,
    Bytes,
    Char,
    String,
    Int(Base, Option<IntKind>),
    Real,
}

impl LiteralKind {
    pub fn to_method_name(&self) -> &'static str {
        match self {
            // unstable constructor
            //LiteralKind::Byte => "byte_character",
            LiteralKind::Bytes => "byte_string",
            LiteralKind::Char => "character",
            LiteralKind::String => "string",
            LiteralKind::Int(_, None) => "u128_unsuffixed",
            LiteralKind::Int(_, Some(int_kind)) => match int_kind {
                IntKind::U8 => "u8_suffixed",
                IntKind::U16 => "u16_suffixed",
                IntKind::U32 => "u32_suffixed",
                IntKind::U64 => "u64_suffixed",
                IntKind::U128 => "u128_suffixed",
                IntKind::USize => "usize_suffixed",
                IntKind::I8 => "i8_suffixed",
                IntKind::I16 => "i16_suffixed",
                IntKind::I32 => "i32_suffixed",
                IntKind::I64 => "i64_suffixed",
                IntKind::I128 => "i128_suffixed",
                IntKind::ISize => "isize_suffixed",
            },
            LiteralKind::Real => {
                todo!("real not implemented yet")
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Base {
    Binary,
    Octal,
    Decimal,
    Hexadecimal,
}

impl Base {
    #[allow(unused)]
    pub fn to_radix(self) -> u32 {
        match self {
            Base::Binary => 2,
            Base::Octal => 8,
            Base::Decimal => 10,
            Base::Hexadecimal => 16,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum IntKind {
    U8,
    U16,
    U32,
    U64,
    U128,
    USize,
    I8,
    I16,
    I32,
    I64,
    I128,
    ISize,
}

/// get the kind of literal
pub fn literal_kind(lit: &Literal) -> LiteralKind {
    let s = lit.to_string();

    assert!(s.len() > 0, "literal cannot represent the empty string");

    let mut chars = s.chars();
    let first = chars.next().unwrap();
    match first {
        '0'..='9' => {
            let Some(second) = chars.next() else {
                return LiteralKind::Int(Base::Decimal, None);
            };
            let base = if second == 'x' {
                Base::Hexadecimal
            } else if second == 'b' {
                Base::Binary
            } else if second == 'o' {
                Base::Octal
            } else {
                Base::Decimal
            };

            let mut dot = false;

            while let Some(x) = chars.next() {
                if base == Base::Decimal && x == '.' {
                    dot = true;
                    break;
                }
                match base {
                    Base::Decimal => {
                        if x.is_ascii_digit() {
                            continue;
                        }
                    }
                    Base::Hexadecimal => {
                        if x.is_ascii_hexdigit() {
                            continue;
                        }
                    }
                    Base::Octal => {
                        if x.is_digit(8) {
                            continue;
                        }
                    }
                    Base::Binary => {
                        if x == '0' || x == '1' {
                            continue;
                        }
                    }
                }
            }

            if dot {
                return LiteralKind::Real;
            }

            let kind = if let Some(x) = chars.next() {
                let kind = if x == 'u' {
                    let ending = chars.collect::<String>();
                    if ending == "8" {
                        IntKind::U8
                    } else if ending == "16" {
                        IntKind::U16
                    } else if ending == "32" {
                        IntKind::U32
                    } else if ending == "64" {
                        IntKind::U64
                    } else if ending == "128" {
                        IntKind::U128
                    } else if ending == "size" {
                        IntKind::USize
                    } else {
                        panic!("unknown kind")
                    }
                } else if x == 'i' {
                    let ending = chars.collect::<String>();
                    if ending == "8" {
                        IntKind::I8
                    } else if ending == "16" {
                        IntKind::I16
                    } else if ending == "32" {
                        IntKind::I32
                    } else if ending == "64" {
                        IntKind::I64
                    } else if ending == "128" {
                        IntKind::I128
                    } else if ending == "size" {
                        IntKind::ISize
                    } else {
                        panic!("unknown kind")
                    }
                } else {
                    panic!("unknown integral literal suffix")
                };
                Some(kind)
            } else {
                None
            };
            LiteralKind::Int(base, kind)
        }
        '\'' => LiteralKind::Char,
        '"' => {
            // this is not complete
            LiteralKind::String
        }
        'b' => {
            let second = chars.next();
            match second {
                None => panic!("unknown literal {}", s),
                Some('\'') => {
                    panic!("byte constructor is unstable :(")
                    // LiteralKind::Byte
                }
                Some('"') => LiteralKind::Bytes,
                _ => panic!("unknown literal {}", s),
            }
        }
        _ => {
            panic!("unknown literal {}", s)
        }
    }
}

/// Transform a proc_macro::Span into a byte range
pub fn span_to_range(span: Span) -> core::ops::Range<usize> {
    let s = format!("{:?}", span);

    let dummy = core::ops::Range { start: 0, end: 0 };

    let Some(start_open) = s.find(|c: char| c == '(') else {
        return dummy;
    };
    let Some(end_open) = s[start_open..].find(|c| c == ')').map(|i| start_open + i) else {
        return dummy;
    };

    let x = &s[start_open + 1..end_open];
    let Some(dot_index) = x.find('.') else {
        return dummy;
    };
    let nb1 = &x[0..dot_index];
    let nb2 = &x[dot_index + 2..];
    let start = usize::from_str_radix(nb1, 10).unwrap_or(0);
    let end = usize::from_str_radix(nb2, 10).unwrap_or(0);
    core::ops::Range { start, end }
}
