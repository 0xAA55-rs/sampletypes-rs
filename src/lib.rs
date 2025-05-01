#![allow(dead_code)]

pub mod mod_i24;
pub mod mod_u24;

pub use mod_i24::*;
pub use mod_u24::*;

use std::{io::{Read, Write, Error}, mem::size_of, fmt::{Debug, Display}, clone::Clone};
use std::ops::{Add, Sub, Mul, Div, AddAssign, SubAssign, MulAssign, DivAssign};
use std::ops::{BitAnd, BitOr, BitXor, Shl, Shr, BitAndAssign, BitOrAssign, BitXorAssign, ShlAssign, ShrAssign, Not};
use std::ops::{Rem, RemAssign};
use std::ops::{Neg};

/// Basic numeric trait can do `+`, `-`, `*`, `/`, `+=`, `-=`, `*=`, `/=`
pub trait Numeric:
Add<Output = Self> + Sub<Output = Self> + Mul<Output = Self> + Div<Output = Self> +
AddAssign + SubAssign + MulAssign + DivAssign +
Debug + Display + Sized + Clone + Copy + 'static {}

impl<T> Numeric for T where T:
Add<Output = Self> + Sub<Output = Self> + Mul<Output = Self> + Div<Output = Self> +
AddAssign + SubAssign + MulAssign + DivAssign +
Debug + Display + Sized + Clone + Copy + 'static {}

/// Integers can do `&`, `|`, `^`
pub trait SampleTypeInteger:
SampleType +
BitAnd<Output = Self> +
BitOr<Output = Self> +
BitXor<Output = Self> +
Shl<Output = Self> +
Shr<Output = Self> +
BitAndAssign +
BitOrAssign +
BitXorAssign +
ShlAssign +
ShrAssign +
Rem<Output = Self> +
RemAssign +
Not<Output = Self> {}

impl<T> SampleTypeInteger for T where T:
SampleType +
BitAnd<Output = Self> +
BitOr<Output = Self> +
BitXor<Output = Self> +
Shl<Output = Self> +
Shr<Output = Self> +
BitAndAssign +
BitOrAssign +
BitXorAssign +
ShlAssign +
ShrAssign +
Rem<Output = Self> +
RemAssign +
Not<Output = Self> {}

/// Signed integers can do `-x`
pub trait SampleTypeIntegerSigned: SampleTypeInteger + Neg {}
impl<T> SampleTypeIntegerSigned for T where T: SampleTypeInteger + Neg {}

/// Get the middle number of a specific type
#[macro_export]
macro_rules! mid_number {
    (i8) => {0i8};
    (i16) => {0i16};
    (i24) => {i24(0, 0, 0)};
    (i32) => {0i32};
    (i64) => {0i64};
    (i128) => {0i128};
    (u8) => {0x80u8};
    (u16) => {0x8000u16};
    (u24) => {u24(0, 0, 0x80)};
    (u32) => {0x80000000u32};
    (u64) => {0x80000000_00000000u64};
    (u128) => {0x80000000_00000000_00000000_00000000u128};
    (f32) => {0.0f32};
    (f64) => {0.0f64};
}

/// Get the zero number of a specific type
#[macro_export]
macro_rules! zero_number {
    (i8) => {0i8};
    (i16) => {0i16};
    (i24) => {i24(0, 0, 0)};
    (i32) => {0i32};
    (i64) => {0i64};
    (i128) => {0i128};
    (u8) => {0u8};
    (u16) => {0u16};
    (u24) => {u24(0, 0, 0)};
    (u32) => {0u32};
    (u64) => {0u64};
    (u128) => {0u128};
    (f32) => {0.0f32};
    (f64) => {0.0f64};
}

/// Get the size of as specific type in bytes
#[macro_export]
macro_rules! sizeof {
    (i8  ) => {1 };
    (i16 ) => {2 };
    (i24 ) => {3 };
    (i32 ) => {4 };
    (i64 ) => {8 };
    (i128) => {16};
    (u8  ) => {1 };
    (u16 ) => {2 };
    (u24 ) => {3 };
    (u32 ) => {4 };
    (u64 ) => {8 };
    (u128) => {16};
    (f32 ) => {4 };
    (f64 ) => {8 };
}

/// Get the signed type of a specific type, no effect to `f32` and `f64`
#[macro_export]
macro_rules! signed_type {
    (i8  ) => {i8  };
    (i16 ) => {i16 };
    (i24 ) => {i24 };
    (i32 ) => {i32 };
    (i64 ) => {i64 };
    (i128) => {i128};
    (u8  ) => {i8  };
    (u16 ) => {i16 };
    (u24 ) => {i24 };
    (u32 ) => {i32 };
    (u64 ) => {i64 };
    (u128) => {i128};
    (f32 ) => {f32 };
    (f64 ) => {f64 };
}

/// Get the unsigned type of a specific type, no effect to `f32` and `f64`
#[macro_export]
macro_rules! unsigned_type {
    (i8  ) => {u8  };
    (i16 ) => {u16 };
    (i24 ) => {u24 };
    (i32 ) => {u32 };
    (i64 ) => {u64 };
    (i128) => {u128};
    (u8  ) => {u8  };
    (u16 ) => {u16 };
    (u24 ) => {u24 };
    (u32 ) => {u32 };
    (u64 ) => {u64 };
    (u128) => {u128};
    (f32 ) => {f32 };
    (f64 ) => {f64 };
}

/// Is this type a signed number?
#[macro_export]
macro_rules! is_signed {
    (i8  ) => {true };
    (i16 ) => {true };
    (i24 ) => {true };
    (i32 ) => {true };
    (i64 ) => {true };
    (i128) => {true };
    (u8  ) => {false};
    (u16 ) => {false};
    (u24 ) => {false};
    (u32 ) => {false};
    (u64 ) => {false};
    (u128) => {false};
    (f32 ) => {true };
    (f64 ) => {true };
}

/// Is this type an unsigned number?
#[macro_export]
macro_rules! is_unsigned {
    ($tp:tt) => {!is_signed!($tp)};
}

/// Scale the type to the corresponding signed type number
#[macro_export]
macro_rules! to_signed {
    (i8  , $v:expr) => {$v};
    (i16 , $v:expr) => {$v};
    (i24 , $v:expr) => {$v};
    (i32 , $v:expr) => {$v};
    (i64 , $v:expr) => {$v};
    (i128, $v:expr) => {$v};
    (u8  , $v:expr) => {$v.wrapping_sub(mid_number!(u8  )) as i8  };
    (u16 , $v:expr) => {$v.wrapping_sub(mid_number!(u16 )) as i16 };
    (u24 , $v:expr) => {i24($v.0, $v.1, to_signed!(u8, $v.2) as u8)};
    (u32 , $v:expr) => {$v.wrapping_sub(mid_number!(u32 )) as i32 };
    (u64 , $v:expr) => {$v.wrapping_sub(mid_number!(u64 )) as i64 };
    (u128, $v:expr) => {$v.wrapping_sub(mid_number!(u128)) as i128};
    (f32 , $v:expr) => {$v};
    (f64 , $v:expr) => {$v};
}

/// Scale the type to the corresponding unsigned type number
#[macro_export]
macro_rules! to_unsigned {
    (i8  , $v:expr) => {($v as u8  ).wrapping_add(mid_number!(u8  ))};
    (i16 , $v:expr) => {($v as u16 ).wrapping_add(mid_number!(u16 ))};
    (i24 , $v:expr) => {u24($v.0, $v.1, to_unsigned!(i8, $v.2 as i8))};
    (i32 , $v:expr) => {($v as u32 ).wrapping_add(mid_number!(u32 ))};
    (i64 , $v:expr) => {($v as u64 ).wrapping_add(mid_number!(u64 ))};
    (i128, $v:expr) => {($v as u128).wrapping_add(mid_number!(u128))};
    (u8  , $v:expr) => {$v};
    (u16 , $v:expr) => {$v};
    (u24 , $v:expr) => {$v};
    (u32 , $v:expr) => {$v};
    (u64 , $v:expr) => {$v};
    (u128, $v:expr) => {$v};
    (f32 , $v:expr) => {$v};
    (f64 , $v:expr) => {$v};
}

/// Is this type an integer?
#[macro_export]
macro_rules! is_integer {
    (i8  ) => {true };
    (i16 ) => {true };
    (i24 ) => {true };
    (i32 ) => {true };
    (i64 ) => {true };
    (i128) => {true };
    (u8  ) => {true };
    (u16 ) => {true };
    (u24 ) => {true };
    (u32 ) => {true };
    (u64 ) => {true };
    (u128) => {true };
    (f32 ) => {false};
    (f64 ) => {false};
}

/// Is this type an IEEE 754 floating point number?
#[macro_export]
macro_rules! is_float {
    ($tp:tt) => {!is_integer!($tp)};
}

/// Get the longer type of the current type.
#[macro_export]
macro_rules! longer_type {
    (i8  ) => {i16 };
    (i16 ) => {i24 };
    (i24 ) => {i32 };
    (i32 ) => {i64 };
    (i64 ) => {i128};
    (i128) => {i128};
    (u8  ) => {u16 };
    (u16 ) => {u24 };
    (u24 ) => {u32 };
    (u32 ) => {u64 };
    (u64 ) => {u128};
    (u128) => {u128};
    (f32 ) => {f64 };
    (f64 ) => {f64 };
}

/// Get the shorter type of the current type.
#[macro_export]
macro_rules! shorter_type {
    (i8  ) => {i8 };
    (i16 ) => {i8 };
    (i24 ) => {i16};
    (i32 ) => {i24};
    (i64 ) => {i32};
    (i128) => {i64};
    (u8  ) => {u8 };
    (u16 ) => {u8 };
    (u24 ) => {u16};
    (u32 ) => {u24};
    (u64 ) => {u32};
    (u128) => {u64};
    (f32 ) => {f32};
    (f64 ) => {f32};
}

/// Cast the current type to the longer type.
#[macro_export]
macro_rules! as_longer {
    (i8  , $v:expr) => {$v as i16};
    (i16 , $v:expr) => {i24::from($v)};
    (i24 , $v:expr) => {$v.as_i32()};
    (i32 , $v:expr) => {$v as i64};
    (i64 , $v:expr) => {$v as i128};
    (i128, $v:expr) => {$v as i128};
    (u8  , $v:expr) => {$v as u16};
    (u16 , $v:expr) => {u24::from($v)};
    (u24 , $v:expr) => {$v.as_u32()};
    (u32 , $v:expr) => {$v as u64};
    (u64 , $v:expr) => {$v as u128};
    (u128, $v:expr) => {$v as u128};
    (f32 , $v:expr) => {$v as f64};
    (f64 , $v:expr) => {$v as f64};
}

/// Cast the current type to the shorter type.
#[macro_export]
macro_rules! as_shorter {
    (i8  , $v:expr) => {$v as i8};
    (i16 , $v:expr) => {$v as i8};
    (i24 , $v:expr) => {$v.as_i16()};
    (i32 , $v:expr) => {i24::from($v)};
    (i64 , $v:expr) => {$v as i32};
    (i128, $v:expr) => {$v as i64};
    (u8  , $v:expr) => {$v as u8};
    (u16 , $v:expr) => {$v as u8};
    (u24 , $v:expr) => {$v.as_u16()};
    (u32 , $v:expr) => {u24::from($v)};
    (u64 , $v:expr) => {$v as u32};
    (u128, $v:expr) => {$v as u64};
    (f32 , $v:expr) => {$v as f32};
    (f64 , $v:expr) => {$v as f32};
}

/// Scale the current type number to the longer type number.
#[macro_export]
macro_rules! to_longer {
    (i8  , $v:expr) => {to_type!(i8  , i16 , $v)};
    (i16 , $v:expr) => {to_type!(i16 , i24 , $v)};
    (i24 , $v:expr) => {to_type!(i24 , i32 , $v)};
    (i32 , $v:expr) => {to_type!(i32 , i64 , $v)};
    (i64 , $v:expr) => {to_type!(i64 , i128, $v)};
    (i128, $v:expr) => {to_type!(i128, i128, $v)};
    (u8  , $v:expr) => {to_type!(u8  , u16 , $v)};
    (u16 , $v:expr) => {to_type!(u16 , u24 , $v)};
    (u24 , $v:expr) => {to_type!(u24 , u32 , $v)};
    (u32 , $v:expr) => {to_type!(u32 , u64 , $v)};
    (u64 , $v:expr) => {to_type!(u64 , u128, $v)};
    (u128, $v:expr) => {to_type!(u128, u128, $v)};
    (f32 , $v:expr) => {to_type!(f32 , f64 , $v)};
    (f64 , $v:expr) => {to_type!(f64 , f64 , $v)};
}

/// Scale the current type number to the shorter type number.
#[macro_export]
macro_rules! to_shorter {
    (i8  , $v:expr) => {to_type!(i8  , i8 , $v)};
    (i16 , $v:expr) => {to_type!(i16 , i8 , $v)};
    (i24 , $v:expr) => {to_type!(i24 , i16, $v)};
    (i32 , $v:expr) => {to_type!(i32 , i24, $v)};
    (i64 , $v:expr) => {to_type!(i64 , i32, $v)};
    (i128, $v:expr) => {to_type!(i128, i64, $v)};
    (u8  , $v:expr) => {to_type!(u8  , u8 , $v)};
    (u16 , $v:expr) => {to_type!(u16 , u8 , $v)};
    (u24 , $v:expr) => {to_type!(u24 , u16, $v)};
    (u32 , $v:expr) => {to_type!(u32 , u24, $v)};
    (u64 , $v:expr) => {to_type!(u64 , u32, $v)};
    (u128, $v:expr) => {to_type!(u128, u64, $v)};
    (f32 , $v:expr) => {to_type!(f32 , f32, $v)};
    (f64 , $v:expr) => {to_type!(f64 , f32, $v)};
}

/// Scale to `i8` range
#[macro_export]
macro_rules! to_i8 {
    (i8  , $v:expr) => {$v};
    (i16 , $v:expr) => {($v >> 8) as i8};
    (i24 , $v:expr) => {$v.2 as i8};
    (i32 , $v:expr) => {($v >> 24) as i8};
    (i64 , $v:expr) => {($v >> 56) as i8};
    (i128, $v:expr) => {($v >> 120) as i8};
    (u8  , $v:expr) => {to_i8!(i8  , to_signed!(u8  , $v))};
    (u16 , $v:expr) => {to_i8!(i16 , to_signed!(u16 , $v))};
    (u24 , $v:expr) => {to_i8!(i24 , to_signed!(u24 , $v))};
    (u32 , $v:expr) => {to_i8!(i32 , to_signed!(u32 , $v))};
    (u64 , $v:expr) => {to_i8!(i64 , to_signed!(u64 , $v))};
    (u128, $v:expr) => {to_i8!(i128, to_signed!(u128, $v))};
    (f32 , $v:expr) => {($v * i8::MAX as f32) as i8};
    (f64 , $v:expr) => {($v * i8::MAX as f64) as i8};
}

/// Scale to `i16` range
#[macro_export]
macro_rules! to_i16 {
    (i8  , $v:expr) => {(($v as i16) << 8) | (to_unsigned!(i8, $v) as i16)};
    (i16 , $v:expr) => {$v};
    (i24 , $v:expr) => {($v.as_i32() >> 8) as i16};
    (i32 , $v:expr) => {($v >> 16) as i16};
    (i64 , $v:expr) => {($v >> 48) as i16};
    (i128, $v:expr) => {($v >> 112) as i16};
    (u8  , $v:expr) => {to_i16!(i8  , to_signed!(u8  , $v))};
    (u16 , $v:expr) => {to_i16!(i16 , to_signed!(u16 , $v))};
    (u24 , $v:expr) => {to_i16!(i24 , to_signed!(u24 , $v))};
    (u32 , $v:expr) => {to_i16!(i32 , to_signed!(u32 , $v))};
    (u64 , $v:expr) => {to_i16!(i64 , to_signed!(u64 , $v))};
    (u128, $v:expr) => {to_i16!(i128, to_signed!(u128, $v))};
    (f32 , $v:expr) => {($v * i16::MAX as f32) as i16};
    (f64 , $v:expr) => {($v * i16::MAX as f64) as i16};
}

/// Scale to `i24` range
#[macro_export]
macro_rules! to_i24 {
    (i8  , $v:expr) => {{let u = to_unsigned!(i8, $v); i24(u, u, $v as u8)}};
    (i16 , $v:expr) => {i24::from(to_i32!(i16, $v) >> 8)};
    (i24 , $v:expr) => {$v};
    (i32 , $v:expr) => {i24::from($v >> 8)};
    (i64 , $v:expr) => {i24::from($v >> 40)};
    (i128, $v:expr) => {i24::from($v >> 104)};
    (u8  , $v:expr) => {to_i24!(i8  , to_signed!(u8  , $v))};
    (u16 , $v:expr) => {to_i24!(i16 , to_signed!(u16 , $v))};
    (u24 , $v:expr) => {to_i24!(i24 , to_signed!(u24 , $v))};
    (u32 , $v:expr) => {to_i24!(i32 , to_signed!(u32 , $v))};
    (u64 , $v:expr) => {to_i24!(i64 , to_signed!(u64 , $v))};
    (u128, $v:expr) => {to_i24!(i128, to_signed!(u128, $v))};
    (f32 , $v:expr) => {<i24 as From<i32>>::from(($v * 0x7FFFFF as f32) as i32)};
    (f64 , $v:expr) => {<i24 as From<i32>>::from(($v * 0x7FFFFF as f64) as i32)};
}

/// Scale to `i32` range
#[macro_export]
macro_rules! to_i32 {
    (i8  , $v:expr) => {{let longer = to_i16!(i8, $v); ((longer as i32) << 16) | (to_unsigned!(i16, longer) as i32)}};
    (i16 , $v:expr) => {(($v as i32) << 16) | (to_unsigned!(i16, $v) as i32)};
    (i24 , $v:expr) => {($v.as_i32() << 8) | (to_unsigned!(i8, $v.2 as i8) as i32)};
    (i32 , $v:expr) => {$v};
    (i64 , $v:expr) => {($v >> 32) as i32};
    (i128, $v:expr) => {($v >> 96) as i32};
    (u8  , $v:expr) => {to_i32!(i8  , to_signed!(u8  , $v))};
    (u16 , $v:expr) => {to_i32!(i16 , to_signed!(u16 , $v))};
    (u24 , $v:expr) => {to_i32!(i24 , to_signed!(u24 , $v))};
    (u32 , $v:expr) => {to_i32!(i32 , to_signed!(u32 , $v))};
    (u64 , $v:expr) => {to_i32!(i64 , to_signed!(u64 , $v))};
    (u128, $v:expr) => {to_i32!(i128, to_signed!(u128, $v))};
    (f32 , $v:expr) => {($v * i32::MAX as f32) as i32};
    (f64 , $v:expr) => {($v * i32::MAX as f64) as i32};
}

/// Scale to `i64` range
#[macro_export]
macro_rules! to_i64 {
    (i8  , $v:expr) => {{let longer = to_i32!(i8 , $v); ((longer as i64) << 32) | (to_unsigned!(i32, longer) as i64)}};
    (i16 , $v:expr) => {{let longer = to_i32!(i16, $v); ((longer as i64) << 32) | (to_unsigned!(i32, longer) as i64)}};
    (i24 , $v:expr) => {{let u2 = to_unsigned!(i8, $v.2 as i8); i64::from_le_bytes([$v.1, u2, $v.0, $v.1, u2, $v.0, $v.1, $v.2])}};
    (i32 , $v:expr) => {(($v as i64) << 32) | (to_unsigned!(i32, $v) as i64)};
    (i64 , $v:expr) => {$v};
    (i128, $v:expr) => {($v >> 64) as i64};
    (u8  , $v:expr) => {to_i64!(i8  , to_signed!(u8  , $v))};
    (u16 , $v:expr) => {to_i64!(i16 , to_signed!(u16 , $v))};
    (u24 , $v:expr) => {i64::from_le_bytes([$v.1, $v.2, $v.0, $v.1, $v.2, $v.0, $v.1, to_signed!(u8, $v.2) as u8])};
    (u32 , $v:expr) => {to_i64!(i32 , to_signed!(u32 , $v))};
    (u64 , $v:expr) => {to_i64!(i64 , to_signed!(u64 , $v))};
    (u128, $v:expr) => {to_i64!(i128, to_signed!(u128, $v))};
    (f32 , $v:expr) => {($v * i64::MAX as f32) as i64};
    (f64 , $v:expr) => {($v * i64::MAX as f64) as i64};
}

/// Scale to `i128` range
#[macro_export]
macro_rules! to_i128 {
    (i8  , $v:expr) => {{let longer = to_i64!(i8 , $v); ((longer as i128) << 64) | (to_unsigned!(i64, longer) as i128)}};
    (i16 , $v:expr) => {{let longer = to_i64!(i16, $v); ((longer as i128) << 64) | (to_unsigned!(i64, longer) as i128)}};
    (i24 , $v:expr) => {{let u2 = to_unsigned!(i8, $v.2 as i8); i128::from_le_bytes([u2, $v.0, $v.1, u2, $v.0, $v.1, u2, $v.0, $v.1, u2, $v.0, $v.1, u2, $v.0, $v.1, $v.2])}};
    (i32 , $v:expr) => {{let longer = to_i64!(i32, $v); ((longer as i128) << 64) | (to_unsigned!(i64, longer) as i128)}};
    (i64 , $v:expr) => {(($v as i128) << 64) | (to_unsigned!(i64, $v) as i128)};
    (i128, $v:expr) => {$v};
    (u8  , $v:expr) => {to_i128!(i8  , to_signed!(u8  , $v))};
    (u16 , $v:expr) => {to_i128!(i16 , to_signed!(u16 , $v))};
    (u24 , $v:expr) => {i128::from_le_bytes([$v.2, $v.0, $v.1, $v.2, $v.0, $v.1, $v.2, $v.0, $v.1, $v.2, $v.0, $v.1, $v.2, $v.0, $v.1, to_signed!(u8, $v.2) as u8])};
    (u32 , $v:expr) => {to_i128!(i32 , to_signed!(u32 , $v))};
    (u64 , $v:expr) => {to_i128!(i64 , to_signed!(u64 , $v))};
    (u128, $v:expr) => {to_i128!(i128, to_signed!(u128, $v))};
    (f32 , $v:expr) => {($v * i128::MAX as f32) as i128};
    (f64 , $v:expr) => {($v * i128::MAX as f64) as i128};
}

/// Scale to `u8` range
#[macro_export]
macro_rules! to_u8 {
    (i8  , $v:expr) => {to_unsigned!(i8, to_i8!(i8  , $v))};
    (i16 , $v:expr) => {to_unsigned!(i8, to_i8!(i16 , $v))};
    (i24 , $v:expr) => {to_unsigned!(i8, to_i8!(i24 , $v))};
    (i32 , $v:expr) => {to_unsigned!(i8, to_i8!(i32 , $v))};
    (i64 , $v:expr) => {to_unsigned!(i8, to_i8!(i64 , $v))};
    (i128, $v:expr) => {to_unsigned!(i8, to_i8!(i128, $v))};
    (u8  , $v:expr) => {to_unsigned!(i8, to_i8!(u8  , $v))};
    (u16 , $v:expr) => {to_unsigned!(i8, to_i8!(u16 , $v))};
    (u24 , $v:expr) => {to_unsigned!(i8, to_i8!(u24 , $v))};
    (u32 , $v:expr) => {to_unsigned!(i8, to_i8!(u32 , $v))};
    (u64 , $v:expr) => {to_unsigned!(i8, to_i8!(u64 , $v))};
    (u128, $v:expr) => {to_unsigned!(i8, to_i8!(u128, $v))};
    (f32 , $v:expr) => {to_unsigned!(i8, to_i8!(f32 , $v))};
    (f64 , $v:expr) => {to_unsigned!(i8, to_i8!(f64 , $v))};
}

/// Scale to `u16` range
#[macro_export]
macro_rules! to_u16 {
    (i8  , $v:expr) => {to_unsigned!(i16, to_i16!(i8  , $v))};
    (i16 , $v:expr) => {to_unsigned!(i16, to_i16!(i16 , $v))};
    (i24 , $v:expr) => {to_unsigned!(i16, to_i16!(i24 , $v))};
    (i32 , $v:expr) => {to_unsigned!(i16, to_i16!(i32 , $v))};
    (i64 , $v:expr) => {to_unsigned!(i16, to_i16!(i64 , $v))};
    (i128, $v:expr) => {to_unsigned!(i16, to_i16!(i128, $v))};
    (u8  , $v:expr) => {to_unsigned!(i16, to_i16!(u8  , $v))};
    (u16 , $v:expr) => {to_unsigned!(i16, to_i16!(u16 , $v))};
    (u24 , $v:expr) => {to_unsigned!(i16, to_i16!(u24 , $v))};
    (u32 , $v:expr) => {to_unsigned!(i16, to_i16!(u32 , $v))};
    (u64 , $v:expr) => {to_unsigned!(i16, to_i16!(u64 , $v))};
    (u128, $v:expr) => {to_unsigned!(i16, to_i16!(u128, $v))};
    (f32 , $v:expr) => {to_unsigned!(i16, to_i16!(f32 , $v))};
    (f64 , $v:expr) => {to_unsigned!(i16, to_i16!(f64 , $v))};
}

/// Scale to `u24` range
#[macro_export]
macro_rules! to_u24 {
    (i8  , $v:expr) => {to_unsigned!(i24, to_i24!(i8  , $v))};
    (i16 , $v:expr) => {to_unsigned!(i24, to_i24!(i16 , $v))};
    (i24 , $v:expr) => {to_unsigned!(i24, to_i24!(i24 , $v))};
    (i32 , $v:expr) => {to_unsigned!(i24, to_i24!(i32 , $v))};
    (i64 , $v:expr) => {to_unsigned!(i24, to_i24!(i64 , $v))};
    (i128, $v:expr) => {to_unsigned!(i24, to_i24!(i128, $v))};
    (u8  , $v:expr) => {to_unsigned!(i24, to_i24!(u8  , $v))};
    (u16 , $v:expr) => {to_unsigned!(i24, to_i24!(u16 , $v))};
    (u24 , $v:expr) => {to_unsigned!(i24, to_i24!(u24 , $v))};
    (u32 , $v:expr) => {to_unsigned!(i24, to_i24!(u32 , $v))};
    (u64 , $v:expr) => {to_unsigned!(i24, to_i24!(u64 , $v))};
    (u128, $v:expr) => {to_unsigned!(i24, to_i24!(u128, $v))};
    (f32 , $v:expr) => {to_unsigned!(i24, to_i24!(f32 , $v))};
    (f64 , $v:expr) => {to_unsigned!(i24, to_i24!(f64 , $v))};
}

/// Scale to `u32` range
#[macro_export]
macro_rules! to_u32 {
    (i8  , $v:expr) => {to_unsigned!(i32, to_i32!(i8  , $v))};
    (i16 , $v:expr) => {to_unsigned!(i32, to_i32!(i16 , $v))};
    (i24 , $v:expr) => {to_unsigned!(i32, to_i32!(i24 , $v))};
    (i32 , $v:expr) => {to_unsigned!(i32, to_i32!(i32 , $v))};
    (i64 , $v:expr) => {to_unsigned!(i32, to_i32!(i64 , $v))};
    (i128, $v:expr) => {to_unsigned!(i32, to_i32!(i128, $v))};
    (u8  , $v:expr) => {to_unsigned!(i32, to_i32!(u8  , $v))};
    (u16 , $v:expr) => {to_unsigned!(i32, to_i32!(u16 , $v))};
    (u24 , $v:expr) => {to_unsigned!(i32, to_i32!(u24 , $v))};
    (u32 , $v:expr) => {to_unsigned!(i32, to_i32!(u32 , $v))};
    (u64 , $v:expr) => {to_unsigned!(i32, to_i32!(u64 , $v))};
    (u128, $v:expr) => {to_unsigned!(i32, to_i32!(u128, $v))};
    (f32 , $v:expr) => {to_unsigned!(i32, to_i32!(f32 , $v))};
    (f64 , $v:expr) => {to_unsigned!(i32, to_i32!(f64 , $v))};
}

/// Scale to `u64` range
#[macro_export]
macro_rules! to_u64 {
    (i8  , $v:expr) => {to_unsigned!(i64, to_i64!(i8  , $v))};
    (i16 , $v:expr) => {to_unsigned!(i64, to_i64!(i16 , $v))};
    (i24 , $v:expr) => {to_unsigned!(i64, to_i64!(i24 , $v))};
    (i32 , $v:expr) => {to_unsigned!(i64, to_i64!(i32 , $v))};
    (i64 , $v:expr) => {to_unsigned!(i64, to_i64!(i64 , $v))};
    (i128, $v:expr) => {to_unsigned!(i64, to_i64!(i128, $v))};
    (u8  , $v:expr) => {to_unsigned!(i64, to_i64!(u8  , $v))};
    (u16 , $v:expr) => {to_unsigned!(i64, to_i64!(u16 , $v))};
    (u24 , $v:expr) => {to_unsigned!(i64, to_i64!(u24 , $v))};
    (u32 , $v:expr) => {to_unsigned!(i64, to_i64!(u32 , $v))};
    (u64 , $v:expr) => {to_unsigned!(i64, to_i64!(u64 , $v))};
    (u128, $v:expr) => {to_unsigned!(i64, to_i64!(u128, $v))};
    (f32 , $v:expr) => {to_unsigned!(i64, to_i64!(f32 , $v))};
    (f64 , $v:expr) => {to_unsigned!(i64, to_i64!(f64 , $v))};
}

/// Scale to `u128` range
#[macro_export]
macro_rules! to_u128 {
    (i8  , $v:expr) => {to_unsigned!(i128, to_i128!(i8  , $v))};
    (i16 , $v:expr) => {to_unsigned!(i128, to_i128!(i16 , $v))};
    (i24 , $v:expr) => {to_unsigned!(i128, to_i128!(i24 , $v))};
    (i32 , $v:expr) => {to_unsigned!(i128, to_i128!(i32 , $v))};
    (i64 , $v:expr) => {to_unsigned!(i128, to_i128!(i64 , $v))};
    (i128, $v:expr) => {to_unsigned!(i128, to_i128!(i128, $v))};
    (u8  , $v:expr) => {to_unsigned!(i128, to_i128!(u8  , $v))};
    (u16 , $v:expr) => {to_unsigned!(i128, to_i128!(u16 , $v))};
    (u24 , $v:expr) => {to_unsigned!(i128, to_i128!(u24 , $v))};
    (u32 , $v:expr) => {to_unsigned!(i128, to_i128!(u32 , $v))};
    (u64 , $v:expr) => {to_unsigned!(i128, to_i128!(u64 , $v))};
    (u128, $v:expr) => {to_unsigned!(i128, to_i128!(u128, $v))};
    (f32 , $v:expr) => {to_unsigned!(i128, to_i128!(f32 , $v))};
    (f64 , $v:expr) => {to_unsigned!(i128, to_i128!(f64 , $v))};
}

/// Scale to `[-1.0, 1.0]` range
#[macro_export]
macro_rules! to_f32 {
    (i8  , $v:expr) => {($v as f32) / (i8  ::MAX) as f32};
    (i16 , $v:expr) => {($v as f32) / (i16 ::MAX) as f32};
    (i24 , $v:expr) => {($v.as_i32() as f32) / (0x7FFFFF as f32)};
    (i32 , $v:expr) => {($v as f32) / (i32 ::MAX) as f32};
    (i64 , $v:expr) => {($v as f32) / (i64 ::MAX) as f32};
    (i128, $v:expr) => {($v as f32) / (i128::MAX) as f32};
    (u8  , $v:expr) => {(to_signed!(u8  , $v) as f32) / (i8  ::MAX) as f32};
    (u16 , $v:expr) => {(to_signed!(u16 , $v) as f32) / (i16 ::MAX) as f32};
    (u24 , $v:expr) => {(to_signed!(u24 , $v).as_i32() as f32) / (0x7FFFFF as f32)};
    (u32 , $v:expr) => {(to_signed!(u32 , $v) as f32) / (i32 ::MAX) as f32};
    (u64 , $v:expr) => {(to_signed!(u64 , $v) as f32) / (i64 ::MAX) as f32};
    (u128, $v:expr) => {(to_signed!(u128, $v) as f32) / (i128::MAX) as f32};
    (f32 , $v:expr) => {$v};
    (f64 , $v:expr) => {$v as f32};
}

/// Scale to `[-1.0, 1.0]` range
#[macro_export]
macro_rules! to_f64 {
    (i8  , $v:expr) => {($v as f64) / (i8  ::MAX as f64)};
    (i16 , $v:expr) => {($v as f64) / (i16 ::MAX as f64)};
    (i24 , $v:expr) => {($v.as_i32() as f64) / (0x7FFFFF as f64)};
    (i32 , $v:expr) => {($v as f64) / (i32 ::MAX as f64)};
    (i64 , $v:expr) => {($v as f64) / (i64 ::MAX as f64)};
    (i128, $v:expr) => {($v as f64) / (i128::MAX as f64)};
    (u8  , $v:expr) => {(to_signed!(u8  , $v) as f64) / (i8  ::MAX as f64)};
    (u16 , $v:expr) => {(to_signed!(u16 , $v) as f64) / (i16 ::MAX as f64)};
    (u24 , $v:expr) => {(to_signed!(u24 , $v).as_i32() as f64) / (0x7FFFFF as f64)};
    (u32 , $v:expr) => {(to_signed!(u32 , $v) as f64) / (i32 ::MAX as f64)};
    (u64 , $v:expr) => {(to_signed!(u64 , $v) as f64) / (i64 ::MAX as f64)};
    (u128, $v:expr) => {(to_signed!(u128, $v) as f64) / (i128::MAX as f64)};
    (f32 , $v:expr) => {$v as f64};
    (f64 , $v:expr) => {$v};
}

/// Scale to a specific type number
#[macro_export]
macro_rules! to_type {
    ($st:tt, i8  , $v:expr) => {to_i8!($st, $v)};
    ($st:tt, i16 , $v:expr) => {to_i16!($st, $v)};
    ($st:tt, i24 , $v:expr) => {to_i24!($st, $v)};
    ($st:tt, i32 , $v:expr) => {to_i32!($st, $v)};
    ($st:tt, i64 , $v:expr) => {to_i64!($st, $v)};
    ($st:tt, i128, $v:expr) => {to_i128!($st, $v)};
    ($st:tt, u8  , $v:expr) => {to_u8!($st, $v)};
    ($st:tt, u16 , $v:expr) => {to_u16!($st, $v)};
    ($st:tt, u24 , $v:expr) => {to_u24!($st, $v)};
    ($st:tt, u32 , $v:expr) => {to_u32!($st, $v)};
    ($st:tt, u64 , $v:expr) => {to_u64!($st, $v)};
    ($st:tt, u128, $v:expr) => {to_u128!($st, $v)};
    ($st:tt, f32 , $v:expr) => {to_f32!($st, $v)};
    ($st:tt, f64 , $v:expr) => {to_f64!($st, $v)};
}

/// Cast to `i8`
#[macro_export]
macro_rules! as_i8 {
    (i8  , $v:expr) => {$v as i8};
    (i16 , $v:expr) => {$v as i8};
    (i24 , $v:expr) => {i24::as_i8($v)};
    (i32 , $v:expr) => {$v as i8};
    (i64 , $v:expr) => {$v as i8};
    (i128, $v:expr) => {$v as i8};
    (u8  , $v:expr) => {$v as i8};
    (u16 , $v:expr) => {$v as i8};
    (u24 , $v:expr) => {u24::as_i8($v)};
    (u32 , $v:expr) => {$v as i8};
    (u64 , $v:expr) => {$v as i8};
    (u128, $v:expr) => {$v as i8};
    (f32 , $v:expr) => {$v as i8};
    (f64 , $v:expr) => {$v as i8};
}

/// Cast to `i16`
#[macro_export]
macro_rules! as_i16 {
    (i8  , $v:expr) => {$v as i16};
    (i16 , $v:expr) => {$v as i16};
    (i24 , $v:expr) => {i24::as_i16($v)};
    (i32 , $v:expr) => {$v as i16};
    (i64 , $v:expr) => {$v as i16};
    (i128, $v:expr) => {$v as i16};
    (u8  , $v:expr) => {$v as i16};
    (u16 , $v:expr) => {$v as i16};
    (u24 , $v:expr) => {u24::as_i16($v)};
    (u32 , $v:expr) => {$v as i16};
    (u64 , $v:expr) => {$v as i16};
    (u128, $v:expr) => {$v as i16};
    (f32 , $v:expr) => {$v as i16};
    (f64 , $v:expr) => {$v as i16};
}

/// Cast to `i24`
#[macro_export]
macro_rules! as_i24 {
    (i8  , $v:expr) => {i24::from($v)};
    (i16 , $v:expr) => {i24::from($v)};
    (i24 , $v:expr) => {i24::from($v)};
    (i32 , $v:expr) => {i24::from($v)};
    (i64 , $v:expr) => {i24::from($v)};
    (i128, $v:expr) => {i24::from($v)};
    (u8  , $v:expr) => {i24::from($v)};
    (u16 , $v:expr) => {i24::from($v)};
    (u24 , $v:expr) => {i24::from($v)};
    (u32 , $v:expr) => {i24::from($v)};
    (u64 , $v:expr) => {i24::from($v)};
    (u128, $v:expr) => {i24::from($v)};
    (f32 , $v:expr) => {i24::from($v)};
    (f64 , $v:expr) => {i24::from($v)};
}

/// Cast to `i32`
#[macro_export]
macro_rules! as_i32 {
    (i8  , $v:expr) => {$v as i32};
    (i16 , $v:expr) => {$v as i32};
    (i24 , $v:expr) => {i24::as_i32($v)};
    (i32 , $v:expr) => {$v as i32};
    (i64 , $v:expr) => {$v as i32};
    (i128, $v:expr) => {$v as i32};
    (u8  , $v:expr) => {$v as i32};
    (u16 , $v:expr) => {$v as i32};
    (u24 , $v:expr) => {u24::as_i32($v)};
    (u32 , $v:expr) => {$v as i32};
    (u64 , $v:expr) => {$v as i32};
    (u128, $v:expr) => {$v as i32};
    (f32 , $v:expr) => {$v as i32};
    (f64 , $v:expr) => {$v as i32};
}

/// Cast to `i64`
#[macro_export]
macro_rules! as_i64 {
    (i8  , $v:expr) => {$v as i64};
    (i16 , $v:expr) => {$v as i64};
    (i24 , $v:expr) => {i24::as_i64($v)};
    (i32 , $v:expr) => {$v as i64};
    (i64 , $v:expr) => {$v as i64};
    (i128, $v:expr) => {$v as i64};
    (u8  , $v:expr) => {$v as i64};
    (u16 , $v:expr) => {$v as i64};
    (u24 , $v:expr) => {u24::as_i64($v)};
    (u32 , $v:expr) => {$v as i64};
    (u64 , $v:expr) => {$v as i64};
    (u128, $v:expr) => {$v as i64};
    (f32 , $v:expr) => {$v as i64};
    (f64 , $v:expr) => {$v as i64};
}

/// Cast to `i128`
#[macro_export]
macro_rules! as_i128 {
    (i8  , $v:expr) => {$v as i128};
    (i16 , $v:expr) => {$v as i128};
    (i24 , $v:expr) => {i24::as_i128($v)};
    (i32 , $v:expr) => {$v as i128};
    (i64 , $v:expr) => {$v as i128};
    (i128, $v:expr) => {$v as i128};
    (u8  , $v:expr) => {$v as i128};
    (u16 , $v:expr) => {$v as i128};
    (u24 , $v:expr) => {u24::as_i128($v)};
    (u32 , $v:expr) => {$v as i128};
    (u64 , $v:expr) => {$v as i128};
    (u128, $v:expr) => {$v as i128};
    (f32 , $v:expr) => {$v as i128};
    (f64 , $v:expr) => {$v as i128};
}

/// Cast to `u8`
#[macro_export]
macro_rules! as_u8 {
    (i8  , $v:expr) => {$v as u8};
    (i16 , $v:expr) => {$v as u8};
    (i24 , $v:expr) => {i24::as_u8($v)};
    (i32 , $v:expr) => {$v as u8};
    (i64 , $v:expr) => {$v as u8};
    (i128, $v:expr) => {$v as u8};
    (u8  , $v:expr) => {$v as u8};
    (u16 , $v:expr) => {$v as u8};
    (u24 , $v:expr) => {u24::as_u8($v)};
    (u32 , $v:expr) => {$v as u8};
    (u64 , $v:expr) => {$v as u8};
    (u128, $v:expr) => {$v as u8};
    (f32 , $v:expr) => {$v as u8};
    (f64 , $v:expr) => {$v as u8};
}

/// Cast to `u16`
#[macro_export]
macro_rules! as_u16 {
    (i8  , $v:expr) => {$v as u16};
    (i16 , $v:expr) => {$v as u16};
    (i24 , $v:expr) => {i24::as_u16($v)};
    (i32 , $v:expr) => {$v as u16};
    (i64 , $v:expr) => {$v as u16};
    (i128, $v:expr) => {$v as u16};
    (u8  , $v:expr) => {$v as u16};
    (u16 , $v:expr) => {$v as u16};
    (u24 , $v:expr) => {u24::as_u16($v)};
    (u32 , $v:expr) => {$v as u16};
    (u64 , $v:expr) => {$v as u16};
    (u128, $v:expr) => {$v as u16};
    (f32 , $v:expr) => {$v as u16};
    (f64 , $v:expr) => {$v as u16};
}

/// Cast to `u24`
#[macro_export]
macro_rules! as_u24 {
    (i8  , $v:expr) => {u24::from($v)};
    (i16 , $v:expr) => {u24::from($v)};
    (i24 , $v:expr) => {u24::from($v)};
    (i32 , $v:expr) => {u24::from($v)};
    (i64 , $v:expr) => {u24::from($v)};
    (i128, $v:expr) => {u24::from($v)};
    (u8  , $v:expr) => {u24::from($v)};
    (u16 , $v:expr) => {u24::from($v)};
    (u24 , $v:expr) => {u24::from($v)};
    (u32 , $v:expr) => {u24::from($v)};
    (u64 , $v:expr) => {u24::from($v)};
    (u128, $v:expr) => {u24::from($v)};
    (f32 , $v:expr) => {u24::from($v)};
    (f64 , $v:expr) => {u24::from($v)};
}

/// Cast to `u32`
#[macro_export]
macro_rules! as_u32 {
    (i8  , $v:expr) => {$v as u32};
    (i16 , $v:expr) => {$v as u32};
    (i24 , $v:expr) => {i24::as_u32($v)};
    (i32 , $v:expr) => {$v as u32};
    (i64 , $v:expr) => {$v as u32};
    (i128, $v:expr) => {$v as u32};
    (u8  , $v:expr) => {$v as u32};
    (u16 , $v:expr) => {$v as u32};
    (u24 , $v:expr) => {u24::as_u32($v)};
    (u32 , $v:expr) => {$v as u32};
    (u64 , $v:expr) => {$v as u32};
    (u128, $v:expr) => {$v as u32};
    (f32 , $v:expr) => {$v as u32};
    (f64 , $v:expr) => {$v as u32};
}

/// Cast to `u64`
#[macro_export]
macro_rules! as_u64 {
    (i8  , $v:expr) => {$v as u64};
    (i16 , $v:expr) => {$v as u64};
    (i24 , $v:expr) => {i24::as_u64($v)};
    (i32 , $v:expr) => {$v as u64};
    (i64 , $v:expr) => {$v as u64};
    (i128, $v:expr) => {$v as u64};
    (u8  , $v:expr) => {$v as u64};
    (u16 , $v:expr) => {$v as u64};
    (u24 , $v:expr) => {u24::as_u64($v)};
    (u32 , $v:expr) => {$v as u64};
    (u64 , $v:expr) => {$v as u64};
    (u128, $v:expr) => {$v as u64};
    (f32 , $v:expr) => {$v as u64};
    (f64 , $v:expr) => {$v as u64};
}

/// Cast to `u128`
#[macro_export]
macro_rules! as_u128 {
    (i8  , $v:expr) => {$v as u128};
    (i16 , $v:expr) => {$v as u128};
    (i24 , $v:expr) => {i24::as_u128($v)};
    (i32 , $v:expr) => {$v as u128};
    (i64 , $v:expr) => {$v as u128};
    (i128, $v:expr) => {$v as u128};
    (u8  , $v:expr) => {$v as u128};
    (u16 , $v:expr) => {$v as u128};
    (u24 , $v:expr) => {u24::as_u128($v)};
    (u32 , $v:expr) => {$v as u128};
    (u64 , $v:expr) => {$v as u128};
    (u128, $v:expr) => {$v as u128};
    (f32 , $v:expr) => {$v as u128};
    (f64 , $v:expr) => {$v as u128};
}

/// Cast to `f32`
#[macro_export]
macro_rules! as_f32 {
    (i8  , $v:expr) => {$v as f32};
    (i16 , $v:expr) => {$v as f32};
    (i24 , $v:expr) => {i24::as_f32($v)};
    (i32 , $v:expr) => {$v as f32};
    (i64 , $v:expr) => {$v as f32};
    (i128, $v:expr) => {$v as f32};
    (u8  , $v:expr) => {$v as f32};
    (u16 , $v:expr) => {$v as f32};
    (u24 , $v:expr) => {u24::as_f32($v)};
    (u32 , $v:expr) => {$v as f32};
    (u64 , $v:expr) => {$v as f32};
    (u128, $v:expr) => {$v as f32};
    (f32 , $v:expr) => {$v as f32};
    (f64 , $v:expr) => {$v as f32};
}

/// Cast to `f64`
#[macro_export]
macro_rules! as_f64 {
    (i8  , $v:expr) => {$v as f64};
    (i16 , $v:expr) => {$v as f64};
    (i24 , $v:expr) => {i24::as_f64($v)};
    (i32 , $v:expr) => {$v as f64};
    (i64 , $v:expr) => {$v as f64};
    (i128, $v:expr) => {$v as f64};
    (u8  , $v:expr) => {$v as f64};
    (u16 , $v:expr) => {$v as f64};
    (u24 , $v:expr) => {u24::as_f64($v)};
    (u32 , $v:expr) => {$v as f64};
    (u64 , $v:expr) => {$v as f64};
    (u128, $v:expr) => {$v as f64};
    (f32 , $v:expr) => {$v as f64};
    (f64 , $v:expr) => {$v as f64};
}

/// Cast to a specific type number
#[macro_export]
macro_rules! as_type {
    ($st:tt, i8  , $v:expr) => {as_i8!($st, $v)};
    ($st:tt, i16 , $v:expr) => {as_i16!($st, $v)};
    ($st:tt, i24 , $v:expr) => {as_i24!($st, $v)};
    ($st:tt, i32 , $v:expr) => {as_i32!($st, $v)};
    ($st:tt, i64 , $v:expr) => {as_i64!($st, $v)};
    ($st:tt, i128, $v:expr) => {as_i128!($st, $v)};
    ($st:tt, u8  , $v:expr) => {as_u8!($st, $v)};
    ($st:tt, u16 , $v:expr) => {as_u16!($st, $v)};
    ($st:tt, u24 , $v:expr) => {as_u24!($st, $v)};
    ($st:tt, u32 , $v:expr) => {as_u32!($st, $v)};
    ($st:tt, u64 , $v:expr) => {as_u64!($st, $v)};
    ($st:tt, u128, $v:expr) => {as_u128!($st, $v)};
    ($st:tt, f32 , $v:expr) => {as_f32!($st, $v)};
    ($st:tt, f64 , $v:expr) => {as_f64!($st, $v)};
}

/// * Let known type of `SampleType` instance to call `to_Xxx()`
#[macro_export]
macro_rules! call_to_type {
    (i8  , $v:expr) => {$v.to_i8  ()};
    (i16 , $v:expr) => {$v.to_i16 ()};
    (i24 , $v:expr) => {$v.to_i24 ()};
    (i32 , $v:expr) => {$v.to_i32 ()};
    (i64 , $v:expr) => {$v.to_i64 ()};
    (i128, $v:expr) => {$v.to_i128()};
    (u8  , $v:expr) => {$v.to_u8  ()};
    (u16 , $v:expr) => {$v.to_u16 ()};
    (u24 , $v:expr) => {$v.to_u24 ()};
    (u32 , $v:expr) => {$v.to_u32 ()};
    (u64 , $v:expr) => {$v.to_u64 ()};
    (u128, $v:expr) => {$v.to_u128()};
    (f32 , $v:expr) => {$v.to_f32 ()};
    (f64 , $v:expr) => {$v.to_f64 ()};
}

/// * Let known type of `SampleType` instance to call `as_Xxx()`
#[macro_export]
macro_rules! call_as_type {
    (i8  , $v:expr) => {$v.as_i8  ()};
    (i16 , $v:expr) => {$v.as_i16 ()};
    (i24 , $v:expr) => {$v.as_i24 ()};
    (i32 , $v:expr) => {$v.as_i32 ()};
    (i64 , $v:expr) => {$v.as_i64 ()};
    (i128, $v:expr) => {$v.as_i128()};
    (u8  , $v:expr) => {$v.as_u8  ()};
    (u16 , $v:expr) => {$v.as_u16 ()};
    (u24 , $v:expr) => {$v.as_u24 ()};
    (u32 , $v:expr) => {$v.as_u32 ()};
    (u64 , $v:expr) => {$v.as_u64 ()};
    (u128, $v:expr) => {$v.as_u128()};
    (f32 , $v:expr) => {$v.as_f32 ()};
    (f64 , $v:expr) => {$v.as_f64 ()};
}

/// * Get the suitable float type for the source type to perform losslessly conversion
#[macro_export]
macro_rules! get_suitable_float {
    (i8  ) => {f32};
    (i16 ) => {f32};
    (i24 ) => {f32};
    (i32 ) => {f64};
    (i64 ) => {f64};
    (i128) => {f64};
    (u8  ) => {f32};
    (u16 ) => {f32};
    (u24 ) => {f32};
    (u32 ) => {f64};
    (u64 ) => {f64};
    (u128) => {f64};
    (f32 ) => {$v};
    (f64 ) => {$v};
}

/// Scale to `[-1.0, 1.0]` range
/// * Scale to the suitable float type for the source type to perform losslessly conversion
#[macro_export]
macro_rules! to_suitable_float {
    (i8  , $v:expr) => {to_f32!(i8  , $v)};
    (i16 , $v:expr) => {to_f32!(i16 , $v)};
    (i24 , $v:expr) => {to_f32!(i24 , $v)};
    (i32 , $v:expr) => {to_f64!(i32 , $v)};
    (i64 , $v:expr) => {to_f64!(i64 , $v)};
    (i128, $v:expr) => {to_f64!(i128, $v)};
    (u8  , $v:expr) => {to_f32!(u8  , $v)};
    (u16 , $v:expr) => {to_f32!(u16 , $v)};
    (u24 , $v:expr) => {to_f32!(u24 , $v)};
    (u32 , $v:expr) => {to_f64!(u32 , $v)};
    (u64 , $v:expr) => {to_f64!(u64 , $v)};
    (u128, $v:expr) => {to_f64!(u128, $v)};
    (f32 , $v:expr) => {$v};
    (f64 , $v:expr) => {$v};
}

/// Get the average value from a sample array.
#[macro_export]
macro_rules! average_arr {
    ($tp:tt, $longer:tt, $arr:expr) => {
        {
            type Longer = $longer;
            let mut sum: Longer = Longer::zero();
            $arr.iter().for_each(|x|{sum += to_longer!($tp, *x);});
            as_type!($longer, $tp, sum / as_type!(u64, $longer, $arr.len() as u64))
        }
    };
}

/// Sine wave generator
/// * The input `x` doesn't need to be related to PI.
/// * e.g. The type is `i8`, the value is -128, then you will get sin(-PI).
/// * e.g. The type is `u8`, the value is 0, then you will get sin(-PI) too.
#[macro_export]
macro_rules! sin {
    (i8  , $v:expr, $nt:tt) => {{let v = (to_f32!(i8  , $v) * std::f32::consts::PI).sin(); to_type!(f32, $nt, v)}};
    (i16 , $v:expr, $nt:tt) => {{let v = (to_f32!(i16 , $v) * std::f32::consts::PI).sin(); to_type!(f32, $nt, v)}};
    (i24 , $v:expr, $nt:tt) => {{let v = (to_f32!(i24 , $v) * std::f32::consts::PI).sin(); to_type!(f32, $nt, v)}};
    (i32 , $v:expr, $nt:tt) => {{let v = (to_f64!(i32 , $v) * std::f64::consts::PI).sin(); to_type!(f64, $nt, v)}};
    (i64 , $v:expr, $nt:tt) => {{let v = (to_f64!(i64 , $v) * std::f64::consts::PI).sin(); to_type!(f64, $nt, v)}};
    (i128, $v:expr, $nt:tt) => {{let v = (to_f64!(i128, $v) * std::f64::consts::PI).sin(); to_type!(f64, $nt, v)}};
    (u8  , $v:expr, $nt:tt) => {{let v = (to_f32!(u8  , $v) * std::f32::consts::PI).sin(); to_type!(f32, $nt, v)}};
    (u16 , $v:expr, $nt:tt) => {{let v = (to_f32!(u16 , $v) * std::f32::consts::PI).sin(); to_type!(f32, $nt, v)}};
    (u24 , $v:expr, $nt:tt) => {{let v = (to_f32!(u24 , $v) * std::f32::consts::PI).sin(); to_type!(f32, $nt, v)}};
    (u32 , $v:expr, $nt:tt) => {{let v = (to_f64!(u32 , $v) * std::f64::consts::PI).sin(); to_type!(f64, $nt, v)}};
    (u64 , $v:expr, $nt:tt) => {{let v = (to_f64!(u64 , $v) * std::f64::consts::PI).sin(); to_type!(f64, $nt, v)}};
    (u128, $v:expr, $nt:tt) => {{let v = (to_f64!(u128, $v) * std::f64::consts::PI).sin(); to_type!(f64, $nt, v)}};
    (f32 , $v:expr, $nt:tt) => {{let v = ($v * std::f32::consts::PI).sin(); to_type!(f32, $nt, v)}};
    (f64 , $v:expr, $nt:tt) => {{let v = ($v * std::f64::consts::PI).sin(); to_type!(f64, $nt, v)}};
}

/// Cosine wave generator
/// * The input `x` doesn't need to be related to PI.
/// * e.g. The type is `i8`, the value is -128, then you will get cos(-PI).
/// * e.g. The type is `u8`, the value is 0, then you will get cos(-PI) too.
#[macro_export]
macro_rules! cos {
    (i8  , $v:expr, $nt:tt) => {{let v = (to_f32!(i8  , $v) * std::f32::consts::PI).cos();to_type!(f32, $nt, v)}};
    (i16 , $v:expr, $nt:tt) => {{let v = (to_f32!(i16 , $v) * std::f32::consts::PI).cos();to_type!(f32, $nt, v)}};
    (i24 , $v:expr, $nt:tt) => {{let v = (to_f32!(i24 , $v) * std::f32::consts::PI).cos();to_type!(f32, $nt, v)}};
    (i32 , $v:expr, $nt:tt) => {{let v = (to_f64!(i32 , $v) * std::f64::consts::PI).cos();to_type!(f64, $nt, v)}};
    (i64 , $v:expr, $nt:tt) => {{let v = (to_f64!(i64 , $v) * std::f64::consts::PI).cos();to_type!(f64, $nt, v)}};
    (i128, $v:expr, $nt:tt) => {{let v = (to_f64!(i128, $v) * std::f64::consts::PI).cos();to_type!(f64, $nt, v)}};
    (u8  , $v:expr, $nt:tt) => {{let v = (to_f32!(u8  , $v) * std::f32::consts::PI).cos();to_type!(f32, $nt, v)}};
    (u16 , $v:expr, $nt:tt) => {{let v = (to_f32!(u16 , $v) * std::f32::consts::PI).cos();to_type!(f32, $nt, v)}};
    (u24 , $v:expr, $nt:tt) => {{let v = (to_f32!(u24 , $v) * std::f32::consts::PI).cos();to_type!(f32, $nt, v)}};
    (u32 , $v:expr, $nt:tt) => {{let v = (to_f64!(u32 , $v) * std::f64::consts::PI).cos();to_type!(f64, $nt, v)}};
    (u64 , $v:expr, $nt:tt) => {{let v = (to_f64!(u64 , $v) * std::f64::consts::PI).cos();to_type!(f64, $nt, v)}};
    (u128, $v:expr, $nt:tt) => {{let v = (to_f64!(u128, $v) * std::f64::consts::PI).cos();to_type!(f64, $nt, v)}};
    (f32 , $v:expr, $nt:tt) => {{let v = ($v * std::f32::consts::PI).cos(); to_type!(f32, $nt, v)}};
    (f64 , $v:expr, $nt:tt) => {{let v = ($v * std::f64::consts::PI).cos(); to_type!(f64, $nt, v)}};
}

/// * The `SampleType` for audio processing.
/// * The `to_*()` methods are for scaling the sample to the another format.
/// * The `as_*()` methods are for casting the sample to the another format.
pub trait SampleType: SampleFrom {
    /// The type we are implementating for
    type ImplFor;

    /// The longer type, e.g. for `i8`, the longer type is `i16`
    type Longer;

    /// The shorter type, e.g. for `i16`, the shorter type is `i8`
    type Shorter;

    /// The signed type, e.g. for `u32`, the signed type is `i32`.
    type Signed;

    /// The Unsigned type, e.g. for `i32`, the unsigned type is `u32`.
    type Unsigned;

    /// The middle number, e.g. for `u16`, the middle number is `32768`.
    const MIDNUM: Self;

    /// The type name, to avoid using `std::any::type_name`
    const TYPE_NAME: &'static str;

    /// Create a new sample, the value is the middle value of the range of the format.
    fn new() -> Self;

    /// Create a new sample, the value is zero.
    fn zero() -> Self;

    /// Scale a sample to this specified format.
    fn scale_from<T>(v: T) -> Self where T: SampleType;

    /// Cast a sample to this specified format.
    fn cast_from<T>(v: T) -> Self where T: SampleType;

    /// Get the average value of two samples.
    fn average(s1: Self, s2: Self) -> Self;

    /// Get the average value from a sample array.
    fn average_arr(arr: &[Self]) -> Self;

    /// Scale to `i8` range
    fn to_i8 (self) -> i8 ;

    /// Scale to `i16` range
    fn to_i16(self) -> i16;

    /// Scale to `i24` range
    fn to_i24(self) -> i24;

    /// Scale to `i32` range
    fn to_i32(self) -> i32;

    /// Scale to `i64` range
    fn to_i64(self) -> i64;

    /// Scale to `u8` range
    fn to_u8 (self) -> u8 ;

    /// Scale to `u16` range
    fn to_u16(self) -> u16;

    /// Scale to `u24` range
    fn to_u24(self) -> u24;

    /// Scale to `u32` range
    fn to_u32(self) -> u32;

    /// Scale to `u64` range
    fn to_u64(self) -> u64;

    /// Scale to `[-1.0, 1.0]` range
    fn to_f32(self) -> f32;

    /// Scale to `[-1.0, 1.0]` range
    fn to_f64(self) -> f64;

    /// Scale to `i128` range
    fn to_i128(self) -> i128;

    /// Scale to `u128` range
    fn to_u128(self) -> u128;

    /// Cast to `i8`
    fn as_i8 (self) -> i8 ;

    /// Cast to `i16`
    fn as_i16(self) -> i16;

    /// Cast to `i24`
    fn as_i24(self) -> i24;

    /// Cast to `i32`
    fn as_i32(self) -> i32;

    /// Cast to `i64`
    fn as_i64(self) -> i64;

    /// Cast to `u8`
    fn as_u8 (self) -> u8 ;

    /// Cast to `u16`
    fn as_u16(self) -> u16;

    /// Cast to `u24`
    fn as_u24(self) -> u24;

    /// Cast to `u32`
    fn as_u32(self) -> u32;

    /// Cast to `u64`
    fn as_u64(self) -> u64;

    /// Cast to `f32`
    fn as_f32(self) -> f32;

    /// Cast to `f64`
    fn as_f64(self) -> f64;

    /// Cast to `i128`
    fn as_i128(self) -> i128;

    /// Cast to `u128`
    fn as_u128(self) -> u128;

    /// Get the size of the sample in bytes
    fn sizeof(self) -> usize {size_of::<Self>()}

    /// Scale to a longer type, the longest type is `i128` or `u128`
    fn to_longer(self) -> Self::Longer;

    /// Scale to a shorter type, the shortest type is `i8` or `u8`
    fn to_shorter(self) -> Self::Shorter;

    /// Is this type a signed type?
    fn is_signed() -> bool;

    /// Is this type an unsigned type?
    fn is_unsigned() -> bool;

    /// Is this type an integer type?
    fn is_integer() -> bool;

    /// Is this type an IEEE 754 floating point number type?
    fn is_float() -> bool;

    /// Convert to a signed number type. No effects to `f32` and `f64`
    fn to_signed(self) -> Self::Signed;

    /// Convert to an unsigned number type. No effects to `f32` and `f64`
    fn to_unsigned(self) -> Self::Unsigned;

    /// Sine wave generator
    /// * The input `x` doesn't need to be related to PI.
    /// * e.g. The type is `i8`, the value is -128, then you will get sin(-PI).
    /// * e.g. The type is `u8`, the value is 0, then you will get sin(-PI) too.
    fn sin<S>(self) -> S where S: SampleType;

    /// Cosine wave generator
    /// * The input `x` doesn't need to be related to PI.
    /// * e.g. The type is `i8`, the value is -128, then you will get cos(-PI).
    /// * e.g. The type is `u8`, the value is 0, then you will get cos(-PI) too.
    fn cos<S>(self) -> S where S: SampleType;

    /// Read from a reader by little-endian
    fn read_le<T>(r: &mut T) -> Result<Self, Error> where T: Read + ?Sized;

    /// Read from a reader by big-endian
    fn read_be<T>(r: &mut T) -> Result<Self, Error> where T: Read + ?Sized;

    /// Write to a writer by little-endian
    fn write_le<T>(self, w: &mut T) -> Result<(), Error> where T: Write + ?Sized;

    /// Write to a writer by big-endian
    fn write_be<T>(self, w: &mut T) -> Result<(), Error> where T: Write + ?Sized;
}

/// * The `SampleFrom` as a utility for `SampleType` to handle function overloading
pub trait SampleFrom: Numeric {
    /// The type we are implementating for
    type ImplFor;

    /// The `to<S>` method, input any type of `SampleType` value, then scale it to `ImplFor` type.
    fn to<S>(s: S) -> Self where S: SampleType;

    /// The `as_<S>` method, input any type of `SampleType` value, then cast it to `ImplFor` type.
    fn as_<S>(s: S) -> Self where S: SampleType;

    /// Sine wave generator
    /// * The input `x` doesn't need to be related to PI.
    /// * e.g. The type is `i8`, the value is -128, then you will get sin(-PI).
    /// * e.g. The type is `u8`, the value is 0, then you will get sin(-PI) too.
    fn sin<S>(s: S) -> Self where S: SampleType;

    /// Cosine wave generator
    /// * The input `x` doesn't need to be related to PI.
    /// * e.g. The type is `i8`, the value is -128, then you will get cos(-PI).
    /// * e.g. The type is `u8`, the value is 0, then you will get cos(-PI) too.
    fn cos<S>(s: S) -> Self where S: SampleType;
}

#[macro_export]
macro_rules! impl_sample_from {
    ($tp:tt) => {
        impl SampleFrom for $tp {
            type ImplFor = $tp;
            #[inline(always)]fn to<S>(s: S) -> Self where S: SampleType {call_to_type!($tp, s)}
            #[inline(always)]fn as_<S>(s: S) -> Self where S: SampleType {call_as_type!($tp, s)}

            fn sin<S>(s: S) -> Self where S: SampleType {
                match S::TYPE_NAME {
                    "i8"   => sin!(i8  , s.as_i8  (), $tp),
                    "i16"  => sin!(i16 , s.as_i16 (), $tp),
                    "i24"  => sin!(i24 , s.as_i24 (), $tp),
                    "i32"  => sin!(i32 , s.as_i32 (), $tp),
                    "i64"  => sin!(i64 , s.as_i64 (), $tp),
                    "i128" => sin!(i128, s.as_i128(), $tp),
                    "u8"   => sin!(u8  , s.as_u8  (), $tp),
                    "u16"  => sin!(u16 , s.as_u16 (), $tp),
                    "u24"  => sin!(u24 , s.as_u24 (), $tp),
                    "u32"  => sin!(u32 , s.as_u32 (), $tp),
                    "u64"  => sin!(u64 , s.as_u64 (), $tp),
                    "u128" => sin!(u128, s.as_u128(), $tp),
                    "f32"  => sin!(f32 , s.as_f32 (), $tp),
                    "f64"  => sin!(f64 , s.as_f64 (), $tp),
                    o => panic!("Unknown type {o}"),
                }
            }

            fn cos<S>(s: S) -> Self where S: SampleType {
                match S::TYPE_NAME {
                    "i8"   => cos!(i8  , s.as_i8  (), $tp),
                    "i16"  => cos!(i16 , s.as_i16 (), $tp),
                    "i24"  => cos!(i24 , s.as_i24 (), $tp),
                    "i32"  => cos!(i32 , s.as_i32 (), $tp),
                    "i64"  => cos!(i64 , s.as_i64 (), $tp),
                    "i128" => cos!(i128, s.as_i128(), $tp),
                    "u8"   => cos!(u8  , s.as_u8  (), $tp),
                    "u16"  => cos!(u16 , s.as_u16 (), $tp),
                    "u24"  => cos!(u24 , s.as_u24 (), $tp),
                    "u32"  => cos!(u32 , s.as_u32 (), $tp),
                    "u64"  => cos!(u64 , s.as_u64 (), $tp),
                    "u128" => cos!(u128, s.as_u128(), $tp),
                    "f32"  => cos!(f32 , s.as_f32 (), $tp),
                    "f64"  => cos!(f64 , s.as_f64 (), $tp),
                    o => panic!("Unknown type {o}"),
                }
            }
        }
    };
}

impl_sample_from!(i8  );
impl_sample_from!(i16 );
impl_sample_from!(i24 );
impl_sample_from!(i32 );
impl_sample_from!(i64 );
impl_sample_from!(i128);
impl_sample_from!(u8  );
impl_sample_from!(u16 );
impl_sample_from!(u24 );
impl_sample_from!(u32 );
impl_sample_from!(u64 );
impl_sample_from!(u128);
impl_sample_from!(f32 );
impl_sample_from!(f64 );

/// * Implement `SampleType` for a specific numeric type.
#[macro_export]
macro_rules! impl_sample_type {
    ($tp:tt, $longer:tt) => {
        impl SampleType for $tp {
            type ImplFor = $tp;
            type Longer = longer_type!($tp);
            type Shorter = shorter_type!($tp);
            type Signed = signed_type!($tp);
            type Unsigned = unsigned_type!($tp);
            const MIDNUM: $tp = mid_number!($tp);
            const TYPE_NAME: &str = stringify!($tp);
            #[inline(always)]
            fn new() -> Self {
                mid_number!($tp)
            }
            #[inline(always)]
            fn zero() -> Self {
                zero_number!($tp)
            }
            #[inline(always)]
            fn scale_from<T>(v: T) -> Self where T: SampleType {
                <$tp as SampleFrom>::to(v)
            }
            #[inline(always)]
            fn cast_from<T>(v: T) -> Self where T: SampleType {
                <$tp as SampleFrom>::as_(v)
            }
            #[inline(always)]
            fn average(s1: Self, s2: Self) -> Self {
                Self::average_arr(&[s1, s2])
            }
            #[inline(always)]
            fn average_arr(arr: &[Self]) -> Self {
                average_arr!($tp, $longer, arr)
            }
            #[inline(always)]fn to_i8 (self) -> i8  {to_i8! ($tp, self)}
            #[inline(always)]fn to_i16(self) -> i16 {to_i16!($tp, self)}
            #[inline(always)]fn to_i24(self) -> i24 {to_i24!($tp, self)}
            #[inline(always)]fn to_i32(self) -> i32 {to_i32!($tp, self)}
            #[inline(always)]fn to_i64(self) -> i64 {to_i64!($tp, self)}
            #[inline(always)]fn to_u8 (self) -> u8  {to_u8! ($tp, self)}
            #[inline(always)]fn to_u16(self) -> u16 {to_u16!($tp, self)}
            #[inline(always)]fn to_u24(self) -> u24 {to_u24!($tp, self)}
            #[inline(always)]fn to_u32(self) -> u32 {to_u32!($tp, self)}
            #[inline(always)]fn to_u64(self) -> u64 {to_u64!($tp, self)}
            #[inline(always)]fn to_f32(self) -> f32 {to_f32!($tp, self)}
            #[inline(always)]fn to_f64(self) -> f64 {to_f64!($tp, self)}
            #[inline(always)]fn to_i128(self) -> i128 {to_i128!($tp, self)}
            #[inline(always)]fn to_u128(self) -> u128 {to_u128!($tp, self)}
            #[inline(always)]fn as_i8 (self) -> i8  {as_i8! ($tp, self)}
            #[inline(always)]fn as_i16(self) -> i16 {as_i16!($tp, self)}
            #[inline(always)]fn as_i24(self) -> i24 {as_i24!($tp, self)}
            #[inline(always)]fn as_i32(self) -> i32 {as_i32!($tp, self)}
            #[inline(always)]fn as_i64(self) -> i64 {as_i64!($tp, self)}
            #[inline(always)]fn as_u8 (self) -> u8  {as_u8! ($tp, self)}
            #[inline(always)]fn as_u16(self) -> u16 {as_u16!($tp, self)}
            #[inline(always)]fn as_u24(self) -> u24 {as_u24!($tp, self)}
            #[inline(always)]fn as_u32(self) -> u32 {as_u32!($tp, self)}
            #[inline(always)]fn as_u64(self) -> u64 {as_u64!($tp, self)}
            #[inline(always)]fn as_f32(self) -> f32 {as_f32!($tp, self)}
            #[inline(always)]fn as_f64(self) -> f64 {as_f64!($tp, self)}
            #[inline(always)]fn as_i128(self) -> i128 {as_i128!($tp, self)}
            #[inline(always)]fn as_u128(self) -> u128 {as_u128!($tp, self)}
            #[inline(always)]
            fn to_longer(self) -> Self::Longer {
                to_longer!($tp, self)
            }
            #[inline(always)]
            fn to_shorter(self) -> Self::Shorter {
                to_shorter!($tp, self)
            }
            #[inline(always)]
            fn is_signed() -> bool {
                is_signed!($tp)
            }
            #[inline(always)]
            fn is_unsigned() -> bool {
                is_unsigned!($tp)
            }
            #[inline(always)]
            fn is_integer() -> bool {
                is_integer!($tp)
            }
            #[inline(always)]
            fn is_float() -> bool {
                is_float!($tp)
            }
            #[inline(always)]
            fn to_signed(self) -> Self::Signed {
                to_signed!($tp, self)
            }
            #[inline(always)]
            fn to_unsigned(self) -> Self::Unsigned {
                to_unsigned!($tp, self)
            }
            #[inline(always)]
            fn read_le<T>(r: &mut T) -> Result<Self, Error>
            where T: Read + ?Sized {
                let mut buf = [0u8; sizeof!($tp)];
                r.read_exact(&mut buf)?;
                Ok(Self::from_le_bytes(buf))
            }
            #[inline(always)]
            fn read_be<T>(r: &mut T) -> Result<Self, Error>
            where T: Read + ?Sized {
                let mut buf = [0u8; sizeof!($tp)];
                r.read_exact(&mut buf)?;
                Ok(Self::from_be_bytes(buf))
            }
            #[inline(always)]
            fn write_le<T>(self, w: &mut T) -> Result<(), Error>
            where T: Write + ?Sized {
                w.write_all(&self.to_le_bytes())
            }
            #[inline(always)]
            fn write_be<T>(self, w: &mut T) -> Result<(), Error>
            where T: Write + ?Sized {
                w.write_all(&self.to_be_bytes())
            }
            fn sin<S>(self) -> S where S: SampleType {
                <S as SampleFrom>::sin(self)
            }
            fn cos<S>(self) -> S where S: SampleType {
                <S as SampleFrom>::cos(self)
            }
        }
    }
}

impl_sample_type!(i8  , i16 );
impl_sample_type!(i16 , i24 );
impl_sample_type!(i24 , i32 );
impl_sample_type!(i32 , i64 );
impl_sample_type!(i64 , i128);
impl_sample_type!(i128, i128);
impl_sample_type!(u8  , u16 );
impl_sample_type!(u16 , u24 );
impl_sample_type!(u24 , u32 );
impl_sample_type!(u32 , u64 );
impl_sample_type!(u64 , u128);
impl_sample_type!(u128, u128);
impl_sample_type!(f32 , f64 );
impl_sample_type!(f64 , f64 );

#[allow(unused_macros)]
macro_rules! test_type {
    ($tp:tt) => {
        test_to_type!($tp, i8  );
        test_to_type!($tp, i16 );
        test_to_type!($tp, i24 );
        test_to_type!($tp, i32 );
        test_to_type!($tp, i64 );
        test_to_type!($tp, i128);
        test_to_type!($tp, u8  );
        test_to_type!($tp, u16 );
        test_to_type!($tp, u24 );
        test_to_type!($tp, u32 );
        test_to_type!($tp, u64 );
        test_to_type!($tp, u128);
        test_to_type!($tp, f32 );
        test_to_type!($tp, f64 );
    }
}

use std::{fs::File, io::BufWriter};

fn write_test<S, D>(filename: &str, data: &[u8])
where
    S: SampleType,
    D: SampleType {
    let mut f = BufWriter::new(File::create(filename).unwrap());

    let test1: Vec<S> = data.iter().map(|v|{S::scale_from(*v)}).collect();
    f.write_all(b"======== TEST1 ========\n").unwrap();
    test1.iter().for_each(|v|{f.write_all(&format!("{v}\n").into_bytes()).unwrap()});

    let test2: Vec<D> = test1.iter().map(|v|{D::scale_from(*v)}).collect();
    f.write_all(b"======== TEST2 ========\n").unwrap();
    test2.iter().for_each(|v|{f.write_all(&format!("{v}\n").into_bytes()).unwrap()});

    let test3: Vec<D> = test1.iter().map(|v|{v.sin::<D>()}).collect();
    f.write_all(b"======== TEST3 ========\n").unwrap();
    test3.iter().for_each(|v|{f.write_all(&format!("{v}\n").into_bytes()).unwrap()});

    let test4: Vec<D> = test1.iter().map(|v|{v.cos::<D>()}).collect();
    f.write_all(b"======== TEST4 ========\n").unwrap();
    test4.iter().for_each(|v|{f.write_all(&format!("{v}\n").into_bytes()).unwrap()});
}

#[allow(unused_macros)]
macro_rules! test_to_type {
    ($tp1:tt, $tp2:tt) => {
        {
            let data = vec![0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF];
            write_test::<$tp1, $tp2>(&format!("test_{}_{}.txt", stringify!($tp1), stringify!($tp2)), &data);
        }
    }
}

#[test]
fn test() {
    test_type!(i8  );
    test_type!(i16 );
    test_type!(i24 );
    test_type!(i32 );
    test_type!(i64 );
    test_type!(i128);
    test_type!(u8  );
    test_type!(u16 );
    test_type!(u24 );
    test_type!(u32 );
    test_type!(u64 );
    test_type!(u128);
    test_type!(f32 );
    test_type!(f64 );
}
