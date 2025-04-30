#![allow(dead_code)]

pub mod mod_i24;
pub mod mod_u24;

pub use mod_i24::*;
pub use mod_u24::*;

use std::{io::{Read, Write, Error}, mem::size_of, fmt::Debug, clone::Clone};
use std::ops::{Add, Sub, Mul, Div, AddAssign, SubAssign, MulAssign, DivAssign};
use std::ops::{BitAnd, BitOr, BitXor, Shl, Shr, BitAndAssign, BitOrAssign, BitXorAssign, ShlAssign, ShrAssign, Not};
use std::ops::{Rem, RemAssign};
use std::ops::{Neg};

pub trait Numeric:
Add<Output = Self> + Sub<Output = Self> + Mul<Output = Self> + Div<Output = Self> +
AddAssign + SubAssign + MulAssign + DivAssign +
Debug + Sized + Clone + Copy + 'static {}

impl<T> Numeric for T where T:
Add<Output = Self> + Sub<Output = Self> + Mul<Output = Self> + Div<Output = Self> +
AddAssign + SubAssign + MulAssign + DivAssign +
Debug + Sized + Clone + Copy + 'static {}

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

pub trait SampleTypeIntegerSigned: SampleTypeInteger + Neg {}
impl<T> SampleTypeIntegerSigned for T where T: SampleTypeInteger + Neg {}

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

macro_rules! is_unsigned {
    ($tp:tt) => {!is_signed!($tp)};
}

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

macro_rules! is_float {
    ($tp:tt) => {!is_integer!($tp)};
}

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

macro_rules! as_longer {
    (i8  , $v:expr) => {$v as i16};
    (i16 , $v:expr) => {<i24 as From>::from($v)};
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

#[allow(unused_macros)]
macro_rules! as_shorter {
    (i8  , $v:expr) => {$v as i8};
    (i16 , $v:expr) => {$v as i8};
    (i24 , $v:expr) => {$v.as_i16()};
    (i32 , $v:expr) => {<i24 as From>::from($v)};
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

macro_rules! to_longer {
    (i8  , $v:expr) => {{let longer = as_longer!(i8, $v); (longer << 8) | (to_unsigned!(i16, longer) as i16)}};
    (i16 , $v:expr) => {{let b = $v.to_le_bytes(); i24(to_unsigned!(i8, b[1] as i8), b[0], b[1])}};
    (i24 , $v:expr) => {($v.as_i32() << 8) | (to_unsigned!(i8, $v.2 as i8) as i32)};
    (i32 , $v:expr) => {{let longer = as_longer!(i32, $v); (longer << 32) | (to_unsigned!(i64, longer) as i64)}};
    (i64 , $v:expr) => {{let longer = as_longer!(i64, $v); (longer << 64) | (to_unsigned!(i128, longer) as i128)}};
    (i128, $v:expr) => {$v};
    (u8  , $v:expr) => {{let longer = as_longer!(u8, $v); (longer << 8) | (longer as u16)}};
    (u16 , $v:expr) => {{let b = $v.to_le_bytes(); u24(b[1], b[0], b[1])}};
    (u24 , $v:expr) => {($v.as_u32() << 8) | ($v.2 as u32)};
    (u32 , $v:expr) => {{let longer = as_longer!(u32, $v); (longer << 32) | (longer as u64)}};
    (u64 , $v:expr) => {{let longer = as_longer!(u64, $v); (longer << 64) | (longer as u128)}};
    (u128, $v:expr) => {$v};
    (f32 , $v:expr) => {$v as f64};
    (f64 , $v:expr) => {$v};
}

macro_rules! to_shorter {
    (i8  , $v:expr) => {$v};
    (i16 , $v:expr) => {($v >> 8) as i8};
    (i24 , $v:expr) => {i16::from_le_bytes([$v.1, $v.2])};
    (i32 , $v:expr) => {<i24 as From<i32>>::from($v >> 8)};
    (i64 , $v:expr) => {($v >> 32) as i32};
    (i128, $v:expr) => {($v >> 64) as i64};
    (u8  , $v:expr) => {$v};
    (u16 , $v:expr) => {($v >> 8) as u8};
    (u24 , $v:expr) => {u16::from_le_bytes([$v.1, $v.2])};
    (u32 , $v:expr) => {<u24 as From<u32>>::from($v >> 8)};
    (u64 , $v:expr) => {($v >> 32) as u32};
    (u128, $v:expr) => {($v >> 64) as u64};
    (f32 , $v:expr) => {$v};
    (f64 , $v:expr) => {$v as f32};
}

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

macro_rules! to_i16 {
    (i8  , $v:expr) => {to_longer!(i8, $v)};
    (i16 , $v:expr) => {$v};
    (i24 , $v:expr) => {to_shorter!(i24, $v)};
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

macro_rules! to_i24 {
    (i8  , $v:expr) => {{let u = to_unsigned!(i8, $v); i24(u, u, $v as u8)}};
    (i16 , $v:expr) => {to_longer!(i16, $v)};
    (i24 , $v:expr) => {$v};
    (i32 , $v:expr) => {to_shorter!(i32, $v)};
    (i64 , $v:expr) => {<i24 as From<i64>>::from($v >> 40)};
    (i128, $v:expr) => {<i24 as From<i128>>::from($v >> 104)};
    (u8  , $v:expr) => {to_i24!(i8  , to_signed!(u8  , $v))};
    (u16 , $v:expr) => {to_i24!(i16 , to_signed!(u16 , $v))};
    (u24 , $v:expr) => {to_i24!(i24 , to_signed!(u24 , $v))};
    (u32 , $v:expr) => {to_i24!(i32 , to_signed!(u32 , $v))};
    (u64 , $v:expr) => {to_i24!(i64 , to_signed!(u64 , $v))};
    (u128, $v:expr) => {to_i24!(i128, to_signed!(u128, $v))};
    (f32 , $v:expr) => {<i24 as From<i32>>::from(($v * 0x7FFFFF as f32) as i32)};
    (f64 , $v:expr) => {<i24 as From<i32>>::from(($v * 0x7FFFFF as f64) as i32)};
}

macro_rules! to_i32 {
    (i8  , $v:expr) => {{let longer = to_i16!(i8, $v); ((longer as i32) << 16) | (to_unsigned!(i16, longer) as i32)}};
    (i16 , $v:expr) => {(($v as i32) << 16) | (to_unsigned!(i16, $v) as i32)};
    (i24 , $v:expr) => {to_longer!(i24, $v)};
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

macro_rules! to_i64 {
    (i8  , $v:expr) => {{let longer = to_i32!(i8 , $v); ((longer as i64) << 32) | (to_unsigned!(i32, longer) as i64)}};
    (i16 , $v:expr) => {{let longer = to_i32!(i16, $v); ((longer as i64) << 32) | (to_unsigned!(i32, longer) as i64)}};
    (i24 , $v:expr) => {{let u2 = to_unsigned!(i8, $v.2 as i8); i64::from_le_bytes([$v.1, u2, $v.0, $v.1, u2, $v.0, $v.1, $v.2])}};
    (i32 , $v:expr) => {to_longer!(i32, $v)};
    (i64 , $v:expr) => {$v};
    (i128, $v:expr) => {to_shorter!(i128, $v)};
    (u8  , $v:expr) => {to_i64!(i8  , to_signed!(u8  , $v))};
    (u16 , $v:expr) => {to_i64!(i16 , to_signed!(u16 , $v))};
    (u24 , $v:expr) => {i64::from_le_bytes([$v.1, $v.2, $v.0, $v.1, $v.2, $v.0, $v.1, to_signed!(u8, $v.2) as u8])};
    (u32 , $v:expr) => {to_i64!(i32 , to_signed!(u32 , $v))};
    (u64 , $v:expr) => {to_i64!(i64 , to_signed!(u64 , $v))};
    (u128, $v:expr) => {to_i64!(i128, to_signed!(u128, $v))};
    (f32 , $v:expr) => {($v * i64::MAX as f32) as i64};
    (f64 , $v:expr) => {($v * i64::MAX as f64) as i64};
}

macro_rules! to_i128 {
    (i8  , $v:expr) => {{let longer = to_i64!(i8 , $v); ((longer as i128) << 64) | (to_unsigned!(i64, longer) as i128)}};
    (i16 , $v:expr) => {{let longer = to_i64!(i16, $v); ((longer as i128) << 64) | (to_unsigned!(i64, longer) as i128)}};
    (i24 , $v:expr) => {{let u2 = to_unsigned!(i8, $v.2 as i8); i128::from_le_bytes([u2, $v.0, $v.1, u2, $v.0, $v.1, u2, $v.0, $v.1, u2, $v.0, $v.1, u2, $v.0, $v.1, $v.2])}};
    (i32 , $v:expr) => {{let longer = to_i64!(i32, $v); ((longer as i128) << 64) | (to_unsigned!(i64, longer) as i128)}};
    (i64 , $v:expr) => {to_longer!(i64, $v)};
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

macro_rules! to_f32 {
    (i8  , $v:expr) => {($v as f32) / (i8  ::MAX) as f32};
    (i16 , $v:expr) => {($v as f32) / (i16 ::MAX) as f32};
    (i24 , $v:expr) => {($v.as_i32() as f32) / (0x7FFFFF) as f32};
    (i32 , $v:expr) => {($v as f32) / (i32 ::MAX) as f32};
    (i64 , $v:expr) => {($v as f32) / (i64 ::MAX) as f32};
    (i128, $v:expr) => {($v as f32) / (i128::MAX) as f32};
    (u8  , $v:expr) => {(to_signed!(u8  , $v) as f32) / (i8  ::MAX) as f32};
    (u16 , $v:expr) => {(to_signed!(u16 , $v) as f32) / (i16 ::MAX) as f32};
    (u24 , $v:expr) => {(to_signed!(u24 , $v).as_i32() as f32) / (0x7FFFFF) as f32};
    (u32 , $v:expr) => {(to_signed!(u32 , $v) as f32) / (i32 ::MAX) as f32};
    (u64 , $v:expr) => {(to_signed!(u64 , $v) as f32) / (i64 ::MAX) as f32};
    (u128, $v:expr) => {(to_signed!(u128, $v) as f32) / (i128::MAX) as f32};
    (f32 , $v:expr) => {$v};
    (f64 , $v:expr) => {$v as f32};
}

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

macro_rules! as_i8 {
    (i8  , $v:expr) => {$v as i8};
    (i16 , $v:expr) => {$v as i8};
    (i24 , $v:expr) => {$v.as_i8()};
    (i32 , $v:expr) => {$v as i8};
    (i64 , $v:expr) => {$v as i8};
    (i128, $v:expr) => {$v as i8};
    (u8  , $v:expr) => {$v as i8};
    (u16 , $v:expr) => {$v as i8};
    (u24 , $v:expr) => {$v.as_i8()};
    (u32 , $v:expr) => {$v as i8};
    (u64 , $v:expr) => {$v as i8};
    (u128, $v:expr) => {$v as i8};
    (f32 , $v:expr) => {$v as i8};
    (f64 , $v:expr) => {$v as i8};
}

macro_rules! as_i16 {
    (i8  , $v:expr) => {$v as i16};
    (i16 , $v:expr) => {$v as i16};
    (i24 , $v:expr) => {$v.as_i16()};
    (i32 , $v:expr) => {$v as i16};
    (i64 , $v:expr) => {$v as i16};
    (i128, $v:expr) => {$v as i16};
    (u8  , $v:expr) => {$v as i16};
    (u16 , $v:expr) => {$v as i16};
    (u24 , $v:expr) => {$v.as_i16()};
    (u32 , $v:expr) => {$v as i16};
    (u64 , $v:expr) => {$v as i16};
    (u128, $v:expr) => {$v as i16};
    (f32 , $v:expr) => {$v as i16};
    (f64 , $v:expr) => {$v as i16};
}

macro_rules! as_i24 {
    (i8  , $v:expr) => {<i24 as From<i8  >>::from($v)};
    (i16 , $v:expr) => {<i24 as From<i16 >>::from($v)};
    (i24 , $v:expr) => {<i24 as From<i24 >>::from($v)};
    (i32 , $v:expr) => {<i24 as From<i32 >>::from($v)};
    (i64 , $v:expr) => {<i24 as From<i64 >>::from($v)};
    (i128, $v:expr) => {<i24 as From<i128>>::from($v)};
    (u8  , $v:expr) => {<i24 as From<u8  >>::from($v)};
    (u16 , $v:expr) => {<i24 as From<u16 >>::from($v)};
    (u24 , $v:expr) => {<i24 as From<u24 >>::from($v)};
    (u32 , $v:expr) => {<i24 as From<u32 >>::from($v)};
    (u64 , $v:expr) => {<i24 as From<u64 >>::from($v)};
    (u128, $v:expr) => {<i24 as From<u128>>::from($v)};
    (f32 , $v:expr) => {<i24 as From<f32 >>::from($v)};
    (f64 , $v:expr) => {<i24 as From<f64 >>::from($v)};
}

macro_rules! as_i32 {
    (i8  , $v:expr) => {$v as i32};
    (i16 , $v:expr) => {$v as i32};
    (i24 , $v:expr) => {$v.as_i32()};
    (i32 , $v:expr) => {$v as i32};
    (i64 , $v:expr) => {$v as i32};
    (i128, $v:expr) => {$v as i32};
    (u8  , $v:expr) => {$v as i32};
    (u16 , $v:expr) => {$v as i32};
    (u24 , $v:expr) => {$v.as_i32()};
    (u32 , $v:expr) => {$v as i32};
    (u64 , $v:expr) => {$v as i32};
    (u128, $v:expr) => {$v as i32};
    (f32 , $v:expr) => {$v as i32};
    (f64 , $v:expr) => {$v as i32};
}

macro_rules! as_i64 {
    (i8  , $v:expr) => {$v as i64};
    (i16 , $v:expr) => {$v as i64};
    (i24 , $v:expr) => {$v.as_i64()};
    (i32 , $v:expr) => {$v as i64};
    (i64 , $v:expr) => {$v as i64};
    (i128, $v:expr) => {$v as i64};
    (u8  , $v:expr) => {$v as i64};
    (u16 , $v:expr) => {$v as i64};
    (u24 , $v:expr) => {$v.as_i64()};
    (u32 , $v:expr) => {$v as i64};
    (u64 , $v:expr) => {$v as i64};
    (u128, $v:expr) => {$v as i64};
    (f32 , $v:expr) => {$v as i64};
    (f64 , $v:expr) => {$v as i64};
}

macro_rules! as_i128 {
    (i8  , $v:expr) => {$v as i128};
    (i16 , $v:expr) => {$v as i128};
    (i24 , $v:expr) => {$v.as_i128()};
    (i32 , $v:expr) => {$v as i128};
    (i64 , $v:expr) => {$v as i128};
    (i128, $v:expr) => {$v as i128};
    (u8  , $v:expr) => {$v as i128};
    (u16 , $v:expr) => {$v as i128};
    (u24 , $v:expr) => {$v.as_i128()};
    (u32 , $v:expr) => {$v as i128};
    (u64 , $v:expr) => {$v as i128};
    (u128, $v:expr) => {$v as i128};
    (f32 , $v:expr) => {$v as i128};
    (f64 , $v:expr) => {$v as i128};
}

macro_rules! as_u8 {
    (i8  , $v:expr) => {$v as u8};
    (i16 , $v:expr) => {$v as u8};
    (i24 , $v:expr) => {$v.as_u8()};
    (i32 , $v:expr) => {$v as u8};
    (i64 , $v:expr) => {$v as u8};
    (i128, $v:expr) => {$v as u8};
    (u8  , $v:expr) => {$v as u8};
    (u16 , $v:expr) => {$v as u8};
    (u24 , $v:expr) => {$v.as_u8()};
    (u32 , $v:expr) => {$v as u8};
    (u64 , $v:expr) => {$v as u8};
    (u128, $v:expr) => {$v as u8};
    (f32 , $v:expr) => {$v as u8};
    (f64 , $v:expr) => {$v as u8};
}

macro_rules! as_u16 {
    (i8  , $v:expr) => {$v as u16};
    (i16 , $v:expr) => {$v as u16};
    (i24 , $v:expr) => {$v.as_u16()};
    (i32 , $v:expr) => {$v as u16};
    (i64 , $v:expr) => {$v as u16};
    (i128, $v:expr) => {$v as u16};
    (u8  , $v:expr) => {$v as u16};
    (u16 , $v:expr) => {$v as u16};
    (u24 , $v:expr) => {$v.as_u16()};
    (u32 , $v:expr) => {$v as u16};
    (u64 , $v:expr) => {$v as u16};
    (u128, $v:expr) => {$v as u16};
    (f32 , $v:expr) => {$v as u16};
    (f64 , $v:expr) => {$v as u16};
}

macro_rules! as_u24 {
    (i8  , $v:expr) => {<u24 as From<i8  >>::from($v)};
    (i16 , $v:expr) => {<u24 as From<i16 >>::from($v)};
    (i24 , $v:expr) => {<u24 as From<i24 >>::from($v)};
    (i32 , $v:expr) => {<u24 as From<i32 >>::from($v)};
    (i64 , $v:expr) => {<u24 as From<i64 >>::from($v)};
    (i128, $v:expr) => {<u24 as From<i128>>::from($v)};
    (u8  , $v:expr) => {<u24 as From<u8  >>::from($v)};
    (u16 , $v:expr) => {<u24 as From<u16 >>::from($v)};
    (u24 , $v:expr) => {<u24 as From<u24 >>::from($v)};
    (u32 , $v:expr) => {<u24 as From<u32 >>::from($v)};
    (u64 , $v:expr) => {<u24 as From<u64 >>::from($v)};
    (u128, $v:expr) => {<u24 as From<u128>>::from($v)};
    (f32 , $v:expr) => {<u24 as From<f32 >>::from($v)};
    (f64 , $v:expr) => {<u24 as From<f64 >>::from($v)};
}

macro_rules! as_u32 {
    (i8  , $v:expr) => {$v as u32};
    (i16 , $v:expr) => {$v as u32};
    (i24 , $v:expr) => {$v.as_u32()};
    (i32 , $v:expr) => {$v as u32};
    (i64 , $v:expr) => {$v as u32};
    (i128, $v:expr) => {$v as u32};
    (u8  , $v:expr) => {$v as u32};
    (u16 , $v:expr) => {$v as u32};
    (u24 , $v:expr) => {$v.as_u32()};
    (u32 , $v:expr) => {$v as u32};
    (u64 , $v:expr) => {$v as u32};
    (u128, $v:expr) => {$v as u32};
    (f32 , $v:expr) => {$v as u32};
    (f64 , $v:expr) => {$v as u32};
}

macro_rules! as_u64 {
    (i8  , $v:expr) => {$v as u64};
    (i16 , $v:expr) => {$v as u64};
    (i24 , $v:expr) => {$v.as_u64()};
    (i32 , $v:expr) => {$v as u64};
    (i64 , $v:expr) => {$v as u64};
    (i128, $v:expr) => {$v as u64};
    (u8  , $v:expr) => {$v as u64};
    (u16 , $v:expr) => {$v as u64};
    (u24 , $v:expr) => {$v.as_u64()};
    (u32 , $v:expr) => {$v as u64};
    (u64 , $v:expr) => {$v as u64};
    (u128, $v:expr) => {$v as u64};
    (f32 , $v:expr) => {$v as u64};
    (f64 , $v:expr) => {$v as u64};
}

macro_rules! as_u128 {
    (i8  , $v:expr) => {$v as u128};
    (i16 , $v:expr) => {$v as u128};
    (i24 , $v:expr) => {$v.as_u128()};
    (i32 , $v:expr) => {$v as u128};
    (i64 , $v:expr) => {$v as u128};
    (i128, $v:expr) => {$v as u128};
    (u8  , $v:expr) => {$v as u128};
    (u16 , $v:expr) => {$v as u128};
    (u24 , $v:expr) => {$v.as_u128()};
    (u32 , $v:expr) => {$v as u128};
    (u64 , $v:expr) => {$v as u128};
    (u128, $v:expr) => {$v as u128};
    (f32 , $v:expr) => {$v as u128};
    (f64 , $v:expr) => {$v as u128};
}

macro_rules! as_f32 {
    (i8  , $v:expr) => {$v as f32};
    (i16 , $v:expr) => {$v as f32};
    (i24 , $v:expr) => {$v.as_f32()};
    (i32 , $v:expr) => {$v as f32};
    (i64 , $v:expr) => {$v as f32};
    (i128, $v:expr) => {$v as f32};
    (u8  , $v:expr) => {$v as f32};
    (u16 , $v:expr) => {$v as f32};
    (u24 , $v:expr) => {$v.as_f32()};
    (u32 , $v:expr) => {$v as f32};
    (u64 , $v:expr) => {$v as f32};
    (u128, $v:expr) => {$v as f32};
    (f32 , $v:expr) => {$v as f32};
    (f64 , $v:expr) => {$v as f32};
}

macro_rules! as_f64 {
    (i8  , $v:expr) => {$v as f64};
    (i16 , $v:expr) => {$v as f64};
    (i24 , $v:expr) => {$v.as_f64()};
    (i32 , $v:expr) => {$v as f64};
    (i64 , $v:expr) => {$v as f64};
    (i128, $v:expr) => {$v as f64};
    (u8  , $v:expr) => {$v as f64};
    (u16 , $v:expr) => {$v as f64};
    (u24 , $v:expr) => {$v.as_f64()};
    (u32 , $v:expr) => {$v as f64};
    (u64 , $v:expr) => {$v as f64};
    (u128, $v:expr) => {$v as f64};
    (f32 , $v:expr) => {$v as f64};
    (f64 , $v:expr) => {$v as f64};
}

macro_rules! average_arr {
    ($tp:tt, $longer:tt, $arr:expr) => {
        {
            type Longer = $longer;
            let mut sum: Longer = Longer::zero();
            $arr.iter().for_each(|x|{sum += to_longer!($tp, *x);});
            to_type!($longer, $tp, sum / to_type!(u64, $longer, $arr.len() as u64))
        }
    };
}

pub trait SampleType: Numeric {
    type Longer;
    type Shorter;
    type Signed;
    type Unsigned;
    const MIDNUM: Self;
    fn new() -> Self;
    fn zero() -> Self;
    fn from(v: impl SampleType) -> Self;
    fn average(s1: Self, s2: Self) -> Self;
    fn average_arr(arr: &[Self]) -> Self;
    fn to_i8 (&self) -> i8 ;
    fn to_i16(&self) -> i16;
    fn to_i24(&self) -> i24;
    fn to_i32(&self) -> i32;
    fn to_i64(&self) -> i64;
    fn to_u8 (&self) -> u8 ;
    fn to_u16(&self) -> u16;
    fn to_u24(&self) -> u24;
    fn to_u32(&self) -> u32;
    fn to_u64(&self) -> u64;
    fn to_f32(&self) -> f32;
    fn to_f64(&self) -> f64;
    fn to_i128(&self) -> i128;
    fn to_u128(&self) -> u128;
    fn as_i8 (&self) -> i8 ;
    fn as_i16(&self) -> i16;
    fn as_i24(&self) -> i24;
    fn as_i32(&self) -> i32;
    fn as_i64(&self) -> i64;
    fn as_u8 (&self) -> u8 ;
    fn as_u16(&self) -> u16;
    fn as_u24(&self) -> u24;
    fn as_u32(&self) -> u32;
    fn as_u64(&self) -> u64;
    fn as_f32(&self) -> f32;
    fn as_f64(&self) -> f64;
    fn as_i128(&self) -> i128;
    fn as_u128(&self) -> u128;
    fn sizeof(&self) -> usize {size_of::<Self>()}
    fn to_longer(&self) -> Self::Longer;
    fn to_shorter(&self) -> Self::Shorter;
    fn is_signed() -> bool;
    fn is_unsigned() -> bool;
    fn is_integer() -> bool;
    fn is_float() -> bool;
    fn to_signed(&self) -> Self::Signed;
    fn to_unsigned(&self) -> Self::Unsigned;
    fn read_le<T>(r: &mut T) -> Result<Self, Error> where T: Read + ?Sized;
    fn read_be<T>(r: &mut T) -> Result<Self, Error> where T: Read + ?Sized;
    fn write_le<T>(&self, w: &mut T) -> Result<(), Error> where T: Write + ?Sized;
    fn write_be<T>(&self, w: &mut T) -> Result<(), Error> where T: Write + ?Sized;
}

pub trait SampleFrom: Numeric {
    fn to(s: impl SampleType) -> Self;
}
impl SampleFrom for i8  {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_i8()  }}
impl SampleFrom for i16 {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_i16() }}
impl SampleFrom for i24 {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_i24() }}
impl SampleFrom for i32 {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_i32() }}
impl SampleFrom for i64 {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_i64() }}
impl SampleFrom for i128{#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_i128()}}
impl SampleFrom for u8  {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_u8()  }}
impl SampleFrom for u16 {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_u16() }}
impl SampleFrom for u24 {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_u24() }}
impl SampleFrom for u32 {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_u32() }}
impl SampleFrom for u64 {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_u64() }}
impl SampleFrom for u128{#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_u128()}}
impl SampleFrom for f32 {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_f32() }}
impl SampleFrom for f64 {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_f64() }}

fn conv<S, T>(v: S) -> T
where
    S: SampleType,
    T: SampleType {
    T::from(v)
}

macro_rules! impl_sample_type {
    ($tp:tt, $longer:tt) => {
        impl SampleType for $tp {
            type Longer = longer_type!($tp);
            type Shorter = shorter_type!($tp);
            type Signed = signed_type!($tp);
            type Unsigned = unsigned_type!($tp);
            const MIDNUM: $tp = mid_number!($tp);
            #[inline(always)]
            fn new() -> Self {
                mid_number!($tp)
            }
            #[inline(always)]
            fn zero() -> Self {
                zero_number!($tp)
            }
            #[inline(always)]
            fn from(v: impl SampleType) -> Self {
                <$tp as SampleFrom>::to(v)
            }
            #[inline(always)]
            fn average(s1: Self, s2: Self) -> Self {
                Self::average_arr(&[s1, s2])
            }
            #[inline(always)]
            fn average_arr(arr: &[Self]) -> Self {
                average_arr!($tp, $longer, arr)
            }
            #[inline(always)]fn to_i8 (&self) -> i8  {to_i8! ($tp, *self)}
            #[inline(always)]fn to_i16(&self) -> i16 {to_i16!($tp, *self)}
            #[inline(always)]fn to_i24(&self) -> i24 {to_i24!($tp, *self)}
            #[inline(always)]fn to_i32(&self) -> i32 {to_i32!($tp, *self)}
            #[inline(always)]fn to_i64(&self) -> i64 {to_i64!($tp, *self)}
            #[inline(always)]fn to_u8 (&self) -> u8  {to_u8! ($tp, *self)}
            #[inline(always)]fn to_u16(&self) -> u16 {to_u16!($tp, *self)}
            #[inline(always)]fn to_u24(&self) -> u24 {to_u24!($tp, *self)}
            #[inline(always)]fn to_u32(&self) -> u32 {to_u32!($tp, *self)}
            #[inline(always)]fn to_u64(&self) -> u64 {to_u64!($tp, *self)}
            #[inline(always)]fn to_f32(&self) -> f32 {to_f32!($tp, *self)}
            #[inline(always)]fn to_f64(&self) -> f64 {to_f64!($tp, *self)}
            #[inline(always)]fn to_i128(&self) -> i128 {to_i128!($tp, *self)}
            #[inline(always)]fn to_u128(&self) -> u128 {to_u128!($tp, *self)}
            #[inline(always)]fn as_i8 (&self) -> i8  {as_i8! ($tp, *self)}
            #[inline(always)]fn as_i16(&self) -> i16 {as_i16!($tp, *self)}
            #[inline(always)]fn as_i24(&self) -> i24 {as_i24!($tp, *self)}
            #[inline(always)]fn as_i32(&self) -> i32 {as_i32!($tp, *self)}
            #[inline(always)]fn as_i64(&self) -> i64 {as_i64!($tp, *self)}
            #[inline(always)]fn as_u8 (&self) -> u8  {as_u8! ($tp, *self)}
            #[inline(always)]fn as_u16(&self) -> u16 {as_u16!($tp, *self)}
            #[inline(always)]fn as_u24(&self) -> u24 {as_u24!($tp, *self)}
            #[inline(always)]fn as_u32(&self) -> u32 {as_u32!($tp, *self)}
            #[inline(always)]fn as_u64(&self) -> u64 {as_u64!($tp, *self)}
            #[inline(always)]fn as_f32(&self) -> f32 {as_f32!($tp, *self)}
            #[inline(always)]fn as_f64(&self) -> f64 {as_f64!($tp, *self)}
            #[inline(always)]fn as_i128(&self) -> i128 {as_i128!($tp, *self)}
            #[inline(always)]fn as_u128(&self) -> u128 {as_u128!($tp, *self)}
            #[inline(always)]
            fn to_longer(&self) -> Self::Longer {
                to_longer!($tp, *self)
            }
            #[inline(always)]
            fn to_shorter(&self) -> Self::Shorter {
                to_shorter!($tp, *self)
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
            fn to_signed(&self) -> Self::Signed {
                to_signed!($tp, *self)
            }
            #[inline(always)]
            fn to_unsigned(&self) -> Self::Unsigned {
                to_unsigned!($tp, *self)
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
            fn write_le<T>(&self, w: &mut T) -> Result<(), Error>
            where T: Write + ?Sized {
                w.write_all(&self.to_le_bytes())
            }
            #[inline(always)]
            fn write_be<T>(&self, w: &mut T) -> Result<(), Error>
            where T: Write + ?Sized {
                w.write_all(&self.to_be_bytes())
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

