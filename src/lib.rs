#![allow(dead_code)]

pub mod mod_i24;
pub mod mod_u24;

pub use mod_i24::*;
pub use mod_u24::*;

use std::{any::type_name, io::{Read, Write, Error}, mem::size_of, fmt::Debug, clone::Clone};
use std::ops::{Add, Sub, Mul, Div, AddAssign, SubAssign, MulAssign, DivAssign};
use std::ops::{BitAnd, BitOr, BitXor, Shl, Shr, BitAndAssign, BitOrAssign, BitXorAssign, ShlAssign, ShrAssign};
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
RemAssign {}

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
RemAssign {}

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

    type Longer;
    type Shorter;
    type Signed;
    type Unsigned;
    fn new() -> Self;
    fn from(v: impl SampleType) -> Self;
    fn average(s1: Self, s2: Self) -> Self;
    fn average_arr(arr: &[Self]) -> Self;
    fn to_i8 (&self) -> i8;
    fn to_i16(&self) -> i16;
    fn to_i24(&self) -> i24;
    fn to_i32(&self) -> i32;
    fn to_i64(&self) -> i64;
    fn to_u8 (&self) -> u8;
    fn to_u16(&self) -> u16;
    fn to_u24(&self) -> u24;
    fn to_u32(&self) -> u32;
    fn to_u64(&self) -> u64;
    fn to_f32(&self) -> f32;
    fn to_f64(&self) -> f64;
    fn as_i8 (&self) -> i8;
    fn as_i16(&self) -> i16;
    fn as_i24(&self) -> i24;
    fn as_i32(&self) -> i32;
    fn as_i64(&self) -> i64;
    fn as_u8 (&self) -> u8;
    fn as_u16(&self) -> u16;
    fn as_u24(&self) -> u24;
    fn as_u32(&self) -> u32;
    fn as_u64(&self) -> u64;
    fn as_f32(&self) -> f32;
    fn as_f64(&self) -> f64;
    fn to_longer(&self) -> Self::Longer;
    fn to_shorter(&self) -> Self::Shorter;
    fn is_signed(&self) -> bool;
    fn is_unsigned(&self) -> bool;
    fn is_integer(&self) -> bool;
    fn is_float(&self) -> bool;
    fn to_signed(&self) -> Self::Signed;
    fn to_unsigned(&self) -> Self::Unsigned {
        panic!("The type `{}` can't be turned to an unsigned type.", type_name::<Self>());
    }
    fn read_le<T>(r: &mut T) -> Result<Self, Error> where T: Read + ?Sized;
    fn read_be<T>(r: &mut T) -> Result<Self, Error> where T: Read + ?Sized;
    fn write_le<T>(&self, w: &mut T) -> Result<(), Error> where T: Write + ?Sized;
    fn write_be<T>(&self, w: &mut T) -> Result<(), Error> where T: Write + ?Sized;
}

pub trait SampleFrom: Debug + Sized + Clone + Copy + 'static {
    fn to(s: impl SampleType) -> Self;
}
impl SampleFrom for i8  {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_i8()  }}
impl SampleFrom for i16 {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_i16() }}
impl SampleFrom for i24 {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_i24() }}
impl SampleFrom for i32 {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_i32() }}
impl SampleFrom for i64 {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_i64() }}
impl SampleFrom for u8  {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_u8()  }}
impl SampleFrom for u16 {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_u16() }}
impl SampleFrom for u24 {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_u24() }}
impl SampleFrom for u32 {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_u32() }}
impl SampleFrom for u64 {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_u64() }}
impl SampleFrom for f32 {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_f32() }}
impl SampleFrom for f64 {#[inline(always)] fn to(s: impl SampleType) -> Self { s.to_f64() }}

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
RemAssign {}


