#![allow(dead_code)]

use std::ops::{Add, Sub, Mul, Div, AddAssign, SubAssign, MulAssign, DivAssign};
use std::ops::{BitAnd, BitOr, BitXor, Shl, Shr, BitAndAssign, BitOrAssign, BitXorAssign, ShlAssign, ShrAssign, Not};
use std::ops::{Rem, RemAssign};


use std::cmp::Ordering;

use std::fmt::{self, Display, Formatter};

use crate::mod_i24::i24;

/// * The tuple struct is little-endian
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
#[allow(non_camel_case_types)]
#[repr(C)]
pub struct u24(pub u8, pub u8, pub u8);

impl u24{
    pub const MAX: u24 = u24(0xFF, 0xFF, 0xFF);
    pub const MIN: u24 = u24(0x00, 0x00, 0x00);
    #[inline(always)]
    pub fn from_le_bytes(bytes: [u8; 3]) -> Self {
        Self(bytes[0], bytes[1], bytes[2])
    }
    #[inline(always)]
    pub fn from_be_bytes(bytes: [u8; 3]) -> Self {
        Self(bytes[2], bytes[1], bytes[0])
    }
    #[inline(always)]
    pub fn to_le_bytes(self) -> [u8; 3] {
        [self.0, self.1, self.2]
    }
    #[inline(always)]
    pub fn to_be_bytes(self) -> [u8; 3] {
        [self.2, self.1, self.0]
    }
    #[inline(always)]
    pub fn as_i128(self) -> i128 {
        i128::from_le_bytes([self.0, self.1, self.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])
    }
    #[inline(always)]
    pub fn as_i64(self) -> i64 {
        i64::from_le_bytes([self.0, self.1, self.2, 0, 0, 0, 0, 0])
    }
    #[inline(always)]
    pub fn as_i32(self) -> i32 {
        i32::from_le_bytes([self.0, self.1, self.2, 0])
    }
    #[inline(always)]
    pub fn as_i16(self) -> i16 {
        i16::from_le_bytes([self.0, self.1])
    }
    #[inline(always)]
    pub fn as_i8(self) -> i8 {
        self.0 as i8
    }
    #[inline(always)]
    pub fn as_u128(self) -> u128 {
        u128::from_le_bytes([self.0, self.1, self.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])
    }
    #[inline(always)]
    pub fn as_u64(self) -> u64 {
        u64::from_le_bytes([self.0, self.1, self.2, 0, 0, 0, 0, 0])
    }
    #[inline(always)]
    pub fn as_u32(self) -> u32 {
        u32::from_le_bytes([self.0, self.1, self.2, 0])
    }
    #[inline(always)]
    pub fn as_u16(self) -> u16 {
        u16::from_le_bytes([self.0, self.1])
    }
    #[inline(always)]
    pub fn as_u8(self) -> u8 {
        self.0
    }
    #[inline(always)]
    pub fn as_f32(self) -> f32 {
        self.as_u32() as f32
    }
    #[inline(always)]
    pub fn as_f64(self) -> f64 {
        self.as_u32() as f64
    }
    #[inline(always)]
    pub fn clamp(self, min: Self, max: Self) -> Self {
        assert!(min <= max);
        if self < min {
            min
        } else if self > max {
            max
        } else {
            self
        }
    }
}

impl PartialOrd for u24 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.as_u32().cmp(&other.as_u32()))
    }
}

impl Ord for u24 {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_u32().cmp(&other.as_u32())
    }
}

impl From<i8> for u24 {
    #[inline(always)]
	fn from(v: i8) -> Self {
		Self::from(v as i32)
	}
}
impl From<i16> for u24 {
    #[inline(always)]
	fn from(v: i16) -> Self {
		Self::from(v as i32)
	}
}
impl From<i24> for u24 {
    #[inline(always)]
    fn from(v: i24) -> Self {
        Self::from(v.as_i32())
    }
}
impl From<i32> for u24 {
    #[inline(always)]
	fn from(v: i32) -> Self {
		let bytes = v.to_le_bytes();
		Self(bytes[0], bytes[1], bytes[2])
	}
}
impl From<i64> for u24 {
    #[inline(always)]
    fn from(v: i64) -> Self {
        Self::from(v as i32)
    }
}
impl From<i128> for u24 {
    #[inline(always)]
    fn from(v: i128) -> Self {
        Self::from(v as i32)
    }
}
impl From<u8> for u24 {
    #[inline(always)]
	fn from(v: u8) -> Self {
		Self::from(v as u32)
	}
}
impl From<u16> for u24 {
    #[inline(always)]
	fn from(v: u16) -> Self {
		Self::from(v as u32)
	}
}
impl From<u32> for u24 {
    #[inline(always)]
	fn from(v: u32) -> Self {
		let bytes = v.to_le_bytes();
		Self(bytes[0], bytes[1], bytes[2])
	}
}
impl From<u64> for u24 {
    #[inline(always)]
    fn from(v: u64) -> Self {
        Self::from(v as u32)
    }
}
impl From<u128> for u24 {
    #[inline(always)]
    fn from(v: u128) -> Self {
        Self::from(v as u32)
    }
}
impl From<f32> for u24 {
    #[inline(always)]
    fn from(v: f32) -> Self {
        Self::from(v as u32)
    }
}
impl From<f64> for u24 {
    #[inline(always)]
    fn from(v: f64) -> Self {
        Self::from(v as u32)
    }
}
impl Into<i8> for u24 {
    #[inline(always)]
    fn into(self) -> i8 {
        self.as_i8()
    }
}
impl Into<i16> for u24 {
    #[inline(always)]
    fn into(self) -> i16 {
        self.as_i16()
    }
}
impl Into<i32> for u24 {
    #[inline(always)]
    fn into(self) -> i32 {
        self.as_i32()
    }
}
impl Into<i64> for u24 {
    #[inline(always)]
    fn into(self) -> i64 {
        self.as_i64()
    }
}
impl Into<i128> for u24 {
    #[inline(always)]
    fn into(self) -> i128 {
        self.as_i128()
    }
}
impl Into<u8> for u24 {
    #[inline(always)]
    fn into(self) -> u8 {
        self.as_u8()
    }
}
impl Into<u16> for u24 {
    #[inline(always)]
    fn into(self) -> u16 {
        self.as_u16()
    }
}
impl Into<u32> for u24 {
    #[inline(always)]
    fn into(self) -> u32 {
        self.as_u32()
    }
}
impl Into<u64> for u24 {
    #[inline(always)]
    fn into(self) -> u64 {
        self.as_u64()
    }
}
impl Into<u128> for u24 {
    #[inline(always)]
    fn into(self) -> u128 {
        self.as_u128()
    }
}
impl Into<f32> for u24 {
    #[inline(always)]
    fn into(self) -> f32 {
        self.as_f32()
    }
}
impl Into<f64> for u24 {
    #[inline(always)]
    fn into(self) -> f64 {
        self.as_f64()
    }
}

impl Add for u24 {
    type Output = Self;
    #[inline(always)]
    fn add(self, rhs: Self) -> Self::Output {
        Self::from(self.as_u32() + rhs.as_u32())
    }
}
impl Sub for u24 {
    type Output = Self;
    #[inline(always)]
    fn sub(self, rhs: Self) -> Self::Output {
        Self::from(self.as_u32() - rhs.as_u32())
    }
}
impl Mul for u24 {
    type Output = Self;
    #[inline(always)]
    fn mul(self, rhs: Self) -> Self::Output {
        Self::from(self.as_u32() * rhs.as_u32())
    }
}
impl Div for u24 {
    type Output = Self;
    #[inline(always)]
    fn div(self, rhs: Self) -> Self::Output {
        Self::from(self.as_u32() / rhs.as_u32())
    }
}
impl AddAssign for u24 {
    #[inline(always)]
    fn add_assign(&mut self, rhs: Self) {
        *self = self.add(rhs);
    }
}
impl SubAssign for u24 {
    #[inline(always)]
    fn sub_assign(&mut self, rhs: Self) {
        *self = self.sub(rhs);
    }
}
impl MulAssign for u24 {
    #[inline(always)]
    fn mul_assign(&mut self, rhs: Self) {
        *self = self.mul(rhs);
    }
}
impl DivAssign for u24 {
    #[inline(always)]
    fn div_assign(&mut self, rhs: Self) {
        *self = self.div(rhs);
    }
}
impl BitAnd for u24 {
    type Output = Self;
    #[inline(always)]
    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0, self.1 & rhs.1, self.2 & rhs.2)
    }
}
impl BitOr for u24 {
    type Output = Self;
    #[inline(always)]
    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0, self.1 | rhs.1, self.2 | rhs.2)
    }
}
impl BitXor for u24 {
    type Output = Self;
    #[inline(always)]
    fn bitxor(self, rhs: Self) -> Self::Output {
        Self(self.0 ^ rhs.0, self.1 ^ rhs.1, self.2 ^ rhs.2)
    }
}
impl Shl for u24 {
    type Output = Self;
    #[inline(always)]
    fn shl(self, rhs: Self) -> Self::Output {
        Self::from(self.as_u32() << rhs.as_u32())
    }
}
impl Shr for u24 {
    type Output = Self;
    #[inline(always)]
    fn shr(self, rhs: Self) -> Self::Output {
        Self::from(self.as_u32() >> rhs.as_u32())
    }
}
impl BitAndAssign for u24 {
    #[inline(always)]
    fn bitand_assign(&mut self, rhs: Self) {
        *self = self.bitand(rhs);
    }
}
impl BitOrAssign for u24 {
    #[inline(always)]
    fn bitor_assign(&mut self, rhs: Self) {
        *self = self.bitor(rhs);
    }
}
impl BitXorAssign for u24 {
    #[inline(always)]
    fn bitxor_assign(&mut self, rhs: Self) {
        *self = self.bitxor(rhs);
    }
}
impl ShlAssign for u24 {
    #[inline(always)]
    fn shl_assign(&mut self, rhs: Self) {
        *self = self.shl(rhs);
    }
}
impl ShrAssign for u24 {
    #[inline(always)]
    fn shr_assign(&mut self, rhs: Self) {
        *self = self.shr(rhs);
    }
}
impl Rem for u24 {
    type Output = Self;
    #[inline(always)]
    fn rem(self, rhs: Self) -> Self::Output {
        Self::from(self.as_u32() % rhs.as_u32())
    }
}
impl RemAssign for u24 {
    #[inline(always)]
    fn rem_assign(&mut self, rhs: Self) {
        *self = self.rem(rhs);
    }
}
impl Not for u24 {
    type Output = Self;
    #[inline(always)]
    fn not(self) -> Self::Output {
        Self(!self.0, !self.1, !self.2)
    }
}

impl Default for u24 {
    fn default() -> Self {
        Self(0, 0, 0)
    }
}

impl Display for u24 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.as_u32())
    }
}
