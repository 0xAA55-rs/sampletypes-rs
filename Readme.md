# SampleTypes

A library for audio processing, specifically for sample format conversion (e.g. `i16` scale up to `i32`, `i16` to `f32`, implementation of `i24`, etc.)

## Usage

The main thing is the `SampleType` trait. Use its methods to scale the samples.
The `SampleType` is designed for you to use as a generic type for your audio processors.

Additionally, we implemented `i24` and `u24` for your convenience to handle 24-bit audio samples.

## The `SampleType` trait

* The `SampleType` trait implements all of the basic numeric types: `i8`, `i16`, `i32`, `i64`, `i128`, `u8`, `u16`, `u32`, `u64`, `u128` and the additionally designed `i24` and `u24` struct.
* The `to()` conversion functions are for scaling the samples to the specific sample format.
* The `as()` casting functions are for casting the sample types.
* Every function is small enough to apply `#[inline(always)]`, and for the best compilation result, in `Cargo.toml` for this project, in the `[profile.release]` section, `lto = "fat"` is specified for running performance.

### Trait definition
```rust
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
    fn to<T>(&self) -> T where T: SampleType;
    fn as_<T>(&self) -> T where T: SampleType;
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
```
