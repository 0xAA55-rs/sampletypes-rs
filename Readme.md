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
    fn as_i8 (self) -> i8;

    /// Cast to `i16`
    fn as_i16(self) -> i16;

    /// Cast to `i24`
    fn as_i24(self) -> i24;

    /// Cast to `i32`
    fn as_i32(self) -> i32;

    /// Cast to `i64`
    fn as_i64(self) -> i64;

    /// Cast to `u8`
    fn as_u8 (self) -> u8;

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

    /// Clamp to `i8`
    fn clamp_to_i8 (self) -> i8;

    /// Clamp to `i16`
    fn clamp_to_i16(self) -> i16;

    /// Clamp to `i24`
    fn clamp_to_i24(self) -> i24;

    /// Clamp to `i32`
    fn clamp_to_i32(self) -> i32;

    /// Clamp to `i64`
    fn clamp_to_i64(self) -> i64;

    /// Clamp to `u8`
    fn clamp_to_u8 (self) -> u8;

    /// Clamp to `u16`
    fn clamp_to_u16(self) -> u16;

    /// Clamp to `u24`
    fn clamp_to_u24(self) -> u24;

    /// Clamp to `u32`
    fn clamp_to_u32(self) -> u32;

    /// Clamp to `u64`
    fn clamp_to_u64(self) -> u64;

    /// Clamp to `f32`
    fn clamp_to_f32(self) -> f32;

    /// Clamp to `f64`
    fn clamp_to_f64(self) -> f64;

    /// Clamp to `i128`
    fn clamp_to_i128(self) -> i128;

    /// Clamp to `u128`
    fn clamp_to_u128(self) -> u128;

    /// Get the size of the sample in bytes
    fn sizeof(self) -> usize {size_of::<Self>()}

    /// Scale to a longer type, the longest type is `i128` or `u128`
    fn to_longer(self) -> Self::Longer;

    /// Scale to a shorter type, the shortest type is `i8` or `u8`
    fn to_shorter(self) -> Self::Shorter;

    /// Cast to a longer type, the longest type is `i128` or `u128`
    fn as_longer(self) -> Self::Longer;

    /// Cast to a shorter type, the shortest type is `i8` or `u8`
    fn as_shorter(self) -> Self::Shorter;

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

    /// interpolate between `self` to `target` by `s`
    fn interpolate(self, target: Self, s: f64) -> Self;

    /// Read from a reader by little-endian
    fn read_le<T>(r: &mut T) -> Result<Self, Error> where T: Read + ?Sized;

    /// Read from a reader by big-endian
    fn read_be<T>(r: &mut T) -> Result<Self, Error> where T: Read + ?Sized;

    /// Write to a writer by little-endian
    fn write_le<T>(self, w: &mut T) -> Result<(), Error> where T: Write + ?Sized;

    /// Write to a writer by big-endian
    fn write_be<T>(self, w: &mut T) -> Result<(), Error> where T: Write + ?Sized;
}
```
