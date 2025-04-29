# SampleTypes

A library for audio processing, specifically for sample format conversion (e.g. `i16` scale up to `i32`, `i16` to `f32`, implementation of `i24`, etc.)

## Usage

The main thing is the `SampleType` trait. Use its methods to scale the samples.
The `SampleType` is designed for you to use as a generic type for your audio processors.

Additionally, we implemented `i24` and `u24` for your convenience to handle 24-bit audio samples.
