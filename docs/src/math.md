# `math`

std/math: scalar math constants and the C standard-library math functions.

Two layers: a handful of convenience constants/helpers written in noir, and
direct bindings to libm. Every libm function comes in an `f64` form and an
`f32` form (suffixed `f`) so single-precision DSP code never widens to double.
Angles are in radians throughout.

## `square`

```nr
proc square(x: f64) f64
```

`x` squared (`x * x`).

## `pi`

```nr
proc pi() f64
```

The constant pi as an `f64`.

## `tau`

```nr
proc tau() f64
```

The constant tau (2*pi) as an `f64`.

## `pi_f32`

```nr
proc pi_f32() f32
```

The constant pi as an `f32`.

## `tau_f32`

```nr
proc tau_f32() f32
```

The constant tau (2*pi) as an `f32`.

## `clampf`

```nr
proc clampf(x: f64, lo: f64, hi: f64) f64
```

Clamp `x` into the inclusive range [lo, hi].

## `sin`

```nr
@alloc(false)
extern sin(x: f64) f64;
```

Sine of `x` (radians).

## `cos`

```nr
@alloc(false)
extern cos(x: f64) f64;
```

Cosine of `x` (radians).

## `tan`

```nr
@alloc(false)
extern tan(x: f64) f64;
```

Tangent of `x` (radians).

## `asin`

```nr
@alloc(false)
extern asin(x: f64) f64;
```

Arcsine of `x`, in radians. `x` must be in [-1, 1].

## `acos`

```nr
@alloc(false)
extern acos(x: f64) f64;
```

Arccosine of `x`, in radians. `x` must be in [-1, 1].

## `atan`

```nr
@alloc(false)
extern atan(x: f64) f64;
```

Arctangent of `x`, in radians.

## `atan2`

```nr
@alloc(false)
extern atan2(y: f64, x: f64) f64;
```

Arctangent of `y/x` in radians, using the signs of both to pick the quadrant.

## `sinh`

```nr
@alloc(false)
extern sinh(x: f64) f64;
```

Hyperbolic sine of `x`.

## `cosh`

```nr
@alloc(false)
extern cosh(x: f64) f64;
```

Hyperbolic cosine of `x`.

## `tanh`

```nr
@alloc(false)
extern tanh(x: f64) f64;
```

Hyperbolic tangent of `x`.

## `exp`

```nr
@alloc(false)
extern exp(x: f64) f64;
```

e raised to the power `x`.

## `log`

```nr
@alloc(false)
extern log(x: f64) f64;
```

Natural (base-e) logarithm of `x`. `x` must be positive.

## `log2`

```nr
@alloc(false)
extern log2(x: f64) f64;
```

Base-2 logarithm of `x`. `x` must be positive.

## `log10`

```nr
@alloc(false)
extern log10(x: f64) f64;
```

Base-10 logarithm of `x`. `x` must be positive.

## `pow`

```nr
@alloc(false)
extern pow(base: f64, exp: f64) f64;
```

`base` raised to the power `exp`.

## `sqrt`

```nr
@alloc(false)
extern sqrt(x: f64) f64;
```

Square root of `x`. `x` must be non-negative.

## `cbrt`

```nr
@alloc(false)
extern cbrt(x: f64) f64;
```

Cube root of `x`.

## `fabs`

```nr
@alloc(false)
extern fabs(x: f64) f64;
```

Absolute value of `x`.

## `floor`

```nr
@alloc(false)
extern floor(x: f64) f64;
```

Largest integer value not greater than `x`, as an `f64`.

## `ceil`

```nr
@alloc(false)
extern ceil(x: f64) f64;
```

Smallest integer value not less than `x`, as an `f64`.

## `round`

```nr
@alloc(false)
extern round(x: f64) f64;
```

`x` rounded to the nearest integer, halves away from zero, as an `f64`.

## `trunc`

```nr
@alloc(false)
extern trunc(x: f64) f64;
```

`x` truncated toward zero, as an `f64`.

## `fmod`

```nr
@alloc(false)
extern fmod(x: f64, y: f64) f64;
```

Floating-point remainder of `x / y`, with the sign of `x`.

## `sinf`

```nr
@alloc(false)
extern sinf(x: f32) f32;
```

Sine of `x` (radians), single precision.

## `cosf`

```nr
@alloc(false)
extern cosf(x: f32) f32;
```

Cosine of `x` (radians), single precision.

## `tanf`

```nr
@alloc(false)
extern tanf(x: f32) f32;
```

Tangent of `x` (radians), single precision.

## `atan2f`

```nr
@alloc(false)
extern atan2f(y: f32, x: f32) f32;
```

Arctangent of `y/x` (radians), quadrant-aware, single precision.

## `tanhf`

```nr
@alloc(false)
extern tanhf(x: f32) f32;
```

Hyperbolic tangent of `x`, single precision.

## `expf`

```nr
@alloc(false)
extern expf(x: f32) f32;
```

e raised to the power `x`, single precision.

## `logf`

```nr
@alloc(false)
extern logf(x: f32) f32;
```

Natural (base-e) logarithm of `x`, single precision. `x` must be positive.

## `log2f`

```nr
@alloc(false)
extern log2f(x: f32) f32;
```

Base-2 logarithm of `x`, single precision. `x` must be positive.

## `log10f`

```nr
@alloc(false)
extern log10f(x: f32) f32;
```

Base-10 logarithm of `x`, single precision. `x` must be positive.

## `powf`

```nr
@alloc(false)
extern powf(base: f32, exp: f32) f32;
```

`base` raised to the power `exp`, single precision.

## `sqrtf`

```nr
@alloc(false)
extern sqrtf(x: f32) f32;
```

Square root of `x`, single precision. `x` must be non-negative.

## `fabsf`

```nr
@alloc(false)
extern fabsf(x: f32) f32;
```

Absolute value of `x`, single precision.

## `floorf`

```nr
@alloc(false)
extern floorf(x: f32) f32;
```

Largest integer value not greater than `x`, single precision.

## `ceilf`

```nr
@alloc(false)
extern ceilf(x: f32) f32;
```

Smallest integer value not less than `x`, single precision.

## `fmodf`

```nr
@alloc(false)
extern fmodf(x: f32, y: f32) f32;
```

Floating-point remainder of `x / y` with the sign of `x`, single precision.

