proc square(x: f64) f64 { return x * x; }
proc pi() f64 { return 3.14159265358979323846f64; }
proc tau() f64 { return 6.28318530717958647692f64; }
proc pi_f32() f32 { return 3.14159265358979323846f32; }
proc tau_f32() f32 { return 6.28318530717958647692f32; }

/// Clamp `x` into the inclusive range [lo, hi].
proc clampf(x: f64, lo: f64, hi: f64) f64 {
    if (x < lo) { return lo; }
    if (x > hi) { return hi; }
    return x;
}

// libm (linked via -lm).
// The extern name is the link symbol, so these bind directly to the C standard
// math library; no wrapper runtime is needed

// f64 (C `double`) variants:
@alloc(false) extern sin(x: f64) f64;
@alloc(false) extern cos(x: f64) f64;
@alloc(false) extern tan(x: f64) f64;
@alloc(false) extern asin(x: f64) f64;
@alloc(false) extern acos(x: f64) f64;
@alloc(false) extern atan(x: f64) f64;
@alloc(false) extern atan2(y: f64, x: f64) f64;
@alloc(false) extern sinh(x: f64) f64;
@alloc(false) extern cosh(x: f64) f64;
@alloc(false) extern tanh(x: f64) f64;
@alloc(false) extern exp(x: f64) f64;
@alloc(false) extern log(x: f64) f64;
@alloc(false) extern log2(x: f64) f64;
@alloc(false) extern log10(x: f64) f64;
@alloc(false) extern pow(base: f64, exp: f64) f64;
@alloc(false) extern sqrt(x: f64) f64;
@alloc(false) extern cbrt(x: f64) f64;
@alloc(false) extern fabs(x: f64) f64;
@alloc(false) extern floor(x: f64) f64;
@alloc(false) extern ceil(x: f64) f64;
@alloc(false) extern round(x: f64) f64;
@alloc(false) extern trunc(x: f64) f64;
@alloc(false) extern fmod(x: f64, y: f64) f64;

// f32 (C `float`) variants, for single-precision DSP:
@alloc(false) extern sinf(x: f32) f32;
@alloc(false) extern cosf(x: f32) f32;
@alloc(false) extern tanf(x: f32) f32;
@alloc(false) extern atan2f(y: f32, x: f32) f32;
@alloc(false) extern tanhf(x: f32) f32;
@alloc(false) extern expf(x: f32) f32;
@alloc(false) extern logf(x: f32) f32;
@alloc(false) extern log2f(x: f32) f32;
@alloc(false) extern log10f(x: f32) f32;
@alloc(false) extern powf(base: f32, exp: f32) f32;
@alloc(false) extern sqrtf(x: f32) f32;
@alloc(false) extern fabsf(x: f32) f32;
@alloc(false) extern floorf(x: f32) f32;
@alloc(false) extern ceilf(x: f32) f32;
@alloc(false) extern fmodf(x: f32, y: f32) f32;