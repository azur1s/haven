# `dsp/filters`

std/dsp/filters: audio filters.

Three drop-in filter types, from cheapest to most capable: a one-pole
lowpass, a general-purpose biquad with the standard RBJ EQ responses
(lowpass, highpass, bandpass, notch, peaking, shelves), and a
state-variable filter that produces lowpass/highpass/bandpass/notch at once.

The usage pattern is the same for all three: build one with a constructor
(`cutoff`/`q`/`sr` in hertz), then call its `*_process` function once per
input sample. State lives in the filter struct, so keep it between samples.
All single precision.

## `OnePole`

```hv
struct OnePole {
    a0: f32,
    b1: f32,
    z1: f32,
}
```

## `onepole_lowpass`

```hv
proc onepole_lowpass(cutoff: f32, sr: f32) OnePole
```

A one-pole lowpass with corner frequency `cutoff` hertz at sample rate `sr`.

## `onepole_set_cutoff`

```hv
proc onepole_set_cutoff(f: *OnePole, cutoff: f32, sr: f32) void
```

Retune an existing one-pole to corner frequency `cutoff` at sample rate `sr`.

## `onepole_process`

```hv
@alloc(false)
proc onepole_process(f: *OnePole, x: f32) f32
```

Filter one input sample `x`, returning the filtered output.

## `Biquad`

```hv
struct Biquad {
    b0: f32,
    b1: f32,
    b2: f32,
    a1: f32,
    a2: f32,
    s1: f32,
    s2: f32,
}
```

## `biquad_from`

```hv
proc biquad_from(b0: f32, b1: f32, b2: f32, a0: f32, a1: f32, a2: f32) Biquad
```

Build a biquad from raw coefficients `b0..a2`, normalizing by `a0` and
starting from zero state. The response constructors below are built on this.

## `biquad_lowpass`

```hv
proc biquad_lowpass(cutoff: f32, q: f32, sr: f32) Biquad
```

Lowpass at `cutoff` hertz with resonance `q`, for sample rate `sr`.

## `biquad_highpass`

```hv
proc biquad_highpass(cutoff: f32, q: f32, sr: f32) Biquad
```

Highpass at `cutoff` hertz with resonance `q`, for sample rate `sr`.

## `biquad_bandpass`

```hv
proc biquad_bandpass(cutoff: f32, q: f32, sr: f32) Biquad
```

Bandpass centered at `cutoff` hertz with `q` setting the width, normalized to
0 dB peak gain, for sample rate `sr`.

## `biquad_notch`

```hv
proc biquad_notch(cutoff: f32, q: f32, sr: f32) Biquad
```

Notch (band-reject) at `cutoff` hertz with `q` setting the width, for sample
rate `sr`.

## `biquad_peak`

```hv
proc biquad_peak(cutoff: f32, q: f32, gain_db: f32, sr: f32) Biquad
```

Peaking EQ: `gain_db` boost (or cut) at `cutoff` hertz, bandwidth set by `q`,
for sample rate `sr`.

## `biquad_lowshelf`

```hv
proc biquad_lowshelf(cutoff: f32, q: f32, gain_db: f32, sr: f32) Biquad
```

Low shelf: `gain_db` boost (or cut) below `cutoff` hertz, slope set by `q`,
for sample rate `sr`.

## `biquad_highshelf`

```hv
proc biquad_highshelf(cutoff: f32, q: f32, gain_db: f32, sr: f32) Biquad
```

High shelf: `gain_db` boost (or cut) above `cutoff` hertz, slope set by `q`,
for sample rate `sr`.

## `biquad_process`

```hv
@alloc(false)
proc biquad_process(f: *Biquad, x: f32) f32
```

Filter one input sample `x`, returning the filtered output.

## `SvfOut`

```hv
struct SvfOut {
    lp: f32,  /// lowpass output
    hp: f32,  /// highpass output
    bp: f32,  /// bandpass output
    notch: f32,  /// notch (lowpass + highpass) output
}
```

The four simultaneous outputs of a state-variable filter for one sample.

## `StateVariable`

```hv
struct StateVariable {
    g: f32,
    k: f32,
    a1: f32,
    a2: f32,
    a3: f32,
    ic1eq: f32,
    ic2eq: f32,
}
```

## `svf_new`

```hv
proc svf_new(cutoff: f32, q: f32, sr: f32) StateVariable
```

A state-variable filter at `cutoff` hertz with resonance `q`, for sample
rate `sr`.

## `svf_set`

```hv
proc svf_set(f: *StateVariable, cutoff: f32, q: f32, sr: f32) void
```

Retune an existing state-variable filter to `cutoff` hertz and resonance `q`,
for sample rate `sr`, keeping its running state.

## `svf_process`

```hv
@alloc(false)
proc svf_process(f: *StateVariable, x: f32) SvfOut
```

Filter one input sample `x`, returning all four responses as an `SvfOut`.

