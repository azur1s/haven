# `dsp/filters`

## `OnePole`

```nr
struct OnePole {
    a0: f32,
    b1: f32,
    z1: f32,
}
```

## `onepole_lowpass`

```nr
proc onepole_lowpass(cutoff: f32, sr: f32) OnePole
```

## `onepole_set_cutoff`

```nr
proc onepole_set_cutoff(f: *OnePole, cutoff: f32, sr: f32) void
```

## `onepole_process`

```nr
@alloc(false)
proc onepole_process(f: *OnePole, x: f32) f32
```

## `Biquad`

```nr
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

```nr
proc biquad_from(b0: f32, b1: f32, b2: f32, a0: f32, a1: f32, a2: f32) Biquad
```

Normalize raw (b0..a2) by a0 and return a zero-state section.

## `biquad_lowpass`

```nr
proc biquad_lowpass(cutoff: f32, q: f32, sr: f32) Biquad
```

## `biquad_highpass`

```nr
proc biquad_highpass(cutoff: f32, q: f32, sr: f32) Biquad
```

## `biquad_bandpass`

```nr
proc biquad_bandpass(cutoff: f32, q: f32, sr: f32) Biquad
```

Constant 0 dB peak-gain bandpass.

## `biquad_notch`

```nr
proc biquad_notch(cutoff: f32, q: f32, sr: f32) Biquad
```

## `biquad_peak`

```nr
proc biquad_peak(cutoff: f32, q: f32, gain_db: f32, sr: f32) Biquad
```

Peaking EQ: `gain_db` boost/cut at `cutoff`, bandwidth set by `q`.

## `biquad_lowshelf`

```nr
proc biquad_lowshelf(cutoff: f32, q: f32, gain_db: f32, sr: f32) Biquad
```

## `biquad_highshelf`

```nr
proc biquad_highshelf(cutoff: f32, q: f32, gain_db: f32, sr: f32) Biquad
```

## `biquad_process`

```nr
@alloc(false)
proc biquad_process(f: *Biquad, x: f32) f32
```

## `SvfOut`

```nr
struct SvfOut {
    lp: f32,
    hp: f32,
    bp: f32,
    notch: f32,
}
```

## `StateVariable`

```nr
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

```nr
proc svf_new(cutoff: f32, q: f32, sr: f32) StateVariable
```

## `svf_set`

```nr
proc svf_set(f: *StateVariable, cutoff: f32, q: f32, sr: f32) void
```

## `svf_process`

```nr
@alloc(false)
proc svf_process(f: *StateVariable, x: f32) SvfOut
```

