# `dsp/osc`

std/dsp/osc: naive oscillators.

A `Phasor` ramp generator and an `Osc` that turns that ramp into the classic
waveform shapes (sine, saw, square, triangle). The shapers are naive, i.e.
not bandlimited, so they alias at high frequencies. Frequencies and sample
rate are in hertz; all sample outputs are single precision.

## `Phasor`

```hv
struct Phasor {
    phase: f32,
    inc: f32,
}
```

A bare ramp generator: outputs a rising phase in [0, 1) that wraps every
cycle. Every oscillator shape below is derived from it.

## `phasor_new`

```hv
proc phasor_new(freq: f32, sr: f32) Phasor
```

A `Phasor` running at `freq` hertz for sample rate `sr`, starting at phase 0.

## `phasor_set_freq`

```hv
proc phasor_set_freq(p: *Phasor, freq: f32, sr: f32) void
```

Retune the phasor to `freq` hertz at sample rate `sr`, keeping its phase.

## `phasor_next`

```hv
@alloc(false)
proc phasor_next(p: *Phasor) f32
```

Advance one sample and return the phase from *before* the step, so a fresh
phasor yields 0.0 on its first call.

## `Osc`

```hv
struct Osc {
    phase: f32,
    inc: f32,
}
```

A phase-accumulating oscillator with naive (non-bandlimited) waveform
shapers. Every shaper advances the phase by one sample and returns a value
in [-1, 1].

## `osc_new`

```hv
proc osc_new(freq: f32, sr: f32) Osc
```

An `Osc` running at `freq` hertz for sample rate `sr`, starting at phase 0.

## `osc_set_freq`

```hv
proc osc_set_freq(o: *Osc, freq: f32, sr: f32) void
```

Retune the oscillator to `freq` hertz at sample rate `sr`, keeping its phase.

## `osc_set_phase`

```hv
proc osc_set_phase(o: *Osc, phase: f32) void
```

Jump the oscillator to `phase` (wrapped into [0, 1)).

## `osc_advance`

```hv
@alloc(false)
proc osc_advance(o: *Osc) void
```

Advance the oscillator by one sample without producing output.

## `osc_sine`

```hv
@alloc(false)
proc osc_sine(o: *Osc) f32
```

Next sine sample, then advance.

## `osc_saw`

```hv
@alloc(false)
proc osc_saw(o: *Osc) f32
```

Next sawtooth sample (rising ramp), then advance.

## `osc_square`

```hv
@alloc(false)
proc osc_square(o: *Osc) f32
```

Next square sample (+1 for the first half of the cycle, -1 for the second),
then advance.

## `osc_tri`

```hv
@alloc(false)
proc osc_tri(o: *Osc) f32
```

Next triangle sample, then advance.

