# `dsp/osc`

## `Phasor`

```nr
struct Phasor {
    phase: f32,
    inc: f32,
}
```

## `phasor_new`

```nr
proc phasor_new(freq: f32, sr: f32) Phasor
```

## `phasor_set_freq`

```nr
proc phasor_set_freq(p: *Phasor, freq: f32, sr: f32) void
```

## `phasor_next`

```nr
@alloc(false)
proc phasor_next(p: *Phasor) f32
```

## `Osc`

```nr
struct Osc {
    phase: f32,
    inc: f32,
}
```

## `osc_new`

```nr
proc osc_new(freq: f32, sr: f32) Osc
```

## `osc_set_freq`

```nr
proc osc_set_freq(o: *Osc, freq: f32, sr: f32) void
```

## `osc_set_phase`

```nr
proc osc_set_phase(o: *Osc, phase: f32) void
```

## `osc_advance`

```nr
@alloc(false)
proc osc_advance(o: *Osc) void
```

## `osc_sine`

```nr
@alloc(false)
proc osc_sine(o: *Osc) f32
```

## `osc_saw`

```nr
@alloc(false)
proc osc_saw(o: *Osc) f32
```

## `osc_square`

```nr
@alloc(false)
proc osc_square(o: *Osc) f32
```

## `osc_tri`

```nr
@alloc(false)
proc osc_tri(o: *Osc) f32
```

