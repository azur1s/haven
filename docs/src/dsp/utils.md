# `dsp/utils`

std/dsp/utils: small audio unit conversions.

Helpers for moving between the units DSP code mixes constantly: linear
amplitude vs decibels, and MIDI note numbers vs frequency in hertz. All
single precision.

## `db_to_lin`

```nr
proc db_to_lin(db: f32) f32
```

Convert a gain in decibels to a linear amplitude multiplier (0 dB -> 1.0).

## `lin_to_db`

```nr
proc lin_to_db(lin: f32) f32
```

Convert a linear amplitude multiplier to decibels (1.0 -> 0 dB).

## `midi_to_hz`

```nr
proc midi_to_hz(note: f32) f32
```

Convert a MIDI note number to frequency in hertz (A4 = note 69 = 440 Hz).

## `hz_to_midi`

```nr
proc hz_to_midi(hz: f32) f32
```

Convert a frequency in hertz to a (fractional) MIDI note number.

