/// std/dsp/osc: naive oscillators.
///
/// A `Phasor` ramp generator and an `Osc` that turns that ramp into the classic
/// waveform shapes (sine, saw, square, triangle). The shapers are naive, i.e.
/// not bandlimited, so they alias at high frequencies. Frequencies and sample
/// rate are in hertz; all sample outputs are single precision.

import std/math { sinf, fabsf, fmodf, tau_f32 }

// --- Phasor ---------------------------------------------------------------

/// A bare ramp generator: outputs a rising phase in [0, 1) that wraps every
/// cycle. Every oscillator shape below is derived from it.
pub struct Phasor {
    phase: f32, // current phase, in [0, 1)
    inc: f32,   // per-sample increment = freq / sample_rate
}

/// A `Phasor` running at `freq` hertz for sample rate `sr`, starting at phase 0.
pub proc phasor_new(freq: f32, sr: f32) Phasor {
    return Phasor { phase: 0.0, inc: freq / sr };
}

/// Retune the phasor to `freq` hertz at sample rate `sr`, keeping its phase.
pub proc phasor_set_freq(p: *Phasor, freq: f32, sr: f32) {
    p.inc = freq / sr;
}

/// Advance one sample and return the phase from *before* the step, so a fresh
/// phasor yields 0.0 on its first call.
@alloc(false)
pub proc phasor_next(p: *Phasor) f32 {
    let out: f32 = p.phase;
    p.phase = p.phase + p.inc;
    if (p.phase >= 1.0) { p.phase = p.phase - 1.0; }
    return out;
}

// --- Osc ------------------------------------------------------------------

/// A phase-accumulating oscillator with naive (non-bandlimited) waveform
/// shapers. Every shaper advances the phase by one sample and returns a value
/// in [-1, 1].
pub struct Osc {
    phase: f32, // in [0, 1)
    inc: f32,   // per-sample increment = freq / sample_rate
}

/// An `Osc` running at `freq` hertz for sample rate `sr`, starting at phase 0.
pub proc osc_new(freq: f32, sr: f32) Osc {
    return Osc { phase: 0.0, inc: freq / sr };
}

/// Retune the oscillator to `freq` hertz at sample rate `sr`, keeping its phase.
pub proc osc_set_freq(o: *Osc, freq: f32, sr: f32) {
    o.inc = freq / sr;
}

/// Jump the oscillator to `phase` (wrapped into [0, 1)).
pub proc osc_set_phase(o: *Osc, phase: f32) {
    o.phase = fmodf(phase, 1.0);
}

/// Advance the oscillator by one sample without producing output.
@alloc(false)
pub proc osc_advance(o: *Osc) {
    o.phase = fmodf(o.phase + o.inc, 1.0);
}

/// Next sine sample, then advance.
@alloc(false)
pub proc osc_sine(o: *Osc) f32 {
    let out: f32 = sinf(tau_f32() * o.phase);
    osc_advance(o);
    return out;
}

/// Next sawtooth sample (rising ramp), then advance.
@alloc(false)
pub proc osc_saw(o: *Osc) f32 {
    // rising ramp mapped from [0,1) to [-1,1)
    let out: f32 = 2.0 * o.phase - 1.0;
    osc_advance(o);
    return out;
}

/// Next square sample (+1 for the first half of the cycle, -1 for the second),
/// then advance.
@alloc(false)
pub proc osc_square(o: *Osc) f32 {
    let out: f32 = 1.0;
    if (o.phase >= 0.5) { out = -1.0; }
    osc_advance(o);
    return out;
}

/// Next triangle sample, then advance.
@alloc(false)
pub proc osc_tri(o: *Osc) f32 {
    // fold the ramp into a triangle: 2*|2p-1| - 1
    let out: f32 = 2.0 * fabsf(2.0 * o.phase - 1.0) - 1.0;
    osc_advance(o);
    return out;
}
