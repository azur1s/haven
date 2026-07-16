import std/math { sinf, fabsf, fmodf, tau_f32 }

// --- Phasor ---------------------------------------------------------------
/// A bare ramp generator: outputs a rising phase in [0, 1) that wraps each cycle.
/// The building block every oscillator shape is derived from.

struct Phasor {
    phase: f32, // current phase, in [0, 1)
    inc: f32,   // per-sample increment = freq / sample_rate
}

proc phasor_new(freq: f32, sr: f32) Phasor {
    return Phasor { phase: 0.0, inc: freq / sr };
}

proc phasor_set_freq(p: *Phasor, freq: f32, sr: f32) {
    p.inc = freq / sr;
}

// advance and return the phase *before* the step, so a fresh Phasor starts at 0.
@alloc(false)
proc phasor_next(p: *Phasor) f32 {
    let out: f32 = p.phase;
    p.phase = p.phase + p.inc;
    if (p.phase >= 1.0) { p.phase = p.phase - 1.0; }
    return out;
}

// --- Osc ------------------------------------------------------------------
/// A phase-accumulating oscillator with the usual naive (non-bandlimited)
/// waveform shapers. All outputs are in [-1, 1].

struct Osc {
    phase: f32, // in [0, 1)
    inc: f32,   // per-sample increment = freq / sample_rate
}

proc osc_new(freq: f32, sr: f32) Osc {
    return Osc { phase: 0.0, inc: freq / sr };
}

proc osc_set_freq(o: *Osc, freq: f32, sr: f32) {
    o.inc = freq / sr;
}

proc osc_set_phase(o: *Osc, phase: f32) {
    o.phase = fmodf(phase, 1.0);
}

@alloc(false)
proc osc_advance(o: *Osc) {
    o.phase = fmodf(o.phase + o.inc, 1.0);
}

@alloc(false)
proc osc_sine(o: *Osc) f32 {
    let out: f32 = sinf(tau_f32() * o.phase);
    osc_advance(o);
    return out;
}

@alloc(false)
proc osc_saw(o: *Osc) f32 {
    // rising ramp mapped from [0,1) to [-1,1)
    let out: f32 = 2.0 * o.phase - 1.0;
    osc_advance(o);
    return out;
}

@alloc(false)
proc osc_square(o: *Osc) f32 {
    let out: f32 = 1.0;
    if (o.phase >= 0.5) { out = -1.0; }
    osc_advance(o);
    return out;
}

@alloc(false)
proc osc_tri(o: *Osc) f32 {
    // fold the ramp into a triangle: 2*|2p-1| - 1
    let out: f32 = 2.0 * fabsf(2.0 * o.phase - 1.0) - 1.0;
    osc_advance(o);
    return out;
}