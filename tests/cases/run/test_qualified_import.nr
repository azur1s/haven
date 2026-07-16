// whole-module `import` makes a module's symbols reachable *only* qualified
// under its last path segment (`math::`, `osc::`). exercises the three qualified
// spellings the import-semantics flip enables: a qualified call, a qualified
// struct *type* in a binding, and qualified struct construction.

import std/math
import std/dsp/osc

@export
proc main() i32 {
    printf::<f64>("%f\n", math::square(3.0f64)); // 9.0

    let o: osc::Osc = osc::osc_new(1.0, 4.0);
    printf32("%f\n", osc::osc_square(&o));       // phase 0 -> 1.0

    let o2: osc::Osc = osc::Osc { phase: 0.0, inc: 0.0 };
    printf32("%f\n", osc::osc_square(&o2));       // phase 0 -> 1.0

    return 0;
}
