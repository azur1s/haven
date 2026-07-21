// a selective `import { sym }` of a non-`pub` item must be rejected. `rt_alloc`
// is a runtime FFI hook inside std/alloc, deliberately kept private (the public
// surface is `alloc`/`realloc`/`dealloc`). importing it names a real symbol that
// just isn't exported, so the diagnostic says "is private", not "no such symbol".

import std/alloc { rt_alloc }

@export
proc main() i32 {
    return 0;
}

//@ error: is private
