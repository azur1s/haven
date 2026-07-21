// A non-void function that can fall off the end without returning must be a
// clean typecheck error, not a compiler panic. A bare `if` (no `else`) can skip
// its body, so this function doesn't return on all paths.
//@ error: not all paths return a value

proc maybe(cond: bool) i32 {
    if (cond) {
        return 1;
    }
}

@export
proc main() i32 {
    return maybe(true);
}
