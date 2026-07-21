//@ error: type arguments (`::<...>`) are only valid on generic or intrinsic calls
// A turbofish on a defined-but-non-generic function is still rejected as a syntax
// misuse (the name resolves, so it isn't the "unknown function" path).

proc regular(x: i32) i32 { return x; }

@export
proc main() i32 {
    let y: i32 = regular::<i32>(5);
    return 0;
}
