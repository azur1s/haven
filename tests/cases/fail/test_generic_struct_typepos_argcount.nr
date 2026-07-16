//@ error: struct 'Option' expects 1 type argument, got 2
// A generic struct used in type position must apply the right number of type
// arguments - checked before the (stage 3) instantiation rejection.

struct Option<T> { present: bool, value: T }

proc take(o: Option<i32, f32>) {}

@export
proc main() i32 { return 0; }
