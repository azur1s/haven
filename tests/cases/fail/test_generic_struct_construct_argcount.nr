//@ error: struct 'Option' expects 1 type argument, got 2
// The turbofish arg count on a construction must match the struct's declared
// arity.

struct Option<T> { present: bool, value: T }

@export
proc main() i32 {
    let o: bool = Option::<i32, f32> { present: true, value: 5 }.present;
    return 0;
}
