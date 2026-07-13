//@ error: expected a const argument for 'N', got a type
// A type where a const arg is expected is rejected.

struct Buf<T, const N: u32> { data: [T; N] }

@export
proc main() i32 {
    let b: Buf<i32, i32> = Buf::<i32, i32> { data: [1, 2] };
    return 0;
}
