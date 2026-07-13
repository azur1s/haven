//@ error: expected a type argument for 'T', got a const value
// A const value where a type arg is expected is rejected.

struct Buf<T, const N: u32> { data: [T; N] }

@export
proc main() i32 {
    let b: Buf<8, 4> = Buf::<8, 4> { data: [1, 2, 3, 4] };
    return 0;
}
