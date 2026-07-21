//@ error: struct 'Vec2' is not generic; no type arguments expected
// A turbofish on a non-generic struct construction is rejected.

struct Vec2 { x: f32, y: f32 }

@export
proc main() i32 {
    let v: f32 = Vec2::<i32> { x: 1.0f32, y: 2.0f32 }.x;
    return 0;
}
