// A struct with an array-typed field, constructed from an array literal. The
// field is inlined aggregate storage, so the literal's contents are copied into
// it (not stored as a pointer), and reading `s.data` yields the field address to
// index. (Regression: this used to emit a type-mismatched `store`.)

struct Grid {
    data: [i32; 3],
    tag: i32,
}

@export
proc main() i32 {
    let s: Grid = Grid { data: [100, 200, 300], tag: 9 };
    printf::<i32>("%d\n", s.data[0]);  // 100
    printf::<i32>("%d\n", s.data[2]);  // 300
    printf::<i32>("%d\n", s.tag);      // 9
    return 0;
}
