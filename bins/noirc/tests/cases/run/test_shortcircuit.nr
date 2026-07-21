//@ exit: any
proc foo() i32 {
    printf::<i32>("%d\n", 42);
    return 42;
}

@export
proc main() {
    if (true || foo() == 42) {
        printf::<i32>("%d\n", 1);
    } else {
        printf::<i32>("%d\n", 0);
    }

    if (false && foo() == 42) {
        printf::<i32>("%d\n", 1);
    } else {
        printf::<i32>("%d\n", 0);
    }
}