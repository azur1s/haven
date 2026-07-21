//@ exit: any
@export
proc main() {
    let i: i32 = 0;
    while (true) {
        printf::<i32>("%d\n", i);

        i = i + 1;
        if (i >= 10) {
            break;
        } else {}
    }
}