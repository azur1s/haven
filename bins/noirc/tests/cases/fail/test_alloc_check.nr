//@ error: marked as @alloc(false)
// printf::<i32>("%d\n",  comes from the prelude; an extern not marked @alloc(false) is
// treated as dirty (may allocate), so anything calling it becomes dirty too
proc dirty() { printf::<i32>("%d\n", 0); }

@alloc(false)
@export
proc main() {
    dirty();
    dirty2();
}

proc dirty2() { dirty(); }