// Const array literals: a module-level constant array, indexed at runtime.

const NUMS: [i32; 3] = [10, 20, 30];

@export
proc main() i32 {
    printf::<i32>("%d\n", NUMS[0]);
    printf::<i32>("%d\n", NUMS[1]);
    printf::<i32>("%d\n", NUMS[2]);
    return 0;
}
