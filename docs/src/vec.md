# `vec`

std/vec: a growable, heap-backed array.

Layout is a three-word triple: a raw `*T` buffer plus the number of live
elements (`len`) and the number the buffer can hold (`cap`). Growth is
amortized O(1): when `len` would exceed `cap` we double the capacity via
`realloc`, so N pushes cost O(N) total.

A freshly `vec_new`'d Vec allocates nothing. `data` is null and `cap` is 0.
Because rt_realloc(null, n) behaves like malloc(n), the first push allocates
lazily. Call `vec_free` when done, a Vec does not free itself.

## `Vec`

```nr
struct Vec<T> {
    data: *T,  /// heap buffer, or null when cap == 0
    len: u64,  /// number of initialized elements
    cap: u64,  /// number of elements the buffer can hold
}
```

## `vec_new`

```nr
proc vec_new<T>() Vec<T>
```

An empty Vec that owns no allocation yet.

## `vec_with_capacity`

```nr
proc vec_with_capacity<T>(cap: u64) Vec<T>
```

An empty Vec with room for `cap` elements already reserved.

## `vec_len`

```nr
proc vec_len<T>(v: *Vec<T>) u64
```

## `vec_cap`

```nr
proc vec_cap<T>(v: *Vec<T>) u64
```

## `vec_is_empty`

```nr
proc vec_is_empty<T>(v: *Vec<T>) bool
```

## `vec_reserve`

```nr
proc vec_reserve<T>(v: *Vec<T>, needed: u64) void
```

Ensure the buffer can hold at least `needed` elements, reallocating (and
doubling) if it currently cannot. Does nothing when there is already room.

## `vec_push`

```nr
proc vec_push<T>(v: *Vec<T>, value: T) void
```

Append `value` to the end, growing the buffer if it is full.

## `vec_get`

```nr
proc vec_get<T>(v: *Vec<T>, i: u64) T
```

Read the element at `i` by value. Out-of-bounds is undefined; guard with len.

## `vec_get_ptr`

```nr
proc vec_get_ptr<T>(v: *Vec<T>, i: u64) *T
```

A pointer to the element at `i`, for in-place mutation of the slot.

## `vec_set`

```nr
proc vec_set<T>(v: *Vec<T>, i: u64, value: T) void
```

Overwrite the element at `i`. Out-of-bounds is undefined; guard with len.

## `vec_pop`

```nr
proc vec_pop<T>(v: *Vec<T>) Option<*T>
```

Remove and return the last element. Returns `Option<*T>`: `some` pointing at
the (now logically dead but still resident) slot, or `none` when empty.
The pointer is valid until the next push or free.

## `vec_clear`

```nr
proc vec_clear<T>(v: *Vec<T>) void
```

Logically empty the Vec without touching its capacity or freeing memory.

## `vec_free`

```nr
proc vec_free<T>(v: *Vec<T>) void
```

Release the backing allocation and reset to the empty, unallocated state.

## `vec_for_each`

```nr
proc vec_for_each<T>(v: *Vec<T>, f: proc(*T) void) void
```

Iterate over the Vec, calling `f` on each element by pointer.

## `vec_map`

```nr
proc vec_map<T, U>(v: *Vec<T>, f: proc(*T) U) Vec<U>
```

Map each element of the Vec to a new Vec by calling `f` on each element by pointer.

