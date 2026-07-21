# `alloc`

std/alloc: typed heap allocation.

`alloc`, `realloc` and `dealloc` manage raw heap buffers of `count` values of
a type `T`, sized automatically from `sizeof::<T>()`. Allocation can fail, so
each returns an `Option<*T>` that is `none` on failure, unwrap it (or use
`unwrap_or`) before dereferencing. Memory is not freed for you; pass every
buffer you allocate to `dealloc` when done. These are the primitives higher
level containers like `Vec<T>` are built on.

## `alloc`

```nr
@alloc(true)
proc alloc<T>(count: u64) Option<*T>
```

Allocate uninitialized room for `count` values of type `T`. Returns `some`
pointer on success, or `none` if the allocation failed.

## `realloc`

```nr
@alloc(true)
proc realloc<T>(ptr: *T, count: u64) Option<*T>
```

Resize the allocation at `ptr` to hold `count` values of type `T`, preserving
existing contents. Returns `some` new pointer on success, or `none` on
failure (in which case the original allocation is left untouched).

## `dealloc`

```nr
@alloc(true)
proc dealloc<T>(ptr: *T) void
```

Release an allocation obtained from `alloc`/`realloc`. Using `ptr` afterward
is undefined.

