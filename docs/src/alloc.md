# `alloc`

std/alloc: heap allocation.

Backed by rt_alloc/rt_realloc/rt_free in crt/rt.c (always linked in). The raw
externs below deal in untyped `*void`; the generic wrappers hand back a typed
`*T` sized by `sizeof::<T>()`. Reach for these to build growable buffers
(e.g. a future `Vec<T>`).

## `rt_alloc`

```nr
extern rt_alloc(size: u64) *void;
```

## `rt_realloc`

```nr
extern rt_realloc(ptr: *void, size: u64) *void;
```

## `rt_free`

```nr
extern rt_free(ptr: *void) void;
```

## `alloc`

```nr
@alloc(true)
proc alloc<T>(count: u64) Option<*T>
```

Allocate room for `count` values of type T, uninitialized. Returns a `*T`.

## `realloc`

```nr
@alloc(true)
proc realloc<T>(ptr: *T, count: u64) Option<*T>
```

Resize a prior allocation to hold `count` values of type T.

## `dealloc`

```nr
@alloc(true)
proc dealloc<T>(ptr: *T) void
```

Free a prior allocation.

