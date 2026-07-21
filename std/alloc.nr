/// std/alloc: heap allocation.
///
/// Backed by rt_alloc/rt_realloc/rt_free in crt/rt.c (always linked in). The raw
/// externs below deal in untyped `*void`; the generic wrappers hand back a typed
/// `*T` sized by `sizeof::<T>()`. Reach for these to build growable buffers
/// (e.g. a future `Vec<T>`).

import std/option { Option, none, some }

extern rt_alloc(size: u64) *void;
extern rt_realloc(ptr: *void, size: u64) *void;
extern rt_free(ptr: *void) void;

/// Allocate room for `count` values of type T, uninitialized. Returns a `*T`.
@alloc(true)
proc alloc<T>(count: u64) Option<*T> {
    let ptr: *void = rt_alloc(count * sizeof::<T>());
    if (ptr == null::<*void>()) {
        return none::<T>();
    }
    return some::<T>(ptr_cast::<*T>(ptr));
}

/// Resize a prior allocation to hold `count` values of type T.
@alloc(true)
proc realloc<T>(ptr: *T, count: u64) Option<*T> {
    let new_ptr: *void = rt_realloc(ptr_cast::<*void>(ptr), count * sizeof::<T>());
    if (new_ptr == null::<*void>()) {
        return none::<T>();
    }
    return some::<T>(ptr_cast::<*T>(new_ptr));
}

/// Free a prior allocation.
@alloc(true)
proc dealloc<T>(ptr: *T) void {
    rt_free(ptr_cast::<*void>(ptr));
}
