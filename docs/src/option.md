# `option`

std/option: an optional pointer type.

`Option<*T>` holds either a valid `*T` (`some`) or nothing (`none`).
Emptiness is represented by a null pointer, so this is the standard way for a
fallible operation (an allocation, a `vec_pop`, a lookup, etc.), to signal "no
value" without a sentinel of its own. Test with `is_some`/`is_none`, then
retrieve with `unwrap` or `unwrap_or`.

## `Option`

```hv
struct Option<T> {
    value: T,
}
```

## `some`

```hv
proc some<T>(value: *T) Option<*T>
```

An `Option` holding the pointer `value`.

## `none`

```hv
proc none<T>() Option<*T>
```

The empty `Option`.

## `is_some`

```hv
proc is_some<T>(o: Option<*T>) bool
```

Whether the `Option` holds a value.

## `is_none`

```hv
proc is_none<T>(o: Option<*T>) bool
```

Whether the `Option` is empty.

## `unwrap`

```hv
proc unwrap<T>(o: Option<*T>) *T
```

The contained pointer. On `none` this prints an error and returns the null
pointer rather than aborting, so guard with `is_some` when `none` is possible.

## `unwrap_or`

```hv
proc unwrap_or<T>(o: Option<*T>, default: *T) *T
```

The contained pointer, or `default` when the `Option` is empty.

