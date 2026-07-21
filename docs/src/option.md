# `option`

## `Option`

```nr
struct Option<T> {
    value: T,
}
```

## `some`

```nr
proc some<T>(value: *T) Option<*T>
```

## `none`

```nr
proc none<T>() Option<*T>
```

## `is_some`

```nr
proc is_some<T>(o: Option<*T>) bool
```

## `is_none`

```nr
proc is_none<T>(o: Option<*T>) bool
```

## `unwrap`

```nr
proc unwrap<T>(o: Option<*T>) *T
```

## `unwrap_or`

```nr
proc unwrap_or<T>(o: Option<*T>, default: *T) *T
```

