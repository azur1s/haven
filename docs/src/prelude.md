# `prelude`

## `strlen`

```nr
extern strlen(s: str) u64;
```

## `printf`

```nr
extern printf<T>(fmt: str, arg: T) void;
```

## `rt_slice_len`

```nr
extern rt_slice_len<T>(slice: *[T]) u64;
```

## `rt_slice_from_raw_parts`

```nr
extern rt_slice_from_raw_parts<T>(ptr: *T, len: u64) *[T];
```

## `print`

```nr
proc print(s: str) void
```

## `println`

```nr
proc println(s: str) void
```

## `printf32`

```nr
proc printf32(fmt: str, arg: f32) void
```

## `slice_len`

```nr
proc slice_len<T>(slice: *[T]) u64
```

