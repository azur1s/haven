# `prelude`

std/prelude: the automatically imported module.

The prelude is imported implicitly into every module, so these items are in
scope everywhere without an `import`. It is deliberately tiny.

## `printf`

```hv
extern printf<T>(fmt: str, arg: T) void;
```

Print `arg` to stdout using the C `printf` format string `fmt`
(e.g. `printf::<i32>("%d\n", 42)`). Exactly one argument is supported, and
`T` must match the conversion specifier in `fmt`.

## `print`

```hv
proc print(s: str) void
```

Write `s` to stdout with no trailing newline.

## `println`

```hv
proc println(s: str) void
```

Write `s` to stdout followed by a newline.

## `printf32`

```hv
proc printf32(fmt: str, arg: f32) void
```

Print a single `f32` using the `printf` format `fmt` (e.g. `"%f\n"`). Use
this rather than `printf` for `f32` values, which variadics can't pass
correctly on their own.

## `slice_len`

```hv
proc slice_len<T>(slice: *[T]) u64
```

The number of elements in the runtime slice `slice`.

