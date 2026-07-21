/// std/prelude: the automatically imported module.
///
/// The prelude is imported implicitly into every module, so these items are in
/// scope everywhere without an `import`. It is deliberately tiny.

// backing symbols from crt/rt.c (always linked in). these are raw runtime
// plumbing, kept private; call the `pub` wrappers below instead.
extern strlen(s: str) u64;

// fixed to one format arg + one value for now.
// TODO: real variadics, or a `format!`-style macro like Rust's.
/// Print `arg` to stdout using the C `printf` format string `fmt`
/// (e.g. `printf::<i32>("%d\n", 42)`). Exactly one argument is supported, and
/// `T` must match the conversion specifier in `fmt`.
pub extern printf<T>(fmt: str, arg: T) void;

extern rt_slice_len<T>(slice: *[T]) u64;
extern rt_slice_from_raw_parts<T>(ptr: *T, len: u64) *[T];

/// Write `s` to stdout with no trailing newline.
pub proc print(s: str) void { printf::<str>("%s", s); }
/// Write `s` to stdout followed by a newline.
pub proc println(s: str) void { printf::<str>("%s\n", s); }

// a bare f32 handed to the variadic `printf` is read back as an f64 and
// misaligns, printing 0. widen to f64 up front until we have trait-based
// promotion or real variadics.
/// Print a single `f32` using the `printf` format `fmt` (e.g. `"%f\n"`). Use
/// this rather than `printf` for `f32` values, which variadics can't pass
/// correctly on their own.
pub proc printf32(fmt: str, arg: f32) void {
    printf::<f64>(fmt, numerical_cast::<f64>(arg));
}

/// The number of elements in the runtime slice `slice`.
pub proc slice_len<T>(slice: *[T]) u64 { return rt_slice_len::<T>(slice); }
