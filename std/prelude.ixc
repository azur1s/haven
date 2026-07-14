// crt/rt.c (always linked in)

extern strlen(s: str) u64;
// fixed to 2 args for now
// TODO macros? like Rust's format!
extern printf<T>(fmt: str, arg: T) void;

extern rt_slice_len<T>(slice: *[T]) u64;
extern rt_slice_from_raw_parts<T>(ptr: *T, len: u64) *[T];

proc print(s: str) void { printf::<str>("%s", s); }
proc println(s: str) void { printf::<str>("%s\n", s); }

// TODO printf32 will read the f32 as f64 and it will be misaligned and
// prints 0, so use this dedicated proc for now until we have traits or something
proc printf32(fmt: str, arg: f32) void {
    printf::<f64>(fmt, numerical_cast::<f64>(arg));
}

proc slice_len<T>(slice: *[T]) u64 { return rt_slice_len::<T>(slice); }