#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>

struct Slice {
    void* data;
    int length;
};

uint64_t rt_slice_len(struct Slice* slice) { return (uint64_t)slice->length; }

void rt_printf(const char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vprintf(fmt, args);
    va_end(args);
}

// Float printing takes a concrete `double` parameter (a fixed prototype), not
// the erased/variadic path `rt_printf` uses. The language emits a plain
// fixed-arity call, so the value travels in an XMM register the callee reads
// directly - no caller-set %al, which the language no longer emits. The inner
// `printf` is a normal C-to-C variadic call the C compiler handles correctly.
void rt_printf_f64(const char* fmt, double d) { printf(fmt, d); }

// Heap allocation, exposed to the language via `std/alloc` (see crt/std/alloc.hv).
// On failure these return NULL rather than aborting: `std/alloc` wraps the result
// in `Option<*T>`, so out-of-memory surfaces as `none` for the caller to handle.
void* rt_alloc(uint64_t size) {
    return malloc((size_t)size);
}
void* rt_realloc(void* ptr, uint64_t size) {
    return realloc(ptr, (size_t)size);
}
void rt_free(void* ptr) { free(ptr); }