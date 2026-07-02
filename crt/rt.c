#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

struct Slice {
    void* data;
    int length;
};

// Heap allocation, exposed to the language via `std/alloc` (see crt/std/alloc.ixc).
static void rt_oom(void) {
    fputs("out of memory\n", stderr);
    abort();
}
void* rt_alloc(uint64_t size) {
    void* p = malloc((size_t)size);
    if (!p && size != 0) rt_oom();
    return p;
}
void* rt_realloc(void* ptr, uint64_t size) {
    void* p = realloc(ptr, (size_t)size);
    if (!p && size != 0) rt_oom();
    return p;
}
void rt_free(void* ptr) { free(ptr); }

// `str` is a { ptr, i32 } fat pointer in the language. The compiler passes it
// by value as that two-field aggregate, which LLVM lowers to two registers (ptr,
// len on both the SysV and Windows x64 ABIs).
// A C `struct` parameter would NOT match on Windows x64 (a 16-byte struct is
// passed *by reference* there), so we take the two fields as separate scalar
// parameters to match the call ABI.
void print(const char* ptr, int length) { fwrite(ptr, 1, (size_t)length, stdout); }
void println(const char* ptr, int length) {
    fwrite(ptr, 1, (size_t)length, stdout);
    putchar('\n');
}

void print_i32(int i) { printf("%d\n", i); }
void print_float(float f) { printf("%f\n", f); }
void print_double(double d) { printf("%f\n", d); }