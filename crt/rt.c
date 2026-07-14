#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

struct Slice {
    void* data;
    int length;
};

uint64_t rt_slice_len(struct Slice* slice) { return (uint64_t)slice->length; }

// Heap allocation, exposed to the language via `std/alloc` (see crt/std/alloc.ixc).
// On failure these return NULL rather than aborting: `std/alloc` wraps the result
// in `Option<*T>`, so out-of-memory surfaces as `none` for the caller to handle.
void* rt_alloc(uint64_t size) {
    return malloc((size_t)size);
}
void* rt_realloc(void* ptr, uint64_t size) {
    return realloc(ptr, (size_t)size);
}
void rt_free(void* ptr) { free(ptr); }