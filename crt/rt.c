#include <stdio.h>
#include <stdlib.h>

struct Slice {
    void* data;
    int length;
};

// `str` is a { ptr, i32 } fat pointer in the language
// no null terminator is assumed, so the explicit length is used
struct Str {
    const char* ptr;
    int length;
};

void print(struct Str s) { fwrite(s.ptr, 1, (size_t)s.length, stdout); }
void println(struct Str s) {
    fwrite(s.ptr, 1, (size_t)s.length, stdout);
    putchar('\n');
}

void print_i32(int i) { printf("%d\n", i); }
void print_float(float f) { printf("%f\n", f); }
void print_double(double d) { printf("%f\n", d); }
void print_float_slice(struct Slice* slice) {
    printf("Slice length: %d\n", slice->length);
    float* data = (float*)slice->data;
    for (int i = 0; i < slice->length; i++) {
        printf("Element %d: %f\n", i, data[i]);
    }
}