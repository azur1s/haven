#include <stdio.h>
#include <stdlib.h>

struct Slice {
    void* data;
    int length;
};

// `str` is a { ptr, i32 } fat pointer in the language. The compiler passes it by
// value as that two-field aggregate, which LLVM lowers to two registers (ptr,
// len) on both the SysV and Windows x64 ABIs. A C `struct` parameter would NOT
// match on Windows x64 (a 16-byte struct is passed *by reference* there), so we
// take the two fields as separate scalar parameters to match the call ABI.
void print(const char* ptr, int length) { fwrite(ptr, 1, (size_t)length, stdout); }
void println(const char* ptr, int length) {
    fwrite(ptr, 1, (size_t)length, stdout);
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