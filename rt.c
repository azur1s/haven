#include <stdio.h>
#include <stdlib.h>

struct Slice {
    void* data;
    int length;
};

void print_i32(int i) { printf("%d\n", i); }
void print_float(float f) { printf("%f\n", f); }
void print_float_slice(struct Slice* slice) {
    printf("Slice length: %d\n", slice->length);
    float* data = (float*)slice->data;
    for (int i = 0; i < slice->length; i++) {
        printf("Element %d: %f\n", i, data[i]);
    }
}