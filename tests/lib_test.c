// Tested with:
// ixc tests/test_stereo_simd.ixc --shared
// clang tests/lib_test.c output.lib -o lib_test.exe
// ./lib_test.exe

#include <stdio.h>
#include <stdlib.h>

extern void process_stereo(float* buf_l, float* buf_r, int num_samples);

int main() {
  int num_samples = 16;
  float* buf_l = (float*)malloc(num_samples * sizeof(float));
  float* buf_r = (float*)malloc(num_samples * sizeof(float));

  for (int i = 0; i < num_samples; i++) {
    // from 0.0 to 1.0
    buf_l[i] = (float)i / (num_samples - 1);
    // from 1.0 to 0.0
    buf_r[i] = 1.0f - (float)i / (num_samples - 1);
  }

  process_stereo(buf_l, buf_r, num_samples);

  for (int i = 0; i < num_samples; i++) {
    printf("buf_l[%d] = %f, buf_r[%d] = %f\n", i, buf_l[i], i, buf_r[i]);
  }

  free(buf_l);
  free(buf_r);

  return 0;
}