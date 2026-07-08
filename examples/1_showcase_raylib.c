// Tested with:
// (Linux)
// $ ixc examples/1_showcase.ixc --shared -o 1_showcase
// $ clang examples/1_showcase_raylib.c ./1_showcase.so -lraylib -lm -o showcase
// $ ./showcase
// (Windows)
// TODO

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "raylib.h"

extern void gen_square_wave(float* buf_l, float* buf_r, int num_samples);
extern void process_stereo(float* buf_l, float* buf_r, int num_samples);

#define WIDTH   400
#define HEIGHT  200

#define PI      3.14159265358979323846f

// value in [-1,1] -> y pixel around the center line
static void draw_wave(const float* buf, int n, float y_center, float y_scale, Color color) {
  if (n < 2) return;
  for (int i = 0; i < n - 1; i++) {
    float x0 = (float)i       / (float)(n - 1) * WIDTH;
    float x1 = (float)(i + 1) / (float)(n - 1) * WIDTH;
    float y0 = y_center - buf[i]     * y_scale;
    float y1 = y_center - buf[i + 1] * y_scale;
    DrawLineEx((Vector2){x0, y0}, (Vector2){x1, y1}, 2.0f, color);
  }
}

int main() {
  InitWindow(WIDTH, HEIGHT, "showcase");
  SetTargetFPS(60);

  int num_samples = 1024;

  float* orig_l = (float*)malloc(num_samples * sizeof(float)); // fresh source each frame
  float* orig_r = (float*)malloc(num_samples * sizeof(float));
  float* work_l = (float*)malloc(num_samples * sizeof(float)); // processed copy
  float* work_r = (float*)malloc(num_samples * sizeof(float));

  gen_square_wave(orig_l, orig_r, num_samples);

  const float y_center = HEIGHT * 0.5f;
  const float y_scale  = HEIGHT * 0.35f;
  float phase = 0.0f;

  while (!WindowShouldClose()) {
    memcpy(work_l, orig_l, num_samples * sizeof(float));
    memcpy(work_r, orig_r, num_samples * sizeof(float));

    process_stereo(work_l, work_r, num_samples);

    BeginDrawing();
    ClearBackground(RAYWHITE);
    DrawLine(0, (int)y_center, WIDTH, (int)y_center, Fade(LIGHTGRAY, 0.5f));

    // one lane, everything overlaid. originals first (gray), modified on top (blue)
    draw_wave(orig_l, num_samples, y_center, y_scale, Fade(GRAY, 0.5f));
    draw_wave(orig_r, num_samples, y_center, y_scale, Fade(GRAY, 0.5f));
    draw_wave(work_l, num_samples, y_center, y_scale, Fade(BLUE, 0.6f));
    draw_wave(work_r, num_samples, y_center, y_scale, Fade(BLUE, 0.6f));

    EndDrawing();
  }

  CloseWindow();
  free(orig_l); free(orig_r); free(work_l); free(work_r);
  return 0;
}
