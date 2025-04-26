#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include <time.h>

#define WIDTH 400 // 2000
#define HEIGHT 300 // 1100

double exp_exp(double x) {
  return (x > 85.0) ? 0.0 : exp(-exp(x));
}

double exp_exp_plus_exp(double x, double y) {
  return (x > 85.0 || y > 85.0) ? 0.0 : exp(- (exp(x) + exp(y)));
}

double C(double x, double y) {
  double num1 = 100 * (y + (x / 4.0) - (1.0 / 25.0));
  double denom1 = 1.0 + fabs(100.0 * x - 25.0 * y - 3.0 * atan(100.0 * x - 25.0 * y));
  double term1 = 14 * atan(num1 / denom1);
  double term2 = 14 * fabs((x / 2.0) - (y / 8.0));
  return pow(sin(term1+term2), 4);
}

double K(int v, double x, double y) {
  double sum1 = 0.0;
  for (int s = 1; s <= 60; s++) {
    double termA = sin(2.0 * s) + (6 + sin(s * s)) * (sin(7.0 * s) * (x / 2.0) + cos(7.0 * s) * ((y - 8.0) / 2.0));
    double termB = sin(3.0 * s) + (6 + 2 * sin(s * s) + (sin(7.0 * s) * ((y - 8) / 2.0) - cos(7.0 * s) * (x / 2.0)));
    double exp0 = - 25 * (pow(sin(termA), 10)*pow(sin(termB), 10) - 0.1);
    sum1 += ((5.0 / 2.0)
             * ((2.0 / 25.0) + ((3.0 / 50.0) * cos(s + 4.0 * 4.0 * v)))
             * ((sin(5.0 * s) + sin(2.0 * s) + 3.0) / 5.0)
             * exp_exp(exp0));
  }
  return sum1;
}

double A(int v, double x, double y, double Cxy) {
  return exp(
             -exp(
                  200.0 * (y + (x / 4.0) + (v / 50.0) - 
                           (0.25 * fabs(sin((12.0 / 5.0) * 
                                            (0.7 * fabs(x - (y / 4.0)) + 
                                             (0.3 * sqrt(fabs(x - (y / 4.0)))))))))
                  ) - 
             exp(
                 -200.0 * (y + (x / 4.0) + (7.0 / 20.0) - 
                           (v * 7.0 / 50.0) + 
                           (0.2 * atan(6.0 * fabs(x - (y / 4.0)))) + 
                           (0.2 * atan(40.0 * fabs(x - (y / 4.0)))) - 
                           ((23.0 / 20.0) * 
                            (1.5 + (1.0 / 25.0) * 
                             cos(10.0 * (y + (x / 4.0) + (6.0 / 25.0))) + 
                             (0.03 * Cxy) + 
                             (0.3 * atan(30.0 * (y + (x / 4.0) - 0.25))) - 
                             fabs(x - (y / 4.0)))))
                 )
             );
}

double E(double x, double y) {
  double exp1a = -100.0 * pow((3 * y + 0.75 * x + 0.27), 4);
  double exp1b = -100.0 * pow(fabs(7 * (1.0 + 1.0 / (sqrt(fabs(100 * y + 25 * x - 6)) + 0.3) - x / 4.0)), (3 * y + 2.27));
  double exp1 = exp1a + exp1b + 10.0;
  double exp2 = 200 * fabs(y + x / 4.0 - 0.2 + 3 * (x - y / 4.0) * (x - y / 4.0)) - 32.0;
  double exp3 = 500 * fabs(y + x / 4.0 - (1.0 / 20.0) - (0.7 * sqrt(fabs(x - y / 4.0)))) - 2.5;

  return -1.0 * exp(exp1) * (1.0 - exp(exp2 + exp3));
}

double L(double x, double y) {
  double sum1 = 0.0;
  for (int s=1; s <= 25; s++) {
    double num1 = 100.0 * y + 25.0 * x - 4.0 * sin(s);
    double denom1 = 1.0 + fabs(100.0 * x - 25.0 * y - 3.0 * atan(100.0 * x - 25.0 * y));
    double term1 = (80.0 + 30.0 * sin(s*s))
      * atan(num1 / denom1);
    sum1 += pow(sin(term1
                    + fabs((x / 2.0) - (y / 8.0))
                    + (4.0 * sin(5.0 * s))),
                6);
  }
  return sum1;
}

double W(double x, double y, double Cxy) {
  double termA;
  double termB;
  double termC;
  termA = (x - (y / 4.0))
    * (x - (y / 4.0));
  termB = (y + (x / 4.0))
    * (y + (x / 4.0));
  double omega1 = (- 40 * Cxy)
    + (196.0 / 5.0)
    + ((4.0 / 5.0)
       * (sqrt(termA + termB)));
  termA = 5.0 * fabs(y
                     + (x / 4.0)
                     - (3.0 / 50.0)
                     + ((1.0/ 3.0)
                        * (x - (y / 4.0))
                        * (x - (y / 4.0))));
  termB = pow(fabs((2.0 * x) - (y / 2.0)), 3);
  double omega2 = - (40.0 * (termA
                             + termB
                             - (2.0 / 5.0)));
  double omega3 =  - 1000 * fabs(x - (y / 4.0))
    + 100.0
    - 90.0 * atan(8.0 * y
                  + 2.0 * x
                  + (8.0 / 5.0)); // to be checked
  termA = fabs(x - y / 4.0);
  termB = - 7.0 / 50.0;
  termC = 9.0 * (y + (x / 4.0) + 0.2) / 20.0;
  double omega4 = 1000 * (termA + termB + termC);
  termA = fabs(y
               + x / 4.0
               - 3.0 / 50.0
               + ((1.0 / 3.0) * (x - (y / 4.0)) * (x - (y / 4.0))));
  termB = (2.0 * x) - (y / 2.0);
  termC = pow(fabs(termB), 3);
  double omega5 = 70 * fabs((5 * termA) + termC - 2.0 / 5.0) - 1.0 / 200.0; // to be checked
  termA = fabs(x - y / 4.0);
  termB = -0.1;
  termC = 0.9 * atan(8.0 * (y + (x / 4.0) + (1 / 5.0))); // to be checked
  double omega6 = 700 * fabs(termA + termB + termC) - 21.0/20.0;
  double res = (- exp_exp_plus_exp(omega1, omega2)) * (1.0 - exp_exp_plus_exp(omega3, omega4))
    - exp_exp(omega5)
    - exp_exp(omega6)
    + 1.0;
  return res;
}

double H(int v, double x, double y) {
  double Cxy = C(x, y);
  double Exy = E(x, y);
  double A0xy = A(0, x, y, Cxy);
  double Lxy = L(x, y);
  double Kxy = K(v, x, y);
  double Wxy = W(x, y, Cxy);
  double exp2a = exp(
                     (2.0 * y)
                     + (0.5 * x)
                     + (2.0 / 5.0)
                     - (2.0 * fabs(x - (y / 4.0))));
  double exp2b = exp(
                     (8.0 * y)
                     + (2.0 * x)
                     + (2.0 / 5.0)
                     - fabs((8.0 * x) - (2.0 * y)));
  double exp2 = exp2a + exp2b;
  double termA = (cos ((2.0 * y) 
                       + (x * 0.5)
                       + (7.0 / 5.0)
                       - fabs((2.0 * x)
                              - (y * 0.5))));
  double termB = sin((20.0 * y)
                     + (5.0 * x)
                     + fabs((20.0 * x) - (5.0 * y)));
  double termC = (2.7 * y)
    + ((27.0 * x) / 40.0)
    + (81.0 / 250.0);
  double exp3 = - (50.0 *
                   ((pow(termA, 80) * pow(termB, 2))
                    - pow(termC, 10)
                    - (49.0 / 50.0)));
  double term1 = ((18 - (9.0 * v) + (v * v)) / 20.0)
    * (1.0 - A0xy)
    * (1.0 - Exy)
    * Kxy;
  double term2 = ((2 + (3.0 * v)) / 5.0)
    * A0xy
    * A(1, x, y, Cxy)
    * (1.0 - Exy)
    * ((50.0 + Lxy) / 50.0)
    * exp_exp(exp2)
    * Wxy; // to be checked
  double term3 = exp_exp(exp3);
  double term4 = 0.1
    * Exy
    * ((v - 1.0) * (v - 1)); // to be checked
  double res = term1 + term2 + term3 + term4;
  return res;
}

int F(double x) {
  double a = - (1000.0 * x);
  double b = 1000.0 * (x - 1.0);
  int res = floor(255.0
                     * exp_exp(a)
                     * pow(fabs(x), exp_exp(b)));
  return res;
}


#pragma pack(push, 1) // no padding

typedef struct {
  uint16_t bfType;
  uint32_t bfSize;
  uint16_t bfReserved1;
  uint16_t bfReserved2;
  uint32_t bfOffBits;
} BITMAPFILEHEADER;

typedef struct {
  uint32_t biSize;
  int32_t  biWidth;
  int32_t  biHeight;
  uint16_t biPlanes;
  uint16_t biBitCount;
  uint32_t biCompression;
  uint32_t biSizeImage;
  int32_t  biXPelsPerMeter;
  int32_t  biYPelsPerMeter;
  uint32_t biClrUsed;
  uint32_t biClrImportant;
} BITMAPINFOHEADER;

#pragma pack(pop)

struct RGB {
  unsigned char r, g, b;
};

void saveBMP(const char *filename, struct RGB *pixels, int width, int height) {
  FILE *f = fopen(filename, "wb");
  if (!f) {
    perror("Cannot open file");
    return;
  }

  int row_padded = (width * 3 + 3) & (~3); // each row must be multiple of 4 bytes
  int filesize = 54 + row_padded * height;

  BITMAPFILEHEADER bfh = {0};
  bfh.bfType = 0x4D42; // 'BM'
  bfh.bfSize = filesize;
  bfh.bfOffBits = 54;

  BITMAPINFOHEADER bih = {0};
  bih.biSize = 40;
  bih.biWidth = width;
  bih.biHeight = height;
  bih.biPlanes = 1;
  bih.biBitCount = 24;
  bih.biSizeImage = row_padded * height;

  fwrite(&bfh, sizeof(bfh), 1, f);
  fwrite(&bih, sizeof(bih), 1, f);

  unsigned char *row = (unsigned char *)calloc(row_padded, 1);
  for (int y = height - 1; y >= 0; y--) { // BMP is bottom-up
    for (int x = 0; x < width; x++) {
      struct RGB pixel = pixels[y * width + x];
      row[x * 3 + 0] = pixel.b; // BMP uses BGR order
      row[x * 3 + 1] = pixel.g;
      row[x * 3 + 2] = pixel.r;
    }
    fwrite(row, row_padded, 1, f);
  }
  free(row);
  fclose(f);
}


int main(int argc, [[maybe_unused]] char* argv[argc+1]) {

  printf("hello\n");

  struct timespec start, end;
  double duration;
  clock_gettime(CLOCK_MONOTONIC, &start);

  int r_array[HEIGHT][WIDTH];
  int g_array[HEIGHT][WIDTH];
  int b_array[HEIGHT][WIDTH];

  for (int n = 1; n <= HEIGHT; n++) {
    for (int m = 1; m <= WIDTH; m++) {
      double x = (m - 1000.0) / 960.0;
      double y = (451.0 - n) / 960.0;
      int r = F(H(0, x, y));
      int g = F(H(1, x, y));
      int b = F(H(2, x, y));
      r_array[n-1][m-1] = r;
      g_array[n-1][m-1] = g;
      b_array[n-1][m-1] = b;
      if ( ((m%100)==0) && ( (n==1) || ((n%100)==0) ) ) {
        printf("n = %d ; m = %d ; RGB = %d %d %d\n", n, m, r, g, b);
      }
    }
  }
  
  clock_gettime(CLOCK_MONOTONIC, &end);
  duration = (end.tv_sec - start.tv_sec) + (end.tv_nsec - start.tv_nsec) / 1e9;
  printf("Duration: %f seconds\n", duration);

  struct RGB image[HEIGHT][WIDTH];
  
  // Fill the image with a gradient
  for (int n = 0; n < HEIGHT; n++) {
    for (int m = 0; m < WIDTH; m++) {
      image[n][m].r = (unsigned char) r_array[n][m];
      image[n][m].g = (unsigned char) g_array[n][m];
      image[n][m].b = (unsigned char) b_array[n][m];
    }
  }

  saveBMP("butterfly-c.bmp", (struct RGB *)image, WIDTH, HEIGHT);
  
  return EXIT_SUCCESS;
}
