#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include <time.h>
#include <assert.h>    

static uint16_t height = 1100; // for n
static uint16_t width = 2000;  // for m
static char* output_image_name = "butterfly-c.bmp";

/* frequent fractions (to avoid frequent divisions) */
static double f_1_2 = 0.5;
static double f_1_3 = 1.0 / 3.0;
static double f_1_4 = 0.25;
static double f_1_5 = 0.2;
static double f_1_8 = 1.0 / 8.0;
static double f_1_20 = 1.0 / 20.0;
static double f_1_25 = 1.0 / 25.0;
static double f_1_40 = 1.0 / 40.0;
static double f_1_50 = 1.0 / 50.0;
static double f_1_200 = 1.0 / 200.0;
static double f_2_5 = 2.0 / 5.0;
static double f_2_25 = 2.0 / 25.0;
static double f_3_50 = 3.0 / 50.0;
static double f_4_5 = 4.0 / 5.0;
static double f_5_2 = 5.0 / 2.0;
static double f_6_25 = 6.0 / 25.0;
static double f_7_5 = 7.0 / 5.0;
static double f_7_20 = 7.0 / 20.0;
static double f_7_50 = 7.0 / 50.0;
static double f_8_5 = 8.0 / 5.0;
static double f_12_5 = 12.0 / 5.0;
static double f_21_20 = 21.0 / 20.0;
static double f_23_20 = 23.0 / 20.0;
static double f_49_50 = 49.0 / 50.0;
static double f_81_250 = 81.0 / 250.0;
static double f_196_5 = 196.0 / 5.0;

double exp_minus_exp(double x) {
  return (x > 85.0) ? 0.0 : exp(-exp(x));
}

double exp_minus_exp_minus_exp(double x, double y) {
  return (x > 85.0 || y > 85.0) ? 0.0 : exp(- exp(x) - exp(y));
}

/* the structure containing RGB arrays */
typedef struct {
  uint16_t height;
  uint16_t width;
  uint8_t** r_array;
  uint8_t** g_array;
  uint8_t** b_array;
} RGBArrays;

/* Creates BMP file from RGB arrays.
   (proposed by ChatGTP and kept untouched since not involved in performance comparison)
   */
void save_bmp(const char *filename, RGBArrays* rgbArrays1) {
    FILE *f = fopen(filename, "wb");
    if (f == NULL) {
        perror("Failed to open file for writing");
        return;
    }

    // BMP file header (14 bytes)
    uint8_t file_header[14] = {
      'B', 'M',                // Signature
      0, 0, 0, 0,              // File size (we'll update later)
      0, 0,                    // Reserved
      0, 0,                    // Reserved
      54, 0, 0, 0              // Pixel data offset (54 bytes)
    };
    
    // BMP info header (40 bytes)
    uint8_t info_header[40] = {
      40, 0, 0, 0,             // Header size
      0, 0, 0, 0,             // Image width (we'll update later)
      0, 0, 0, 0,             // Image height (we'll update later)
      1, 0,                   // Color planes (1)
      24, 0,                  // Bits per pixel (24 for RGB)
      0, 0, 0, 0,             // Compression (none)
      0, 0, 0, 0,             // Image size (we'll update later)
      0, 0, 0, 0,             // Horizontal resolution
      0, 0, 0, 0,             // Vertical resolution
      0, 0, 0, 0              // Colors in palette (0 means no palette)
    };
    
    // Calculate row size and padding
    int row_size = rgbArrays1->width * 3;  // 3 bytes per pixel (RGB)
    int padding = (4 - (row_size % 4)) % 4;  // Padding to align rows to 4 bytes
    
    // Calculate the total file size
    int data_size = (row_size + padding) * rgbArrays1->height;  // Pixel data size
    int file_size = 54 + data_size;  // File header + info header + pixel data

    // Update file header with the actual file size
    file_header[2] = (file_size & 0xFF);
    file_header[3] = (file_size >> 8) & 0xFF;
    file_header[4] = (file_size >> 16) & 0xFF;
    file_header[5] = (file_size >> 24) & 0xFF;

    // Update info header with image width and height
    info_header[4] = (rgbArrays1->width & 0xFF);
    info_header[5] = (rgbArrays1->width >> 8) & 0xFF;
    info_header[6] = (rgbArrays1->width >> 16) & 0xFF;
    info_header[7] = (rgbArrays1->width >> 24) & 0xFF;

    info_header[8] = (rgbArrays1->height & 0xFF);
    info_header[9] = (rgbArrays1->height >> 8) & 0xFF;
    info_header[10] = (rgbArrays1->height >> 16) & 0xFF;
    info_header[11] = (rgbArrays1->height >> 24) & 0xFF;

    // Write file header and info header to file
    fwrite(file_header, sizeof(uint8_t), 14, f);
    fwrite(info_header, sizeof(uint8_t), 40, f);

    // Write pixel data (bottom-up order)
    for (int y = rgbArrays1->height - 1; y >= 0; y--) {
        for (int x = 0; x < rgbArrays1->width; x++) {
            // BMP stores pixels in BGR order, not RGB
            uint8_t b = rgbArrays1->b_array[y][x];
            uint8_t g = rgbArrays1->g_array[y][x];
            uint8_t r = rgbArrays1->r_array[y][x];
            fwrite(&b, sizeof(uint8_t), 1, f);
            fwrite(&g, sizeof(uint8_t), 1, f);
            fwrite(&r, sizeof(uint8_t), 1, f);
        }
        // Add padding to the row if necessary
        for (int i = 0; i < padding; i++) {
            uint8_t pad = 0;
            fwrite(&pad, sizeof(uint8_t), 1, f);
        }
    }

    // Close the file
    fclose(f);
    printf("BMP file saved as %s\n", filename);
}

/* Calculates C(x,y) */
double C(double x, double y) {
  double res = pow(sin(0
                       + ( 14 * atan(
                                     ( 100 * (y + (x * f_1_4) - f_1_25) )
                                     / ( 1.0 + fabs(100.0 * x - 25.0 * y - 3.0 * atan(100.0 * x - 25.0 * y)) )) )
                       + ( 14 * fabs((x * f_1_2) - (y * f_1_8)) )),
                   4);
  return res;
}

/* Calculates E(x,y) */
double E(double x, double y) {
  double res = 1.0
        - exp(-exp( 0
                    + ( -100.0 * pow((3.0 * y + 0.75 * x + 0.27), 4) )
                    + ( -100.0 * pow(fabs(7
                                          * ( 1.0 + 1.0 / ( sqrt(fabs(100.0 * y + 25.0 * x - 6.0)) + 0.3 ) )
                                          * (x - y * f_1_4)),
                                     (3.0 * y + 0.75 * x + 2.27)) )
                    + 10.0))
        * (1.0 - exp_minus_exp_minus_exp(
                                         ( 200.0 * fabs(y + x * f_1_4 - 0.2 + 3 * (x - y * f_1_4) * (x - y * f_1_4)) - 32.0),
                                         (500.0 * fabs(y + x * f_1_4 - f_1_20 - (0.7 * sqrt(fabs(x - y * f_1_4)))) - 2.5)));
  return res;
  }

/* Calculates L(x,y) */
double L(double x, double y) {
  double res = 0.0;
  for (uint8_t s = 1; s <= 25; s++) {
    res += pow(sin(( (80.0 + 30.0 * sin(s*s))
                     * atan(( 100.0 * y + 25.0 * x - 4.0 * sin(s) )
                            / ( 1.0 + fabs(100.0 * x - 25.0 * y - 3.0 * atan(100.0 * x - 25.0 * y)) )) )
                   + fabs((x * f_1_2) - (y * f_1_8))
                   + (4.0 * sin(5.0 * s))),
               6);
  }
  return res;
}

/* Calculates W(x,y) knowing C(x,y) */
double W(double x, double y, double Cxy) {
double omega1 = 0.0
        + (- 40 * Cxy)
        + f_196_5
        + (f_4_5
           * (sqrt(0
                   + ( (x - (y * f_1_4)) * (x - (y * f_1_4)) )
                   + ( (y + (x * f_1_4)) * (y + (x * f_1_4)) ))));
      double omega2 = - (40.0 * (0
                                 + ( 5.0 * fabs(y
                                                + (x * f_1_4)
                                                - f_3_50
                                                + (f_1_3
                                                   * (x - (y * f_1_4))
                                                   * (x - (y * f_1_4)))) )
                                 + pow(fabs((2.0 * x) - (y * f_1_2)), 3)
                                 - f_2_5));
      double omega3 =  - 1000 * fabs(x - (y * f_1_4))
        + 100.0
        - 90.0 * atan(8.0 * y
                      + 2.0 * x
                      + f_8_5);
      double omega4 = 1000 * (0.0
                              + fabs(x - y * f_1_4)
                              - f_7_50
                              + ( 9.0 * (y + (x * f_1_4) + 0.2) * f_1_20 ));
      double omega5 = 70 * fabs((5 * ( fabs(y
                                            + x * f_1_4
                                            - f_3_50
                                            + (f_1_3 * (x - (y * f_1_4)) * (x - (y * f_1_4)))) ))
                                + ( pow(fabs((2.0 * x) - (y * f_1_2)), 3) )
                                - f_2_5)
        - f_1_200;
      double omega6 = 700 * fabs(0.0
                                 + fabs(x - y * f_1_4)
                                 - 0.1
                                 + (  0.9 * atan(8.0 * (y + (x * f_1_4) + f_1_5)) ))
        - f_21_20;
      double res = (- exp_minus_exp_minus_exp(omega1, omega2)) * (1.0 - exp_minus_exp_minus_exp(omega3, omega4))
        - exp_minus_exp(omega5)
        - exp_minus_exp(omega6)
        + 1.0;
      return res;
  }

/* Calculates A(v,x,y) knowing C(x,y)*/
void A(double* Axy, double x, double y, double Cxy) {

  double A_part1 = (x * f_1_4)
    + y
    -0.25 * fabs(sin( f_12_5 * ( (0.7 * fabs(x - (y * f_1_4))
                                        + (0.3 * sqrt(fabs(x - y * f_1_4)))) ) ));
  
  double A_part2 = x * f_1_4
    + f_7_20
    + y
    + 0.2 * atan(6.0 * fabs(x - (y * f_1_4)))
    + 0.2 * atan(40.0 * fabs(x - (y * f_1_4)))
    - f_23_20
    * ( 1.5
        + f_1_25 * cos(10.0 * (y + (x * f_1_4) + f_6_25))
        + 0.03 * Cxy
        + 0.3 * atan(30.0 * (y + (x * f_1_4) - 0.25)) )
    * fabs(x - (y * f_1_4));
  
  for (uint8_t v = 0; v <= 1; v++) {
    Axy[v] = exp(-exp(200.0 * ( v * f_1_50 + A_part1))
                 - exp(-200.0 * (A_part2 - v * f_7_50)));    
  }
}

/* Calculates K(v, x,y) */
double K(uint8_t v, double x, double y) { 
  double Kvxy = 0.0;
  for (uint8_t i = 1; i <= 60; i++) {
    double s = i;
    Kvxy +=  ( f_5_2
               * ( f_2_25 + f_3_50 * cos(s * (4.0 + 4.0 * v)) )
               * (sin(5.0 * s) + sin(2.0 * s) + 3.0) * f_1_5
               * exp(-exp(
                          
                          - 25.0
                          * ( pow(sin( sin(2.0 * s)
                                       + (6 + sin(s * s))
                                       * ( sin(7.0 * s) * (x * f_1_2) + cos(7.0 * s) * ((y - 8.0) * f_1_2) ) ),
                                  10)
                              * pow(sin( sin(3.0 * s)
                                         + (6 + 2 * sin(s * s))
                                         * ( sin(7.0 * s) * ((y - 8) * f_1_2) - cos(7.0 * s) * (x * f_1_2) ) ),
                                    10) - 0.1))));
                          
  }
  return Kvxy;
}

/* Calculates H(v,x,y) knowing E(x,y), L(x,y), W(x,y), A(v,x,y) */
void H(double* Hxy, double x, double y, double Exy, double Lxy, double Wxy, double* Axy) {

  double H_part1 = Axy[0]
    * Axy[1]
    * (1.0 - Exy)
    * (1.0 + Lxy * f_1_50)
    * exp_minus_exp(0
                    + exp(
                          (2.0 * y)
                          + (0.5 * x)
                          + f_2_5
                          - (2.0 * fabs(x - (y * f_1_4))))
                    + exp(
                          (8.0 * y)
                          + (2.0 * x)
                          + f_2_5
                          - fabs((8.0 * x) - (2.0 * y))))
    * Wxy;
  
  double H_part2 = exp_minus_exp( - (50.0 *
                                     ( (pow(cos ((2.0 * y) 
                                                 + (x * 0.5)
                                                 + f_7_5
                                                 - fabs((2.0 * x)
                                                        - (y * 0.5))),
                                            80)
                                        * pow(sin((20.0 * y)
                                                  + (5.0 * x)
                                                  + fabs((20.0 * x) - (5.0 * y))),
                                              2))
                                       - pow(0.0
                                             + (2.7 * y)
                                             + ((27.0 * x) * f_1_40)
                                             + f_81_250,
                                             10)
                                       - f_49_50)));
  
  
  for (uint8_t v = 0; v <= 2; v++) {
    double Kvxy = K(v, x, y);
    Hxy[v] =  0.0
      + ( ((18 - (9.0 * v) + (v * v)) * f_1_20)
          * (1.0 - Axy[0])
          * (1.0 - Exy)
          * Kvxy )
      + ((2 + (3.0 * v)) * f_1_5) * H_part1
      + H_part2
      + 0.1 * Exy * ((v - 1.0) * (v - 1));
  }
}

/* Calcules F(z) */
double F(double z) {
  double res = floor(255.0
                     * exp_minus_exp(- (1000.0 * z))
                     * pow(fabs(z),
                           exp_minus_exp(1000.0 * (z - 1.0))));
  return res;
}

/* Allocates memory for 2D array */
uint8_t** malloc2DArray(uint16_t height, uint16_t width) {
  uint8_t** tmp_array = (uint8_t**) malloc(sizeof(uint8_t*) * height);
  for (uint16_t n = 0; n < height; n++) { 
    tmp_array[n] = (uint8_t*) malloc(sizeof(uint8_t) * width);
  }
  assert(tmp_array && "Impossible to malloc 2D array");
  return tmp_array;
}

/* Allocates memory for a RGBArrays struct */
RGBArrays* mallocRGBArrays(uint16_t height, uint16_t width) {
  RGBArrays* rgbArrays1 = malloc(sizeof(RGBArrays));
  assert(rgbArrays1 && "Impossible to malloc RGB array");
  rgbArrays1->height = height;
  rgbArrays1->width = width;
  rgbArrays1->r_array = malloc2DArray(height, width);
  rgbArrays1->g_array = malloc2DArray(height, width);
  rgbArrays1->b_array = malloc2DArray(height, width);
  return rgbArrays1;
}

/* Frees memory allocated to a 2D array */
void free2DArray(uint8_t** p, uint16_t height) {
  for (uint16_t n = 0; n < height; n++) {
    free(p[n]);
  }
  free(p);
}

/* Frees memory allocated to a RGBArrays struct */
void freeRGBArrays(RGBArrays* rgbArrays1) {
  free2DArray(rgbArrays1->r_array, rgbArrays1->height);
  free2DArray(rgbArrays1->g_array, rgbArrays1->height);
  free2DArray(rgbArrays1->b_array, rgbArrays1->height);
  free(rgbArrays1);
}

/* Populates an RGBArrays struct with calculated values */
void populateRGBArrays_1(RGBArrays* rgbArrays1) {
  double Axy[2];
  double Hxy[3];
  double x;
  double y;
  double Cxy;
  double Exy;
  double Lxy;
  double Wxy;
  
  for (uint16_t n = 1; n <= rgbArrays1->height; n++) {
    for (uint16_t m = 1; m <= rgbArrays1->width; m++) {
      
      x = (m - 1000.0) / 960.0;
      y = (451.0 - n) / 960.0;
      Cxy = C(x, y);
      Exy = E(x, y);
      Lxy = L(x, y);
      Wxy = W(x, y, Cxy);
      A(Axy, x, y, Cxy);
      H(Hxy, x, y, Exy, Lxy, Wxy, Axy);

      rgbArrays1->r_array[n-1][m-1] = F(Hxy[0]);
      rgbArrays1->g_array[n-1][m-1] = F(Hxy[1]);
      rgbArrays1->b_array[n-1][m-1] = F(Hxy[2]);

      if ( (m == 1) && ( (n == 1) || ((n % 100) == 0) )) {
        printf("n = %d\n", n );
      }
    }
  }
}

/* Parallelization with pragma with chunks */
void populateRGBArrays_2(RGBArrays* rgbArrays1) {

  int chunk_size = rgbArrays1->height / 100;
    
  #pragma omp parallel for schedule(static, chunk_size)
  for (uint16_t n = 1; n <= rgbArrays1->height; n++) {

    double Axy[2];
    double Hxy[3];
    double x;
    double y;
    double Cxy;
    double Exy;
    double Lxy;
    double Wxy;
    
    for (uint16_t m = 1; m <= rgbArrays1->width; m++) {
      
      x = (m - 1000.0) / 960.0;
      y = (451.0 - n) / 960.0;
      Cxy = C(x, y);
      Exy = E(x, y);
      Lxy = L(x, y);
      Wxy = W(x, y, Cxy);
      A(Axy, x, y, Cxy);
      H(Hxy, x, y, Exy, Lxy, Wxy, Axy);

      rgbArrays1->r_array[n-1][m-1] = F(Hxy[0]);
      rgbArrays1->g_array[n-1][m-1] = F(Hxy[1]);
      rgbArrays1->b_array[n-1][m-1] = F(Hxy[2]);

      if ( (m == 1) && ( (n == 1) || ((n % 100) == 0) )) {
        printf("n = %d\n", n );
      }
    }
  }
}

int main(int argc, [[maybe_unused]] char* argv[argc+1]) {
  
  struct timespec start, end;
  double duration;
  clock_gettime(CLOCK_MONOTONIC, &start);

  RGBArrays* rgbArrays1 = mallocRGBArrays(height, width);

  populateRGBArrays_2(rgbArrays1);

  clock_gettime(CLOCK_MONOTONIC, &end);
  duration = (end.tv_sec - start.tv_sec) + (end.tv_nsec - start.tv_nsec) / 1e9;
  printf("Duration: %f seconds\n", duration);

  fflush(stdout);
  
  printf("Creating image...\n");
  save_bmp(output_image_name, rgbArrays1);

  printf("Freeing memory allocated to RGB array...\n");
  freeRGBArrays(rgbArrays1);

  return EXIT_SUCCESS;
}
