#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include <time.h>

#define HEIGHT 1100 // for n
#define WIDTH 2000  // for m

#define ONE_OVER_TWO 0.5
#define ONE_OVER_THREE 0.33333333333333333
#define ONE_OVER_FOUR 0.25
#define ONE_OVER_FIVE 0.2
#define ONE_OVER_EIGHT 0.125
#define ONE_OVER_TWENTY 0.05
#define ONE_OVER_TWENTY_FIVE 0.04
#define ONE_OVER_FORTY 0.025
#define ONE_OVER_FIFTY 0.02
#define ONE_OVER_TWO_HUNDRED 0.005
#define TWO_OVER_FIVE 0.4
#define TWO_OVER_TWENTY_FIVE 0.08
#define THREE_OVER_FIFTY 0.06
#define FOUR_OVER_FIVE 0.8
#define FIVE_OVER_TWO 2.5
#define SIX_OVER_TWENTY_FIVE 0.24
#define SEVEN_OVER_FIVE 1.4
#define SEVEN_OVER_TWENTY 0.35
#define SEVEN_OVER_FIFTY 0.14
#define EIGHT_OVER_FIVE 1.6
#define TWELVE_OVER_FIVE 2.4
#define TWENTY_ONE_OVER_TWENTY 1.05
#define TWENTY_THREE_OVER_TWENTY 1.15
#define FORTY_NINE_OVER_FIFTY 0.98
#define EIGHTY_ONE_OVER_TWO_HUNDRED_FIFTY 0.324
#define ONE_HUNDRED_NINETY_SIX_OVER_FIVE 39.2

double exp_minus_exp(double x) {
  return (x > 85.0) ? 0.0 : exp(-exp(x));
}

double exp_minus_exp_minus_exp(double x, double y) {
  return (x > 85.0 || y > 85.0) ? 0.0 : exp(- exp(x) - exp(y));
}

// Function to create the BMP file
// proposed by ChatGPT
void save_bmp(const char *filename, uint8_t **r_array, uint8_t **g_array, uint8_t **b_array, int width, int height) {
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
    int row_size = width * 3;  // 3 bytes per pixel (RGB)
    int padding = (4 - (row_size % 4)) % 4;  // Padding to align rows to 4 bytes
    
    // Calculate the total file size
    int data_size = (row_size + padding) * height;  // Pixel data size
    int file_size = 54 + data_size;  // File header + info header + pixel data

    // Update file header with the actual file size
    file_header[2] = (file_size & 0xFF);
    file_header[3] = (file_size >> 8) & 0xFF;
    file_header[4] = (file_size >> 16) & 0xFF;
    file_header[5] = (file_size >> 24) & 0xFF;

    // Update info header with image width and height
    info_header[4] = (width & 0xFF);
    info_header[5] = (width >> 8) & 0xFF;
    info_header[6] = (width >> 16) & 0xFF;
    info_header[7] = (width >> 24) & 0xFF;

    info_header[8] = (height & 0xFF);
    info_header[9] = (height >> 8) & 0xFF;
    info_header[10] = (height >> 16) & 0xFF;
    info_header[11] = (height >> 24) & 0xFF;

    // Write file header and info header to file
    fwrite(file_header, sizeof(uint8_t), 14, f);
    fwrite(info_header, sizeof(uint8_t), 40, f);

    // Write pixel data (bottom-up order)
    for (int y = height - 1; y >= 0; y--) {
        for (int x = 0; x < width; x++) {
            // BMP stores pixels in BGR order, not RGB
            uint8_t b = b_array[y][x];
            uint8_t g = g_array[y][x];
            uint8_t r = r_array[y][x];
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

double C(double x, double y) {
  double res = pow(sin(0
                       + ( 14 * atan(
                                     ( 100 * (y + (x * ONE_OVER_FOUR) - ONE_OVER_TWENTY_FIVE) )
                                     / ( 1.0 + fabs(100.0 * x - 25.0 * y - 3.0 * atan(100.0 * x - 25.0 * y)) )) )
                       + ( 14 * fabs((x * ONE_OVER_TWO) - (y * ONE_OVER_EIGHT)) )),
                   4);
  return res;
}

double E(double x, double y) {
  double res = 1.0
        - exp(-exp( 0
                    + ( -100.0 * pow((3.0 * y + 0.75 * x + 0.27), 4) )
                    + ( -100.0 * pow(fabs(7
                                          * ( 1.0 + 1.0 / ( sqrt(fabs(100.0 * y + 25.0 * x - 6.0)) + 0.3 ) )
                                          * (x - y * ONE_OVER_FOUR)),
                                     (3.0 * y + 0.75 * x + 2.27)) )
                    + 10.0))
        * (1.0 - exp_minus_exp_minus_exp(
                                         ( 200.0 * fabs(y + x * ONE_OVER_FOUR - 0.2 + 3 * (x - y * ONE_OVER_FOUR) * (x - y * ONE_OVER_FOUR)) - 32.0),
                                         (500.0 * fabs(y + x * ONE_OVER_FOUR - ONE_OVER_TWENTY - (0.7 * sqrt(fabs(x - y * ONE_OVER_FOUR)))) - 2.5)));
  return res;
  }

double L(double x, double y) {
  double res = 0.0;
  for (uint8_t s = 1; s <= 25; s++) {
    res += pow(sin(( (80.0 + 30.0 * sin(s*s))
                     * atan(( 100.0 * y + 25.0 * x - 4.0 * sin(s) )
                            / ( 1.0 + fabs(100.0 * x - 25.0 * y - 3.0 * atan(100.0 * x - 25.0 * y)) )) )
                   + fabs((x * ONE_OVER_TWO) - (y * ONE_OVER_EIGHT))
                   + (4.0 * sin(5.0 * s))),
               6);
  }
  return res;
}

double W(double x, double y, double Cxy) {
double omega1 = 0.0
        + (- 40 * Cxy)
        + ONE_HUNDRED_NINETY_SIX_OVER_FIVE
        + (FOUR_OVER_FIVE
           * (sqrt(0
                   + ( (x - (y * ONE_OVER_FOUR)) * (x - (y * ONE_OVER_FOUR)) )
                   + ( (y + (x * ONE_OVER_FOUR)) * (y + (x * ONE_OVER_FOUR)) ))));
      double omega2 = - (40.0 * (0
                                 + ( 5.0 * fabs(y
                                                + (x * ONE_OVER_FOUR)
                                                - THREE_OVER_FIFTY
                                                + (ONE_OVER_THREE
                                                   * (x - (y * ONE_OVER_FOUR))
                                                   * (x - (y * ONE_OVER_FOUR)))) )
                                 + pow(fabs((2.0 * x) - (y * ONE_OVER_TWO)), 3)
                                 - TWO_OVER_FIVE));
      double omega3 =  - 1000 * fabs(x - (y * ONE_OVER_FOUR))
        + 100.0
        - 90.0 * atan(8.0 * y
                      + 2.0 * x
                      + EIGHT_OVER_FIVE);
      double omega4 = 1000 * (0.0
                              + fabs(x - y * ONE_OVER_FOUR)
                              - 7.0 / 50.0
                              + ( 9.0 * (y + (x * ONE_OVER_FOUR) + 0.2) * ONE_OVER_TWENTY ));
      double omega5 = 70 * fabs((5 * ( fabs(y
                                            + x * ONE_OVER_FOUR
                                            - THREE_OVER_FIFTY
                                            + (ONE_OVER_THREE * (x - (y * ONE_OVER_FOUR)) * (x - (y * ONE_OVER_FOUR)))) ))
                                + ( pow(fabs((2.0 * x) - (y * ONE_OVER_TWO)), 3) )
                                - TWO_OVER_FIVE)
        - ONE_OVER_TWO_HUNDRED;
      double omega6 = 700 * fabs(0.0
                                 + fabs(x - y * ONE_OVER_FOUR)
                                 - 0.1
                                 + (  0.9 * atan(8.0 * (y + (x * ONE_OVER_FOUR) + ONE_OVER_FIVE)) ))
        - TWENTY_ONE_OVER_TWENTY;
      double res = (- exp_minus_exp_minus_exp(omega1, omega2)) * (1.0 - exp_minus_exp_minus_exp(omega3, omega4))
        - exp_minus_exp(omega5)
        - exp_minus_exp(omega6)
        + 1.0;
      return res;
  }

void A(double* Axy, double x, double y, double Cxy) {

  double A_part1 = (x * ONE_OVER_FOUR)
    + y
    -0.25 * fabs(sin( TWELVE_OVER_FIVE * ( (0.7 * fabs(x - (y * ONE_OVER_FOUR))
                                        + (0.3 * sqrt(fabs(x - y * ONE_OVER_FOUR)))) ) ));
  
  double A_part2 = x * ONE_OVER_FOUR
    + SEVEN_OVER_TWENTY
    + y
    + 0.2 * atan(6.0 * fabs(x - (y * ONE_OVER_FOUR)))
    + 0.2 * atan(40.0 * fabs(x - (y * ONE_OVER_FOUR)))
    - TWENTY_THREE_OVER_TWENTY
    * ( 1.5
        + ONE_OVER_TWENTY_FIVE * cos(10.0 * (y + (x * ONE_OVER_FOUR) + SIX_OVER_TWENTY_FIVE))
        + 0.03 * Cxy
        + 0.3 * atan(30.0 * (y + (x * ONE_OVER_FOUR) - 0.25)) )
    * fabs(x - (y * ONE_OVER_FOUR));
  
  for (uint8_t v = 0; v <= 1; v++) {
    Axy[v] = exp(-exp(200.0 * ( v * ONE_OVER_FIFTY + A_part1))
                 - exp(-200.0 * (A_part2 - v * SEVEN_OVER_FIFTY)));    
  }
}

double K(uint8_t v, double x, double y) { 
  double Kvxy = 0.0;
  for (uint8_t i = 1; i <= 60; i++) {
    double s = i;
    Kvxy +=  ( FIVE_OVER_TWO
               * ( TWO_OVER_TWENTY_FIVE + THREE_OVER_FIFTY * cos(s * (4.0 + 4.0 * v)) )
               * (sin(5.0 * s) + sin(2.0 * s) + 3.0) * ONE_OVER_FIVE
               * exp(-exp(
                          
                          - 25.0
                          * ( pow(sin( sin(2.0 * s)
                                       + (6 + sin(s * s))
                                       * ( sin(7.0 * s) * (x * ONE_OVER_TWO) + cos(7.0 * s) * ((y - 8.0) * ONE_OVER_TWO) ) ),
                                  10)
                              * pow(sin( sin(3.0 * s)
                                         + (6 + 2 * sin(s * s))
                                         * ( sin(7.0 * s) * ((y - 8) * ONE_OVER_TWO) - cos(7.0 * s) * (x * ONE_OVER_TWO) ) ),
                                    10) - 0.1))));
                          
  }
  return Kvxy;
}

void H(double* Hxy, double x, double y, double Exy, double Lxy, double Wxy, double* Axy) {

  double H_part1 = Axy[0]
    * Axy[1]
    * (1.0 - Exy)
    * (1.0 + Lxy * ONE_OVER_FIFTY)
    * exp_minus_exp(0
                    + exp(
                          (2.0 * y)
                          + (0.5 * x)
                          + TWO_OVER_FIVE
                          - (2.0 * fabs(x - (y * ONE_OVER_FOUR))))
                    + exp(
                          (8.0 * y)
                          + (2.0 * x)
                          + TWO_OVER_FIVE
                          - fabs((8.0 * x) - (2.0 * y))))
    * Wxy;
  
  double H_part2 = exp_minus_exp( - (50.0 *
                                     ( (pow(cos ((2.0 * y) 
                                                 + (x * 0.5)
                                                 + SEVEN_OVER_FIVE
                                                 - fabs((2.0 * x)
                                                        - (y * 0.5))),
                                            80)
                                        * pow(sin((20.0 * y)
                                                  + (5.0 * x)
                                                  + fabs((20.0 * x) - (5.0 * y))),
                                              2))
                                       - pow(0.0
                                             + (2.7 * y)
                                             + ((27.0 * x) * ONE_OVER_FORTY)
                                             + EIGHTY_ONE_OVER_TWO_HUNDRED_FIFTY,
                                             10)
                                       - FORTY_NINE_OVER_FIFTY)));
  
  
  for (uint8_t v = 0; v <= 2; v++) {
    double Kvxy = K(v, x, y);
    Hxy[v] =  0.0
      + ( ((18 - (9.0 * v) + (v * v)) * ONE_OVER_TWENTY)
          * (1.0 - Axy[0])
          * (1.0 - Exy)
          * Kvxy )
      + ((2 + (3.0 * v)) * ONE_OVER_FIVE) * H_part1
      + H_part2
      + 0.1 * Exy * ((v - 1.0) * (v - 1));
  }
}

double F(double z) {
  double res = floor(255.0
                     * exp_minus_exp(- (1000.0 * z))
                     * pow(fabs(z),
                           exp_minus_exp(1000.0 * (z - 1.0))));
  return res;
}

int main(int argc, [[maybe_unused]] char* argv[argc+1]) {

  struct timespec start, end;
  double duration;
  clock_gettime(CLOCK_MONOTONIC, &start);

  printf("Allocating memory to r_array...\n");
  uint8_t** r_array = (uint8_t**) malloc(sizeof(uint8_t*) * HEIGHT);
  for (uint16_t n = 0; n < HEIGHT; n++) { 
    r_array[n] = (uint8_t*)malloc(sizeof(uint8_t) * WIDTH);
  }
  if (r_array == NULL) {
    printf("Memory allocation of r_array failed.\n");
    return EXIT_FAILURE;
  }

  printf("Allocating memory to g_array...\n");
  uint8_t** g_array = (uint8_t**) malloc(sizeof(uint8_t*) * HEIGHT);
  for (uint16_t n = 0; n < HEIGHT; n++) { 
    g_array[n] = (uint8_t*)malloc(sizeof(uint8_t) * WIDTH);
  }
  if (g_array == NULL) {
    printf("Memory allocation of g_array failed.\n");
    return EXIT_FAILURE;
  }

  printf("Allocating memory to b_array...\n");
  uint8_t** b_array = (uint8_t**) malloc(sizeof(uint8_t*) * HEIGHT);
  for (uint16_t n = 0; n < HEIGHT; n++) { 
    b_array[n] = (uint8_t*)malloc(sizeof(uint8_t) * WIDTH);
  }
  if (b_array == NULL) {
    printf("Memory allocation of b_array failed.\n");
    return EXIT_FAILURE;
  }

  double Axy[2];
  double Hxy[3];
  double x;
  double y;
  double Cxy;
  double Exy;
  double Lxy;
  double Wxy;
  
  for (uint16_t n = 1; n <= HEIGHT; n++) {
    for (uint16_t m = 1; m <= WIDTH; m++) {
      
      x = (m - 1000.0) / 960.0;
      y = (451.0 - n) / 960.0;
      Cxy = C(x, y);
      Exy = E(x, y);
      Lxy = L(x, y);
      Wxy = W(x, y, Cxy);
      A(Axy, x, y, Cxy);
      H(Hxy, x, y, Exy, Lxy, Wxy, Axy);

      r_array[n-1][m-1] = F(Hxy[0]);
      g_array[n-1][m-1] = F(Hxy[1]);
      b_array[n-1][m-1] = F(Hxy[2]);

      if ( (m == 1) && ( (n == 1) || ((n % 100) == 0) )) {
        printf("n = %d\n", n );
      }
    }
  }

  clock_gettime(CLOCK_MONOTONIC, &end);
  duration = (end.tv_sec - start.tv_sec) + (end.tv_nsec - start.tv_nsec) / 1e9;
  printf("Duration: %f seconds\n", duration);
  fflush(stdout);
  
  printf("Creating image...\n");
  save_bmp("butterfly-c.bmp", r_array, g_array, b_array, WIDTH, HEIGHT);

  printf("Freeing memory allocated to r_array...\n");
  for (uint16_t n = 0; n < HEIGHT; n++) {
    free(r_array[n]);
  }
  free(r_array);

  printf("Freeing memory allocated to g_array...\n");
  for (uint16_t n = 0; n < HEIGHT; n++) {
    free(g_array[n]);
  }
  free(g_array);

  printf("Freeing memory allocated to b_array...\n");
  for (uint16_t n = 0; n < HEIGHT; n++) {
    free(b_array[n]);
  }
  free(b_array);

  printf("End of program.\n");

  return EXIT_SUCCESS;
}

