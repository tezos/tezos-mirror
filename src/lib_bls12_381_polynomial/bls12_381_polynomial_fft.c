/* MIT License
 * Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include "bls12_381_polynomial_fft.h"
#include "caml_bls12_381_stubs.h"
#include <caml/fail.h>
#include <string.h>

// IMPROVEME: can be improve it with lookups?
int bls12_381_polynomial_bitreverse(int n, int l) {
  int r = 0;
  while (l-- > 0) {
    r = (r << 1) | (n & 1);
    n = n >> 1;
  }
  return r;
}

void bls12_381_polynomial_reorg_fr_array_coefficients(
    blst_fr *coefficients, int n, int logn) {
  blst_fr tmp;
  for (int i = 0; i < n; i++) {
    int reverse_i = bls12_381_polynomial_bitreverse(i, logn);
    if (i < reverse_i) {
      memcpy(&tmp, coefficients + i, sizeof(blst_fr));
      memcpy(coefficients + i, coefficients + reverse_i, sizeof(blst_fr));
      memcpy(coefficients + reverse_i, &tmp, sizeof(blst_fr));
    }
  }
}

// Fr
void bls12_381_polynomial_fft_inplace_aux(blst_fr *coefficients,
                                                   blst_fr *domain,
                                                   int log_domain_size,
                                                   int log_diff,
                                                   int inverse_domain) {
  int domain_size = 1 << log_domain_size;
  int m = 1 << log_diff;
  blst_fr tmp;
  int domain_idx;
  for (int i = 0; i < log_domain_size - log_diff; i++) {
    int exponent = domain_size / (2 * m);
    for (int k = 0; k < domain_size; k += (2 * m)) {
      for (int j = 0; j < m; j++) {
        if (inverse_domain == 0) {
          domain_idx = exponent * j;
        } else {
          domain_idx = domain_size - (exponent * j);
        };
        if (domain_idx == domain_size) {
          domain_idx = 0;
        };
        blst_fr_mul(&tmp, coefficients + (k + j + m), domain + domain_idx);
        blst_fr_sub(coefficients + (k + j + m), coefficients + (k + j), &tmp);
        blst_fr_add(coefficients + (k + j), coefficients + (k + j), &tmp);
      }
    }
    m = 2 * m;
  }
}

void bls12_381_polynomial_fft_inplace(blst_fr *coefficients,
                                               blst_fr *domain,
                                               int log_domain_size,
                                               int log_degree) {
  int domain_size = 1 << log_domain_size;
  int degree_next_pow_of_2 = 1 << log_degree;

  int log_diff = log_domain_size - log_degree;

  if (log_diff > 0) {
    bls12_381_polynomial_reorg_fr_array_coefficients(
        coefficients, degree_next_pow_of_2, log_degree);
    int ratio = 1 << log_diff;
    for (int i = degree_next_pow_of_2 - 1; i >= 0; i--) {
      for (int j = 0; j < ratio; j++) {
        memcpy(coefficients + (i * ratio) + j, coefficients + i,
               sizeof(blst_fr));
      }
    }
  } else {
    bls12_381_polynomial_reorg_fr_array_coefficients(
        coefficients, domain_size, log_domain_size);
  }
  bls12_381_polynomial_fft_inplace_aux(coefficients, domain,
                                                log_domain_size, log_diff, 0);
}

void bls12_381_polynomial_ifft_inplace(blst_fr *coefficients,
                                                blst_fr *domain,
                                                int log_domain_size) {
  int domain_size = 1 << log_domain_size;
  uint64_t n[4] = {domain_size, 0, 0, 0};
  blst_fr inverse_n;

  bls12_381_polynomial_reorg_fr_array_coefficients(
      coefficients, domain_size, log_domain_size);
  bls12_381_polynomial_fft_inplace_aux(coefficients, domain,
                                                log_domain_size, 0, 1);

  blst_fr_from_uint64(&inverse_n, n);
  blst_fr_inverse(&inverse_n, &inverse_n);
  for (int i = 0; i < domain_size; i++) {
    blst_fr_mul(coefficients + i, coefficients + i, &inverse_n);
  }
}

void bls12_381_polynomial_dft_inplace(blst_fr *coefficients,
                                               blst_fr *domain, blst_fr *buffer,
                                               int length, int inverse) {
  // We copy the coefficients to the buffer to modify the coefficients
  // in-place
  memcpy(buffer, coefficients, length * sizeof(blst_fr));
  blst_fr tmp;
  for (int i = 0; i < length; i++) {
    blst_fr_set_to_zero(coefficients + i);
    for (int j = 0; j < length; j++) {
      int idx = ((i * j) % length);
      if (inverse && (idx != 0)) {
        idx = length - idx;
      }
      blst_fr_mul(&tmp, buffer + j, domain + (idx));
      blst_fr_add(coefficients + i, coefficients + i, &tmp);
    }
  }
  if (inverse) {
    blst_fr inv_n, n;
    blst_fr_from_uint64(&n, (uint64_t[4]){length, 0, 0, 0});
    blst_fr_inverse(&inv_n, &n);
    for (int i = 0; i < length; i++) {
      blst_fr_mul(coefficients + i, coefficients + i, &inv_n);
    }
  }
}

int bls12_381_polynomial_is_power_of_two(int n) {
  return (n & (n - 1)) == 0;
}

int bls12_381_polynomial_log2(int n) {
  int l = 0;
  while (n >>= 1) {
    ++l;
  }
  return l;
}

void bls12_381_polynomial_transpose(blst_fr *rows, blst_fr *columns,
                                             int n1, int n2) {
  for (int i = 0; i < n1; i++) {
    for (int j = 0; j < n2; j++) {
      rows[j * n1 + i] = columns[j + i * n2];
    }
  }
}

int bls12_381_polynomial_max(int a, int b) { return a >= b ? a : b; }

void bls12_381_polynomial_fft_round(blst_fr *buffer, blst_fr *domain2,
                                             int length1, int length2,
                                             blst_fr *buffer_dft, int inverse) {
  if (bls12_381_polynomial_is_power_of_two(length2)) {
    int length2_log = bls12_381_polynomial_log2(length2);
    if (inverse) {
      for (int i = 0; i < length1; i++) {
        bls12_381_polynomial_ifft_inplace(buffer + (i * length2),
                                                   domain2, length2_log);
      }
    } else {
      for (int i = 0; i < length1; i++) {
        bls12_381_polynomial_fft_inplace(
            buffer + (i * length2), domain2, length2_log, length2_log);
      }
    }
  } else {
    for (int i = 0; i < length1; i++) {
      bls12_381_polynomial_dft_inplace(buffer + (i * length2), domain2,
                                                buffer_dft, length2, inverse);
    }
  }
}

// The buffer must have size at least 2 * |domain1| * |domain2|
void bls12_381_polynomial_prime_factor_algorithm_fft(
    blst_fr *coefficients, blst_fr *domain1, blst_fr *domain2, int length1,
    int length2, int inverse) {
  int dft_length =
      (!bls12_381_polynomial_is_power_of_two(length1) &&
       !bls12_381_polynomial_is_power_of_two(length2))
          ? bls12_381_polynomial_max(length1, length2)
          : (bls12_381_polynomial_is_power_of_two(length1) ? length2
                                                                    : length1);
  // We allocate buffer_dft on the stack because dft_length is <= 2^10 at most
  // in our use case
  blst_fr buffer_dft[dft_length];
  int length = length1 * length2;

  blst_fr *buffer = malloc(2 * length * sizeof(blst_fr));
  if (buffer == NULL) {
    caml_raise_out_of_memory();
  }

  for (int i = 0; i < length; i++) {
    buffer[(i % length1) * length2 + (i % length2)] = coefficients[i];
  }

  bls12_381_polynomial_fft_round(buffer, domain2, length1, length2,
                                          buffer_dft, inverse);

  blst_fr *new_buffer = buffer + length;

  bls12_381_polynomial_transpose(new_buffer, buffer, length1, length2);

  bls12_381_polynomial_fft_round(new_buffer, domain1, length2, length1,
                                          buffer_dft, inverse);

  for (int i = 0; i < length1; i++) {
    for (int j = 0; j < length2; j++) {
      coefficients[(length1 * j + length2 * i) % length] =
          new_buffer[j * length1 + i];
    }
  }

  free(buffer);
}

// G1
void bls12_381_polynomial_reorg_g1_array_coefficients(
    int n, int logn, blst_p1 *coefficients) {
  blst_p1 buffer;
  for (int i = 0; i < n; i++) {
    int reverse_i = bls12_381_polynomial_bitreverse(i, logn);
    if (i < reverse_i) {
      memcpy(&buffer, coefficients + i, sizeof(blst_p1));
      memcpy(coefficients + i, coefficients + reverse_i, sizeof(blst_p1));
      memcpy(coefficients + reverse_i, &buffer, sizeof(blst_p1));
    }
  }
}

void bls12_381_polynomial_fft_g1_inplace_aux(blst_p1 *coefficients,
                                                      blst_fr *domain,
                                                      int log_domain_size,
                                                      int log_diff,
                                                      int inverse_domain) {
  blst_p1 buffer;
  blst_p1 buffer_neg;
  blst_scalar scalar;
  byte le_scalar[32];

  int domain_size = 1 << log_domain_size;
  int m = 1 << log_diff;
  int domain_idx;
  for (int i = 0; i < log_domain_size - log_diff; i++) {
    int exponent = domain_size / (2 * m);
    for (int k = 0; k < domain_size; k += (2 * m)) {
      for (int j = 0; j < m; j++) {
        if (inverse_domain == 0) {
          domain_idx = exponent * j;
        } else {
          domain_idx = domain_size - (exponent * j);
        };
        if (domain_idx == domain_size) {
          domain_idx = 0;
        };
        blst_scalar_from_fr(&scalar, domain + domain_idx);
        blst_lendian_from_scalar(le_scalar, &scalar);
        blst_p1_mult(&buffer, coefficients + (k + j + m), le_scalar, 256);

        memcpy(&buffer_neg, &buffer, sizeof(blst_p1));
        blst_p1_cneg(&buffer_neg, 1);
        blst_p1_add_or_double(coefficients + (k + j + m),
                              coefficients + (k + j), &buffer_neg);

        blst_p1_add_or_double(coefficients + (k + j), coefficients + (k + j),
                              &buffer);
      }
    }
    m = 2 * m;
  }
}

void bls12_381_polynomial_fft_g1_inplace(blst_p1 *coefficients,
                                                  blst_fr *domain,
                                                  int log_domain_size,
                                                  int log_degree) {
  int domain_size = 1 << log_domain_size;
  int degree_next_pow_of_2 = 1 << log_degree;

  int log_diff = log_domain_size - log_degree;

  if (log_diff > 0) {
    bls12_381_polynomial_reorg_g1_array_coefficients(
        degree_next_pow_of_2, log_degree, coefficients);
    int ratio = 1 << log_diff;
    for (int i = degree_next_pow_of_2 - 1; i >= 0; i--) {
      for (int j = 0; j < ratio; j++) {
        memcpy(coefficients + (i * ratio) + j, coefficients + i,
               sizeof(blst_p1));
      }
    }
  } else {
    bls12_381_polynomial_reorg_g1_array_coefficients(
        domain_size, log_domain_size, coefficients);
  }

  bls12_381_polynomial_fft_g1_inplace_aux(
      coefficients, domain, log_domain_size, log_diff, 0);
}

void bls12_381_polynomial_ifft_g1_inplace(blst_p1 *coefficients,
                                                   blst_fr *domain,
                                                   int log_domain_size) {

  int domain_size = 1 << log_domain_size;
  bls12_381_polynomial_reorg_g1_array_coefficients(
      domain_size, log_domain_size, coefficients);
  bls12_381_polynomial_fft_g1_inplace_aux(coefficients, domain,
                                                   log_domain_size, 0, 1);

  uint64_t n[4] = {domain_size, 0, 0, 0};
  blst_fr inverse_n;
  blst_fr_from_uint64(&inverse_n, n);
  blst_fr_inverse(&inverse_n, &inverse_n);

  blst_scalar scalar;
  byte le_scalar[32];

  blst_scalar_from_fr(&scalar, &inverse_n);
  blst_lendian_from_scalar(le_scalar, &scalar);

  for (int i = 0; i < domain_size; i++) {
    blst_p1_mult(coefficients + i, coefficients + i, le_scalar, 256);
  }
}

// G2
void bls12_381_polynomial_reorg_g2_array_coefficients(
    int n, int logn, blst_p2 *coefficients) {
  blst_p2 buffer;
  for (int i = 0; i < n; i++) {
    int reverse_i = bls12_381_polynomial_bitreverse(i, logn);
    if (i < reverse_i) {
      memcpy(&buffer, coefficients + i, sizeof(blst_p2));
      memcpy(coefficients + i, coefficients + reverse_i, sizeof(blst_p2));
      memcpy(coefficients + reverse_i, &buffer, sizeof(blst_p2));
    }
  }
}

void bls12_381_polynomial_fft_g2_inplace_aux(blst_p2 *coefficients,
                                                      blst_fr *domain,
                                                      int log_domain_size,
                                                      int log_diff,
                                                      int inverse_domain) {
  blst_p2 buffer;
  blst_p2 buffer_neg;
  blst_scalar scalar;
  byte le_scalar[32];

  int domain_size = 1 << log_domain_size;
  int m = 1 << log_diff;
  int domain_idx;
  for (int i = 0; i < log_domain_size - log_diff; i++) {
    int exponent = domain_size / (2 * m);
    for (int k = 0; k < domain_size; k += (2 * m)) {
      for (int j = 0; j < m; j++) {
        if (inverse_domain == 0) {
          domain_idx = exponent * j;
        } else {
          domain_idx = domain_size - (exponent * j);
        };
        if (domain_idx == domain_size) {
          domain_idx = 0;
        };
        blst_scalar_from_fr(&scalar, domain + domain_idx);
        blst_lendian_from_scalar(le_scalar, &scalar);
        blst_p2_mult(&buffer, coefficients + (k + j + m), le_scalar, 256);

        memcpy(&buffer_neg, &buffer, sizeof(blst_p2));
        blst_p2_cneg(&buffer_neg, 1);
        blst_p2_add_or_double(coefficients + (k + j + m),
                              coefficients + (k + j), &buffer_neg);

        blst_p2_add_or_double(coefficients + (k + j), coefficients + (k + j),
                              &buffer);
      }
    }
    m = 2 * m;
  }
}

void bls12_381_polynomial_fft_g2_inplace(blst_p2 *coefficients,
                                                  blst_fr *domain,
                                                  int log_domain_size,
                                                  int log_degree) {
  int domain_size = 1 << log_domain_size;
  int degree_next_pow_of_2 = 1 << log_degree;

  int log_diff = log_domain_size - log_degree;

  if (log_diff > 0) {
    bls12_381_polynomial_reorg_g2_array_coefficients(
        degree_next_pow_of_2, log_degree, coefficients);
    int ratio = 1 << log_diff;
    for (int i = degree_next_pow_of_2 - 1; i >= 0; i--) {
      for (int j = 0; j < ratio; j++) {
        memcpy(coefficients + (i * ratio) + j, coefficients + i,
               sizeof(blst_p2));
      }
    }
  } else {
    bls12_381_polynomial_reorg_g2_array_coefficients(
        domain_size, log_domain_size, coefficients);
  }

  bls12_381_polynomial_fft_g2_inplace_aux(
      coefficients, domain, log_domain_size, log_diff, 0);
}

void bls12_381_polynomial_ifft_g2_inplace(blst_p2 *coefficients,
                                                   blst_fr *domain,
                                                   int log_domain_size) {
  int domain_size = 1 << log_domain_size;
  bls12_381_polynomial_reorg_g2_array_coefficients(
      domain_size, log_domain_size, coefficients);
  bls12_381_polynomial_fft_g2_inplace_aux(coefficients, domain,
                                                   log_domain_size, 0, 1);

  uint64_t n[4] = {domain_size, 0, 0, 0};
  blst_fr inverse_n;
  blst_fr_from_uint64(&inverse_n, n);
  blst_fr_inverse(&inverse_n, &inverse_n);

  blst_scalar scalar;
  byte le_scalar[32];

  blst_scalar_from_fr(&scalar, &inverse_n);
  blst_lendian_from_scalar(le_scalar, &scalar);

  for (int i = 0; i < domain_size; i++) {
    blst_p2_mult(coefficients + i, coefficients + i, le_scalar, 256);
  }
}
