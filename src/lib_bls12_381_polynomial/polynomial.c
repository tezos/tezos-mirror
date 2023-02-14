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

#include "polynomial.h"
#include <caml/fail.h>

// IMPROVEME: can be improve it with lookups?
// FIXME: bitreverse is also exported by ocaml-bls12-381. Must be removed.
int polynomial_bitreverse_(int n, int l) {
  int r = 0;
  while (l-- > 0) {
    r = (r << 1) | (n & 1);
    n = n >> 1;
  }
  return r;
}

void polynomial_reorg_fr_array_coefficients(blst_fr *coefficients, int n,
                                            int logn) {
  blst_fr tmp;
  for (int i = 0; i < n; i++) {
    int reverse_i = polynomial_bitreverse_(i, logn);
    if (i < reverse_i) {
      memcpy(&tmp, coefficients + i, sizeof(blst_fr));
      memcpy(coefficients + i, coefficients + reverse_i, sizeof(blst_fr));
      memcpy(coefficients + reverse_i, &tmp, sizeof(blst_fr));
    }
  }
}

void polynomial_fft_inplace_aux(blst_fr *coefficients, blst_fr *domain,
                                int log_domain_size, int log_diff,
                                int inverse_domain) {
  int domain_size = 1 << log_domain_size;
  int m = 1 << log_diff;
  blst_fr tmp;
  int exponent;
  int k;
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

void polynomial_fft_inplace(blst_fr *coefficients, blst_fr *domain,
                            int log_domain_size, int degree, int log_degree) {
  int domain_size = 1 << log_domain_size;
  int degree_next_pow_of_2 = 1 << log_degree;
  int log_diff;

  if (domain_size > degree) {
    polynomial_reorg_fr_array_coefficients(coefficients, degree_next_pow_of_2,
                                           log_degree);
    int ratio = domain_size / degree_next_pow_of_2;
    blst_fr *cp = malloc(sizeof(blst_fr) * degree_next_pow_of_2);
    if (cp == NULL) {
      caml_raise_out_of_memory();
    }
    memcpy(cp, coefficients, sizeof(blst_fr) * degree_next_pow_of_2);
    for (int i = 0; i < degree_next_pow_of_2; i++) {
      for (int j = 0; j < ratio; j++) {
        memcpy(coefficients + (i * ratio) + j, cp + i, sizeof(blst_fr));
      }
    }
    log_diff = log_domain_size - log_degree;
    free(cp);
  } else {
    polynomial_reorg_fr_array_coefficients(coefficients, domain_size,
                                           log_domain_size);
    log_diff = 0;
  }
  polynomial_fft_inplace_aux(coefficients, domain, log_domain_size, log_diff,
                             0);
}

void polynomial_ifft_inplace(blst_fr *coefficients, blst_fr *domain,
                             int log_domain_size) {
  int domain_size = 1 << log_domain_size;
  uint64_t n[4] = {domain_size, 0, 0, 0};
  blst_fr inverse_n;

  polynomial_reorg_fr_array_coefficients(coefficients, domain_size,
                                         log_domain_size);
  polynomial_fft_inplace_aux(coefficients, domain, log_domain_size, 0, 1);

  blst_fr_from_uint64(&inverse_n, n);
  blst_fr_inverse(&inverse_n, &inverse_n);
  for (int i = 0; i < domain_size; i++) {
    blst_fr_mul(coefficients + i, coefficients + i, &inverse_n);
  }
}

void polynomial_dft_inplace(blst_fr *coefficients, blst_fr *domain,
                            blst_fr *buffer, int length, int inverse) {
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

int polynomial_is_power_of_two(int n) { return (n & (n - 1)) == 0; }

int polynomial_log2(int n) {
  int l = 0;
  while (n >>= 1) {
    ++l;
  }
  return l;
}

void polynomial_transpose(blst_fr *rows, blst_fr *columns, int n1, int n2) {
  for (int i = 0; i < n1; i++) {
    for (int j = 0; j < n2; j++) {
      rows[j * n1 + i] = columns[j + i * n2];
    }
  }
}

int polynomial_max(int a, int b) { return a >= b ? a : b; }

void polynomial_fft_round(blst_fr *buffer, blst_fr *domain1, blst_fr *domain2,
                          int length1, int length2, blst_fr *buffer_dft,
                          int inverse) {
  if (polynomial_is_power_of_two(length2)) {
    int length2_log = polynomial_log2(length2);
    if (inverse) {
      for (int i = 0; i < length1; i++) {
        polynomial_ifft_inplace(buffer + (i * length2), domain2, length2_log);
      }

    } else {
      for (int i = 0; i < length1; i++) {
        polynomial_fft_inplace(buffer + (i * length2), domain2, length2_log,
                               length2, length2_log);
      }
    }
  } else {
    for (int i = 0; i < length1; i++) {
      polynomial_dft_inplace(buffer + (i * length2), domain2, buffer_dft,
                             length2, inverse);
    }
  }
}

// The buffer must have size at least 2 * |domain1| * |domain2|
void polynomial_prime_factor_algorithm_fft(blst_fr *coefficients,
                                           blst_fr *domain1, blst_fr *domain2,
                                           int length1, int length2,
                                           int inverse) {
  int dft_length =
      (!polynomial_is_power_of_two(length1) &&
       !polynomial_is_power_of_two(length2))
          ? polynomial_max(length1, length2)
          : (polynomial_is_power_of_two(length1) ? length2 : length1);
  // We allocate buffer_dft on the stack because dft_length is <= 2^10 at most in our
  // use case
  blst_fr buffer_dft[dft_length];
  int length = length1 * length2;

  blst_fr *buffer = malloc(2 * length * sizeof(blst_fr));
  if (buffer == NULL) {
    caml_raise_out_of_memory();
  }

  for (int i = 0; i < length; i++) {
    buffer[(i % length1) * length2 + (i % length2)] = coefficients[i];
  }

  polynomial_fft_round(buffer, domain1, domain2, length1, length2, buffer_dft,
                       inverse);

  blst_fr *new_buffer = buffer + length;

  polynomial_transpose(new_buffer, buffer, length1, length2);

  polynomial_fft_round(new_buffer, domain2, domain1, length2, length1,
                       buffer_dft, inverse);

  for (int i = 0; i < length1; i++) {
    for (int j = 0; j < length2; j++) {
      coefficients[(length1 * j + length2 * i) % length] =
          new_buffer[j * length1 + i];
    }
  }

  free(buffer);
}

void polynomial_add(blst_fr *res, blst_fr *poly_1, blst_fr *poly_2,
                    const int size_1, const int size_2) {
  if (size_1 >= size_2) {
    for (int i = 0; i < size_2; i++) {
      blst_fr_add(res + i, poly_1 + i, poly_2 + i);
    };
    for (int i = size_2; i < size_1; i++) {
      memcpy(res + i, poly_1 + i, sizeof(blst_fr));
    };
  } else {
    for (int i = 0; i < size_1; i++) {
      blst_fr_add(res + i, poly_1 + i, poly_2 + i);
    };
    for (int i = size_1; i < size_2; i++) {
      memcpy(res + i, poly_2 + i, sizeof(blst_fr));
    };
  }
}

void polynomial_sub(blst_fr *res, blst_fr *poly_1, blst_fr *poly_2,
                    const int size_1, const int size_2) {
  if (size_1 >= size_2) {
    for (int i = 0; i < size_2; i++) {
      blst_fr_sub(res + i, poly_1 + i, poly_2 + i);
    };
    for (int i = size_2; i < size_1; i++) {
      memcpy(res + i, poly_1 + i, sizeof(blst_fr));
    };
  } else {
    for (int i = 0; i < size_1; i++) {
      blst_fr_sub(res + i, poly_1 + i, poly_2 + i);
    };
    for (int i = size_1; i < size_2; i++) {
      blst_fr_cneg(res + i, poly_2 + i, 1);
    };
  }
}

void polynomial_mul(blst_fr *res, const blst_fr *poly_1, const blst_fr *poly_2,
                    const int size_1, const int size_2) {
  blst_fr tmp;
  for (int i = 0; i < size_1; i++) {
    for (int j = 0; j < size_2; j++) {
      blst_fr_mul(&tmp, poly_1 + i, poly_2 + j);
      blst_fr_add(res + i + j, res + i + j, &tmp);
    }
  }
}

void polynomial_mul_by_scalar(blst_fr *res, blst_fr *scalar, blst_fr *arg,
                              int size) {
  for (int i = 0; i < size; i++) {
    blst_fr_mul(res + i, arg + i, scalar);
  };
}

// res is initialized with blst-fr zeros
void polynomial_linear(blst_fr *res, blst_fr **polynomials,
                       int *polynomials_len, blst_fr *linear_coeffs,
                       int nb_polys) {
  int poly_len_j;
  blst_fr tmp;

  poly_len_j = polynomials_len[0];
  for (int i = 0; i < poly_len_j; i++) {
    blst_fr_mul(res + i, polynomials[0] + i, linear_coeffs);
  }

  for (int j = 1; j < nb_polys; j++) {
    poly_len_j = polynomials_len[j];
    for (int i = 0; i < poly_len_j; i++) {
      blst_fr_mul(&tmp, polynomials[j] + i, linear_coeffs + j);
      blst_fr_add(res + i, res + i, &tmp);
    }
  }
}

// res is initialized with blst-fr zeros
// p_0 + s * p_1 + s^2 * p_2 + ... + s^n * p_n =
// (..((p_n * s) + p_{n-1}) * s + .. + p_1) * s + p_0
void polynomial_linear_with_powers_horner(blst_fr *res, blst_fr **polynomials,
                                          int *polynomials_len,
                                          blst_fr *linear_coeff, int nb_polys) {
  int poly_len_j;
  int max_len;
  blst_fr tmp;

  poly_len_j = polynomials_len[nb_polys - 1];
  max_len = poly_len_j;
  for (int i = 0; i < poly_len_j; i++) {
    memcpy(res + i, polynomials[nb_polys - 1] + i, sizeof(blst_fr));
  }

  for (int j = nb_polys - 2; j >= 0; j--) {
    for (int i = 0; i < max_len; i++) {
      blst_fr_mul(res + i, res + i, linear_coeff);
    }
    poly_len_j = polynomials_len[j];
    max_len = max_len < poly_len_j ? poly_len_j : max_len;
    for (int i = 0; i < poly_len_j; i++) {
      blst_fr_add(res + i, res + i, polynomials[j] + i);
    }
  }
}

// res is initialized with blst-fr zeros
// p_0 + s * p_1 + s^2 * p_2 + ... + s^n * p_n
void polynomial_linear_with_powers_naive(blst_fr *res, blst_fr **polynomials,
                                         int *polynomials_len,
                                         blst_fr *linear_coeff, int nb_polys) {
  int poly_len_j;
  blst_fr tmp;
  blst_fr coeff_j;

  poly_len_j = polynomials_len[0];
  for (int i = 0; i < poly_len_j; i++) {
    memcpy(res + i, polynomials[0] + i, sizeof(blst_fr));
  }

  memcpy(&coeff_j, linear_coeff, sizeof(blst_fr));
  for (int j = 1; j < nb_polys; j++) {
    poly_len_j = polynomials_len[j];
    for (int i = 0; i < poly_len_j; i++) {
      blst_fr_mul(&tmp, polynomials[j] + i, &coeff_j);
      blst_fr_add(res + i, res + i, &tmp);
    }
    blst_fr_mul(&coeff_j, &coeff_j, linear_coeff);
  }
}

void polynomial_linear_with_powers(blst_fr *res, blst_fr **polynomials,
                                   int *polynomials_len, blst_fr *linear_coeff,
                                   int nb_polys) {

  int poly_len_i = polynomials_len[0];
  int max_len = poly_len_i;
  int min_len = poly_len_i;

  for (int i = 1; i < nb_polys; i++) {
    poly_len_i = polynomials_len[i];
    max_len = max_len < poly_len_i ? poly_len_i : max_len;
    min_len = poly_len_i < min_len ? poly_len_i : min_len;
  }

  if (max_len != min_len) {
    polynomial_linear_with_powers_naive(res, polynomials, polynomials_len,
                                        linear_coeff, nb_polys);
  } else {
    polynomial_linear_with_powers_horner(res, polynomials, polynomials_len,
                                         linear_coeff, nb_polys);
  }
}

void polynomial_negate(blst_fr *res, blst_fr *arg, int size) {
  for (int i = 0; i < size; i++) {
    blst_fr_cneg(res + i, arg + i, 1);
  };
}

void polynomial_evaluate(blst_fr *res, const blst_fr *arg, const int size,
                         const blst_fr *scalar) {
  *res = *(arg + size - 1);
  for (int i = size - 2; i >= 0; i--) {
    blst_fr_mul(res, res, scalar);
    blst_fr_add(res, res, arg + i);
  };
}

// poly / (X^n + 1), size > n
void polynomial_division_xn_one(blst_fr *res_q, blst_fr *res_r,
                                const blst_fr *poly, const int size,
                                const int n) {

  if (size >= 2 * n) {
    for (int i = size - 2 * n; i < size - n; i++) {
      memcpy(res_q + i, poly + i + n, sizeof(blst_fr));
    };

    for (int i = size - 2 * n - 1; i >= 0; i--) {
      blst_fr_sub(res_q + i, poly + i + n, res_q + i + n);
    };

    // remainder
    for (int i = 0; i < n; i++) {
      blst_fr_sub(res_r + i, poly + i, res_q + i);
    };
  } else {
    for (int i = 0; i < size - n; i++) {
      memcpy(res_q + i, poly + i + n, sizeof(blst_fr));
    };

    // remainder
    for (int i = 0; i < size - n; i++) {
      blst_fr_sub(res_r + i, poly + i, res_q + i);
    };

    for (int i = size - n; i < n; i++) {
      memcpy(res_r + i, poly + i, sizeof(blst_fr));
    };
  }
}

// poly / (X^n - 1), size > n
void polynomial_division_xn_minus_one(blst_fr *res_q, blst_fr *res_r,
                                      const blst_fr *poly, const int size,
                                      const int n) {

  if (size >= 2 * n) {
    for (int i = size - 2 * n; i < size - n; i++) {
      memcpy(res_q + i, poly + i + n, sizeof(blst_fr));
    };

    for (int i = size - 2 * n - 1; i >= 0; i--) {
      blst_fr_add(res_q + i, poly + i + n, res_q + i + n);
    };

    // remainder
    for (int i = 0; i < n; i++) {
      blst_fr_add(res_r + i, poly + i, res_q + i);
    };
  } else {
    for (int i = 0; i < size - n; i++) {
      memcpy(res_q + i, poly + i + n, sizeof(blst_fr));
    };

    // remainder
    for (int i = 0; i < size - n; i++) {
      blst_fr_add(res_r + i, poly + i, res_q + i);
    };

    for (int i = size - n; i < n; i++) {
      memcpy(res_r + i, poly + i, sizeof(blst_fr));
    };
  }
}

// poly / (X^n + scalar), size > n
void polynomial_division_xn(blst_fr *res_q, blst_fr *res_r, const blst_fr *poly,
                            const int size, const int n,
                            const blst_fr *scalar) {

  blst_fr minus_one;
  blst_fr_set_to_one(&minus_one);
  blst_fr_cneg(&minus_one, &minus_one, 1);

  if (blst_fr_is_equal(scalar, &minus_one)) {
    polynomial_division_xn_minus_one(res_q, res_r, poly, size, n);
    return;
  };

  if (blst_fr_is_one(scalar)) {
    polynomial_division_xn_one(res_q, res_r, poly, size, n);
    return;
  };

  blst_fr tmp;
  if (size >= 2 * n) {
    for (int i = size - 2 * n; i < size - n; i++) {
      memcpy(res_q + i, poly + i + n, sizeof(blst_fr));
    };

    for (int i = size - 2 * n - 1; i >= 0; i--) {
      blst_fr_mul(&tmp, scalar, res_q + i + n);
      blst_fr_sub(res_q + i, poly + i + n, &tmp);
    };

    // remainder
    for (int i = 0; i < n; i++) {
      blst_fr_mul(&tmp, scalar, res_q + i);
      blst_fr_sub(res_r + i, poly + i, &tmp);
    };
  } else {
    for (int i = 0; i < size - n; i++) {
      memcpy(res_q + i, poly + i + n, sizeof(blst_fr));
    };

    // remainder
    for (int i = 0; i < size - n; i++) {
      blst_fr_mul(&tmp, scalar, res_q + i);
      blst_fr_sub(res_r + i, poly + i, &tmp);
    };

    for (int i = size - n; i < n; i++) {
      memcpy(res_r + i, poly + i, sizeof(blst_fr));
    };
  }
}

// poly * (X^n + 1)
// res is initialized with blst-fr zeros
void polynomial_mul_xn_one(blst_fr *res, const blst_fr *poly, const int size,
                           const int n) {

  if (size >= n) {
    for (int i = 0; i < n; i++) {
      memcpy(res + i, poly + i, sizeof(blst_fr));
    };

    for (int i = n; i < size; i++) {
      blst_fr_add(res + i, poly + i - n, poly + i);
    };

    for (int i = size; i < size + n; i++) {
      memcpy(res + i, poly + i - n, sizeof(blst_fr));
    };
  } else {
    for (int i = 0; i < size; i++) {
      memcpy(res + i, poly + i, sizeof(blst_fr));
    };
    // res[i] = 0 for i is in [size; n)
    for (int i = n; i < size + n; i++) {
      memcpy(res + i, poly + i - n, sizeof(blst_fr));
    };
  }
}

// poly * (X^n - 1)
// res is initialized with blst-fr zeros
void polynomial_mul_xn_minus_one(blst_fr *res, const blst_fr *poly,
                                 const int size, const int n) {

  if (size >= n) {
    for (int i = 0; i < n; i++) {
      blst_fr_cneg(res + i, poly + i, 1);
    };

    for (int i = n; i < size; i++) {
      blst_fr_sub(res + i, poly + i - n, poly + i);
    };

    for (int i = size; i < size + n; i++) {
      memcpy(res + i, poly + i - n, sizeof(blst_fr));
    };
  } else {
    for (int i = 0; i < size; i++) {
      blst_fr_cneg(res + i, poly + i, 1);
    };
    // res[i] = 0 for i is in [size; n)
    for (int i = n; i < size + n; i++) {
      memcpy(res + i, poly + i - n, sizeof(blst_fr));
    };
  }
}

// poly * (X^n + scalar)
// res is initialized with blst-fr zeros
void polynomial_mul_xn(blst_fr *res, const blst_fr *poly, const int size,
                       const int n, const blst_fr *scalar) {

  blst_fr minus_one;
  blst_fr_set_to_one(&minus_one);
  blst_fr_cneg(&minus_one, &minus_one, 1);

  if (blst_fr_is_equal(scalar, &minus_one)) {
    polynomial_mul_xn_minus_one(res, poly, size, n);
    return;
  };

  if (blst_fr_is_one(scalar)) {
    polynomial_mul_xn_one(res, poly, size, n);
    return;
  };

  blst_fr tmp;
  if (size >= n) {
    for (int i = 0; i < n; i++) {
      blst_fr_mul(res + i, scalar, poly + i);
    };

    for (int i = n; i < size; i++) {
      blst_fr_mul(&tmp, scalar, poly + i);
      blst_fr_add(res + i, poly + i - n, &tmp);
    };

    for (int i = size; i < size + n; i++) {
      memcpy(res + i, poly + i - n, sizeof(blst_fr));
    };
  } else {
    for (int i = 0; i < size; i++) {
      blst_fr_mul(res + i, scalar, poly + i);
    };
    // res[i] = 0 for i is in [size; n)
    for (int i = n; i < size + n; i++) {
      memcpy(res + i, poly + i - n, sizeof(blst_fr));
    };
  }
}

void polynomial_evaluations_add(blst_fr *res, blst_fr *eval_1, blst_fr *eval_2,
                                int size_1, int size_2) {
  int size_res = size_2 >= size_1 ? size_1 : size_2;
  int step1 = size_1 / size_res;
  int step2 = size_2 / size_res;

  for (int i = 0; i < size_res; i++) {
    blst_fr_add(res + i, eval_1 + i * step1, eval_2 + i * step2);
  };
}

void polynomial_evaluations_rescale(blst_fr *res, blst_fr *eval, int size_res,
                                    int size_eval) {
  int step = size_eval / size_res;

  for (int i = 0; i < size_res; i++) {
    memcpy(res + i, eval + i * step, sizeof(blst_fr));
  };
}

void polynomial_evaluations_mul_arrays(blst_fr *res, blst_fr **evaluations,
                                       int *evaluations_len,
                                       int *composition_gx, byte **powers,
                                       int *powers_numbits, int size_res,
                                       int nb_evals) {
  int ind;
  int eval_len_j;
  int step_j;
  int composition_gx_j;
  int exp_pow_len_j;
  blst_fr tmp;

  eval_len_j = evaluations_len[0];
  step_j = eval_len_j / size_res;
  composition_gx_j = composition_gx[0];
  exp_pow_len_j = powers_numbits[0];

  for (int i = 0; i < size_res; i++) {
    ind = (i + composition_gx_j) * step_j % eval_len_j;
    blst_fr_pow(res + i, evaluations[0] + ind, powers[0], exp_pow_len_j);
  };

  for (int j = 1; j < nb_evals; j++) {
    eval_len_j = evaluations_len[j];
    step_j = eval_len_j / size_res;
    composition_gx_j = composition_gx[j];
    exp_pow_len_j = powers_numbits[j];

    for (int i = 0; i < size_res; i++) {
      ind = (i + composition_gx_j) * step_j % eval_len_j;
      blst_fr_pow(&tmp, evaluations[j] + ind, powers[j], exp_pow_len_j);
      blst_fr_mul(res + i, res + i, &tmp);
    }
  }
}

void polynomial_evaluations_linear_arrays(blst_fr *res, blst_fr **evaluations,
                                          int *evaluations_len,
                                          blst_fr *linear_coeffs,
                                          int *composition_gx,
                                          blst_fr *add_constant, int size_res,
                                          int nb_evals) {

  int ind;
  int eval_len_j;
  int step_j;
  int composition_gx_j;
  blst_fr tmp;

  blst_fr minus_one;
  blst_fr_set_to_one(&minus_one);
  blst_fr_cneg(&minus_one, &minus_one, 1);

  for (int i = 0; i < size_res; i++) {
    memcpy(res + i, add_constant, sizeof(blst_fr));
  };

  for (int j = 0; j < nb_evals; j++) {
    eval_len_j = evaluations_len[j];
    step_j = eval_len_j / size_res;
    composition_gx_j = composition_gx[j];

    if (blst_fr_is_one(linear_coeffs + j)) {
      for (int i = 0; i < size_res; i++) {
        ind = (i + composition_gx_j) * step_j % eval_len_j;
        blst_fr_add(res + i, res + i, evaluations[j] + ind);
      }
    } else {
      if (blst_fr_is_equal(linear_coeffs + j, &minus_one)) {
        for (int i = 0; i < size_res; i++) {
          ind = (i + composition_gx_j) * step_j % eval_len_j;
          blst_fr_sub(res + i, res + i, evaluations[j] + ind);
        }
      } else {
        for (int i = 0; i < size_res; i++) {
          ind = (i + composition_gx_j) * step_j % eval_len_j;
          blst_fr_mul(&tmp, linear_coeffs + j, evaluations[j] + ind);
          blst_fr_add(res + i, res + i, &tmp);
        }
      }
    }
  }
}

void polynomial_derivative(blst_fr *res, blst_fr *poly, const int size) {
  blst_fr scalar;
  uint64_t n[4] = {0};
  for (int i = 1; i < size; i++) {
    n[0] = i;
    blst_fr_from_uint64(&scalar, n);
    blst_fr_mul(res + (i - 1), &scalar, poly + i);
  };
}
