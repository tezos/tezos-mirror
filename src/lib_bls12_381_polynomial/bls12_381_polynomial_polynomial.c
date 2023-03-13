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

#include "bls12_381_polynomial_polynomial.h"
#include <caml/fail.h>

void bls12_381_polynomial_polynomial_add(blst_fr *res, blst_fr *poly_1,
                                                  blst_fr *poly_2,
                                                  const int size_1,
                                                  const int size_2) {
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

void bls12_381_polynomial_polynomial_sub(blst_fr *res, blst_fr *poly_1,
                                                  blst_fr *poly_2,
                                                  const int size_1,
                                                  const int size_2) {
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

void bls12_381_polynomial_polynomial_mul(blst_fr *res,
                                                  const blst_fr *poly_1,
                                                  const blst_fr *poly_2,
                                                  const int size_1,
                                                  const int size_2) {
  blst_fr tmp;
  for (int i = 0; i < size_1; i++) {
    for (int j = 0; j < size_2; j++) {
      blst_fr_mul(&tmp, poly_1 + i, poly_2 + j);
      blst_fr_add(res + i + j, res + i + j, &tmp);
    }
  }
}

void bls12_381_polynomial_polynomial_mul_by_scalar(blst_fr *res,
                                                            blst_fr *scalar,
                                                            blst_fr *arg,
                                                            int size) {
  for (int i = 0; i < size; i++) {
    blst_fr_mul(res + i, arg + i, scalar);
  };
}

// res is initialized with blst-fr zeros
void bls12_381_polynomial_polynomial_linear(blst_fr *res,
                                                     blst_fr **polynomials,
                                                     int *polynomials_len,
                                                     blst_fr *linear_coeffs,
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
void bls12_381_polynomial_polynomial_linear_with_powers_horner(
    blst_fr *res, blst_fr **polynomials, int *polynomials_len,
    blst_fr *linear_coeff, int nb_polys) {
  int poly_len_j;
  int max_len;

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
void bls12_381_polynomial_polynomial_linear_with_powers_naive(
    blst_fr *res, blst_fr **polynomials, int *polynomials_len,
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

void bls12_381_polynomial_polynomial_linear_with_powers(
    blst_fr *res, blst_fr **polynomials, int *polynomials_len,
    blst_fr *linear_coeff, int nb_polys) {

  int poly_len_i = polynomials_len[0];
  int max_len = poly_len_i;
  int min_len = poly_len_i;

  for (int i = 1; i < nb_polys; i++) {
    poly_len_i = polynomials_len[i];
    max_len = max_len < poly_len_i ? poly_len_i : max_len;
    min_len = poly_len_i < min_len ? poly_len_i : min_len;
  }

  if (max_len != min_len) {
    bls12_381_polynomial_polynomial_linear_with_powers_naive(
        res, polynomials, polynomials_len, linear_coeff, nb_polys);
  } else {
    bls12_381_polynomial_polynomial_linear_with_powers_horner(
        res, polynomials, polynomials_len, linear_coeff, nb_polys);
  }
}

void bls12_381_polynomial_polynomial_negate(blst_fr *res, blst_fr *arg,
                                                     int size) {
  for (int i = 0; i < size; i++) {
    blst_fr_cneg(res + i, arg + i, 1);
  };
}

void bls12_381_polynomial_polynomial_evaluate(blst_fr *res,
                                                       const blst_fr *arg,
                                                       const int size,
                                                       const blst_fr *scalar) {
  *res = *(arg + size - 1);
  for (int i = size - 2; i >= 0; i--) {
    blst_fr_mul(res, res, scalar);
    blst_fr_add(res, res, arg + i);
  };
}

// poly / (X^n + 1), size > n
void bls12_381_polynomial_polynomial_division_xn_one(
    blst_fr *res_q, blst_fr *res_r, const blst_fr *poly, const int size,
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
void bls12_381_polynomial_polynomial_division_xn_minus_one(
    blst_fr *res_q, blst_fr *res_r, const blst_fr *poly, const int size,
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
void bls12_381_polynomial_polynomial_division_xn(
    blst_fr *res_q, blst_fr *res_r, const blst_fr *poly, const int size,
    const int n, const blst_fr *scalar) {

  blst_fr minus_one;
  blst_fr_set_to_one(&minus_one);
  blst_fr_cneg(&minus_one, &minus_one, 1);

  if (blst_fr_is_equal(scalar, &minus_one)) {
    bls12_381_polynomial_polynomial_division_xn_minus_one(
        res_q, res_r, poly, size, n);
    return;
  };

  if (blst_fr_is_one(scalar)) {
    bls12_381_polynomial_polynomial_division_xn_one(res_q, res_r, poly,
                                                             size, n);
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
void bls12_381_polynomial_polynomial_mul_xn_one(blst_fr *res,
                                                         const blst_fr *poly,
                                                         const int size,
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
void bls12_381_polynomial_polynomial_mul_xn_minus_one(
    blst_fr *res, const blst_fr *poly, const int size, const int n) {

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
void bls12_381_polynomial_polynomial_mul_xn(blst_fr *res,
                                                     const blst_fr *poly,
                                                     const int size,
                                                     const int n,
                                                     const blst_fr *scalar) {

  blst_fr minus_one;
  blst_fr_set_to_one(&minus_one);
  blst_fr_cneg(&minus_one, &minus_one, 1);

  if (blst_fr_is_equal(scalar, &minus_one)) {
    bls12_381_polynomial_polynomial_mul_xn_minus_one(res, poly, size,
                                                              n);
    return;
  };

  if (blst_fr_is_one(scalar)) {
    bls12_381_polynomial_polynomial_mul_xn_one(res, poly, size, n);
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

void bls12_381_polynomial_polynomial_evaluations_add(
    blst_fr *res, blst_fr *eval_1, blst_fr *eval_2, int size_1, int size_2) {
  int size_res = size_2 >= size_1 ? size_1 : size_2;
  int step1 = size_1 / size_res;
  int step2 = size_2 / size_res;

  for (int i = 0; i < size_res; i++) {
    blst_fr_add(res + i, eval_1 + i * step1, eval_2 + i * step2);
  };
}

void bls12_381_polynomial_polynomial_evaluations_rescale(
    blst_fr *res, blst_fr *eval, int size_res, int size_eval) {
  int step = size_eval / size_res;

  for (int i = 0; i < size_res; i++) {
    memcpy(res + i, eval + i * step, sizeof(blst_fr));
  };
}

void bls12_381_polynomial_polynomial_evaluations_mul_arrays(
    blst_fr *res, blst_fr **evaluations, int *evaluations_len,
    int *composition_gx, byte **powers, int *powers_numbits, int size_res,
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

void bls12_381_polynomial_polynomial_evaluations_linear_arrays(
    blst_fr *res, blst_fr **evaluations, int *evaluations_len,
    blst_fr *linear_coeffs, int *composition_gx, blst_fr *add_constant,
    int size_res, int nb_evals) {

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

void bls12_381_polynomial_polynomial_derivative(blst_fr *res,
                                                         blst_fr *poly,
                                                         const int size) {
  blst_fr scalar;
  uint64_t n[4] = {0};
  for (int i = 1; i < size; i++) {
    n[0] = i;
    blst_fr_from_uint64(&scalar, n);
    blst_fr_mul(res + (i - 1), &scalar, poly + i);
  };
}
