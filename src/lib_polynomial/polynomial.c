#include "polynomial.h"

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
                                int log_domain_size, int inverse_domain) {

  int domain_size = 1 << log_domain_size;
  polynomial_reorg_fr_array_coefficients(coefficients, domain_size,
                                         log_domain_size);

  int m = 1;
  blst_fr tmp;
  int exponent;
  int k;
  int domain_idx;
  for (int i = 0; i < log_domain_size; i++) {
    int exponent = domain_size / (2 * m);
    int k = 0;
    while (k < domain_size) {
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
      k = k + (2 * m);
    }
    m = 2 * m;
  }
}

void polynomial_fft_inplace(blst_fr *coefficients, blst_fr *domain,
                            int log_domain_size) {
  polynomial_fft_inplace_aux(coefficients, domain, log_domain_size, 0);
}

void polynomial_ifft_inplace(blst_fr *coefficients, blst_fr *domain,
                             int log_domain_size) {
  int domain_size = 1 << log_domain_size;
  uint64_t n[4] = {domain_size, 0, 0, 0};
  blst_fr inverse_n;

  polynomial_fft_inplace_aux(coefficients, domain, log_domain_size, 1);

  blst_fr_from_uint64(&inverse_n, n);
  blst_fr_inverse(&inverse_n, &inverse_n);
  for (int i = 0; i < domain_size; i++) {
    blst_fr_mul(coefficients + i, coefficients + i, &inverse_n);
  }
}

int polynomial_degree(blst_fr *arg, const int n) {
  for (int i = n - 1; i >= 0; i--) {
    if (!blst_fr_is_zero(arg + i)) {
      return i;
    }
  }
  return -1;
}

// caml enforce that the first poly the biggest size
bool polynomial_eq(blst_fr *poly_1, blst_fr *poly_2, int size_1, int size_2) {
  bool is_equal = 1;
  for (int i = 0; i < size_2; i++) {
    is_equal = is_equal && blst_fr_is_equal(poly_1 + i, poly_2 + i);
  };
  for (int i = size_2; i < size_1; i++) {
    is_equal = is_equal && blst_fr_is_zero(poly_1 + i);
  };
  return (is_equal);
}

// caml enforce that the first poly the biggest size
void polynomial_add(blst_fr *res, blst_fr *poly_1, blst_fr *poly_2,
                    const int size_1, const int size_2) {
  for (int i = 0; i < size_2; i++) {
    blst_fr_add(res + i, poly_1 + i, poly_2 + i);
  };
  for (int i = size_2; i < size_1; i++) {
    memcpy(res + i, poly_1 + i, sizeof(blst_fr));
  };
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

void polynomial_negate(blst_fr *res, blst_fr *arg, int size) {
  for (int i = 0; i < size; i++) {
    blst_fr_cneg(res + i, arg + i, 1);
  };
}

bool polynomial_is_zero(blst_fr *poly, const int size) {
  bool is_zero = 1;
  for (int i = 0; i < size; i++) {
    is_zero = is_zero && blst_fr_is_zero(poly + i);
  };
  return (is_zero);
}

void polynomial_evaluate(blst_fr *res, const blst_fr *arg, const int size,
                         const blst_fr *scalar) {
  *res = *(arg + size - 1);
  for (int i = size - 2; i >= 0; i--) {
    blst_fr_mul(res, res, scalar);
    blst_fr_add(res, res, arg + i);
  };
}

void polynomial_division_x_z(blst_fr *res, const blst_fr *poly, const int size,
                             blst_fr *z) {
  memcpy(res + size - 2, poly + size - 1, sizeof(blst_fr));

  blst_fr tmp;
  for (int i = size - 2; i > 0; i--) {
    blst_fr_mul(&tmp, z, res + i);
    blst_fr_add(res + i - 1, &tmp, poly + i);
  };
}

void polynomial_division_zs(blst_fr *res, const blst_fr *poly, const int size,
                            const int n) {

  for (int i = 0; i < n; i++) {
    blst_fr_cneg(res + i, poly + i, 1);
  };

  for (int i = n; i < size - 2 * n; i++) {
    blst_fr_sub(res + i, res + i - n, poly + i);
  };

  for (int i = size - 2 * n; i < size - n; i++) {
    memcpy(res + i, poly + i + n, sizeof(blst_fr));
  };
}

void polynomial_mul_zs(blst_fr *res, const blst_fr *poly, const int size,
                       const int n) {
  for (int i = 0; i < n; i++) {
    blst_fr_cneg(res + i, poly + i, 1);
  };

  for (int i = n; i < size; i++) {
    blst_fr_sub(res + i, poly + i - n, poly + i);
  };

  for (int i = size; i < size + n; i++) {
    memcpy(res + i, poly + i - n, sizeof(blst_fr));
  };
}

// caml enforce that the first eval the smallest size
void polynomial_evaluations_add(blst_fr *res, blst_fr *eval_1, blst_fr *eval_2,
                                int size_1, int size_2) {
  // int step1 = size_1 / size_1 = 1;
  int step2 = size_2 / size_1;

  for (int i = 0; i < size_1; i++) {
    blst_fr_add(res + i, eval_1 + i, eval_2 + i * step2);
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

void set_fr_to_one_(blst_fr *x) {
  uint64_t x_uint_64[4];
  x_uint_64[0] = 1lu;
  x_uint_64[1] = 0lu;
  x_uint_64[2] = 0lu;
  x_uint_64[3] = 0lu;
  blst_fr_from_uint64(x, x_uint_64);
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
  set_fr_to_one_(&minus_one); // TODO: replace with the lib function
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