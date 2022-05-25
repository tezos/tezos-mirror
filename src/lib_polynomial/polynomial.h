#ifndef POLYNOMIAL_H
#define POLYNOMIAL_H

#include "blst.h"
#include "blst_misc.h"
#include <stdlib.h>
#include <string.h>

// H: domain_size = polynomial degree
// Implementation with side effect. The FFT will be inplace, i.e. the array is
// going to be modified.
void polynomial_fft_inplace(blst_fr *coefficients, blst_fr *domain,
                            int log_domain_size);

void polynomial_ifft_inplace(blst_fr *coefficients, blst_fr *domain,
                             int log_domain_size);

int polynomial_degree(blst_fr *arg, const int n);

bool polynomial_eq(blst_fr *poly_1, blst_fr *poly_2, int size_1, int size_2);

void polynomial_add(blst_fr *res, blst_fr *arg_1, blst_fr *arg_2, int size_1,
                    int size_2);

void polynomial_sub(blst_fr *res, blst_fr *arg_1, blst_fr *arg_2, int size_1,
                    int size_2);

void polynomial_mul(blst_fr *res, const blst_fr *arg_1, const blst_fr *arg_2,
                    const int size_1, const int size_2);

void polynomial_mul_by_scalar(blst_fr *res, blst_fr *scalar, blst_fr *arg,
                              int size);

void polynomial_negate(blst_fr *res, blst_fr *arg, int size);

bool polynomial_is_zero(blst_fr *poly, int size);

void polynomial_evaluate(blst_fr *res, const blst_fr *arg, const int size,
                         const blst_fr *scalar);

void polynomial_division_x_z(blst_fr *res, const blst_fr *poly, int size,
                             blst_fr *z);

void polynomial_division_zs(blst_fr *res, const blst_fr *poly, int size, int n);

void polynomial_mul_zs(blst_fr *res, const blst_fr *poly, const int size,
                       const int n);

void polynomial_evaluations_add(blst_fr *res, blst_fr *eval_1, blst_fr *eval_2,
                                int size_1, int size_2);

void polynomial_evaluations_rescale(blst_fr *res, blst_fr *eval, int size_res,
                                    int size_eval);

void polynomial_evaluations_mul_arrays(blst_fr *res, blst_fr **evaluations,
                                       int *evaluations_len,
                                       int *composition_gx, byte **powers,
                                       int *powers_numbits, int size_res,
                                       int nb_evals);

void polynomial_evaluations_linear_arrays(blst_fr *res, blst_fr **evaluations,
                                          int *evaluations_len,
                                          blst_fr *linear_coeffs,
                                          int *composition_gx,
                                          blst_fr *add_constant, int size_res,
                                          int nb_evals);

void polynomial_derivative(blst_fr *res, blst_fr *poly, const int size);

#endif
