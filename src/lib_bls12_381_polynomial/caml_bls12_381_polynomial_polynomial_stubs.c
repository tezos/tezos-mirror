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
#include "caml_bls12_381_stubs.h"
#include "ocaml_integers.h"
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#define Blst_fr_array_val(v) ((blst_fr *)Caml_ba_data_val(v))

CAMLprim value caml_bls12_381_polynomial_polynomial_carray_get_stubs(
    value elt, value carray, value idx, value size) {
  CAMLparam4(elt, carray, idx, size);
  void *elt_c = Data_custom_val(elt);
  char *carray_c = Caml_ba_data_val(carray);

  int idx_c = Int_val(idx);
  int size_c = Int_val(size);

  memcpy(elt_c, carray_c + (idx_c * size_c), size_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_polynomial_polynomial_carray_set_stubs(
    value carray, value elt, value idx, value size) {
  CAMLparam4(elt, carray, idx, size);
  void *elt_c = Data_custom_val(elt);
  char *carray_c = Caml_ba_data_val(carray);

  int idx_c = Int_val(idx);
  int size_c = Int_val(size);

  memcpy(carray_c + (idx_c * size_c), elt_c, size_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_polynomial_polynomial_memset_zero_stubs(
    value carray, value n) {
  CAMLparam2(carray, n);
  int n_c = Int_val(n);
  void *carray_c = Caml_ba_data_val(carray);
  memset(carray_c, 0, n_c);
  CAMLreturn(Val_unit);
}

CAMLprim value
caml_bls12_381_polynomial_polynomial_compute_domain_stubs(
    value buffer, value n, value root_of_unity) {
  CAMLparam3(buffer, n, root_of_unity);
  blst_fr *root_of_unity_c = Blst_fr_val(root_of_unity);
  int n_c = Int_val(n);
  blst_fr *buffer_c = Blst_fr_array_val(buffer);

  blst_fr_set_to_one(buffer_c);
  memcpy(buffer_c + 1, root_of_unity_c, sizeof(blst_fr));

  for (int i = 1; i < n_c / 2; i++) {
    blst_fr_sqr(buffer_c + 2 * i, buffer_c + i);
    blst_fr_mul(buffer_c + 2 * i + 1, buffer_c + 2 * i, root_of_unity_c);
  }

  if (n_c % 2 != 0) {
    blst_fr_mul(buffer_c + n_c - 1, buffer_c + n_c - 2, root_of_unity_c);
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_polynomial_polynomial_of_sparse_stubs(
    value polynomial, value coefficients, value nb_coefficients) {
  CAMLparam3(polynomial, coefficients, nb_coefficients);

  blst_fr *polynomial_c = Blst_fr_array_val(polynomial);
  int nb_coefficients_c = Int_val(nb_coefficients);

  value idx_i;
  blst_fr *c;
  int d;
  for (int i = 0; i < nb_coefficients_c; i++) {
    idx_i = Field(coefficients, i);
    c = Blst_fr_val(Field(idx_i, 0));
    d = Int_val(Field(idx_i, 1));
    memcpy(polynomial_c + d, c, sizeof(blst_fr));
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_polynomial_polynomial_add_stubs(
    value res, value arg_1, value arg_2, value size_1, value size_2) {
  CAMLparam5(res, arg_1, arg_2, size_1, size_2);
  blst_fr *arg_1_c = Blst_fr_array_val(arg_1);
  blst_fr *arg_2_c = Blst_fr_array_val(arg_2);
  blst_fr *res_c = Blst_fr_array_val(res);
  int size_1_c = Int_val(size_1);
  int size_2_c = Int_val(size_2);

  bls12_381_polynomial_polynomial_add(res_c, arg_1_c, arg_2_c,
                                               size_1_c, size_2_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_polynomial_polynomial_sub_stubs(
    value res, value arg_1, value arg_2, value size_1, value size_2) {
  CAMLparam5(res, arg_1, arg_2, size_1, size_2);
  blst_fr *arg_1_c = Blst_fr_array_val(arg_1);
  blst_fr *arg_2_c = Blst_fr_array_val(arg_2);
  blst_fr *res_c = Blst_fr_array_val(res);
  int size_1_c = Int_val(size_1);
  int size_2_c = Int_val(size_2);

  bls12_381_polynomial_polynomial_sub(res_c, arg_1_c, arg_2_c,
                                               size_1_c, size_2_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_polynomial_polynomial_mul_stubs(
    value res, value arg_1, value arg_2, value size_1, value size_2) {
  CAMLparam5(res, arg_1, arg_2, size_1, size_2);
  blst_fr *arg_1_c = Blst_fr_array_val(arg_1);
  blst_fr *arg_2_c = Blst_fr_array_val(arg_2);
  blst_fr *res_c = Blst_fr_array_val(res);
  int size_1_c = Int_val(size_1);
  int size_2_c = Int_val(size_2);

  bls12_381_polynomial_polynomial_mul(res_c, arg_1_c, arg_2_c,
                                               size_1_c, size_2_c);
  CAMLreturn(Val_unit);
}

CAMLprim value
caml_bls12_381_polynomial_polynomial_mul_by_scalar_stubs(value res,
                                                                  value scalar,
                                                                  value arg,
                                                                  value size) {
  CAMLparam4(res, scalar, arg, size);
  blst_fr *res_c = Blst_fr_array_val(res);
  blst_fr *scalar_c = Blst_fr_val(scalar);
  blst_fr *arg_c = Blst_fr_array_val(arg);
  int size_c = Int_val(size);

  bls12_381_polynomial_polynomial_mul_by_scalar(res_c, scalar_c, arg_c,
                                                         size_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_polynomial_polynomial_linear_stubs(
    value res, value poly_polylen_coeff, value nb_polys) {
  CAMLparam3(res, poly_polylen_coeff, nb_polys);
  blst_fr *res_c = Blst_fr_array_val(res);
  int nb_polys_c = Int_val(nb_polys);

  blst_fr *polys_c[nb_polys_c];
  int polys_len_c[nb_polys_c];
  blst_fr linear_coeffs_c[nb_polys_c];

  value idx_i;
  blst_fr *c;
  for (int i = 0; i < nb_polys_c; i++) {
    idx_i = Field(poly_polylen_coeff, i);
    polys_c[i] = Blst_fr_array_val(Field(idx_i, 0));
    polys_len_c[i] = Int_val(Field(idx_i, 1));
    c = Blst_fr_val(Field(idx_i, 2));
    memcpy(linear_coeffs_c + i, c, sizeof(blst_fr));
  }

  bls12_381_polynomial_polynomial_linear(res_c, polys_c, polys_len_c,
                                                  linear_coeffs_c, nb_polys_c);
  CAMLreturn(Val_unit);
}

CAMLprim value
caml_bls12_381_polynomial_polynomial_linear_with_powers_stubs(
    value res, value coeff, value poly_polylen, value nb_polys) {
  CAMLparam4(res, coeff, poly_polylen, nb_polys);
  blst_fr *res_c = Blst_fr_array_val(res);
  int nb_polys_c = Int_val(nb_polys);
  blst_fr *coeff_c = Blst_fr_val(coeff);

  blst_fr *polys_c[nb_polys_c];
  int polys_len_c[nb_polys_c];

  value idx_i;
  for (int i = 0; i < nb_polys_c; i++) {
    idx_i = Field(poly_polylen, i);
    polys_c[i] = Blst_fr_array_val(Field(idx_i, 0));
    polys_len_c[i] = Int_val(Field(idx_i, 1));
  }

  bls12_381_polynomial_polynomial_linear_with_powers(
      res_c, polys_c, polys_len_c, coeff_c, nb_polys_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_polynomial_polynomial_negate_stubs(
    value res, value arg, value size) {
  CAMLparam3(res, arg, size);
  blst_fr *res_c = Blst_fr_array_val(res);
  blst_fr *arg_c = Blst_fr_array_val(arg);
  int size_c = Int_val(size);

  bls12_381_polynomial_polynomial_negate(res_c, arg_c, size_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_polynomial_polynomial_evaluate_stubs(
    value res, value arg, value size, value scalar) {
  CAMLparam4(res, arg, size, scalar);
  blst_fr *res_c = Blst_fr_val(res);
  blst_fr *arg_c = Blst_fr_array_val(arg);
  int size_c = Int_val(size);
  blst_fr *scalar_c = Blst_fr_val(scalar);

  bls12_381_polynomial_polynomial_evaluate(res_c, arg_c, size_c,
                                                    scalar_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_polynomial_polynomial_division_xn_stubs(
    value res_q, value res_r, value poly, value size, value n_and_scalar) {
  CAMLparam5(res_q, res_r, poly, size, n_and_scalar);
  blst_fr *poly_c = Blst_fr_array_val(poly);
  blst_fr *res_q_c = Blst_fr_array_val(res_q);
  blst_fr *res_r_c = Blst_fr_array_val(res_r);
  int size_c = Int_val(size);
  int n_c = Int_val(Field(n_and_scalar, 0));
  blst_fr *scalar_c = Blst_fr_val(Field(n_and_scalar, 1));
  bls12_381_polynomial_polynomial_division_xn(res_q_c, res_r_c, poly_c,
                                                       size_c, n_c, scalar_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_polynomial_polynomial_mul_xn_stubs(
    value res, value poly, value size, value n, value scalar) {
  CAMLparam4(res, poly, size, n);
  blst_fr *poly_c = Blst_fr_array_val(poly);
  blst_fr *res_c = Blst_fr_array_val(res);
  int size_c = Int_val(size);
  int n_c = Int_val(n);
  blst_fr *scalar_c = Blst_fr_val(scalar);
  bls12_381_polynomial_polynomial_mul_xn(res_c, poly_c, size_c, n_c,
                                                  scalar_c);
  CAMLreturn(Val_unit);
}

// Bindings for evaluations.ml

CAMLprim value
caml_bls12_381_polynomial_polynomial_evaluations_add_stubs(
    value res, value eval_1, value eval_2, value size_1, value size_2) {
  CAMLparam5(res, eval_1, eval_2, size_1, size_2);
  blst_fr *res_c = Blst_fr_array_val(res);
  blst_fr *eval_1_c = Blst_fr_array_val(eval_1);
  blst_fr *eval_2_c = Blst_fr_array_val(eval_2);
  int size_1_c = Int_val(size_1);
  int size_2_c = Int_val(size_2);

  bls12_381_polynomial_polynomial_evaluations_add(
      res_c, eval_1_c, eval_2_c, size_1_c, size_2_c);
  CAMLreturn(Val_unit);
}

CAMLprim value
caml_bls12_381_polynomial_polynomial_evaluations_rescale_stubs(
    value res, value eval, value size_res, value size_eval) {
  CAMLparam4(res, eval, size_res, size_eval);
  blst_fr *res_c = Blst_fr_array_val(res);
  blst_fr *eval_c = Blst_fr_array_val(eval);
  int size_res_c = Int_val(size_res);
  int size_eval_c = Int_val(size_eval);

  bls12_381_polynomial_polynomial_evaluations_rescale(
      res_c, eval_c, size_res_c, size_eval_c);
  CAMLreturn(Val_unit);
}

CAMLprim value
caml_bls12_381_polynomial_polynomial_evaluations_mul_arrays_stubs(
    value res, value eval_evallen_comp_power_powlen, value size_res,
    value nb_evals) {
  CAMLparam4(res, eval_evallen_comp_power_powlen, size_res, nb_evals);
  blst_fr *res_c = Blst_fr_array_val(res);
  int size_res_c = Int_val(size_res);
  int nb_evals_c = Int_val(nb_evals);

  blst_fr *evaluations_c[nb_evals_c];
  int evaluations_len_c[nb_evals_c];
  int composition_gx_c[nb_evals_c];
  byte *powers_c[nb_evals_c];
  int powers_len_c[nb_evals_c];

  value idx_i;
  for (int i = 0; i < nb_evals_c; i++) {
    idx_i = Field(eval_evallen_comp_power_powlen, i);
    evaluations_c[i] = Blst_fr_array_val(Field(idx_i, 0));
    evaluations_len_c[i] = Int_val(Field(idx_i, 1));
    composition_gx_c[i] = Int_val(Field(idx_i, 2));
    powers_c[i] = Bytes_val(Field(idx_i, 3));
    powers_len_c[i] = Int_val(Field(idx_i, 4));
  }

  bls12_381_polynomial_polynomial_evaluations_mul_arrays(
      res_c, evaluations_c, evaluations_len_c, composition_gx_c, powers_c,
      powers_len_c, size_res_c, nb_evals_c);
  CAMLreturn(Val_unit);
}

CAMLprim value
caml_bls12_381_polynomial_polynomial_evaluations_linear_arrays_stubs(
    value res, value eval_evallen_coeff_comp, value add_constant,
    value size_res, value nb_evals) {
  CAMLparam5(res, eval_evallen_coeff_comp, add_constant, size_res, nb_evals);
  blst_fr *res_c = Blst_fr_array_val(res);
  blst_fr *add_constant_c = Blst_fr_val(add_constant);
  int size_res_c = Int_val(size_res);
  int nb_evals_c = Int_val(nb_evals);

  blst_fr *evaluations_c[nb_evals_c];
  int evaluations_len_c[nb_evals_c];
  blst_fr linear_coeffs_c[nb_evals_c];
  int composition_gx_c[nb_evals_c];

  value idx_i;
  blst_fr *c;
  for (int i = 0; i < nb_evals_c; i++) {
    idx_i = Field(eval_evallen_coeff_comp, i);
    evaluations_c[i] = Blst_fr_array_val(Field(idx_i, 0));
    evaluations_len_c[i] = Int_val(Field(idx_i, 1));
    c = Blst_fr_val(Field(idx_i, 2));
    memcpy(linear_coeffs_c + i, c, sizeof(blst_fr));
    composition_gx_c[i] = Int_val(Field(idx_i, 3));
  }

  bls12_381_polynomial_polynomial_evaluations_linear_arrays(
      res_c, evaluations_c, evaluations_len_c, linear_coeffs_c,
      composition_gx_c, add_constant_c, size_res_c, nb_evals_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_polynomial_polynomial_derivative_stubs(
    value res, value poly, value size) {
  CAMLparam3(res, poly, size);
  blst_fr *poly_c = Blst_fr_array_val(poly);
  blst_fr *res_c = Blst_fr_array_val(res);
  int size_c = Int_val(size);
  bls12_381_polynomial_polynomial_derivative(res_c, poly_c, size_c);
  CAMLreturn(Val_unit);
}
