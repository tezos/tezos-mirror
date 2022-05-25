#include "caml_bls12_381_stubs.h"
#include "ocaml_integers.h"
#include "polynomial.h"
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#define Blst_fr_array_val(v) (*(blst_fr **)Data_custom_val(v))

static void finalize_blst_fr_array(value v) { free(Blst_fr_array_val(v)); }
static struct custom_operations blst_fr_array_ops = {
    "blst_fr_array",
    finalize_blst_fr_array,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default};

CAMLprim value caml_polynomial_allocate_fr_array_stubs(value n) {
  CAMLparam1(n);
  CAMLlocal1(block);
  int n_c = Int_val(n);
  // Allocating the polynomial in the C heap to avoid too big values in the Caml
  // heap
  block = caml_alloc_custom(&blst_fr_array_ops, sizeof(blst_fr *), 0, 1);
  void *p = calloc(1, sizeof(blst_fr) * n_c);
  if (p == NULL) {
    caml_raise_out_of_memory();
  }
  blst_fr **block_fr = (blst_fr **)(Data_custom_val(block));
  *block_fr = (blst_fr *)p;
  CAMLreturn(block);
}

CAMLprim value caml_polynomial_compute_domain_stubs(value buffer, value n,
                                                    value root_of_unity) {
  CAMLparam3(buffer, n, root_of_unity);
  blst_fr *root_of_unity_c = Blst_fr_val(root_of_unity);
  int n_c = Int_val(n);
  blst_fr *buffer_c = Blst_fr_array_val(buffer);

  uint64_t one[4] = {1, 0, 0, 0};
  blst_fr_from_uint64(buffer_c, one);
  memcpy(buffer_c + 1, root_of_unity_c, sizeof(blst_fr));

  for (int i = 1; i < n_c / 2; i++) {
    blst_fr_sqr(buffer_c + 2 * i, buffer_c + i);
    blst_fr_mul(buffer_c + 2 * i + 1, buffer_c + 2 * i, root_of_unity_c);
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_polynomial_of_fr_array_stubs(value buffer, value vs,
                                                 value n) {
  CAMLparam3(buffer, vs, n);

  blst_fr *vs_c = Blst_fr_array_val(vs);
  int buffer_size = Int_val(n);
  for (int i = 0; i < buffer_size; i++) {
    memcpy(Fr_val_k(buffer, i), vs_c + i, sizeof(blst_fr));
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_polynomial_degree_stubs(value arg, value n) {
  CAMLparam2(arg, n);

  blst_fr *arg_c = Blst_fr_array_val(arg);
  int n_c = Int_val(n);
  int res = polynomial_degree(arg_c, n_c);
  CAMLreturn(Val_int(res));
}

CAMLprim value caml_polynomial_get_stubs(value res, value arg, value idx) {
  CAMLparam3(res, arg, idx);
  blst_fr *res_c = Blst_fr_val(res);
  blst_fr *arg_c = Blst_fr_array_val(arg);
  int idx_c = Int_val(idx);

  memcpy(res_c, arg_c + idx_c, sizeof(blst_fr));
  CAMLreturn(Val_unit);
}

CAMLprim value caml_polynomial_copy_stubs(value buffer, value vs, value n) {
  CAMLparam3(buffer, vs, n);

  blst_fr *vs_c = Blst_fr_array_val(vs);
  blst_fr *buffer_c = Blst_fr_array_val(buffer);
  int buffer_size = Int_val(n);
  for (int i = 0; i < buffer_size; i++) {
    memcpy(buffer_c + i, vs_c + i, sizeof(blst_fr));
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_polynomial_fft_inplace_on_stubs(value coefficients,
                                                    value domain,
                                                    value log_domain_size) {
  CAMLparam3(coefficients, domain, log_domain_size);

  int log_domain_size_c = Int_val(log_domain_size);
  blst_fr *domain_c = Blst_fr_array_val(domain);
  blst_fr *coefficients_c = Blst_fr_array_val(coefficients);
  polynomial_fft_inplace(coefficients_c, domain_c, log_domain_size_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_polynomial_ifft_inplace_on_stubs(value coefficients,
                                                     value domain,
                                                     value log_domain_size) {
  CAMLparam3(coefficients, domain, log_domain_size);

  int log_domain_size_c = Int_val(log_domain_size);
  blst_fr *domain_c = Blst_fr_array_val(domain);
  blst_fr *coefficients_c = Blst_fr_array_val(coefficients);
  polynomial_ifft_inplace(coefficients_c, domain_c, log_domain_size_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_polynomial_of_sparse_stubs(value polynomial,
                                               value coefficients,
                                               value nb_coefficients) {
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

CAMLprim value caml_polynomial_of_dense_stubs(value res, value coefficients,
                                              value nb_coefficients) {
  CAMLparam3(res, coefficients, nb_coefficients);

  blst_fr *res_c = Blst_fr_array_val(res);
  int nb_coefficients_c = Int_val(nb_coefficients);

  blst_fr *c;
  for (int i = 0; i < nb_coefficients_c; i++) {
    c = Blst_fr_val(Field(coefficients, i));
    memcpy(res_c + i, c, sizeof(blst_fr));
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_polynomial_eq_stubs(value polynomial_1, value polynomial_2,
                                        value size_1, value size_2) {
  CAMLparam4(polynomial_1, polynomial_2, size_1, size_2);

  blst_fr *poly_1_c = Blst_fr_array_val(polynomial_1);
  int size_1_c = Int_val(size_1);
  blst_fr *poly_2_c = Blst_fr_array_val(polynomial_2);
  int size_2_c = Int_val(size_2);
  bool is_equal = polynomial_eq(poly_1_c, poly_2_c, size_1_c, size_2_c);
  CAMLreturn(Val_bool(is_equal));
}

CAMLprim value caml_polynomial_add_stubs(value res, value arg_1, value arg_2,
                                         value size_1, value size_2) {
  CAMLparam5(res, arg_1, arg_2, size_1, size_2);
  blst_fr *arg_1_c = Blst_fr_array_val(arg_1);
  blst_fr *arg_2_c = Blst_fr_array_val(arg_2);
  blst_fr *res_c = Blst_fr_array_val(res);
  int size_1_c = Int_val(size_1);
  int size_2_c = Int_val(size_2);

  polynomial_add(res_c, arg_1_c, arg_2_c, size_1_c, size_2_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_polynomial_sub_stubs(value res, value arg_1, value arg_2,
                                         value size_1, value size_2) {
  CAMLparam5(res, arg_1, arg_2, size_1, size_2);
  blst_fr *arg_1_c = Blst_fr_array_val(arg_1);
  blst_fr *arg_2_c = Blst_fr_array_val(arg_2);
  blst_fr *res_c = Blst_fr_array_val(res);
  int size_1_c = Int_val(size_1);
  int size_2_c = Int_val(size_2);

  polynomial_sub(res_c, arg_1_c, arg_2_c, size_1_c, size_2_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_polynomial_mul_stubs(value res, value arg_1, value arg_2,
                                         value size_1, value size_2) {
  CAMLparam5(res, arg_1, arg_2, size_1, size_2);
  blst_fr *arg_1_c = Blst_fr_array_val(arg_1);
  blst_fr *arg_2_c = Blst_fr_array_val(arg_2);
  blst_fr *res_c = Blst_fr_array_val(res);
  int size_1_c = Int_val(size_1);
  int size_2_c = Int_val(size_2);

  polynomial_mul(res_c, arg_1_c, arg_2_c, size_1_c, size_2_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_polynomial_mul_by_scalar_stubs(value res, value scalar,
                                                   value arg, value size) {
  CAMLparam4(res, scalar, arg, size);
  blst_fr *res_c = Blst_fr_array_val(res);
  blst_fr *scalar_c = Blst_fr_val(scalar);
  blst_fr *arg_c = Blst_fr_array_val(arg);
  int size_c = Int_val(size);

  polynomial_mul_by_scalar(res_c, scalar_c, arg_c, size_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_polynomial_negate_stubs(value res, value arg, value size) {
  CAMLparam3(res, arg, size);
  blst_fr *res_c = Blst_fr_array_val(res);
  blst_fr *arg_c = Blst_fr_array_val(arg);
  int size_c = Int_val(size);

  polynomial_negate(res_c, arg_c, size_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_polynomial_evaluate_stubs(value res, value arg, value size,
                                              value scalar) {
  CAMLparam4(res, arg, size, scalar);
  blst_fr *res_c = Blst_fr_val(res);
  blst_fr *arg_c = Blst_fr_array_val(arg);
  int size_c = Int_val(size);
  blst_fr *scalar_c = Blst_fr_val(scalar);

  polynomial_evaluate(res_c, arg_c, size_c, scalar_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_polynomial_is_zero_stubs(value poly, value size) {
  CAMLparam2(poly, size);
  blst_fr *poly_c = Blst_fr_array_val(poly);
  int size_c = Int_val(size);
  bool res = polynomial_is_zero(poly_c, size_c);
  CAMLreturn(Val_bool(res));
}

CAMLprim value caml_polynomial_division_x_z_stubs(value res, value poly,
                                                  value size, value z) {
  CAMLparam4(res, poly, size, z);
  blst_fr *poly_c = Blst_fr_array_val(poly);
  blst_fr *res_c = Blst_fr_array_val(res);
  int size_c = Int_val(size);
  blst_fr *z_c = Blst_fr_val(z);
  polynomial_division_x_z(res_c, poly_c, size_c, z_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_polynomial_division_zs_stubs(value res, value poly,
                                                 value size, value n) {
  CAMLparam4(res, poly, size, n);
  blst_fr *poly_c = Blst_fr_array_val(poly);
  blst_fr *res_c = Blst_fr_array_val(res);
  int size_c = Int_val(size);
  int n_c = Int_val(n);
  polynomial_division_zs(res_c, poly_c, size_c, n_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_polynomial_mul_zs_stubs(value res, value poly, value size,
                                            value n) {
  CAMLparam4(res, poly, size, n);
  blst_fr *poly_c = Blst_fr_array_val(poly);
  blst_fr *res_c = Blst_fr_array_val(res);
  int size_c = Int_val(size);
  int n_c = Int_val(n);
  polynomial_mul_zs(res_c, poly_c, size_c, n_c);
  CAMLreturn(Val_unit);
}

// Bindings for evaluations.ml

CAMLprim value caml_polynomial_evaluations_add_stubs(value res, value eval_1,
                                                     value eval_2, value size_1,
                                                     value size_2) {
  CAMLparam5(res, eval_1, eval_2, size_1, size_2);
  blst_fr *res_c = Blst_fr_array_val(res);
  blst_fr *eval_1_c = Blst_fr_array_val(eval_1);
  blst_fr *eval_2_c = Blst_fr_array_val(eval_2);
  int size_1_c = Int_val(size_1);
  int size_2_c = Int_val(size_2);

  polynomial_evaluations_add(res_c, eval_1_c, eval_2_c, size_1_c, size_2_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_polynomial_evaluations_rescale_stubs(value res, value eval,
                                                         value size_res,
                                                         value size_eval) {
  CAMLparam4(res, eval, size_res, size_eval);
  blst_fr *res_c = Blst_fr_array_val(res);
  blst_fr *eval_c = Blst_fr_array_val(eval);
  int size_res_c = Int_val(size_res);
  int size_eval_c = Int_val(size_eval);

  polynomial_evaluations_rescale(res_c, eval_c, size_res_c, size_eval_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_polynomial_evaluations_mul_arrays_stubs(
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

  polynomial_evaluations_mul_arrays(res_c, evaluations_c, evaluations_len_c,
                                    composition_gx_c, powers_c, powers_len_c,
                                    size_res_c, nb_evals_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_polynomial_evaluations_linear_arrays_stubs(
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

  polynomial_evaluations_linear_arrays(res_c, evaluations_c, evaluations_len_c,
                                       linear_coeffs_c, composition_gx_c,
                                       add_constant_c, size_res_c, nb_evals_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_polynomial_derivative_stubs(value res, value poly,
                                                value size) {
  CAMLparam3(res, poly, size);
  blst_fr *poly_c = Blst_fr_array_val(poly);
  blst_fr *res_c = Blst_fr_array_val(res);
  int size_c = Int_val(size);
  polynomial_derivative(res_c, poly_c, size_c);
  CAMLreturn(Val_unit);
}