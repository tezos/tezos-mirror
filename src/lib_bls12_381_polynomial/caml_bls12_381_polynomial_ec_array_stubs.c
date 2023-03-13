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

#include "caml_bls12_381_stubs.h"
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

CAMLprim value caml_bls12_381_polynomial_carray_g1_add_inplace_stubs(
    value carray_g1_1, value carray_g1_2, value size) {
  CAMLparam3(carray_g1_1, carray_g1_2, size);
  blst_p1 *carray_g1_1_c = Caml_ba_data_val(carray_g1_1);
  blst_p1 *carray_g1_2_c = Caml_ba_data_val(carray_g1_2);
  int size_c = Int_val(size);

  for (int i = 0; i < size_c; i++) {
    blst_p1_add_or_double(carray_g1_1_c + i, carray_g1_1_c + i,
                          carray_g1_2_c + i);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_polynomial_carray_g2_add_inplace_stubs(
    value carray_g2_1, value carray_g2_2, value size) {
  CAMLparam3(carray_g2_1, carray_g2_2, size);
  blst_p2 *carray_g2_1_c = Caml_ba_data_val(carray_g2_1);
  blst_p2 *carray_g2_2_c = Caml_ba_data_val(carray_g2_2);
  int size_c = Int_val(size);

  for (int i = 0; i < size_c; i++) {
    blst_p2_add_or_double(carray_g2_1_c + i, carray_g2_1_c + i,
                          carray_g2_2_c + i);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value
caml_bls12_381_polynomial_evaluations_mul_arrays_g1_stubs(
    value res, value evaluations, value g1_arrays, value array_dimensions) {
  CAMLparam4(res, evaluations, array_dimensions, g1_arrays);
  int size_arrays_c = Int_val(Field(array_dimensions, 0));
  int size_array_c = Int_val(Field(array_dimensions, 1));

  blst_scalar scalar;
  byte le_scalar[32];

  for (int j = 0; j < size_arrays_c; j++) {
    blst_fr *eval = (blst_fr *)Caml_ba_data_val(Field(evaluations, j));
    blst_p1 *g1_array = (blst_p1 *)Caml_ba_data_val(Field(g1_arrays, j));
    blst_p1 *resj = (blst_p1 *)Caml_ba_data_val(Field(res, j));
    for (int i = 0; i < size_array_c; i++) {
      blst_scalar_from_fr(&scalar, eval + i);
      blst_lendian_from_scalar(le_scalar, &scalar);
      blst_p1_mult(resj + i, g1_array + i, le_scalar, 256);
    }
  }

  CAMLreturn(Val_unit);
}

CAMLprim value
caml_bls12_381_polynomial_evaluations_mul_arrays_g2_stubs(
    value res, value evaluations, value g2_arrays, value array_dimensions) {
  CAMLparam4(res, evaluations, array_dimensions, g2_arrays);
  int size_arrays_c = Int_val(Field(array_dimensions, 0));
  int size_array_c = Int_val(Field(array_dimensions, 1));

  blst_scalar scalar;
  byte le_scalar[32];

  for (int j = 0; j < size_arrays_c; j++) {
    blst_fr *eval = (blst_fr *)Caml_ba_data_val(Field(evaluations, j));
    blst_p2 *g2_array = (blst_p2 *)Caml_ba_data_val(Field(g2_arrays, j));
    blst_p2 *resj = (blst_p2 *)Caml_ba_data_val(Field(res, j));
    for (int i = 0; i < size_array_c; i++) {
      blst_scalar_from_fr(&scalar, eval + i);
      blst_lendian_from_scalar(le_scalar, &scalar);
      blst_p2_mult(resj + i, g2_array + i, le_scalar, 256);
    }
  }

  CAMLreturn(Val_unit);
}
