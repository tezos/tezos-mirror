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
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#define Blst_fr_array_val(v) ((blst_fr *)Caml_ba_data_val(v))
#define Blst_g1_array_val(v) ((blst_p1 *)Caml_ba_data_val(v))
#define Blst_g2_array_val(v) ((blst_p2 *)Caml_ba_data_val(v))

CAMLprim value caml_bls12_381_polynomial_fft_inplace_on_stubs(
    value coefficients, value domain, value log_domain_size, value log_degree) {
  CAMLparam4(coefficients, domain, log_domain_size, log_degree);

  int log_domain_size_c = Int_val(log_domain_size);
  int log_degree_c = Int_val(log_degree);
  blst_fr *domain_c = Blst_fr_array_val(domain);
  blst_fr *coefficients_c = Blst_fr_array_val(coefficients);
  bls12_381_polynomial_fft_inplace(coefficients_c, domain_c,
                                            log_domain_size_c, log_degree_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_polynomial_ifft_inplace_on_stubs(
    value coefficients, value domain, value log_domain_size) {
  CAMLparam3(coefficients, domain, log_domain_size);

  int log_domain_size_c = Int_val(log_domain_size);
  blst_fr *domain_c = Blst_fr_array_val(domain);
  blst_fr *coefficients_c = Blst_fr_array_val(coefficients);
  bls12_381_polynomial_ifft_inplace(coefficients_c, domain_c,
                                             log_domain_size_c);
  CAMLreturn(Val_unit);
}

CAMLprim value
caml_bls12_381_polynomial_prime_factor_algorithm_fft_stubs(
    value coefficients, value domain1_length1, value domain2_length2,
    value inverse) {
  CAMLparam4(inverse, domain1_length1, domain2_length2, coefficients);
  blst_fr *domain1_c = Blst_fr_array_val(Field(domain1_length1, 0));
  int length1_c = Int_val(Field(domain1_length1, 1));
  blst_fr *domain2_c = Blst_fr_array_val(Field(domain2_length2, 0));
  int length2_c = Int_val(Field(domain2_length2, 1));
  bool inverse_c = Bool_val(inverse);
  blst_fr *coefficients_c = Blst_fr_array_val(coefficients);

  bls12_381_polynomial_prime_factor_algorithm_fft(
      coefficients_c, domain1_c, domain2_c, length1_c, length2_c, inverse_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_polynomial_dft_stubs(value coefficients,
                                                            value domain,
                                                            value inverse,
                                                            value length) {
  CAMLparam4(coefficients, domain, length, inverse);
  blst_fr *coefficients_c = Blst_fr_array_val(coefficients);
  blst_fr *domain_c = Blst_fr_array_val(domain);
  blst_fr buffer_dft[length];
  bls12_381_polynomial_dft_inplace(
      coefficients_c, domain_c, buffer_dft, Int_val(length), Bool_val(inverse));
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_polynomial_fft_g1_inplace_on_stubs(
    value coefficients, value domain, value log_domain_size, value log_degree) {
  CAMLparam4(coefficients, domain, log_domain_size, log_degree);

  int log_domain_size_c = Int_val(log_domain_size);
  int log_degree_c = Int_val(log_degree);
  blst_fr *domain_c = Blst_fr_array_val(domain);
  blst_p1 *coefficients_c = Blst_g1_array_val(coefficients);
  bls12_381_polynomial_fft_g1_inplace(coefficients_c, domain_c,
                                               log_domain_size_c, log_degree_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_polynomial_ifft_g1_inplace_on_stubs(
    value coefficients, value domain, value log_domain_size) {
  CAMLparam3(coefficients, domain, log_domain_size);

  int log_domain_size_c = Int_val(log_domain_size);
  blst_fr *domain_c = Blst_fr_array_val(domain);
  blst_p1 *coefficients_c = Blst_g1_array_val(coefficients);
  bls12_381_polynomial_ifft_g1_inplace(coefficients_c, domain_c,
                                                log_domain_size_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_polynomial_fft_g2_inplace_on_stubs(
    value coefficients, value domain, value log_domain_size, value log_degree) {
  CAMLparam4(coefficients, domain, log_domain_size, log_degree);

  int log_domain_size_c = Int_val(log_domain_size);
  int log_degree_c = Int_val(log_degree);
  blst_fr *domain_c = Blst_fr_array_val(domain);
  blst_p2 *coefficients_c = Blst_g2_array_val(coefficients);
  bls12_381_polynomial_fft_g2_inplace(coefficients_c, domain_c,
                                               log_domain_size_c, log_degree_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_polynomial_ifft_g2_inplace_on_stubs(
    value coefficients, value domain, value log_domain_size) {
  CAMLparam3(coefficients, domain, log_domain_size);

  int log_domain_size_c = Int_val(log_domain_size);
  blst_fr *domain_c = Blst_fr_array_val(domain);
  blst_p2 *coefficients_c = Blst_g2_array_val(coefficients);
  bls12_381_polynomial_ifft_g2_inplace(coefficients_c, domain_c,
                                                log_domain_size_c);
  CAMLreturn(Val_unit);
}
