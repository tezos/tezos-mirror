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

#ifndef BLS12_381_POLYNOMIAL_INTERNAL_FFT_H
#define BLS12_381_POLYNOMIAL_INTERNAL_FFT_H

#include "blst.h"

// H: domain_size = polynomial degree
// Implementation with side effect. The FFT will be inplace, i.e. the array is
// going to be modified.
void bls12_381_polynomial_fft_inplace(blst_fr *coefficients,
                                               blst_fr *domain,
                                               int log_domain_size,
                                               int log_degree);

void bls12_381_polynomial_ifft_inplace(blst_fr *coefficients,
                                                blst_fr *domain,
                                                int log_domain_size);

void bls12_381_polynomial_dft_inplace(blst_fr *coefficients,
                                               blst_fr *domain, blst_fr *buffer,
                                               int length, int inverse);

void bls12_381_polynomial_prime_factor_algorithm_fft(
    blst_fr *coefficients, blst_fr *domain1, blst_fr *domain2, int length1,
    int length2, int inverse);

void bls12_381_polynomial_fft_g1_inplace(blst_p1 *coefficients,
                                                  blst_fr *domain,
                                                  int log_domain_size,
                                                  int log_degree);

void bls12_381_polynomial_ifft_g1_inplace(blst_p1 *coefficients,
                                                   blst_fr *domain,
                                                   int log_domain_size);

void bls12_381_polynomial_fft_g2_inplace(blst_p2 *coefficients,
                                                  blst_fr *domain,
                                                  int log_domain_size,
                                                  int log_degree);

void bls12_381_polynomial_ifft_g2_inplace(blst_p2 *coefficients,
                                                   blst_fr *domain,
                                                   int log_domain_size);
#endif
