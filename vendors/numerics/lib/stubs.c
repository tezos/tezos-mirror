#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
#include <caml/fail.h>

#include <complex.h>
#include <stdio.h>
#include <assert.h>

void float64_vec_add(double* dst,
                     const double* src1,
                     const double* src2,
                     int len)
{
     for(int i = 0 ; i < len ; i++) {
          dst[i] = src1[i] + src2[i];
     }
}

void float64_vec_mul(double* dst,
                     const double* src1,
                     const double* src2,
                     int len)
{
     for(int i = 0 ; i < len ; i++) {
          dst[i] = src1[i] * src2[i];
     }
}

void complex64_vec_add(complex double* dst,
                       const complex double* src1,
                       const complex double* src2,
                       int len)
{
     for(int i = 0 ; i < len ; i++) {
          dst[i] = src1[i] + src2[i];
     }
}

void complex64_vec_mul(complex double* dst,
                       const complex double* src1,
                       const complex double* src2,
                       int len)
{
     for(int i = 0 ; i < len ; i++) {
          dst[i] = src1[i] * src2[i];
     }
}

void float64_div_scalar_(double* vec,
                         double x,
                         int len)
{
     for(int i = 0 ; i < len ; i++) {
          vec[i] = vec[i] / x;
     }
}

void complex64_div_scalar_(complex double* vec,
                           double x,
                           int len)
{
     for(int i = 0 ; i < len ; i++) {
          vec[i] = vec[i] / x;
     }
}

int bigarray_length(value ba)
{
     int acc = 1;
     for(int i = 0 ; i < Caml_ba_array_val(ba)->num_dims ; i++) {
          acc *= Caml_ba_array_val(ba)->dim[i];
     }
     return acc;
}

int bigarray_dims_equal(value ba1, value ba2)
{
     int dims1 = Caml_ba_array_val(ba1)->num_dims;
     int dims2 = Caml_ba_array_val(ba2)->num_dims;

     if(dims1 != dims2)
          return 0;

     for(int i = 0 ; i < dims1; i++) {
          if(Caml_ba_array_val(ba1)->dim[i] != Caml_ba_array_val(ba2)->dim[i])
               return 0;
     }

     return 1;
}

value caml_float64_vec_add_(value dst, value src1, value src2)
{
     if(! (bigarray_dims_equal(src1, src2) &&
           bigarray_dims_equal(dst, src1))) {
          caml_failwith("Numerics stubs: dimensions mismatch");
     }

     assert((Caml_ba_array_val(dst)->flags & BIGARRAY_KIND_MASK) == CAML_BA_FLOAT64);
     assert((Caml_ba_array_val(src1)->flags & BIGARRAY_KIND_MASK) == CAML_BA_FLOAT64);
     assert((Caml_ba_array_val(src2)->flags & BIGARRAY_KIND_MASK) == CAML_BA_FLOAT64);

     const double* data1 = Caml_ba_data_val(src1);
     const double* data2 = Caml_ba_data_val(src2);
     double* dest  = Caml_ba_data_val(dst);
     int len = bigarray_length(src1);

     float64_vec_add(dest, data1, data2, len);

     return Val_unit;
}

value caml_float64_vec_mul_(value dst, value src1, value src2)
{
     if(! (bigarray_dims_equal(src1, src2) &&
           bigarray_dims_equal(dst, src1))) {
          caml_failwith("Numerics stubs: dimensions mismatch");
     }

     assert((Caml_ba_array_val(dst)->flags & BIGARRAY_KIND_MASK) == CAML_BA_FLOAT64);
     assert((Caml_ba_array_val(src1)->flags & BIGARRAY_KIND_MASK) == CAML_BA_FLOAT64);
     assert((Caml_ba_array_val(src2)->flags & BIGARRAY_KIND_MASK) == CAML_BA_FLOAT64);

     const double* data1 = Caml_ba_data_val(src1);
     const double* data2 = Caml_ba_data_val(src2);
     double* dest  = Caml_ba_data_val(dst);
     int len = bigarray_length(src1);

     float64_vec_mul(dest, data1, data2, len);

     return Val_unit;
}

value caml_complex64_vec_add_(value dst, value src1, value src2)
{
     if(! (bigarray_dims_equal(src1, src2) &&
           bigarray_dims_equal(dst, src1))) {
          caml_failwith("Numerics stubs: dimensions mismatch");
     }

     assert((Caml_ba_array_val(dst)->flags & BIGARRAY_KIND_MASK) == CAML_BA_COMPLEX64);
     assert((Caml_ba_array_val(src1)->flags & BIGARRAY_KIND_MASK) == CAML_BA_COMPLEX64);
     assert((Caml_ba_array_val(src2)->flags & BIGARRAY_KIND_MASK) == CAML_BA_COMPLEX64);

     const complex double* data1 = Caml_ba_data_val(src1);
     const complex double* data2 = Caml_ba_data_val(src2);
     complex double* dest = Caml_ba_data_val(dst);
     int len = bigarray_length(src1);

     complex64_vec_add(dest, data1, data2, len);

     return Val_unit;
}

value caml_complex64_vec_mul_(value dst, value src1, value src2)
{
     if(! (bigarray_dims_equal(src1, src2) &&
           bigarray_dims_equal(dst, src1))) {
          caml_failwith("Numerics stubs: dimensions mismatch");
     }

     assert((Caml_ba_array_val(dst)->flags & BIGARRAY_KIND_MASK) == CAML_BA_COMPLEX64);
     assert((Caml_ba_array_val(src1)->flags & BIGARRAY_KIND_MASK) == CAML_BA_COMPLEX64);
     assert((Caml_ba_array_val(src2)->flags & BIGARRAY_KIND_MASK) == CAML_BA_COMPLEX64);

     const complex double* data1 = Caml_ba_data_val(src1);
     const complex double* data2 = Caml_ba_data_val(src2);
     complex double* dest  = Caml_ba_data_val(dst);
     int len = bigarray_length(src1);

     complex64_vec_mul(dest, data1, data2, len);

     return Val_unit;
}


value caml_float64_vec_div_scalar_(value v, value x)
{
     assert((Caml_ba_array_val(v)->flags & BIGARRAY_KIND_MASK) == CAML_BA_FLOAT64);

     double* data = Caml_ba_data_val(v);
     int len = bigarray_length(v);

     float64_div_scalar_(data, Double_val(x), len);

     return Val_unit;
}

value caml_complex64_vec_div_scalar_(value v, value x)
{
     assert((Caml_ba_array_val(v)->flags & BIGARRAY_KIND_MASK) == CAML_BA_COMPLEX64);

     complex double* data = Caml_ba_data_val(v);
     int len = bigarray_length(v);

     complex64_div_scalar_(data, Double_val(x), len);

     return Val_unit;
}

#define Treal double

#if defined(__STRICT_ANSI__)
  #define OWL_INLINE __inline__
#else
  #define OWL_INLINE inline
#endif

// calculate the number of elements given a bigarray
int c_ndarray_numel (struct caml_ba_array *X) {
  int n = 1;

  for (int i = 0; i < X->num_dims; i ++)
    n *= X->dim[i];

  return n;
}

// calculate the stride of a given dimension of a bigarray
int c_ndarray_stride_dim (struct caml_ba_array *X, int d) {
  int s = 1;

  for (int i = X->num_dims - 1; i > d; i--)
    s *= X->dim[i];

  return s;
}

// calculate the slice size of a given dimension of a bigarray
int c_ndarray_slice_dim (struct caml_ba_array *X, int d) {
  int s = 1;

  for (int i = X->num_dims - 1; i >= d; i--)
    s *= X->dim[i];

  return s;
}

// calculate the stride size of all dimensions of a bigarray
void c_ndarray_stride (struct caml_ba_array *X, int *stride) {
  int i = X->num_dims - 1;
  *(stride + i) = 1;

  for ( ; i > 0; i--)
    *(stride + i - 1) = *(stride + i) * X->dim[i];
}

// copy x to y with given offset and stride
OWL_INLINE void owl_float64_copy (int N, double* x, int ofsx, int incx, double* y, int ofsy, int incy) {
  for (int i = 0; i < N; i++) {
    *(y + ofsy) = *(x + ofsx);
    ofsx += incx;
    ofsy += incy;
  }
}

// copy x to y with given offset and stride
OWL_INLINE void owl_complex64_copy (int N, _Complex double* x, int ofsx, int incx, _Complex double* y, int ofsy, int incy) {
  for (int i = 0; i < N; i++) {
    *(y + ofsy) = *(x + ofsx);
    ofsx += incx;
    ofsy += incy;
  }
}

#define COMPLEX_COPY owl_complex64_copy
#define REAL_COPY owl_float64_copy

#include "owl_fftpack_impl.h"
