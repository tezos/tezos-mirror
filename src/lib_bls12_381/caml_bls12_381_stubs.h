#ifndef CAML_BLS12_381_STUBS
#define CAML_BLS12_381_STUBS

#include "blst.h"
#include "blst_misc.h"
#include <caml/custom.h>
#include <caml/mlvalues.h>

#define CAML_BLS12_381_OUTPUT_SUCCESS Val_int(0)

#define CAML_BLS12_381_OUTPUT_OUT_OF_MEMORY Val_int(1)

#define CAML_BLS12_381_OUTPUT_INVALID_ARGUMENT Val_int(2)

#define Blst_fr_val(v) ((blst_fr *)Data_custom_val(v))

#define Blst_scalar_val(v) ((blst_scalar *)Data_custom_val(v))

#define Blst_fp_val(v) ((blst_fp *)Data_custom_val(v))

#define Blst_fp2_val(v) ((blst_fp2 *)Data_custom_val(v))

#define Blst_fp12_val(v) ((blst_fp12 *)Data_custom_val(v))

#define Blst_p1_val(v) ((blst_p1 *)Data_custom_val(v))

#define Blst_p1_affine_val(v) ((blst_p1_affine *)Data_custom_val(v))

#define Blst_p2_val(v) ((blst_p2 *)Data_custom_val(v))

#define Blst_p2_affine_val(v) ((blst_p2_affine *)Data_custom_val(v))

#define Fr_val_k(v, k) (Blst_fr_val(Field(v, k)))

#define Fr_val_ij(v, i, j) Blst_fr_val(Field(Field(v, i), j))

#define G1_val_k(v, k) (Blst_p1_val(Field(v, k)))

#define G2_val_k(v, k) (Blst_p2_val(Field(v, k)))

#endif
