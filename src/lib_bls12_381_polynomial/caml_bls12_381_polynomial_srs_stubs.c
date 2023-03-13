#include "caml_bls12_381_stubs.h"
#include "ocaml_integers.h"
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <string.h>

#define Blst_fr_array_val(v) ((blst_fr *)Caml_ba_data_val(v))

#define Srs_val_g1(v) ((blst_p1_affine *)Caml_ba_data_val(v))

#define Srs_val_g2(v) ((blst_p2_affine *)Caml_ba_data_val(v))

CAMLprim value caml_bls12_381_polynomial_srs_g1_pippenger_stubs(
    value res, value srs, value poly, value start, value len) {
  CAMLparam5(res, srs, poly, start, len);
  int start_c = Int_val(start);
  int len_c = Int_val(len);
  blst_fr *poly_c = Blst_fr_array_val(poly) + start_c;
  const blst_p1_affine *srs_c = Srs_val_g1(srs) + start_c;

  byte *bs_c = malloc(sizeof(byte[32]) * len_c);
  if (bs_c == NULL) {
    caml_raise_out_of_memory();
  }
  for (int i = 0; i < len_c; i++) {
    // bs_c[i] points to the ith element of the contiguous C array of bytes of
    // size ctxt_size. See caml_polynomial_allocate_pippenger_ctxt_stubs
    blst_lendian_from_fr(bs_c + (32 * i), poly_c + i);
  }

  // blst does not support pippenger for 1 element
  if (len_c == 1) {
    blst_p1 tmp;
    blst_p1_from_affine(&tmp, &srs_c[0]);
    blst_p1_mult(Blst_p1_val(res), &tmp, &bs_c[0], 256);
  } else {
    limb_t *scratch = calloc(1, blst_p1s_mult_pippenger_scratch_sizeof(len_c));
    if (scratch == NULL) {
      free(bs_c);
      caml_raise_out_of_memory();
    }

    blst_p1s_mult_pippenger_cont(Blst_p1_val(res), srs_c, len_c,
                                 (const byte *)bs_c, 256, scratch);
    free(scratch);
  }
  free(bs_c);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_polynomial_srs_g2_pippenger_stubs(
    value res, value srs, value poly, value start, value len) {
  CAMLparam5(res, srs, poly, start, len);
  int start_c = Int_val(start);
  int len_c = Int_val(len);
  blst_fr *poly_c = Blst_fr_array_val(poly) + start_c;
  const blst_p2_affine *srs_c = Srs_val_g2(srs) + start_c;

  byte *bs_c = malloc(sizeof(byte[32]) * len_c);
  if (bs_c == NULL) {
    caml_raise_out_of_memory();
  }
  for (int i = 0; i < len_c; i++) {
    // bs_c[i] points to the ith element of the contiguous C array of bytes of
    // size ctxt_size. See caml_polynomial_allocate_pippenger_ctxt_stubs
    blst_lendian_from_fr(bs_c + (32 * i), poly_c + i);
  }

  // blst does not support pippenger for 1 element
  if (len_c == 1) {
    blst_p2 tmp;
    blst_p2_from_affine(&tmp, &srs_c[0]);
    blst_p2_mult(Blst_p2_val(res), &tmp, &bs_c[0], 256);
  } else {
    limb_t *scratch = calloc(1, blst_p2s_mult_pippenger_scratch_sizeof(len_c));
    if (scratch == NULL) {
      free(bs_c);
      caml_raise_out_of_memory();
    }

    blst_p2s_mult_pippenger_cont(Blst_p2_val(res), srs_c, len_c,
                                 (const byte *)bs_c, 256, scratch);
    free(scratch);
  }
  free(bs_c);
  CAMLreturn(Val_unit);
}
