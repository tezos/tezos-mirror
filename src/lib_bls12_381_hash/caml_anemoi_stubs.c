#include "blst.h"
#include "blst_misc.h"

#include "anemoi.h"
#include "caml_bls12_381_stubs.h"

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#define Anemoi_ctxt_val(v) (*((anemoi_ctxt_t **)Data_custom_val(v)))

static void finalize_free_anemoi_ctxt(value vctxt) {
  anemoi_ctxt_t *ctxt = Anemoi_ctxt_val(vctxt);
  free(ctxt->ctxt);
  free(ctxt);
}

static struct custom_operations anemoi_ctxt_ops = {"anemoi_ctxt_t",
                                                   finalize_free_anemoi_ctxt,
                                                   custom_compare_default,
                                                   custom_hash_default,
                                                   custom_serialize_default,
                                                   custom_deserialize_default,
                                                   custom_compare_ext_default,
                                                   custom_fixed_length_default};

CAMLprim value caml_bls12_381_hash_anemoi_allocate_ctxt_stubs(
    value vmds, value vconstants, value vbeta, value vdelta, value vl,
    value vnb_rounds) {
  CAMLparam4(vmds, vconstants, vl, vnb_rounds);
  CAMLxparam2(vbeta, vdelta);
  CAMLlocal1(vblock);

  int l = Int_val(vl);
  int state_size = 2 * l;

  int nb_rounds = Int_val(vnb_rounds);

  int mds_size = l * l;
  int nb_constants = l * 2 * nb_rounds;
  // + 2 for beta and delta
  int total_blst_fr_elements = state_size + mds_size + nb_constants + 2;

  // Initialize state. It contains the constants and the MDS
  blst_fr *ctxt_internal = malloc(sizeof(blst_fr) * total_blst_fr_elements);

  if (ctxt_internal == NULL) {
    caml_raise_out_of_memory();
  }

  blst_fr *state = ctxt_internal;
  memset(state, 0, sizeof(blst_fr) * state_size);

  // Copying MDS
  blst_fr *mds = ctxt_internal + state_size;
  for (int i = 0; i < l; i++) {
    for (int j = 0; j < l; j++) {
      memcpy(mds + i * l + j, Fr_val_ij(vmds, i, j), sizeof(blst_fr));
    }
  }

  // Copying round constants
  blst_fr *constants = ctxt_internal + state_size + mds_size;
  for (int i = 0; i < nb_constants; i++) {
    memcpy(constants + i, Fr_val_k(vconstants, i), sizeof(blst_fr));
  }

  blst_fr *beta = constants + nb_constants;
  blst_fr *delta = constants + nb_constants + 1;
  memcpy(beta, Blst_fr_val(vbeta), sizeof(blst_fr));
  memcpy(delta, Blst_fr_val(vdelta), sizeof(blst_fr));

  anemoi_ctxt_t *ctxt = malloc(sizeof(anemoi_ctxt_t));
  if (ctxt == NULL) {
    free(ctxt_internal);
    caml_raise_out_of_memory();
  }
  ctxt->ctxt = ctxt_internal;
  ctxt->l = l;
  ctxt->nb_rounds = nb_rounds;

  size_t out_of_heap_memory_size =
      sizeof(blst_fr) * total_blst_fr_elements + sizeof(anemoi_ctxt_t);
  vblock = caml_alloc_custom_mem(&anemoi_ctxt_ops, sizeof(anemoi_ctxt_t *),
                                 out_of_heap_memory_size);

  anemoi_ctxt_t **block = (anemoi_ctxt_t **)Data_custom_val(vblock);
  *block = ctxt;

  CAMLreturn(vblock);
}

CAMLprim value
caml_bls12_381_hash_anemoi_allocate_ctxt_stubs_bytecode(value *argv, int argc) {
  if (argc != 6) {
    caml_failwith("caml_bls12_381_hash_anemoi_allocate_ctxt_stubs_bytecode: "
                  "wrong argc value");
  }
  return caml_bls12_381_hash_anemoi_allocate_ctxt_stubs(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value caml_bls12_381_hash_anemoi_get_state_stubs(value vbuffer,
                                                          value vctxt) {
  CAMLparam2(vbuffer, vctxt);
  anemoi_ctxt_t *ctxt = Anemoi_ctxt_val(vctxt);
  blst_fr *state = anemoi_get_state_from_context(ctxt);
  int state_size = anemoi_get_state_size_from_context(ctxt);

  for (int i = 0; i < state_size; i++) {
    memcpy(Fr_val_k(vbuffer, i), state + i, sizeof(blst_fr));
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_hash_anemoi_set_state_stubs(value vctxt,
                                                          value vstate) {
  CAMLparam2(vctxt, vstate);

  anemoi_ctxt_t *ctxt = Anemoi_ctxt_val(vctxt);
  blst_fr *state = anemoi_get_state_from_context(ctxt);
  int state_size = anemoi_get_state_size_from_context(ctxt);

  for (int i = 0; i < state_size; i++) {
    memcpy(state + i, Fr_val_k(vstate, i), sizeof(blst_fr));
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_hash_anemoi_get_state_size_stubs(value vctxt) {
  CAMLparam1(vctxt);
  anemoi_ctxt_t *ctxt = Anemoi_ctxt_val(vctxt);
  CAMLreturn(Val_int(anemoi_get_state_size_from_context(ctxt)));
}

CAMLprim value caml_bls12_381_hash_anemoi_apply_permutation_stubs(value vctxt) {
  CAMLparam1(vctxt);
  anemoi_ctxt_t *ctxt = Anemoi_ctxt_val(vctxt);
  anemoi_apply_permutation(ctxt);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_hash_anemoi_apply_one_round_stubs(value vctxt,
                                                                value vround) {
  CAMLparam2(vctxt, vround);
  anemoi_ctxt_t *ctxt = Anemoi_ctxt_val(vctxt);
  int round = Int_val(vround);
  anemoi_apply_one_round(ctxt, round);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_hash_anemoi_apply_constants_addition_stubs(
    value vctxt, value vround) {
  CAMLparam2(vctxt, vround);
  anemoi_ctxt_t *ctxt = Anemoi_ctxt_val(vctxt);
  int round = Int_val(vround);
  anemoi_apply_constants_addition(ctxt, round);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_hash_anemoi_apply_flystel_stubs(value vctxt) {
  CAMLparam1(vctxt);
  anemoi_ctxt_t *ctxt = Anemoi_ctxt_val(vctxt);
  anemoi_apply_flystel(ctxt);
  CAMLreturn(Val_unit);
}

CAMLprim value
caml_bls12_381_hash_anemoi_apply_linear_layer_stubs(value vctxt) {
  CAMLparam1(vctxt);
  anemoi_ctxt_t *ctxt = Anemoi_ctxt_val(vctxt);
  anemoi_apply_linear_layer(ctxt);
  CAMLreturn(Val_unit);
}
