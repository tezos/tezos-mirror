#include "blst.h"
#include "rescue.h"
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdlib.h>
#include <string.h>

#include "caml_bls12_381_stubs.h"

#define Rescue_ctxt_val(v) (*((rescue_ctxt_t **)Data_custom_val(v)))

static void finalize_free_rescue_ctxt(value vctxt) {
  rescue_ctxt_t *ctxt = Rescue_ctxt_val(vctxt);
  blst_fr *state = rescue_get_state_from_context(ctxt);
  free(state);
  free(ctxt);
}

static struct custom_operations rescue_ctxt_ops = {"rescue_ctxt_t",
                                                   finalize_free_rescue_ctxt,
                                                   custom_compare_default,
                                                   custom_hash_default,
                                                   custom_serialize_default,
                                                   custom_deserialize_default,
                                                   custom_compare_ext_default,
                                                   custom_fixed_length_default};

CAMLprim value caml_bls12_381_hash_rescue_allocate_ctxt_stubs(
    value vmds, value vconstants, value vnb_rounds, value vstate_size) {
  CAMLparam4(vmds, vconstants, vnb_rounds, vstate_size);
  CAMLlocal1(vblock);

  int state_size = Int_val(vstate_size);
  int nb_rounds = Int_val(vnb_rounds);
  int mds_size = state_size * state_size;
  int nb_constants = state_size * nb_rounds * 2;

  // Initialize state. It contains the constants and the MDS
  blst_fr *ctxt_internal =
      malloc(sizeof(blst_fr) * (state_size + mds_size + nb_constants));

  if (ctxt_internal == NULL) {
    caml_raise_out_of_memory();
  }

  blst_fr *state = ctxt_internal;
  memset(state, 0, sizeof(blst_fr) * state_size);

  blst_fr *mds = ctxt_internal + state_size;
  blst_fr *constants = ctxt_internal + state_size + mds_size;

  // Copying MDS
  for (int i = 0; i < state_size; i++) {
    for (int j = 0; j < state_size; j++) {
      memcpy(mds + i * state_size + j, Fr_val_ij(vmds, i, j), sizeof(blst_fr));
    }
  }

  // Copying ark
  for (int i = 0; i < nb_constants; i++) {
    memcpy(constants + i, Fr_val_k(vconstants, i), sizeof(blst_fr));
  }

  rescue_ctxt_t *ctxt = malloc(sizeof(rescue_ctxt_t));
  if (ctxt == NULL) {
    free(ctxt_internal);
    caml_raise_out_of_memory();
  }
  ctxt->state = ctxt_internal;
  ctxt->state_size = state_size;
  ctxt->nb_rounds = nb_rounds;

  size_t out_of_heap_memory_size =
      sizeof(blst_fr) * (state_size + mds_size + nb_constants) +
      sizeof(rescue_ctxt_t);
  vblock = caml_alloc_custom_mem(&rescue_ctxt_ops, sizeof(rescue_ctxt_t *),
                                 out_of_heap_memory_size);

  rescue_ctxt_t **block = (rescue_ctxt_t **)Data_custom_val(vblock);
  *block = ctxt;

  CAMLreturn(vblock);
}

CAMLprim value caml_bls12_381_hash_rescue_apply_permutation_stubs(value vctxt) {
  CAMLparam1(vctxt);
  marvellous_apply_permutation(Rescue_ctxt_val(vctxt));
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_hash_rescue_get_state_stubs(value vbuffer,
                                                          value vctxt) {
  CAMLparam2(vbuffer, vctxt);
  rescue_ctxt_t *ctxt = Rescue_ctxt_val(vctxt);
  blst_fr *state = rescue_get_state_from_context(ctxt);
  int state_size = ctxt->state_size;
  for (int i = 0; i < state_size; i++) {
    memcpy(Fr_val_k(vbuffer, i), state + i, sizeof(blst_fr));
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_hash_rescue_set_state_stubs(value vctxt,
                                                          value vstate) {
  CAMLparam2(vctxt, vstate);

  rescue_ctxt_t *ctxt = Rescue_ctxt_val(vctxt);
  blst_fr *state = rescue_get_state_from_context(ctxt);

  for (int i = 0; i < ctxt->state_size; i++) {
    memcpy(state + i, Fr_val_k(vstate, i), sizeof(blst_fr));
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_hash_rescue_get_state_size_stubs(value vctxt) {
  CAMLparam1(vctxt);
  int state_size = rescue_get_state_size_from_context(Rescue_ctxt_val(vctxt));
  CAMLreturn(Val_int(state_size));
}
