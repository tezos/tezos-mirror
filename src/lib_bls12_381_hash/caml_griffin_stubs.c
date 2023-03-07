#include "blst.h"
#include "griffin.h"
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdlib.h>
#include <string.h>

#include "caml_bls12_381_stubs.h"

#define Griffin_ctxt_val(v) (*((griffin_ctxt_t **)Data_custom_val(v)))

static void finalize_free_griffin_ctxt(value vctxt) {
  griffin_ctxt_t *ctxt = Griffin_ctxt_val(vctxt);
  free(ctxt->state);
  free(ctxt);
}

static struct custom_operations griffin_ctxt_ops = {
    "griffin_ctxt_t",           finalize_free_griffin_ctxt,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

CAMLprim value caml_bls12_381_hash_griffin_allocate_ctxt_stubs(
    value vnb_rounds, value vstate_size, value vconstants,
    value valpha_beta_s) {
  CAMLparam4(vnb_rounds, vstate_size, vconstants, valpha_beta_s);
  CAMLlocal1(vblock);
  int state_size = Int_val(vstate_size);
  int nb_rounds = Int_val(vnb_rounds);

  // state_size + constants + nb alpha_beta_s
  int nb_alpha_beta_s = (state_size - 2) * 2;
  int nb_constants = nb_rounds * state_size;
  int state_full_size = state_size + nb_constants + nb_alpha_beta_s;

  // must be allocated on the heap as we pass the values to functions later
  blst_fr *alpha_beta_s = malloc(sizeof(blst_fr) * nb_alpha_beta_s);
  if (alpha_beta_s == NULL) {
    caml_raise_out_of_memory();
  }
  blst_fr *constants = malloc(sizeof(blst_fr) * nb_constants);
  if (constants == NULL) {
    free(alpha_beta_s);
    caml_raise_out_of_memory();
  }

  for (int i = 0; i < nb_alpha_beta_s; i++) {
    memcpy(alpha_beta_s + i, Fr_val_k(valpha_beta_s, i), sizeof(blst_fr));
  }

  for (int i = 0; i < nb_constants; i++) {
    memcpy(constants + i, Fr_val_k(vconstants, i), sizeof(blst_fr));
  }

  griffin_ctxt_t *ctxt =
      griffin_allocate_context(state_size, nb_rounds, constants, alpha_beta_s);
  // we don't need it anymore
  free(alpha_beta_s);
  free(constants);

  if (ctxt == NULL) {
    caml_raise_out_of_memory();
  }

  int out_of_heap_size = sizeof(griffin_ctxt_t *) +
                         state_full_size * sizeof(blst_fr) +
                         sizeof(griffin_ctxt_t);

  vblock = caml_alloc_custom_mem(&griffin_ctxt_ops, sizeof(griffin_ctxt_t *),
                                 out_of_heap_size);

  griffin_ctxt_t **block = (griffin_ctxt_t **)(Data_custom_val(vblock));
  *block = ctxt;
  CAMLreturn(vblock);
}

CAMLprim value caml_bls12_381_hash_griffin_set_state_stubs(value vctxt,
                                                           value vstate) {
  CAMLparam2(vctxt, vstate);

  griffin_ctxt_t *ctxt = Griffin_ctxt_val(vctxt);
  blst_fr *state = ctxt->state;
  int state_size = griffin_get_state_size_from_context(ctxt);

  for (int i = 0; i < state_size; i++) {
    memcpy(state + i, Fr_val_k(vstate, i), sizeof(blst_fr));
  }

  CAMLreturn(Val_unit);
}

CAMLprim value
caml_bls12_381_hash_griffin_apply_permutation_stubs(value vctxt) {
  CAMLparam1(vctxt);
  griffin_ctxt_t *ctxt = Griffin_ctxt_val(vctxt);
  griffin_apply_permutation(ctxt);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_hash_griffin_apply_one_round_stubs(
    value vctxt, value vi_round_key) {
  CAMLparam2(vctxt, vi_round_key);
  griffin_ctxt_t *ctxt = Griffin_ctxt_val(vctxt);
  int i_round_key = Int_val(vi_round_key);
  griffin_apply_one_round(ctxt, i_round_key);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_hash_griffin_get_state_stubs(value vbuffer,
                                                           value vctxt) {
  CAMLparam2(vbuffer, vctxt);
  griffin_ctxt_t *ctxt = Griffin_ctxt_val(vctxt);
  blst_fr *state = griffin_get_state_from_context(ctxt);
  int state_size = griffin_get_state_size_from_context(ctxt);
  for (int i = 0; i < state_size; i++) {
    memcpy(Fr_val_k(vbuffer, i), state + i, sizeof(blst_fr));
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_hash_griffin_get_state_size_stubs(value vctxt) {
  CAMLparam1(vctxt);
  griffin_ctxt_t *ctxt = Griffin_ctxt_val(vctxt);
  int state_size = griffin_get_state_size_from_context(ctxt);
  CAMLreturn(Val_int(state_size));
}
