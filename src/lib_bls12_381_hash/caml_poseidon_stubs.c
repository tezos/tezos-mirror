#include "blst.h"
#include "blst_misc.h"

#include "caml_bls12_381_stubs.h"
#include "ocaml_integers.h"
#include "poseidon.h"
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdlib.h>
#include <string.h>

#define Poseidon_ctxt_val(v) (*((poseidon_ctxt_t **)Data_custom_val(v)))

static void finalize_free_poseidon_ctxt(value vctxt) {
  poseidon_ctxt_t *ctxt = Poseidon_ctxt_val(vctxt);
  blst_fr *state = poseidon_get_state_from_context(ctxt);
  free(state);
  free(ctxt);
}

static struct custom_operations poseidon_ctxt_ops = {
    "poseidon_ctxt_t",          finalize_free_poseidon_ctxt,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

/*
  A context is a contiguous C piece of memory containing the state, the ark and
  the MDS in the following way:

  | state[0] | state[1] | ... | state[W - 1] | ark[0] | ark[1] ... | ark[N] |
    MDS[0][0] | MDS[0][1] | ... | MDS[0][W - 1] | ... | MDS[W - 1][W - 1] |

  The goal is to use the CPU cache and use the instance parameters as values on
  the stack
  */
CAMLprim value caml_bls12_381_hash_poseidon_allocate_ctxt_stubs(
    value vwidth, value vnb_full_rounds, value vnb_partial_rounds,
    value vbatch_size, value vark, value vmds) {
  // ark and mds are of correct size. We do not perform any check
  CAMLparam5(vwidth, vnb_full_rounds, vnb_partial_rounds, vbatch_size, vark);
  CAMLxparam1(vmds);

  CAMLlocal1(vblock);

  int width = Int_val(vwidth);
  int nb_full_rounds = Int_val(vnb_full_rounds);
  int nb_partial_rounds = Int_val(vnb_partial_rounds);
  int batch_size = Int_val(vbatch_size);
  int nb_constants = poseidon_compute_number_of_constants(
      batch_size, nb_partial_rounds, nb_full_rounds, width);
  // state + ark length + MDS
  int nb_blst_fr_elem = width + nb_constants + width * width;

  blst_fr *state = malloc(sizeof(blst_fr) * nb_blst_fr_elem);
  if (state == NULL) {
    caml_raise_out_of_memory();
  }
  memset(state, 0, sizeof(blst_fr) * width);

  blst_fr *ctxt_mds = state + width;
  blst_fr *ctxt_ark = ctxt_mds + width * width;
  // Copying ark
  for (int i = 0; i < nb_constants; i++) {
    memcpy(ctxt_ark + i, Fr_val_k(vark, i), sizeof(blst_fr));
  }
  // Copying MDS
  for (int i = 0; i < width; i++) {
    for (int j = 0; j < width; j++) {
      memcpy(ctxt_mds + i * width + j, Fr_val_ij(vmds, i, j), sizeof(blst_fr));
    }
  }

  poseidon_ctxt_t *ctxt = malloc(sizeof(poseidon_ctxt_t));
  if (ctxt == NULL) {
    free(state);
    caml_raise_out_of_memory();
  }
  ctxt->state = state;
  ctxt->state_size = width;
  ctxt->nb_full_rounds = nb_full_rounds;
  ctxt->nb_partial_rounds = nb_partial_rounds;
  ctxt->batch_size = batch_size;

  size_t out_of_heap_memory_size =
      sizeof(blst_fr) * nb_blst_fr_elem + sizeof(poseidon_ctxt_t);
  vblock = caml_alloc_custom_mem(&poseidon_ctxt_ops, sizeof(poseidon_ctxt_t *),
                                 out_of_heap_memory_size);

  poseidon_ctxt_t **block = (poseidon_ctxt_t **)Data_custom_val(vblock);
  *block = ctxt;
  CAMLreturn(vblock);
}

CAMLprim value caml_bls12_381_hash_poseidon_allocate_ctxt_stubs_bytecode(
    value *argv, value argc) {
  if (argc != 6) {
    caml_failwith("caml_bls12_381_hash_poseidon_allocate_ctxt_stubs_bytecode: "
                  "wrong argc value");
  }
  return caml_bls12_381_hash_poseidon_allocate_ctxt_stubs(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value caml_bls12_381_hash_poseidon_set_state_stubs(value vctxt,
                                                            value vstate) {
  CAMLparam2(vctxt, vstate);

  poseidon_ctxt_t *ctxt = Poseidon_ctxt_val(vctxt);
  blst_fr *state = poseidon_get_state_from_context(ctxt);

  for (int i = 0; i < ctxt->state_size; i++) {
    memcpy(state + i, Fr_val_k(vstate, i), sizeof(blst_fr));
  }

  CAMLreturn(Val_unit);
}

CAMLprim value
caml_bls12_381_hash_poseidon_apply_permutation_stubs(value vctxt) {
  CAMLparam1(vctxt);
  poseidon_ctxt_t *ctxt = Poseidon_ctxt_val(vctxt);
  poseidon_apply_permutation(ctxt);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_hash_poseidon_get_state_stubs(value vbuffer,
                                                            value vctxt) {
  CAMLparam2(vbuffer, vctxt);
  poseidon_ctxt_t *ctxt = Poseidon_ctxt_val(vctxt);
  blst_fr *state = poseidon_get_state_from_context(ctxt);
  int state_size = ctxt->state_size;
  for (int i = 0; i < state_size; i++) {
    memcpy(Fr_val_k(vbuffer, i), state + i, sizeof(blst_fr));
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bls12_381_hash_poseidon_get_state_size_stubs(value vctxt) {
  CAMLparam1(vctxt);
  poseidon_ctxt_t *ctxt = Poseidon_ctxt_val(vctxt);
  CAMLreturn(Val_int(ctxt->state_size));
}
