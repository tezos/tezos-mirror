#include "poseidon.h"

int poseidon_compute_number_of_constants(int batch_size, int nb_partial_rounds,
                                         int nb_full_rounds, int width) {
  int nb_tmp_var = batch_size - 1;
  int nb_batched_partial_rounds = nb_partial_rounds / batch_size;
  int nb_unbatched_partial_rounds = nb_partial_rounds % batch_size;
  int nb_constants_full_rounds = nb_full_rounds * width;
  int nb_constants_unbatched_partial_rounds =
      nb_unbatched_partial_rounds * width;
  int nb_constants_per_batched_partial_rounds_tmp_var =
      nb_tmp_var + width * nb_tmp_var + (nb_tmp_var * (nb_tmp_var - 1) / 2);
  int nb_constants_per_batched_partial_rounds_final_computation =
      (nb_tmp_var + width) * width + width;
  int nb_constants_per_batched_partial_rounds =
      nb_constants_per_batched_partial_rounds_tmp_var +
      nb_constants_per_batched_partial_rounds_final_computation;
  int nb_constants_batched_partial_rounds =
      nb_batched_partial_rounds * nb_constants_per_batched_partial_rounds;
  // we add width zero's at the end
  int nb_constants = nb_constants_full_rounds +
                     nb_constants_batched_partial_rounds +
                     nb_constants_unbatched_partial_rounds + width;
  return (nb_constants);
}

blst_fr *poseidon_get_state_from_context(poseidon_ctxt_t *ctxt) {
  return (ctxt->state);
}

int poseidon_get_state_size_from_context(poseidon_ctxt_t *ctxt) {
  return (ctxt->state_size);
}

blst_fr *poseidon_get_mds_from_context(poseidon_ctxt_t *ctxt) {
  // MDS is after the state
  return (ctxt->state + ctxt->state_size);
}

blst_fr *poseidon_get_round_constants_from_context(poseidon_ctxt_t *ctxt) {
  // round constants are after the state and the MDS
  return (ctxt->state + ctxt->state_size + ctxt->state_size * ctxt->state_size);
}

void poseidon_apply_sbox(poseidon_ctxt_t *ctxt, int full) {
  blst_fr *state = poseidon_get_state_from_context(ctxt);
  int state_size = poseidon_get_state_size_from_context(ctxt);
  blst_fr buffer;
  int partial_round_idx_sbox = state_size - 1;
  int begin_idx = full ? 0 : partial_round_idx_sbox;
  int end_idx = state_size;
  for (int i = begin_idx; i < end_idx; i++) {
    // x * (x^2)^2
    blst_fr_sqr(&buffer, state + i);
    blst_fr_sqr(&buffer, &buffer);
    blst_fr_mul(state + i, &buffer, state + i);
  }
}

void poseidon_apply_matrix_multiplication(poseidon_ctxt_t *ctxt) {
  int state_size = poseidon_get_state_size_from_context(ctxt);
  blst_fr buffer;
  blst_fr res[state_size];
  blst_fr *state = poseidon_get_state_from_context(ctxt);
  blst_fr *mds = poseidon_get_mds_from_context(ctxt);
  for (int i = 0; i < state_size; i++) {
    for (int j = 0; j < state_size; j++) {
      if (j == 0) {
        blst_fr_mul(res + i, mds + i * state_size + j, state + j);
      } else {
        blst_fr_mul(&buffer, mds + i * state_size + j, state + j);
        blst_fr_add(res + i, res + i, &buffer);
      }
    }
  }
  for (int i = 0; i < state_size; i++) {
    memcpy(state + i, res + i, sizeof(blst_fr));
  }
}

int poseidon_apply_cst(poseidon_ctxt_t *ctxt, int offset) {
  int state_size = poseidon_get_state_size_from_context(ctxt);
  blst_fr *state = poseidon_get_state_from_context(ctxt);
  blst_fr *ark = poseidon_get_round_constants_from_context(ctxt);
  for (int i = 0; i < state_size; i++) {
    blst_fr_add(state + i, state + i, ark + offset++);
  }
  return (offset);
}

int poseidon_apply_batched_partial_round(poseidon_ctxt_t *ctxt,
                                         int offset_ark) {
  int batch_size = ctxt->batch_size;
  int state_size = poseidon_get_state_size_from_context(ctxt);
  // FIXME: if batch_size is 0, fails
  int nb_tmp_var = batch_size - 1;
  blst_fr buffer;
  blst_fr *ark = poseidon_get_round_constants_from_context(ctxt);
  blst_fr *state = poseidon_get_state_from_context(ctxt);
  blst_fr intermediary_state[state_size + nb_tmp_var];
  for (int i = 0; i < state_size; i++) {
    memcpy(intermediary_state + i, state + i, sizeof(blst_fr));
  }

  // Apply sbox on the last element of the state
  blst_fr_sqr(&buffer, intermediary_state + state_size - 1);
  blst_fr_sqr(&buffer, &buffer);
  blst_fr_mul(intermediary_state + state_size - 1, &buffer,
              intermediary_state + state_size - 1);

  // Computing the temporary variables
  for (int i = 0; i < nb_tmp_var; i++) {
    // we start with the first element
    blst_fr_mul(intermediary_state + state_size + i, ark + offset_ark++,
                intermediary_state);
    for (int j = 1; j < state_size + i; j++) {
      blst_fr_mul(&buffer, ark + offset_ark++, intermediary_state + j);
      blst_fr_add(intermediary_state + state_size + i,
                  intermediary_state + state_size + i, &buffer);
    }
    // We add the constant
    blst_fr_add(intermediary_state + state_size + i,
                intermediary_state + state_size + i, ark + offset_ark++);

    // Applying sbox
    blst_fr_sqr(&buffer, intermediary_state + i + state_size);
    blst_fr_sqr(&buffer, &buffer);
    blst_fr_mul(intermediary_state + i + state_size, &buffer,
                intermediary_state + i + state_size);
  }

  // Computing the final state
  for (int i = 0; i < state_size; i++) {
    blst_fr_mul(state + i, ark + offset_ark++, intermediary_state);
    for (int j = 1; j < state_size + nb_tmp_var; j++) {
      blst_fr_mul(&buffer, intermediary_state + j, ark + offset_ark++);
      blst_fr_add(state + i, &buffer, state + i);
    }
    blst_fr_add(state + i, state + i, ark + offset_ark++);
  }
  return (offset_ark);
}

void poseidon_apply_permutation(poseidon_ctxt_t *ctxt) {
  int nb_batched_partial_rounds = ctxt->nb_partial_rounds / ctxt->batch_size;
  int nb_unbatched_partial_rounds = ctxt->nb_partial_rounds % ctxt->batch_size;
  int offset_ark = 0;
  offset_ark = poseidon_apply_cst(ctxt, offset_ark);
  for (int i = 0; i < ctxt->nb_full_rounds / 2; i++) {
    poseidon_apply_sbox(ctxt, 1);
    poseidon_apply_matrix_multiplication(ctxt);
    offset_ark = poseidon_apply_cst(ctxt, offset_ark);
  }
  for (int i = 0; i < nb_batched_partial_rounds; i++) {
    offset_ark = poseidon_apply_batched_partial_round(ctxt, offset_ark);
  }
  for (int i = 0; i < nb_unbatched_partial_rounds; i++) {
    poseidon_apply_sbox(ctxt, 0);
    poseidon_apply_matrix_multiplication(ctxt);
    offset_ark = poseidon_apply_cst(ctxt, offset_ark);
  }
  for (int i = 0; i < ctxt->nb_full_rounds / 2; i++) {
    poseidon_apply_sbox(ctxt, 1);
    poseidon_apply_matrix_multiplication(ctxt);
    offset_ark = poseidon_apply_cst(ctxt, offset_ark);
  }
}
