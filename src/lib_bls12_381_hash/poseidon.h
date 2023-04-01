#ifndef POSEIDON_H
#define POSEIDON_H

#include "blst.h"
#include <stdlib.h>
#include <string.h>

typedef struct poseidon_ctxt_s {
  // Containts the state, the MDS and the constants
  blst_fr *state;
  int nb_full_rounds;
  int nb_partial_rounds;
  int batch_size;
  int state_size;
} poseidon_ctxt_t;

int poseidon_compute_number_of_constants(int batch_size, int nb_partial_rounds,
                                         int nb_full_rounds, int width);

void poseidon_apply_permutation(poseidon_ctxt_t *ctxt);

blst_fr *poseidon_get_state_from_context(poseidon_ctxt_t *ctxt);

int poseidon_get_state_size_from_context(poseidon_ctxt_t *ctxt);

blst_fr *poseidon_get_mds_from_context(poseidon_ctxt_t *ctxt);

blst_fr *poseidon_get_round_constants_from_context(poseidon_ctxt_t *ctxt);

#endif
