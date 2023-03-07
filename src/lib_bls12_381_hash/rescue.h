#ifndef RESCUE_H
#define RESCUE_H

#include "blst.h"
#include "blst_misc.h"

typedef struct rescue_ctxt_s {
  // Containts the state, the MDS and the constants
  blst_fr *state;
  int nb_rounds;
  int state_size;
} rescue_ctxt_t;

void marvellous_apply_permutation(rescue_ctxt_t *ctxt);

blst_fr *rescue_get_state_from_context(rescue_ctxt_t *ctxt);

blst_fr *rescue_get_mds_from_context(rescue_ctxt_t *ctxt);

blst_fr *rescue_get_round_constants_from_context(rescue_ctxt_t *ctxt);

int rescue_get_state_size_from_context(rescue_ctxt_t *ctxt);

int rescue_get_number_of_constants_from_context(rescue_ctxt_t *ctxt);

#endif
