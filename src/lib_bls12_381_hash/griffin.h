#ifndef GRIFFIN_H
#define GRIFFIN_H

#include "blst.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

typedef struct griffin_ctxt_s {
  // State. Contains the actual state, the alpha beta and the round constants
  blst_fr *state;
  // state_size := m = 2 * l
  int state_size;
  int nb_rounds;
} griffin_ctxt_t;

/** Apply a permutation on the context */
void griffin_apply_permutation(griffin_ctxt_t *ctxt);

/** Apply one round on the context */
int griffin_apply_one_round(griffin_ctxt_t *ctxt, int i_round_key);

/** Return a pointer to the state in the context */
blst_fr *griffin_get_state_from_context(griffin_ctxt_t *ctxt);

/** Set the state of the context to the given state */
void griffin_set_state_from_context(griffin_ctxt_t *ctxt, blst_fr *state);

/** Return the state size of the context */
int griffin_get_state_size_from_context(griffin_ctxt_t *ctxt);

/** Return a pointer to the alpha/beta's present in the context */
blst_fr *griffin_get_alpha_beta_from_context(griffin_ctxt_t *ctxt);

/** Return a pointer to the round constants present in the context */
blst_fr *griffin_get_round_constants_from_context(griffin_ctxt_t *ctxt);

/**
  Allocate a new griffin context.
  If the state size is not 3 or a multiple of 4, return NULL.
  Return NULL if one stack allocation went wrong.
*/
griffin_ctxt_t *griffin_allocate_context(int state_size, int nb_rounds,
                                         blst_fr *constants,
                                         blst_fr *alpha_beta_s);

void griffin_free_context(griffin_ctxt_t *ctxt);

#endif
