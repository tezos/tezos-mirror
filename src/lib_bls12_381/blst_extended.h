// Include this file in blst.h or cherry-pick 95dfbdd when updating blst

#ifndef BLST_EXTENDED_H
#define BLST_EXTENDED_H

#include "blst.h"

bool blst_fr_is_zero(const blst_fr *a);
bool blst_fr_is_one(const blst_fr *a);
bool blst_fr_is_equal(const blst_fr *a, const blst_fr *b);
void blst_fr_set_to_zero(blst_fr *a);
void blst_fr_set_to_one(blst_fr *a);

void blst_p1s_mult_pippenger_cont(blst_p1 *ret, const blst_p1_affine points[],
                                  size_t npoints, const byte scalars[],
                                  size_t nbits, limb_t *scratch);

void blst_p2s_mult_pippenger_cont(blst_p2 *ret, const blst_p2_affine points[],
                                  size_t npoints, const byte scalars[],
                                  size_t nbits, limb_t *scratch);

#endif
