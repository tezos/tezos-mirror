#include "anemoi.h"
#include <stdio.h>
#include <stdlib.h>

int anemoi_get_state_size_from_context(anemoi_ctxt_t *ctxt) {
  // shift by state size
  return (2 * ctxt->l);
}

blst_fr *anemoi_get_state_from_context(anemoi_ctxt_t *ctxt) {
  // shift by state size
  return (ctxt->ctxt);
}

blst_fr *anemoi_get_mds_from_context(anemoi_ctxt_t *ctxt) {
  // shift by state size
  return (ctxt->ctxt + 2 * ctxt->l);
}

blst_fr *anemoi_get_round_constants_from_context(anemoi_ctxt_t *ctxt) {
  // shift by state size and MDS
  int state_size = anemoi_get_state_size_from_context(ctxt);
  return (ctxt->ctxt + state_size + ctxt->l * ctxt->l);
}

int anemoi_get_number_of_constants(anemoi_ctxt_t *ctxt) {
  return (ctxt->l * ctxt->nb_rounds * 2);
}

blst_fr *anemoi_get_beta_from_context(anemoi_ctxt_t *ctxt) {
  int nb_constants = anemoi_get_number_of_constants(ctxt);
  int state_size = anemoi_get_state_size_from_context(ctxt);
  return (ctxt->ctxt + state_size + ctxt->l * ctxt->l + nb_constants);
}

blst_fr *anemoi_get_delta_from_context(anemoi_ctxt_t *ctxt) {
  int nb_constants = anemoi_get_number_of_constants(ctxt);
  int state_size = anemoi_get_state_size_from_context(ctxt);
  return (ctxt->ctxt + state_size + ctxt->l * ctxt->l + nb_constants + 1);
}

void anemoi_fr_multiply_by_g(blst_fr *res, blst_fr *v) {
  blst_fr tmp;

  // Compute g * y and save it in tmp.
  // multiply by 7
  // y + y
  blst_fr_add(&tmp, v, v);
  // 2y + y
  blst_fr_add(&tmp, &tmp, v);
  // 3y + 3y
  blst_fr_add(&tmp, &tmp, &tmp);
  // 6y + y
  blst_fr_add(res, &tmp, v);
}

void blst_fr_double(blst_fr *r, blst_fr *x) { blst_fr_add(r, x, x); }

void anemoi_apply_constants_addition(anemoi_ctxt_t *ctxt, int round) {
  blst_fr *state = anemoi_get_state_from_context(ctxt);
  blst_fr *state_x = state;
  blst_fr *state_y = state + ctxt->l;
  blst_fr *constants = anemoi_get_round_constants_from_context(ctxt);
  blst_fr *constants_x = constants;
  blst_fr *constants_y = constants + ctxt->nb_rounds * ctxt->l;
  for (int i = 0; i < ctxt->l; i++) {
    blst_fr_add(state_x + i, state_x + i, constants_x + ctxt->l * round + i);
    blst_fr_add(state_y + i, state_y + i, constants_y + ctxt->l * round + i);
  }
}

void anemoi_apply_shift_state(anemoi_ctxt_t *ctxt) {
  blst_fr *state = anemoi_get_state_from_context(ctxt);
  blst_fr *state_y = state + ctxt->l;
  blst_fr tmp;

  memcpy(&tmp, state_y, sizeof(blst_fr));
  // And we apply the rotation
  // y_(i) <- y_(i + 1)
  for (int i = 0; i < ctxt->l - 1; i++) {
    memcpy(state_y + i, state_y + i + 1, sizeof(blst_fr));
  }
  // Put y_0 into y_(l - 1)
  memcpy(state_y + ctxt->l - 1, &tmp, sizeof(blst_fr));
}

void anemoi_1_apply_linear_layer(anemoi_ctxt_t *ctxt) {
  blst_fr tmp;
  blst_fr *state = anemoi_get_state_from_context(ctxt);

  // Compute "g * y' and save it in tmp.
  anemoi_fr_multiply_by_g(&tmp, state + 1);
  // x += g * y. Inplace operation
  blst_fr_add(state, state, &tmp);

  // Compute "g * x' and save it in tmp.
  anemoi_fr_multiply_by_g(&tmp, state);

  blst_fr_add(state + 1, state + 1, &tmp);
}

void anemoi_2_apply_linear_layer(anemoi_ctxt_t *ctxt) {
  blst_fr *state_x = anemoi_get_state_from_context(ctxt);
  blst_fr *state_y = state_x + ctxt->l;
  blst_fr tmp;

  // Apply M_x
  // Compute "g * y' and save it in tmp.
  anemoi_fr_multiply_by_g(&tmp, state_x + 1);
  // x += g * y. Inplace operation
  blst_fr_add(state_x, state_x, &tmp);

  // Compute "g * x' and save it in tmp.
  anemoi_fr_multiply_by_g(&tmp, state_x);

  blst_fr_add(state_x + 1, state_x + 1, &tmp);

  // swap y_1 et y_0 for linear layer
  memcpy(&tmp, state_y, sizeof(blst_fr));
  memcpy(state_y, state_y + 1, sizeof(blst_fr));
  memcpy(state_y + 1, &tmp, sizeof(blst_fr));

  // Apply M_y
  // Compute "g * y' and save it in tmp.
  anemoi_fr_multiply_by_g(&tmp, state_y + 1);
  // x += g * y. Inplace operation
  blst_fr_add(state_y, state_y, &tmp);

  // Compute "g * x' and save it in tmp.
  anemoi_fr_multiply_by_g(&tmp, state_y);

  blst_fr_add(state_y + 1, state_y + 1, &tmp);
}

// l = 3
void anemoi_3_apply_matrix(blst_fr *ctxt) {
  blst_fr tmp;
  blst_fr g_x0;

  // t = x[0] + g * x[2]
  anemoi_fr_multiply_by_g(&tmp, ctxt + 2);
  blst_fr_add(&tmp, &tmp, ctxt);

  // x[2] += x[1]
  blst_fr_add(ctxt + 2, ctxt + 2, ctxt + 1);
  // x[2] += b * x[0]
  anemoi_fr_multiply_by_g(&g_x0, ctxt);
  blst_fr_add(ctxt + 2, ctxt + 2, &g_x0);

  // x[0] = t + x[2]
  blst_fr_add(ctxt, ctxt + 2, &tmp);
  // x[1] += t
  blst_fr_add(ctxt + 1, ctxt + 1, &tmp);
}

void anemoi_3_apply_linear_layer(anemoi_ctxt_t *ctxt) {
  blst_fr *state = anemoi_get_state_from_context(ctxt);
  blst_fr *state_x = state;
  blst_fr *state_y = state_x + ctxt->l;

  anemoi_3_apply_matrix(state_x);
  anemoi_apply_shift_state(ctxt);
  anemoi_3_apply_matrix(state_y);
}

// l = 4
void anemoi_4_apply_matrix(blst_fr *ctxt) {
  blst_fr tmp;

  // x[0] += x[1]
  blst_fr_add(ctxt, ctxt, ctxt + 1);
  // x[2] += x[3]
  blst_fr_add(ctxt + 2, ctxt + 2, ctxt + 3);
  // x[3] += g x[0]
  anemoi_fr_multiply_by_g(&tmp, ctxt);
  blst_fr_add(ctxt + 3, ctxt + 3, &tmp);
  // x[1] = g * (x[1] + x[2])
  blst_fr_add(&tmp, ctxt + 1, ctxt + 2);
  anemoi_fr_multiply_by_g(ctxt + 1, &tmp);
  // x[0] += x[1]
  blst_fr_add(ctxt, ctxt, ctxt + 1);
  // x[2] += g x[3]
  anemoi_fr_multiply_by_g(&tmp, ctxt + 3);
  blst_fr_add(ctxt + 2, ctxt + 2, &tmp);
  // x[1] += x[2]
  blst_fr_add(ctxt + 1, ctxt + 1, ctxt + 2);
  // x[3] += x[0]
  blst_fr_add(ctxt + 3, ctxt + 3, ctxt);
}

void anemoi_4_apply_linear_layer(anemoi_ctxt_t *ctxt) {
  blst_fr *state = anemoi_get_state_from_context(ctxt);
  blst_fr *state_x = state;
  blst_fr *state_y = state_x + ctxt->l;

  anemoi_4_apply_matrix(state_x);
  anemoi_apply_shift_state(ctxt);
  anemoi_4_apply_matrix(state_y);
}

void anemoi_addchain_alpha_inv(blst_fr *res, blst_fr *x) {
  // Allocating on the stack 32 bytes * 36 (= 1152 bytes) for intermediary
  // variables to compute the addition chain. Less values might be required.
  // There is still place for improvements.
  // Values are allocated in a contiguous array to hope using the CPU cache.
  blst_fr tmp[36];
  // _10      = 2*1
  //   -> tmp = x * x
  blst_fr_sqr(tmp, x);
  // _100     = 2*_10
  //   -> tmp1 = tmp * tmp
  blst_fr_sqr(tmp + 1, tmp);
  // _101     = 1 + _100
  //   -> tmp2 = x * tmp1
  blst_fr_mul(tmp + 2, x, tmp + 1);
  // _111     = _10 + _101
  //   -> tmp3 = tmp * tmp2
  blst_fr_mul(tmp + 3, tmp, tmp + 2);
  // _1010    = 2*_101
  //   -> tmp4 = tmp2 * tmp2
  blst_fr_sqr(tmp + 4, tmp + 2);
  // _1011    = 1 + _1010
  //   -> tmp5 = x * tmp4
  blst_fr_mul(tmp + 5, x, tmp + 4);
  // _1101    = _10 + _1011
  //   -> tmp6 = tmp * tmp5
  blst_fr_mul(tmp + 6, tmp, tmp + 5);
  // _1111    = _10 + _1101
  //   -> tmp7 = tmp * tmp6
  blst_fr_mul(tmp + 7, tmp, tmp + 6);
  // _10001   = _10 + _1111
  //   -> tmp8 = tmp * tmp7
  blst_fr_mul(tmp + 8, tmp, tmp + 7);
  // _10011   = _10 + _10001
  //   -> tmp9 = tmp * tmp8
  blst_fr_mul(tmp + 9, tmp, tmp + 8);
  // _10111   = _100 + _10011
  //   -> tmp10 = tmp1 * tmp9
  blst_fr_mul(tmp + 10, tmp + 1, tmp + 9);
  // _100001  = _1010 + _10111
  //   -> tmp11 = tmp4 * tmp10
  blst_fr_mul(tmp + 11, tmp + 4, tmp + 10);
  // _100011  = _10 + _100001
  //   -> tmp12 = tmp * tmp11
  blst_fr_mul(tmp + 12, tmp, tmp + 11);
  // _100101  = _10 + _100011
  //   -> tmp13 = tmp * tmp12
  blst_fr_mul(tmp + 13, tmp, tmp + 12);
  // _100111  = _10 + _100101
  //   -> tmp14 = tmp * tmp13
  blst_fr_mul(tmp + 14, tmp, tmp + 13);
  // _101011  = _100 + _100111
  //   -> tmp15 = tmp1 * tmp14
  blst_fr_mul(tmp + 15, tmp + 1, tmp + 14);
  // _101111  = _100 + _101011
  //   -> tmp16 = tmp1 * tmp15
  blst_fr_mul(tmp + 16, tmp + 1, tmp + 15);
  // _110011  = _100 + _101111
  //   -> tmp17 = tmp1 * tmp16
  blst_fr_mul(tmp + 17, tmp + 1, tmp + 16);
  // _110101  = _10 + _110011
  //   -> tmp18 = tmp * tmp17
  blst_fr_mul(tmp + 18, tmp, tmp + 17);
  // _111001  = _100 + _110101
  //   -> tmp19 = tmp1 * tmp18
  blst_fr_mul(tmp + 19, tmp + 1, tmp + 18);
  // _111101  = _100 + _111001
  //   -> tmp20 = tmp1 * tmp19
  blst_fr_mul(tmp + 20, tmp + 1, tmp + 19);
  // _1011100 = _100011 + _111001
  //   -> tmp21 = tmp12 * tmp19
  blst_fr_mul(tmp + 21, tmp + 12, tmp + 19);
  // i42      = ((_1011100 << 6 + _101111) << 6 + _100001) << 6
  //   -> _1011100 << 6
  //      tmp22 = tmp21^(2^6)
  memcpy(tmp + 22, tmp + 21, sizeof(blst_fr));
  for (int i = 0; i < 6; i++) {
    blst_fr_sqr(tmp + 22, tmp + 22);
  }
  //   -> (_1011100 << 6 + _101111
  //      tmp22 = tmp21^(2^6) * tmp16
  //            = tmp22 * tmp16
  blst_fr_mul(tmp + 22, tmp + 22, tmp + 16);
  //   -> (_1011100 << 6 + _101111) << 6
  //      tmp22 = (tmp21^(2^6) * tmp16)^(2^6)
  //            = tmp22^(2^6)
  for (int i = 0; i < 6; i++) {
    blst_fr_sqr(tmp + 22, tmp + 22);
  }
  //   -> ((_1011100 << 6 + _101111) << 6 + _100001)
  //      tmp22 = ((tmp21^(2^6) * tmp16)^(2^6)) * tmp11
  //            = tmp22 * tmp11
  blst_fr_mul(tmp + 22, tmp + 22, tmp + 11);
  //   -> ((_1011100 << 6 + _101111) << 6 + _100001) << 6
  //      tmp22 = (((tmp21^(2^6) * tmp16)^(2^6)) * tmp11)^(2^6)
  //            = tmp22^(2^6)
  for (int i = 0; i < 6; i++) {
    blst_fr_sqr(tmp + 22, tmp + 22);
  }
  //   i56      = ((_111101 + i42) << 6 + _110101) << 5 + _10111
  //   -> (_111101 + i42)
  //      tmp23 = tmp20 * tmp22
  blst_fr_mul(tmp + 23, tmp + 22, tmp + 20);
  //   -> (_111101 + i42) << 6
  //      tmp23 = (tmp20 * tmp22)^(2^6)
  //            = tmp22^(2^6)
  for (int i = 0; i < 6; i++) {
    blst_fr_sqr(tmp + 23, tmp + 23);
  }
  //   -> ((_111101 + i42) << 6 + _110101)
  //      tmp23 = tmp22 * tmp18
  blst_fr_mul(tmp + 23, tmp + 23, tmp + 18);
  //   -> ((_111101 + i42) << 6 + _110101) << 5
  //      tmp23 = tmp22^(2^5)
  for (int i = 0; i < 5; i++) {
    blst_fr_sqr(tmp + 23, tmp + 23);
  }
  //   -> ((_111101 + i42) << 6 + _110101) << 5 + _10111
  //      tmp23 = tmp22 * tmp10
  blst_fr_mul(tmp + 23, tmp + 10, tmp + 23);
  //   tmp + 24
  //   i80      = ((i56 << 6 + _10111) << 8 + _100011) << 8
  memcpy(tmp + 24, tmp + 23, sizeof(blst_fr));
  //   -> i56 << 6
  //      tmp24 = tmp23^(2^6)
  for (int i = 0; i < 6; i++) {
    blst_fr_sqr(tmp + 24, tmp + 24);
  }
  //   -> (i56 << 6 + _10111)
  //      tmp24 = tmp24 * tmp10
  blst_fr_mul(tmp + 24, tmp + 24, tmp + 10);
  //   -> (i56 << 6 + _10111) << 8
  //      tmp24 = tmp24^(2^8)
  for (int i = 0; i < 8; i++) {
    blst_fr_sqr(tmp + 24, tmp + 24);
  }
  //   -> (i56 << 6 + _10111) << 8 + _100011
  //      tmp24 = tmp24 * tmp12
  blst_fr_mul(tmp + 24, tmp + 24, tmp + 12);
  //   -> ((i56 << 6 + _10111) << 8 + _100011) << 8
  //      tmp24 = tmp24^(2^8)
  for (int i = 0; i < 8; i++) {
    blst_fr_sqr(tmp + 24, tmp + 24);
  }
  //   tmp + 25
  //   i95      = ((_100001 + i80) << 6 + _110011) << 6 + _100001
  //   -> (_100001 + i80)
  //      tmp25 = tmp24 * tmp11
  blst_fr_mul(tmp + 25, tmp + 11, tmp + 24);
  //   -> (_100001 + i80) << 6
  //      tmp25 = tmp25^(2^6)
  for (int i = 0; i < 6; i++) {
    blst_fr_sqr(tmp + 25, tmp + 25);
  }
  //   -> (_100001 + i80) << 6 + _110011
  //      tmp25 = tmp25 * tmp17
  blst_fr_mul(tmp + 25, tmp + 17, tmp + 25);
  //   -> ((_100001 + i80) << 6 + _110011) << 6
  //      tmp25 = tmp25^(2^6)
  for (int i = 0; i < 6; i++) {
    blst_fr_sqr(tmp + 25, tmp + 25);
  }
  //   -> ((_100001 + i80) << 6 + _110011) << 6 + _100001
  //      tmp25 = tmp25 * tmp11
  blst_fr_mul(tmp + 25, tmp + 25, tmp + 11);
  //   i118     = ((i95 << 7 + _100101) << 8 + _101011) << 6
  //   -> i95 << 7
  //      tmp26 = tmp25^(2^7)
  memcpy(tmp + 26, tmp + 25, sizeof(blst_fr));
  for (int i = 0; i < 7; i++) {
    blst_fr_sqr(tmp + 26, tmp + 26);
  }
  //   -> (i95 << 7 + _100101)
  //      tmp26 = tmp26 * tmp13
  blst_fr_mul(tmp + 26, tmp + 26, tmp + 13);
  //   -> (i95 << 7 + _100101) << 8
  //      tmp26 = tmp26^(2^8)
  for (int i = 0; i < 8; i++) {
    blst_fr_sqr(tmp + 26, tmp + 26);
  }
  //   -> ((i95 << 7 + _100101) << 8 + _101011)
  //      tmp26 = tmp26 * tmp15
  blst_fr_mul(tmp + 26, tmp + 26, tmp + 15);
  //   -> ((i95 << 7 + _100101) << 8 + _101011) << 6
  //      tmp26 = tmp26^(2^6)
  for (int i = 0; i < 6; i++) {
    blst_fr_sqr(tmp + 26, tmp + 26);
  }
  //   tmp + 27
  //   i135     = ((_1101 + i118) << 8 + _110011) << 6 + _101011
  //   -> _1101 + i118
  //      tmp27 = tmp26 * tmp6
  blst_fr_mul(tmp + 27, tmp + 26, tmp + 6);
  //   -> (_1101 + i118) << 8
  //      tmp27 = tmp27^(2^8)
  for (int i = 0; i < 8; i++) {
    blst_fr_sqr(tmp + 27, tmp + 27);
  }
  //   -> (_1101 + i118) << 8 + _110011
  //      tmp27 = tmp27 * tmp17
  blst_fr_mul(tmp + 27, tmp + 27, tmp + 17);
  //   -> ((_1101 + i118) << 8 + _110011) << 6
  //      tmp27 = tmp27^(2^6)
  for (int i = 0; i < 6; i++) {
    blst_fr_sqr(tmp + 27, tmp + 27);
  }
  //   -> ((_1101 + i118) << 8 + _110011) << 6 + _101011
  //      tmp27 = tmp27 * tmp15
  blst_fr_mul(tmp + 27, tmp + 27, tmp + 15);
  //   tmp + 28
  //   i161     = ((i135 << 6 + _100111) << 3 + _111) << 15
  //      tmp28 = tmp27^(2^6)
  memcpy(tmp + 28, tmp + 27, sizeof(blst_fr));
  for (int i = 0; i < 6; i++) {
    blst_fr_sqr(tmp + 28, tmp + 28);
  }
  //   -> (i135 << 6 + _100111)
  //      tmp28 = tmp28 * tmp14
  blst_fr_mul(tmp + 28, tmp + 28, tmp + 14);
  //   -> (i135 << 6 + _100111) << 3
  //      tmp28 = tmp28^(2^3)
  for (int i = 0; i < 3; i++) {
    blst_fr_sqr(tmp + 28, tmp + 28);
  }
  //   -> ((i135 << 6 + _100111) << 3 + _111)
  //      tmp28 = tmp28 * tmp3
  blst_fr_mul(tmp + 28, tmp + 28, tmp + 3);
  //   -> ((i135 << 6 + _100111) << 3 + _111) << 15
  //      tmp28 = tmp28^(2^15)
  for (int i = 0; i < 15; i++) {
    blst_fr_sqr(tmp + 28, tmp + 28);
  }
  //   tmp + 29
  //   i176     = ((_10001 + i161) << 5 + 1) << 7 + _111101
  //   -> _10001 + i161
  //      tmp29 = tmp8 * tmp28
  blst_fr_mul(tmp + 29, tmp + 8, tmp + 28);
  //   -> (_10001 + i161) << 5
  //      tmp29 = tmp29^(2^5)
  for (int i = 0; i < 5; i++) {
    blst_fr_sqr(tmp + 29, tmp + 29);
  }
  //      tmp29 = tmp29 * x
  //   -> ((_10001 + i161) << 5 + 1)
  blst_fr_mul(tmp + 29, tmp + 29, x);
  //      tmp29 = tmp29^(2^7)
  //   -> ((_10001 + i161) << 5 + 1) << 7
  for (int i = 0; i < 7; i++) {
    blst_fr_sqr(tmp + 29, tmp + 29);
  }
  //   -> ((_10001 + i161) << 5 + 1) << 7 + _111101
  //      tmp29 = tmp29 * tmp20
  blst_fr_mul(tmp + 29, tmp + 29, tmp + 20);
  //   i195     = ((2*i176 + _101) << 10 + _111001) << 6
  //   -> 2*i176
  //      tmp30 = tmp29^2
  blst_fr_sqr(tmp + 30, tmp + 29);
  //   -> (2*i176 + _101)
  //      tmp30 = tmp30 * tmp2
  blst_fr_mul(tmp + 30, tmp + 30, tmp + 2);
  //   -> (2*i176 + _101) << 10
  for (int i = 0; i < 10; i++) {
    blst_fr_sqr(tmp + 30, tmp + 30);
  }
  //   -> ((2*i176 + _101) << 10 + _111001)
  blst_fr_mul(tmp + 30, tmp + 30, tmp + 19);
  //   -> ((2*i176 + _101) << 10 + _111001) << 6
  for (int i = 0; i < 6; i++) {
    blst_fr_sqr(tmp + 30, tmp + 30);
  }
  //   i211     = ((_100111 + i195) << 5 + _10011) << 8 + _110011
  blst_fr_mul(tmp + 31, tmp + 30, tmp + 14);
  // (_100111 + i195) << 5
  for (int i = 0; i < 5; i++) {
    blst_fr_sqr(tmp + 31, tmp + 31);
  }
  //   -> ((_100111 + i195) << 5 + _10011)
  blst_fr_mul(tmp + 31, tmp + 31, tmp + 9);
  //   -> ((_100111 + i195) << 5 + _10011) << 8
  for (int i = 0; i < 8; i++) {
    blst_fr_sqr(tmp + 31, tmp + 31);
  }
  //   -> ((_100111 + i195) << 5 + _10011) << 8 + _110011
  blst_fr_mul(tmp + 31, tmp + 31, tmp + 17);
  //   tmp + 32
  //   i236     = ((i211 << 7 + _1111) << 9 + _110011) << 7
  memcpy(tmp + 32, tmp + 31, sizeof(blst_fr));
  //   -> i211 << 7
  for (int i = 0; i < 7; i++) {
    blst_fr_sqr(tmp + 32, tmp + 32);
  }
  //  -> (i211 << 7 + _1111)
  blst_fr_mul(tmp + 32, tmp + 32, tmp + 7);
  //  -> (i211 << 7 + _1111) << 9
  for (int i = 0; i < 9; i++) {
    blst_fr_sqr(tmp + 32, tmp + 32);
  }
  //  -> ((i211 << 7 + _1111) << 9 + _110011)
  blst_fr_mul(tmp + 32, tmp + 32, tmp + 17);
  //  -> ((i211 << 7 + _1111) << 9 + _110011) << 7
  for (int i = 0; i < 7; i++) {
    blst_fr_sqr(tmp + 32, tmp + 32);
  }
  //   tmp + 33
  //   i255     = ((_10011 + i236) << 8 + _110011) << 8 + _110011
  //   -> (_10011 + i236)
  blst_fr_mul(tmp + 33, tmp + 32, tmp + 9);
  //   -> (_10011 + i236) << 8
  for (int i = 0; i < 8; i++) {
    blst_fr_sqr(tmp + 33, tmp + 33);
  }
  //   -> ((_10011 + i236) << 8 + _110011)
  blst_fr_mul(tmp + 33, tmp + 33, tmp + 17);
  //   -> ((_10011 + i236) << 8 + _110011) << 8
  for (int i = 0; i < 8; i++) {
    blst_fr_sqr(tmp + 33, tmp + 33);
  }
  //   -> ((_10011 + i236) << 8 + _110011) << 8 + _110011
  blst_fr_mul(tmp + 33, tmp + 33, tmp + 17);
  //   tmp + 34
  //   i279     = ((i255 << 8 + _110011) << 6 + _1011) << 8
  memcpy(tmp + 34, tmp + 33, sizeof(blst_fr));
  //   -> i255 << 8
  for (int i = 0; i < 8; i++) {
    blst_fr_sqr(tmp + 34, tmp + 34);
  }
  //   -> (i255 << 8 + _110011)
  blst_fr_mul(tmp + 34, tmp + 34, tmp + 17);
  //   -> ((i255 << 8 + _110011) << 6
  for (int i = 0; i < 6; i++) {
    blst_fr_sqr(tmp + 34, tmp + 34);
  }
  //   -> ((i255 << 8 + _110011) << 6 + _1011)
  blst_fr_mul(tmp + 34, tmp + 34, tmp + 5);
  //   -> ((i255 << 8 + _110011) << 6 + _1011) << 8
  for (int i = 0; i < 8; i++) {
    blst_fr_sqr(tmp + 34, tmp + 34);
  }
  //   tmp + 35
  //   i298     = ((_110011 + i279) << 8 + _110011) << 8 + _110011
  //   -> _110011 + i279
  blst_fr_mul(tmp + 35, tmp + 17, tmp + 34);

  //   -> (_110011 + i279) << 8
  for (int i = 0; i < 8; i++) {
    blst_fr_sqr(tmp + 35, tmp + 35);
  }
  //   -> ((_110011 + i279) << 8 + _110011)
  blst_fr_mul(tmp + 35, tmp + 17, tmp + 35);
  //   -> ((_110011 + i279) << 8 + _110011) << 8
  for (int i = 0; i < 8; i++) {
    blst_fr_sqr(tmp + 35, tmp + 35);
  }
  //   -> ((_110011 + i279) << 8 + _110011) << 8 + _110011
  blst_fr_mul(tmp + 35, tmp + 17, tmp + 35);
  //   return     i298 << 6 + _1101
  for (int i = 0; i < 6; i++) {
    blst_fr_sqr(tmp + 35, tmp + 35);
  }
  // Put in res
  blst_fr_mul(res, tmp + 6, tmp + 35);
}

void anemoi_apply_s_box(blst_fr *x, blst_fr *y, blst_fr *beta, blst_fr *delta) {
  blst_fr tmp;
  // First we compute x_i = x_i - beta * y^2 = x_i - Q_i(y_i)
  // -- compute y^2
  blst_fr_sqr(&tmp, y);
  // -- Compute beta * y^2
  blst_fr_mul(&tmp, &tmp, beta);
  // -- Compute x = x - beta * y^2
  blst_fr_sub(x, x, &tmp);
  // Computing E(x)
  // -- Coppute x^alpha_inv and save it in tmp.
  // NB: this is the costly operation.
  // IMPROVEME: can be improved using addchain. Would be 21% faster (305 ops
  // instead of 384).
  // > addchain search
  // '20974350070050476191779096203274386335076221000211055129041463479975432473805'
  // > addition cost: 305
  anemoi_addchain_alpha_inv(&tmp, x);
  /* blst_fr_pow(&tmp, ctxt, ALPHA_INV_BYTES, ALPHA_INV_NUMBITS); */
  // -- Compute y_i = y_i - x^(alpha_inv) = y_i - E(x_i)
  blst_fr_sub(y, y, &tmp);
  // Computing x_i = x_i + (beta * y^2 + delta) = x_i + Q_f(x_i)
  // -- compute y^2
  blst_fr_sqr(&tmp, y);
  // -- compute beta * y^2
  blst_fr_mul(&tmp, &tmp, beta);
  // -- compute beta * y^2 + delta
  blst_fr_add(&tmp, &tmp, delta);
  // -- compute x + x + beta * y^2 + delta
  blst_fr_add(x, x, &tmp);
}

void anemoi_1_apply_flystel(anemoi_ctxt_t *ctxt) {
  blst_fr *beta = anemoi_get_beta_from_context(ctxt);
  blst_fr *delta = anemoi_get_delta_from_context(ctxt);
  blst_fr *state = anemoi_get_state_from_context(ctxt);
  anemoi_apply_s_box(state, state + 1, beta, delta);
}

void anemoi_1_apply(anemoi_ctxt_t *ctxt) {
  for (int i = 0; i < ctxt->nb_rounds; i++) {
    // add cst
    anemoi_apply_constants_addition(ctxt, i);
    // apply linear layer
    anemoi_1_apply_linear_layer(ctxt);
    // apply sbox
    anemoi_1_apply_flystel(ctxt);
  }

  // Final call to linear layer. See page 15, High Level Algorithms
  anemoi_1_apply_linear_layer(ctxt);
}

void anemoi_generic_apply_flystel(anemoi_ctxt_t *ctxt) {
  blst_fr *state = anemoi_get_state_from_context(ctxt);
  blst_fr *beta = anemoi_get_beta_from_context(ctxt);
  blst_fr *delta = anemoi_get_delta_from_context(ctxt);
  for (int i = 0; i < ctxt->l; i++) {
    anemoi_apply_s_box(state + i, state + ctxt->l + i, beta, delta);
  }
}

void anemoi_generic_apply_linear_layer(anemoi_ctxt_t *ctxt) {
  blst_fr *state = anemoi_get_state_from_context(ctxt);
  blst_fr *state_x = state;
  blst_fr *state_y = state + ctxt->l;
  blst_fr *mds = anemoi_get_mds_from_context(ctxt);

  blst_fr buffer[ctxt->l];

  blst_fr tmp;

  // Applying matrix multiplication
  for (int i = 0; i < ctxt->l; i++) {
    memset(buffer + i, 0, sizeof(blst_fr));
    for (int j = 0; j < ctxt->l; j++) {
      blst_fr_mul(&tmp, state_x + j, mds + j * ctxt->l + i);
      blst_fr_add(buffer + i, buffer + i, &tmp);
    }
  }

  // Copying the buffer into state
  for (int i = 0; i < ctxt->l; i++) {
    memcpy(state_x + i, buffer + i, sizeof(blst_fr));
  }

  memcpy(&tmp, state_y, sizeof(blst_fr));
  // And we apply the rotation
  // y_(i) <- y_(i + 1)
  for (int i = 0; i < ctxt->l - 1; i++) {
    memcpy(state_y + i, state_y + i + 1, sizeof(blst_fr));
  }
  // Put y_0 into y_(l - 1)
  memcpy(state_y + ctxt->l - 1, &tmp, sizeof(blst_fr));

  // Applying matrix multiplication
  for (int i = 0; i < ctxt->l; i++) {
    memset(buffer + i, 0, sizeof(blst_fr));
    for (int j = 0; j < ctxt->l; j++) {
      blst_fr_mul(&tmp, state_y + j, mds + j * ctxt->l + i);
      blst_fr_add(buffer + i, buffer + i, &tmp);
    }
  }

  // Copying the buffer into state
  for (int i = 0; i < ctxt->l; i++) {
    memcpy(state_y + i, buffer + i, sizeof(blst_fr));
  }
}

void anemoi_apply_flystel(anemoi_ctxt_t *ctxt) {
  if (ctxt->l == 1) {
    anemoi_1_apply_flystel(ctxt);
  } else
    anemoi_generic_apply_flystel(ctxt);
}

void anemoi_apply_linear_layer(anemoi_ctxt_t *ctxt) {
  if (ctxt->l == 1) {
    anemoi_1_apply_linear_layer(ctxt);
  }

  else if (ctxt->l == 2) {
    anemoi_2_apply_linear_layer(ctxt);
  }

  else if (ctxt->l == 3) {
    anemoi_3_apply_linear_layer(ctxt);
  }

  else if (ctxt->l == 4) {
    anemoi_4_apply_linear_layer(ctxt);
  }

  else {
    anemoi_generic_apply_linear_layer(ctxt);
  }
}

void anemoi_apply_one_round(anemoi_ctxt_t *ctxt, int round) {
  anemoi_apply_constants_addition(ctxt, round);
  anemoi_apply_linear_layer(ctxt);
  anemoi_apply_flystel(ctxt);
}

void anemoi_apply_permutation(anemoi_ctxt_t *ctxt) {
  for (int i = 0; i < ctxt->nb_rounds; i++) {
    anemoi_apply_one_round(ctxt, i);
  }

  anemoi_apply_linear_layer(ctxt);
}

void anemoi_set_state_from_context(anemoi_ctxt_t *ctxt, blst_fr *state) {
  int state_size = anemoi_get_state_size_from_context(ctxt);
  blst_fr *ctxt_state = anemoi_get_state_from_context(ctxt);

  for (int i = 0; i < state_size; i++) {
    memcpy(ctxt_state + i, state + i, sizeof(blst_fr));
  }
}

anemoi_ctxt_t *anemoi_allocate_context(int l, int nb_rounds) {
  // Returning null because we do not support bigger state size than 8 at the
  // moment
  if (l > 4 || l < 0) {
    return (NULL);
  }

  anemoi_ctxt_t *ctxt = malloc(sizeof(anemoi_ctxt_t));
  if (ctxt == NULL) {
    return (NULL);
  }
  blst_fr *state = malloc(sizeof(anemoi_ctxt_t));
  if (state == NULL) {
    free(ctxt);
    return (NULL);
  }

  ctxt->l = l;
  ctxt->nb_rounds = nb_rounds;
  ctxt->ctxt = state;

  return (ctxt);
}

void anemoi_free_context(anemoi_ctxt_t *ctxt) {
  if (ctxt != NULL) {
    free(ctxt->ctxt);
    free(ctxt);
  }
}
