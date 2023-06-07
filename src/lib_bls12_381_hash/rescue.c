#include "rescue.h"
#include <stdlib.h>
#include <string.h>

blst_fr *rescue_get_state_from_context(rescue_ctxt_t *ctxt) {
  return (ctxt->state);
}

int rescue_get_state_size_from_context(rescue_ctxt_t *ctxt) {
  return (ctxt->state_size);
}

blst_fr *rescue_get_mds_from_context(rescue_ctxt_t *ctxt) {
  // contstants stars after alpha_betas and the state
  return (ctxt->state + ctxt->state_size);
}

blst_fr *rescue_get_round_constants_from_context(rescue_ctxt_t *ctxt) {
  // contstants stars after alpha_betas and the state
  return (ctxt->state + ctxt->state_size + ctxt->state_size * ctxt->state_size);
}

int rescue_get_number_of_constants_from_context(rescue_ctxt_t *ctxt) {
  return (ctxt->state_size * ctxt->nb_rounds * 2);
}

void marvellous_apply_nonlinear_alpha(rescue_ctxt_t *ctxt) {
  blst_fr *state = rescue_get_state_from_context(ctxt);

  blst_fr buffer;
  for (int i = 0; i < ctxt->state_size; i++) {
    // x * (x^2)^2
    blst_fr_sqr(&buffer, state + i);
    blst_fr_sqr(&buffer, &buffer);
    blst_fr_mul(state + i, &buffer, state + i);
  }
}

void marvellous_apply_nonlinear_alphainv_addchain(blst_fr *res, blst_fr *x) {
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

void marvellous_apply_nonlinear_alphainv(rescue_ctxt_t *ctxt) {
  blst_fr *state = rescue_get_state_from_context(ctxt);

  for (int i = 0; i < ctxt->state_size; i++) {
    marvellous_apply_nonlinear_alphainv_addchain(state + i, state + i);
  }
}

void marvellous_apply_linear(rescue_ctxt_t *ctxt) {
  blst_fr *state = rescue_get_state_from_context(ctxt);
  blst_fr *mds = rescue_get_mds_from_context(ctxt);

  blst_fr buffer;
  blst_fr res[ctxt->state_size];
  for (int i = 0; i < ctxt->state_size; i++) {
    for (int j = 0; j < ctxt->state_size; j++) {
      if (j == 0) {
        blst_fr_mul(res + i, mds + i * ctxt->state_size + j, state + j);
      } else {
        blst_fr_mul(&buffer, mds + i * ctxt->state_size + j, state + j);
        blst_fr_add(res + i, res + i, &buffer);
      }
    }
  }
  for (int i = 0; i < ctxt->state_size; i++) {
    memcpy(state + i, res + i, sizeof(blst_fr));
  }
}

int marvellous_apply_cst(rescue_ctxt_t *ctxt, int i_round_key) {
  blst_fr *state = rescue_get_state_from_context(ctxt);
  blst_fr *round_constants = rescue_get_round_constants_from_context(ctxt);
  for (int i = 0; i < ctxt->state_size; i++) {
    blst_fr_add(state + i, state + i, round_constants + i_round_key++);
  }
  return (i_round_key);
}

void marvellous_apply_permutation(rescue_ctxt_t *ctxt) {
  int i_round_key = 0;

  for (int i = 0; i < ctxt->nb_rounds; i++) {
    marvellous_apply_nonlinear_alpha(ctxt);
    marvellous_apply_linear(ctxt);
    i_round_key = marvellous_apply_cst(ctxt, i_round_key);
    marvellous_apply_nonlinear_alphainv(ctxt);
    marvellous_apply_linear(ctxt);
    i_round_key = marvellous_apply_cst(ctxt, i_round_key);
  }
}
