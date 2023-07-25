#include "blst.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

size_t blst_scalar_sizeof() { return sizeof(blst_scalar); }

size_t blst_fr_sizeof() { return sizeof(blst_fr); }

int blst_fr_compare(blst_fr *s_c, blst_fr *t_c) {
  uint64_t s_uint64[4];
  uint64_t t_uint64[4];

  blst_uint64_from_fr(s_uint64, s_c);
  blst_uint64_from_fr(t_uint64, t_c);

  // Check first it is equal.
  bool is_equal = 1;
  is_equal = is_equal && (s_uint64[0] == t_uint64[0]);
  is_equal = is_equal && (s_uint64[1] == t_uint64[1]);
  is_equal = is_equal && (s_uint64[2] == t_uint64[2]);
  is_equal = is_equal && (s_uint64[3] == t_uint64[3]);
  if (is_equal == 1) {
    return (0);
  }
  bool lt = 1;
  lt = lt && (s_uint64[0] <= t_uint64[0]);
  lt = lt && (s_uint64[1] <= t_uint64[1]);
  lt = lt && (s_uint64[2] <= t_uint64[2]);
  lt = lt && (s_uint64[3] <= t_uint64[3]);
  return (lt ? -1 : 1);
}

bool blst_fr_from_lendian(blst_fr *x, byte b[32]) {
  blst_scalar s;
  blst_scalar_from_lendian(&s, b);
  bool is_ok = blst_scalar_fr_check(&s);
  if (is_ok) {
    blst_fr_from_scalar(x, &s);
  }
  return (is_ok);
}

void blst_lendian_from_fr(byte b[32], blst_fr *x) {
  blst_scalar s;
  blst_scalar_from_fr(&s, x);
  blst_lendian_from_scalar(b, &s);
}

int blst_fr_pow(blst_fr *out, blst_fr *x, byte *exp, int exp_nb_bits) {
  if (exp_nb_bits == 0) {
    // out = x^0 = one
    blst_fr_set_to_one(out);
    return 0;
  }

  if (blst_fr_is_zero(x) || blst_fr_is_one(x)) {
    // out = 0^exp = 0 (exp <> 0) or out = 1^exp = 1
    memcpy(out, x, sizeof(blst_fr));
    return 0;
  }

  // Assert that the most significant bit of exp is 1, otherwise
  // fail with error value 2 (Invalid_Argument).
  if (!(exp[(exp_nb_bits - 1) / 8] & (1 << (exp_nb_bits - 1) % 8)))
    return 2;

  // Given that the msb of exp is 1, we set our accumulator to x and
  // start the square and multiply algorithm from the second msb.
  memcpy(out, x, sizeof(blst_fr));

  // Square and multiply
  for (int i = exp_nb_bits - 2; i >= 0; i--) {
    blst_fr_sqr(out, out);
    if (exp[i / 8] & (1 << (i % 8)))
      blst_fr_mul(out, out, x);
  }
  return 0;
}

size_t blst_fp_sizeof() { return sizeof(blst_fp); }

size_t blst_fp2_sizeof() { return sizeof(blst_fp2); }

void blst_fp2_assign(blst_fp2 *p_c, blst_fp *x1_c, blst_fp *x2_c) {
  (p_c->fp[0]).l[0] = x1_c->l[0];
  (p_c->fp[0]).l[1] = x1_c->l[1];
  (p_c->fp[0]).l[2] = x1_c->l[2];
  (p_c->fp[0]).l[3] = x1_c->l[3];
  (p_c->fp[1]).l[0] = x2_c->l[0];
  (p_c->fp[1]).l[1] = x2_c->l[1];
  (p_c->fp[1]).l[2] = x2_c->l[2];
  (p_c->fp[1]).l[3] = x2_c->l[3];
}

void blst_fp2_zero(blst_fp2 *buffer_c) {
  byte zero_bytes[48] = {0};
  blst_fp_from_lendian(&buffer_c->fp[0], zero_bytes);
  blst_fp_from_lendian(&buffer_c->fp[1], zero_bytes);
}

void blst_fp2_set_to_one(blst_fp2 *buffer_c) {
  byte bytes[48] = {0};
  blst_fp_from_lendian(&buffer_c->fp[0], bytes);
  bytes[0] = 1;
  blst_fp_from_lendian(&buffer_c->fp[1], bytes);
}

void blst_fp2_of_bytes_components(blst_fp2 *buffer_c, byte *x1, byte *x2) {
  // FIXME: add a check on the length
  blst_fp_from_lendian(&buffer_c->fp[0], x1);
  blst_fp_from_lendian(&buffer_c->fp[1], x2);
}

void blst_fp2_to_bytes(byte *out, blst_fp2 *p_c) {
  blst_lendian_from_fp(out, &p_c->fp[0]);
  blst_lendian_from_fp(out + 48, &p_c->fp[1]);
}

size_t blst_fp12_sizeof() { return sizeof(blst_fp12); }

void blst_fp12_set_to_one(blst_fp12 *buffer_c) {
  // Set all coordinates to 0. If allocated with allocate_fp12_stubs it's
  // normally fine, but doing it just in case...
  memset(buffer_c, 0, sizeof(blst_fp12));
  // And override for 1.
  byte out[48] = {0};
  out[0] = 1;
  blst_fp_from_lendian(&(buffer_c->fp6[0].fp2[0].fp[0]), out);
}

bool blst_fp12_is_zero(blst_fp12 *p) {
  unsigned char zero[48 * 12] = {0};
  return memcmp(p, zero, sizeof(blst_fp12)) == 0;
}

void blst_fp12_to_bytes(byte *buffer, blst_fp12 *p_c) {
  // FIXME: add a check on the length
  blst_lendian_from_fp(buffer, &(p_c->fp6[0].fp2[0].fp[0]));
  blst_lendian_from_fp(buffer + 1 * 48, &(p_c->fp6[0].fp2[0].fp[1]));
  blst_lendian_from_fp(buffer + 2 * 48, &(p_c->fp6[0].fp2[1].fp[0]));
  blst_lendian_from_fp(buffer + 3 * 48, &(p_c->fp6[0].fp2[1].fp[1]));
  blst_lendian_from_fp(buffer + 4 * 48, &(p_c->fp6[0].fp2[2].fp[0]));
  blst_lendian_from_fp(buffer + 5 * 48, &(p_c->fp6[0].fp2[2].fp[1]));
  blst_lendian_from_fp(buffer + 6 * 48, &(p_c->fp6[1].fp2[0].fp[0]));
  blst_lendian_from_fp(buffer + 7 * 48, &(p_c->fp6[1].fp2[0].fp[1]));
  blst_lendian_from_fp(buffer + 8 * 48, &(p_c->fp6[1].fp2[1].fp[0]));
  blst_lendian_from_fp(buffer + 9 * 48, &(p_c->fp6[1].fp2[1].fp[1]));
  blst_lendian_from_fp(buffer + 10 * 48, &(p_c->fp6[1].fp2[2].fp[0]));
  blst_lendian_from_fp(buffer + 11 * 48, &(p_c->fp6[1].fp2[2].fp[1]));
}

void blst_fp12_of_bytes(blst_fp12 *buffer_c, byte *p) {
  // FIXME: add a check on the length
  blst_fp_from_lendian(&(buffer_c->fp6[0].fp2[0].fp[0]), p);
  blst_fp_from_lendian(&(buffer_c->fp6[0].fp2[0].fp[1]), p + 48);
  blst_fp_from_lendian(&(buffer_c->fp6[0].fp2[1].fp[0]), p + 2 * 48);
  blst_fp_from_lendian(&(buffer_c->fp6[0].fp2[1].fp[1]), p + 3 * 48);
  blst_fp_from_lendian(&(buffer_c->fp6[0].fp2[2].fp[0]), p + 4 * 48);
  blst_fp_from_lendian(&(buffer_c->fp6[0].fp2[2].fp[1]), p + 5 * 48);
  blst_fp_from_lendian(&(buffer_c->fp6[1].fp2[0].fp[0]), p + 6 * 48);
  blst_fp_from_lendian(&(buffer_c->fp6[1].fp2[0].fp[1]), p + 7 * 48);
  blst_fp_from_lendian(&(buffer_c->fp6[1].fp2[1].fp[0]), p + 8 * 48);
  blst_fp_from_lendian(&(buffer_c->fp6[1].fp2[1].fp[1]), p + 9 * 48);
  blst_fp_from_lendian(&(buffer_c->fp6[1].fp2[2].fp[0]), p + 10 * 48);
  blst_fp_from_lendian(&(buffer_c->fp6[1].fp2[2].fp[1]), p + 11 * 48);
}

bool blst_fp12_is_ff_one(blst_fp12 *p) {
  unsigned char one[48 * 12] = {0};
  one[0] = 1;
  return memcmp(p, one, sizeof(blst_fp12)) == 0;
}

int blst_fp12_pow(blst_fp12 *out, blst_fp12 *x, byte *exp, int exp_nb_bits) {
  if (exp_nb_bits == 0) {
    // out = x^0 = one
    blst_fp12_set_to_one(out);
    return 0;
  }

  // We use blst_fq12_is_ff_one because in blst, blst_fq12_is_one is defined as
  // the generator of the prime subgroup of Fq12.
  if (blst_fp12_is_zero(x) || blst_fp12_is_ff_one(x)) {
    // out = 0^exp = 0 (exp <> 0) or out = 1^exp = 1
    memcpy(out, x, sizeof(blst_fp12));
    return 0;
  }

  // Assert that the most significant bit of exp is 1, otherwise
  // fail with error value 2 (Invalid_Argument).
  if (!(exp[(exp_nb_bits - 1) / 8] & (1 << (exp_nb_bits - 1) % 8)))
    return 2;

  // Given that the msb of exp is 1, we set our accumulator to x and
  // start the square and multiply algorithm from the second msb.
  memcpy(out, x, sizeof(blst_fp12));

  // Square and multiply
  for (int i = exp_nb_bits - 2; i >= 0; i--) {
    blst_fp12_sqr(out, out);
    if (exp[i / 8] & (1 << (i % 8)))
      blst_fp12_mul(out, out, x);
  }
  return 0;
}

size_t blst_p1_sizeof() { return sizeof(blst_p1); }

size_t blst_p1_affine_sizeof() { return sizeof(blst_p1_affine); }

void blst_p1_set_coordinates(blst_p1 *buffer_c, blst_fp *x_c, blst_fp *y_c) {
  buffer_c->x = *x_c;
  buffer_c->y = *y_c;
}

size_t blst_p2_sizeof() { return sizeof(blst_p2); }

size_t blst_p2_affine_sizeof() { return sizeof(blst_p2_affine); }

void blst_p2_set_coordinates(blst_p2 *buffer_c, blst_fp2 *x_c, blst_fp2 *y_c) {
  byte out[96];

  blst_lendian_from_fp(out, &x_c->fp[0]);
  blst_lendian_from_fp(out + 48, &x_c->fp[1]);
  blst_fp_from_lendian(&buffer_c->x.fp[0], out);
  blst_fp_from_lendian(&buffer_c->x.fp[1], out + 48);

  blst_lendian_from_fp(out, &y_c->fp[0]);
  blst_lendian_from_fp(out + 48, &y_c->fp[1]);
  blst_fp_from_lendian(&buffer_c->y.fp[0], out);
  blst_fp_from_lendian(&buffer_c->y.fp[1], out + 48);
}
