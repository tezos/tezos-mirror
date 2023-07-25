// Include this file in server.c or cherry-pick 95dfbdd when updating blst

const vec256 BLS12_381_rR = {/* (1<<256)%r, "radix", one-in-Montgomery */
                             TO_LIMB_T(0x00000001fffffffe),
                             TO_LIMB_T(0x5884b7fa00034802),
                             TO_LIMB_T(0x998c4fefecbc4ff5),
                             TO_LIMB_T(0x1824b159acc5056f)};

bool_t blst_fr_is_zero(const vec256 a_fr) {
  return vec_is_zero(a_fr, sizeof(vec256));
}

bool_t blst_fr_is_one(const vec256 a_fr) {
  return vec_is_equal(a_fr, BLS12_381_rR, sizeof(vec256));
}

bool_t blst_fr_is_equal(const vec256 a, const vec256 b) {
  return vec_is_equal(a, b, sizeof(vec256));
}

void blst_fr_set_to_zero(vec256 a_fr) { vec_zero(a_fr, sizeof(vec256)); }

void blst_fr_set_to_one(vec256 a_fr) {
  vec_copy(a_fr, BLS12_381_rR, sizeof(vec256));
}

// Improve pippenger using contiguous C array for scalars and affine points.
// FIXME: let's rename it? ATM, we use a suffix _cont
#define POINTS_MULT_PIPPENGER_CONT_IMPL(prefix, ptype)                         \
  static void ptype##s_tile_pippenger_cont(                                    \
      ptype *ret, const ptype##_affine points[], size_t npoints,               \
      const byte scalars[], size_t nbits, ptype##xyzz buckets[], size_t bit0,  \
      size_t wbits, size_t cbits) {                                            \
    limb_t wmask, wval, wnxt;                                                  \
    size_t i, z, nbytes;                                                       \
    nbytes = (nbits + 7) / 8; /* convert |nbits| to bytes */                   \
    const byte *scalar = scalars;                                              \
    const ptype##_affine *point = points++;                                    \
                                                                               \
    wmask = ((limb_t)1 << (wbits + 1)) - 1;                                    \
    z = is_zero(bit0);                                                         \
    bit0 -= z ^ 1;                                                             \
    wbits += z ^ 1;                                                            \
    wval = (get_wval_limb(scalar, bit0, wbits) << z) & wmask;                  \
    wval = booth_encode(wval, cbits);                                          \
    scalar = scalar + nbytes /* ? scalars+nbytes : scalar+nbytes */;           \
    wnxt = (get_wval_limb(scalar, bit0, wbits) << z) & wmask;                  \
    wnxt = booth_encode(wnxt, cbits);                                          \
    npoints--; /* account for prefetch */                                      \
                                                                               \
    ptype##_bucket(buckets, wval, cbits, point);                               \
    for (i = 1; i < npoints; i++) {                                            \
      wval = wnxt;                                                             \
      scalar = scalar + nbytes /* ? scalars+nbytes : scalar+nbytes */;         \
      wnxt = (get_wval_limb(scalar, bit0, wbits) << z) & wmask;                \
      wnxt = booth_encode(wnxt, cbits);                                        \
      ptype##_prefetch(buckets, wnxt, cbits);                                  \
      point = points ? points++ : point + 1;                                   \
      ptype##_bucket(buckets, wval, cbits, point);                             \
    }                                                                          \
    point = points ? points++ : point + 1;                                     \
    ptype##_bucket(buckets, wnxt, cbits, point);                               \
    ptype##_integrate_buckets(ret, buckets, cbits - 1);                        \
  }                                                                            \
                                                                               \
  static void ptype##s_mult_pippenger_cont(                                    \
      ptype *ret, const ptype##_affine points[], size_t npoints,               \
      const byte scalars[], size_t nbits, ptype##xyzz buckets[],               \
      size_t window) {                                                         \
    size_t i, wbits, cbits, bit0 = nbits;                                      \
    ptype tile[1];                                                             \
                                                                               \
    window = window ? window : pippenger_window_size(npoints);                 \
    vec_zero(buckets, sizeof(buckets[0]) << (window - 1));                     \
    vec_zero(ret, sizeof(*ret));                                               \
                                                                               \
    /* top excess bits modulo target window size */                            \
    wbits = nbits % window; /* yes, it may be zero */                          \
    cbits = wbits + 1;                                                         \
    while (bit0 -= wbits) {                                                    \
      ptype##s_tile_pippenger_cont(tile, points, npoints, scalars, nbits,      \
                                   buckets, bit0, wbits, cbits);               \
      ptype##_dadd(ret, ret, tile, NULL);                                      \
      for (i = 0; i < window; i++)                                             \
        ptype##_double(ret, ret);                                              \
      cbits = wbits = window;                                                  \
    }                                                                          \
    ptype##s_tile_pippenger_cont(tile, points, npoints, scalars, nbits,        \
                                 buckets, 0, wbits, cbits);                    \
    ptype##_dadd(ret, ret, tile, NULL);                                        \
  }                                                                            \
                                                                               \
  void prefix##s_mult_pippenger_cont(                                          \
      ptype *ret, const ptype##_affine points[], size_t npoints,               \
      const byte scalars[], size_t nbits, ptype##xyzz scratch[]) {             \
    ptype##s_mult_pippenger_cont(ret, points, npoints, scalars, nbits,         \
                                 scratch, 0);                                  \
  }

POINTS_MULT_PIPPENGER_CONT_IMPL(blst_p1, POINTonE1)
POINTS_MULT_PIPPENGER_CONT_IMPL(blst_p2, POINTonE2)
