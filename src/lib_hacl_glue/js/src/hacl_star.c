#include <stdint.h>
#include <stdbool.h>

bool _1_Lib_RandomBuffer_System_randombytes(uint8_t *buf, uint32_t len){ return 0; };

void Hacl_Hash_Core_SHA2_init_256(uint32_t *s){ return ;};

void Hacl_Hash_Core_SHA2_update_256(uint32_t *hash, uint8_t *block){ return ;};

void Hacl_Hash_Core_SHA2_finish_256(uint32_t *s, uint8_t *dst){ return ;};

void Hacl_Hash_Core_SHA2_init_512(uint64_t *s){ return ;};

void Hacl_Hash_Core_SHA2_update_512(uint64_t *hash, uint8_t *block){ return ;};

void Hacl_Hash_Core_SHA2_finish_512(uint64_t *s, uint8_t *dst){ return ;};

void
Hacl_Impl_SHA3_keccak(
  uint32_t rate,
  uint32_t capacity,
  uint32_t inputByteLen,
  uint8_t *input,
  uint8_t delimitedSuffix,
  uint32_t outputByteLen,
  uint8_t *output
){ return ;};

void Hacl_Hash_SHA2_hash_256(uint8_t *input, uint32_t input_len, uint8_t *dst){ return ;};

void Hacl_Hash_SHA2_hash_512(uint8_t *input, uint32_t input_len, uint8_t *dst){ return ;};

void Hacl_SHA3_sha3_256(uint32_t inputByteLen, uint8_t *input, uint8_t *output){ return ;};

void Hacl_SHA3_sha3_512(uint32_t inputByteLen, uint8_t *input, uint8_t *output){ return ;};

void
Hacl_HMAC_compute_sha2_256(
  uint8_t *dst,
  uint8_t *key,
  uint32_t key_len,
  uint8_t *data,
  uint32_t data_len
){ return ;};

void
Hacl_HMAC_compute_sha2_512(
  uint8_t *dst,
  uint8_t *key,
  uint32_t key_len,
  uint8_t *data,
  uint32_t data_len
){ return ;};

void
Hacl_Blake2b_32_blake2b(
  uint32_t nn,
  uint8_t *output,
  uint32_t ll,
  uint8_t *d,
  uint32_t kk,
  uint8_t *k
){ return ;};

uint32_t
Hacl_NaCl_crypto_secretbox_easy(uint8_t *c, uint8_t *m, uint32_t mlen, uint8_t *n, uint8_t *k){ return 0;};

uint32_t
Hacl_NaCl_crypto_secretbox_open_easy(
  uint8_t *m,
  uint8_t *c,
  uint32_t clen,
  uint8_t *n,
  uint8_t *k
){ return 0;};

void Hacl_Curve25519_51_scalarmult(uint8_t *out, uint8_t *priv, uint8_t *pub){ return ;};

uint32_t Hacl_NaCl_crypto_box_beforenm(uint8_t *k, uint8_t *pk, uint8_t *sk){ return 0;};

uint32_t
Hacl_NaCl_crypto_box_easy_afternm(
  uint8_t *c,
  uint8_t *m,
  uint32_t mlen,
  uint8_t *n,
  uint8_t *k
){ return 0;};

uint32_t
Hacl_NaCl_crypto_box_open_easy_afternm(
  uint8_t *m,
  uint8_t *c,
  uint32_t clen,
  uint8_t *n,
  uint8_t *k
){ return 0;};

uint32_t
Hacl_NaCl_crypto_box_detached_afternm(
  uint8_t *c,
  uint8_t *tag,
  uint8_t *m,
  uint32_t mlen,
  uint8_t *n,
  uint8_t *k
){ return 0;};

uint32_t
Hacl_NaCl_crypto_box_open_detached_afternm(
  uint8_t *m,
  uint8_t *c,
  uint8_t *tag,
  uint32_t mlen,
  uint8_t *n,
  uint8_t *k
){ return 0;};

void Hacl_Ed25519_secret_to_public(uint8_t *pub, uint8_t *priv){ return ;};

void Hacl_Ed25519_sign(uint8_t *signature, uint8_t *priv, uint32_t len, uint8_t *msg){ return ;};

bool Hacl_Ed25519_verify(uint8_t *pub, uint32_t len, uint8_t *msg, uint8_t *signature){ return 0;};

bool
Hacl_P256_ecdsa_sign_p256_without_hash(
  uint8_t *result,
  uint32_t mLen,
  uint8_t *m,
  uint8_t *privKey,
  uint8_t *k
) { return 0; };

bool
Hacl_P256_ecdsa_verif_without_hash(
  uint32_t mLen,
  uint8_t *m,
  uint8_t *pubKey,
  uint8_t *r,
  uint8_t *s
) { return 0; };

bool Hacl_P256_verify_q(uint8_t *pubKey) { return 0; };

bool Hacl_P256_ecp256dh_i(uint8_t *result, uint8_t *scalar) { return 0; };

bool Hacl_P256_is_more_than_zero_less_than_order(uint8_t *x) { return 0; };

void Hacl_P256_compression_compressed_form(uint8_t *b, uint8_t *result) {return; };

void Hacl_P256_compression_not_compressed_form(uint8_t *b, uint8_t *result) {return; };

bool Hacl_P256_decompression_compressed_form(uint8_t *b, uint8_t *result) {return 0; };

bool Hacl_P256_decompression_not_compressed_form(uint8_t *b, uint8_t *result) {return 0; };
