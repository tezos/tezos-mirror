// This file was automatically generated, do not edit.
// Edit file src/lib_hacl/gen/gen.ml instead


//Provides: _17_EverCrypt_AutoConfig2_init
//Requires: EverCrypt_AutoConfig2_init
function _17_EverCrypt_AutoConfig2_init (x0) {
  return EverCrypt_AutoConfig2_init(x0);
}

//Provides: _2_EverCrypt_Curve25519_scalarmult
//Requires: EverCrypt_Curve25519_scalarmult
function _2_EverCrypt_Curve25519_scalarmult (x0, x1, x2) {
  return EverCrypt_Curve25519_scalarmult(x0, x1, x2);
}

//Provides: _8_EverCrypt_HMAC_compute
//Requires: EverCrypt_HMAC_compute
function _8_EverCrypt_HMAC_compute (x0, x1, x2, x3, x4, x5) {
  return EverCrypt_HMAC_compute(x0, x1, x2, x3, x4, x5);
}

//Provides: _8_EverCrypt_HMAC_compute_byte6
//Requires: _8_EverCrypt_HMAC_compute
function _8_EverCrypt_HMAC_compute_byte6 (x0, x1, x2, x3, x4, x5) {
  return _8_EverCrypt_HMAC_compute(x0, x1, x2, x3, x4, x5);
}

//Provides: _18_EverCrypt_Hash_hash
//Requires: EverCrypt_Hash_hash
function _18_EverCrypt_Hash_hash (x0, x1, x2, x3) {
  return EverCrypt_Hash_hash(x0, x1, x2, x3);
}

//Provides: _2_Hacl_Hash_Definitions_block_len
//Requires: Hacl_Hash_Definitions_block_len
function _2_Hacl_Hash_Definitions_block_len (x0) {
  return Hacl_Hash_Definitions_block_len(x0);
}

//Provides: _4_Hacl_Hash_Definitions_hash_len
//Requires: Hacl_Hash_Definitions_hash_len
function _4_Hacl_Hash_Definitions_hash_len (x0) {
  return Hacl_Hash_Definitions_hash_len(x0);
}

//Provides: _1_Lib_RandomBuffer_System_randombytes
//Requires: Lib_RandomBuffer_System_randombytes
function _1_Lib_RandomBuffer_System_randombytes (x0, x1) {
  return Lib_RandomBuffer_System_randombytes(x0, x1);
}

//Provides: _10_Hacl_Blake2b_32_blake2b
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _10_Hacl_Blake2b_32_blake2b (output_len, output, data_len, data, key_len, key) {
  var H = globalThis._HACL;
  var i_output_len = integers_int32_of_uint32(output_len)
  var a_data = hacl_create_buffer(data,integers_int32_of_uint32(data_len))
  var a_key = hacl_create_buffer(key,integers_int32_of_uint32(key_len))
  var ret = H.Blake2.blake2b(i_output_len, a_data, a_key);
  hacl_blit_buf_to_bytes(ret[0], output, integers_int32_of_uint32(output_len))
  return 0;
}
//Provides: _10_Hacl_Blake2b_32_blake2b_byte6
//Requires: _10_Hacl_Blake2b_32_blake2b
function _10_Hacl_Blake2b_32_blake2b_byte6 (x0, x1, x2, x3, x4, x5) {
  return _10_Hacl_Blake2b_32_blake2b(x0, x1, x2, x3, x4, x5);
}

//Provides: _16_Hacl_Blake2s_32_blake2s
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _16_Hacl_Blake2s_32_blake2s (output_len, output, data_len, data, key_len, key) {
  var H = globalThis._HACL;
  var i_output_len = integers_int32_of_uint32(output_len)
  var a_data = hacl_create_buffer(data,integers_int32_of_uint32(data_len))
  var a_key = hacl_create_buffer(key,integers_int32_of_uint32(key_len))
  var ret = H.Blake2.blake2s(i_output_len, a_data, a_key);
  hacl_blit_buf_to_bytes(ret[0], output, integers_int32_of_uint32(output_len))
  return 0;
}
//Provides: _16_Hacl_Blake2s_32_blake2s_byte6
//Requires: _16_Hacl_Blake2s_32_blake2s
function _16_Hacl_Blake2s_32_blake2s_byte6 (x0, x1, x2, x3, x4, x5) {
  return _16_Hacl_Blake2s_32_blake2s(x0, x1, x2, x3, x4, x5);
}

//Provides: _2_Hacl_Chacha20Poly1305_32_aead_decrypt
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _2_Hacl_Chacha20Poly1305_32_aead_decrypt (key, nonce, alen, aad, len, plaintext, ciphertext, mac) {
  var H = globalThis._HACL;
  var a_key = hacl_create_buffer(key,32)
  var a_nonce = hacl_create_buffer(nonce,12)
  var a_aad = hacl_create_buffer(aad,integers_int32_of_uint32(alen))
  var a_ciphertext = hacl_create_buffer(ciphertext,integers_int32_of_uint32(len))
  var a_mac = hacl_create_buffer(mac,16)
  var ret = H.Chacha20Poly1305.aead_decrypt(a_key, a_nonce, a_aad, a_ciphertext, a_mac);
  hacl_blit_buf_to_bytes(ret[1], plaintext, integers_int32_of_uint32(len))
  return integers_uint32_of_int32(ret[0]);
}
//Provides: _2_Hacl_Chacha20Poly1305_32_aead_decrypt_byte8
//Requires: _2_Hacl_Chacha20Poly1305_32_aead_decrypt
function _2_Hacl_Chacha20Poly1305_32_aead_decrypt_byte8 (x0, x1, x2, x3, x4, x5, x6, x7) {
  return _2_Hacl_Chacha20Poly1305_32_aead_decrypt(x0, x1, x2, x3, x4, x5, x6, x7);
}

//Provides: _1_Hacl_Chacha20Poly1305_32_aead_encrypt
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _1_Hacl_Chacha20Poly1305_32_aead_encrypt (key, nonce, alen, aad, len, plaintext, ciphertext, mac) {
  var H = globalThis._HACL;
  var a_key = hacl_create_buffer(key,32)
  var a_nonce = hacl_create_buffer(nonce,12)
  var a_aad = hacl_create_buffer(aad,integers_int32_of_uint32(alen))
  var a_plaintext = hacl_create_buffer(plaintext,integers_int32_of_uint32(len))
  var ret = H.Chacha20Poly1305.aead_encrypt(a_key, a_nonce, a_aad, a_plaintext);
  hacl_blit_buf_to_bytes(ret[0], ciphertext, integers_int32_of_uint32(len))
  hacl_blit_buf_to_bytes(ret[1], mac, 16)
  return 0;
}
//Provides: _1_Hacl_Chacha20Poly1305_32_aead_encrypt_byte8
//Requires: _1_Hacl_Chacha20Poly1305_32_aead_encrypt
function _1_Hacl_Chacha20Poly1305_32_aead_encrypt_byte8 (x0, x1, x2, x3, x4, x5, x6, x7) {
  return _1_Hacl_Chacha20Poly1305_32_aead_encrypt(x0, x1, x2, x3, x4, x5, x6, x7);
}

//Provides: _3_Hacl_Curve25519_51_ecdh
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _3_Hacl_Curve25519_51_ecdh (result, scalar, input) {
  var H = globalThis._HACL;
  var a_scalar = hacl_create_buffer(scalar,32)
  var a_input = hacl_create_buffer(input,32)
  var ret = H.Curve25519_51.ecdh(a_scalar, a_input);
  hacl_blit_buf_to_bytes(ret[1], result, 32)
  return (ret[0]?1:0);
}
//Provides: Hacl_Curve25519_51_scalarmult
//Requires: _1_Hacl_Curve25519_51_scalarmult
var Hacl_Curve25519_51_scalarmult = _1_Hacl_Curve25519_51_scalarmult

//Provides: _1_Hacl_Curve25519_51_scalarmult
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _1_Hacl_Curve25519_51_scalarmult (result, scalar, input) {
  var H = globalThis._HACL;
  var a_scalar = hacl_create_buffer(scalar,32)
  var a_input = hacl_create_buffer(input,32)
  var ret = H.Curve25519_51.scalarmult(a_scalar, a_input);
  hacl_blit_buf_to_bytes(ret[0], result, 32)
  return 0;
}
//Provides: _2_Hacl_Curve25519_51_secret_to_public
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _2_Hacl_Curve25519_51_secret_to_public (result, scalar) {
  var H = globalThis._HACL;
  var a_scalar = hacl_create_buffer(scalar,32)
  var ret = H.Curve25519_51.secret_to_public(a_scalar);
  hacl_blit_buf_to_bytes(ret[0], result, 32)
  return 0;
}
//Provides: _13_Hacl_Ed25519_secret_to_public
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _13_Hacl_Ed25519_secret_to_public (pub, priv) {
  var H = globalThis._HACL;
  var a_priv = hacl_create_buffer(priv,32)
  var ret = H.Ed25519.secret_to_public(a_priv);
  hacl_blit_buf_to_bytes(ret[0], pub, 32)
  return 0;
}
//Provides: _11_Hacl_Ed25519_sign
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _11_Hacl_Ed25519_sign (signature, priv, len, message) {
  var H = globalThis._HACL;
  var a_priv = hacl_create_buffer(priv,32)
  var a_message = hacl_create_buffer(message,integers_int32_of_uint32(len))
  var ret = H.Ed25519.sign(a_priv, a_message);
  hacl_blit_buf_to_bytes(ret[0], signature, 64)
  return 0;
}
//Provides: _12_Hacl_Ed25519_verify
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _12_Hacl_Ed25519_verify (pub, len, message, signature) {
  var H = globalThis._HACL;
  var a_pub = hacl_create_buffer(pub,32)
  var a_message = hacl_create_buffer(message,integers_int32_of_uint32(len))
  var a_signature = hacl_create_buffer(signature,64)
  var ret = H.Ed25519.verify(a_pub, a_message, a_signature);
  return (ret[0]?1:0);
}
//Provides: _1_Hacl_HKDF_expand_sha2_256
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _1_Hacl_HKDF_expand_sha2_256 (okm, prk, prk_len, info, infolen, len) {
  var H = globalThis._HACL;
  var a_prk = hacl_create_buffer(prk,integers_int32_of_uint32(prk_len))
  var a_info = hacl_create_buffer(info,integers_int32_of_uint32(infolen))
  var i_len = integers_int32_of_uint32(len)
  var ret = H.HKDF.expand_sha2_256(a_prk, a_info, i_len);
  hacl_blit_buf_to_bytes(ret[0], okm, integers_int32_of_uint32(len))
  return 0;
}
//Provides: _1_Hacl_HKDF_expand_sha2_256_byte6
//Requires: _1_Hacl_HKDF_expand_sha2_256
function _1_Hacl_HKDF_expand_sha2_256_byte6 (x0, x1, x2, x3, x4, x5) {
  return _1_Hacl_HKDF_expand_sha2_256(x0, x1, x2, x3, x4, x5);
}

//Provides: _2_Hacl_HKDF_extract_sha2_256
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _2_Hacl_HKDF_extract_sha2_256 (prk, salt, salt_len, ikm, ikm_len) {
  var H = globalThis._HACL;
  var a_salt = hacl_create_buffer(salt,integers_int32_of_uint32(salt_len))
  var a_ikm = hacl_create_buffer(ikm,integers_int32_of_uint32(ikm_len))
  var ret = H.HKDF.extract_sha2_256(a_salt, a_ikm);
  hacl_blit_buf_to_bytes(ret[0], prk, 32)
  return 0;
}
//Provides: Hacl_HMAC_compute_sha2_256
//Requires: _2_Hacl_HMAC_compute_sha2_256
var Hacl_HMAC_compute_sha2_256 = _2_Hacl_HMAC_compute_sha2_256

//Provides: _2_Hacl_HMAC_compute_sha2_256
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _2_Hacl_HMAC_compute_sha2_256 (tag, key, key_len, data, data_len) {
  var H = globalThis._HACL;
  var a_key = hacl_create_buffer(key,integers_int32_of_uint32(key_len))
  var a_data = hacl_create_buffer(data,integers_int32_of_uint32(data_len))
  var ret = H.HMAC.sha256(a_key, a_data);
  hacl_blit_buf_to_bytes(ret[0], tag, 32)
  return 0;
}
//Provides: Hacl_HMAC_compute_sha2_512
//Requires: _4_Hacl_HMAC_compute_sha2_512
var Hacl_HMAC_compute_sha2_512 = _4_Hacl_HMAC_compute_sha2_512

//Provides: _4_Hacl_HMAC_compute_sha2_512
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _4_Hacl_HMAC_compute_sha2_512 (tag, key, key_len, data, data_len) {
  var H = globalThis._HACL;
  var a_key = hacl_create_buffer(key,integers_int32_of_uint32(key_len))
  var a_data = hacl_create_buffer(data,integers_int32_of_uint32(data_len))
  var ret = H.HMAC.sha512(a_key, a_data);
  hacl_blit_buf_to_bytes(ret[0], tag, 64)
  return 0;
}
//Provides: Hacl_Hash_SHA2_hash_256
//Requires: _19_Hacl_Hash_SHA2_hash_256
var Hacl_Hash_SHA2_hash_256 = _19_Hacl_Hash_SHA2_hash_256

//Provides: _19_Hacl_Hash_SHA2_hash_256
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _19_Hacl_Hash_SHA2_hash_256 (input, input_len, hash) {
  var H = globalThis._HACL;
  var a_input = hacl_create_buffer(input,integers_int32_of_uint32(input_len))
  var ret = H.SHA2.hash_256(a_input);
  hacl_blit_buf_to_bytes(ret[0], hash, 32)
  return 0;
}
//Provides: _20_Hacl_Hash_SHA2_hash_384
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _20_Hacl_Hash_SHA2_hash_384 (input, input_len, hash) {
  var H = globalThis._HACL;
  var a_input = hacl_create_buffer(input,integers_int32_of_uint32(input_len))
  var ret = H.SHA2.hash_384(a_input);
  hacl_blit_buf_to_bytes(ret[0], hash, 48)
  return 0;
}
//Provides: Hacl_Hash_SHA2_hash_512
//Requires: _21_Hacl_Hash_SHA2_hash_512
var Hacl_Hash_SHA2_hash_512 = _21_Hacl_Hash_SHA2_hash_512

//Provides: _21_Hacl_Hash_SHA2_hash_512
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _21_Hacl_Hash_SHA2_hash_512 (input, input_len, hash) {
  var H = globalThis._HACL;
  var a_input = hacl_create_buffer(input,integers_int32_of_uint32(input_len))
  var ret = H.SHA2.hash_512(a_input);
  hacl_blit_buf_to_bytes(ret[0], hash, 64)
  return 0;
}
//Provides: _7_Hacl_Impl_SHA3_keccak
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _7_Hacl_Impl_SHA3_keccak (rate, capacity, input_len, input, suffix, output_len, digest) {
  var H = globalThis._HACL;
  var i_rate = integers_int32_of_uint32(rate)
  var i_capacity = integers_int32_of_uint32(capacity)
  var a_input = hacl_create_buffer(input,integers_int32_of_uint32(input_len))
  var i_output_len = integers_int32_of_uint32(output_len)
  var ret = H.SHA3.keccak(i_rate, i_capacity, a_input, suffix, i_output_len);
  hacl_blit_buf_to_bytes(ret[0], digest, integers_int32_of_uint32(output_len))
  return 0;
}
//Provides: _7_Hacl_Impl_SHA3_keccak_byte7
//Requires: _7_Hacl_Impl_SHA3_keccak
function _7_Hacl_Impl_SHA3_keccak_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _7_Hacl_Impl_SHA3_keccak(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _5_Hacl_NaCl_crypto_box_beforenm
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _5_Hacl_NaCl_crypto_box_beforenm (k, pk, sk) {
  var H = globalThis._HACL;
  var a_pk = hacl_create_buffer(pk,32)
  var a_sk = hacl_create_buffer(sk,32)
  var ret = H.NaCl.box_beforenm(a_pk, a_sk);
  hacl_blit_buf_to_bytes(ret[1], k, 32)
  return integers_uint32_of_int32(ret[0]);
}
//Provides: _6_Hacl_NaCl_crypto_box_detached_afternm
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _6_Hacl_NaCl_crypto_box_detached_afternm (c, tag, m, mlen, n, k) {
  var H = globalThis._HACL;
  var a_m = hacl_create_buffer(m,integers_int32_of_uint32(mlen))
  var a_n = hacl_create_buffer(n,24)
  var a_k = hacl_create_buffer(k,32)
  var ret = H.NaCl.box_detached_afternm(a_m, a_n, a_k);
  hacl_blit_buf_to_bytes(ret[1], c, integers_int32_of_uint32(mlen))
  hacl_blit_buf_to_bytes(ret[2], tag, 16)
  return integers_uint32_of_int32(ret[0]);
}
//Provides: _6_Hacl_NaCl_crypto_box_detached_afternm_byte6
//Requires: _6_Hacl_NaCl_crypto_box_detached_afternm
function _6_Hacl_NaCl_crypto_box_detached_afternm_byte6 (x0, x1, x2, x3, x4, x5) {
  return _6_Hacl_NaCl_crypto_box_detached_afternm(x0, x1, x2, x3, x4, x5);
}

//Provides: _10_Hacl_NaCl_crypto_box_easy_afternm
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _10_Hacl_NaCl_crypto_box_easy_afternm (c, m, mlen, n, k) {
  var H = globalThis._HACL;
  var a_m = hacl_create_buffer(m,integers_int32_of_uint32(mlen))
  var a_n = hacl_create_buffer(n,24)
  var a_k = hacl_create_buffer(k,32)
  var ret = H.NaCl.box_easy_afternm(a_m, a_n, a_k);
  hacl_blit_buf_to_bytes(ret[1], c, integers_int32_of_uint32(mlen)+16)
  return integers_uint32_of_int32(ret[0]);
}
//Provides: _8_Hacl_NaCl_crypto_box_open_detached_afternm
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _8_Hacl_NaCl_crypto_box_open_detached_afternm (m, c, tag, mlen, n, k) {
  var H = globalThis._HACL;
  var a_c = hacl_create_buffer(c,integers_int32_of_uint32(mlen))
  var a_tag = hacl_create_buffer(tag,16)
  var a_n = hacl_create_buffer(n,24)
  var a_k = hacl_create_buffer(k,32)
  var ret = H.NaCl.box_open_detached_afternm(a_c, a_tag, a_n, a_k);
  hacl_blit_buf_to_bytes(ret[1], m, integers_int32_of_uint32(mlen))
  return integers_uint32_of_int32(ret[0]);
}
//Provides: _8_Hacl_NaCl_crypto_box_open_detached_afternm_byte6
//Requires: _8_Hacl_NaCl_crypto_box_open_detached_afternm
function _8_Hacl_NaCl_crypto_box_open_detached_afternm_byte6 (x0, x1, x2, x3, x4, x5) {
  return _8_Hacl_NaCl_crypto_box_open_detached_afternm(x0, x1, x2, x3, x4, x5);
}

//Provides: _12_Hacl_NaCl_crypto_box_open_easy_afternm
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _12_Hacl_NaCl_crypto_box_open_easy_afternm (m, c, clen, n, k) {
  var H = globalThis._HACL;
  var a_c = hacl_create_buffer(c,integers_int32_of_uint32(clen))
  var a_n = hacl_create_buffer(n,24)
  var a_k = hacl_create_buffer(k,32)
  var ret = H.NaCl.box_open_easy_afternm(a_c, a_n, a_k);
  hacl_blit_buf_to_bytes(ret[1], m, integers_int32_of_uint32(clen)+16)
  return integers_uint32_of_int32(ret[0]);
}
//Provides: _3_Hacl_NaCl_crypto_secretbox_easy
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _3_Hacl_NaCl_crypto_secretbox_easy (c, m, mlen, n, k) {
  var H = globalThis._HACL;
  var a_m = hacl_create_buffer(m,integers_int32_of_uint32(mlen))
  var a_n = hacl_create_buffer(n,24)
  var a_k = hacl_create_buffer(k,32)
  var ret = H.NaCl.secretbox_easy(a_m, a_n, a_k);
  hacl_blit_buf_to_bytes(ret[1], c, integers_int32_of_uint32(mlen)+16)
  return integers_uint32_of_int32(ret[0]);
}
//Provides: _4_Hacl_NaCl_crypto_secretbox_open_easy
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _4_Hacl_NaCl_crypto_secretbox_open_easy (m, c, clen, n, k) {
  var H = globalThis._HACL;
  var a_c = hacl_create_buffer(c,integers_int32_of_uint32(clen))
  var a_n = hacl_create_buffer(n,24)
  var a_k = hacl_create_buffer(k,32)
  var ret = H.NaCl.secretbox_open_easy(a_c, a_n, a_k);
  hacl_blit_buf_to_bytes(ret[1], m, integers_int32_of_uint32(clen)+16)
  return integers_uint32_of_int32(ret[0]);
}
//Provides: _19_Hacl_P256_compression_compressed_form
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _19_Hacl_P256_compression_compressed_form (b, result) {
  var H = globalThis._HACL;
  var a_b = hacl_create_buffer(b,64)
  var ret = H.P256.compression_compressed_form(a_b);
  hacl_blit_buf_to_bytes(ret[0], result, 33)
  return 0;
}
//Provides: _18_Hacl_P256_compression_not_compressed_form
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _18_Hacl_P256_compression_not_compressed_form (b, result) {
  var H = globalThis._HACL;
  var a_b = hacl_create_buffer(b,64)
  var ret = H.P256.compression_not_compressed_form(a_b);
  hacl_blit_buf_to_bytes(ret[0], result, 65)
  return 0;
}
//Provides: _17_Hacl_P256_decompression_compressed_form
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _17_Hacl_P256_decompression_compressed_form (b, result) {
  var H = globalThis._HACL;
  var a_b = hacl_create_buffer(b,33)
  var ret = H.P256.decompression_compressed_form(a_b);
  hacl_blit_buf_to_bytes(ret[1], result, 64)
  return (ret[0]?1:0);
}
//Provides: _16_Hacl_P256_decompression_not_compressed_form
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _16_Hacl_P256_decompression_not_compressed_form (b, result) {
  var H = globalThis._HACL;
  var a_b = hacl_create_buffer(b,65)
  var ret = H.P256.decompression_not_compressed_form(a_b);
  hacl_blit_buf_to_bytes(ret[1], result, 64)
  return (ret[0]?1:0);
}
//Provides: _7_Hacl_P256_ecdsa_sign_p256_sha2
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _7_Hacl_P256_ecdsa_sign_p256_sha2 (result, mlen, m, privkey, k) {
  var H = globalThis._HACL;
  var a_m = hacl_create_buffer(m,integers_int32_of_uint32(mlen))
  var a_privkey = hacl_create_buffer(privkey,32)
  var a_k = hacl_create_buffer(k,32)
  var ret = H.P256.ecdsa_sign_sha2(a_m, a_privkey, a_k);
  hacl_blit_buf_to_bytes(ret[1], result, 64)
  return (ret[0]?1:0);
}
//Provides: _10_Hacl_P256_ecdsa_sign_p256_without_hash
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _10_Hacl_P256_ecdsa_sign_p256_without_hash (result, mlen, m, privkey, k) {
  var H = globalThis._HACL;
  var a_m = hacl_create_buffer(m,integers_int32_of_uint32(mlen))
  var a_privkey = hacl_create_buffer(privkey,32)
  var a_k = hacl_create_buffer(k,32)
  var ret = H.P256.ecdsa_sign_without_hash(a_m, a_privkey, a_k);
  hacl_blit_buf_to_bytes(ret[1], result, 64)
  return (ret[0]?1:0);
}
//Provides: _11_Hacl_P256_ecdsa_verif_p256_sha2
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _11_Hacl_P256_ecdsa_verif_p256_sha2 (mlen, m, pubkey, r, s) {
  var H = globalThis._HACL;
  var a_m = hacl_create_buffer(m,integers_int32_of_uint32(mlen))
  var a_pubkey = hacl_create_buffer(pubkey,64)
  var a_r = hacl_create_buffer(r,32)
  var a_s = hacl_create_buffer(s,32)
  var ret = H.P256.ecdsa_verif_sha2(a_m, a_pubkey, a_r, a_s);
  return (ret[0]?1:0);
}
//Provides: _14_Hacl_P256_ecdsa_verif_without_hash
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _14_Hacl_P256_ecdsa_verif_without_hash (mlen, m, pubkey, r, s) {
  var H = globalThis._HACL;
  var a_m = hacl_create_buffer(m,integers_int32_of_uint32(mlen))
  var a_pubkey = hacl_create_buffer(pubkey,64)
  var a_r = hacl_create_buffer(r,32)
  var a_s = hacl_create_buffer(s,32)
  var ret = H.P256.ecdsa_verif_without_hash(a_m, a_pubkey, a_r, a_s);
  return (ret[0]?1:0);
}
//Provides: _20_Hacl_P256_ecp256dh_i
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _20_Hacl_P256_ecp256dh_i (result, scalar) {
  var H = globalThis._HACL;
  var a_scalar = hacl_create_buffer(scalar,32)
  var ret = H.P256.dh_initiator(a_scalar);
  hacl_blit_buf_to_bytes(ret[1], result, 64)
  return (ret[0]?1:0);
}
//Provides: _21_Hacl_P256_ecp256dh_r
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _21_Hacl_P256_ecp256dh_r (result, pubKey, scalar) {
  var H = globalThis._HACL;
  var a_pubKey = hacl_create_buffer(pubKey,64)
  var a_scalar = hacl_create_buffer(scalar,32)
  var ret = H.P256.dh_responder(a_pubKey, a_scalar);
  hacl_blit_buf_to_bytes(ret[1], result, 64)
  return (ret[0]?1:0);
}
//Provides: _22_Hacl_P256_is_more_than_zero_less_than_order
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _22_Hacl_P256_is_more_than_zero_less_than_order (pubKey) {
  var H = globalThis._HACL;
  var a_pubKey = hacl_create_buffer(pubKey,32)
  var ret = H.P256.is_more_than_zero_less_than_order(a_pubKey);
  return (ret[0]?1:0);
}
//Provides: _15_Hacl_P256_verify_q
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _15_Hacl_P256_verify_q (pubKey) {
  var H = globalThis._HACL;
  var a_pubKey = hacl_create_buffer(pubKey,64)
  var ret = H.P256.verify_q(a_pubKey);
  return (ret[0]?1:0);
}
//Provides: _10_Hacl_SHA3_sha3_224
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _10_Hacl_SHA3_sha3_224 (input_len, input, hash) {
  var H = globalThis._HACL;
  var a_input = hacl_create_buffer(input,integers_int32_of_uint32(input_len))
  var ret = H.SHA3.hash_224(a_input);
  hacl_blit_buf_to_bytes(ret[0], hash, 28)
  return 0;
}
//Provides: _11_Hacl_SHA3_sha3_256
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _11_Hacl_SHA3_sha3_256 (input_len, input, hash) {
  var H = globalThis._HACL;
  var a_input = hacl_create_buffer(input,integers_int32_of_uint32(input_len))
  var ret = H.SHA3.hash_256(a_input);
  hacl_blit_buf_to_bytes(ret[0], hash, 32)
  return 0;
}
//Provides: _12_Hacl_SHA3_sha3_384
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _12_Hacl_SHA3_sha3_384 (input_len, input, hash) {
  var H = globalThis._HACL;
  var a_input = hacl_create_buffer(input,integers_int32_of_uint32(input_len))
  var ret = H.SHA3.hash_384(a_input);
  hacl_blit_buf_to_bytes(ret[0], hash, 48)
  return 0;
}
//Provides: _13_Hacl_SHA3_sha3_512
//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes
//Requires: integers_int32_of_uint32, integers_uint32_of_int32
function _13_Hacl_SHA3_sha3_512 (input_len, input, hash) {
  var H = globalThis._HACL;
  var a_input = hacl_create_buffer(input,integers_int32_of_uint32(input_len))
  var ret = H.SHA3.hash_512(a_input);
  hacl_blit_buf_to_bytes(ret[0], hash, 64)
  return 0;
}
//Provides: _10_EverCrypt_AutoConfig2_has_rdrand
//Requires: integers_uint32_of_int32
function _10_EverCrypt_AutoConfig2_has_rdrand(_unit) { return 0 }

//Provides: _11_EverCrypt_AutoConfig2_has_avx512
//Requires: integers_uint32_of_int32
function _11_EverCrypt_AutoConfig2_has_avx512(_unit) { return 0 }

//Provides: _12_EverCrypt_AutoConfig2_wants_vale
//Requires: integers_uint32_of_int32
function _12_EverCrypt_AutoConfig2_wants_vale(_unit) { return 0 }

//Provides: _13_EverCrypt_AutoConfig2_wants_hacl
//Requires: integers_uint32_of_int32
function _13_EverCrypt_AutoConfig2_wants_hacl(_unit) { return 0 }

//Provides: _14_EverCrypt_AutoConfig2_wants_openssl
//Requires: integers_uint32_of_int32
function _14_EverCrypt_AutoConfig2_wants_openssl(_unit) { return 0 }

//Provides: _15_EverCrypt_AutoConfig2_wants_bcrypt
//Requires: integers_uint32_of_int32
function _15_EverCrypt_AutoConfig2_wants_bcrypt(_unit) { return 0 }

//Provides: _1_EverCrypt_AutoConfig2_has_shaext
//Requires: integers_uint32_of_int32
function _1_EverCrypt_AutoConfig2_has_shaext(_unit) { return 0 }

//Provides: _1_EverCrypt_DRBG_reseed_interval
//Requires: integers_uint32_of_int32
function _1_EverCrypt_DRBG_reseed_interval(_unit) { return integers_uint32_of_int32(1024) }

//Provides: _1_EverCrypt_StaticConfig_hacl
//Requires: integers_uint32_of_int32
function _1_EverCrypt_StaticConfig_hacl(_unit) { return 0 }

//Provides: _1_Hacl_Frodo1344_crypto_bytes
//Requires: integers_uint32_of_int32
function _1_Hacl_Frodo1344_crypto_bytes(_unit) { return integers_uint32_of_int32(32) }

//Provides: _1_Hacl_Frodo640_crypto_bytes
//Requires: integers_uint32_of_int32
function _1_Hacl_Frodo640_crypto_bytes(_unit) { return integers_uint32_of_int32(16) }

//Provides: _1_Hacl_Frodo64_crypto_bytes
//Requires: integers_uint32_of_int32
function _1_Hacl_Frodo64_crypto_bytes(_unit) { return integers_uint32_of_int32(16) }

//Provides: _1_Hacl_Frodo976_crypto_bytes
//Requires: integers_uint32_of_int32
function _1_Hacl_Frodo976_crypto_bytes(_unit) { return integers_uint32_of_int32(24) }

//Provides: _1_Hacl_HMAC_DRBG_reseed_interval
//Requires: integers_uint32_of_int32
function _1_Hacl_HMAC_DRBG_reseed_interval(_unit) { return integers_uint32_of_int32(1024) }

//Provides: _1_Hacl_Poly1305_128_blocklen
//Requires: integers_uint32_of_int32
function _1_Hacl_Poly1305_128_blocklen(_unit) { return integers_uint32_of_int32(16) }

//Provides: _1_Hacl_Poly1305_256_blocklen
//Requires: integers_uint32_of_int32
function _1_Hacl_Poly1305_256_blocklen(_unit) { return integers_uint32_of_int32(16) }

//Provides: _1_Hacl_Poly1305_32_blocklen
//Requires: integers_uint32_of_int32
function _1_Hacl_Poly1305_32_blocklen(_unit) { return integers_uint32_of_int32(16) }

//Provides: _2_EverCrypt_AutoConfig2_has_aesni
//Requires: integers_uint32_of_int32
function _2_EverCrypt_AutoConfig2_has_aesni(_unit) { return 0 }

//Provides: _2_EverCrypt_DRBG_max_output_length
//Requires: integers_uint32_of_int32
function _2_EverCrypt_DRBG_max_output_length(_unit) { return integers_uint32_of_int32(65536) }

//Provides: _2_EverCrypt_StaticConfig_vale
//Requires: integers_uint32_of_int32
function _2_EverCrypt_StaticConfig_vale(_unit) { return 0 }

//Provides: _2_Hacl_Frodo1344_crypto_publickeybytes
//Requires: integers_uint32_of_int32
function _2_Hacl_Frodo1344_crypto_publickeybytes(_unit) { return integers_uint32_of_int32(21520) }

//Provides: _2_Hacl_Frodo640_crypto_publickeybytes
//Requires: integers_uint32_of_int32
function _2_Hacl_Frodo640_crypto_publickeybytes(_unit) { return integers_uint32_of_int32(9616) }

//Provides: _2_Hacl_Frodo64_crypto_publickeybytes
//Requires: integers_uint32_of_int32
function _2_Hacl_Frodo64_crypto_publickeybytes(_unit) { return integers_uint32_of_int32(976) }

//Provides: _2_Hacl_Frodo976_crypto_publickeybytes
//Requires: integers_uint32_of_int32
function _2_Hacl_Frodo976_crypto_publickeybytes(_unit) { return integers_uint32_of_int32(15632) }

//Provides: _2_Hacl_HMAC_DRBG_max_output_length
//Requires: integers_uint32_of_int32
function _2_Hacl_HMAC_DRBG_max_output_length(_unit) { return integers_uint32_of_int32(65536) }

//Provides: _33_EverCrypt_AutoConfig2_has_vec128
//Requires: integers_uint32_of_int32
function _33_EverCrypt_AutoConfig2_has_vec128(_unit) { return 0 }

//Provides: _34_EverCrypt_AutoConfig2_has_vec256
//Requires: integers_uint32_of_int32
function _34_EverCrypt_AutoConfig2_has_vec256(_unit) { return 0 }

//Provides: _3_EverCrypt_AutoConfig2_has_pclmulqdq
//Requires: integers_uint32_of_int32
function _3_EverCrypt_AutoConfig2_has_pclmulqdq(_unit) { return 0 }

//Provides: _3_EverCrypt_DRBG_max_length
//Requires: integers_uint32_of_int32
function _3_EverCrypt_DRBG_max_length(_unit) { return integers_uint32_of_int32(65536) }

//Provides: _3_EverCrypt_StaticConfig_openssl
//Requires: integers_uint32_of_int32
function _3_EverCrypt_StaticConfig_openssl(_unit) { return 0 }

//Provides: _3_Hacl_Frodo1344_crypto_secretkeybytes
//Requires: integers_uint32_of_int32
function _3_Hacl_Frodo1344_crypto_secretkeybytes(_unit) { return integers_uint32_of_int32(43088) }

//Provides: _3_Hacl_Frodo640_crypto_secretkeybytes
//Requires: integers_uint32_of_int32
function _3_Hacl_Frodo640_crypto_secretkeybytes(_unit) { return integers_uint32_of_int32(19888) }

//Provides: _3_Hacl_Frodo64_crypto_secretkeybytes
//Requires: integers_uint32_of_int32
function _3_Hacl_Frodo64_crypto_secretkeybytes(_unit) { return integers_uint32_of_int32(2032) }

//Provides: _3_Hacl_Frodo976_crypto_secretkeybytes
//Requires: integers_uint32_of_int32
function _3_Hacl_Frodo976_crypto_secretkeybytes(_unit) { return integers_uint32_of_int32(31296) }

//Provides: _3_Hacl_HMAC_DRBG_max_length
//Requires: integers_uint32_of_int32
function _3_Hacl_HMAC_DRBG_max_length(_unit) { return integers_uint32_of_int32(65536) }

//Provides: _4_EverCrypt_AutoConfig2_has_avx2
//Requires: integers_uint32_of_int32
function _4_EverCrypt_AutoConfig2_has_avx2(_unit) { return 0 }

//Provides: _4_EverCrypt_DRBG_max_personalization_string_length
//Requires: integers_uint32_of_int32
function _4_EverCrypt_DRBG_max_personalization_string_length(_unit) { return integers_uint32_of_int32(65536) }

//Provides: _4_EverCrypt_StaticConfig_bcrypt
//Requires: integers_uint32_of_int32
function _4_EverCrypt_StaticConfig_bcrypt(_unit) { return 0 }

//Provides: _4_Hacl_Frodo1344_crypto_ciphertextbytes
//Requires: integers_uint32_of_int32
function _4_Hacl_Frodo1344_crypto_ciphertextbytes(_unit) { return integers_uint32_of_int32(21632) }

//Provides: _4_Hacl_Frodo640_crypto_ciphertextbytes
//Requires: integers_uint32_of_int32
function _4_Hacl_Frodo640_crypto_ciphertextbytes(_unit) { return integers_uint32_of_int32(9720) }

//Provides: _4_Hacl_Frodo64_crypto_ciphertextbytes
//Requires: integers_uint32_of_int32
function _4_Hacl_Frodo64_crypto_ciphertextbytes(_unit) { return integers_uint32_of_int32(1080) }

//Provides: _4_Hacl_Frodo976_crypto_ciphertextbytes
//Requires: integers_uint32_of_int32
function _4_Hacl_Frodo976_crypto_ciphertextbytes(_unit) { return integers_uint32_of_int32(15744) }

//Provides: _4_Hacl_HMAC_DRBG_max_personalization_string_length
//Requires: integers_uint32_of_int32
function _4_Hacl_HMAC_DRBG_max_personalization_string_length(_unit) { return integers_uint32_of_int32(65536) }

//Provides: _5_EverCrypt_AutoConfig2_has_avx
//Requires: integers_uint32_of_int32
function _5_EverCrypt_AutoConfig2_has_avx(_unit) { return 0 }

//Provides: _5_EverCrypt_DRBG_max_additional_input_length
//Requires: integers_uint32_of_int32
function _5_EverCrypt_DRBG_max_additional_input_length(_unit) { return integers_uint32_of_int32(65536) }

//Provides: _5_Hacl_HMAC_DRBG_max_additional_input_length
//Requires: integers_uint32_of_int32
function _5_Hacl_HMAC_DRBG_max_additional_input_length(_unit) { return integers_uint32_of_int32(65536) }

//Provides: _6_EverCrypt_AutoConfig2_has_bmi2
//Requires: integers_uint32_of_int32
function _6_EverCrypt_AutoConfig2_has_bmi2(_unit) { return 0 }

//Provides: _7_EverCrypt_AutoConfig2_has_adx
//Requires: integers_uint32_of_int32
function _7_EverCrypt_AutoConfig2_has_adx(_unit) { return 0 }

//Provides: _8_EverCrypt_AutoConfig2_has_sse
//Requires: integers_uint32_of_int32
function _8_EverCrypt_AutoConfig2_has_sse(_unit) { return 0 }

//Provides: _9_EverCrypt_AutoConfig2_has_movbe
//Requires: integers_uint32_of_int32
function _9_EverCrypt_AutoConfig2_has_movbe(_unit) { return 0 }

//Provides: _1_EverCrypt_AEAD_alg_of_state
//Requires: caml_failwith
function _1_EverCrypt_AEAD_alg_of_state (x0) {
  caml_failwith('EverCrypt_AEAD_alg_of_state unimplemetned');
}

//Provides: _2_EverCrypt_AEAD_create_in
//Requires: caml_failwith
function _2_EverCrypt_AEAD_create_in (x0, x1, x2) {
  caml_failwith('EverCrypt_AEAD_create_in unimplemetned');
}

//Provides: _10_EverCrypt_AEAD_decrypt
//Requires: caml_failwith
function _10_EverCrypt_AEAD_decrypt (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  caml_failwith('EverCrypt_AEAD_decrypt unimplemetned');
}

//Provides: _10_EverCrypt_AEAD_decrypt_byte9
//Requires: _10_EverCrypt_AEAD_decrypt
function _10_EverCrypt_AEAD_decrypt_byte9 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  return _10_EverCrypt_AEAD_decrypt(x0, x1, x2, x3, x4, x5, x6, x7, x8);
}

//Provides: _16_EverCrypt_AEAD_decrypt_expand
//Requires: caml_failwith
function _16_EverCrypt_AEAD_decrypt_expand (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) {
  caml_failwith('EverCrypt_AEAD_decrypt_expand unimplemetned');
}

//Provides: _16_EverCrypt_AEAD_decrypt_expand_byte10
//Requires: _16_EverCrypt_AEAD_decrypt_expand
function _16_EverCrypt_AEAD_decrypt_expand_byte10 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) {
  return _16_EverCrypt_AEAD_decrypt_expand(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9);
}

//Provides: _13_EverCrypt_AEAD_decrypt_expand_aes128_gcm
//Requires: caml_failwith
function _13_EverCrypt_AEAD_decrypt_expand_aes128_gcm (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  caml_failwith('EverCrypt_AEAD_decrypt_expand_aes128_gcm unimplemetned');
}

//Provides: _13_EverCrypt_AEAD_decrypt_expand_aes128_gcm_byte9
//Requires: _13_EverCrypt_AEAD_decrypt_expand_aes128_gcm
function _13_EverCrypt_AEAD_decrypt_expand_aes128_gcm_byte9 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  return _13_EverCrypt_AEAD_decrypt_expand_aes128_gcm(x0, x1, x2, x3, x4, x5, x6, x7, x8);
}

//Provides: _11_EverCrypt_AEAD_decrypt_expand_aes128_gcm_no_check
//Requires: caml_failwith
function _11_EverCrypt_AEAD_decrypt_expand_aes128_gcm_no_check (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  caml_failwith('EverCrypt_AEAD_decrypt_expand_aes128_gcm_no_check unimplemetned');
}

//Provides: _11_EverCrypt_AEAD_decrypt_expand_aes128_gcm_no_check_byte9
//Requires: _11_EverCrypt_AEAD_decrypt_expand_aes128_gcm_no_check
function _11_EverCrypt_AEAD_decrypt_expand_aes128_gcm_no_check_byte9 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  return _11_EverCrypt_AEAD_decrypt_expand_aes128_gcm_no_check(x0, x1, x2, x3, x4, x5, x6, x7, x8);
}

//Provides: _14_EverCrypt_AEAD_decrypt_expand_aes256_gcm
//Requires: caml_failwith
function _14_EverCrypt_AEAD_decrypt_expand_aes256_gcm (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  caml_failwith('EverCrypt_AEAD_decrypt_expand_aes256_gcm unimplemetned');
}

//Provides: _14_EverCrypt_AEAD_decrypt_expand_aes256_gcm_byte9
//Requires: _14_EverCrypt_AEAD_decrypt_expand_aes256_gcm
function _14_EverCrypt_AEAD_decrypt_expand_aes256_gcm_byte9 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  return _14_EverCrypt_AEAD_decrypt_expand_aes256_gcm(x0, x1, x2, x3, x4, x5, x6, x7, x8);
}

//Provides: _12_EverCrypt_AEAD_decrypt_expand_aes256_gcm_no_check
//Requires: caml_failwith
function _12_EverCrypt_AEAD_decrypt_expand_aes256_gcm_no_check (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  caml_failwith('EverCrypt_AEAD_decrypt_expand_aes256_gcm_no_check unimplemetned');
}

//Provides: _12_EverCrypt_AEAD_decrypt_expand_aes256_gcm_no_check_byte9
//Requires: _12_EverCrypt_AEAD_decrypt_expand_aes256_gcm_no_check
function _12_EverCrypt_AEAD_decrypt_expand_aes256_gcm_no_check_byte9 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  return _12_EverCrypt_AEAD_decrypt_expand_aes256_gcm_no_check(x0, x1, x2, x3, x4, x5, x6, x7, x8);
}

//Provides: _15_EverCrypt_AEAD_decrypt_expand_chacha20_poly1305
//Requires: caml_failwith
function _15_EverCrypt_AEAD_decrypt_expand_chacha20_poly1305 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  caml_failwith('EverCrypt_AEAD_decrypt_expand_chacha20_poly1305 unimplemetned');
}

//Provides: _15_EverCrypt_AEAD_decrypt_expand_chacha20_poly1305_byte9
//Requires: _15_EverCrypt_AEAD_decrypt_expand_chacha20_poly1305
function _15_EverCrypt_AEAD_decrypt_expand_chacha20_poly1305_byte9 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  return _15_EverCrypt_AEAD_decrypt_expand_chacha20_poly1305(x0, x1, x2, x3, x4, x5, x6, x7, x8);
}

//Provides: _3_EverCrypt_AEAD_encrypt
//Requires: caml_failwith
function _3_EverCrypt_AEAD_encrypt (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  caml_failwith('EverCrypt_AEAD_encrypt unimplemetned');
}

//Provides: _3_EverCrypt_AEAD_encrypt_byte9
//Requires: _3_EverCrypt_AEAD_encrypt
function _3_EverCrypt_AEAD_encrypt_byte9 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  return _3_EverCrypt_AEAD_encrypt(x0, x1, x2, x3, x4, x5, x6, x7, x8);
}

//Provides: _9_EverCrypt_AEAD_encrypt_expand
//Requires: caml_failwith
function _9_EverCrypt_AEAD_encrypt_expand (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) {
  caml_failwith('EverCrypt_AEAD_encrypt_expand unimplemetned');
}

//Provides: _9_EverCrypt_AEAD_encrypt_expand_byte10
//Requires: _9_EverCrypt_AEAD_encrypt_expand
function _9_EverCrypt_AEAD_encrypt_expand_byte10 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) {
  return _9_EverCrypt_AEAD_encrypt_expand(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9);
}

//Provides: _6_EverCrypt_AEAD_encrypt_expand_aes128_gcm
//Requires: caml_failwith
function _6_EverCrypt_AEAD_encrypt_expand_aes128_gcm (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  caml_failwith('EverCrypt_AEAD_encrypt_expand_aes128_gcm unimplemetned');
}

//Provides: _6_EverCrypt_AEAD_encrypt_expand_aes128_gcm_byte9
//Requires: _6_EverCrypt_AEAD_encrypt_expand_aes128_gcm
function _6_EverCrypt_AEAD_encrypt_expand_aes128_gcm_byte9 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  return _6_EverCrypt_AEAD_encrypt_expand_aes128_gcm(x0, x1, x2, x3, x4, x5, x6, x7, x8);
}

//Provides: _4_EverCrypt_AEAD_encrypt_expand_aes128_gcm_no_check
//Requires: caml_failwith
function _4_EverCrypt_AEAD_encrypt_expand_aes128_gcm_no_check (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  caml_failwith('EverCrypt_AEAD_encrypt_expand_aes128_gcm_no_check unimplemetned');
}

//Provides: _4_EverCrypt_AEAD_encrypt_expand_aes128_gcm_no_check_byte9
//Requires: _4_EverCrypt_AEAD_encrypt_expand_aes128_gcm_no_check
function _4_EverCrypt_AEAD_encrypt_expand_aes128_gcm_no_check_byte9 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  return _4_EverCrypt_AEAD_encrypt_expand_aes128_gcm_no_check(x0, x1, x2, x3, x4, x5, x6, x7, x8);
}

//Provides: _7_EverCrypt_AEAD_encrypt_expand_aes256_gcm
//Requires: caml_failwith
function _7_EverCrypt_AEAD_encrypt_expand_aes256_gcm (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  caml_failwith('EverCrypt_AEAD_encrypt_expand_aes256_gcm unimplemetned');
}

//Provides: _7_EverCrypt_AEAD_encrypt_expand_aes256_gcm_byte9
//Requires: _7_EverCrypt_AEAD_encrypt_expand_aes256_gcm
function _7_EverCrypt_AEAD_encrypt_expand_aes256_gcm_byte9 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  return _7_EverCrypt_AEAD_encrypt_expand_aes256_gcm(x0, x1, x2, x3, x4, x5, x6, x7, x8);
}

//Provides: _5_EverCrypt_AEAD_encrypt_expand_aes256_gcm_no_check
//Requires: caml_failwith
function _5_EverCrypt_AEAD_encrypt_expand_aes256_gcm_no_check (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  caml_failwith('EverCrypt_AEAD_encrypt_expand_aes256_gcm_no_check unimplemetned');
}

//Provides: _5_EverCrypt_AEAD_encrypt_expand_aes256_gcm_no_check_byte9
//Requires: _5_EverCrypt_AEAD_encrypt_expand_aes256_gcm_no_check
function _5_EverCrypt_AEAD_encrypt_expand_aes256_gcm_no_check_byte9 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  return _5_EverCrypt_AEAD_encrypt_expand_aes256_gcm_no_check(x0, x1, x2, x3, x4, x5, x6, x7, x8);
}

//Provides: _8_EverCrypt_AEAD_encrypt_expand_chacha20_poly1305
//Requires: caml_failwith
function _8_EverCrypt_AEAD_encrypt_expand_chacha20_poly1305 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  caml_failwith('EverCrypt_AEAD_encrypt_expand_chacha20_poly1305 unimplemetned');
}

//Provides: _8_EverCrypt_AEAD_encrypt_expand_chacha20_poly1305_byte9
//Requires: _8_EverCrypt_AEAD_encrypt_expand_chacha20_poly1305
function _8_EverCrypt_AEAD_encrypt_expand_chacha20_poly1305_byte9 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  return _8_EverCrypt_AEAD_encrypt_expand_chacha20_poly1305(x0, x1, x2, x3, x4, x5, x6, x7, x8);
}

//Provides: _17_EverCrypt_AEAD_free
//Requires: caml_failwith
function _17_EverCrypt_AEAD_free (x0) {
  caml_failwith('EverCrypt_AEAD_free unimplemetned');
}

//Provides: _21_EverCrypt_AutoConfig2_disable_adx
//Requires: caml_failwith
function _21_EverCrypt_AutoConfig2_disable_adx (x0) {
  caml_failwith('EverCrypt_AutoConfig2_disable_adx unimplemetned');
}

//Provides: _23_EverCrypt_AutoConfig2_disable_aesni
//Requires: caml_failwith
function _23_EverCrypt_AutoConfig2_disable_aesni (x0) {
  caml_failwith('EverCrypt_AutoConfig2_disable_aesni unimplemetned');
}

//Provides: _19_EverCrypt_AutoConfig2_disable_avx
//Requires: caml_failwith
function _19_EverCrypt_AutoConfig2_disable_avx (x0) {
  caml_failwith('EverCrypt_AutoConfig2_disable_avx unimplemetned');
}

//Provides: _18_EverCrypt_AutoConfig2_disable_avx2
//Requires: caml_failwith
function _18_EverCrypt_AutoConfig2_disable_avx2 (x0) {
  caml_failwith('EverCrypt_AutoConfig2_disable_avx2 unimplemetned');
}

//Provides: _28_EverCrypt_AutoConfig2_disable_avx512
//Requires: caml_failwith
function _28_EverCrypt_AutoConfig2_disable_avx512 (x0) {
  caml_failwith('EverCrypt_AutoConfig2_disable_avx512 unimplemetned');
}

//Provides: _32_EverCrypt_AutoConfig2_disable_bcrypt
//Requires: caml_failwith
function _32_EverCrypt_AutoConfig2_disable_bcrypt (x0) {
  caml_failwith('EverCrypt_AutoConfig2_disable_bcrypt unimplemetned');
}

//Provides: _20_EverCrypt_AutoConfig2_disable_bmi2
//Requires: caml_failwith
function _20_EverCrypt_AutoConfig2_disable_bmi2 (x0) {
  caml_failwith('EverCrypt_AutoConfig2_disable_bmi2 unimplemetned');
}

//Provides: _30_EverCrypt_AutoConfig2_disable_hacl
//Requires: caml_failwith
function _30_EverCrypt_AutoConfig2_disable_hacl (x0) {
  caml_failwith('EverCrypt_AutoConfig2_disable_hacl unimplemetned');
}

//Provides: _26_EverCrypt_AutoConfig2_disable_movbe
//Requires: caml_failwith
function _26_EverCrypt_AutoConfig2_disable_movbe (x0) {
  caml_failwith('EverCrypt_AutoConfig2_disable_movbe unimplemetned');
}

//Provides: _31_EverCrypt_AutoConfig2_disable_openssl
//Requires: caml_failwith
function _31_EverCrypt_AutoConfig2_disable_openssl (x0) {
  caml_failwith('EverCrypt_AutoConfig2_disable_openssl unimplemetned');
}

//Provides: _24_EverCrypt_AutoConfig2_disable_pclmulqdq
//Requires: caml_failwith
function _24_EverCrypt_AutoConfig2_disable_pclmulqdq (x0) {
  caml_failwith('EverCrypt_AutoConfig2_disable_pclmulqdq unimplemetned');
}

//Provides: _27_EverCrypt_AutoConfig2_disable_rdrand
//Requires: caml_failwith
function _27_EverCrypt_AutoConfig2_disable_rdrand (x0) {
  caml_failwith('EverCrypt_AutoConfig2_disable_rdrand unimplemetned');
}

//Provides: _22_EverCrypt_AutoConfig2_disable_shaext
//Requires: caml_failwith
function _22_EverCrypt_AutoConfig2_disable_shaext (x0) {
  caml_failwith('EverCrypt_AutoConfig2_disable_shaext unimplemetned');
}

//Provides: _25_EverCrypt_AutoConfig2_disable_sse
//Requires: caml_failwith
function _25_EverCrypt_AutoConfig2_disable_sse (x0) {
  caml_failwith('EverCrypt_AutoConfig2_disable_sse unimplemetned');
}

//Provides: _29_EverCrypt_AutoConfig2_disable_vale
//Requires: caml_failwith
function _29_EverCrypt_AutoConfig2_disable_vale (x0) {
  caml_failwith('EverCrypt_AutoConfig2_disable_vale unimplemetned');
}

//Provides: _16_EverCrypt_AutoConfig2_recall
//Requires: caml_failwith
function _16_EverCrypt_AutoConfig2_recall (x0) {
  caml_failwith('EverCrypt_AutoConfig2_recall unimplemetned');
}

//Provides: _2_EverCrypt_CTR_alg_of_state
//Requires: caml_failwith
function _2_EverCrypt_CTR_alg_of_state (x0) {
  caml_failwith('EverCrypt_CTR_alg_of_state unimplemetned');
}

//Provides: _3_EverCrypt_CTR_create_in
//Requires: caml_failwith
function _3_EverCrypt_CTR_create_in (x0, x1, x2, x3, x4, x5) {
  caml_failwith('EverCrypt_CTR_create_in unimplemetned');
}

//Provides: _3_EverCrypt_CTR_create_in_byte6
//Requires: _3_EverCrypt_CTR_create_in
function _3_EverCrypt_CTR_create_in_byte6 (x0, x1, x2, x3, x4, x5) {
  return _3_EverCrypt_CTR_create_in(x0, x1, x2, x3, x4, x5);
}

//Provides: _6_EverCrypt_CTR_free
//Requires: caml_failwith
function _6_EverCrypt_CTR_free (x0) {
  caml_failwith('EverCrypt_CTR_free unimplemetned');
}

//Provides: _4_EverCrypt_CTR_init
//Requires: caml_failwith
function _4_EverCrypt_CTR_init (x0, x1, x2, x3, x4) {
  caml_failwith('EverCrypt_CTR_init unimplemetned');
}

//Provides: _5_EverCrypt_CTR_update_block
//Requires: caml_failwith
function _5_EverCrypt_CTR_update_block (x0, x1, x2) {
  caml_failwith('EverCrypt_CTR_update_block unimplemetned');
}

//Provides: _1_EverCrypt_CTR_xor8
//Requires: caml_failwith
function _1_EverCrypt_CTR_xor8 (x0, x1) {
  caml_failwith('EverCrypt_CTR_xor8 unimplemetned');
}

//Provides: _2_EverCrypt_Chacha20Poly1305_aead_decrypt
//Requires: caml_failwith
function _2_EverCrypt_Chacha20Poly1305_aead_decrypt (x0, x1, x2, x3, x4, x5, x6, x7) {
  caml_failwith('EverCrypt_Chacha20Poly1305_aead_decrypt unimplemetned');
}

//Provides: _2_EverCrypt_Chacha20Poly1305_aead_decrypt_byte8
//Requires: _2_EverCrypt_Chacha20Poly1305_aead_decrypt
function _2_EverCrypt_Chacha20Poly1305_aead_decrypt_byte8 (x0, x1, x2, x3, x4, x5, x6, x7) {
  return _2_EverCrypt_Chacha20Poly1305_aead_decrypt(x0, x1, x2, x3, x4, x5, x6, x7);
}

//Provides: _1_EverCrypt_Chacha20Poly1305_aead_encrypt
//Requires: caml_failwith
function _1_EverCrypt_Chacha20Poly1305_aead_encrypt (x0, x1, x2, x3, x4, x5, x6, x7) {
  caml_failwith('EverCrypt_Chacha20Poly1305_aead_encrypt unimplemetned');
}

//Provides: _1_EverCrypt_Chacha20Poly1305_aead_encrypt_byte8
//Requires: _1_EverCrypt_Chacha20Poly1305_aead_encrypt
function _1_EverCrypt_Chacha20Poly1305_aead_encrypt_byte8 (x0, x1, x2, x3, x4, x5, x6, x7) {
  return _1_EverCrypt_Chacha20Poly1305_aead_encrypt(x0, x1, x2, x3, x4, x5, x6, x7);
}

//Provides: _1_EverCrypt_Cipher_chacha20
//Requires: caml_failwith
function _1_EverCrypt_Cipher_chacha20 (x0, x1, x2, x3, x4, x5) {
  caml_failwith('EverCrypt_Cipher_chacha20 unimplemetned');
}

//Provides: _1_EverCrypt_Cipher_chacha20_byte6
//Requires: _1_EverCrypt_Cipher_chacha20
function _1_EverCrypt_Cipher_chacha20_byte6 (x0, x1, x2, x3, x4, x5) {
  return _1_EverCrypt_Cipher_chacha20(x0, x1, x2, x3, x4, x5);
}

//Provides: _3_EverCrypt_Curve25519_ecdh
//Requires: caml_failwith
function _3_EverCrypt_Curve25519_ecdh (x0, x1, x2) {
  caml_failwith('EverCrypt_Curve25519_ecdh unimplemetned');
}

//Provides: _1_EverCrypt_Curve25519_secret_to_public
//Requires: caml_failwith
function _1_EverCrypt_Curve25519_secret_to_public (x0, x1) {
  caml_failwith('EverCrypt_Curve25519_secret_to_public unimplemetned');
}

//Provides: _7_EverCrypt_DRBG_create
//Requires: caml_failwith
function _7_EverCrypt_DRBG_create (x0) {
  caml_failwith('EverCrypt_DRBG_create unimplemetned');
}

//Provides: _26_EverCrypt_DRBG_generate
//Requires: caml_failwith
function _26_EverCrypt_DRBG_generate (x0, x1, x2, x3, x4) {
  caml_failwith('EverCrypt_DRBG_generate unimplemetned');
}

//Provides: _16_EverCrypt_DRBG_generate_sha1
//Requires: caml_failwith
function _16_EverCrypt_DRBG_generate_sha1 (x0, x1, x2, x3, x4) {
  caml_failwith('EverCrypt_DRBG_generate_sha1 unimplemetned');
}

//Provides: _17_EverCrypt_DRBG_generate_sha2_256
//Requires: caml_failwith
function _17_EverCrypt_DRBG_generate_sha2_256 (x0, x1, x2, x3, x4) {
  caml_failwith('EverCrypt_DRBG_generate_sha2_256 unimplemetned');
}

//Provides: _18_EverCrypt_DRBG_generate_sha2_384
//Requires: caml_failwith
function _18_EverCrypt_DRBG_generate_sha2_384 (x0, x1, x2, x3, x4) {
  caml_failwith('EverCrypt_DRBG_generate_sha2_384 unimplemetned');
}

//Provides: _19_EverCrypt_DRBG_generate_sha2_512
//Requires: caml_failwith
function _19_EverCrypt_DRBG_generate_sha2_512 (x0, x1, x2, x3, x4) {
  caml_failwith('EverCrypt_DRBG_generate_sha2_512 unimplemetned');
}

//Provides: _24_EverCrypt_DRBG_instantiate
//Requires: caml_failwith
function _24_EverCrypt_DRBG_instantiate (x0, x1, x2) {
  caml_failwith('EverCrypt_DRBG_instantiate unimplemetned');
}

//Provides: _8_EverCrypt_DRBG_instantiate_sha1
//Requires: caml_failwith
function _8_EverCrypt_DRBG_instantiate_sha1 (x0, x1, x2) {
  caml_failwith('EverCrypt_DRBG_instantiate_sha1 unimplemetned');
}

//Provides: _9_EverCrypt_DRBG_instantiate_sha2_256
//Requires: caml_failwith
function _9_EverCrypt_DRBG_instantiate_sha2_256 (x0, x1, x2) {
  caml_failwith('EverCrypt_DRBG_instantiate_sha2_256 unimplemetned');
}

//Provides: _10_EverCrypt_DRBG_instantiate_sha2_384
//Requires: caml_failwith
function _10_EverCrypt_DRBG_instantiate_sha2_384 (x0, x1, x2) {
  caml_failwith('EverCrypt_DRBG_instantiate_sha2_384 unimplemetned');
}

//Provides: _11_EverCrypt_DRBG_instantiate_sha2_512
//Requires: caml_failwith
function _11_EverCrypt_DRBG_instantiate_sha2_512 (x0, x1, x2) {
  caml_failwith('EverCrypt_DRBG_instantiate_sha2_512 unimplemetned');
}

//Provides: _6_EverCrypt_DRBG_min_length
//Requires: caml_failwith
function _6_EverCrypt_DRBG_min_length (x0) {
  caml_failwith('EverCrypt_DRBG_min_length unimplemetned');
}

//Provides: _25_EverCrypt_DRBG_reseed
//Requires: caml_failwith
function _25_EverCrypt_DRBG_reseed (x0, x1, x2) {
  caml_failwith('EverCrypt_DRBG_reseed unimplemetned');
}

//Provides: _12_EverCrypt_DRBG_reseed_sha1
//Requires: caml_failwith
function _12_EverCrypt_DRBG_reseed_sha1 (x0, x1, x2) {
  caml_failwith('EverCrypt_DRBG_reseed_sha1 unimplemetned');
}

//Provides: _13_EverCrypt_DRBG_reseed_sha2_256
//Requires: caml_failwith
function _13_EverCrypt_DRBG_reseed_sha2_256 (x0, x1, x2) {
  caml_failwith('EverCrypt_DRBG_reseed_sha2_256 unimplemetned');
}

//Provides: _14_EverCrypt_DRBG_reseed_sha2_384
//Requires: caml_failwith
function _14_EverCrypt_DRBG_reseed_sha2_384 (x0, x1, x2) {
  caml_failwith('EverCrypt_DRBG_reseed_sha2_384 unimplemetned');
}

//Provides: _15_EverCrypt_DRBG_reseed_sha2_512
//Requires: caml_failwith
function _15_EverCrypt_DRBG_reseed_sha2_512 (x0, x1, x2) {
  caml_failwith('EverCrypt_DRBG_reseed_sha2_512 unimplemetned');
}

//Provides: _27_EverCrypt_DRBG_uninstantiate
//Requires: caml_failwith
function _27_EverCrypt_DRBG_uninstantiate (x0) {
  caml_failwith('EverCrypt_DRBG_uninstantiate unimplemetned');
}

//Provides: _20_EverCrypt_DRBG_uninstantiate_sha1
//Requires: caml_failwith
function _20_EverCrypt_DRBG_uninstantiate_sha1 (x0) {
  caml_failwith('EverCrypt_DRBG_uninstantiate_sha1 unimplemetned');
}

//Provides: _21_EverCrypt_DRBG_uninstantiate_sha2_256
//Requires: caml_failwith
function _21_EverCrypt_DRBG_uninstantiate_sha2_256 (x0) {
  caml_failwith('EverCrypt_DRBG_uninstantiate_sha2_256 unimplemetned');
}

//Provides: _22_EverCrypt_DRBG_uninstantiate_sha2_384
//Requires: caml_failwith
function _22_EverCrypt_DRBG_uninstantiate_sha2_384 (x0) {
  caml_failwith('EverCrypt_DRBG_uninstantiate_sha2_384 unimplemetned');
}

//Provides: _23_EverCrypt_DRBG_uninstantiate_sha2_512
//Requires: caml_failwith
function _23_EverCrypt_DRBG_uninstantiate_sha2_512 (x0) {
  caml_failwith('EverCrypt_DRBG_uninstantiate_sha2_512 unimplemetned');
}

//Provides: _4_EverCrypt_Ed25519_expand_keys
//Requires: caml_failwith
function _4_EverCrypt_Ed25519_expand_keys (x0, x1) {
  caml_failwith('EverCrypt_Ed25519_expand_keys unimplemetned');
}

//Provides: _3_EverCrypt_Ed25519_secret_to_public
//Requires: caml_failwith
function _3_EverCrypt_Ed25519_secret_to_public (x0, x1) {
  caml_failwith('EverCrypt_Ed25519_secret_to_public unimplemetned');
}

//Provides: _1_EverCrypt_Ed25519_sign
//Requires: caml_failwith
function _1_EverCrypt_Ed25519_sign (x0, x1, x2, x3) {
  caml_failwith('EverCrypt_Ed25519_sign unimplemetned');
}

//Provides: _5_EverCrypt_Ed25519_sign_expanded
//Requires: caml_failwith
function _5_EverCrypt_Ed25519_sign_expanded (x0, x1, x2, x3) {
  caml_failwith('EverCrypt_Ed25519_sign_expanded unimplemetned');
}

//Provides: _2_EverCrypt_Ed25519_verify
//Requires: caml_failwith
function _2_EverCrypt_Ed25519_verify (x0, x1, x2, x3) {
  caml_failwith('EverCrypt_Ed25519_verify unimplemetned');
}

//Provides: _13_EverCrypt_HKDF_expand
//Requires: caml_failwith
function _13_EverCrypt_HKDF_expand (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('EverCrypt_HKDF_expand unimplemetned');
}

//Provides: _13_EverCrypt_HKDF_expand_byte7
//Requires: _13_EverCrypt_HKDF_expand
function _13_EverCrypt_HKDF_expand_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _13_EverCrypt_HKDF_expand(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _11_EverCrypt_HKDF_expand_blake2b
//Requires: caml_failwith
function _11_EverCrypt_HKDF_expand_blake2b (x0, x1, x2, x3, x4, x5) {
  caml_failwith('EverCrypt_HKDF_expand_blake2b unimplemetned');
}

//Provides: _11_EverCrypt_HKDF_expand_blake2b_byte6
//Requires: _11_EverCrypt_HKDF_expand_blake2b
function _11_EverCrypt_HKDF_expand_blake2b_byte6 (x0, x1, x2, x3, x4, x5) {
  return _11_EverCrypt_HKDF_expand_blake2b(x0, x1, x2, x3, x4, x5);
}

//Provides: _9_EverCrypt_HKDF_expand_blake2s
//Requires: caml_failwith
function _9_EverCrypt_HKDF_expand_blake2s (x0, x1, x2, x3, x4, x5) {
  caml_failwith('EverCrypt_HKDF_expand_blake2s unimplemetned');
}

//Provides: _9_EverCrypt_HKDF_expand_blake2s_byte6
//Requires: _9_EverCrypt_HKDF_expand_blake2s
function _9_EverCrypt_HKDF_expand_blake2s_byte6 (x0, x1, x2, x3, x4, x5) {
  return _9_EverCrypt_HKDF_expand_blake2s(x0, x1, x2, x3, x4, x5);
}

//Provides: _1_EverCrypt_HKDF_expand_sha1
//Requires: caml_failwith
function _1_EverCrypt_HKDF_expand_sha1 (x0, x1, x2, x3, x4, x5) {
  caml_failwith('EverCrypt_HKDF_expand_sha1 unimplemetned');
}

//Provides: _1_EverCrypt_HKDF_expand_sha1_byte6
//Requires: _1_EverCrypt_HKDF_expand_sha1
function _1_EverCrypt_HKDF_expand_sha1_byte6 (x0, x1, x2, x3, x4, x5) {
  return _1_EverCrypt_HKDF_expand_sha1(x0, x1, x2, x3, x4, x5);
}

//Provides: _3_EverCrypt_HKDF_expand_sha2_256
//Requires: caml_failwith
function _3_EverCrypt_HKDF_expand_sha2_256 (x0, x1, x2, x3, x4, x5) {
  caml_failwith('EverCrypt_HKDF_expand_sha2_256 unimplemetned');
}

//Provides: _3_EverCrypt_HKDF_expand_sha2_256_byte6
//Requires: _3_EverCrypt_HKDF_expand_sha2_256
function _3_EverCrypt_HKDF_expand_sha2_256_byte6 (x0, x1, x2, x3, x4, x5) {
  return _3_EverCrypt_HKDF_expand_sha2_256(x0, x1, x2, x3, x4, x5);
}

//Provides: _5_EverCrypt_HKDF_expand_sha2_384
//Requires: caml_failwith
function _5_EverCrypt_HKDF_expand_sha2_384 (x0, x1, x2, x3, x4, x5) {
  caml_failwith('EverCrypt_HKDF_expand_sha2_384 unimplemetned');
}

//Provides: _5_EverCrypt_HKDF_expand_sha2_384_byte6
//Requires: _5_EverCrypt_HKDF_expand_sha2_384
function _5_EverCrypt_HKDF_expand_sha2_384_byte6 (x0, x1, x2, x3, x4, x5) {
  return _5_EverCrypt_HKDF_expand_sha2_384(x0, x1, x2, x3, x4, x5);
}

//Provides: _7_EverCrypt_HKDF_expand_sha2_512
//Requires: caml_failwith
function _7_EverCrypt_HKDF_expand_sha2_512 (x0, x1, x2, x3, x4, x5) {
  caml_failwith('EverCrypt_HKDF_expand_sha2_512 unimplemetned');
}

//Provides: _7_EverCrypt_HKDF_expand_sha2_512_byte6
//Requires: _7_EverCrypt_HKDF_expand_sha2_512
function _7_EverCrypt_HKDF_expand_sha2_512_byte6 (x0, x1, x2, x3, x4, x5) {
  return _7_EverCrypt_HKDF_expand_sha2_512(x0, x1, x2, x3, x4, x5);
}

//Provides: _14_EverCrypt_HKDF_extract
//Requires: caml_failwith
function _14_EverCrypt_HKDF_extract (x0, x1, x2, x3, x4, x5) {
  caml_failwith('EverCrypt_HKDF_extract unimplemetned');
}

//Provides: _14_EverCrypt_HKDF_extract_byte6
//Requires: _14_EverCrypt_HKDF_extract
function _14_EverCrypt_HKDF_extract_byte6 (x0, x1, x2, x3, x4, x5) {
  return _14_EverCrypt_HKDF_extract(x0, x1, x2, x3, x4, x5);
}

//Provides: _12_EverCrypt_HKDF_extract_blake2b
//Requires: caml_failwith
function _12_EverCrypt_HKDF_extract_blake2b (x0, x1, x2, x3, x4) {
  caml_failwith('EverCrypt_HKDF_extract_blake2b unimplemetned');
}

//Provides: _10_EverCrypt_HKDF_extract_blake2s
//Requires: caml_failwith
function _10_EverCrypt_HKDF_extract_blake2s (x0, x1, x2, x3, x4) {
  caml_failwith('EverCrypt_HKDF_extract_blake2s unimplemetned');
}

//Provides: _2_EverCrypt_HKDF_extract_sha1
//Requires: caml_failwith
function _2_EverCrypt_HKDF_extract_sha1 (x0, x1, x2, x3, x4) {
  caml_failwith('EverCrypt_HKDF_extract_sha1 unimplemetned');
}

//Provides: _4_EverCrypt_HKDF_extract_sha2_256
//Requires: caml_failwith
function _4_EverCrypt_HKDF_extract_sha2_256 (x0, x1, x2, x3, x4) {
  caml_failwith('EverCrypt_HKDF_extract_sha2_256 unimplemetned');
}

//Provides: _6_EverCrypt_HKDF_extract_sha2_384
//Requires: caml_failwith
function _6_EverCrypt_HKDF_extract_sha2_384 (x0, x1, x2, x3, x4) {
  caml_failwith('EverCrypt_HKDF_extract_sha2_384 unimplemetned');
}

//Provides: _8_EverCrypt_HKDF_extract_sha2_512
//Requires: caml_failwith
function _8_EverCrypt_HKDF_extract_sha2_512 (x0, x1, x2, x3, x4) {
  caml_failwith('EverCrypt_HKDF_extract_sha2_512 unimplemetned');
}

//Provides: _15_EverCrypt_HKDF_hkdf_expand
//Requires: caml_failwith
function _15_EverCrypt_HKDF_hkdf_expand (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('EverCrypt_HKDF_hkdf_expand unimplemetned');
}

//Provides: _15_EverCrypt_HKDF_hkdf_expand_byte7
//Requires: _15_EverCrypt_HKDF_hkdf_expand
function _15_EverCrypt_HKDF_hkdf_expand_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _15_EverCrypt_HKDF_hkdf_expand(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _16_EverCrypt_HKDF_hkdf_extract
//Requires: caml_failwith
function _16_EverCrypt_HKDF_hkdf_extract (x0, x1, x2, x3, x4, x5) {
  caml_failwith('EverCrypt_HKDF_hkdf_extract unimplemetned');
}

//Provides: _16_EverCrypt_HKDF_hkdf_extract_byte6
//Requires: _16_EverCrypt_HKDF_hkdf_extract
function _16_EverCrypt_HKDF_hkdf_extract_byte6 (x0, x1, x2, x3, x4, x5) {
  return _16_EverCrypt_HKDF_hkdf_extract(x0, x1, x2, x3, x4, x5);
}

//Provides: _6_EverCrypt_HMAC_compute_blake2b
//Requires: caml_failwith
function _6_EverCrypt_HMAC_compute_blake2b (x0, x1, x2, x3, x4) {
  caml_failwith('EverCrypt_HMAC_compute_blake2b unimplemetned');
}

//Provides: _5_EverCrypt_HMAC_compute_blake2s
//Requires: caml_failwith
function _5_EverCrypt_HMAC_compute_blake2s (x0, x1, x2, x3, x4) {
  caml_failwith('EverCrypt_HMAC_compute_blake2s unimplemetned');
}

//Provides: _1_EverCrypt_HMAC_compute_sha1
//Requires: caml_failwith
function _1_EverCrypt_HMAC_compute_sha1 (x0, x1, x2, x3, x4) {
  caml_failwith('EverCrypt_HMAC_compute_sha1 unimplemetned');
}

//Provides: _2_EverCrypt_HMAC_compute_sha2_256
//Requires: caml_failwith
function _2_EverCrypt_HMAC_compute_sha2_256 (x0, x1, x2, x3, x4) {
  caml_failwith('EverCrypt_HMAC_compute_sha2_256 unimplemetned');
}

//Provides: _3_EverCrypt_HMAC_compute_sha2_384
//Requires: caml_failwith
function _3_EverCrypt_HMAC_compute_sha2_384 (x0, x1, x2, x3, x4) {
  caml_failwith('EverCrypt_HMAC_compute_sha2_384 unimplemetned');
}

//Provides: _4_EverCrypt_HMAC_compute_sha2_512
//Requires: caml_failwith
function _4_EverCrypt_HMAC_compute_sha2_512 (x0, x1, x2, x3, x4) {
  caml_failwith('EverCrypt_HMAC_compute_sha2_512 unimplemetned');
}

//Provides: _7_EverCrypt_HMAC_is_supported_alg
//Requires: caml_failwith
function _7_EverCrypt_HMAC_is_supported_alg (x0) {
  caml_failwith('EverCrypt_HMAC_is_supported_alg unimplemetned');
}

//Provides: _32_EverCrypt_Hash_Incremental_alg_of_state
//Requires: caml_failwith
function _32_EverCrypt_Hash_Incremental_alg_of_state (x0) {
  caml_failwith('EverCrypt_Hash_Incremental_alg_of_state unimplemetned');
}

//Provides: _20_EverCrypt_Hash_Incremental_block_len
//Requires: caml_failwith
function _20_EverCrypt_Hash_Incremental_block_len (x0) {
  caml_failwith('EverCrypt_Hash_Incremental_block_len unimplemetned');
}

//Provides: _21_EverCrypt_Hash_Incremental_create_in
//Requires: caml_failwith
function _21_EverCrypt_Hash_Incremental_create_in (x0) {
  caml_failwith('EverCrypt_Hash_Incremental_create_in unimplemetned');
}

//Provides: _33_EverCrypt_Hash_Incremental_finish
//Requires: caml_failwith
function _33_EverCrypt_Hash_Incremental_finish (x0, x1) {
  caml_failwith('EverCrypt_Hash_Incremental_finish unimplemetned');
}

//Provides: _31_EverCrypt_Hash_Incremental_finish_blake2b
//Requires: caml_failwith
function _31_EverCrypt_Hash_Incremental_finish_blake2b (x0, x1) {
  caml_failwith('EverCrypt_Hash_Incremental_finish_blake2b unimplemetned');
}

//Provides: _30_EverCrypt_Hash_Incremental_finish_blake2s
//Requires: caml_failwith
function _30_EverCrypt_Hash_Incremental_finish_blake2s (x0, x1) {
  caml_failwith('EverCrypt_Hash_Incremental_finish_blake2s unimplemetned');
}

//Provides: _24_EverCrypt_Hash_Incremental_finish_md5
//Requires: caml_failwith
function _24_EverCrypt_Hash_Incremental_finish_md5 (x0, x1) {
  caml_failwith('EverCrypt_Hash_Incremental_finish_md5 unimplemetned');
}

//Provides: _25_EverCrypt_Hash_Incremental_finish_sha1
//Requires: caml_failwith
function _25_EverCrypt_Hash_Incremental_finish_sha1 (x0, x1) {
  caml_failwith('EverCrypt_Hash_Incremental_finish_sha1 unimplemetned');
}

//Provides: _26_EverCrypt_Hash_Incremental_finish_sha224
//Requires: caml_failwith
function _26_EverCrypt_Hash_Incremental_finish_sha224 (x0, x1) {
  caml_failwith('EverCrypt_Hash_Incremental_finish_sha224 unimplemetned');
}

//Provides: _27_EverCrypt_Hash_Incremental_finish_sha256
//Requires: caml_failwith
function _27_EverCrypt_Hash_Incremental_finish_sha256 (x0, x1) {
  caml_failwith('EverCrypt_Hash_Incremental_finish_sha256 unimplemetned');
}

//Provides: _28_EverCrypt_Hash_Incremental_finish_sha384
//Requires: caml_failwith
function _28_EverCrypt_Hash_Incremental_finish_sha384 (x0, x1) {
  caml_failwith('EverCrypt_Hash_Incremental_finish_sha384 unimplemetned');
}

//Provides: _29_EverCrypt_Hash_Incremental_finish_sha512
//Requires: caml_failwith
function _29_EverCrypt_Hash_Incremental_finish_sha512 (x0, x1) {
  caml_failwith('EverCrypt_Hash_Incremental_finish_sha512 unimplemetned');
}

//Provides: _34_EverCrypt_Hash_Incremental_free
//Requires: caml_failwith
function _34_EverCrypt_Hash_Incremental_free (x0) {
  caml_failwith('EverCrypt_Hash_Incremental_free unimplemetned');
}

//Provides: _19_EverCrypt_Hash_Incremental_hash_len
//Requires: caml_failwith
function _19_EverCrypt_Hash_Incremental_hash_len (x0) {
  caml_failwith('EverCrypt_Hash_Incremental_hash_len unimplemetned');
}

//Provides: _22_EverCrypt_Hash_Incremental_init
//Requires: caml_failwith
function _22_EverCrypt_Hash_Incremental_init (x0) {
  caml_failwith('EverCrypt_Hash_Incremental_init unimplemetned');
}

//Provides: _23_EverCrypt_Hash_Incremental_update
//Requires: caml_failwith
function _23_EverCrypt_Hash_Incremental_update (x0, x1, x2) {
  caml_failwith('EverCrypt_Hash_Incremental_update unimplemetned');
}

//Provides: _1_EverCrypt_Hash_alg_of_state
//Requires: caml_failwith
function _1_EverCrypt_Hash_alg_of_state (x0) {
  caml_failwith('EverCrypt_Hash_alg_of_state unimplemetned');
}

//Provides: _15_EverCrypt_Hash_copy
//Requires: caml_failwith
function _15_EverCrypt_Hash_copy (x0, x1) {
  caml_failwith('EverCrypt_Hash_copy unimplemetned');
}

//Provides: _3_EverCrypt_Hash_create
//Requires: caml_failwith
function _3_EverCrypt_Hash_create (x0) {
  caml_failwith('EverCrypt_Hash_create unimplemetned');
}

//Provides: _2_EverCrypt_Hash_create_in
//Requires: caml_failwith
function _2_EverCrypt_Hash_create_in (x0) {
  caml_failwith('EverCrypt_Hash_create_in unimplemetned');
}

//Provides: _13_EverCrypt_Hash_finish
//Requires: caml_failwith
function _13_EverCrypt_Hash_finish (x0, x1) {
  caml_failwith('EverCrypt_Hash_finish unimplemetned');
}

//Provides: _14_EverCrypt_Hash_free
//Requires: caml_failwith
function _14_EverCrypt_Hash_free (x0) {
  caml_failwith('EverCrypt_Hash_free unimplemetned');
}

//Provides: _17_EverCrypt_Hash_hash_224
//Requires: caml_failwith
function _17_EverCrypt_Hash_hash_224 (x0, x1, x2) {
  caml_failwith('EverCrypt_Hash_hash_224 unimplemetned');
}

//Provides: _16_EverCrypt_Hash_hash_256
//Requires: caml_failwith
function _16_EverCrypt_Hash_hash_256 (x0, x1, x2) {
  caml_failwith('EverCrypt_Hash_hash_256 unimplemetned');
}

//Provides: _4_EverCrypt_Hash_init
//Requires: caml_failwith
function _4_EverCrypt_Hash_init (x0) {
  caml_failwith('EverCrypt_Hash_init unimplemetned');
}

//Provides: _7_EverCrypt_Hash_update
//Requires: caml_failwith
function _7_EverCrypt_Hash_update (x0, x1) {
  caml_failwith('EverCrypt_Hash_update unimplemetned');
}

//Provides: _6_EverCrypt_Hash_update2
//Requires: caml_failwith
function _6_EverCrypt_Hash_update2 (x0, x1, x2) {
  caml_failwith('EverCrypt_Hash_update2 unimplemetned');
}

//Provides: _12_EverCrypt_Hash_update_last
//Requires: caml_failwith
function _12_EverCrypt_Hash_update_last (x0, x1, x2) {
  caml_failwith('EverCrypt_Hash_update_last unimplemetned');
}

//Provides: _11_EverCrypt_Hash_update_last2
//Requires: caml_failwith
function _11_EverCrypt_Hash_update_last2 (x0, x1, x2, x3) {
  caml_failwith('EverCrypt_Hash_update_last2 unimplemetned');
}

//Provides: _10_EverCrypt_Hash_update_last_256
//Requires: caml_failwith
function _10_EverCrypt_Hash_update_last_256 (x0, x1, x2, x3) {
  caml_failwith('EverCrypt_Hash_update_last_256 unimplemetned');
}

//Provides: _9_EverCrypt_Hash_update_multi
//Requires: caml_failwith
function _9_EverCrypt_Hash_update_multi (x0, x1, x2) {
  caml_failwith('EverCrypt_Hash_update_multi unimplemetned');
}

//Provides: _8_EverCrypt_Hash_update_multi2
//Requires: caml_failwith
function _8_EverCrypt_Hash_update_multi2 (x0, x1, x2, x3) {
  caml_failwith('EverCrypt_Hash_update_multi2 unimplemetned');
}

//Provides: _5_EverCrypt_Hash_update_multi_256
//Requires: caml_failwith
function _5_EverCrypt_Hash_update_multi_256 (x0, x1, x2) {
  caml_failwith('EverCrypt_Hash_update_multi_256 unimplemetned');
}

//Provides: _1_EverCrypt_Poly1305_poly1305
//Requires: caml_failwith
function _1_EverCrypt_Poly1305_poly1305 (x0, x1, x2, x3) {
  caml_failwith('EverCrypt_Poly1305_poly1305 unimplemetned');
}

//Provides: _2_Hacl_Bignum25519_inverse
//Requires: caml_failwith
function _2_Hacl_Bignum25519_inverse (x0, x1) {
  caml_failwith('Hacl_Bignum25519_inverse unimplemetned');
}

//Provides: _3_Hacl_Bignum25519_load_51
//Requires: caml_failwith
function _3_Hacl_Bignum25519_load_51 (x0, x1) {
  caml_failwith('Hacl_Bignum25519_load_51 unimplemetned');
}

//Provides: _1_Hacl_Bignum25519_reduce_513
//Requires: caml_failwith
function _1_Hacl_Bignum25519_reduce_513 (x0) {
  caml_failwith('Hacl_Bignum25519_reduce_513 unimplemetned');
}

//Provides: _4_Hacl_Bignum25519_store_51
//Requires: caml_failwith
function _4_Hacl_Bignum25519_store_51 (x0, x1) {
  caml_failwith('Hacl_Bignum25519_store_51 unimplemetned');
}

//Provides: _1_Hacl_Bignum256_32_add
//Requires: caml_failwith
function _1_Hacl_Bignum256_32_add (x0, x1, x2) {
  caml_failwith('Hacl_Bignum256_32_add unimplemetned');
}

//Provides: _3_Hacl_Bignum256_32_add_mod
//Requires: caml_failwith
function _3_Hacl_Bignum256_32_add_mod (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum256_32_add_mod unimplemetned');
}

//Provides: _19_Hacl_Bignum256_32_bn_to_bytes_be
//Requires: caml_failwith
function _19_Hacl_Bignum256_32_bn_to_bytes_be (x0, x1) {
  caml_failwith('Hacl_Bignum256_32_bn_to_bytes_be unimplemetned');
}

//Provides: _20_Hacl_Bignum256_32_bn_to_bytes_le
//Requires: caml_failwith
function _20_Hacl_Bignum256_32_bn_to_bytes_le (x0, x1) {
  caml_failwith('Hacl_Bignum256_32_bn_to_bytes_le unimplemetned');
}

//Provides: _22_Hacl_Bignum256_32_eq_mask
//Requires: caml_failwith
function _22_Hacl_Bignum256_32_eq_mask (x0, x1) {
  caml_failwith('Hacl_Bignum256_32_eq_mask unimplemetned');
}

//Provides: _21_Hacl_Bignum256_32_lt_mask
//Requires: caml_failwith
function _21_Hacl_Bignum256_32_lt_mask (x0, x1) {
  caml_failwith('Hacl_Bignum256_32_lt_mask unimplemetned');
}

//Provides: _7_Hacl_Bignum256_32_mod
//Requires: caml_failwith
function _7_Hacl_Bignum256_32_mod (x0, x1, x2) {
  caml_failwith('Hacl_Bignum256_32_mod unimplemetned');
}

//Provides: _9_Hacl_Bignum256_32_mod_exp_consttime
//Requires: caml_failwith
function _9_Hacl_Bignum256_32_mod_exp_consttime (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum256_32_mod_exp_consttime unimplemetned');
}

//Provides: _15_Hacl_Bignum256_32_mod_exp_consttime_precomp
//Requires: caml_failwith
function _15_Hacl_Bignum256_32_mod_exp_consttime_precomp (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum256_32_mod_exp_consttime_precomp unimplemetned');
}

//Provides: _8_Hacl_Bignum256_32_mod_exp_vartime
//Requires: caml_failwith
function _8_Hacl_Bignum256_32_mod_exp_vartime (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum256_32_mod_exp_vartime unimplemetned');
}

//Provides: _14_Hacl_Bignum256_32_mod_exp_vartime_precomp
//Requires: caml_failwith
function _14_Hacl_Bignum256_32_mod_exp_vartime_precomp (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum256_32_mod_exp_vartime_precomp unimplemetned');
}

//Provides: _10_Hacl_Bignum256_32_mod_inv_prime_vartime
//Requires: caml_failwith
function _10_Hacl_Bignum256_32_mod_inv_prime_vartime (x0, x1, x2) {
  caml_failwith('Hacl_Bignum256_32_mod_inv_prime_vartime unimplemetned');
}

//Provides: _16_Hacl_Bignum256_32_mod_inv_prime_vartime_precomp
//Requires: caml_failwith
function _16_Hacl_Bignum256_32_mod_inv_prime_vartime_precomp (x0, x1, x2) {
  caml_failwith('Hacl_Bignum256_32_mod_inv_prime_vartime_precomp unimplemetned');
}

//Provides: _13_Hacl_Bignum256_32_mod_precomp
//Requires: caml_failwith
function _13_Hacl_Bignum256_32_mod_precomp (x0, x1, x2) {
  caml_failwith('Hacl_Bignum256_32_mod_precomp unimplemetned');
}

//Provides: _12_Hacl_Bignum256_32_mont_ctx_free
//Requires: caml_failwith
function _12_Hacl_Bignum256_32_mont_ctx_free (x0) {
  caml_failwith('Hacl_Bignum256_32_mont_ctx_free unimplemetned');
}

//Provides: _11_Hacl_Bignum256_32_mont_ctx_init
//Requires: caml_failwith
function _11_Hacl_Bignum256_32_mont_ctx_init (x0) {
  caml_failwith('Hacl_Bignum256_32_mont_ctx_init unimplemetned');
}

//Provides: _5_Hacl_Bignum256_32_mul
//Requires: caml_failwith
function _5_Hacl_Bignum256_32_mul (x0, x1, x2) {
  caml_failwith('Hacl_Bignum256_32_mul unimplemetned');
}

//Provides: _17_Hacl_Bignum256_32_new_bn_from_bytes_be
//Requires: caml_failwith
function _17_Hacl_Bignum256_32_new_bn_from_bytes_be (x0, x1) {
  caml_failwith('Hacl_Bignum256_32_new_bn_from_bytes_be unimplemetned');
}

//Provides: _18_Hacl_Bignum256_32_new_bn_from_bytes_le
//Requires: caml_failwith
function _18_Hacl_Bignum256_32_new_bn_from_bytes_le (x0, x1) {
  caml_failwith('Hacl_Bignum256_32_new_bn_from_bytes_le unimplemetned');
}

//Provides: _6_Hacl_Bignum256_32_sqr
//Requires: caml_failwith
function _6_Hacl_Bignum256_32_sqr (x0, x1) {
  caml_failwith('Hacl_Bignum256_32_sqr unimplemetned');
}

//Provides: _2_Hacl_Bignum256_32_sub
//Requires: caml_failwith
function _2_Hacl_Bignum256_32_sub (x0, x1, x2) {
  caml_failwith('Hacl_Bignum256_32_sub unimplemetned');
}

//Provides: _4_Hacl_Bignum256_32_sub_mod
//Requires: caml_failwith
function _4_Hacl_Bignum256_32_sub_mod (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum256_32_sub_mod unimplemetned');
}

//Provides: _1_Hacl_Bignum256_add
//Requires: caml_failwith
function _1_Hacl_Bignum256_add (x0, x1, x2) {
  caml_failwith('Hacl_Bignum256_add unimplemetned');
}

//Provides: _3_Hacl_Bignum256_add_mod
//Requires: caml_failwith
function _3_Hacl_Bignum256_add_mod (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum256_add_mod unimplemetned');
}

//Provides: _19_Hacl_Bignum256_bn_to_bytes_be
//Requires: caml_failwith
function _19_Hacl_Bignum256_bn_to_bytes_be (x0, x1) {
  caml_failwith('Hacl_Bignum256_bn_to_bytes_be unimplemetned');
}

//Provides: _20_Hacl_Bignum256_bn_to_bytes_le
//Requires: caml_failwith
function _20_Hacl_Bignum256_bn_to_bytes_le (x0, x1) {
  caml_failwith('Hacl_Bignum256_bn_to_bytes_le unimplemetned');
}

//Provides: _22_Hacl_Bignum256_eq_mask
//Requires: caml_failwith
function _22_Hacl_Bignum256_eq_mask (x0, x1) {
  caml_failwith('Hacl_Bignum256_eq_mask unimplemetned');
}

//Provides: _21_Hacl_Bignum256_lt_mask
//Requires: caml_failwith
function _21_Hacl_Bignum256_lt_mask (x0, x1) {
  caml_failwith('Hacl_Bignum256_lt_mask unimplemetned');
}

//Provides: _7_Hacl_Bignum256_mod
//Requires: caml_failwith
function _7_Hacl_Bignum256_mod (x0, x1, x2) {
  caml_failwith('Hacl_Bignum256_mod unimplemetned');
}

//Provides: _9_Hacl_Bignum256_mod_exp_consttime
//Requires: caml_failwith
function _9_Hacl_Bignum256_mod_exp_consttime (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum256_mod_exp_consttime unimplemetned');
}

//Provides: _15_Hacl_Bignum256_mod_exp_consttime_precomp
//Requires: caml_failwith
function _15_Hacl_Bignum256_mod_exp_consttime_precomp (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum256_mod_exp_consttime_precomp unimplemetned');
}

//Provides: _8_Hacl_Bignum256_mod_exp_vartime
//Requires: caml_failwith
function _8_Hacl_Bignum256_mod_exp_vartime (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum256_mod_exp_vartime unimplemetned');
}

//Provides: _14_Hacl_Bignum256_mod_exp_vartime_precomp
//Requires: caml_failwith
function _14_Hacl_Bignum256_mod_exp_vartime_precomp (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum256_mod_exp_vartime_precomp unimplemetned');
}

//Provides: _10_Hacl_Bignum256_mod_inv_prime_vartime
//Requires: caml_failwith
function _10_Hacl_Bignum256_mod_inv_prime_vartime (x0, x1, x2) {
  caml_failwith('Hacl_Bignum256_mod_inv_prime_vartime unimplemetned');
}

//Provides: _16_Hacl_Bignum256_mod_inv_prime_vartime_precomp
//Requires: caml_failwith
function _16_Hacl_Bignum256_mod_inv_prime_vartime_precomp (x0, x1, x2) {
  caml_failwith('Hacl_Bignum256_mod_inv_prime_vartime_precomp unimplemetned');
}

//Provides: _13_Hacl_Bignum256_mod_precomp
//Requires: caml_failwith
function _13_Hacl_Bignum256_mod_precomp (x0, x1, x2) {
  caml_failwith('Hacl_Bignum256_mod_precomp unimplemetned');
}

//Provides: _12_Hacl_Bignum256_mont_ctx_free
//Requires: caml_failwith
function _12_Hacl_Bignum256_mont_ctx_free (x0) {
  caml_failwith('Hacl_Bignum256_mont_ctx_free unimplemetned');
}

//Provides: _11_Hacl_Bignum256_mont_ctx_init
//Requires: caml_failwith
function _11_Hacl_Bignum256_mont_ctx_init (x0) {
  caml_failwith('Hacl_Bignum256_mont_ctx_init unimplemetned');
}

//Provides: _5_Hacl_Bignum256_mul
//Requires: caml_failwith
function _5_Hacl_Bignum256_mul (x0, x1, x2) {
  caml_failwith('Hacl_Bignum256_mul unimplemetned');
}

//Provides: _17_Hacl_Bignum256_new_bn_from_bytes_be
//Requires: caml_failwith
function _17_Hacl_Bignum256_new_bn_from_bytes_be (x0, x1) {
  caml_failwith('Hacl_Bignum256_new_bn_from_bytes_be unimplemetned');
}

//Provides: _18_Hacl_Bignum256_new_bn_from_bytes_le
//Requires: caml_failwith
function _18_Hacl_Bignum256_new_bn_from_bytes_le (x0, x1) {
  caml_failwith('Hacl_Bignum256_new_bn_from_bytes_le unimplemetned');
}

//Provides: _6_Hacl_Bignum256_sqr
//Requires: caml_failwith
function _6_Hacl_Bignum256_sqr (x0, x1) {
  caml_failwith('Hacl_Bignum256_sqr unimplemetned');
}

//Provides: _2_Hacl_Bignum256_sub
//Requires: caml_failwith
function _2_Hacl_Bignum256_sub (x0, x1, x2) {
  caml_failwith('Hacl_Bignum256_sub unimplemetned');
}

//Provides: _4_Hacl_Bignum256_sub_mod
//Requires: caml_failwith
function _4_Hacl_Bignum256_sub_mod (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum256_sub_mod unimplemetned');
}

//Provides: _1_Hacl_Bignum32_add
//Requires: caml_failwith
function _1_Hacl_Bignum32_add (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum32_add unimplemetned');
}

//Provides: _3_Hacl_Bignum32_add_mod
//Requires: caml_failwith
function _3_Hacl_Bignum32_add_mod (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum32_add_mod unimplemetned');
}

//Provides: _19_Hacl_Bignum32_bn_to_bytes_be
//Requires: caml_failwith
function _19_Hacl_Bignum32_bn_to_bytes_be (x0, x1, x2) {
  caml_failwith('Hacl_Bignum32_bn_to_bytes_be unimplemetned');
}

//Provides: _20_Hacl_Bignum32_bn_to_bytes_le
//Requires: caml_failwith
function _20_Hacl_Bignum32_bn_to_bytes_le (x0, x1, x2) {
  caml_failwith('Hacl_Bignum32_bn_to_bytes_le unimplemetned');
}

//Provides: _22_Hacl_Bignum32_eq_mask
//Requires: caml_failwith
function _22_Hacl_Bignum32_eq_mask (x0, x1, x2) {
  caml_failwith('Hacl_Bignum32_eq_mask unimplemetned');
}

//Provides: _21_Hacl_Bignum32_lt_mask
//Requires: caml_failwith
function _21_Hacl_Bignum32_lt_mask (x0, x1, x2) {
  caml_failwith('Hacl_Bignum32_lt_mask unimplemetned');
}

//Provides: _7_Hacl_Bignum32_mod
//Requires: caml_failwith
function _7_Hacl_Bignum32_mod (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum32_mod unimplemetned');
}

//Provides: _9_Hacl_Bignum32_mod_exp_consttime
//Requires: caml_failwith
function _9_Hacl_Bignum32_mod_exp_consttime (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Bignum32_mod_exp_consttime unimplemetned');
}

//Provides: _9_Hacl_Bignum32_mod_exp_consttime_byte6
//Requires: _9_Hacl_Bignum32_mod_exp_consttime
function _9_Hacl_Bignum32_mod_exp_consttime_byte6 (x0, x1, x2, x3, x4, x5) {
  return _9_Hacl_Bignum32_mod_exp_consttime(x0, x1, x2, x3, x4, x5);
}

//Provides: _15_Hacl_Bignum32_mod_exp_consttime_precomp
//Requires: caml_failwith
function _15_Hacl_Bignum32_mod_exp_consttime_precomp (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum32_mod_exp_consttime_precomp unimplemetned');
}

//Provides: _8_Hacl_Bignum32_mod_exp_vartime
//Requires: caml_failwith
function _8_Hacl_Bignum32_mod_exp_vartime (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Bignum32_mod_exp_vartime unimplemetned');
}

//Provides: _8_Hacl_Bignum32_mod_exp_vartime_byte6
//Requires: _8_Hacl_Bignum32_mod_exp_vartime
function _8_Hacl_Bignum32_mod_exp_vartime_byte6 (x0, x1, x2, x3, x4, x5) {
  return _8_Hacl_Bignum32_mod_exp_vartime(x0, x1, x2, x3, x4, x5);
}

//Provides: _14_Hacl_Bignum32_mod_exp_vartime_precomp
//Requires: caml_failwith
function _14_Hacl_Bignum32_mod_exp_vartime_precomp (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum32_mod_exp_vartime_precomp unimplemetned');
}

//Provides: _10_Hacl_Bignum32_mod_inv_prime_vartime
//Requires: caml_failwith
function _10_Hacl_Bignum32_mod_inv_prime_vartime (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum32_mod_inv_prime_vartime unimplemetned');
}

//Provides: _16_Hacl_Bignum32_mod_inv_prime_vartime_precomp
//Requires: caml_failwith
function _16_Hacl_Bignum32_mod_inv_prime_vartime_precomp (x0, x1, x2) {
  caml_failwith('Hacl_Bignum32_mod_inv_prime_vartime_precomp unimplemetned');
}

//Provides: _13_Hacl_Bignum32_mod_precomp
//Requires: caml_failwith
function _13_Hacl_Bignum32_mod_precomp (x0, x1, x2) {
  caml_failwith('Hacl_Bignum32_mod_precomp unimplemetned');
}

//Provides: _12_Hacl_Bignum32_mont_ctx_free
//Requires: caml_failwith
function _12_Hacl_Bignum32_mont_ctx_free (x0) {
  caml_failwith('Hacl_Bignum32_mont_ctx_free unimplemetned');
}

//Provides: _11_Hacl_Bignum32_mont_ctx_init
//Requires: caml_failwith
function _11_Hacl_Bignum32_mont_ctx_init (x0, x1) {
  caml_failwith('Hacl_Bignum32_mont_ctx_init unimplemetned');
}

//Provides: _5_Hacl_Bignum32_mul
//Requires: caml_failwith
function _5_Hacl_Bignum32_mul (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum32_mul unimplemetned');
}

//Provides: _17_Hacl_Bignum32_new_bn_from_bytes_be
//Requires: caml_failwith
function _17_Hacl_Bignum32_new_bn_from_bytes_be (x0, x1) {
  caml_failwith('Hacl_Bignum32_new_bn_from_bytes_be unimplemetned');
}

//Provides: _18_Hacl_Bignum32_new_bn_from_bytes_le
//Requires: caml_failwith
function _18_Hacl_Bignum32_new_bn_from_bytes_le (x0, x1) {
  caml_failwith('Hacl_Bignum32_new_bn_from_bytes_le unimplemetned');
}

//Provides: _6_Hacl_Bignum32_sqr
//Requires: caml_failwith
function _6_Hacl_Bignum32_sqr (x0, x1, x2) {
  caml_failwith('Hacl_Bignum32_sqr unimplemetned');
}

//Provides: _2_Hacl_Bignum32_sub
//Requires: caml_failwith
function _2_Hacl_Bignum32_sub (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum32_sub unimplemetned');
}

//Provides: _4_Hacl_Bignum32_sub_mod
//Requires: caml_failwith
function _4_Hacl_Bignum32_sub_mod (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum32_sub_mod unimplemetned');
}

//Provides: _1_Hacl_Bignum4096_32_add
//Requires: caml_failwith
function _1_Hacl_Bignum4096_32_add (x0, x1, x2) {
  caml_failwith('Hacl_Bignum4096_32_add unimplemetned');
}

//Provides: _3_Hacl_Bignum4096_32_add_mod
//Requires: caml_failwith
function _3_Hacl_Bignum4096_32_add_mod (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum4096_32_add_mod unimplemetned');
}

//Provides: _19_Hacl_Bignum4096_32_bn_to_bytes_be
//Requires: caml_failwith
function _19_Hacl_Bignum4096_32_bn_to_bytes_be (x0, x1) {
  caml_failwith('Hacl_Bignum4096_32_bn_to_bytes_be unimplemetned');
}

//Provides: _20_Hacl_Bignum4096_32_bn_to_bytes_le
//Requires: caml_failwith
function _20_Hacl_Bignum4096_32_bn_to_bytes_le (x0, x1) {
  caml_failwith('Hacl_Bignum4096_32_bn_to_bytes_le unimplemetned');
}

//Provides: _22_Hacl_Bignum4096_32_eq_mask
//Requires: caml_failwith
function _22_Hacl_Bignum4096_32_eq_mask (x0, x1) {
  caml_failwith('Hacl_Bignum4096_32_eq_mask unimplemetned');
}

//Provides: _21_Hacl_Bignum4096_32_lt_mask
//Requires: caml_failwith
function _21_Hacl_Bignum4096_32_lt_mask (x0, x1) {
  caml_failwith('Hacl_Bignum4096_32_lt_mask unimplemetned');
}

//Provides: _7_Hacl_Bignum4096_32_mod
//Requires: caml_failwith
function _7_Hacl_Bignum4096_32_mod (x0, x1, x2) {
  caml_failwith('Hacl_Bignum4096_32_mod unimplemetned');
}

//Provides: _9_Hacl_Bignum4096_32_mod_exp_consttime
//Requires: caml_failwith
function _9_Hacl_Bignum4096_32_mod_exp_consttime (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum4096_32_mod_exp_consttime unimplemetned');
}

//Provides: _15_Hacl_Bignum4096_32_mod_exp_consttime_precomp
//Requires: caml_failwith
function _15_Hacl_Bignum4096_32_mod_exp_consttime_precomp (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum4096_32_mod_exp_consttime_precomp unimplemetned');
}

//Provides: _8_Hacl_Bignum4096_32_mod_exp_vartime
//Requires: caml_failwith
function _8_Hacl_Bignum4096_32_mod_exp_vartime (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum4096_32_mod_exp_vartime unimplemetned');
}

//Provides: _14_Hacl_Bignum4096_32_mod_exp_vartime_precomp
//Requires: caml_failwith
function _14_Hacl_Bignum4096_32_mod_exp_vartime_precomp (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum4096_32_mod_exp_vartime_precomp unimplemetned');
}

//Provides: _10_Hacl_Bignum4096_32_mod_inv_prime_vartime
//Requires: caml_failwith
function _10_Hacl_Bignum4096_32_mod_inv_prime_vartime (x0, x1, x2) {
  caml_failwith('Hacl_Bignum4096_32_mod_inv_prime_vartime unimplemetned');
}

//Provides: _16_Hacl_Bignum4096_32_mod_inv_prime_vartime_precomp
//Requires: caml_failwith
function _16_Hacl_Bignum4096_32_mod_inv_prime_vartime_precomp (x0, x1, x2) {
  caml_failwith('Hacl_Bignum4096_32_mod_inv_prime_vartime_precomp unimplemetned');
}

//Provides: _13_Hacl_Bignum4096_32_mod_precomp
//Requires: caml_failwith
function _13_Hacl_Bignum4096_32_mod_precomp (x0, x1, x2) {
  caml_failwith('Hacl_Bignum4096_32_mod_precomp unimplemetned');
}

//Provides: _12_Hacl_Bignum4096_32_mont_ctx_free
//Requires: caml_failwith
function _12_Hacl_Bignum4096_32_mont_ctx_free (x0) {
  caml_failwith('Hacl_Bignum4096_32_mont_ctx_free unimplemetned');
}

//Provides: _11_Hacl_Bignum4096_32_mont_ctx_init
//Requires: caml_failwith
function _11_Hacl_Bignum4096_32_mont_ctx_init (x0) {
  caml_failwith('Hacl_Bignum4096_32_mont_ctx_init unimplemetned');
}

//Provides: _5_Hacl_Bignum4096_32_mul
//Requires: caml_failwith
function _5_Hacl_Bignum4096_32_mul (x0, x1, x2) {
  caml_failwith('Hacl_Bignum4096_32_mul unimplemetned');
}

//Provides: _17_Hacl_Bignum4096_32_new_bn_from_bytes_be
//Requires: caml_failwith
function _17_Hacl_Bignum4096_32_new_bn_from_bytes_be (x0, x1) {
  caml_failwith('Hacl_Bignum4096_32_new_bn_from_bytes_be unimplemetned');
}

//Provides: _18_Hacl_Bignum4096_32_new_bn_from_bytes_le
//Requires: caml_failwith
function _18_Hacl_Bignum4096_32_new_bn_from_bytes_le (x0, x1) {
  caml_failwith('Hacl_Bignum4096_32_new_bn_from_bytes_le unimplemetned');
}

//Provides: _6_Hacl_Bignum4096_32_sqr
//Requires: caml_failwith
function _6_Hacl_Bignum4096_32_sqr (x0, x1) {
  caml_failwith('Hacl_Bignum4096_32_sqr unimplemetned');
}

//Provides: _2_Hacl_Bignum4096_32_sub
//Requires: caml_failwith
function _2_Hacl_Bignum4096_32_sub (x0, x1, x2) {
  caml_failwith('Hacl_Bignum4096_32_sub unimplemetned');
}

//Provides: _4_Hacl_Bignum4096_32_sub_mod
//Requires: caml_failwith
function _4_Hacl_Bignum4096_32_sub_mod (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum4096_32_sub_mod unimplemetned');
}

//Provides: _1_Hacl_Bignum4096_add
//Requires: caml_failwith
function _1_Hacl_Bignum4096_add (x0, x1, x2) {
  caml_failwith('Hacl_Bignum4096_add unimplemetned');
}

//Provides: _3_Hacl_Bignum4096_add_mod
//Requires: caml_failwith
function _3_Hacl_Bignum4096_add_mod (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum4096_add_mod unimplemetned');
}

//Provides: _19_Hacl_Bignum4096_bn_to_bytes_be
//Requires: caml_failwith
function _19_Hacl_Bignum4096_bn_to_bytes_be (x0, x1) {
  caml_failwith('Hacl_Bignum4096_bn_to_bytes_be unimplemetned');
}

//Provides: _20_Hacl_Bignum4096_bn_to_bytes_le
//Requires: caml_failwith
function _20_Hacl_Bignum4096_bn_to_bytes_le (x0, x1) {
  caml_failwith('Hacl_Bignum4096_bn_to_bytes_le unimplemetned');
}

//Provides: _22_Hacl_Bignum4096_eq_mask
//Requires: caml_failwith
function _22_Hacl_Bignum4096_eq_mask (x0, x1) {
  caml_failwith('Hacl_Bignum4096_eq_mask unimplemetned');
}

//Provides: _21_Hacl_Bignum4096_lt_mask
//Requires: caml_failwith
function _21_Hacl_Bignum4096_lt_mask (x0, x1) {
  caml_failwith('Hacl_Bignum4096_lt_mask unimplemetned');
}

//Provides: _7_Hacl_Bignum4096_mod
//Requires: caml_failwith
function _7_Hacl_Bignum4096_mod (x0, x1, x2) {
  caml_failwith('Hacl_Bignum4096_mod unimplemetned');
}

//Provides: _9_Hacl_Bignum4096_mod_exp_consttime
//Requires: caml_failwith
function _9_Hacl_Bignum4096_mod_exp_consttime (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum4096_mod_exp_consttime unimplemetned');
}

//Provides: _15_Hacl_Bignum4096_mod_exp_consttime_precomp
//Requires: caml_failwith
function _15_Hacl_Bignum4096_mod_exp_consttime_precomp (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum4096_mod_exp_consttime_precomp unimplemetned');
}

//Provides: _8_Hacl_Bignum4096_mod_exp_vartime
//Requires: caml_failwith
function _8_Hacl_Bignum4096_mod_exp_vartime (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum4096_mod_exp_vartime unimplemetned');
}

//Provides: _14_Hacl_Bignum4096_mod_exp_vartime_precomp
//Requires: caml_failwith
function _14_Hacl_Bignum4096_mod_exp_vartime_precomp (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum4096_mod_exp_vartime_precomp unimplemetned');
}

//Provides: _10_Hacl_Bignum4096_mod_inv_prime_vartime
//Requires: caml_failwith
function _10_Hacl_Bignum4096_mod_inv_prime_vartime (x0, x1, x2) {
  caml_failwith('Hacl_Bignum4096_mod_inv_prime_vartime unimplemetned');
}

//Provides: _16_Hacl_Bignum4096_mod_inv_prime_vartime_precomp
//Requires: caml_failwith
function _16_Hacl_Bignum4096_mod_inv_prime_vartime_precomp (x0, x1, x2) {
  caml_failwith('Hacl_Bignum4096_mod_inv_prime_vartime_precomp unimplemetned');
}

//Provides: _13_Hacl_Bignum4096_mod_precomp
//Requires: caml_failwith
function _13_Hacl_Bignum4096_mod_precomp (x0, x1, x2) {
  caml_failwith('Hacl_Bignum4096_mod_precomp unimplemetned');
}

//Provides: _12_Hacl_Bignum4096_mont_ctx_free
//Requires: caml_failwith
function _12_Hacl_Bignum4096_mont_ctx_free (x0) {
  caml_failwith('Hacl_Bignum4096_mont_ctx_free unimplemetned');
}

//Provides: _11_Hacl_Bignum4096_mont_ctx_init
//Requires: caml_failwith
function _11_Hacl_Bignum4096_mont_ctx_init (x0) {
  caml_failwith('Hacl_Bignum4096_mont_ctx_init unimplemetned');
}

//Provides: _5_Hacl_Bignum4096_mul
//Requires: caml_failwith
function _5_Hacl_Bignum4096_mul (x0, x1, x2) {
  caml_failwith('Hacl_Bignum4096_mul unimplemetned');
}

//Provides: _17_Hacl_Bignum4096_new_bn_from_bytes_be
//Requires: caml_failwith
function _17_Hacl_Bignum4096_new_bn_from_bytes_be (x0, x1) {
  caml_failwith('Hacl_Bignum4096_new_bn_from_bytes_be unimplemetned');
}

//Provides: _18_Hacl_Bignum4096_new_bn_from_bytes_le
//Requires: caml_failwith
function _18_Hacl_Bignum4096_new_bn_from_bytes_le (x0, x1) {
  caml_failwith('Hacl_Bignum4096_new_bn_from_bytes_le unimplemetned');
}

//Provides: _6_Hacl_Bignum4096_sqr
//Requires: caml_failwith
function _6_Hacl_Bignum4096_sqr (x0, x1) {
  caml_failwith('Hacl_Bignum4096_sqr unimplemetned');
}

//Provides: _2_Hacl_Bignum4096_sub
//Requires: caml_failwith
function _2_Hacl_Bignum4096_sub (x0, x1, x2) {
  caml_failwith('Hacl_Bignum4096_sub unimplemetned');
}

//Provides: _4_Hacl_Bignum4096_sub_mod
//Requires: caml_failwith
function _4_Hacl_Bignum4096_sub_mod (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum4096_sub_mod unimplemetned');
}

//Provides: _1_Hacl_Bignum64_add
//Requires: caml_failwith
function _1_Hacl_Bignum64_add (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum64_add unimplemetned');
}

//Provides: _3_Hacl_Bignum64_add_mod
//Requires: caml_failwith
function _3_Hacl_Bignum64_add_mod (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum64_add_mod unimplemetned');
}

//Provides: _19_Hacl_Bignum64_bn_to_bytes_be
//Requires: caml_failwith
function _19_Hacl_Bignum64_bn_to_bytes_be (x0, x1, x2) {
  caml_failwith('Hacl_Bignum64_bn_to_bytes_be unimplemetned');
}

//Provides: _20_Hacl_Bignum64_bn_to_bytes_le
//Requires: caml_failwith
function _20_Hacl_Bignum64_bn_to_bytes_le (x0, x1, x2) {
  caml_failwith('Hacl_Bignum64_bn_to_bytes_le unimplemetned');
}

//Provides: _22_Hacl_Bignum64_eq_mask
//Requires: caml_failwith
function _22_Hacl_Bignum64_eq_mask (x0, x1, x2) {
  caml_failwith('Hacl_Bignum64_eq_mask unimplemetned');
}

//Provides: _21_Hacl_Bignum64_lt_mask
//Requires: caml_failwith
function _21_Hacl_Bignum64_lt_mask (x0, x1, x2) {
  caml_failwith('Hacl_Bignum64_lt_mask unimplemetned');
}

//Provides: _7_Hacl_Bignum64_mod
//Requires: caml_failwith
function _7_Hacl_Bignum64_mod (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum64_mod unimplemetned');
}

//Provides: _9_Hacl_Bignum64_mod_exp_consttime
//Requires: caml_failwith
function _9_Hacl_Bignum64_mod_exp_consttime (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Bignum64_mod_exp_consttime unimplemetned');
}

//Provides: _9_Hacl_Bignum64_mod_exp_consttime_byte6
//Requires: _9_Hacl_Bignum64_mod_exp_consttime
function _9_Hacl_Bignum64_mod_exp_consttime_byte6 (x0, x1, x2, x3, x4, x5) {
  return _9_Hacl_Bignum64_mod_exp_consttime(x0, x1, x2, x3, x4, x5);
}

//Provides: _15_Hacl_Bignum64_mod_exp_consttime_precomp
//Requires: caml_failwith
function _15_Hacl_Bignum64_mod_exp_consttime_precomp (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum64_mod_exp_consttime_precomp unimplemetned');
}

//Provides: _8_Hacl_Bignum64_mod_exp_vartime
//Requires: caml_failwith
function _8_Hacl_Bignum64_mod_exp_vartime (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Bignum64_mod_exp_vartime unimplemetned');
}

//Provides: _8_Hacl_Bignum64_mod_exp_vartime_byte6
//Requires: _8_Hacl_Bignum64_mod_exp_vartime
function _8_Hacl_Bignum64_mod_exp_vartime_byte6 (x0, x1, x2, x3, x4, x5) {
  return _8_Hacl_Bignum64_mod_exp_vartime(x0, x1, x2, x3, x4, x5);
}

//Provides: _14_Hacl_Bignum64_mod_exp_vartime_precomp
//Requires: caml_failwith
function _14_Hacl_Bignum64_mod_exp_vartime_precomp (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum64_mod_exp_vartime_precomp unimplemetned');
}

//Provides: _10_Hacl_Bignum64_mod_inv_prime_vartime
//Requires: caml_failwith
function _10_Hacl_Bignum64_mod_inv_prime_vartime (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum64_mod_inv_prime_vartime unimplemetned');
}

//Provides: _16_Hacl_Bignum64_mod_inv_prime_vartime_precomp
//Requires: caml_failwith
function _16_Hacl_Bignum64_mod_inv_prime_vartime_precomp (x0, x1, x2) {
  caml_failwith('Hacl_Bignum64_mod_inv_prime_vartime_precomp unimplemetned');
}

//Provides: _13_Hacl_Bignum64_mod_precomp
//Requires: caml_failwith
function _13_Hacl_Bignum64_mod_precomp (x0, x1, x2) {
  caml_failwith('Hacl_Bignum64_mod_precomp unimplemetned');
}

//Provides: _12_Hacl_Bignum64_mont_ctx_free
//Requires: caml_failwith
function _12_Hacl_Bignum64_mont_ctx_free (x0) {
  caml_failwith('Hacl_Bignum64_mont_ctx_free unimplemetned');
}

//Provides: _11_Hacl_Bignum64_mont_ctx_init
//Requires: caml_failwith
function _11_Hacl_Bignum64_mont_ctx_init (x0, x1) {
  caml_failwith('Hacl_Bignum64_mont_ctx_init unimplemetned');
}

//Provides: _5_Hacl_Bignum64_mul
//Requires: caml_failwith
function _5_Hacl_Bignum64_mul (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum64_mul unimplemetned');
}

//Provides: _17_Hacl_Bignum64_new_bn_from_bytes_be
//Requires: caml_failwith
function _17_Hacl_Bignum64_new_bn_from_bytes_be (x0, x1) {
  caml_failwith('Hacl_Bignum64_new_bn_from_bytes_be unimplemetned');
}

//Provides: _18_Hacl_Bignum64_new_bn_from_bytes_le
//Requires: caml_failwith
function _18_Hacl_Bignum64_new_bn_from_bytes_le (x0, x1) {
  caml_failwith('Hacl_Bignum64_new_bn_from_bytes_le unimplemetned');
}

//Provides: _6_Hacl_Bignum64_sqr
//Requires: caml_failwith
function _6_Hacl_Bignum64_sqr (x0, x1, x2) {
  caml_failwith('Hacl_Bignum64_sqr unimplemetned');
}

//Provides: _2_Hacl_Bignum64_sub
//Requires: caml_failwith
function _2_Hacl_Bignum64_sub (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum64_sub unimplemetned');
}

//Provides: _4_Hacl_Bignum64_sub_mod
//Requires: caml_failwith
function _4_Hacl_Bignum64_sub_mod (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum64_sub_mod unimplemetned');
}

//Provides: _7_Hacl_Bignum_Addition_bn_add_eq_len_u32
//Requires: caml_failwith
function _7_Hacl_Bignum_Addition_bn_add_eq_len_u32 (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum_Addition_bn_add_eq_len_u32 unimplemetned');
}

//Provides: _8_Hacl_Bignum_Addition_bn_add_eq_len_u64
//Requires: caml_failwith
function _8_Hacl_Bignum_Addition_bn_add_eq_len_u64 (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum_Addition_bn_add_eq_len_u64 unimplemetned');
}

//Provides: _5_Hacl_Bignum_Addition_bn_sub_eq_len_u32
//Requires: caml_failwith
function _5_Hacl_Bignum_Addition_bn_sub_eq_len_u32 (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum_Addition_bn_sub_eq_len_u32 unimplemetned');
}

//Provides: _6_Hacl_Bignum_Addition_bn_sub_eq_len_u64
//Requires: caml_failwith
function _6_Hacl_Bignum_Addition_bn_sub_eq_len_u64 (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum_Addition_bn_sub_eq_len_u64 unimplemetned');
}

//Provides: _2_Hacl_Bignum_Base_mul_wide_add2_u32
//Requires: caml_failwith
function _2_Hacl_Bignum_Base_mul_wide_add2_u32 (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum_Base_mul_wide_add2_u32 unimplemetned');
}

//Provides: _3_Hacl_Bignum_Base_mul_wide_add2_u64
//Requires: caml_failwith
function _3_Hacl_Bignum_Base_mul_wide_add2_u64 (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum_Base_mul_wide_add2_u64 unimplemetned');
}

//Provides: _1_Hacl_Bignum_Base_mul_wide_add_u64
//Requires: caml_failwith
function _1_Hacl_Bignum_Base_mul_wide_add_u64 (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum_Base_mul_wide_add_u64 unimplemetned');
}

//Provides: _1_Hacl_Bignum_Convert_bn_from_bytes_be_uint64
//Requires: caml_failwith
function _1_Hacl_Bignum_Convert_bn_from_bytes_be_uint64 (x0, x1, x2) {
  caml_failwith('Hacl_Bignum_Convert_bn_from_bytes_be_uint64 unimplemetned');
}

//Provides: _2_Hacl_Bignum_Convert_bn_to_bytes_be_uint64
//Requires: caml_failwith
function _2_Hacl_Bignum_Convert_bn_to_bytes_be_uint64 (x0, x1, x2) {
  caml_failwith('Hacl_Bignum_Convert_bn_to_bytes_be_uint64 unimplemetned');
}

//Provides: _33_Hacl_Bignum_Exponentiation_bn_check_mod_exp_u32
//Requires: caml_failwith
function _33_Hacl_Bignum_Exponentiation_bn_check_mod_exp_u32 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum_Exponentiation_bn_check_mod_exp_u32 unimplemetned');
}

//Provides: _38_Hacl_Bignum_Exponentiation_bn_check_mod_exp_u64
//Requires: caml_failwith
function _38_Hacl_Bignum_Exponentiation_bn_check_mod_exp_u64 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum_Exponentiation_bn_check_mod_exp_u64 unimplemetned');
}

//Provides: _35_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u32
//Requires: caml_failwith
function _35_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u32 (x0, x1, x2, x3, x4, x5, x6, x7) {
  caml_failwith('Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u32 unimplemetned');
}

//Provides: _35_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u32_byte8
//Requires: _35_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u32
function _35_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u32_byte8 (x0, x1, x2, x3, x4, x5, x6, x7) {
  return _35_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u32(x0, x1, x2, x3, x4, x5, x6, x7);
}

//Provides: _40_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u64
//Requires: caml_failwith
function _40_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u64 (x0, x1, x2, x3, x4, x5, x6, x7) {
  caml_failwith('Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u64 unimplemetned');
}

//Provides: _40_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u64_byte8
//Requires: _40_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u64
function _40_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u64_byte8 (x0, x1, x2, x3, x4, x5, x6, x7) {
  return _40_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u64(x0, x1, x2, x3, x4, x5, x6, x7);
}

//Provides: _37_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u32
//Requires: caml_failwith
function _37_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u32 (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u32 unimplemetned');
}

//Provides: _37_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u32_byte7
//Requires: _37_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u32
function _37_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u32_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _37_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u32(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _42_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u64
//Requires: caml_failwith
function _42_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u64 (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u64 unimplemetned');
}

//Provides: _42_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u64_byte7
//Requires: _42_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u64
function _42_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u64_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _42_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u64(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _34_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u32
//Requires: caml_failwith
function _34_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u32 (x0, x1, x2, x3, x4, x5, x6, x7) {
  caml_failwith('Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u32 unimplemetned');
}

//Provides: _34_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u32_byte8
//Requires: _34_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u32
function _34_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u32_byte8 (x0, x1, x2, x3, x4, x5, x6, x7) {
  return _34_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u32(x0, x1, x2, x3, x4, x5, x6, x7);
}

//Provides: _39_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u64
//Requires: caml_failwith
function _39_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u64 (x0, x1, x2, x3, x4, x5, x6, x7) {
  caml_failwith('Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u64 unimplemetned');
}

//Provides: _39_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u64_byte8
//Requires: _39_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u64
function _39_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u64_byte8 (x0, x1, x2, x3, x4, x5, x6, x7) {
  return _39_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u64(x0, x1, x2, x3, x4, x5, x6, x7);
}

//Provides: _36_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u32
//Requires: caml_failwith
function _36_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u32 (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u32 unimplemetned');
}

//Provides: _36_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u32_byte7
//Requires: _36_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u32
function _36_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u32_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _36_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u32(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _41_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u64
//Requires: caml_failwith
function _41_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u64 (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u64 unimplemetned');
}

//Provides: _41_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u64_byte7
//Requires: _41_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u64
function _41_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u64_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _41_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u64(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _9_Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint32
//Requires: caml_failwith
function _9_Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint32 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint32 unimplemetned');
}

//Provides: _10_Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint64
//Requires: caml_failwith
function _10_Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint64 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint64 unimplemetned');
}

//Provides: _11_Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint32
//Requires: caml_failwith
function _11_Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint32 (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint32 unimplemetned');
}

//Provides: _12_Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint64
//Requires: caml_failwith
function _12_Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint64 (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint64 unimplemetned');
}

//Provides: _3_Hacl_Bignum_Lib_bn_get_top_index_u32
//Requires: caml_failwith
function _3_Hacl_Bignum_Lib_bn_get_top_index_u32 (x0, x1) {
  caml_failwith('Hacl_Bignum_Lib_bn_get_top_index_u32 unimplemetned');
}

//Provides: _4_Hacl_Bignum_Lib_bn_get_top_index_u64
//Requires: caml_failwith
function _4_Hacl_Bignum_Lib_bn_get_top_index_u64 (x0, x1) {
  caml_failwith('Hacl_Bignum_Lib_bn_get_top_index_u64 unimplemetned');
}

//Provides: _17_Hacl_Bignum_ModInvLimb_mod_inv_uint32
//Requires: caml_failwith
function _17_Hacl_Bignum_ModInvLimb_mod_inv_uint32 (x0) {
  caml_failwith('Hacl_Bignum_ModInvLimb_mod_inv_uint32 unimplemetned');
}

//Provides: _18_Hacl_Bignum_ModInvLimb_mod_inv_uint64
//Requires: caml_failwith
function _18_Hacl_Bignum_ModInvLimb_mod_inv_uint64 (x0) {
  caml_failwith('Hacl_Bignum_ModInvLimb_mod_inv_uint64 unimplemetned');
}

//Provides: _19_Hacl_Bignum_Montgomery_bn_check_modulus_u32
//Requires: caml_failwith
function _19_Hacl_Bignum_Montgomery_bn_check_modulus_u32 (x0, x1) {
  caml_failwith('Hacl_Bignum_Montgomery_bn_check_modulus_u32 unimplemetned');
}

//Provides: _26_Hacl_Bignum_Montgomery_bn_check_modulus_u64
//Requires: caml_failwith
function _26_Hacl_Bignum_Montgomery_bn_check_modulus_u64 (x0, x1) {
  caml_failwith('Hacl_Bignum_Montgomery_bn_check_modulus_u64 unimplemetned');
}

//Provides: _23_Hacl_Bignum_Montgomery_bn_from_mont_u32
//Requires: caml_failwith
function _23_Hacl_Bignum_Montgomery_bn_from_mont_u32 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum_Montgomery_bn_from_mont_u32 unimplemetned');
}

//Provides: _30_Hacl_Bignum_Montgomery_bn_from_mont_u64
//Requires: caml_failwith
function _30_Hacl_Bignum_Montgomery_bn_from_mont_u64 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum_Montgomery_bn_from_mont_u64 unimplemetned');
}

//Provides: _24_Hacl_Bignum_Montgomery_bn_mont_mul_u32
//Requires: caml_failwith
function _24_Hacl_Bignum_Montgomery_bn_mont_mul_u32 (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Bignum_Montgomery_bn_mont_mul_u32 unimplemetned');
}

//Provides: _24_Hacl_Bignum_Montgomery_bn_mont_mul_u32_byte6
//Requires: _24_Hacl_Bignum_Montgomery_bn_mont_mul_u32
function _24_Hacl_Bignum_Montgomery_bn_mont_mul_u32_byte6 (x0, x1, x2, x3, x4, x5) {
  return _24_Hacl_Bignum_Montgomery_bn_mont_mul_u32(x0, x1, x2, x3, x4, x5);
}

//Provides: _31_Hacl_Bignum_Montgomery_bn_mont_mul_u64
//Requires: caml_failwith
function _31_Hacl_Bignum_Montgomery_bn_mont_mul_u64 (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Bignum_Montgomery_bn_mont_mul_u64 unimplemetned');
}

//Provides: _31_Hacl_Bignum_Montgomery_bn_mont_mul_u64_byte6
//Requires: _31_Hacl_Bignum_Montgomery_bn_mont_mul_u64
function _31_Hacl_Bignum_Montgomery_bn_mont_mul_u64_byte6 (x0, x1, x2, x3, x4, x5) {
  return _31_Hacl_Bignum_Montgomery_bn_mont_mul_u64(x0, x1, x2, x3, x4, x5);
}

//Provides: _21_Hacl_Bignum_Montgomery_bn_mont_reduction_u32
//Requires: caml_failwith
function _21_Hacl_Bignum_Montgomery_bn_mont_reduction_u32 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum_Montgomery_bn_mont_reduction_u32 unimplemetned');
}

//Provides: _28_Hacl_Bignum_Montgomery_bn_mont_reduction_u64
//Requires: caml_failwith
function _28_Hacl_Bignum_Montgomery_bn_mont_reduction_u64 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum_Montgomery_bn_mont_reduction_u64 unimplemetned');
}

//Provides: _25_Hacl_Bignum_Montgomery_bn_mont_sqr_u32
//Requires: caml_failwith
function _25_Hacl_Bignum_Montgomery_bn_mont_sqr_u32 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum_Montgomery_bn_mont_sqr_u32 unimplemetned');
}

//Provides: _32_Hacl_Bignum_Montgomery_bn_mont_sqr_u64
//Requires: caml_failwith
function _32_Hacl_Bignum_Montgomery_bn_mont_sqr_u64 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum_Montgomery_bn_mont_sqr_u64 unimplemetned');
}

//Provides: _20_Hacl_Bignum_Montgomery_bn_precomp_r2_mod_n_u32
//Requires: caml_failwith
function _20_Hacl_Bignum_Montgomery_bn_precomp_r2_mod_n_u32 (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum_Montgomery_bn_precomp_r2_mod_n_u32 unimplemetned');
}

//Provides: _27_Hacl_Bignum_Montgomery_bn_precomp_r2_mod_n_u64
//Requires: caml_failwith
function _27_Hacl_Bignum_Montgomery_bn_precomp_r2_mod_n_u64 (x0, x1, x2, x3) {
  caml_failwith('Hacl_Bignum_Montgomery_bn_precomp_r2_mod_n_u64 unimplemetned');
}

//Provides: _22_Hacl_Bignum_Montgomery_bn_to_mont_u32
//Requires: caml_failwith
function _22_Hacl_Bignum_Montgomery_bn_to_mont_u32 (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Bignum_Montgomery_bn_to_mont_u32 unimplemetned');
}

//Provides: _22_Hacl_Bignum_Montgomery_bn_to_mont_u32_byte6
//Requires: _22_Hacl_Bignum_Montgomery_bn_to_mont_u32
function _22_Hacl_Bignum_Montgomery_bn_to_mont_u32_byte6 (x0, x1, x2, x3, x4, x5) {
  return _22_Hacl_Bignum_Montgomery_bn_to_mont_u32(x0, x1, x2, x3, x4, x5);
}

//Provides: _29_Hacl_Bignum_Montgomery_bn_to_mont_u64
//Requires: caml_failwith
function _29_Hacl_Bignum_Montgomery_bn_to_mont_u64 (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Bignum_Montgomery_bn_to_mont_u64 unimplemetned');
}

//Provides: _29_Hacl_Bignum_Montgomery_bn_to_mont_u64_byte6
//Requires: _29_Hacl_Bignum_Montgomery_bn_to_mont_u64
function _29_Hacl_Bignum_Montgomery_bn_to_mont_u64_byte6 (x0, x1, x2, x3, x4, x5) {
  return _29_Hacl_Bignum_Montgomery_bn_to_mont_u64(x0, x1, x2, x3, x4, x5);
}

//Provides: _13_Hacl_Bignum_bn_add_mod_n_u32
//Requires: caml_failwith
function _13_Hacl_Bignum_bn_add_mod_n_u32 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum_bn_add_mod_n_u32 unimplemetned');
}

//Provides: _14_Hacl_Bignum_bn_add_mod_n_u64
//Requires: caml_failwith
function _14_Hacl_Bignum_bn_add_mod_n_u64 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum_bn_add_mod_n_u64 unimplemetned');
}

//Provides: _15_Hacl_Bignum_bn_sub_mod_n_u32
//Requires: caml_failwith
function _15_Hacl_Bignum_bn_sub_mod_n_u32 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum_bn_sub_mod_n_u32 unimplemetned');
}

//Provides: _16_Hacl_Bignum_bn_sub_mod_n_u64
//Requires: caml_failwith
function _16_Hacl_Bignum_bn_sub_mod_n_u64 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Bignum_bn_sub_mod_n_u64 unimplemetned');
}

//Provides: _2_Hacl_Blake2b_256_blake2b
//Requires: caml_failwith
function _2_Hacl_Blake2b_256_blake2b (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Blake2b_256_blake2b unimplemetned');
}

//Provides: _2_Hacl_Blake2b_256_blake2b_byte6
//Requires: _2_Hacl_Blake2b_256_blake2b
function _2_Hacl_Blake2b_256_blake2b_byte6 (x0, x1, x2, x3, x4, x5) {
  return _2_Hacl_Blake2b_256_blake2b(x0, x1, x2, x3, x4, x5);
}

//Provides: _9_Hacl_Blake2b_32_blake2b_finish
//Requires: caml_failwith
function _9_Hacl_Blake2b_32_blake2b_finish (x0, x1, x2) {
  caml_failwith('Hacl_Blake2b_32_blake2b_finish unimplemetned');
}

//Provides: _7_Hacl_Blake2b_32_blake2b_init
//Requires: caml_failwith
function _7_Hacl_Blake2b_32_blake2b_init (x0, x1, x2) {
  caml_failwith('Hacl_Blake2b_32_blake2b_init unimplemetned');
}

//Provides: _8_Hacl_Blake2b_32_blake2b_update_key
//Requires: caml_failwith
function _8_Hacl_Blake2b_32_blake2b_update_key (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Blake2b_32_blake2b_update_key unimplemetned');
}

//Provides: _2_Hacl_Blake2s_128_blake2s
//Requires: caml_failwith
function _2_Hacl_Blake2s_128_blake2s (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Blake2s_128_blake2s unimplemetned');
}

//Provides: _2_Hacl_Blake2s_128_blake2s_byte6
//Requires: _2_Hacl_Blake2s_128_blake2s
function _2_Hacl_Blake2s_128_blake2s_byte6 (x0, x1, x2, x3, x4, x5) {
  return _2_Hacl_Blake2s_128_blake2s(x0, x1, x2, x3, x4, x5);
}

//Provides: _15_Hacl_Blake2s_32_blake2s_finish
//Requires: caml_failwith
function _15_Hacl_Blake2s_32_blake2s_finish (x0, x1, x2) {
  caml_failwith('Hacl_Blake2s_32_blake2s_finish unimplemetned');
}

//Provides: _11_Hacl_Blake2s_32_blake2s_init
//Requires: caml_failwith
function _11_Hacl_Blake2s_32_blake2s_init (x0, x1, x2) {
  caml_failwith('Hacl_Blake2s_32_blake2s_init unimplemetned');
}

//Provides: _12_Hacl_Blake2s_32_blake2s_update_key
//Requires: caml_failwith
function _12_Hacl_Blake2s_32_blake2s_update_key (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Blake2s_32_blake2s_update_key unimplemetned');
}

//Provides: _14_Hacl_Blake2s_32_blake2s_update_last
//Requires: caml_failwith
function _14_Hacl_Blake2s_32_blake2s_update_last (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Blake2s_32_blake2s_update_last unimplemetned');
}

//Provides: _14_Hacl_Blake2s_32_blake2s_update_last_byte6
//Requires: _14_Hacl_Blake2s_32_blake2s_update_last
function _14_Hacl_Blake2s_32_blake2s_update_last_byte6 (x0, x1, x2, x3, x4, x5) {
  return _14_Hacl_Blake2s_32_blake2s_update_last(x0, x1, x2, x3, x4, x5);
}

//Provides: _13_Hacl_Blake2s_32_blake2s_update_multi
//Requires: caml_failwith
function _13_Hacl_Blake2s_32_blake2s_update_multi (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Blake2s_32_blake2s_update_multi unimplemetned');
}

//Provides: _13_Hacl_Blake2s_32_blake2s_update_multi_byte6
//Requires: _13_Hacl_Blake2s_32_blake2s_update_multi
function _13_Hacl_Blake2s_32_blake2s_update_multi_byte6 (x0, x1, x2, x3, x4, x5) {
  return _13_Hacl_Blake2s_32_blake2s_update_multi(x0, x1, x2, x3, x4, x5);
}

//Provides: _2_Hacl_Chacha20Poly1305_128_aead_decrypt
//Requires: caml_failwith
function _2_Hacl_Chacha20Poly1305_128_aead_decrypt (x0, x1, x2, x3, x4, x5, x6, x7) {
  caml_failwith('Hacl_Chacha20Poly1305_128_aead_decrypt unimplemetned');
}

//Provides: _2_Hacl_Chacha20Poly1305_128_aead_decrypt_byte8
//Requires: _2_Hacl_Chacha20Poly1305_128_aead_decrypt
function _2_Hacl_Chacha20Poly1305_128_aead_decrypt_byte8 (x0, x1, x2, x3, x4, x5, x6, x7) {
  return _2_Hacl_Chacha20Poly1305_128_aead_decrypt(x0, x1, x2, x3, x4, x5, x6, x7);
}

//Provides: _1_Hacl_Chacha20Poly1305_128_aead_encrypt
//Requires: caml_failwith
function _1_Hacl_Chacha20Poly1305_128_aead_encrypt (x0, x1, x2, x3, x4, x5, x6, x7) {
  caml_failwith('Hacl_Chacha20Poly1305_128_aead_encrypt unimplemetned');
}

//Provides: _1_Hacl_Chacha20Poly1305_128_aead_encrypt_byte8
//Requires: _1_Hacl_Chacha20Poly1305_128_aead_encrypt
function _1_Hacl_Chacha20Poly1305_128_aead_encrypt_byte8 (x0, x1, x2, x3, x4, x5, x6, x7) {
  return _1_Hacl_Chacha20Poly1305_128_aead_encrypt(x0, x1, x2, x3, x4, x5, x6, x7);
}

//Provides: _2_Hacl_Chacha20Poly1305_256_aead_decrypt
//Requires: caml_failwith
function _2_Hacl_Chacha20Poly1305_256_aead_decrypt (x0, x1, x2, x3, x4, x5, x6, x7) {
  caml_failwith('Hacl_Chacha20Poly1305_256_aead_decrypt unimplemetned');
}

//Provides: _2_Hacl_Chacha20Poly1305_256_aead_decrypt_byte8
//Requires: _2_Hacl_Chacha20Poly1305_256_aead_decrypt
function _2_Hacl_Chacha20Poly1305_256_aead_decrypt_byte8 (x0, x1, x2, x3, x4, x5, x6, x7) {
  return _2_Hacl_Chacha20Poly1305_256_aead_decrypt(x0, x1, x2, x3, x4, x5, x6, x7);
}

//Provides: _1_Hacl_Chacha20Poly1305_256_aead_encrypt
//Requires: caml_failwith
function _1_Hacl_Chacha20Poly1305_256_aead_encrypt (x0, x1, x2, x3, x4, x5, x6, x7) {
  caml_failwith('Hacl_Chacha20Poly1305_256_aead_encrypt unimplemetned');
}

//Provides: _1_Hacl_Chacha20Poly1305_256_aead_encrypt_byte8
//Requires: _1_Hacl_Chacha20Poly1305_256_aead_encrypt
function _1_Hacl_Chacha20Poly1305_256_aead_encrypt_byte8 (x0, x1, x2, x3, x4, x5, x6, x7) {
  return _1_Hacl_Chacha20Poly1305_256_aead_encrypt(x0, x1, x2, x3, x4, x5, x6, x7);
}

//Provides: _2_Hacl_Chacha20_Vec128_chacha20_decrypt_128
//Requires: caml_failwith
function _2_Hacl_Chacha20_Vec128_chacha20_decrypt_128 (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Chacha20_Vec128_chacha20_decrypt_128 unimplemetned');
}

//Provides: _2_Hacl_Chacha20_Vec128_chacha20_decrypt_128_byte6
//Requires: _2_Hacl_Chacha20_Vec128_chacha20_decrypt_128
function _2_Hacl_Chacha20_Vec128_chacha20_decrypt_128_byte6 (x0, x1, x2, x3, x4, x5) {
  return _2_Hacl_Chacha20_Vec128_chacha20_decrypt_128(x0, x1, x2, x3, x4, x5);
}

//Provides: _1_Hacl_Chacha20_Vec128_chacha20_encrypt_128
//Requires: caml_failwith
function _1_Hacl_Chacha20_Vec128_chacha20_encrypt_128 (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Chacha20_Vec128_chacha20_encrypt_128 unimplemetned');
}

//Provides: _1_Hacl_Chacha20_Vec128_chacha20_encrypt_128_byte6
//Requires: _1_Hacl_Chacha20_Vec128_chacha20_encrypt_128
function _1_Hacl_Chacha20_Vec128_chacha20_encrypt_128_byte6 (x0, x1, x2, x3, x4, x5) {
  return _1_Hacl_Chacha20_Vec128_chacha20_encrypt_128(x0, x1, x2, x3, x4, x5);
}

//Provides: _2_Hacl_Chacha20_Vec256_chacha20_decrypt_256
//Requires: caml_failwith
function _2_Hacl_Chacha20_Vec256_chacha20_decrypt_256 (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Chacha20_Vec256_chacha20_decrypt_256 unimplemetned');
}

//Provides: _2_Hacl_Chacha20_Vec256_chacha20_decrypt_256_byte6
//Requires: _2_Hacl_Chacha20_Vec256_chacha20_decrypt_256
function _2_Hacl_Chacha20_Vec256_chacha20_decrypt_256_byte6 (x0, x1, x2, x3, x4, x5) {
  return _2_Hacl_Chacha20_Vec256_chacha20_decrypt_256(x0, x1, x2, x3, x4, x5);
}

//Provides: _1_Hacl_Chacha20_Vec256_chacha20_encrypt_256
//Requires: caml_failwith
function _1_Hacl_Chacha20_Vec256_chacha20_encrypt_256 (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Chacha20_Vec256_chacha20_encrypt_256 unimplemetned');
}

//Provides: _1_Hacl_Chacha20_Vec256_chacha20_encrypt_256_byte6
//Requires: _1_Hacl_Chacha20_Vec256_chacha20_encrypt_256
function _1_Hacl_Chacha20_Vec256_chacha20_encrypt_256_byte6 (x0, x1, x2, x3, x4, x5) {
  return _1_Hacl_Chacha20_Vec256_chacha20_encrypt_256(x0, x1, x2, x3, x4, x5);
}

//Provides: _2_Hacl_Chacha20_Vec32_chacha20_decrypt_32
//Requires: caml_failwith
function _2_Hacl_Chacha20_Vec32_chacha20_decrypt_32 (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Chacha20_Vec32_chacha20_decrypt_32 unimplemetned');
}

//Provides: _2_Hacl_Chacha20_Vec32_chacha20_decrypt_32_byte6
//Requires: _2_Hacl_Chacha20_Vec32_chacha20_decrypt_32
function _2_Hacl_Chacha20_Vec32_chacha20_decrypt_32_byte6 (x0, x1, x2, x3, x4, x5) {
  return _2_Hacl_Chacha20_Vec32_chacha20_decrypt_32(x0, x1, x2, x3, x4, x5);
}

//Provides: _1_Hacl_Chacha20_Vec32_chacha20_encrypt_32
//Requires: caml_failwith
function _1_Hacl_Chacha20_Vec32_chacha20_encrypt_32 (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Chacha20_Vec32_chacha20_encrypt_32 unimplemetned');
}

//Provides: _1_Hacl_Chacha20_Vec32_chacha20_encrypt_32_byte6
//Requires: _1_Hacl_Chacha20_Vec32_chacha20_encrypt_32
function _1_Hacl_Chacha20_Vec32_chacha20_encrypt_32_byte6 (x0, x1, x2, x3, x4, x5) {
  return _1_Hacl_Chacha20_Vec32_chacha20_encrypt_32(x0, x1, x2, x3, x4, x5);
}

//Provides: _5_Hacl_Chacha20_chacha20_decrypt
//Requires: caml_failwith
function _5_Hacl_Chacha20_chacha20_decrypt (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Chacha20_chacha20_decrypt unimplemetned');
}

//Provides: _5_Hacl_Chacha20_chacha20_decrypt_byte6
//Requires: _5_Hacl_Chacha20_chacha20_decrypt
function _5_Hacl_Chacha20_chacha20_decrypt_byte6 (x0, x1, x2, x3, x4, x5) {
  return _5_Hacl_Chacha20_chacha20_decrypt(x0, x1, x2, x3, x4, x5);
}

//Provides: _4_Hacl_Chacha20_chacha20_encrypt
//Requires: caml_failwith
function _4_Hacl_Chacha20_chacha20_encrypt (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Chacha20_chacha20_encrypt unimplemetned');
}

//Provides: _4_Hacl_Chacha20_chacha20_encrypt_byte6
//Requires: _4_Hacl_Chacha20_chacha20_encrypt
function _4_Hacl_Chacha20_chacha20_encrypt_byte6 (x0, x1, x2, x3, x4, x5) {
  return _4_Hacl_Chacha20_chacha20_encrypt(x0, x1, x2, x3, x4, x5);
}

//Provides: _3_Hacl_Curve25519_64_Slow_ecdh
//Requires: caml_failwith
function _3_Hacl_Curve25519_64_Slow_ecdh (x0, x1, x2) {
  caml_failwith('Hacl_Curve25519_64_Slow_ecdh unimplemetned');
}

//Provides: _1_Hacl_Curve25519_64_Slow_scalarmult
//Requires: caml_failwith
function _1_Hacl_Curve25519_64_Slow_scalarmult (x0, x1, x2) {
  caml_failwith('Hacl_Curve25519_64_Slow_scalarmult unimplemetned');
}

//Provides: _2_Hacl_Curve25519_64_Slow_secret_to_public
//Requires: caml_failwith
function _2_Hacl_Curve25519_64_Slow_secret_to_public (x0, x1) {
  caml_failwith('Hacl_Curve25519_64_Slow_secret_to_public unimplemetned');
}

//Provides: _3_Hacl_Curve25519_64_ecdh
//Requires: caml_failwith
function _3_Hacl_Curve25519_64_ecdh (x0, x1, x2) {
  caml_failwith('Hacl_Curve25519_64_ecdh unimplemetned');
}

//Provides: _1_Hacl_Curve25519_64_scalarmult
//Requires: caml_failwith
function _1_Hacl_Curve25519_64_scalarmult (x0, x1, x2) {
  caml_failwith('Hacl_Curve25519_64_scalarmult unimplemetned');
}

//Provides: _2_Hacl_Curve25519_64_secret_to_public
//Requires: caml_failwith
function _2_Hacl_Curve25519_64_secret_to_public (x0, x1) {
  caml_failwith('Hacl_Curve25519_64_secret_to_public unimplemetned');
}

//Provides: _3_Hacl_EC_Ed25519_felem_add
//Requires: caml_failwith
function _3_Hacl_EC_Ed25519_felem_add (x0, x1, x2) {
  caml_failwith('Hacl_EC_Ed25519_felem_add unimplemetned');
}

//Provides: _6_Hacl_EC_Ed25519_felem_inv
//Requires: caml_failwith
function _6_Hacl_EC_Ed25519_felem_inv (x0, x1) {
  caml_failwith('Hacl_EC_Ed25519_felem_inv unimplemetned');
}

//Provides: _7_Hacl_EC_Ed25519_felem_load
//Requires: caml_failwith
function _7_Hacl_EC_Ed25519_felem_load (x0, x1) {
  caml_failwith('Hacl_EC_Ed25519_felem_load unimplemetned');
}

//Provides: _5_Hacl_EC_Ed25519_felem_mul
//Requires: caml_failwith
function _5_Hacl_EC_Ed25519_felem_mul (x0, x1, x2) {
  caml_failwith('Hacl_EC_Ed25519_felem_mul unimplemetned');
}

//Provides: _8_Hacl_EC_Ed25519_felem_store
//Requires: caml_failwith
function _8_Hacl_EC_Ed25519_felem_store (x0, x1) {
  caml_failwith('Hacl_EC_Ed25519_felem_store unimplemetned');
}

//Provides: _4_Hacl_EC_Ed25519_felem_sub
//Requires: caml_failwith
function _4_Hacl_EC_Ed25519_felem_sub (x0, x1, x2) {
  caml_failwith('Hacl_EC_Ed25519_felem_sub unimplemetned');
}

//Provides: _10_Hacl_EC_Ed25519_mk_base_point
//Requires: caml_failwith
function _10_Hacl_EC_Ed25519_mk_base_point (x0) {
  caml_failwith('Hacl_EC_Ed25519_mk_base_point unimplemetned');
}

//Provides: _2_Hacl_EC_Ed25519_mk_felem_one
//Requires: caml_failwith
function _2_Hacl_EC_Ed25519_mk_felem_one (x0) {
  caml_failwith('Hacl_EC_Ed25519_mk_felem_one unimplemetned');
}

//Provides: _1_Hacl_EC_Ed25519_mk_felem_zero
//Requires: caml_failwith
function _1_Hacl_EC_Ed25519_mk_felem_zero (x0) {
  caml_failwith('Hacl_EC_Ed25519_mk_felem_zero unimplemetned');
}

//Provides: _9_Hacl_EC_Ed25519_mk_point_at_inf
//Requires: caml_failwith
function _9_Hacl_EC_Ed25519_mk_point_at_inf (x0) {
  caml_failwith('Hacl_EC_Ed25519_mk_point_at_inf unimplemetned');
}

//Provides: _12_Hacl_EC_Ed25519_point_add
//Requires: caml_failwith
function _12_Hacl_EC_Ed25519_point_add (x0, x1, x2) {
  caml_failwith('Hacl_EC_Ed25519_point_add unimplemetned');
}

//Provides: _15_Hacl_EC_Ed25519_point_compress
//Requires: caml_failwith
function _15_Hacl_EC_Ed25519_point_compress (x0, x1) {
  caml_failwith('Hacl_EC_Ed25519_point_compress unimplemetned');
}

//Provides: _16_Hacl_EC_Ed25519_point_decompress
//Requires: caml_failwith
function _16_Hacl_EC_Ed25519_point_decompress (x0, x1) {
  caml_failwith('Hacl_EC_Ed25519_point_decompress unimplemetned');
}

//Provides: _14_Hacl_EC_Ed25519_point_eq
//Requires: caml_failwith
function _14_Hacl_EC_Ed25519_point_eq (x0, x1) {
  caml_failwith('Hacl_EC_Ed25519_point_eq unimplemetned');
}

//Provides: _13_Hacl_EC_Ed25519_point_mul
//Requires: caml_failwith
function _13_Hacl_EC_Ed25519_point_mul (x0, x1, x2) {
  caml_failwith('Hacl_EC_Ed25519_point_mul unimplemetned');
}

//Provides: _11_Hacl_EC_Ed25519_point_negate
//Requires: caml_failwith
function _11_Hacl_EC_Ed25519_point_negate (x0, x1) {
  caml_failwith('Hacl_EC_Ed25519_point_negate unimplemetned');
}

//Provides: _14_Hacl_Ed25519_expand_keys
//Requires: caml_failwith
function _14_Hacl_Ed25519_expand_keys (x0, x1) {
  caml_failwith('Hacl_Ed25519_expand_keys unimplemetned');
}

//Provides: _15_Hacl_Ed25519_sign_expanded
//Requires: caml_failwith
function _15_Hacl_Ed25519_sign_expanded (x0, x1, x2, x3) {
  caml_failwith('Hacl_Ed25519_sign_expanded unimplemetned');
}

//Provides: _1_Hacl_FFDHE_ffdhe_len
//Requires: caml_failwith
function _1_Hacl_FFDHE_ffdhe_len (x0) {
  caml_failwith('Hacl_FFDHE_ffdhe_len unimplemetned');
}

//Provides: _4_Hacl_FFDHE_ffdhe_secret_to_public
//Requires: caml_failwith
function _4_Hacl_FFDHE_ffdhe_secret_to_public (x0, x1, x2) {
  caml_failwith('Hacl_FFDHE_ffdhe_secret_to_public unimplemetned');
}

//Provides: _3_Hacl_FFDHE_ffdhe_secret_to_public_precomp
//Requires: caml_failwith
function _3_Hacl_FFDHE_ffdhe_secret_to_public_precomp (x0, x1, x2, x3) {
  caml_failwith('Hacl_FFDHE_ffdhe_secret_to_public_precomp unimplemetned');
}

//Provides: _6_Hacl_FFDHE_ffdhe_shared_secret
//Requires: caml_failwith
function _6_Hacl_FFDHE_ffdhe_shared_secret (x0, x1, x2, x3) {
  caml_failwith('Hacl_FFDHE_ffdhe_shared_secret unimplemetned');
}

//Provides: _5_Hacl_FFDHE_ffdhe_shared_secret_precomp
//Requires: caml_failwith
function _5_Hacl_FFDHE_ffdhe_shared_secret_precomp (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_FFDHE_ffdhe_shared_secret_precomp unimplemetned');
}

//Provides: _2_Hacl_FFDHE_new_ffdhe_precomp_p
//Requires: caml_failwith
function _2_Hacl_FFDHE_new_ffdhe_precomp_p (x0) {
  caml_failwith('Hacl_FFDHE_new_ffdhe_precomp_p unimplemetned');
}

//Provides: _7_Hacl_Frodo1344_crypto_kem_dec
//Requires: caml_failwith
function _7_Hacl_Frodo1344_crypto_kem_dec (x0, x1, x2) {
  caml_failwith('Hacl_Frodo1344_crypto_kem_dec unimplemetned');
}

//Provides: _6_Hacl_Frodo1344_crypto_kem_enc
//Requires: caml_failwith
function _6_Hacl_Frodo1344_crypto_kem_enc (x0, x1, x2) {
  caml_failwith('Hacl_Frodo1344_crypto_kem_enc unimplemetned');
}

//Provides: _5_Hacl_Frodo1344_crypto_kem_keypair
//Requires: caml_failwith
function _5_Hacl_Frodo1344_crypto_kem_keypair (x0, x1) {
  caml_failwith('Hacl_Frodo1344_crypto_kem_keypair unimplemetned');
}

//Provides: _7_Hacl_Frodo640_crypto_kem_dec
//Requires: caml_failwith
function _7_Hacl_Frodo640_crypto_kem_dec (x0, x1, x2) {
  caml_failwith('Hacl_Frodo640_crypto_kem_dec unimplemetned');
}

//Provides: _6_Hacl_Frodo640_crypto_kem_enc
//Requires: caml_failwith
function _6_Hacl_Frodo640_crypto_kem_enc (x0, x1, x2) {
  caml_failwith('Hacl_Frodo640_crypto_kem_enc unimplemetned');
}

//Provides: _5_Hacl_Frodo640_crypto_kem_keypair
//Requires: caml_failwith
function _5_Hacl_Frodo640_crypto_kem_keypair (x0, x1) {
  caml_failwith('Hacl_Frodo640_crypto_kem_keypair unimplemetned');
}

//Provides: _7_Hacl_Frodo64_crypto_kem_dec
//Requires: caml_failwith
function _7_Hacl_Frodo64_crypto_kem_dec (x0, x1, x2) {
  caml_failwith('Hacl_Frodo64_crypto_kem_dec unimplemetned');
}

//Provides: _6_Hacl_Frodo64_crypto_kem_enc
//Requires: caml_failwith
function _6_Hacl_Frodo64_crypto_kem_enc (x0, x1, x2) {
  caml_failwith('Hacl_Frodo64_crypto_kem_enc unimplemetned');
}

//Provides: _5_Hacl_Frodo64_crypto_kem_keypair
//Requires: caml_failwith
function _5_Hacl_Frodo64_crypto_kem_keypair (x0, x1) {
  caml_failwith('Hacl_Frodo64_crypto_kem_keypair unimplemetned');
}

//Provides: _7_Hacl_Frodo976_crypto_kem_dec
//Requires: caml_failwith
function _7_Hacl_Frodo976_crypto_kem_dec (x0, x1, x2) {
  caml_failwith('Hacl_Frodo976_crypto_kem_dec unimplemetned');
}

//Provides: _6_Hacl_Frodo976_crypto_kem_enc
//Requires: caml_failwith
function _6_Hacl_Frodo976_crypto_kem_enc (x0, x1, x2) {
  caml_failwith('Hacl_Frodo976_crypto_kem_enc unimplemetned');
}

//Provides: _5_Hacl_Frodo976_crypto_kem_keypair
//Requires: caml_failwith
function _5_Hacl_Frodo976_crypto_kem_keypair (x0, x1) {
  caml_failwith('Hacl_Frodo976_crypto_kem_keypair unimplemetned');
}

//Provides: _7_Hacl_GenericField32_add
//Requires: caml_failwith
function _7_Hacl_GenericField32_add (x0, x1, x2, x3) {
  caml_failwith('Hacl_GenericField32_add unimplemetned');
}

//Provides: _12_Hacl_GenericField32_exp_consttime
//Requires: caml_failwith
function _12_Hacl_GenericField32_exp_consttime (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_GenericField32_exp_consttime unimplemetned');
}

//Provides: _13_Hacl_GenericField32_exp_vartime
//Requires: caml_failwith
function _13_Hacl_GenericField32_exp_vartime (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_GenericField32_exp_vartime unimplemetned');
}

//Provides: _3_Hacl_GenericField32_field_free
//Requires: caml_failwith
function _3_Hacl_GenericField32_field_free (x0) {
  caml_failwith('Hacl_GenericField32_field_free unimplemetned');
}

//Provides: _4_Hacl_GenericField32_field_get_len
//Requires: caml_failwith
function _4_Hacl_GenericField32_field_get_len (x0) {
  caml_failwith('Hacl_GenericField32_field_get_len unimplemetned');
}

//Provides: _2_Hacl_GenericField32_field_init
//Requires: caml_failwith
function _2_Hacl_GenericField32_field_init (x0, x1) {
  caml_failwith('Hacl_GenericField32_field_init unimplemetned');
}

//Provides: _1_Hacl_GenericField32_field_modulus_check
//Requires: caml_failwith
function _1_Hacl_GenericField32_field_modulus_check (x0, x1) {
  caml_failwith('Hacl_GenericField32_field_modulus_check unimplemetned');
}

//Provides: _6_Hacl_GenericField32_from_field
//Requires: caml_failwith
function _6_Hacl_GenericField32_from_field (x0, x1, x2) {
  caml_failwith('Hacl_GenericField32_from_field unimplemetned');
}

//Provides: _14_Hacl_GenericField32_inverse
//Requires: caml_failwith
function _14_Hacl_GenericField32_inverse (x0, x1, x2) {
  caml_failwith('Hacl_GenericField32_inverse unimplemetned');
}

//Provides: _9_Hacl_GenericField32_mul
//Requires: caml_failwith
function _9_Hacl_GenericField32_mul (x0, x1, x2, x3) {
  caml_failwith('Hacl_GenericField32_mul unimplemetned');
}

//Provides: _11_Hacl_GenericField32_one
//Requires: caml_failwith
function _11_Hacl_GenericField32_one (x0, x1) {
  caml_failwith('Hacl_GenericField32_one unimplemetned');
}

//Provides: _10_Hacl_GenericField32_sqr
//Requires: caml_failwith
function _10_Hacl_GenericField32_sqr (x0, x1, x2) {
  caml_failwith('Hacl_GenericField32_sqr unimplemetned');
}

//Provides: _8_Hacl_GenericField32_sub
//Requires: caml_failwith
function _8_Hacl_GenericField32_sub (x0, x1, x2, x3) {
  caml_failwith('Hacl_GenericField32_sub unimplemetned');
}

//Provides: _5_Hacl_GenericField32_to_field
//Requires: caml_failwith
function _5_Hacl_GenericField32_to_field (x0, x1, x2) {
  caml_failwith('Hacl_GenericField32_to_field unimplemetned');
}

//Provides: _7_Hacl_GenericField64_add
//Requires: caml_failwith
function _7_Hacl_GenericField64_add (x0, x1, x2, x3) {
  caml_failwith('Hacl_GenericField64_add unimplemetned');
}

//Provides: _12_Hacl_GenericField64_exp_consttime
//Requires: caml_failwith
function _12_Hacl_GenericField64_exp_consttime (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_GenericField64_exp_consttime unimplemetned');
}

//Provides: _13_Hacl_GenericField64_exp_vartime
//Requires: caml_failwith
function _13_Hacl_GenericField64_exp_vartime (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_GenericField64_exp_vartime unimplemetned');
}

//Provides: _3_Hacl_GenericField64_field_free
//Requires: caml_failwith
function _3_Hacl_GenericField64_field_free (x0) {
  caml_failwith('Hacl_GenericField64_field_free unimplemetned');
}

//Provides: _4_Hacl_GenericField64_field_get_len
//Requires: caml_failwith
function _4_Hacl_GenericField64_field_get_len (x0) {
  caml_failwith('Hacl_GenericField64_field_get_len unimplemetned');
}

//Provides: _2_Hacl_GenericField64_field_init
//Requires: caml_failwith
function _2_Hacl_GenericField64_field_init (x0, x1) {
  caml_failwith('Hacl_GenericField64_field_init unimplemetned');
}

//Provides: _1_Hacl_GenericField64_field_modulus_check
//Requires: caml_failwith
function _1_Hacl_GenericField64_field_modulus_check (x0, x1) {
  caml_failwith('Hacl_GenericField64_field_modulus_check unimplemetned');
}

//Provides: _6_Hacl_GenericField64_from_field
//Requires: caml_failwith
function _6_Hacl_GenericField64_from_field (x0, x1, x2) {
  caml_failwith('Hacl_GenericField64_from_field unimplemetned');
}

//Provides: _14_Hacl_GenericField64_inverse
//Requires: caml_failwith
function _14_Hacl_GenericField64_inverse (x0, x1, x2) {
  caml_failwith('Hacl_GenericField64_inverse unimplemetned');
}

//Provides: _9_Hacl_GenericField64_mul
//Requires: caml_failwith
function _9_Hacl_GenericField64_mul (x0, x1, x2, x3) {
  caml_failwith('Hacl_GenericField64_mul unimplemetned');
}

//Provides: _11_Hacl_GenericField64_one
//Requires: caml_failwith
function _11_Hacl_GenericField64_one (x0, x1) {
  caml_failwith('Hacl_GenericField64_one unimplemetned');
}

//Provides: _10_Hacl_GenericField64_sqr
//Requires: caml_failwith
function _10_Hacl_GenericField64_sqr (x0, x1, x2) {
  caml_failwith('Hacl_GenericField64_sqr unimplemetned');
}

//Provides: _8_Hacl_GenericField64_sub
//Requires: caml_failwith
function _8_Hacl_GenericField64_sub (x0, x1, x2, x3) {
  caml_failwith('Hacl_GenericField64_sub unimplemetned');
}

//Provides: _5_Hacl_GenericField64_to_field
//Requires: caml_failwith
function _5_Hacl_GenericField64_to_field (x0, x1, x2) {
  caml_failwith('Hacl_GenericField64_to_field unimplemetned');
}

//Provides: _1_Hacl_HKDF_Blake2b_256_expand_blake2b_256
//Requires: caml_failwith
function _1_Hacl_HKDF_Blake2b_256_expand_blake2b_256 (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_HKDF_Blake2b_256_expand_blake2b_256 unimplemetned');
}

//Provides: _1_Hacl_HKDF_Blake2b_256_expand_blake2b_256_byte6
//Requires: _1_Hacl_HKDF_Blake2b_256_expand_blake2b_256
function _1_Hacl_HKDF_Blake2b_256_expand_blake2b_256_byte6 (x0, x1, x2, x3, x4, x5) {
  return _1_Hacl_HKDF_Blake2b_256_expand_blake2b_256(x0, x1, x2, x3, x4, x5);
}

//Provides: _2_Hacl_HKDF_Blake2b_256_extract_blake2b_256
//Requires: caml_failwith
function _2_Hacl_HKDF_Blake2b_256_extract_blake2b_256 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_HKDF_Blake2b_256_extract_blake2b_256 unimplemetned');
}

//Provides: _1_Hacl_HKDF_Blake2s_128_expand_blake2s_128
//Requires: caml_failwith
function _1_Hacl_HKDF_Blake2s_128_expand_blake2s_128 (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_HKDF_Blake2s_128_expand_blake2s_128 unimplemetned');
}

//Provides: _1_Hacl_HKDF_Blake2s_128_expand_blake2s_128_byte6
//Requires: _1_Hacl_HKDF_Blake2s_128_expand_blake2s_128
function _1_Hacl_HKDF_Blake2s_128_expand_blake2s_128_byte6 (x0, x1, x2, x3, x4, x5) {
  return _1_Hacl_HKDF_Blake2s_128_expand_blake2s_128(x0, x1, x2, x3, x4, x5);
}

//Provides: _2_Hacl_HKDF_Blake2s_128_extract_blake2s_128
//Requires: caml_failwith
function _2_Hacl_HKDF_Blake2s_128_extract_blake2s_128 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_HKDF_Blake2s_128_extract_blake2s_128 unimplemetned');
}

//Provides: _7_Hacl_HKDF_expand_blake2b_32
//Requires: caml_failwith
function _7_Hacl_HKDF_expand_blake2b_32 (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_HKDF_expand_blake2b_32 unimplemetned');
}

//Provides: _7_Hacl_HKDF_expand_blake2b_32_byte6
//Requires: _7_Hacl_HKDF_expand_blake2b_32
function _7_Hacl_HKDF_expand_blake2b_32_byte6 (x0, x1, x2, x3, x4, x5) {
  return _7_Hacl_HKDF_expand_blake2b_32(x0, x1, x2, x3, x4, x5);
}

//Provides: _5_Hacl_HKDF_expand_blake2s_32
//Requires: caml_failwith
function _5_Hacl_HKDF_expand_blake2s_32 (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_HKDF_expand_blake2s_32 unimplemetned');
}

//Provides: _5_Hacl_HKDF_expand_blake2s_32_byte6
//Requires: _5_Hacl_HKDF_expand_blake2s_32
function _5_Hacl_HKDF_expand_blake2s_32_byte6 (x0, x1, x2, x3, x4, x5) {
  return _5_Hacl_HKDF_expand_blake2s_32(x0, x1, x2, x3, x4, x5);
}

//Provides: _3_Hacl_HKDF_expand_sha2_512
//Requires: caml_failwith
function _3_Hacl_HKDF_expand_sha2_512 (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_HKDF_expand_sha2_512 unimplemetned');
}

//Provides: _3_Hacl_HKDF_expand_sha2_512_byte6
//Requires: _3_Hacl_HKDF_expand_sha2_512
function _3_Hacl_HKDF_expand_sha2_512_byte6 (x0, x1, x2, x3, x4, x5) {
  return _3_Hacl_HKDF_expand_sha2_512(x0, x1, x2, x3, x4, x5);
}

//Provides: _8_Hacl_HKDF_extract_blake2b_32
//Requires: caml_failwith
function _8_Hacl_HKDF_extract_blake2b_32 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_HKDF_extract_blake2b_32 unimplemetned');
}

//Provides: _6_Hacl_HKDF_extract_blake2s_32
//Requires: caml_failwith
function _6_Hacl_HKDF_extract_blake2s_32 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_HKDF_extract_blake2s_32 unimplemetned');
}

//Provides: _4_Hacl_HKDF_extract_sha2_512
//Requires: caml_failwith
function _4_Hacl_HKDF_extract_sha2_512 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_HKDF_extract_sha2_512 unimplemetned');
}

//Provides: _1_Hacl_HMAC_Blake2b_256_compute_blake2b_256
//Requires: caml_failwith
function _1_Hacl_HMAC_Blake2b_256_compute_blake2b_256 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_HMAC_Blake2b_256_compute_blake2b_256 unimplemetned');
}

//Provides: _1_Hacl_HMAC_Blake2s_128_compute_blake2s_128
//Requires: caml_failwith
function _1_Hacl_HMAC_Blake2s_128_compute_blake2s_128 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_HMAC_Blake2s_128_compute_blake2s_128 unimplemetned');
}

//Provides: _7_Hacl_HMAC_DRBG_create_in
//Requires: caml_failwith
function _7_Hacl_HMAC_DRBG_create_in (x0) {
  caml_failwith('Hacl_HMAC_DRBG_create_in unimplemetned');
}

//Provides: _10_Hacl_HMAC_DRBG_generate
//Requires: caml_failwith
function _10_Hacl_HMAC_DRBG_generate (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_HMAC_DRBG_generate unimplemetned');
}

//Provides: _10_Hacl_HMAC_DRBG_generate_byte6
//Requires: _10_Hacl_HMAC_DRBG_generate
function _10_Hacl_HMAC_DRBG_generate_byte6 (x0, x1, x2, x3, x4, x5) {
  return _10_Hacl_HMAC_DRBG_generate(x0, x1, x2, x3, x4, x5);
}

//Provides: _8_Hacl_HMAC_DRBG_instantiate
//Requires: caml_failwith
function _8_Hacl_HMAC_DRBG_instantiate (x0, x1, x2, x3, x4, x5, x6, x7) {
  caml_failwith('Hacl_HMAC_DRBG_instantiate unimplemetned');
}

//Provides: _8_Hacl_HMAC_DRBG_instantiate_byte8
//Requires: _8_Hacl_HMAC_DRBG_instantiate
function _8_Hacl_HMAC_DRBG_instantiate_byte8 (x0, x1, x2, x3, x4, x5, x6, x7) {
  return _8_Hacl_HMAC_DRBG_instantiate(x0, x1, x2, x3, x4, x5, x6, x7);
}

//Provides: _6_Hacl_HMAC_DRBG_min_length
//Requires: caml_failwith
function _6_Hacl_HMAC_DRBG_min_length (x0) {
  caml_failwith('Hacl_HMAC_DRBG_min_length unimplemetned');
}

//Provides: _9_Hacl_HMAC_DRBG_reseed
//Requires: caml_failwith
function _9_Hacl_HMAC_DRBG_reseed (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_HMAC_DRBG_reseed unimplemetned');
}

//Provides: _9_Hacl_HMAC_DRBG_reseed_byte6
//Requires: _9_Hacl_HMAC_DRBG_reseed
function _9_Hacl_HMAC_DRBG_reseed_byte6 (x0, x1, x2, x3, x4, x5) {
  return _9_Hacl_HMAC_DRBG_reseed(x0, x1, x2, x3, x4, x5);
}

//Provides: _6_Hacl_HMAC_compute_blake2b_32
//Requires: caml_failwith
function _6_Hacl_HMAC_compute_blake2b_32 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_HMAC_compute_blake2b_32 unimplemetned');
}

//Provides: _5_Hacl_HMAC_compute_blake2s_32
//Requires: caml_failwith
function _5_Hacl_HMAC_compute_blake2s_32 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_HMAC_compute_blake2s_32 unimplemetned');
}

//Provides: _3_Hacl_HMAC_compute_sha2_384
//Requires: caml_failwith
function _3_Hacl_HMAC_compute_sha2_384 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_HMAC_compute_sha2_384 unimplemetned');
}

//Provides: _1_Hacl_HMAC_legacy_compute_sha1
//Requires: caml_failwith
function _1_Hacl_HMAC_legacy_compute_sha1 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_HMAC_legacy_compute_sha1 unimplemetned');
}

//Provides: _4_Hacl_HPKE_Curve51_CP128_SHA256_openBase
//Requires: caml_failwith
function _4_Hacl_HPKE_Curve51_CP128_SHA256_openBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve51_CP128_SHA256_openBase unimplemetned');
}

//Provides: _4_Hacl_HPKE_Curve51_CP128_SHA256_openBase_byte7
//Requires: _4_Hacl_HPKE_Curve51_CP128_SHA256_openBase
function _4_Hacl_HPKE_Curve51_CP128_SHA256_openBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _4_Hacl_HPKE_Curve51_CP128_SHA256_openBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _3_Hacl_HPKE_Curve51_CP128_SHA256_sealBase
//Requires: caml_failwith
function _3_Hacl_HPKE_Curve51_CP128_SHA256_sealBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve51_CP128_SHA256_sealBase unimplemetned');
}

//Provides: _3_Hacl_HPKE_Curve51_CP128_SHA256_sealBase_byte7
//Requires: _3_Hacl_HPKE_Curve51_CP128_SHA256_sealBase
function _3_Hacl_HPKE_Curve51_CP128_SHA256_sealBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _3_Hacl_HPKE_Curve51_CP128_SHA256_sealBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _1_Hacl_HPKE_Curve51_CP128_SHA256_setupBaseI
//Requires: caml_failwith
function _1_Hacl_HPKE_Curve51_CP128_SHA256_setupBaseI (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve51_CP128_SHA256_setupBaseI unimplemetned');
}

//Provides: _1_Hacl_HPKE_Curve51_CP128_SHA256_setupBaseI_byte7
//Requires: _1_Hacl_HPKE_Curve51_CP128_SHA256_setupBaseI
function _1_Hacl_HPKE_Curve51_CP128_SHA256_setupBaseI_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _1_Hacl_HPKE_Curve51_CP128_SHA256_setupBaseI(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _2_Hacl_HPKE_Curve51_CP128_SHA256_setupBaseR
//Requires: caml_failwith
function _2_Hacl_HPKE_Curve51_CP128_SHA256_setupBaseR (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_HPKE_Curve51_CP128_SHA256_setupBaseR unimplemetned');
}

//Provides: _2_Hacl_HPKE_Curve51_CP128_SHA256_setupBaseR_byte6
//Requires: _2_Hacl_HPKE_Curve51_CP128_SHA256_setupBaseR
function _2_Hacl_HPKE_Curve51_CP128_SHA256_setupBaseR_byte6 (x0, x1, x2, x3, x4, x5) {
  return _2_Hacl_HPKE_Curve51_CP128_SHA256_setupBaseR(x0, x1, x2, x3, x4, x5);
}

//Provides: _4_Hacl_HPKE_Curve51_CP128_SHA512_openBase
//Requires: caml_failwith
function _4_Hacl_HPKE_Curve51_CP128_SHA512_openBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve51_CP128_SHA512_openBase unimplemetned');
}

//Provides: _4_Hacl_HPKE_Curve51_CP128_SHA512_openBase_byte7
//Requires: _4_Hacl_HPKE_Curve51_CP128_SHA512_openBase
function _4_Hacl_HPKE_Curve51_CP128_SHA512_openBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _4_Hacl_HPKE_Curve51_CP128_SHA512_openBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _3_Hacl_HPKE_Curve51_CP128_SHA512_sealBase
//Requires: caml_failwith
function _3_Hacl_HPKE_Curve51_CP128_SHA512_sealBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve51_CP128_SHA512_sealBase unimplemetned');
}

//Provides: _3_Hacl_HPKE_Curve51_CP128_SHA512_sealBase_byte7
//Requires: _3_Hacl_HPKE_Curve51_CP128_SHA512_sealBase
function _3_Hacl_HPKE_Curve51_CP128_SHA512_sealBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _3_Hacl_HPKE_Curve51_CP128_SHA512_sealBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _1_Hacl_HPKE_Curve51_CP128_SHA512_setupBaseI
//Requires: caml_failwith
function _1_Hacl_HPKE_Curve51_CP128_SHA512_setupBaseI (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve51_CP128_SHA512_setupBaseI unimplemetned');
}

//Provides: _1_Hacl_HPKE_Curve51_CP128_SHA512_setupBaseI_byte7
//Requires: _1_Hacl_HPKE_Curve51_CP128_SHA512_setupBaseI
function _1_Hacl_HPKE_Curve51_CP128_SHA512_setupBaseI_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _1_Hacl_HPKE_Curve51_CP128_SHA512_setupBaseI(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _2_Hacl_HPKE_Curve51_CP128_SHA512_setupBaseR
//Requires: caml_failwith
function _2_Hacl_HPKE_Curve51_CP128_SHA512_setupBaseR (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_HPKE_Curve51_CP128_SHA512_setupBaseR unimplemetned');
}

//Provides: _2_Hacl_HPKE_Curve51_CP128_SHA512_setupBaseR_byte6
//Requires: _2_Hacl_HPKE_Curve51_CP128_SHA512_setupBaseR
function _2_Hacl_HPKE_Curve51_CP128_SHA512_setupBaseR_byte6 (x0, x1, x2, x3, x4, x5) {
  return _2_Hacl_HPKE_Curve51_CP128_SHA512_setupBaseR(x0, x1, x2, x3, x4, x5);
}

//Provides: _4_Hacl_HPKE_Curve51_CP256_SHA256_openBase
//Requires: caml_failwith
function _4_Hacl_HPKE_Curve51_CP256_SHA256_openBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve51_CP256_SHA256_openBase unimplemetned');
}

//Provides: _4_Hacl_HPKE_Curve51_CP256_SHA256_openBase_byte7
//Requires: _4_Hacl_HPKE_Curve51_CP256_SHA256_openBase
function _4_Hacl_HPKE_Curve51_CP256_SHA256_openBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _4_Hacl_HPKE_Curve51_CP256_SHA256_openBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _3_Hacl_HPKE_Curve51_CP256_SHA256_sealBase
//Requires: caml_failwith
function _3_Hacl_HPKE_Curve51_CP256_SHA256_sealBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve51_CP256_SHA256_sealBase unimplemetned');
}

//Provides: _3_Hacl_HPKE_Curve51_CP256_SHA256_sealBase_byte7
//Requires: _3_Hacl_HPKE_Curve51_CP256_SHA256_sealBase
function _3_Hacl_HPKE_Curve51_CP256_SHA256_sealBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _3_Hacl_HPKE_Curve51_CP256_SHA256_sealBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _1_Hacl_HPKE_Curve51_CP256_SHA256_setupBaseI
//Requires: caml_failwith
function _1_Hacl_HPKE_Curve51_CP256_SHA256_setupBaseI (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve51_CP256_SHA256_setupBaseI unimplemetned');
}

//Provides: _1_Hacl_HPKE_Curve51_CP256_SHA256_setupBaseI_byte7
//Requires: _1_Hacl_HPKE_Curve51_CP256_SHA256_setupBaseI
function _1_Hacl_HPKE_Curve51_CP256_SHA256_setupBaseI_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _1_Hacl_HPKE_Curve51_CP256_SHA256_setupBaseI(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _2_Hacl_HPKE_Curve51_CP256_SHA256_setupBaseR
//Requires: caml_failwith
function _2_Hacl_HPKE_Curve51_CP256_SHA256_setupBaseR (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_HPKE_Curve51_CP256_SHA256_setupBaseR unimplemetned');
}

//Provides: _2_Hacl_HPKE_Curve51_CP256_SHA256_setupBaseR_byte6
//Requires: _2_Hacl_HPKE_Curve51_CP256_SHA256_setupBaseR
function _2_Hacl_HPKE_Curve51_CP256_SHA256_setupBaseR_byte6 (x0, x1, x2, x3, x4, x5) {
  return _2_Hacl_HPKE_Curve51_CP256_SHA256_setupBaseR(x0, x1, x2, x3, x4, x5);
}

//Provides: _4_Hacl_HPKE_Curve51_CP256_SHA512_openBase
//Requires: caml_failwith
function _4_Hacl_HPKE_Curve51_CP256_SHA512_openBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve51_CP256_SHA512_openBase unimplemetned');
}

//Provides: _4_Hacl_HPKE_Curve51_CP256_SHA512_openBase_byte7
//Requires: _4_Hacl_HPKE_Curve51_CP256_SHA512_openBase
function _4_Hacl_HPKE_Curve51_CP256_SHA512_openBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _4_Hacl_HPKE_Curve51_CP256_SHA512_openBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _3_Hacl_HPKE_Curve51_CP256_SHA512_sealBase
//Requires: caml_failwith
function _3_Hacl_HPKE_Curve51_CP256_SHA512_sealBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve51_CP256_SHA512_sealBase unimplemetned');
}

//Provides: _3_Hacl_HPKE_Curve51_CP256_SHA512_sealBase_byte7
//Requires: _3_Hacl_HPKE_Curve51_CP256_SHA512_sealBase
function _3_Hacl_HPKE_Curve51_CP256_SHA512_sealBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _3_Hacl_HPKE_Curve51_CP256_SHA512_sealBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _1_Hacl_HPKE_Curve51_CP256_SHA512_setupBaseI
//Requires: caml_failwith
function _1_Hacl_HPKE_Curve51_CP256_SHA512_setupBaseI (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve51_CP256_SHA512_setupBaseI unimplemetned');
}

//Provides: _1_Hacl_HPKE_Curve51_CP256_SHA512_setupBaseI_byte7
//Requires: _1_Hacl_HPKE_Curve51_CP256_SHA512_setupBaseI
function _1_Hacl_HPKE_Curve51_CP256_SHA512_setupBaseI_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _1_Hacl_HPKE_Curve51_CP256_SHA512_setupBaseI(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _2_Hacl_HPKE_Curve51_CP256_SHA512_setupBaseR
//Requires: caml_failwith
function _2_Hacl_HPKE_Curve51_CP256_SHA512_setupBaseR (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_HPKE_Curve51_CP256_SHA512_setupBaseR unimplemetned');
}

//Provides: _2_Hacl_HPKE_Curve51_CP256_SHA512_setupBaseR_byte6
//Requires: _2_Hacl_HPKE_Curve51_CP256_SHA512_setupBaseR
function _2_Hacl_HPKE_Curve51_CP256_SHA512_setupBaseR_byte6 (x0, x1, x2, x3, x4, x5) {
  return _2_Hacl_HPKE_Curve51_CP256_SHA512_setupBaseR(x0, x1, x2, x3, x4, x5);
}

//Provides: _4_Hacl_HPKE_Curve51_CP32_SHA256_openBase
//Requires: caml_failwith
function _4_Hacl_HPKE_Curve51_CP32_SHA256_openBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve51_CP32_SHA256_openBase unimplemetned');
}

//Provides: _4_Hacl_HPKE_Curve51_CP32_SHA256_openBase_byte7
//Requires: _4_Hacl_HPKE_Curve51_CP32_SHA256_openBase
function _4_Hacl_HPKE_Curve51_CP32_SHA256_openBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _4_Hacl_HPKE_Curve51_CP32_SHA256_openBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _3_Hacl_HPKE_Curve51_CP32_SHA256_sealBase
//Requires: caml_failwith
function _3_Hacl_HPKE_Curve51_CP32_SHA256_sealBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve51_CP32_SHA256_sealBase unimplemetned');
}

//Provides: _3_Hacl_HPKE_Curve51_CP32_SHA256_sealBase_byte7
//Requires: _3_Hacl_HPKE_Curve51_CP32_SHA256_sealBase
function _3_Hacl_HPKE_Curve51_CP32_SHA256_sealBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _3_Hacl_HPKE_Curve51_CP32_SHA256_sealBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _1_Hacl_HPKE_Curve51_CP32_SHA256_setupBaseI
//Requires: caml_failwith
function _1_Hacl_HPKE_Curve51_CP32_SHA256_setupBaseI (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve51_CP32_SHA256_setupBaseI unimplemetned');
}

//Provides: _1_Hacl_HPKE_Curve51_CP32_SHA256_setupBaseI_byte7
//Requires: _1_Hacl_HPKE_Curve51_CP32_SHA256_setupBaseI
function _1_Hacl_HPKE_Curve51_CP32_SHA256_setupBaseI_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _1_Hacl_HPKE_Curve51_CP32_SHA256_setupBaseI(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _2_Hacl_HPKE_Curve51_CP32_SHA256_setupBaseR
//Requires: caml_failwith
function _2_Hacl_HPKE_Curve51_CP32_SHA256_setupBaseR (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_HPKE_Curve51_CP32_SHA256_setupBaseR unimplemetned');
}

//Provides: _2_Hacl_HPKE_Curve51_CP32_SHA256_setupBaseR_byte6
//Requires: _2_Hacl_HPKE_Curve51_CP32_SHA256_setupBaseR
function _2_Hacl_HPKE_Curve51_CP32_SHA256_setupBaseR_byte6 (x0, x1, x2, x3, x4, x5) {
  return _2_Hacl_HPKE_Curve51_CP32_SHA256_setupBaseR(x0, x1, x2, x3, x4, x5);
}

//Provides: _4_Hacl_HPKE_Curve51_CP32_SHA512_openBase
//Requires: caml_failwith
function _4_Hacl_HPKE_Curve51_CP32_SHA512_openBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve51_CP32_SHA512_openBase unimplemetned');
}

//Provides: _4_Hacl_HPKE_Curve51_CP32_SHA512_openBase_byte7
//Requires: _4_Hacl_HPKE_Curve51_CP32_SHA512_openBase
function _4_Hacl_HPKE_Curve51_CP32_SHA512_openBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _4_Hacl_HPKE_Curve51_CP32_SHA512_openBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _3_Hacl_HPKE_Curve51_CP32_SHA512_sealBase
//Requires: caml_failwith
function _3_Hacl_HPKE_Curve51_CP32_SHA512_sealBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve51_CP32_SHA512_sealBase unimplemetned');
}

//Provides: _3_Hacl_HPKE_Curve51_CP32_SHA512_sealBase_byte7
//Requires: _3_Hacl_HPKE_Curve51_CP32_SHA512_sealBase
function _3_Hacl_HPKE_Curve51_CP32_SHA512_sealBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _3_Hacl_HPKE_Curve51_CP32_SHA512_sealBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _1_Hacl_HPKE_Curve51_CP32_SHA512_setupBaseI
//Requires: caml_failwith
function _1_Hacl_HPKE_Curve51_CP32_SHA512_setupBaseI (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve51_CP32_SHA512_setupBaseI unimplemetned');
}

//Provides: _1_Hacl_HPKE_Curve51_CP32_SHA512_setupBaseI_byte7
//Requires: _1_Hacl_HPKE_Curve51_CP32_SHA512_setupBaseI
function _1_Hacl_HPKE_Curve51_CP32_SHA512_setupBaseI_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _1_Hacl_HPKE_Curve51_CP32_SHA512_setupBaseI(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _2_Hacl_HPKE_Curve51_CP32_SHA512_setupBaseR
//Requires: caml_failwith
function _2_Hacl_HPKE_Curve51_CP32_SHA512_setupBaseR (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_HPKE_Curve51_CP32_SHA512_setupBaseR unimplemetned');
}

//Provides: _2_Hacl_HPKE_Curve51_CP32_SHA512_setupBaseR_byte6
//Requires: _2_Hacl_HPKE_Curve51_CP32_SHA512_setupBaseR
function _2_Hacl_HPKE_Curve51_CP32_SHA512_setupBaseR_byte6 (x0, x1, x2, x3, x4, x5) {
  return _2_Hacl_HPKE_Curve51_CP32_SHA512_setupBaseR(x0, x1, x2, x3, x4, x5);
}

//Provides: _4_Hacl_HPKE_Curve64_CP128_SHA256_openBase
//Requires: caml_failwith
function _4_Hacl_HPKE_Curve64_CP128_SHA256_openBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve64_CP128_SHA256_openBase unimplemetned');
}

//Provides: _4_Hacl_HPKE_Curve64_CP128_SHA256_openBase_byte7
//Requires: _4_Hacl_HPKE_Curve64_CP128_SHA256_openBase
function _4_Hacl_HPKE_Curve64_CP128_SHA256_openBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _4_Hacl_HPKE_Curve64_CP128_SHA256_openBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _3_Hacl_HPKE_Curve64_CP128_SHA256_sealBase
//Requires: caml_failwith
function _3_Hacl_HPKE_Curve64_CP128_SHA256_sealBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve64_CP128_SHA256_sealBase unimplemetned');
}

//Provides: _3_Hacl_HPKE_Curve64_CP128_SHA256_sealBase_byte7
//Requires: _3_Hacl_HPKE_Curve64_CP128_SHA256_sealBase
function _3_Hacl_HPKE_Curve64_CP128_SHA256_sealBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _3_Hacl_HPKE_Curve64_CP128_SHA256_sealBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _1_Hacl_HPKE_Curve64_CP128_SHA256_setupBaseI
//Requires: caml_failwith
function _1_Hacl_HPKE_Curve64_CP128_SHA256_setupBaseI (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve64_CP128_SHA256_setupBaseI unimplemetned');
}

//Provides: _1_Hacl_HPKE_Curve64_CP128_SHA256_setupBaseI_byte7
//Requires: _1_Hacl_HPKE_Curve64_CP128_SHA256_setupBaseI
function _1_Hacl_HPKE_Curve64_CP128_SHA256_setupBaseI_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _1_Hacl_HPKE_Curve64_CP128_SHA256_setupBaseI(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _2_Hacl_HPKE_Curve64_CP128_SHA256_setupBaseR
//Requires: caml_failwith
function _2_Hacl_HPKE_Curve64_CP128_SHA256_setupBaseR (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_HPKE_Curve64_CP128_SHA256_setupBaseR unimplemetned');
}

//Provides: _2_Hacl_HPKE_Curve64_CP128_SHA256_setupBaseR_byte6
//Requires: _2_Hacl_HPKE_Curve64_CP128_SHA256_setupBaseR
function _2_Hacl_HPKE_Curve64_CP128_SHA256_setupBaseR_byte6 (x0, x1, x2, x3, x4, x5) {
  return _2_Hacl_HPKE_Curve64_CP128_SHA256_setupBaseR(x0, x1, x2, x3, x4, x5);
}

//Provides: _4_Hacl_HPKE_Curve64_CP128_SHA512_openBase
//Requires: caml_failwith
function _4_Hacl_HPKE_Curve64_CP128_SHA512_openBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve64_CP128_SHA512_openBase unimplemetned');
}

//Provides: _4_Hacl_HPKE_Curve64_CP128_SHA512_openBase_byte7
//Requires: _4_Hacl_HPKE_Curve64_CP128_SHA512_openBase
function _4_Hacl_HPKE_Curve64_CP128_SHA512_openBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _4_Hacl_HPKE_Curve64_CP128_SHA512_openBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _3_Hacl_HPKE_Curve64_CP128_SHA512_sealBase
//Requires: caml_failwith
function _3_Hacl_HPKE_Curve64_CP128_SHA512_sealBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve64_CP128_SHA512_sealBase unimplemetned');
}

//Provides: _3_Hacl_HPKE_Curve64_CP128_SHA512_sealBase_byte7
//Requires: _3_Hacl_HPKE_Curve64_CP128_SHA512_sealBase
function _3_Hacl_HPKE_Curve64_CP128_SHA512_sealBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _3_Hacl_HPKE_Curve64_CP128_SHA512_sealBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _1_Hacl_HPKE_Curve64_CP128_SHA512_setupBaseI
//Requires: caml_failwith
function _1_Hacl_HPKE_Curve64_CP128_SHA512_setupBaseI (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve64_CP128_SHA512_setupBaseI unimplemetned');
}

//Provides: _1_Hacl_HPKE_Curve64_CP128_SHA512_setupBaseI_byte7
//Requires: _1_Hacl_HPKE_Curve64_CP128_SHA512_setupBaseI
function _1_Hacl_HPKE_Curve64_CP128_SHA512_setupBaseI_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _1_Hacl_HPKE_Curve64_CP128_SHA512_setupBaseI(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _2_Hacl_HPKE_Curve64_CP128_SHA512_setupBaseR
//Requires: caml_failwith
function _2_Hacl_HPKE_Curve64_CP128_SHA512_setupBaseR (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_HPKE_Curve64_CP128_SHA512_setupBaseR unimplemetned');
}

//Provides: _2_Hacl_HPKE_Curve64_CP128_SHA512_setupBaseR_byte6
//Requires: _2_Hacl_HPKE_Curve64_CP128_SHA512_setupBaseR
function _2_Hacl_HPKE_Curve64_CP128_SHA512_setupBaseR_byte6 (x0, x1, x2, x3, x4, x5) {
  return _2_Hacl_HPKE_Curve64_CP128_SHA512_setupBaseR(x0, x1, x2, x3, x4, x5);
}

//Provides: _4_Hacl_HPKE_Curve64_CP256_SHA256_openBase
//Requires: caml_failwith
function _4_Hacl_HPKE_Curve64_CP256_SHA256_openBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve64_CP256_SHA256_openBase unimplemetned');
}

//Provides: _4_Hacl_HPKE_Curve64_CP256_SHA256_openBase_byte7
//Requires: _4_Hacl_HPKE_Curve64_CP256_SHA256_openBase
function _4_Hacl_HPKE_Curve64_CP256_SHA256_openBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _4_Hacl_HPKE_Curve64_CP256_SHA256_openBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _3_Hacl_HPKE_Curve64_CP256_SHA256_sealBase
//Requires: caml_failwith
function _3_Hacl_HPKE_Curve64_CP256_SHA256_sealBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve64_CP256_SHA256_sealBase unimplemetned');
}

//Provides: _3_Hacl_HPKE_Curve64_CP256_SHA256_sealBase_byte7
//Requires: _3_Hacl_HPKE_Curve64_CP256_SHA256_sealBase
function _3_Hacl_HPKE_Curve64_CP256_SHA256_sealBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _3_Hacl_HPKE_Curve64_CP256_SHA256_sealBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _1_Hacl_HPKE_Curve64_CP256_SHA256_setupBaseI
//Requires: caml_failwith
function _1_Hacl_HPKE_Curve64_CP256_SHA256_setupBaseI (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve64_CP256_SHA256_setupBaseI unimplemetned');
}

//Provides: _1_Hacl_HPKE_Curve64_CP256_SHA256_setupBaseI_byte7
//Requires: _1_Hacl_HPKE_Curve64_CP256_SHA256_setupBaseI
function _1_Hacl_HPKE_Curve64_CP256_SHA256_setupBaseI_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _1_Hacl_HPKE_Curve64_CP256_SHA256_setupBaseI(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _2_Hacl_HPKE_Curve64_CP256_SHA256_setupBaseR
//Requires: caml_failwith
function _2_Hacl_HPKE_Curve64_CP256_SHA256_setupBaseR (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_HPKE_Curve64_CP256_SHA256_setupBaseR unimplemetned');
}

//Provides: _2_Hacl_HPKE_Curve64_CP256_SHA256_setupBaseR_byte6
//Requires: _2_Hacl_HPKE_Curve64_CP256_SHA256_setupBaseR
function _2_Hacl_HPKE_Curve64_CP256_SHA256_setupBaseR_byte6 (x0, x1, x2, x3, x4, x5) {
  return _2_Hacl_HPKE_Curve64_CP256_SHA256_setupBaseR(x0, x1, x2, x3, x4, x5);
}

//Provides: _4_Hacl_HPKE_Curve64_CP256_SHA512_openBase
//Requires: caml_failwith
function _4_Hacl_HPKE_Curve64_CP256_SHA512_openBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve64_CP256_SHA512_openBase unimplemetned');
}

//Provides: _4_Hacl_HPKE_Curve64_CP256_SHA512_openBase_byte7
//Requires: _4_Hacl_HPKE_Curve64_CP256_SHA512_openBase
function _4_Hacl_HPKE_Curve64_CP256_SHA512_openBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _4_Hacl_HPKE_Curve64_CP256_SHA512_openBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _3_Hacl_HPKE_Curve64_CP256_SHA512_sealBase
//Requires: caml_failwith
function _3_Hacl_HPKE_Curve64_CP256_SHA512_sealBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve64_CP256_SHA512_sealBase unimplemetned');
}

//Provides: _3_Hacl_HPKE_Curve64_CP256_SHA512_sealBase_byte7
//Requires: _3_Hacl_HPKE_Curve64_CP256_SHA512_sealBase
function _3_Hacl_HPKE_Curve64_CP256_SHA512_sealBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _3_Hacl_HPKE_Curve64_CP256_SHA512_sealBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _1_Hacl_HPKE_Curve64_CP256_SHA512_setupBaseI
//Requires: caml_failwith
function _1_Hacl_HPKE_Curve64_CP256_SHA512_setupBaseI (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve64_CP256_SHA512_setupBaseI unimplemetned');
}

//Provides: _1_Hacl_HPKE_Curve64_CP256_SHA512_setupBaseI_byte7
//Requires: _1_Hacl_HPKE_Curve64_CP256_SHA512_setupBaseI
function _1_Hacl_HPKE_Curve64_CP256_SHA512_setupBaseI_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _1_Hacl_HPKE_Curve64_CP256_SHA512_setupBaseI(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _2_Hacl_HPKE_Curve64_CP256_SHA512_setupBaseR
//Requires: caml_failwith
function _2_Hacl_HPKE_Curve64_CP256_SHA512_setupBaseR (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_HPKE_Curve64_CP256_SHA512_setupBaseR unimplemetned');
}

//Provides: _2_Hacl_HPKE_Curve64_CP256_SHA512_setupBaseR_byte6
//Requires: _2_Hacl_HPKE_Curve64_CP256_SHA512_setupBaseR
function _2_Hacl_HPKE_Curve64_CP256_SHA512_setupBaseR_byte6 (x0, x1, x2, x3, x4, x5) {
  return _2_Hacl_HPKE_Curve64_CP256_SHA512_setupBaseR(x0, x1, x2, x3, x4, x5);
}

//Provides: _4_Hacl_HPKE_Curve64_CP32_SHA256_openBase
//Requires: caml_failwith
function _4_Hacl_HPKE_Curve64_CP32_SHA256_openBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve64_CP32_SHA256_openBase unimplemetned');
}

//Provides: _4_Hacl_HPKE_Curve64_CP32_SHA256_openBase_byte7
//Requires: _4_Hacl_HPKE_Curve64_CP32_SHA256_openBase
function _4_Hacl_HPKE_Curve64_CP32_SHA256_openBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _4_Hacl_HPKE_Curve64_CP32_SHA256_openBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _3_Hacl_HPKE_Curve64_CP32_SHA256_sealBase
//Requires: caml_failwith
function _3_Hacl_HPKE_Curve64_CP32_SHA256_sealBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve64_CP32_SHA256_sealBase unimplemetned');
}

//Provides: _3_Hacl_HPKE_Curve64_CP32_SHA256_sealBase_byte7
//Requires: _3_Hacl_HPKE_Curve64_CP32_SHA256_sealBase
function _3_Hacl_HPKE_Curve64_CP32_SHA256_sealBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _3_Hacl_HPKE_Curve64_CP32_SHA256_sealBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _1_Hacl_HPKE_Curve64_CP32_SHA256_setupBaseI
//Requires: caml_failwith
function _1_Hacl_HPKE_Curve64_CP32_SHA256_setupBaseI (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve64_CP32_SHA256_setupBaseI unimplemetned');
}

//Provides: _1_Hacl_HPKE_Curve64_CP32_SHA256_setupBaseI_byte7
//Requires: _1_Hacl_HPKE_Curve64_CP32_SHA256_setupBaseI
function _1_Hacl_HPKE_Curve64_CP32_SHA256_setupBaseI_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _1_Hacl_HPKE_Curve64_CP32_SHA256_setupBaseI(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _2_Hacl_HPKE_Curve64_CP32_SHA256_setupBaseR
//Requires: caml_failwith
function _2_Hacl_HPKE_Curve64_CP32_SHA256_setupBaseR (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_HPKE_Curve64_CP32_SHA256_setupBaseR unimplemetned');
}

//Provides: _2_Hacl_HPKE_Curve64_CP32_SHA256_setupBaseR_byte6
//Requires: _2_Hacl_HPKE_Curve64_CP32_SHA256_setupBaseR
function _2_Hacl_HPKE_Curve64_CP32_SHA256_setupBaseR_byte6 (x0, x1, x2, x3, x4, x5) {
  return _2_Hacl_HPKE_Curve64_CP32_SHA256_setupBaseR(x0, x1, x2, x3, x4, x5);
}

//Provides: _4_Hacl_HPKE_Curve64_CP32_SHA512_openBase
//Requires: caml_failwith
function _4_Hacl_HPKE_Curve64_CP32_SHA512_openBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve64_CP32_SHA512_openBase unimplemetned');
}

//Provides: _4_Hacl_HPKE_Curve64_CP32_SHA512_openBase_byte7
//Requires: _4_Hacl_HPKE_Curve64_CP32_SHA512_openBase
function _4_Hacl_HPKE_Curve64_CP32_SHA512_openBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _4_Hacl_HPKE_Curve64_CP32_SHA512_openBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _3_Hacl_HPKE_Curve64_CP32_SHA512_sealBase
//Requires: caml_failwith
function _3_Hacl_HPKE_Curve64_CP32_SHA512_sealBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve64_CP32_SHA512_sealBase unimplemetned');
}

//Provides: _3_Hacl_HPKE_Curve64_CP32_SHA512_sealBase_byte7
//Requires: _3_Hacl_HPKE_Curve64_CP32_SHA512_sealBase
function _3_Hacl_HPKE_Curve64_CP32_SHA512_sealBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _3_Hacl_HPKE_Curve64_CP32_SHA512_sealBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _1_Hacl_HPKE_Curve64_CP32_SHA512_setupBaseI
//Requires: caml_failwith
function _1_Hacl_HPKE_Curve64_CP32_SHA512_setupBaseI (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_Curve64_CP32_SHA512_setupBaseI unimplemetned');
}

//Provides: _1_Hacl_HPKE_Curve64_CP32_SHA512_setupBaseI_byte7
//Requires: _1_Hacl_HPKE_Curve64_CP32_SHA512_setupBaseI
function _1_Hacl_HPKE_Curve64_CP32_SHA512_setupBaseI_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _1_Hacl_HPKE_Curve64_CP32_SHA512_setupBaseI(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _2_Hacl_HPKE_Curve64_CP32_SHA512_setupBaseR
//Requires: caml_failwith
function _2_Hacl_HPKE_Curve64_CP32_SHA512_setupBaseR (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_HPKE_Curve64_CP32_SHA512_setupBaseR unimplemetned');
}

//Provides: _2_Hacl_HPKE_Curve64_CP32_SHA512_setupBaseR_byte6
//Requires: _2_Hacl_HPKE_Curve64_CP32_SHA512_setupBaseR
function _2_Hacl_HPKE_Curve64_CP32_SHA512_setupBaseR_byte6 (x0, x1, x2, x3, x4, x5) {
  return _2_Hacl_HPKE_Curve64_CP32_SHA512_setupBaseR(x0, x1, x2, x3, x4, x5);
}

//Provides: _4_Hacl_HPKE_P256_CP128_SHA256_openBase
//Requires: caml_failwith
function _4_Hacl_HPKE_P256_CP128_SHA256_openBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_P256_CP128_SHA256_openBase unimplemetned');
}

//Provides: _4_Hacl_HPKE_P256_CP128_SHA256_openBase_byte7
//Requires: _4_Hacl_HPKE_P256_CP128_SHA256_openBase
function _4_Hacl_HPKE_P256_CP128_SHA256_openBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _4_Hacl_HPKE_P256_CP128_SHA256_openBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _3_Hacl_HPKE_P256_CP128_SHA256_sealBase
//Requires: caml_failwith
function _3_Hacl_HPKE_P256_CP128_SHA256_sealBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_P256_CP128_SHA256_sealBase unimplemetned');
}

//Provides: _3_Hacl_HPKE_P256_CP128_SHA256_sealBase_byte7
//Requires: _3_Hacl_HPKE_P256_CP128_SHA256_sealBase
function _3_Hacl_HPKE_P256_CP128_SHA256_sealBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _3_Hacl_HPKE_P256_CP128_SHA256_sealBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _1_Hacl_HPKE_P256_CP128_SHA256_setupBaseI
//Requires: caml_failwith
function _1_Hacl_HPKE_P256_CP128_SHA256_setupBaseI (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_P256_CP128_SHA256_setupBaseI unimplemetned');
}

//Provides: _1_Hacl_HPKE_P256_CP128_SHA256_setupBaseI_byte7
//Requires: _1_Hacl_HPKE_P256_CP128_SHA256_setupBaseI
function _1_Hacl_HPKE_P256_CP128_SHA256_setupBaseI_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _1_Hacl_HPKE_P256_CP128_SHA256_setupBaseI(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _2_Hacl_HPKE_P256_CP128_SHA256_setupBaseR
//Requires: caml_failwith
function _2_Hacl_HPKE_P256_CP128_SHA256_setupBaseR (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_HPKE_P256_CP128_SHA256_setupBaseR unimplemetned');
}

//Provides: _2_Hacl_HPKE_P256_CP128_SHA256_setupBaseR_byte6
//Requires: _2_Hacl_HPKE_P256_CP128_SHA256_setupBaseR
function _2_Hacl_HPKE_P256_CP128_SHA256_setupBaseR_byte6 (x0, x1, x2, x3, x4, x5) {
  return _2_Hacl_HPKE_P256_CP128_SHA256_setupBaseR(x0, x1, x2, x3, x4, x5);
}

//Provides: _4_Hacl_HPKE_P256_CP256_SHA256_openBase
//Requires: caml_failwith
function _4_Hacl_HPKE_P256_CP256_SHA256_openBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_P256_CP256_SHA256_openBase unimplemetned');
}

//Provides: _4_Hacl_HPKE_P256_CP256_SHA256_openBase_byte7
//Requires: _4_Hacl_HPKE_P256_CP256_SHA256_openBase
function _4_Hacl_HPKE_P256_CP256_SHA256_openBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _4_Hacl_HPKE_P256_CP256_SHA256_openBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _3_Hacl_HPKE_P256_CP256_SHA256_sealBase
//Requires: caml_failwith
function _3_Hacl_HPKE_P256_CP256_SHA256_sealBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_P256_CP256_SHA256_sealBase unimplemetned');
}

//Provides: _3_Hacl_HPKE_P256_CP256_SHA256_sealBase_byte7
//Requires: _3_Hacl_HPKE_P256_CP256_SHA256_sealBase
function _3_Hacl_HPKE_P256_CP256_SHA256_sealBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _3_Hacl_HPKE_P256_CP256_SHA256_sealBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _1_Hacl_HPKE_P256_CP256_SHA256_setupBaseI
//Requires: caml_failwith
function _1_Hacl_HPKE_P256_CP256_SHA256_setupBaseI (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_P256_CP256_SHA256_setupBaseI unimplemetned');
}

//Provides: _1_Hacl_HPKE_P256_CP256_SHA256_setupBaseI_byte7
//Requires: _1_Hacl_HPKE_P256_CP256_SHA256_setupBaseI
function _1_Hacl_HPKE_P256_CP256_SHA256_setupBaseI_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _1_Hacl_HPKE_P256_CP256_SHA256_setupBaseI(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _2_Hacl_HPKE_P256_CP256_SHA256_setupBaseR
//Requires: caml_failwith
function _2_Hacl_HPKE_P256_CP256_SHA256_setupBaseR (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_HPKE_P256_CP256_SHA256_setupBaseR unimplemetned');
}

//Provides: _2_Hacl_HPKE_P256_CP256_SHA256_setupBaseR_byte6
//Requires: _2_Hacl_HPKE_P256_CP256_SHA256_setupBaseR
function _2_Hacl_HPKE_P256_CP256_SHA256_setupBaseR_byte6 (x0, x1, x2, x3, x4, x5) {
  return _2_Hacl_HPKE_P256_CP256_SHA256_setupBaseR(x0, x1, x2, x3, x4, x5);
}

//Provides: _4_Hacl_HPKE_P256_CP32_SHA256_openBase
//Requires: caml_failwith
function _4_Hacl_HPKE_P256_CP32_SHA256_openBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_P256_CP32_SHA256_openBase unimplemetned');
}

//Provides: _4_Hacl_HPKE_P256_CP32_SHA256_openBase_byte7
//Requires: _4_Hacl_HPKE_P256_CP32_SHA256_openBase
function _4_Hacl_HPKE_P256_CP32_SHA256_openBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _4_Hacl_HPKE_P256_CP32_SHA256_openBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _3_Hacl_HPKE_P256_CP32_SHA256_sealBase
//Requires: caml_failwith
function _3_Hacl_HPKE_P256_CP32_SHA256_sealBase (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_P256_CP32_SHA256_sealBase unimplemetned');
}

//Provides: _3_Hacl_HPKE_P256_CP32_SHA256_sealBase_byte7
//Requires: _3_Hacl_HPKE_P256_CP32_SHA256_sealBase
function _3_Hacl_HPKE_P256_CP32_SHA256_sealBase_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _3_Hacl_HPKE_P256_CP32_SHA256_sealBase(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _1_Hacl_HPKE_P256_CP32_SHA256_setupBaseI
//Requires: caml_failwith
function _1_Hacl_HPKE_P256_CP32_SHA256_setupBaseI (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_HPKE_P256_CP32_SHA256_setupBaseI unimplemetned');
}

//Provides: _1_Hacl_HPKE_P256_CP32_SHA256_setupBaseI_byte7
//Requires: _1_Hacl_HPKE_P256_CP32_SHA256_setupBaseI
function _1_Hacl_HPKE_P256_CP32_SHA256_setupBaseI_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _1_Hacl_HPKE_P256_CP32_SHA256_setupBaseI(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _2_Hacl_HPKE_P256_CP32_SHA256_setupBaseR
//Requires: caml_failwith
function _2_Hacl_HPKE_P256_CP32_SHA256_setupBaseR (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_HPKE_P256_CP32_SHA256_setupBaseR unimplemetned');
}

//Provides: _2_Hacl_HPKE_P256_CP32_SHA256_setupBaseR_byte6
//Requires: _2_Hacl_HPKE_P256_CP32_SHA256_setupBaseR
function _2_Hacl_HPKE_P256_CP32_SHA256_setupBaseR_byte6 (x0, x1, x2, x3, x4, x5) {
  return _2_Hacl_HPKE_P256_CP32_SHA256_setupBaseR(x0, x1, x2, x3, x4, x5);
}

//Provides: _6_Hacl_Hash_Blake2_hash_blake2b_32
//Requires: caml_failwith
function _6_Hacl_Hash_Blake2_hash_blake2b_32 (x0, x1, x2) {
  caml_failwith('Hacl_Hash_Blake2_hash_blake2b_32 unimplemetned');
}

//Provides: _5_Hacl_Hash_Blake2_hash_blake2s_32
//Requires: caml_failwith
function _5_Hacl_Hash_Blake2_hash_blake2s_32 (x0, x1, x2) {
  caml_failwith('Hacl_Hash_Blake2_hash_blake2s_32 unimplemetned');
}

//Provides: _4_Hacl_Hash_Blake2_update_last_blake2s_32
//Requires: caml_failwith
function _4_Hacl_Hash_Blake2_update_last_blake2s_32 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Hash_Blake2_update_last_blake2s_32 unimplemetned');
}

//Provides: _3_Hacl_Hash_Blake2_update_multi_blake2s_32
//Requires: caml_failwith
function _3_Hacl_Hash_Blake2_update_multi_blake2s_32 (x0, x1, x2, x3) {
  caml_failwith('Hacl_Hash_Blake2_update_multi_blake2s_32 unimplemetned');
}

//Provides: _1_Hacl_Hash_Blake2b_256_hash_blake2b_256
//Requires: caml_failwith
function _1_Hacl_Hash_Blake2b_256_hash_blake2b_256 (x0, x1, x2) {
  caml_failwith('Hacl_Hash_Blake2b_256_hash_blake2b_256 unimplemetned');
}

//Provides: _1_Hacl_Hash_Blake2s_128_hash_blake2s_128
//Requires: caml_failwith
function _1_Hacl_Hash_Blake2s_128_hash_blake2s_128 (x0, x1, x2) {
  caml_failwith('Hacl_Hash_Blake2s_128_hash_blake2s_128 unimplemetned');
}

//Provides: _2_Hacl_Hash_Core_Blake2_finish_blake2s_32
//Requires: caml_failwith
function _2_Hacl_Hash_Core_Blake2_finish_blake2s_32 (x0, x1, x2) {
  caml_failwith('Hacl_Hash_Core_Blake2_finish_blake2s_32 unimplemetned');
}

//Provides: _1_Hacl_Hash_Core_Blake2_update_blake2s_32
//Requires: caml_failwith
function _1_Hacl_Hash_Core_Blake2_update_blake2s_32 (x0, x1, x2) {
  caml_failwith('Hacl_Hash_Core_Blake2_update_blake2s_32 unimplemetned');
}

//Provides: _3_Hacl_Hash_Core_MD5_legacy_finish
//Requires: caml_failwith
function _3_Hacl_Hash_Core_MD5_legacy_finish (x0, x1) {
  caml_failwith('Hacl_Hash_Core_MD5_legacy_finish unimplemetned');
}

//Provides: _1_Hacl_Hash_Core_MD5_legacy_init
//Requires: caml_failwith
function _1_Hacl_Hash_Core_MD5_legacy_init (x0) {
  caml_failwith('Hacl_Hash_Core_MD5_legacy_init unimplemetned');
}

//Provides: _2_Hacl_Hash_Core_MD5_legacy_update
//Requires: caml_failwith
function _2_Hacl_Hash_Core_MD5_legacy_update (x0, x1) {
  caml_failwith('Hacl_Hash_Core_MD5_legacy_update unimplemetned');
}

//Provides: _3_Hacl_Hash_Core_SHA1_legacy_finish
//Requires: caml_failwith
function _3_Hacl_Hash_Core_SHA1_legacy_finish (x0, x1) {
  caml_failwith('Hacl_Hash_Core_SHA1_legacy_finish unimplemetned');
}

//Provides: _1_Hacl_Hash_Core_SHA1_legacy_init
//Requires: caml_failwith
function _1_Hacl_Hash_Core_SHA1_legacy_init (x0) {
  caml_failwith('Hacl_Hash_Core_SHA1_legacy_init unimplemetned');
}

//Provides: _2_Hacl_Hash_Core_SHA1_legacy_update
//Requires: caml_failwith
function _2_Hacl_Hash_Core_SHA1_legacy_update (x0, x1) {
  caml_failwith('Hacl_Hash_Core_SHA1_legacy_update unimplemetned');
}

//Provides: _8_Hacl_Hash_Core_SHA2_finish_224
//Requires: caml_failwith
function _8_Hacl_Hash_Core_SHA2_finish_224 (x0, x1) {
  caml_failwith('Hacl_Hash_Core_SHA2_finish_224 unimplemetned');
}

//Provides: _9_Hacl_Hash_Core_SHA2_finish_256
//Requires: caml_failwith
function _9_Hacl_Hash_Core_SHA2_finish_256 (x0, x1) {
  caml_failwith('Hacl_Hash_Core_SHA2_finish_256 unimplemetned');
}

//Provides: _10_Hacl_Hash_Core_SHA2_finish_384
//Requires: caml_failwith
function _10_Hacl_Hash_Core_SHA2_finish_384 (x0, x1) {
  caml_failwith('Hacl_Hash_Core_SHA2_finish_384 unimplemetned');
}

//Provides: _11_Hacl_Hash_Core_SHA2_finish_512
//Requires: caml_failwith
function _11_Hacl_Hash_Core_SHA2_finish_512 (x0, x1) {
  caml_failwith('Hacl_Hash_Core_SHA2_finish_512 unimplemetned');
}

//Provides: _1_Hacl_Hash_Core_SHA2_init_224
//Requires: caml_failwith
function _1_Hacl_Hash_Core_SHA2_init_224 (x0) {
  caml_failwith('Hacl_Hash_Core_SHA2_init_224 unimplemetned');
}

//Provides: _2_Hacl_Hash_Core_SHA2_init_256
//Requires: caml_failwith
function _2_Hacl_Hash_Core_SHA2_init_256 (x0) {
  caml_failwith('Hacl_Hash_Core_SHA2_init_256 unimplemetned');
}

//Provides: _3_Hacl_Hash_Core_SHA2_init_384
//Requires: caml_failwith
function _3_Hacl_Hash_Core_SHA2_init_384 (x0) {
  caml_failwith('Hacl_Hash_Core_SHA2_init_384 unimplemetned');
}

//Provides: _4_Hacl_Hash_Core_SHA2_init_512
//Requires: caml_failwith
function _4_Hacl_Hash_Core_SHA2_init_512 (x0) {
  caml_failwith('Hacl_Hash_Core_SHA2_init_512 unimplemetned');
}

//Provides: _7_Hacl_Hash_Core_SHA2_pad_256
//Requires: caml_failwith
function _7_Hacl_Hash_Core_SHA2_pad_256 (x0, x1) {
  caml_failwith('Hacl_Hash_Core_SHA2_pad_256 unimplemetned');
}

//Provides: _5_Hacl_Hash_Core_SHA2_update_384
//Requires: caml_failwith
function _5_Hacl_Hash_Core_SHA2_update_384 (x0, x1) {
  caml_failwith('Hacl_Hash_Core_SHA2_update_384 unimplemetned');
}

//Provides: _6_Hacl_Hash_Core_SHA2_update_512
//Requires: caml_failwith
function _6_Hacl_Hash_Core_SHA2_update_512 (x0, x1) {
  caml_failwith('Hacl_Hash_Core_SHA2_update_512 unimplemetned');
}

//Provides: _3_Hacl_Hash_Definitions_hash_word_len
//Requires: caml_failwith
function _3_Hacl_Hash_Definitions_hash_word_len (x0) {
  caml_failwith('Hacl_Hash_Definitions_hash_word_len unimplemetned');
}

//Provides: _1_Hacl_Hash_Definitions_word_len
//Requires: caml_failwith
function _1_Hacl_Hash_Definitions_word_len (x0) {
  caml_failwith('Hacl_Hash_Definitions_word_len unimplemetned');
}

//Provides: _6_Hacl_Hash_MD5_legacy_hash
//Requires: caml_failwith
function _6_Hacl_Hash_MD5_legacy_hash (x0, x1, x2) {
  caml_failwith('Hacl_Hash_MD5_legacy_hash unimplemetned');
}

//Provides: _5_Hacl_Hash_MD5_legacy_update_last
//Requires: caml_failwith
function _5_Hacl_Hash_MD5_legacy_update_last (x0, x1, x2, x3) {
  caml_failwith('Hacl_Hash_MD5_legacy_update_last unimplemetned');
}

//Provides: _4_Hacl_Hash_MD5_legacy_update_multi
//Requires: caml_failwith
function _4_Hacl_Hash_MD5_legacy_update_multi (x0, x1, x2) {
  caml_failwith('Hacl_Hash_MD5_legacy_update_multi unimplemetned');
}

//Provides: _6_Hacl_Hash_SHA1_legacy_hash
//Requires: caml_failwith
function _6_Hacl_Hash_SHA1_legacy_hash (x0, x1, x2) {
  caml_failwith('Hacl_Hash_SHA1_legacy_hash unimplemetned');
}

//Provides: _5_Hacl_Hash_SHA1_legacy_update_last
//Requires: caml_failwith
function _5_Hacl_Hash_SHA1_legacy_update_last (x0, x1, x2, x3) {
  caml_failwith('Hacl_Hash_SHA1_legacy_update_last unimplemetned');
}

//Provides: _4_Hacl_Hash_SHA1_legacy_update_multi
//Requires: caml_failwith
function _4_Hacl_Hash_SHA1_legacy_update_multi (x0, x1, x2) {
  caml_failwith('Hacl_Hash_SHA1_legacy_update_multi unimplemetned');
}

//Provides: _18_Hacl_Hash_SHA2_hash_224
//Requires: caml_failwith
function _18_Hacl_Hash_SHA2_hash_224 (x0, x1, x2) {
  caml_failwith('Hacl_Hash_SHA2_hash_224 unimplemetned');
}

//Provides: _16_Hacl_Hash_SHA2_update_last_224
//Requires: caml_failwith
function _16_Hacl_Hash_SHA2_update_last_224 (x0, x1, x2, x3) {
  caml_failwith('Hacl_Hash_SHA2_update_last_224 unimplemetned');
}

//Provides: _17_Hacl_Hash_SHA2_update_last_256
//Requires: caml_failwith
function _17_Hacl_Hash_SHA2_update_last_256 (x0, x1, x2, x3) {
  caml_failwith('Hacl_Hash_SHA2_update_last_256 unimplemetned');
}

//Provides: _12_Hacl_Hash_SHA2_update_multi_224
//Requires: caml_failwith
function _12_Hacl_Hash_SHA2_update_multi_224 (x0, x1, x2) {
  caml_failwith('Hacl_Hash_SHA2_update_multi_224 unimplemetned');
}

//Provides: _13_Hacl_Hash_SHA2_update_multi_256
//Requires: caml_failwith
function _13_Hacl_Hash_SHA2_update_multi_256 (x0, x1, x2) {
  caml_failwith('Hacl_Hash_SHA2_update_multi_256 unimplemetned');
}

//Provides: _14_Hacl_Hash_SHA2_update_multi_384
//Requires: caml_failwith
function _14_Hacl_Hash_SHA2_update_multi_384 (x0, x1, x2) {
  caml_failwith('Hacl_Hash_SHA2_update_multi_384 unimplemetned');
}

//Provides: _15_Hacl_Hash_SHA2_update_multi_512
//Requires: caml_failwith
function _15_Hacl_Hash_SHA2_update_multi_512 (x0, x1, x2) {
  caml_failwith('Hacl_Hash_SHA2_update_multi_512 unimplemetned');
}

//Provides: _2_Hacl_Impl_Chacha20_chacha20_encrypt_block
//Requires: caml_failwith
function _2_Hacl_Impl_Chacha20_chacha20_encrypt_block (x0, x1, x2, x3) {
  caml_failwith('Hacl_Impl_Chacha20_chacha20_encrypt_block unimplemetned');
}

//Provides: _1_Hacl_Impl_Chacha20_chacha20_init
//Requires: caml_failwith
function _1_Hacl_Impl_Chacha20_chacha20_init (x0, x1, x2, x3) {
  caml_failwith('Hacl_Impl_Chacha20_chacha20_init unimplemetned');
}

//Provides: _3_Hacl_Impl_Chacha20_chacha20_update
//Requires: caml_failwith
function _3_Hacl_Impl_Chacha20_chacha20_update (x0, x1, x2, x3) {
  caml_failwith('Hacl_Impl_Chacha20_chacha20_update unimplemetned');
}

//Provides: _5_Hacl_Impl_Curve25519_Field51_cswap2
//Requires: caml_failwith
function _5_Hacl_Impl_Curve25519_Field51_cswap2 (x0, x1, x2) {
  caml_failwith('Hacl_Impl_Curve25519_Field51_cswap2 unimplemetned');
}

//Provides: _1_Hacl_Impl_Curve25519_Field51_fadd
//Requires: caml_failwith
function _1_Hacl_Impl_Curve25519_Field51_fadd (x0, x1, x2) {
  caml_failwith('Hacl_Impl_Curve25519_Field51_fadd unimplemetned');
}

//Provides: _3_Hacl_Impl_Curve25519_Field51_fmul1
//Requires: caml_failwith
function _3_Hacl_Impl_Curve25519_Field51_fmul1 (x0, x1, x2) {
  caml_failwith('Hacl_Impl_Curve25519_Field51_fmul1 unimplemetned');
}

//Provides: _2_Hacl_Impl_Curve25519_Field51_fsub
//Requires: caml_failwith
function _2_Hacl_Impl_Curve25519_Field51_fsub (x0, x1, x2) {
  caml_failwith('Hacl_Impl_Curve25519_Field51_fsub unimplemetned');
}

//Provides: _4_Hacl_Impl_Curve25519_Field51_store_felem
//Requires: caml_failwith
function _4_Hacl_Impl_Curve25519_Field51_store_felem (x0, x1) {
  caml_failwith('Hacl_Impl_Curve25519_Field51_store_felem unimplemetned');
}

//Provides: _6_Hacl_Impl_Ed25519_Ladder_point_mul
//Requires: caml_failwith
function _6_Hacl_Impl_Ed25519_Ladder_point_mul (x0, x1, x2) {
  caml_failwith('Hacl_Impl_Ed25519_Ladder_point_mul unimplemetned');
}

//Provides: _5_Hacl_Impl_Ed25519_PointAdd_point_add
//Requires: caml_failwith
function _5_Hacl_Impl_Ed25519_PointAdd_point_add (x0, x1, x2) {
  caml_failwith('Hacl_Impl_Ed25519_PointAdd_point_add unimplemetned');
}

//Provides: _7_Hacl_Impl_Ed25519_PointCompress_point_compress
//Requires: caml_failwith
function _7_Hacl_Impl_Ed25519_PointCompress_point_compress (x0, x1) {
  caml_failwith('Hacl_Impl_Ed25519_PointCompress_point_compress unimplemetned');
}

//Provides: _8_Hacl_Impl_Ed25519_PointDecompress_point_decompress
//Requires: caml_failwith
function _8_Hacl_Impl_Ed25519_PointDecompress_point_decompress (x0, x1) {
  caml_failwith('Hacl_Impl_Ed25519_PointDecompress_point_decompress unimplemetned');
}

//Provides: _9_Hacl_Impl_Ed25519_PointEqual_point_equal
//Requires: caml_failwith
function _9_Hacl_Impl_Ed25519_PointEqual_point_equal (x0, x1) {
  caml_failwith('Hacl_Impl_Ed25519_PointEqual_point_equal unimplemetned');
}

//Provides: _10_Hacl_Impl_Ed25519_PointNegate_point_negate
//Requires: caml_failwith
function _10_Hacl_Impl_Ed25519_PointNegate_point_negate (x0, x1) {
  caml_failwith('Hacl_Impl_Ed25519_PointNegate_point_negate unimplemetned');
}

//Provides: _20_Hacl_Impl_Frodo_Encode_frodo_key_decode
//Requires: caml_failwith
function _20_Hacl_Impl_Frodo_Encode_frodo_key_decode (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Impl_Frodo_Encode_frodo_key_decode unimplemetned');
}

//Provides: _19_Hacl_Impl_Frodo_Encode_frodo_key_encode
//Requires: caml_failwith
function _19_Hacl_Impl_Frodo_Encode_frodo_key_encode (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Impl_Frodo_Encode_frodo_key_encode unimplemetned');
}

//Provides: _10_Hacl_Impl_Frodo_Gen_frodo_gen_matrix_shake_4x
//Requires: caml_failwith
function _10_Hacl_Impl_Frodo_Gen_frodo_gen_matrix_shake_4x (x0, x1, x2) {
  caml_failwith('Hacl_Impl_Frodo_Gen_frodo_gen_matrix_shake_4x unimplemetned');
}

//Provides: _17_Hacl_Impl_Frodo_Pack_frodo_pack
//Requires: caml_failwith
function _17_Hacl_Impl_Frodo_Pack_frodo_pack (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Impl_Frodo_Pack_frodo_pack unimplemetned');
}

//Provides: _18_Hacl_Impl_Frodo_Pack_frodo_unpack
//Requires: caml_failwith
function _18_Hacl_Impl_Frodo_Pack_frodo_unpack (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Impl_Frodo_Pack_frodo_unpack unimplemetned');
}

//Provides: _11_Hacl_Impl_Frodo_Params_frodo_gen_matrix
//Requires: caml_failwith
function _11_Hacl_Impl_Frodo_Params_frodo_gen_matrix (x0, x1, x2, x3) {
  caml_failwith('Hacl_Impl_Frodo_Params_frodo_gen_matrix unimplemetned');
}

//Provides: _15_Hacl_Impl_Frodo_Sample_frodo_sample_matrix1344
//Requires: caml_failwith
function _15_Hacl_Impl_Frodo_Sample_frodo_sample_matrix1344 (x0, x1, x2, x3) {
  caml_failwith('Hacl_Impl_Frodo_Sample_frodo_sample_matrix1344 unimplemetned');
}

//Provides: _12_Hacl_Impl_Frodo_Sample_frodo_sample_matrix64
//Requires: caml_failwith
function _12_Hacl_Impl_Frodo_Sample_frodo_sample_matrix64 (x0, x1, x2, x3) {
  caml_failwith('Hacl_Impl_Frodo_Sample_frodo_sample_matrix64 unimplemetned');
}

//Provides: _13_Hacl_Impl_Frodo_Sample_frodo_sample_matrix640
//Requires: caml_failwith
function _13_Hacl_Impl_Frodo_Sample_frodo_sample_matrix640 (x0, x1, x2, x3) {
  caml_failwith('Hacl_Impl_Frodo_Sample_frodo_sample_matrix640 unimplemetned');
}

//Provides: _14_Hacl_Impl_Frodo_Sample_frodo_sample_matrix976
//Requires: caml_failwith
function _14_Hacl_Impl_Frodo_Sample_frodo_sample_matrix976 (x0, x1, x2, x3) {
  caml_failwith('Hacl_Impl_Frodo_Sample_frodo_sample_matrix976 unimplemetned');
}

//Provides: _3_Hacl_Impl_Matrix_matrix_add
//Requires: caml_failwith
function _3_Hacl_Impl_Matrix_matrix_add (x0, x1, x2, x3) {
  caml_failwith('Hacl_Impl_Matrix_matrix_add unimplemetned');
}

//Provides: _7_Hacl_Impl_Matrix_matrix_eq
//Requires: caml_failwith
function _7_Hacl_Impl_Matrix_matrix_eq (x0, x1, x2, x3) {
  caml_failwith('Hacl_Impl_Matrix_matrix_eq unimplemetned');
}

//Provides: _9_Hacl_Impl_Matrix_matrix_from_lbytes
//Requires: caml_failwith
function _9_Hacl_Impl_Matrix_matrix_from_lbytes (x0, x1, x2, x3) {
  caml_failwith('Hacl_Impl_Matrix_matrix_from_lbytes unimplemetned');
}

//Provides: _5_Hacl_Impl_Matrix_matrix_mul
//Requires: caml_failwith
function _5_Hacl_Impl_Matrix_matrix_mul (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Impl_Matrix_matrix_mul unimplemetned');
}

//Provides: _5_Hacl_Impl_Matrix_matrix_mul_byte6
//Requires: _5_Hacl_Impl_Matrix_matrix_mul
function _5_Hacl_Impl_Matrix_matrix_mul_byte6 (x0, x1, x2, x3, x4, x5) {
  return _5_Hacl_Impl_Matrix_matrix_mul(x0, x1, x2, x3, x4, x5);
}

//Provides: _6_Hacl_Impl_Matrix_matrix_mul_s
//Requires: caml_failwith
function _6_Hacl_Impl_Matrix_matrix_mul_s (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Impl_Matrix_matrix_mul_s unimplemetned');
}

//Provides: _6_Hacl_Impl_Matrix_matrix_mul_s_byte6
//Requires: _6_Hacl_Impl_Matrix_matrix_mul_s
function _6_Hacl_Impl_Matrix_matrix_mul_s_byte6 (x0, x1, x2, x3, x4, x5) {
  return _6_Hacl_Impl_Matrix_matrix_mul_s(x0, x1, x2, x3, x4, x5);
}

//Provides: _4_Hacl_Impl_Matrix_matrix_sub
//Requires: caml_failwith
function _4_Hacl_Impl_Matrix_matrix_sub (x0, x1, x2, x3) {
  caml_failwith('Hacl_Impl_Matrix_matrix_sub unimplemetned');
}

//Provides: _8_Hacl_Impl_Matrix_matrix_to_lbytes
//Requires: caml_failwith
function _8_Hacl_Impl_Matrix_matrix_to_lbytes (x0, x1, x2, x3) {
  caml_failwith('Hacl_Impl_Matrix_matrix_to_lbytes unimplemetned');
}

//Provides: _2_Hacl_Impl_Matrix_mod_pow2
//Requires: caml_failwith
function _2_Hacl_Impl_Matrix_mod_pow2 (x0, x1, x2, x3) {
  caml_failwith('Hacl_Impl_Matrix_mod_pow2 unimplemetned');
}

//Provides: _4_Hacl_Impl_P256_Core_isPointAtInfinityPrivate
//Requires: caml_failwith
function _4_Hacl_Impl_P256_Core_isPointAtInfinityPrivate (x0) {
  caml_failwith('Hacl_Impl_P256_Core_isPointAtInfinityPrivate unimplemetned');
}

//Provides: _5_Hacl_Impl_P256_Core_secretToPublic
//Requires: caml_failwith
function _5_Hacl_Impl_P256_Core_secretToPublic (x0, x1, x2) {
  caml_failwith('Hacl_Impl_P256_Core_secretToPublic unimplemetned');
}

//Provides: _6_Hacl_Impl_P256_DH__ecp256dh_r
//Requires: caml_failwith
function _6_Hacl_Impl_P256_DH__ecp256dh_r (x0, x1, x2) {
  caml_failwith('Hacl_Impl_P256_DH__ecp256dh_r unimplemetned');
}

//Provides: _2_Hacl_Impl_P256_LowLevel_changeEndian
//Requires: caml_failwith
function _2_Hacl_Impl_P256_LowLevel_changeEndian (x0) {
  caml_failwith('Hacl_Impl_P256_LowLevel_changeEndian unimplemetned');
}

//Provides: _3_Hacl_Impl_P256_LowLevel_toUint64ChangeEndian
//Requires: caml_failwith
function _3_Hacl_Impl_P256_LowLevel_toUint64ChangeEndian (x0, x1) {
  caml_failwith('Hacl_Impl_P256_LowLevel_toUint64ChangeEndian unimplemetned');
}

//Provides: _1_Hacl_Impl_P256_LowLevel_toUint8
//Requires: caml_failwith
function _1_Hacl_Impl_P256_LowLevel_toUint8 (x0, x1) {
  caml_failwith('Hacl_Impl_P256_LowLevel_toUint8 unimplemetned');
}

//Provides: _5_Hacl_Impl_SHA3_absorb
//Requires: caml_failwith
function _5_Hacl_Impl_SHA3_absorb (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_Impl_SHA3_absorb unimplemetned');
}

//Provides: _3_Hacl_Impl_SHA3_loadState
//Requires: caml_failwith
function _3_Hacl_Impl_SHA3_loadState (x0, x1, x2) {
  caml_failwith('Hacl_Impl_SHA3_loadState unimplemetned');
}

//Provides: _1_Hacl_Impl_SHA3_rotl
//Requires: caml_failwith
function _1_Hacl_Impl_SHA3_rotl (x0, x1) {
  caml_failwith('Hacl_Impl_SHA3_rotl unimplemetned');
}

//Provides: _6_Hacl_Impl_SHA3_squeeze
//Requires: caml_failwith
function _6_Hacl_Impl_SHA3_squeeze (x0, x1, x2, x3) {
  caml_failwith('Hacl_Impl_SHA3_squeeze unimplemetned');
}

//Provides: _2_Hacl_Impl_SHA3_state_permute
//Requires: caml_failwith
function _2_Hacl_Impl_SHA3_state_permute (x0) {
  caml_failwith('Hacl_Impl_SHA3_state_permute unimplemetned');
}

//Provides: _4_Hacl_Impl_SHA3_storeState
//Requires: caml_failwith
function _4_Hacl_Impl_SHA3_storeState (x0, x1, x2) {
  caml_failwith('Hacl_Impl_SHA3_storeState unimplemetned');
}

//Provides: _1_Hacl_IntTypes_Intrinsics_128_add_carry_u64
//Requires: caml_failwith
function _1_Hacl_IntTypes_Intrinsics_128_add_carry_u64 (x0, x1, x2, x3) {
  caml_failwith('Hacl_IntTypes_Intrinsics_128_add_carry_u64 unimplemetned');
}

//Provides: _2_Hacl_IntTypes_Intrinsics_128_sub_borrow_u64
//Requires: caml_failwith
function _2_Hacl_IntTypes_Intrinsics_128_sub_borrow_u64 (x0, x1, x2, x3) {
  caml_failwith('Hacl_IntTypes_Intrinsics_128_sub_borrow_u64 unimplemetned');
}

//Provides: _1_Hacl_IntTypes_Intrinsics_add_carry_u32
//Requires: caml_failwith
function _1_Hacl_IntTypes_Intrinsics_add_carry_u32 (x0, x1, x2, x3) {
  caml_failwith('Hacl_IntTypes_Intrinsics_add_carry_u32 unimplemetned');
}

//Provides: _3_Hacl_IntTypes_Intrinsics_add_carry_u64
//Requires: caml_failwith
function _3_Hacl_IntTypes_Intrinsics_add_carry_u64 (x0, x1, x2, x3) {
  caml_failwith('Hacl_IntTypes_Intrinsics_add_carry_u64 unimplemetned');
}

//Provides: _2_Hacl_IntTypes_Intrinsics_sub_borrow_u32
//Requires: caml_failwith
function _2_Hacl_IntTypes_Intrinsics_sub_borrow_u32 (x0, x1, x2, x3) {
  caml_failwith('Hacl_IntTypes_Intrinsics_sub_borrow_u32 unimplemetned');
}

//Provides: _4_Hacl_IntTypes_Intrinsics_sub_borrow_u64
//Requires: caml_failwith
function _4_Hacl_IntTypes_Intrinsics_sub_borrow_u64 (x0, x1, x2, x3) {
  caml_failwith('Hacl_IntTypes_Intrinsics_sub_borrow_u64 unimplemetned');
}

//Provides: _1_Hacl_Keccak_shake128_4x
//Requires: caml_failwith
function _1_Hacl_Keccak_shake128_4x (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) {
  caml_failwith('Hacl_Keccak_shake128_4x unimplemetned');
}

//Provides: _1_Hacl_Keccak_shake128_4x_byte10
//Requires: _1_Hacl_Keccak_shake128_4x
function _1_Hacl_Keccak_shake128_4x_byte10 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) {
  return _1_Hacl_Keccak_shake128_4x(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9);
}

//Provides: _7_Hacl_NaCl_crypto_box_detached
//Requires: caml_failwith
function _7_Hacl_NaCl_crypto_box_detached (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_NaCl_crypto_box_detached unimplemetned');
}

//Provides: _7_Hacl_NaCl_crypto_box_detached_byte7
//Requires: _7_Hacl_NaCl_crypto_box_detached
function _7_Hacl_NaCl_crypto_box_detached_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _7_Hacl_NaCl_crypto_box_detached(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _11_Hacl_NaCl_crypto_box_easy
//Requires: caml_failwith
function _11_Hacl_NaCl_crypto_box_easy (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_NaCl_crypto_box_easy unimplemetned');
}

//Provides: _11_Hacl_NaCl_crypto_box_easy_byte6
//Requires: _11_Hacl_NaCl_crypto_box_easy
function _11_Hacl_NaCl_crypto_box_easy_byte6 (x0, x1, x2, x3, x4, x5) {
  return _11_Hacl_NaCl_crypto_box_easy(x0, x1, x2, x3, x4, x5);
}

//Provides: _9_Hacl_NaCl_crypto_box_open_detached
//Requires: caml_failwith
function _9_Hacl_NaCl_crypto_box_open_detached (x0, x1, x2, x3, x4, x5, x6) {
  caml_failwith('Hacl_NaCl_crypto_box_open_detached unimplemetned');
}

//Provides: _9_Hacl_NaCl_crypto_box_open_detached_byte7
//Requires: _9_Hacl_NaCl_crypto_box_open_detached
function _9_Hacl_NaCl_crypto_box_open_detached_byte7 (x0, x1, x2, x3, x4, x5, x6) {
  return _9_Hacl_NaCl_crypto_box_open_detached(x0, x1, x2, x3, x4, x5, x6);
}

//Provides: _13_Hacl_NaCl_crypto_box_open_easy
//Requires: caml_failwith
function _13_Hacl_NaCl_crypto_box_open_easy (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_NaCl_crypto_box_open_easy unimplemetned');
}

//Provides: _13_Hacl_NaCl_crypto_box_open_easy_byte6
//Requires: _13_Hacl_NaCl_crypto_box_open_easy
function _13_Hacl_NaCl_crypto_box_open_easy_byte6 (x0, x1, x2, x3, x4, x5) {
  return _13_Hacl_NaCl_crypto_box_open_easy(x0, x1, x2, x3, x4, x5);
}

//Provides: _1_Hacl_NaCl_crypto_secretbox_detached
//Requires: caml_failwith
function _1_Hacl_NaCl_crypto_secretbox_detached (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_NaCl_crypto_secretbox_detached unimplemetned');
}

//Provides: _1_Hacl_NaCl_crypto_secretbox_detached_byte6
//Requires: _1_Hacl_NaCl_crypto_secretbox_detached
function _1_Hacl_NaCl_crypto_secretbox_detached_byte6 (x0, x1, x2, x3, x4, x5) {
  return _1_Hacl_NaCl_crypto_secretbox_detached(x0, x1, x2, x3, x4, x5);
}

//Provides: _2_Hacl_NaCl_crypto_secretbox_open_detached
//Requires: caml_failwith
function _2_Hacl_NaCl_crypto_secretbox_open_detached (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_NaCl_crypto_secretbox_open_detached unimplemetned');
}

//Provides: _2_Hacl_NaCl_crypto_secretbox_open_detached_byte6
//Requires: _2_Hacl_NaCl_crypto_secretbox_open_detached
function _2_Hacl_NaCl_crypto_secretbox_open_detached_byte6 (x0, x1, x2, x3, x4, x5) {
  return _2_Hacl_NaCl_crypto_secretbox_open_detached(x0, x1, x2, x3, x4, x5);
}

//Provides: _8_Hacl_P256_ecdsa_sign_p256_sha384
//Requires: caml_failwith
function _8_Hacl_P256_ecdsa_sign_p256_sha384 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_P256_ecdsa_sign_p256_sha384 unimplemetned');
}

//Provides: _9_Hacl_P256_ecdsa_sign_p256_sha512
//Requires: caml_failwith
function _9_Hacl_P256_ecdsa_sign_p256_sha512 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_P256_ecdsa_sign_p256_sha512 unimplemetned');
}

//Provides: _12_Hacl_P256_ecdsa_verif_p256_sha384
//Requires: caml_failwith
function _12_Hacl_P256_ecdsa_verif_p256_sha384 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_P256_ecdsa_verif_p256_sha384 unimplemetned');
}

//Provides: _13_Hacl_P256_ecdsa_verif_p256_sha512
//Requires: caml_failwith
function _13_Hacl_P256_ecdsa_verif_p256_sha512 (x0, x1, x2, x3, x4) {
  caml_failwith('Hacl_P256_ecdsa_verif_p256_sha512 unimplemetned');
}

//Provides: _2_Hacl_Poly1305_128_poly1305_mac
//Requires: caml_failwith
function _2_Hacl_Poly1305_128_poly1305_mac (x0, x1, x2, x3) {
  caml_failwith('Hacl_Poly1305_128_poly1305_mac unimplemetned');
}

//Provides: _2_Hacl_Poly1305_256_poly1305_mac
//Requires: caml_failwith
function _2_Hacl_Poly1305_256_poly1305_mac (x0, x1, x2, x3) {
  caml_failwith('Hacl_Poly1305_256_poly1305_mac unimplemetned');
}

//Provides: _5_Hacl_Poly1305_32_poly1305_finish
//Requires: caml_failwith
function _5_Hacl_Poly1305_32_poly1305_finish (x0, x1, x2) {
  caml_failwith('Hacl_Poly1305_32_poly1305_finish unimplemetned');
}

//Provides: _2_Hacl_Poly1305_32_poly1305_init
//Requires: caml_failwith
function _2_Hacl_Poly1305_32_poly1305_init (x0, x1) {
  caml_failwith('Hacl_Poly1305_32_poly1305_init unimplemetned');
}

//Provides: _6_Hacl_Poly1305_32_poly1305_mac
//Requires: caml_failwith
function _6_Hacl_Poly1305_32_poly1305_mac (x0, x1, x2, x3) {
  caml_failwith('Hacl_Poly1305_32_poly1305_mac unimplemetned');
}

//Provides: _4_Hacl_Poly1305_32_poly1305_update
//Requires: caml_failwith
function _4_Hacl_Poly1305_32_poly1305_update (x0, x1, x2) {
  caml_failwith('Hacl_Poly1305_32_poly1305_update unimplemetned');
}

//Provides: _3_Hacl_Poly1305_32_poly1305_update1
//Requires: caml_failwith
function _3_Hacl_Poly1305_32_poly1305_update1 (x0, x1) {
  caml_failwith('Hacl_Poly1305_32_poly1305_update1 unimplemetned');
}

//Provides: _3_Hacl_RSAPSS_new_rsapss_load_pkey
//Requires: caml_failwith
function _3_Hacl_RSAPSS_new_rsapss_load_pkey (x0, x1, x2, x3) {
  caml_failwith('Hacl_RSAPSS_new_rsapss_load_pkey unimplemetned');
}

//Provides: _4_Hacl_RSAPSS_new_rsapss_load_skey
//Requires: caml_failwith
function _4_Hacl_RSAPSS_new_rsapss_load_skey (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_RSAPSS_new_rsapss_load_skey unimplemetned');
}

//Provides: _4_Hacl_RSAPSS_new_rsapss_load_skey_byte6
//Requires: _4_Hacl_RSAPSS_new_rsapss_load_skey
function _4_Hacl_RSAPSS_new_rsapss_load_skey_byte6 (x0, x1, x2, x3, x4, x5) {
  return _4_Hacl_RSAPSS_new_rsapss_load_skey(x0, x1, x2, x3, x4, x5);
}

//Provides: _6_Hacl_RSAPSS_rsapss_pkey_verify
//Requires: caml_failwith
function _6_Hacl_RSAPSS_rsapss_pkey_verify (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) {
  caml_failwith('Hacl_RSAPSS_rsapss_pkey_verify unimplemetned');
}

//Provides: _6_Hacl_RSAPSS_rsapss_pkey_verify_byte10
//Requires: _6_Hacl_RSAPSS_rsapss_pkey_verify
function _6_Hacl_RSAPSS_rsapss_pkey_verify_byte10 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) {
  return _6_Hacl_RSAPSS_rsapss_pkey_verify(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9);
}

//Provides: _1_Hacl_RSAPSS_rsapss_sign
//Requires: caml_failwith
function _1_Hacl_RSAPSS_rsapss_sign (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) {
  caml_failwith('Hacl_RSAPSS_rsapss_sign unimplemetned');
}

//Provides: _1_Hacl_RSAPSS_rsapss_sign_byte10
//Requires: _1_Hacl_RSAPSS_rsapss_sign
function _1_Hacl_RSAPSS_rsapss_sign_byte10 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) {
  return _1_Hacl_RSAPSS_rsapss_sign(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9);
}

//Provides: _5_Hacl_RSAPSS_rsapss_skey_sign
//Requires: caml_failwith
function _5_Hacl_RSAPSS_rsapss_skey_sign (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) {
  caml_failwith('Hacl_RSAPSS_rsapss_skey_sign unimplemetned');
}

//Provides: _5_Hacl_RSAPSS_rsapss_skey_sign_byte12
//Requires: _5_Hacl_RSAPSS_rsapss_skey_sign
function _5_Hacl_RSAPSS_rsapss_skey_sign_byte12 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) {
  return _5_Hacl_RSAPSS_rsapss_skey_sign(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11);
}

//Provides: _2_Hacl_RSAPSS_rsapss_verify
//Requires: caml_failwith
function _2_Hacl_RSAPSS_rsapss_verify (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  caml_failwith('Hacl_RSAPSS_rsapss_verify unimplemetned');
}

//Provides: _2_Hacl_RSAPSS_rsapss_verify_byte9
//Requires: _2_Hacl_RSAPSS_rsapss_verify
function _2_Hacl_RSAPSS_rsapss_verify_byte9 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  return _2_Hacl_RSAPSS_rsapss_verify(x0, x1, x2, x3, x4, x5, x6, x7, x8);
}

//Provides: _1_Hacl_SHA2_Scalar32_sha224
//Requires: caml_failwith
function _1_Hacl_SHA2_Scalar32_sha224 (x0, x1, x2) {
  caml_failwith('Hacl_SHA2_Scalar32_sha224 unimplemetned');
}

//Provides: _2_Hacl_SHA2_Scalar32_sha256
//Requires: caml_failwith
function _2_Hacl_SHA2_Scalar32_sha256 (x0, x1, x2) {
  caml_failwith('Hacl_SHA2_Scalar32_sha256 unimplemetned');
}

//Provides: _3_Hacl_SHA2_Scalar32_sha384
//Requires: caml_failwith
function _3_Hacl_SHA2_Scalar32_sha384 (x0, x1, x2) {
  caml_failwith('Hacl_SHA2_Scalar32_sha384 unimplemetned');
}

//Provides: _4_Hacl_SHA2_Scalar32_sha512
//Requires: caml_failwith
function _4_Hacl_SHA2_Scalar32_sha512 (x0, x1, x2) {
  caml_failwith('Hacl_SHA2_Scalar32_sha512 unimplemetned');
}

//Provides: _1_Hacl_SHA2_Vec128_sha224_4
//Requires: caml_failwith
function _1_Hacl_SHA2_Vec128_sha224_4 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  caml_failwith('Hacl_SHA2_Vec128_sha224_4 unimplemetned');
}

//Provides: _1_Hacl_SHA2_Vec128_sha224_4_byte9
//Requires: _1_Hacl_SHA2_Vec128_sha224_4
function _1_Hacl_SHA2_Vec128_sha224_4_byte9 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  return _1_Hacl_SHA2_Vec128_sha224_4(x0, x1, x2, x3, x4, x5, x6, x7, x8);
}

//Provides: _2_Hacl_SHA2_Vec128_sha256_4
//Requires: caml_failwith
function _2_Hacl_SHA2_Vec128_sha256_4 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  caml_failwith('Hacl_SHA2_Vec128_sha256_4 unimplemetned');
}

//Provides: _2_Hacl_SHA2_Vec128_sha256_4_byte9
//Requires: _2_Hacl_SHA2_Vec128_sha256_4
function _2_Hacl_SHA2_Vec128_sha256_4_byte9 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  return _2_Hacl_SHA2_Vec128_sha256_4(x0, x1, x2, x3, x4, x5, x6, x7, x8);
}

//Provides: _1_Hacl_SHA2_Vec256_sha224_8
//Requires: caml_failwith
function _1_Hacl_SHA2_Vec256_sha224_8 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) {
  caml_failwith('Hacl_SHA2_Vec256_sha224_8 unimplemetned');
}

//Provides: _1_Hacl_SHA2_Vec256_sha224_8_byte17
//Requires: _1_Hacl_SHA2_Vec256_sha224_8
function _1_Hacl_SHA2_Vec256_sha224_8_byte17 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) {
  return _1_Hacl_SHA2_Vec256_sha224_8(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16);
}

//Provides: _2_Hacl_SHA2_Vec256_sha256_8
//Requires: caml_failwith
function _2_Hacl_SHA2_Vec256_sha256_8 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) {
  caml_failwith('Hacl_SHA2_Vec256_sha256_8 unimplemetned');
}

//Provides: _2_Hacl_SHA2_Vec256_sha256_8_byte17
//Requires: _2_Hacl_SHA2_Vec256_sha256_8
function _2_Hacl_SHA2_Vec256_sha256_8_byte17 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) {
  return _2_Hacl_SHA2_Vec256_sha256_8(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16);
}

//Provides: _3_Hacl_SHA2_Vec256_sha384_4
//Requires: caml_failwith
function _3_Hacl_SHA2_Vec256_sha384_4 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  caml_failwith('Hacl_SHA2_Vec256_sha384_4 unimplemetned');
}

//Provides: _3_Hacl_SHA2_Vec256_sha384_4_byte9
//Requires: _3_Hacl_SHA2_Vec256_sha384_4
function _3_Hacl_SHA2_Vec256_sha384_4_byte9 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  return _3_Hacl_SHA2_Vec256_sha384_4(x0, x1, x2, x3, x4, x5, x6, x7, x8);
}

//Provides: _4_Hacl_SHA2_Vec256_sha512_4
//Requires: caml_failwith
function _4_Hacl_SHA2_Vec256_sha512_4 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  caml_failwith('Hacl_SHA2_Vec256_sha512_4 unimplemetned');
}

//Provides: _4_Hacl_SHA2_Vec256_sha512_4_byte9
//Requires: _4_Hacl_SHA2_Vec256_sha512_4
function _4_Hacl_SHA2_Vec256_sha512_4_byte9 (x0, x1, x2, x3, x4, x5, x6, x7, x8) {
  return _4_Hacl_SHA2_Vec256_sha512_4(x0, x1, x2, x3, x4, x5, x6, x7, x8);
}

//Provides: _8_Hacl_SHA3_shake128_hacl
//Requires: caml_failwith
function _8_Hacl_SHA3_shake128_hacl (x0, x1, x2, x3) {
  caml_failwith('Hacl_SHA3_shake128_hacl unimplemetned');
}

//Provides: _9_Hacl_SHA3_shake256_hacl
//Requires: caml_failwith
function _9_Hacl_SHA3_shake256_hacl (x0, x1, x2, x3) {
  caml_failwith('Hacl_SHA3_shake256_hacl unimplemetned');
}

//Provides: _4_Hacl_Salsa20_hsalsa20
//Requires: caml_failwith
function _4_Hacl_Salsa20_hsalsa20 (x0, x1, x2) {
  caml_failwith('Hacl_Salsa20_hsalsa20 unimplemetned');
}

//Provides: _2_Hacl_Salsa20_salsa20_decrypt
//Requires: caml_failwith
function _2_Hacl_Salsa20_salsa20_decrypt (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Salsa20_salsa20_decrypt unimplemetned');
}

//Provides: _2_Hacl_Salsa20_salsa20_decrypt_byte6
//Requires: _2_Hacl_Salsa20_salsa20_decrypt
function _2_Hacl_Salsa20_salsa20_decrypt_byte6 (x0, x1, x2, x3, x4, x5) {
  return _2_Hacl_Salsa20_salsa20_decrypt(x0, x1, x2, x3, x4, x5);
}

//Provides: _1_Hacl_Salsa20_salsa20_encrypt
//Requires: caml_failwith
function _1_Hacl_Salsa20_salsa20_encrypt (x0, x1, x2, x3, x4, x5) {
  caml_failwith('Hacl_Salsa20_salsa20_encrypt unimplemetned');
}

//Provides: _1_Hacl_Salsa20_salsa20_encrypt_byte6
//Requires: _1_Hacl_Salsa20_salsa20_encrypt
function _1_Hacl_Salsa20_salsa20_encrypt_byte6 (x0, x1, x2, x3, x4, x5) {
  return _1_Hacl_Salsa20_salsa20_encrypt(x0, x1, x2, x3, x4, x5);
}

//Provides: _3_Hacl_Salsa20_salsa20_key_block0
//Requires: caml_failwith
function _3_Hacl_Salsa20_salsa20_key_block0 (x0, x1, x2) {
  caml_failwith('Hacl_Salsa20_salsa20_key_block0 unimplemetned');
}

//Provides: _7_Hacl_Streaming_Blake2_blake2b_32_no_key_create_in
//Requires: caml_failwith
function _7_Hacl_Streaming_Blake2_blake2b_32_no_key_create_in (x0) {
  caml_failwith('Hacl_Streaming_Blake2_blake2b_32_no_key_create_in unimplemetned');
}

//Provides: _10_Hacl_Streaming_Blake2_blake2b_32_no_key_finish
//Requires: caml_failwith
function _10_Hacl_Streaming_Blake2_blake2b_32_no_key_finish (x0, x1) {
  caml_failwith('Hacl_Streaming_Blake2_blake2b_32_no_key_finish unimplemetned');
}

//Provides: _11_Hacl_Streaming_Blake2_blake2b_32_no_key_free
//Requires: caml_failwith
function _11_Hacl_Streaming_Blake2_blake2b_32_no_key_free (x0) {
  caml_failwith('Hacl_Streaming_Blake2_blake2b_32_no_key_free unimplemetned');
}

//Provides: _8_Hacl_Streaming_Blake2_blake2b_32_no_key_init
//Requires: caml_failwith
function _8_Hacl_Streaming_Blake2_blake2b_32_no_key_init (x0) {
  caml_failwith('Hacl_Streaming_Blake2_blake2b_32_no_key_init unimplemetned');
}

//Provides: _9_Hacl_Streaming_Blake2_blake2b_32_no_key_update
//Requires: caml_failwith
function _9_Hacl_Streaming_Blake2_blake2b_32_no_key_update (x0, x1, x2) {
  caml_failwith('Hacl_Streaming_Blake2_blake2b_32_no_key_update unimplemetned');
}

//Provides: _2_Hacl_Streaming_Blake2_blake2s_32_no_key_create_in
//Requires: caml_failwith
function _2_Hacl_Streaming_Blake2_blake2s_32_no_key_create_in (x0) {
  caml_failwith('Hacl_Streaming_Blake2_blake2s_32_no_key_create_in unimplemetned');
}

//Provides: _5_Hacl_Streaming_Blake2_blake2s_32_no_key_finish
//Requires: caml_failwith
function _5_Hacl_Streaming_Blake2_blake2s_32_no_key_finish (x0, x1) {
  caml_failwith('Hacl_Streaming_Blake2_blake2s_32_no_key_finish unimplemetned');
}

//Provides: _6_Hacl_Streaming_Blake2_blake2s_32_no_key_free
//Requires: caml_failwith
function _6_Hacl_Streaming_Blake2_blake2s_32_no_key_free (x0) {
  caml_failwith('Hacl_Streaming_Blake2_blake2s_32_no_key_free unimplemetned');
}

//Provides: _3_Hacl_Streaming_Blake2_blake2s_32_no_key_init
//Requires: caml_failwith
function _3_Hacl_Streaming_Blake2_blake2s_32_no_key_init (x0) {
  caml_failwith('Hacl_Streaming_Blake2_blake2s_32_no_key_init unimplemetned');
}

//Provides: _4_Hacl_Streaming_Blake2_blake2s_32_no_key_update
//Requires: caml_failwith
function _4_Hacl_Streaming_Blake2_blake2s_32_no_key_update (x0, x1, x2) {
  caml_failwith('Hacl_Streaming_Blake2_blake2s_32_no_key_update unimplemetned');
}

//Provides: _1_Hacl_Streaming_Blake2_blocks_state_len
//Requires: caml_failwith
function _1_Hacl_Streaming_Blake2_blocks_state_len (x0, x1) {
  caml_failwith('Hacl_Streaming_Blake2_blocks_state_len unimplemetned');
}

//Provides: _1_Hacl_Streaming_MD5_legacy_create_in_md5
//Requires: caml_failwith
function _1_Hacl_Streaming_MD5_legacy_create_in_md5 (x0) {
  caml_failwith('Hacl_Streaming_MD5_legacy_create_in_md5 unimplemetned');
}

//Provides: _4_Hacl_Streaming_MD5_legacy_finish_md5
//Requires: caml_failwith
function _4_Hacl_Streaming_MD5_legacy_finish_md5 (x0, x1) {
  caml_failwith('Hacl_Streaming_MD5_legacy_finish_md5 unimplemetned');
}

//Provides: _5_Hacl_Streaming_MD5_legacy_free_md5
//Requires: caml_failwith
function _5_Hacl_Streaming_MD5_legacy_free_md5 (x0) {
  caml_failwith('Hacl_Streaming_MD5_legacy_free_md5 unimplemetned');
}

//Provides: _2_Hacl_Streaming_MD5_legacy_init_md5
//Requires: caml_failwith
function _2_Hacl_Streaming_MD5_legacy_init_md5 (x0) {
  caml_failwith('Hacl_Streaming_MD5_legacy_init_md5 unimplemetned');
}

//Provides: _3_Hacl_Streaming_MD5_legacy_update_md5
//Requires: caml_failwith
function _3_Hacl_Streaming_MD5_legacy_update_md5 (x0, x1, x2) {
  caml_failwith('Hacl_Streaming_MD5_legacy_update_md5 unimplemetned');
}

//Provides: _1_Hacl_Streaming_Poly1305_32_create_in
//Requires: caml_failwith
function _1_Hacl_Streaming_Poly1305_32_create_in (x0) {
  caml_failwith('Hacl_Streaming_Poly1305_32_create_in unimplemetned');
}

//Provides: _4_Hacl_Streaming_Poly1305_32_finish
//Requires: caml_failwith
function _4_Hacl_Streaming_Poly1305_32_finish (x0, x1) {
  caml_failwith('Hacl_Streaming_Poly1305_32_finish unimplemetned');
}

//Provides: _5_Hacl_Streaming_Poly1305_32_free
//Requires: caml_failwith
function _5_Hacl_Streaming_Poly1305_32_free (x0) {
  caml_failwith('Hacl_Streaming_Poly1305_32_free unimplemetned');
}

//Provides: _2_Hacl_Streaming_Poly1305_32_init
//Requires: caml_failwith
function _2_Hacl_Streaming_Poly1305_32_init (x0, x1) {
  caml_failwith('Hacl_Streaming_Poly1305_32_init unimplemetned');
}

//Provides: _3_Hacl_Streaming_Poly1305_32_update
//Requires: caml_failwith
function _3_Hacl_Streaming_Poly1305_32_update (x0, x1, x2) {
  caml_failwith('Hacl_Streaming_Poly1305_32_update unimplemetned');
}

//Provides: _1_Hacl_Streaming_SHA1_legacy_create_in_sha1
//Requires: caml_failwith
function _1_Hacl_Streaming_SHA1_legacy_create_in_sha1 (x0) {
  caml_failwith('Hacl_Streaming_SHA1_legacy_create_in_sha1 unimplemetned');
}

//Provides: _4_Hacl_Streaming_SHA1_legacy_finish_sha1
//Requires: caml_failwith
function _4_Hacl_Streaming_SHA1_legacy_finish_sha1 (x0, x1) {
  caml_failwith('Hacl_Streaming_SHA1_legacy_finish_sha1 unimplemetned');
}

//Provides: _5_Hacl_Streaming_SHA1_legacy_free_sha1
//Requires: caml_failwith
function _5_Hacl_Streaming_SHA1_legacy_free_sha1 (x0) {
  caml_failwith('Hacl_Streaming_SHA1_legacy_free_sha1 unimplemetned');
}

//Provides: _2_Hacl_Streaming_SHA1_legacy_init_sha1
//Requires: caml_failwith
function _2_Hacl_Streaming_SHA1_legacy_init_sha1 (x0) {
  caml_failwith('Hacl_Streaming_SHA1_legacy_init_sha1 unimplemetned');
}

//Provides: _3_Hacl_Streaming_SHA1_legacy_update_sha1
//Requires: caml_failwith
function _3_Hacl_Streaming_SHA1_legacy_update_sha1 (x0, x1, x2) {
  caml_failwith('Hacl_Streaming_SHA1_legacy_update_sha1 unimplemetned');
}

//Provides: _1_Hacl_Streaming_SHA2_create_in_224
//Requires: caml_failwith
function _1_Hacl_Streaming_SHA2_create_in_224 (x0) {
  caml_failwith('Hacl_Streaming_SHA2_create_in_224 unimplemetned');
}

//Provides: _6_Hacl_Streaming_SHA2_create_in_256
//Requires: caml_failwith
function _6_Hacl_Streaming_SHA2_create_in_256 (x0) {
  caml_failwith('Hacl_Streaming_SHA2_create_in_256 unimplemetned');
}

//Provides: _11_Hacl_Streaming_SHA2_create_in_384
//Requires: caml_failwith
function _11_Hacl_Streaming_SHA2_create_in_384 (x0) {
  caml_failwith('Hacl_Streaming_SHA2_create_in_384 unimplemetned');
}

//Provides: _16_Hacl_Streaming_SHA2_create_in_512
//Requires: caml_failwith
function _16_Hacl_Streaming_SHA2_create_in_512 (x0) {
  caml_failwith('Hacl_Streaming_SHA2_create_in_512 unimplemetned');
}

//Provides: _4_Hacl_Streaming_SHA2_finish_224
//Requires: caml_failwith
function _4_Hacl_Streaming_SHA2_finish_224 (x0, x1) {
  caml_failwith('Hacl_Streaming_SHA2_finish_224 unimplemetned');
}

//Provides: _9_Hacl_Streaming_SHA2_finish_256
//Requires: caml_failwith
function _9_Hacl_Streaming_SHA2_finish_256 (x0, x1) {
  caml_failwith('Hacl_Streaming_SHA2_finish_256 unimplemetned');
}

//Provides: _14_Hacl_Streaming_SHA2_finish_384
//Requires: caml_failwith
function _14_Hacl_Streaming_SHA2_finish_384 (x0, x1) {
  caml_failwith('Hacl_Streaming_SHA2_finish_384 unimplemetned');
}

//Provides: _19_Hacl_Streaming_SHA2_finish_512
//Requires: caml_failwith
function _19_Hacl_Streaming_SHA2_finish_512 (x0, x1) {
  caml_failwith('Hacl_Streaming_SHA2_finish_512 unimplemetned');
}

//Provides: _5_Hacl_Streaming_SHA2_free_224
//Requires: caml_failwith
function _5_Hacl_Streaming_SHA2_free_224 (x0) {
  caml_failwith('Hacl_Streaming_SHA2_free_224 unimplemetned');
}

//Provides: _10_Hacl_Streaming_SHA2_free_256
//Requires: caml_failwith
function _10_Hacl_Streaming_SHA2_free_256 (x0) {
  caml_failwith('Hacl_Streaming_SHA2_free_256 unimplemetned');
}

//Provides: _15_Hacl_Streaming_SHA2_free_384
//Requires: caml_failwith
function _15_Hacl_Streaming_SHA2_free_384 (x0) {
  caml_failwith('Hacl_Streaming_SHA2_free_384 unimplemetned');
}

//Provides: _20_Hacl_Streaming_SHA2_free_512
//Requires: caml_failwith
function _20_Hacl_Streaming_SHA2_free_512 (x0) {
  caml_failwith('Hacl_Streaming_SHA2_free_512 unimplemetned');
}

//Provides: _2_Hacl_Streaming_SHA2_init_224
//Requires: caml_failwith
function _2_Hacl_Streaming_SHA2_init_224 (x0) {
  caml_failwith('Hacl_Streaming_SHA2_init_224 unimplemetned');
}

//Provides: _7_Hacl_Streaming_SHA2_init_256
//Requires: caml_failwith
function _7_Hacl_Streaming_SHA2_init_256 (x0) {
  caml_failwith('Hacl_Streaming_SHA2_init_256 unimplemetned');
}

//Provides: _12_Hacl_Streaming_SHA2_init_384
//Requires: caml_failwith
function _12_Hacl_Streaming_SHA2_init_384 (x0) {
  caml_failwith('Hacl_Streaming_SHA2_init_384 unimplemetned');
}

//Provides: _17_Hacl_Streaming_SHA2_init_512
//Requires: caml_failwith
function _17_Hacl_Streaming_SHA2_init_512 (x0) {
  caml_failwith('Hacl_Streaming_SHA2_init_512 unimplemetned');
}

//Provides: _3_Hacl_Streaming_SHA2_update_224
//Requires: caml_failwith
function _3_Hacl_Streaming_SHA2_update_224 (x0, x1, x2) {
  caml_failwith('Hacl_Streaming_SHA2_update_224 unimplemetned');
}

//Provides: _8_Hacl_Streaming_SHA2_update_256
//Requires: caml_failwith
function _8_Hacl_Streaming_SHA2_update_256 (x0, x1, x2) {
  caml_failwith('Hacl_Streaming_SHA2_update_256 unimplemetned');
}

//Provides: _13_Hacl_Streaming_SHA2_update_384
//Requires: caml_failwith
function _13_Hacl_Streaming_SHA2_update_384 (x0, x1, x2) {
  caml_failwith('Hacl_Streaming_SHA2_update_384 unimplemetned');
}

//Provides: _18_Hacl_Streaming_SHA2_update_512
//Requires: caml_failwith
function _18_Hacl_Streaming_SHA2_update_512 (x0, x1, x2) {
  caml_failwith('Hacl_Streaming_SHA2_update_512 unimplemetned');
}

//Provides: _16_randombytes_
//Requires: caml_failwith
function _16_randombytes_ (x0, x1) {
  caml_failwith('randombytes_ unimplemetned');
}

