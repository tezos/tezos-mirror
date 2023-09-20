//   Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

//Provides: EverCrypt_AutoConfig2_init
//Requires: caml_failwith
function EverCrypt_AutoConfig2_init(_) {
  return 0;
}

//Provides: Spec_Hash_Definitions
var Spec_Hash_Definitions = {};
Spec_Hash_Definitions.SHA2_224 = 0;
Spec_Hash_Definitions.SHA2_256 = 1;
Spec_Hash_Definitions.SHA2_384 = 2;
Spec_Hash_Definitions.SHA2_512 = 3;
Spec_Hash_Definitions.SHA1 = 4;
Spec_Hash_Definitions.MD5 = 5;
Spec_Hash_Definitions.Blake2S = 6;
Spec_Hash_Definitions.Blake2B = 7;

//Provides: Hacl_Hash_Definitions_hash_len
//Requires: caml_failwith, Spec_Hash_Definitions, integers_uint32_of_int32
function Hacl_Hash_Definitions_hash_len(a) {
  switch (a) {
    case Spec_Hash_Definitions.MD5:
      return integers_uint32_of_int32(16);
    case Spec_Hash_Definitions.SHA1:
      return integers_uint32_of_int32(20);
    case Spec_Hash_Definitions.SHA2_224:
      return integers_uint32_of_int32(28);
    case Spec_Hash_Definitions.SHA2_256:
      return integers_uint32_of_int32(32);
    case Spec_Hash_Definitions.SHA2_384:
      return integers_uint32_of_int32(48);
    case Spec_Hash_Definitions.SHA2_512:
      return integers_uint32_of_int32(64);
    case Spec_Hash_Definitions.Blake2S:
      return integers_uint32_of_int32(32);
    case Spec_Hash_Definitions.Blake2B:
      return integers_uint32_of_int32(64);
    default:
      caml_failwith(
        "KreMLin incomplete match in Hacl_Hash_Definitions_hash_len"
      );
  }
}

//Provides: Hacl_Hash_Definitions_block_len
//Requires: caml_failwith, Spec_Hash_Definitions, integers_uint32_of_int32
function Hacl_Hash_Definitions_block_len(a) {
  switch (a) {
    case Spec_Hash_Definitions.MD5:
      return integers_uint32_of_int32(64);
    case Spec_Hash_Definitions.SHA1:
      return integers_uint32_of_int32(64);
    case Spec_Hash_Definitions.SHA2_224:
      return integers_uint32_of_int32(64);
    case Spec_Hash_Definitions.SHA2_256:
      return integers_uint32_of_int32(64);
    case Spec_Hash_Definitions.SHA2_384:
      return integers_uint32_of_int32(128);
    case Spec_Hash_Definitions.SHA2_512:
      return integers_uint32_of_int32(128);
    case Spec_Hash_Definitions.Blake2S:
      return integers_uint32_of_int32(64);
    case Spec_Hash_Definitions.Blake2B:
      return integers_uint32_of_int32(128);
    default:
      caml_failwith(
        "KreMLin incomplete match at Hacl_Hash_Definitions_block_len"
      );
  }
}

//Provides: EverCrypt_HMAC_compute
//Requires: caml_failwith, Spec_Hash_Definitions, Hacl_HMAC_compute_sha2_256, Hacl_HMAC_compute_sha2_512
function EverCrypt_HMAC_compute(a, tag, key, key_len, data, data_len) {
  switch (a) {
    case Spec_Hash_Definitions.SHA2_256:
      return Hacl_HMAC_compute_sha2_256(tag, key, key_len, data, data_len);
    case Spec_Hash_Definitions.SHA2_512:
      return Hacl_HMAC_compute_sha2_512(tag, key, key_len, data, data_len);
    default:
      caml_failwith("EverCrypt_HMAC_compute unimplemented for this algo");
  }
}

//Provides: EverCrypt_Hash_Incremental_hash
//Requires: caml_failwith, Hacl_Streaming_SHA2_hash_256, Hacl_Streaming_SHA2_hash_512, Spec_Hash_Definitions
function EverCrypt_Hash_Incremental_hash(a, hash, input, input_len) {
  switch (a) {
    case Spec_Hash_Definitions.SHA2_256:
      return Hacl_Streaming_SHA2_hash_256(input, input_len, hash);
    case Spec_Hash_Definitions.SHA2_512:
      return Hacl_Streaming_SHA2_hash_512(input, input_len, hash);
    default:
      caml_failwith("EverCrypt_Hash_Incremental_hash unimplemented for this algo" + a);
  }
}

//Provides: EverCrypt_Curve25519_scalarmult
//Requires: Hacl_Curve25519_51_scalarmult
function EverCrypt_Curve25519_scalarmult(result, scalar, input) {
  return Hacl_Curve25519_51_scalarmult(result, scalar, input);
}
