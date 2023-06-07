// Fr
//Provides: Blst_pairing_val
function Blst_pairing_val(v) {
  return v.v;
}

//Provides: blst_pairing_sizeof
//Requires: wasm_call
function blst_pairing_sizeof() {
  return wasm_call('_blst_pairing_sizeof');
}

//Provides: Blst_pairing
//Requires: blst_pairing_sizeof
//Requires: bls_allocate_mlbytes, bls_free
if (typeof globalThis.FinalizationRegistry === 'function') {
  var blst_pairing_finalizer = new globalThis.FinalizationRegistry(bls_free);
} else {
  var blst_pairing_finalizer = null;
}
function Blst_pairing(dst) {
  this.v = new globalThis.Uint8Array(blst_pairing_sizeof());
  this.dst = bls_allocate_mlbytes(dst);
  if (blst_pairing_finalizer) blst_pairing_finalizer.register(this.v, this.dst);
}

//Provides: caml_bls12_381_signature_blst_pairing_init_stubs
//Requires: wasm_call
//Requires: Blst_pairing_val
//Requires: integers_int32_of_uint32
//Requires: Blst_pairing
function caml_bls12_381_signature_blst_pairing_init_stubs(check, dst, dst_length) {
  var b = new Blst_pairing(dst);
  wasm_call(
      '_blst_pairing_init',
      Blst_pairing_val(b),
      check,
      b.dst,
      integers_int32_of_uint32(dst_length)
  );
  return b;
}

//Provides: caml_bls12_381_signature_blst_pairing_commit_stubs
//Requires: wasm_call
//Requires: Blst_pairing_val
function caml_bls12_381_signature_blst_pairing_commit_stubs(buffer) {
  wasm_call('_blst_pairing_commit', Blst_pairing_val(buffer));
  return 0;
}

//Provides: caml_bls12_381_signature_blst_pairing_finalverify_stubs
//Requires: wasm_call
//Requires: Blst_pairing_val
function caml_bls12_381_signature_blst_pairing_finalverify_stubs(buffer) {
  var r = wasm_call(
      '_blst_pairing_finalverify',
      Blst_pairing_val(buffer),
      null
  );
  return r ? 1 : 0;
}

//Provides: caml_bls12_381_signature_blst_signature_keygen_stubs
//Requires: wasm_call
//Requires: Blst_scalar_val
//Requires: integers_int32_of_uint32
function caml_bls12_381_signature_blst_signature_keygen_stubs(
    buffer,
    ikm,
    ikm_length,
    key_info,
    key_info_length
) {
  wasm_call(
      '_blst_keygen',
      Blst_scalar_val(buffer),
      ikm,
      integers_int32_of_uint32(ikm_length),
      key_info,
      integers_int32_of_uint32(key_info_length)
  );
  return 0;
}

// Pk in G1, Signature in G2

//Provides: caml_bls12_381_signature_blst_sk_to_pk_in_g1_stubs
//Requires: wasm_call
//Requires: Blst_p1_val, Blst_scalar_val
function caml_bls12_381_signature_blst_sk_to_pk_in_g1_stubs(buffer, scalar) {
  wasm_call(
      '_blst_sk_to_pk_in_g1',
      Blst_p1_val(buffer),
      Blst_scalar_val(scalar)
  );
  return 0;
}

//Provides: caml_bls12_381_signature_blst_sign_pk_in_g1_stubs
//Requires: wasm_call
//Requires: Blst_p2_val, Blst_scalar_val
function caml_bls12_381_signature_blst_sign_pk_in_g1_stubs(buffer, p, s) {
  wasm_call(
      '_blst_sign_pk_in_g1',
      Blst_p2_val(buffer),
      Blst_p2_val(p),
      Blst_scalar_val(s)
  );
  return 0;
}

//Provides: caml_bls12_381_signature_blst_aggregate_signature_pk_in_g1_stubs
//Requires: wasm_call
//Requires: Blst_pairing_val, Blst_p1_affine_val, Blst_p2_affine_val
//Requires: integers_int32_of_uint32
function caml_bls12_381_signature_blst_aggregate_signature_pk_in_g1_stubs(
    buffer,
    g1,
    g2,
    msg,
    msg_length,
    aug,
    aug_length
) {
  var r /* int */ = wasm_call(
      '_blst_pairing_aggregate_pk_in_g1',
      Blst_pairing_val(buffer),
      Blst_p1_affine_val(g1),
      Blst_p2_affine_val(g2),
      msg,
      integers_int32_of_uint32(msg_length),
      aug,
      integers_int32_of_uint32(aug_length)
  );
  return r;
}

//Provides: caml_bls12_381_signature_blst_aggregate_signature_pk_in_g1_stubs_bytecode
//Requires: caml_bls12_381_signature_blst_aggregate_signature_pk_in_g1_stubs
function caml_bls12_381_signature_blst_aggregate_signature_pk_in_g1_stubs_bytecode(
    buffer,
    g1,
    g2,
    msg,
    msg_length,
    aug,
    aug_length
) {
  return caml_bls12_381_signature_blst_aggregate_signature_pk_in_g1_stubs(
      buffer,
      g1,
      g2,
      msg,
      msg_length,
      aug,
      aug_length
  );
}

//Provides: caml_bls12_381_signature_blst_pairing_chk_n_mul_n_aggr_pk_in_g1_stubs
//Requires: wasm_call
//Requires: Blst_pairing_val, Blst_p1_affine_val, Blst_p2_affine_val
//Requires: integers_int32_of_uint32
function caml_bls12_381_signature_blst_pairing_chk_n_mul_n_aggr_pk_in_g1_stubs(
    buffer,
    pk,
    check_pk,
    signature,
    check_signature,
    scalar,
    nbits,
    msg,
    msg_length,
    aug,
    aug_length
) {
  var signature_c;
  if (signature == 0) {
    signature_c = null;
  } else {
    signature_c = Blst_p2_affine_val(signature[1]);
  }
  var r /* int */ = wasm_call(
      '_blst_pairing_chk_n_mul_n_aggr_pk_in_g1',
      Blst_pairing_val(buffer),
      Blst_p1_affine_val(pk),
      check_pk,
      signature_c,
      check_signature,
      scalar,
      integers_int32_of_uint32(nbits),
      msg,
      integers_int32_of_uint32(msg_length),
      aug,
      integers_int32_of_uint32(aug_length)
  );
  return r;
}

//Provides: caml_bls12_381_signature_blst_pairing_chk_n_mul_n_aggr_pk_in_g1_stubs_bytecode
//Requires: caml_bls12_381_signature_blst_pairing_chk_n_mul_n_aggr_pk_in_g1_stubs
function caml_bls12_381_signature_blst_pairing_chk_n_mul_n_aggr_pk_in_g1_stubs_bytecode(
    buffer,
    pk,
    check_pk,
    signature,
    check_signature,
    scalar,
    nbits,
    msg,
    msg_length,
    aug,
    aug_length
) {
  return caml_bls12_381_signature_blst_pairing_chk_n_mul_n_aggr_pk_in_g1_stubs(
      buffer,
      pk,
      check_pk,
      signature,
      check_signature,
      scalar,
      nbits,
      msg,
      msg_length,
      aug,
      aug_length
  );
}

// Pk in G2, signature in G1

//Provides: caml_bls12_381_signature_blst_sk_to_pk_in_g2_stubs
//Requires: wasm_call
//Requires: Blst_p2_val, Blst_scalar_val
function caml_bls12_381_signature_blst_sk_to_pk_in_g2_stubs(buffer, scalar) {
  wasm_call(
      '_blst_sk_to_pk_in_g2',
      Blst_p2_val(buffer),
      Blst_scalar_val(scalar)
  );
  return 0;
}

//Provides: caml_bls12_381_signature_blst_sign_pk_in_g2_stubs
//Requires: wasm_call
//Requires: Blst_p1_val, Blst_scalar_val
function caml_bls12_381_signature_blst_sign_pk_in_g2_stubs(buffer, p, s) {
  wasm_call(
      '_blst_sign_pk_in_g2',
      Blst_p1_val(buffer),
      Blst_p1_val(p),
      Blst_scalar_val(s)
  );
  return 0;
}

//Provides: caml_bls12_381_signature_blst_aggregate_signature_pk_in_g2_stubs
//Requires: wasm_call
//Requires: Blst_pairing_val, Blst_p2_affine_val, Blst_p1_affine_val
//Requires: integers_int32_of_uint32
function caml_bls12_381_signature_blst_aggregate_signature_pk_in_g2_stubs(
    buffer,
    g1,
    g2,
    msg,
    msg_length,
    aug,
    aug_length
) {
  var r = wasm_call(
      '_blst_pairing_aggregate_pk_in_g2',
      Blst_pairing_val(buffer),
      Blst_p2_affine_val(g1),
      Blst_p1_affine_val(g2),
      msg,
      integers_int32_of_uint32(msg_length),
      aug,
      integers_int32_of_uint32(aug_length)
  );
  return r;
}

//Provides: caml_bls12_381_signature_blst_aggregate_signature_pk_in_g2_stubs_bytecode
//Requires: caml_bls12_381_signature_blst_aggregate_signature_pk_in_g2_stubs
function caml_bls12_381_signature_blst_aggregate_signature_pk_in_g2_stubs_bytecode(
    buffer,
    g1,
    g2,
    msg,
    msg_length,
    aug,
    aug_length
) {
  return caml_bls12_381_signature_blst_aggregate_signature_pk_in_g2_stubs(
      buffer,
      g1,
      g2,
      msg,
      msg_length,
      aug,
      aug_length
  );
}
//Provides: caml_bls12_381_signature_blst_pairing_chk_n_mul_n_aggr_pk_in_g2_stubs
//Requires: wasm_call
//Requires: Blst_pairing_val, Blst_p2_affine_val
//Requires: Blst_p1_affine_val, Blst_p1_affine
//Requires: integers_int32_of_uint32
function caml_bls12_381_signature_blst_pairing_chk_n_mul_n_aggr_pk_in_g2_stubs(
    buffer,
    pk,
    check_pk,
    signature,
    check_signature,
    scalar,
    nbits,
    msg,
    msg_length,
    aug,
    aug_length
) {
  var signature_c = new Blst_p1_affine();
  if (signature == 0) {
    signature_c = null;
  } else {
    signature_c = Blst_p1_affine_val(signature[1]);
  }
  var r = wasm_call(
      '_blst_pairing_chk_n_mul_n_aggr_pk_in_g2',
      Blst_pairing_val(buffer),
      Blst_p2_affine_val(pk),
      check_pk,
      signature_c,
      check_signature,
      scalar,
      integers_int32_of_uint32(nbits),
      msg,
      integers_int32_of_uint32(msg_length),
      aug,
      integers_int32_of_uint32(aug_length)
  );
  return r;
}
//Provides: caml_bls12_381_signature_blst_pairing_chk_n_mul_n_aggr_pk_in_g2_stubs_bytecode
//Requires: caml_bls12_381_signature_blst_pairing_chk_n_mul_n_aggr_pk_in_g2_stubs
function caml_bls12_381_signature_blst_pairing_chk_n_mul_n_aggr_pk_in_g2_stubs_bytecode(
    buffer,
    pk,
    check_pk,
    signature,
    check_signature,
    scalar,
    nbits,
    msg,
    msg_length,
    aug,
    aug_length
) {
  return caml_bls12_381_signature_blst_pairing_chk_n_mul_n_aggr_pk_in_g2_stubs(
      buffer,
      pk,
      check_pk,
      signature,
      check_signature,
      scalar,
      nbits,
      msg,
      msg_length,
      aug,
      aug_length
  );
}
