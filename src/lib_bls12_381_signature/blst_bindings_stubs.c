#include "blst.h"
#include "blst_misc.h"
#include "caml_bls12_381_stubs.h"
#include "ocaml_integers.h"
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdlib.h>
#include <string.h>

// From ocaml-ctypes:
// https://github.com/ocamllabs/ocaml-ctypes/blob/9048ac78b885cc3debeeb020c56ea91f459a4d33/src/ctypes/ctypes_primitives.h#L110
#if SIZE_MAX == UINT64_MAX
#define ctypes_size_t_val Uint64_val
#define ctypes_copy_size_t integers_copy_uint64
#else
#error "No suitable OCaml type available for representing size_t values"
#endif

// From ocaml/ocaml
// https://github.com/ocaml/ocaml/blob/aca84729327d327eaf6e82f3ae15d0a63953288e/runtime/caml/mlvalues.h#L401
#if OCAML_VERSION < 412000
#define Val_none Val_int(0)
#define Some_val(v) Field(v, 0)
#define Tag_some 0
#define Is_none(v) ((v) == Val_none)
#define Is_some(v) Is_block(v)
#endif

#define Blst_pairing_val(v) (*(blst_pairing **)Data_custom_val(v))

// For signatures
static void finalize_free_pairing(value v) {
  byte *dst = (byte *)blst_pairing_get_dst(Blst_pairing_val(v));
  // See libblst/src/aggregate.c
  if ((uintptr_t)dst != (uintptr_t)42) {
    free(dst);
  }
  free(Blst_pairing_val(v));
}

static struct custom_operations blst_pairing_ops = {
    "blst_pairing",
    finalize_free_pairing,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default};

// Common to both instantiations
CAMLprim value caml_bls12_381_signature_blst_pairing_init_stubs(
    value check, value dst, value dst_length) {
  CAMLparam3(check, dst, dst_length);
  CAMLlocal1(block);
  size_t dst_length_c = ctypes_size_t_val(dst_length);
  // See
  // https://gitlab.com/nomadic-labs/cryptography/ocaml-bls12-381/-/merge_requests/195
  byte *dst_copy = malloc(sizeof(byte) * dst_length_c);
  if (dst_copy == NULL) {
    caml_raise_out_of_memory();
  }
  memcpy(dst_copy, Bytes_val(dst), dst_length_c * sizeof(byte));
  void *p = calloc(1, blst_pairing_sizeof());
  if (p == NULL) {
    free(dst_copy);
    caml_raise_out_of_memory();
  }
  size_t out_of_heap_memory_size =
      blst_pairing_sizeof() + sizeof(byte) * dst_length_c;
  block = caml_alloc_custom_mem(&blst_pairing_ops, sizeof(blst_pairing *),
                                out_of_heap_memory_size);
  blst_pairing **d = (blst_pairing **)Data_custom_val(block);
  *d = p;
  blst_pairing_init(Blst_pairing_val(block), Bool_val(check), dst_copy,
                    dst_length_c);
  CAMLreturn(block);
}

CAMLprim value
caml_bls12_381_signature_blst_pairing_commit_stubs(value buffer) {
  CAMLparam1(buffer);
  blst_pairing_commit(Blst_pairing_val(buffer));
  CAMLreturn(CAML_BLS12_381_OUTPUT_SUCCESS);
}

CAMLprim value
caml_bls12_381_signature_blst_pairing_finalverify_stubs(value buffer) {
  CAMLparam1(buffer);
  bool r = blst_pairing_finalverify(Blst_pairing_val(buffer), NULL);
  CAMLreturn(Val_bool(r));
}

CAMLprim value caml_bls12_381_signature_blst_signature_keygen_stubs(
    value buffer, value ikm, value ikm_length, value key_info,
    value key_info_length) {
  CAMLparam5(buffer, ikm, ikm_length, key_info, key_info_length);
  blst_keygen(Blst_scalar_val(buffer), Bytes_val(ikm),
              ctypes_size_t_val(ikm_length), Bytes_val(key_info),
              ctypes_size_t_val(key_info_length));
  CAMLreturn(CAML_BLS12_381_OUTPUT_SUCCESS);
}

// Pk in G1, Signature in G2
CAMLprim value
caml_bls12_381_signature_blst_sk_to_pk_in_g1_stubs(value buffer, value scalar) {
  CAMLparam2(buffer, scalar);
  blst_sk_to_pk_in_g1(Blst_p1_val(buffer), Blst_scalar_val(scalar));
  CAMLreturn(CAML_BLS12_381_OUTPUT_SUCCESS);
}

CAMLprim value caml_bls12_381_signature_blst_sign_pk_in_g1_stubs(value buffer,
                                                                 value p,
                                                                 value s) {
  CAMLparam3(buffer, p, s);
  blst_sign_pk_in_g1(Blst_p2_val(buffer), Blst_p2_val(p), Blst_scalar_val(s));
  CAMLreturn(CAML_BLS12_381_OUTPUT_SUCCESS);
}

CAMLprim value caml_bls12_381_signature_blst_aggregate_signature_pk_in_g1_stubs(
    value buffer, value g1, value g2, value msg, value msg_length, value aug,
    value aug_length) {
  CAMLparam5(buffer, g1, g2, msg, msg_length);
  CAMLxparam2(aug, aug_length);
  int r = blst_pairing_aggregate_pk_in_g1(
      Blst_pairing_val(buffer), Blst_p1_affine_val(g1), Blst_p2_affine_val(g2),
      Bytes_val(msg), ctypes_size_t_val(msg_length), Bytes_val(aug),
      ctypes_size_t_val(aug_length));
  CAMLreturn(Val_int(r));
}

CAMLprim value
caml_bls12_381_signature_blst_aggregate_signature_pk_in_g1_stubs_bytecode(
    value *argv, int argn) {
  if (argn != 7) {
    caml_failwith("caml_bls12_381_signature_blst_pairing_chk_n_mul_n_aggr_pk_"
                  "in_g2_stubs_bytecode: wrong value argn");
  }
  return caml_bls12_381_signature_blst_aggregate_signature_pk_in_g1_stubs(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}

CAMLprim value
caml_bls12_381_signature_blst_pairing_chk_n_mul_n_aggr_pk_in_g1_stubs(
    value buffer, value pk, value check_pk, value signature,
    value check_signature, value scalar, value nbits, value msg,
    value msg_length, value aug, value aug_length) {
  CAMLparam5(buffer, pk, check_pk, signature, check_signature);
  CAMLxparam5(scalar, nbits, msg, msg_length, aug);
  CAMLxparam1(aug_length);
  blst_p2_affine *signature_c;
  if (Is_none(signature)) {
    signature_c = NULL;
  } else {
    signature_c = Blst_p2_affine_val(Some_val(signature));
  }
  int r = blst_pairing_chk_n_mul_n_aggr_pk_in_g1(
      Blst_pairing_val(buffer), Blst_p1_affine_val(pk), Bool_val(check_pk),
      signature_c, Bool_val(check_signature), Bytes_val(scalar),
      ctypes_size_t_val(nbits), Bytes_val(msg), ctypes_size_t_val(msg_length),
      Bytes_val(aug), ctypes_size_t_val(aug_length));
  CAMLreturn(Val_int(r));
}

CAMLprim value
caml_bls12_381_signature_blst_pairing_chk_n_mul_n_aggr_pk_in_g1_stubs_bytecode(
    value *argv, int argn) {
  if (argn != 11) {
    caml_failwith("caml_bls12_381_signature_blst_pairing_chk_n_mul_n_aggr_pk_"
                  "in_g2_stubs_bytecode: wrong value argn");
  }
  return caml_bls12_381_signature_blst_pairing_chk_n_mul_n_aggr_pk_in_g1_stubs(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7],
      argv[8], argv[9], argv[10]);
}

// Pk in G2, signature in G1
CAMLprim value
caml_bls12_381_signature_blst_sk_to_pk_in_g2_stubs(value buffer, value scalar) {
  CAMLparam2(buffer, scalar);
  blst_sk_to_pk_in_g2(Blst_p2_val(buffer), Blst_scalar_val(scalar));
  CAMLreturn(CAML_BLS12_381_OUTPUT_SUCCESS);
}

CAMLprim value caml_bls12_381_signature_blst_sign_pk_in_g2_stubs(value buffer,
                                                                 value p,
                                                                 value s) {
  CAMLparam3(buffer, p, s);
  blst_sign_pk_in_g2(Blst_p1_val(buffer), Blst_p1_val(p), Blst_scalar_val(s));
  CAMLreturn(CAML_BLS12_381_OUTPUT_SUCCESS);
}

CAMLprim value caml_bls12_381_signature_blst_aggregate_signature_pk_in_g2_stubs(
    value buffer, value g1, value g2, value msg, value msg_length, value aug,
    value aug_length) {
  CAMLparam5(buffer, g1, g2, msg, msg_length);
  CAMLxparam2(aug, aug_length);
  int r = blst_pairing_aggregate_pk_in_g2(
      Blst_pairing_val(buffer), Blst_p2_affine_val(g1), Blst_p1_affine_val(g2),
      Bytes_val(msg), ctypes_size_t_val(msg_length), Bytes_val(aug),
      ctypes_size_t_val(aug_length));
  CAMLreturn(Val_int(r));
}

CAMLprim value
caml_bls12_381_signature_blst_aggregate_signature_pk_in_g2_stubs_bytecode(
    value *argv, int argn) {
  if (argn != 7) {
    caml_failwith("caml_bls12_381_signature_blst_pairing_chk_n_mul_n_aggr_pk_"
                  "in_g2_stubs_bytecode: wrong value argn");
  }
  return caml_bls12_381_signature_blst_aggregate_signature_pk_in_g2_stubs(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}

CAMLprim value
caml_bls12_381_signature_blst_pairing_chk_n_mul_n_aggr_pk_in_g2_stubs(
    value buffer, value pk, value check_pk, value signature,
    value check_signature, value scalar, value nbits, value msg,
    value msg_length, value aug, value aug_length) {
  CAMLparam5(buffer, pk, check_pk, signature, check_signature);
  CAMLxparam5(scalar, nbits, msg, msg_length, aug);
  CAMLxparam1(aug_length);
  blst_p1_affine *signature_c;
  if (Is_none(signature)) {
    signature_c = NULL;
  } else {
    signature_c = Blst_p1_affine_val(Some_val(signature));
  }
  int r = blst_pairing_chk_n_mul_n_aggr_pk_in_g2(
      Blst_pairing_val(buffer), Blst_p2_affine_val(pk), Bool_val(check_pk),
      signature_c, Bool_val(check_signature), Bytes_val(scalar),
      ctypes_size_t_val(nbits), Bytes_val(msg), ctypes_size_t_val(msg_length),
      Bytes_val(aug), ctypes_size_t_val(aug_length));
  CAMLreturn(Val_int(r));
}

CAMLprim value
caml_bls12_381_signature_blst_pairing_chk_n_mul_n_aggr_pk_in_g2_stubs_bytecode(
    value *argv, int argn) {
  if (argn != 11) {
    caml_failwith("caml_bls12_381_signature_blst_pairing_chk_n_mul_n_aggr_pk_"
                  "in_g2_stubs_bytecode: wrong value argn");
  }
  return caml_bls12_381_signature_blst_pairing_chk_n_mul_n_aggr_pk_in_g2_stubs(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7],
      argv[8], argv[9], argv[10]);
}
