(*
  IMPORTANT:
  ocaml_bytes is translated into a C unsigned char* since ctypes 0.17.0
  https://github.com/ocamllabs/ocaml-ctypes/blob/0.17.0/CHANGES.md#ctypes-0170
  and commit: https://github.com/ocamllabs/ocaml-ctypes/commit/f13b5fc0736e226a134a880a366bd2c2f453d066

  char * and unsigned char * correspond to u8 in Rust, but c_uchar used in the
  binding is supposed to be an unsigned char in C.
*)
open Ctypes

module Bindings (F : Cstubs.FOREIGN) = struct
  open F

  let codeunit = uint8_t

  (* We don't load sprout's parameters.
     Parameters of type Rust `usize` are converted to OCaml `int` because they are only file paths.
     NULL is a void pointer.
     FIXME: ptr uchar is used for codeunit, which is uint8 on non WIN32
     machines. It is fine because unsigned char * is exactly uint8*. What about
     WIN32 machines?
     ptr uchar is used to give a NULL value using Ctypes.(from_voidp uchar null)
  *)
  let init_zksnark_params =
    foreign
      "librustzcash_init_zksnark_params"
      (ocaml_bytes @-> size_t @-> ocaml_string @-> ocaml_bytes @-> size_t
     @-> ocaml_string @-> ptr uchar @-> size_t @-> ocaml_string
     @-> returning void)

  let nsk_to_nk =
    foreign
      "librustzcash_nsk_to_nk"
      (ocaml_bytes @-> ocaml_bytes @-> returning void)

  let ask_to_ak =
    foreign
      "librustzcash_ask_to_ak"
      (ocaml_bytes @-> ocaml_bytes @-> returning void)

  let crh_ivk =
    foreign
      "librustzcash_crh_ivk"
      (ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning void)

  let check_diversifier =
    foreign "librustzcash_check_diversifier" (ocaml_bytes @-> returning bool)

  let ivk_to_pkd =
    foreign
      "librustzcash_ivk_to_pkd"
      (ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning bool)

  let sapling_generate_r =
    foreign "librustzcash_sapling_generate_r" (ocaml_bytes @-> returning void)

  let sapling_compute_nf =
    foreign
      "librustzcash_sapling_compute_nf"
      (ocaml_bytes @-> ocaml_bytes @-> uint64_t @-> ocaml_bytes @-> ocaml_bytes
     @-> ocaml_bytes @-> uint64_t @-> ocaml_bytes @-> returning bool)

  let sapling_compute_cm =
    foreign
      "librustzcash_sapling_compute_cm"
      (ocaml_bytes @-> ocaml_bytes @-> uint64_t @-> ocaml_bytes @-> ocaml_bytes
     @-> returning bool)

  let sapling_ka_agree =
    foreign
      "librustzcash_sapling_ka_agree"
      (ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning bool)

  let sapling_ka_derivepublic =
    foreign
      "librustzcash_sapling_ka_derivepublic"
      (ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning bool)

  let sapling_spend_sig =
    foreign
      "librustzcash_sapling_spend_sig"
      (ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
     @-> returning bool)

  let merkle_hash =
    foreign
      "librustzcash_merkle_hash"
      (size_t @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning void)

  let to_scalar =
    foreign
      "librustzcash_to_scalar"
      (ocaml_bytes @-> ocaml_bytes @-> returning void)

  (* ZIP32 functions *)
  let zip32_xsk_master =
    foreign
      "librustzcash_zip32_xsk_master"
      (ocaml_bytes @-> size_t @-> ocaml_bytes @-> returning void)

  let zip32_xfvk_address =
    foreign
      "librustzcash_zip32_xfvk_address"
      (ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
     @-> returning bool)

  let zip32_xsk_derive =
    foreign
      "librustzcash_zip32_xsk_derive"
      (ocaml_bytes @-> uint32_t @-> ocaml_bytes @-> returning void)

  let zip32_xfvk_derive =
    foreign
      "librustzcash_zip32_xfvk_derive"
      (ocaml_bytes @-> uint32_t @-> ocaml_bytes @-> returning bool)

  (* Prover *)
  let proving_ctx_init =
    foreign
      "librustzcash_sapling_proving_ctx_init"
      (void @-> returning (ptr void))

  let proving_ctx_free =
    foreign "librustzcash_sapling_proving_ctx_free" (ptr void @-> returning void)

  let sapling_spend_proof =
    foreign
      "librustzcash_sapling_spend_proof"
      (ptr void @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
     @-> ocaml_bytes @-> uint64_t @-> ocaml_bytes @-> ocaml_bytes
     @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning bool)

  let sapling_binding_sig =
    foreign
      "librustzcash_sapling_binding_sig"
      (ptr void @-> int64_t @-> ocaml_bytes @-> ocaml_bytes @-> returning bool)

  let sapling_output_proof =
    foreign
      "librustzcash_sapling_output_proof"
      (ptr void @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> uint64_t
     @-> ocaml_bytes @-> ocaml_bytes @-> returning bool)

  (* Verifier *)
  let verification_ctx_init =
    foreign
      "librustzcash_sapling_verification_ctx_init"
      (void @-> returning (ptr void))

  let verification_ctx_free =
    foreign
      "librustzcash_sapling_verification_ctx_free"
      (ptr void @-> returning void)

  let sapling_check_spend =
    foreign
      "librustzcash_sapling_check_spend"
      (ptr void @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
     @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning bool)

  let sapling_check_output =
    foreign
      "librustzcash_sapling_check_output"
      (ptr void @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
     @-> returning bool)

  let sapling_final_check =
    foreign
      "librustzcash_sapling_final_check"
      (ptr void @-> int64_t @-> ocaml_bytes @-> ocaml_bytes @-> returning bool)
end
