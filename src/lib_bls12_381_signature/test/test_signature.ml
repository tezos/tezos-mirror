(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    BLS12_381 signature
   Invocation:   dune exec src/lib_bls12_381_signature/test/main.exe \
                  -- --file test_signature.ml
   Subject:      Test BLS12_381 signature
*)

open Utils

(* Related to sk *)
let test_sk_size_in_bytes () =
  let ikm = generate_random_bytes 32 in
  let sk = Bls12_381_signature.generate_sk ikm in
  assert (
    Bls12_381_signature.sk_size_in_bytes
    = Bytes.length (Bls12_381_signature.sk_to_bytes sk))

let test_sk_of_bytes_exn_and_to_bytes_are_inverse_functions () =
  let bytes = Bls12_381.Fr.(to_bytes (random ())) in
  assert (
    Bytes.equal Bls12_381_signature.(sk_to_bytes (sk_of_bytes_exn bytes)) bytes) ;
  let sk = Bls12_381_signature.(generate_sk (generate_random_bytes 32)) in
  let sk_bytes = Bls12_381_signature.sk_to_bytes sk in
  assert (
    Bytes.equal
      sk_bytes
      Bls12_381_signature.(sk_to_bytes (sk_of_bytes_exn sk_bytes)))

let test_sk_of_bytes_opt_and_to_bytes_are_inverse_functions () =
  let bytes = Bls12_381.Fr.(to_bytes (random ())) in
  assert (
    Bytes.equal
      Bls12_381_signature.(sk_to_bytes (Option.get @@ sk_of_bytes_opt bytes))
      bytes) ;
  let sk = Bls12_381_signature.(generate_sk (generate_random_bytes 32)) in
  let sk_bytes = Bls12_381_signature.sk_to_bytes sk in
  assert (
    Bytes.equal
      sk_bytes
      Bls12_381_signature.(sk_to_bytes (Option.get @@ sk_of_bytes_opt sk_bytes)))

let test_sk_of_bytes_opt_valid_values () =
  let bytes = Bytes.of_string @@ Z.to_bits Bls12_381.Fr.(to_z (random ())) in
  assert (Option.is_some (Bls12_381_signature.sk_of_bytes_opt bytes))

let test_sk_of_bytes_exn_valid_values () =
  let bytes = Bytes.of_string @@ Z.to_bits Bls12_381.Fr.(to_z (random ())) in
  ignore @@ Bls12_381_signature.sk_of_bytes_exn bytes

(* let test_sk_of_bytes_opt_accepts_less_than_32_bytes () = *)
(*   let bytes = generate_random_bytes (Random.int 32) in *)
(*   assert (Option.is_some (Bls12_381_signature.sk_of_bytes_opt bytes)) *)

(* let test_sk_of_bytes_exn_accepts_less_than_32_bytes () = *)
(*   let bytes = generate_random_bytes (Random.int 32) in *)
(*   ignore (Bls12_381_signature.sk_of_bytes_exn bytes) *)

let test_sk_of_bytes_exn_does_not_accept_more_than_32_bytes () =
  let bytes = generate_random_bytes (32 + Random.int 1_000_000) in
  let err_msg =
    "Input should be maximum 32 bytes, encoded the secret key in little endian \
     and must be smaller than the order of Bls12_381.Fr"
  in
  Alcotest.check_raises "" (Invalid_argument err_msg) (fun () ->
      ignore @@ Bls12_381_signature.sk_of_bytes_exn bytes)

let test_sk_of_bytes_opt_does_not_accept_more_than_32_bytes () =
  let bytes = generate_random_bytes (32 + Random.int 1_000_000) in
  assert (Option.is_none (Bls12_381_signature.sk_of_bytes_opt bytes))

let test_sk_of_bytes_opt_does_not_accept_elements_higher_than_the_modulus_but_still_on_32_bytes
    () =
  (* last byte of Bls12_381.Fr.order is 115 *)
  let r =
    Bytes.init 32 (fun i ->
        char_of_int
        @@ if i = 31 then 116 + Random.int (256 - 116) else Random.int 256)
  in
  assert (Option.is_none (Bls12_381_signature.sk_of_bytes_opt r))

let test_sk_of_bytes_exn_does_not_accept_elements_higher_than_the_modulus_but_still_on_32_bytes
    () =
  (* last byte of Bls12_381.Fr.order is 115 *)
  let bytes =
    Bytes.init 32 (fun i ->
        char_of_int
        @@ if i = 31 then 116 + Random.int (256 - 116) else Random.int 256)
  in
  let err_msg =
    "Input should be maximum 32 bytes, encoded the secret key in little endian \
     and must be smaller than the order of Bls12_381.Fr"
  in
  Alcotest.check_raises "" (Invalid_argument err_msg) (fun () ->
      ignore @@ Bls12_381_signature.sk_of_bytes_exn bytes)

let test_keygen_raise_invalid_argument_if_ikm_too_small () =
  ignore
  @@ Alcotest.check_raises
       ""
       (Invalid_argument
          "generate_sk: ikm argument must be at least 32 bytes long")
       (fun () ->
         let ikm = generate_random_bytes (Random.int 32) in
         ignore @@ Bls12_381_signature.generate_sk ikm)

(* Both can be used i.e. MinPk or MinSig. They must share the same interface. *)
module type SIGNATURE_INSTANTIATION = module type of Bls12_381_signature.MinPk

module MakeTestsForInstantiation (MISC : sig
  val sig_basic_filenames : string list

  val sig_aug_filenames : string list

  val sig_pop_filenames : string list

  val pop_filenames : string list

  val pk_not_in_subgroup : string list

  val signature_not_in_subgroup : string list
end)
(PkGroup : Bls12_381.CURVE)
(SigGroup : Bls12_381.CURVE)
(SignatureM : SIGNATURE_INSTANTIATION) =
struct
  let test_pk_size_in_bytes () =
    let ikm = generate_random_bytes 32 in
    let sk = Bls12_381_signature.generate_sk ikm in
    let pk = SignatureM.derive_pk sk in
    assert (
      SignatureM.pk_size_in_bytes = Bytes.length (SignatureM.pk_to_bytes pk))

  let test_signature_size_in_bytes () =
    let ikm = generate_random_bytes 32 in
    let sk = Bls12_381_signature.generate_sk ikm in
    let msg_length = 1 + Random.int 512 in
    let msg = generate_random_bytes msg_length in
    let signature = SignatureM.Basic.sign sk msg in
    assert (
      SignatureM.signature_size_in_bytes
      = Bytes.length (SignatureM.signature_to_bytes signature)) ;
    let signature = SignatureM.Aug.sign sk msg in
    assert (
      SignatureM.signature_size_in_bytes
      = Bytes.length (SignatureM.signature_to_bytes signature)) ;
    let signature = SignatureM.Pop.sign sk msg in
    assert (
      SignatureM.signature_size_in_bytes
      = Bytes.length (SignatureM.signature_to_bytes signature))

  let test_unsafe_pk_of_bytes_does_no_check_on_the_input () =
    let bytes = generate_random_bytes (Random.int 1000) in
    ignore @@ SignatureM.unsafe_pk_of_bytes bytes

  let test_pk_of_bytes_opt_does_check_the_input () =
    let bytes = generate_random_bytes (Random.int 1000) in
    assert (Option.is_none (SignatureM.pk_of_bytes_opt bytes))

  let test_pk_of_bytes_exn_does_check_the_input () =
    let bytes = generate_random_bytes (Random.int 1000) in
    let err_msg =
      Printf.sprintf
        "%s is not a valid public key"
        Hex.(show (`Hex (Bytes.to_string bytes)))
    in
    Alcotest.check_raises "" (Invalid_argument err_msg) (fun () ->
        ignore @@ SignatureM.pk_of_bytes_exn bytes)

  let test_pk_of_bytes_opt_accepts_points_in_the_subgroup_and_in_compressed_form
      () =
    let pk = PkGroup.random () in
    let pk_compressed_bytes = PkGroup.to_compressed_bytes pk in
    assert (Option.is_some (SignatureM.pk_of_bytes_opt pk_compressed_bytes))

  let test_pk_of_bytes_exn_accepts_points_in_the_subgroup_and_in_compressed_form
      () =
    let pk = PkGroup.random () in
    let pk_compressed_bytes = PkGroup.to_compressed_bytes pk in
    ignore @@ SignatureM.pk_of_bytes_exn pk_compressed_bytes

  let test_pk_of_bytes_exn_does_not_accept_points_in_the_subgroup_and_in_uncompressed_form
      () =
    let pk = PkGroup.random () in
    let pk_uncompressed_bytes = PkGroup.to_bytes pk in
    let err_msg =
      Printf.sprintf
        "%s is not a valid public key"
        Hex.(show (`Hex (Bytes.to_string pk_uncompressed_bytes)))
    in
    Alcotest.check_raises "" (Invalid_argument err_msg) (fun () ->
        ignore @@ SignatureM.pk_of_bytes_exn pk_uncompressed_bytes)

  let test_pk_of_bytes_opt_does_not_accept_points_in_the_subgroup_and_in_uncompressed_form
      () =
    let pk = PkGroup.random () in
    let pk_uncompressed_bytes = PkGroup.to_bytes pk in
    assert (Option.is_none (SignatureM.pk_of_bytes_opt pk_uncompressed_bytes))

  let test_pk_to_bytes_of_bytes_exn_are_inverse_functions_on_valid_inputs () =
    let pk = PkGroup.random () in
    let pk_bytes = PkGroup.to_compressed_bytes pk in
    assert (
      Bytes.equal
        pk_bytes
        (SignatureM.pk_to_bytes (SignatureM.pk_of_bytes_exn pk_bytes)))

  let test_pk_to_bytes_of_bytes_opt_are_inverse_functions_on_valid_inputs () =
    let pk = PkGroup.random () in
    let pk_bytes = PkGroup.to_compressed_bytes pk in
    assert (
      Bytes.equal
        pk_bytes
        (SignatureM.pk_to_bytes
           (Option.get (SignatureM.pk_of_bytes_opt pk_bytes))))

  let test_pk_to_bytes_unsafe_of_bytes_are_inverse_functions_on_valid_inputs ()
      =
    let pk = PkGroup.random () in
    let pk_bytes = PkGroup.to_compressed_bytes pk in
    assert (
      Bytes.equal
        pk_bytes
        (SignatureM.pk_to_bytes (SignatureM.unsafe_pk_of_bytes pk_bytes)))

  let test_pk_to_bytes_unsafe_of_bytes_are_inverse_functions_on_any_input () =
    let pk_bytes = generate_random_bytes (Random.int 1000) in
    assert (
      Bytes.equal
        pk_bytes
        (SignatureM.pk_to_bytes (SignatureM.unsafe_pk_of_bytes pk_bytes)))

  let test_unsafe_pk_of_bytes_copies_the_input () =
    let initial_pk_bytes = generate_random_bytes (1 + Random.int 1000) in
    let pk_bytes =
      SignatureM.(pk_to_bytes (unsafe_pk_of_bytes initial_pk_bytes))
    in
    let i = Random.int (Bytes.length initial_pk_bytes) in
    let b_i = Bytes.get initial_pk_bytes i in
    Bytes.set initial_pk_bytes i (char_of_int ((int_of_char b_i + 1) mod 256)) ;
    let res = Bytes.equal pk_bytes initial_pk_bytes in
    assert (not res)

  let test_pk_of_bytes_exn_copies_the_input () =
    let pk = PkGroup.random () in
    let initial_pk_bytes = PkGroup.to_compressed_bytes pk in
    let pk_bytes =
      SignatureM.(pk_to_bytes (pk_of_bytes_exn initial_pk_bytes))
    in
    let i = Random.int (Bytes.length initial_pk_bytes) in
    let b_i = Bytes.get initial_pk_bytes i in
    Bytes.set initial_pk_bytes i (char_of_int ((int_of_char b_i + 1) mod 256)) ;
    let res = Bytes.equal pk_bytes initial_pk_bytes in
    assert (not res)

  let test_pk_of_bytes_opt_copies_the_input () =
    let pk = PkGroup.random () in
    let initial_pk_bytes = PkGroup.to_compressed_bytes pk in
    let pk_bytes =
      SignatureM.(pk_to_bytes (Option.get @@ pk_of_bytes_opt initial_pk_bytes))
    in
    let i = Random.int (Bytes.length initial_pk_bytes) in
    let b_i = Bytes.get initial_pk_bytes i in
    Bytes.set initial_pk_bytes i (char_of_int ((int_of_char b_i + 1) mod 256)) ;
    let res = Bytes.equal pk_bytes initial_pk_bytes in
    assert (not res)

  let test_unsafe_pk_of_bytes_accepts_points_not_in_the_subgroup () =
    List.iter
      (fun pk_bytes_str ->
        let pk_bytes = Hex.(to_bytes (`Hex pk_bytes_str)) in
        ignore @@ SignatureM.unsafe_pk_of_bytes pk_bytes)
      MISC.pk_not_in_subgroup

  let test_pk_of_bytes_exn_does_verify_the_input_represents_a_point_in_the_subgroup
      () =
    List.iter
      (fun pk_bytes_str ->
        let pk_bytes = Hex.(to_bytes (`Hex pk_bytes_str)) in
        let err_msg =
          Printf.sprintf
            "%s is not a valid public key"
            Hex.(show (`Hex (Bytes.to_string pk_bytes)))
        in
        Alcotest.check_raises "" (Invalid_argument err_msg) (fun () ->
            ignore @@ SignatureM.pk_of_bytes_exn pk_bytes))
      MISC.pk_not_in_subgroup

  let test_pk_of_bytes_opt_does_verify_the_input_represents_a_point_in_the_subgroup
      () =
    List.iter
      (fun pk_bytes_str ->
        let pk_bytes = Hex.(to_bytes (`Hex pk_bytes_str)) in
        assert (Option.is_none (SignatureM.pk_of_bytes_opt pk_bytes)))
      MISC.pk_not_in_subgroup

  let test_unsafe_signature_of_bytes_does_no_check_on_the_input () =
    let bytes = generate_random_bytes (Random.int 1000) in
    ignore @@ SignatureM.unsafe_signature_of_bytes bytes

  let test_signature_of_bytes_opt_does_check_the_input () =
    let bytes = generate_random_bytes (Random.int 1000) in
    assert (Option.is_none (SignatureM.signature_of_bytes_opt bytes))

  let test_signature_of_bytes_exn_does_check_the_input () =
    let bytes = generate_random_bytes (Random.int 1000) in
    let err_msg =
      Printf.sprintf
        "%s is not a valid signature"
        Hex.(show (`Hex (Bytes.to_string bytes)))
    in
    Alcotest.check_raises "" (Invalid_argument err_msg) (fun () ->
        ignore @@ SignatureM.signature_of_bytes_exn bytes)

  let test_signature_of_bytes_opt_accepts_points_in_the_subgroup_and_in_compressed_form
      () =
    let r = SigGroup.random () in
    let compressed_bytes = SigGroup.to_compressed_bytes r in
    assert (Option.is_some (SignatureM.signature_of_bytes_opt compressed_bytes))

  let test_signature_of_bytes_exn_accepts_points_in_the_subgroup_and_in_compressed_form
      () =
    let r = SigGroup.random () in
    let compressed_bytes = SigGroup.to_compressed_bytes r in
    ignore @@ SignatureM.signature_of_bytes_exn compressed_bytes

  let test_signature_of_bytes_exn_does_not_accept_points_in_the_subgroup_and_in_uncompressed_form
      () =
    let r = SigGroup.random () in
    let uncompressed_bytes = SigGroup.to_bytes r in
    let err_msg =
      Printf.sprintf
        "%s is not a valid signature"
        Hex.(show (`Hex (Bytes.to_string uncompressed_bytes)))
    in
    Alcotest.check_raises "" (Invalid_argument err_msg) (fun () ->
        ignore @@ SignatureM.signature_of_bytes_exn uncompressed_bytes)

  let test_signature_of_bytes_opt_does_not_accept_points_in_the_subgroup_and_in_uncompressed_form
      () =
    let r = SigGroup.random () in
    let uncompressed_bytes = SigGroup.to_bytes r in
    assert (
      Option.is_none (SignatureM.signature_of_bytes_opt uncompressed_bytes))

  let test_signature_to_bytes_of_bytes_exn_are_inverse_functions_on_valid_inputs
      () =
    let r = SigGroup.random () in
    let bytes = SigGroup.to_compressed_bytes r in
    assert (
      Bytes.equal
        bytes
        (SignatureM.signature_to_bytes
           (SignatureM.signature_of_bytes_exn bytes)))

  let test_signature_to_bytes_of_bytes_opt_are_inverse_functions_on_valid_inputs
      () =
    let r = SigGroup.random () in
    let bytes = SigGroup.to_compressed_bytes r in
    assert (
      Bytes.equal
        bytes
        (SignatureM.signature_to_bytes
           (Option.get (SignatureM.signature_of_bytes_opt bytes))))

  let test_signature_to_bytes_unsafe_of_bytes_are_inverse_functions_on_valid_inputs
      () =
    let r = SigGroup.random () in
    let bytes = SigGroup.to_compressed_bytes r in
    assert (
      Bytes.equal
        bytes
        (SignatureM.signature_to_bytes
           (SignatureM.unsafe_signature_of_bytes bytes)))

  let test_signature_to_bytes_unsafe_of_bytes_are_inverse_functions_on_any_input
      () =
    let bytes = generate_random_bytes (Random.int 1000) in
    assert (
      Bytes.equal
        bytes
        (SignatureM.signature_to_bytes
           (SignatureM.unsafe_signature_of_bytes bytes)))

  let test_unsafe_signature_of_bytes_copies_the_input () =
    let initial_bytes = generate_random_bytes (1 + Random.int 1000) in
    let bytes =
      SignatureM.(signature_to_bytes (unsafe_signature_of_bytes initial_bytes))
    in
    let i = Random.int (Bytes.length initial_bytes) in
    let b_i = Bytes.get initial_bytes i in
    Bytes.set initial_bytes i (char_of_int ((int_of_char b_i + 1) mod 256)) ;
    let res = Bytes.equal bytes initial_bytes in
    assert (not res)

  let test_signature_of_bytes_exn_copies_the_input () =
    let r = SigGroup.random () in
    let initial_bytes = SigGroup.to_compressed_bytes r in
    let bytes =
      SignatureM.(signature_to_bytes (signature_of_bytes_exn initial_bytes))
    in
    let i = Random.int (Bytes.length initial_bytes) in
    let b_i = Bytes.get initial_bytes i in
    Bytes.set initial_bytes i (char_of_int ((int_of_char b_i + 1) mod 256)) ;
    let res = Bytes.equal bytes initial_bytes in
    assert (not res)

  let test_signature_of_bytes_opt_copies_the_input () =
    let r = SigGroup.random () in
    let initial_bytes = SigGroup.to_compressed_bytes r in
    let bytes =
      SignatureM.(
        signature_to_bytes (Option.get @@ signature_of_bytes_opt initial_bytes))
    in
    let i = Random.int (Bytes.length initial_bytes) in
    let b_i = Bytes.get initial_bytes i in
    Bytes.set initial_bytes i (char_of_int ((int_of_char b_i + 1) mod 256)) ;
    let res = Bytes.equal bytes initial_bytes in
    assert (not res)

  let test_unsafe_signature_of_bytes_accepts_points_not_in_the_subgroup () =
    List.iter
      (fun bytes_str ->
        let bytes = Hex.(to_bytes (`Hex bytes_str)) in
        ignore @@ SignatureM.unsafe_signature_of_bytes bytes)
      MISC.signature_not_in_subgroup

  let test_signature_of_bytes_exn_does_verify_the_input_represents_a_point_in_the_subgroup
      () =
    List.iter
      (fun bytes_str ->
        let bytes = Hex.(to_bytes (`Hex bytes_str)) in
        let err_msg =
          Printf.sprintf
            "%s is not a valid signature"
            Hex.(show (`Hex (Bytes.to_string bytes)))
        in
        Alcotest.check_raises "" (Invalid_argument err_msg) (fun () ->
            ignore @@ SignatureM.signature_of_bytes_exn bytes))
      MISC.signature_not_in_subgroup

  let test_signature_of_bytes_opt_does_verify_the_input_represents_a_point_in_the_subgroup
      () =
    List.iter
      (fun bytes_str ->
        let bytes = Hex.(to_bytes (`Hex bytes_str)) in
        assert (Option.is_none (SignatureM.signature_of_bytes_opt bytes)))
      MISC.signature_not_in_subgroup

  let test_pop_prove_verify_with_correct_keys () =
    let ikm = generate_random_bytes 32 in
    let sk = Bls12_381_signature.generate_sk ikm in
    let pk = SignatureM.derive_pk sk in
    (* sign a random message *)
    let proof = SignatureM.Pop.pop_prove sk in
    assert (SignatureM.Pop.pop_verify pk proof)

  let test_pop_prove_verify_with_different_pk_for_verify () =
    let ikm = generate_random_bytes 32 in
    let ikm' = generate_random_bytes 32 in
    let sk = Bls12_381_signature.generate_sk ikm in
    let pk' = SignatureM.(derive_pk (Bls12_381_signature.generate_sk ikm')) in
    (* sign a random message *)
    let proof = SignatureM.Pop.pop_prove sk in
    assert (not (SignatureM.Pop.pop_verify pk' proof))

  let test_pop_verify_random_proof () =
    let ikm = generate_random_bytes 32 in
    let pk = SignatureM.(derive_pk (Bls12_381_signature.generate_sk ikm)) in
    let proof = generate_random_bytes (PkGroup.size_in_bytes / 2) in
    assert (not (SignatureM.Pop.pop_verify pk proof))

  module MakeProperties (Scheme : sig
    val test_vector_filenames : string list

    val sign : Bls12_381_signature.sk -> Bytes.t -> SignatureM.signature

    val verify : SignatureM.pk -> Bytes.t -> SignatureM.signature -> bool

    val name : string
  end) =
  struct
    let test_sign_and_verify_same_message_with_correct_keys () =
      let ikm = generate_random_bytes 32 in
      let sk = Bls12_381_signature.generate_sk ikm in
      let pk = SignatureM.derive_pk sk in
      (* sign a random message *)
      let msg_length = 1 + Random.int 512 in
      let msg = generate_random_bytes msg_length in
      let signature = Scheme.sign sk msg in
      assert (Scheme.verify pk msg signature)

    let test_sign_and_verify_different_message_with_correct_keys () =
      let ikm = generate_random_bytes 32 in
      let sk = Bls12_381_signature.generate_sk ikm in
      let pk = SignatureM.derive_pk sk in
      (* sign a random message *)
      let msg_length = 1 + Random.int 512 in
      let msg = generate_random_bytes msg_length in
      let msg'_length = 1 + Random.int 512 in
      let msg' = generate_random_bytes msg'_length in
      let signature = Scheme.sign sk msg in
      assert (not (Scheme.verify pk msg' signature))

    let test_sign_and_verify_different_message_with_different_keys () =
      let ikm = generate_random_bytes 32 in
      let sk = Bls12_381_signature.generate_sk ikm in
      let _pk = SignatureM.derive_pk sk in

      let ikm' = generate_random_bytes 32 in
      let sk' = Bls12_381_signature.generate_sk ikm' in
      let pk' = SignatureM.derive_pk sk' in

      (* sign a random message *)
      let msg_length = 1 + Random.int 512 in
      let msg = generate_random_bytes msg_length in
      let msg'_length = 1 + Random.int 512 in
      let msg' = generate_random_bytes msg'_length in
      let signature = Scheme.sign sk msg in
      assert (not (Scheme.verify pk' msg' signature))

    let test_sign_and_verify_same_message_with_different_keys () =
      let ikm = generate_random_bytes 32 in
      let sk = Bls12_381_signature.generate_sk ikm in
      let _pk = SignatureM.derive_pk sk in

      let ikm' = generate_random_bytes 32 in
      let sk' = Bls12_381_signature.generate_sk ikm' in
      let pk' = SignatureM.derive_pk sk' in

      (* sign a random message *)
      let msg_length = 1 + Random.int 512 in
      let msg = generate_random_bytes msg_length in
      let signature = Scheme.sign sk msg in
      assert (not (Scheme.verify pk' msg signature))

    let test_full_sign_and_verify_with_different_ikm_sizes () =
      let ikm = generate_random_bytes (32 + Random.int 1000) in
      let sk = Bls12_381_signature.generate_sk ikm in
      let pk = SignatureM.derive_pk sk in
      (* sign a random message *)
      let msg_length = 1 + Random.int 512 in
      let msg = generate_random_bytes msg_length in
      let signature = Scheme.sign sk msg in
      assert (Scheme.verify pk msg signature)

    (* let test_verify_signature_which_represents_point_on_the_curve_but_not_in_the_prime_subgroup *)
    (*     () = *)
    (*   List.iter *)
    (*     (fun signature_hex -> *)
    (*       let ikm = Bytes.init 32 (fun _i -> char_of_int @@ Random.int 256) in *)
    (*       let sk = Bls12_381_signature.generate_sk ikm in *)
    (*       let pk = SignatureM.derive_pk sk in *)
    (*       let msg = Bytes.of_string "Hello" in *)
    (*       let signature = Hex.(to_bytes (`Hex signature_hex)) in *)
    (*       let signature = SignatureM.unsafe_signature_of_bytes signature in *)
    (*       assert (not (Scheme.verify pk msg signature))) *)
    (*     MISC.signature_not_in_subgroup *)

    let test_sign_and_verify_with_a_pk_not_in_the_subgroup () =
      List.iter
        (fun invalid_pk_bytes ->
          let invalid_pk_bytes = Hex.(to_bytes (`Hex invalid_pk_bytes)) in
          let sk =
            Bls12_381_signature.sk_of_bytes_exn Bls12_381.Fr.(to_bytes one)
          in
          let pk = SignatureM.unsafe_pk_of_bytes invalid_pk_bytes in
          let msg = Bytes.of_string "Hello" in
          let signature = Scheme.sign sk msg in
          (* Even when not checking the pk belongs to the subgroup in the
             core_verify algorithm, the verification fails. Better attacks must
             be implemented. *)
          assert (not (Scheme.verify pk msg signature)))
        MISC.pk_not_in_subgroup

    let test_vectors_from_bls_sigs_ref_files () =
      let aux filename =
        let contents = read_file filename in
        List.iter
          (fun content ->
            let contents = String.split_on_char ' ' content in
            let msg_str, ikm_str, expected_result_str =
              (List.nth contents 0, List.nth contents 1, List.nth contents 2)
            in
            let msg = Hex.(to_bytes (`Hex msg_str)) in
            let ikm = Hex.to_bytes (`Hex ikm_str) in
            if Bytes.length ikm < 32 then ()
            else
              let sk = Bls12_381_signature.generate_sk ikm in
              let expected_result = Hex.(to_bytes (`Hex expected_result_str)) in
              let res = Scheme.sign sk msg in
              let res = SignatureM.signature_to_bytes res in
              if not @@ Bytes.equal res expected_result then
                Alcotest.failf
                  "Expected result is %s on input %s with ikm %s, but computed \
                   %s"
                  Hex.(show (Hex.of_bytes expected_result))
                  msg_str
                  ikm_str
                  Hex.(show (Hex.of_bytes res)))
          contents
      in
      List.iter (fun filename -> aux filename) Scheme.test_vector_filenames

    let get_tests () =
      let open Alcotest in
      ( Printf.sprintf "Properties and test vectors for %s" Scheme.name,
        [
          test_case
            "Sign and verify same message with correct keys"
            `Quick
            (Utils.repeat
               100
               test_sign_and_verify_same_message_with_correct_keys);
          test_case
            "Sign and verify different message with correct keys"
            `Quick
            (Utils.repeat
               100
               test_sign_and_verify_different_message_with_correct_keys);
          test_case
            "Sign and verify same message with different keys"
            `Quick
            (Utils.repeat
               100
               test_sign_and_verify_same_message_with_different_keys);
          test_case
            "Sign and verify different message with different keys"
            `Quick
            (Utils.repeat
               100
               test_sign_and_verify_different_message_with_different_keys);
          test_case
            "Test full sign and verify with different ikm sizes"
            `Quick
            (Utils.repeat
               100
               test_full_sign_and_verify_with_different_ikm_sizes);
          test_case
            "Sign and verify with a pk not in the prime subgroup"
            `Quick
            test_sign_and_verify_with_a_pk_not_in_the_subgroup;
          test_case
            "Test vectors from bls_sigs_ref"
            `Quick
            test_vectors_from_bls_sigs_ref_files;
        ] )
  end

  module BasicProperties = MakeProperties (struct
    let test_vector_filenames = MISC.sig_basic_filenames

    let name = "Basic"

    let sign = SignatureM.Basic.sign

    let verify = SignatureM.Basic.verify
  end)

  module AugProperties = MakeProperties (struct
    let test_vector_filenames = MISC.sig_aug_filenames

    let name = "Message augmentation"

    let sign = SignatureM.Aug.sign

    let verify = SignatureM.Aug.verify
  end)

  module PopProperties = MakeProperties (struct
    let test_vector_filenames = MISC.sig_pop_filenames

    let name = "Proof of possession"

    let sign = SignatureM.Pop.sign

    let verify = SignatureM.Pop.verify
  end)

  let test_pop_g2_from_blst_sigs_ref_files () =
    let aux filename =
      let contents = read_file filename in
      List.iter
        (fun content ->
          let contents = String.split_on_char ' ' content in
          let ikm_str, exp_result_str =
            (List.nth contents 1, List.nth contents 2)
          in
          let ikm_bytes = Hex.(to_bytes (`Hex ikm_str)) in
          if Bytes.length ikm_bytes < 32 then ()
          else
            let sk = Bls12_381_signature.generate_sk ikm_bytes in
            let exp_result_bytes = Hex.(to_bytes (`Hex exp_result_str)) in
            let res = SignatureM.Pop.pop_prove sk in
            if not @@ Bytes.equal res exp_result_bytes then
              Alcotest.failf
                "Expected result is %s with ikm %s, but computed %s"
                exp_result_str
                ikm_str
                Hex.(show (Hex.of_bytes res)))
        contents
    in
    List.iter (fun filename -> aux filename) MISC.pop_filenames

  let get_tests () =
    let open Alcotest in
    [
      ( "Size in bytes",
        [
          test_case "pk" `Quick test_pk_size_in_bytes;
          test_case "signature" `Quick test_signature_size_in_bytes;
        ] );
      ( "Auxiliary functions",
        [
          test_case
            "unsafe_pk_of_bytes does no check on the input"
            `Quick
            test_unsafe_pk_of_bytes_does_no_check_on_the_input;
          test_case
            "pk_of_bytes_exn accepts points in the subgroup and in compressed \
             form"
            `Quick
            (Utils.repeat
               10
               test_pk_of_bytes_exn_accepts_points_in_the_subgroup_and_in_compressed_form);
          test_case
            "pk_of_bytes_opt accepts points in the subgroup and in compressed \
             form"
            `Quick
            (Utils.repeat
               10
               test_pk_of_bytes_opt_accepts_points_in_the_subgroup_and_in_compressed_form);
          test_case
            "pk_of_bytes_exn does not accept points in the subgroup and in \
             uncompressed form"
            `Quick
            (Utils.repeat
               10
               test_pk_of_bytes_exn_does_not_accept_points_in_the_subgroup_and_in_uncompressed_form);
          test_case
            "pk_of_bytes_opt does not accept points in the subgroup and in \
             uncompressed form"
            `Quick
            (Utils.repeat
               10
               test_pk_of_bytes_opt_does_not_accept_points_in_the_subgroup_and_in_uncompressed_form);
          test_case
            "unsafe_pk_of_bytes copies the input"
            `Quick
            (Utils.repeat 10 test_unsafe_pk_of_bytes_copies_the_input);
          test_case
            "pk_of_bytes_exn copies the input"
            `Quick
            (Utils.repeat 10 test_pk_of_bytes_exn_copies_the_input);
          test_case
            "pk_of_bytes_opt copies the input"
            `Quick
            (Utils.repeat 10 test_pk_of_bytes_opt_copies_the_input);
          test_case
            "unsafe_pk_of_bytes accepts points not in the subgroup"
            `Quick
            test_unsafe_pk_of_bytes_accepts_points_not_in_the_subgroup;
          test_case
            "pk_of_bytes_exn does check the input"
            `Quick
            test_pk_of_bytes_exn_does_check_the_input;
          test_case
            "pk_of_bytes_opt and pk_to_bytes are inverse functions on valid \
             inputs"
            `Quick
            (Utils.repeat
               10
               test_pk_to_bytes_of_bytes_opt_are_inverse_functions_on_valid_inputs);
          test_case
            "unsafe_pk_of_bytes and pk_to_bytes are inverse functions on valid \
             inputs"
            `Quick
            (Utils.repeat
               10
               test_pk_to_bytes_unsafe_of_bytes_are_inverse_functions_on_valid_inputs);
          test_case
            "pk_of_bytes_exn and pk_to_bytes are inverse functions on valid \
             inputs"
            `Quick
            (Utils.repeat
               10
               test_pk_to_bytes_of_bytes_exn_are_inverse_functions_on_valid_inputs);
          test_case
            "unsafe_pk_of_bytes and pk_to_bytes are inverse functions on any \
             valid input"
            `Quick
            (Utils.repeat
               10
               test_pk_to_bytes_unsafe_of_bytes_are_inverse_functions_on_any_input);
          test_case
            "pk_of_bytes_exn does verify the input represents a point in the \
             subgroup"
            `Quick
            test_pk_of_bytes_exn_does_verify_the_input_represents_a_point_in_the_subgroup;
          test_case
            "pk_of_bytes_opt does verify the input represents a point in the \
             subgroup"
            `Quick
            test_pk_of_bytes_opt_does_verify_the_input_represents_a_point_in_the_subgroup;
          test_case
            "pk_of_bytes_opt does check the input"
            `Quick
            test_pk_of_bytes_opt_does_check_the_input;
          test_case
            "unsafe_signature_of_bytes does no check on the input"
            `Quick
            test_unsafe_signature_of_bytes_does_no_check_on_the_input;
          test_case
            "signature_of_bytes_exn accepts points in the subgroup and in \
             compressed form"
            `Quick
            (Utils.repeat
               10
               test_signature_of_bytes_exn_accepts_points_in_the_subgroup_and_in_compressed_form);
          test_case
            "signature_of_bytes_opt accepts points in the subgroup and in \
             compressed form"
            `Quick
            (Utils.repeat
               10
               test_signature_of_bytes_opt_accepts_points_in_the_subgroup_and_in_compressed_form);
          test_case
            "signature_of_bytes_exn does not accept points in the subgroup and \
             in uncompressed form"
            `Quick
            (Utils.repeat
               10
               test_signature_of_bytes_exn_does_not_accept_points_in_the_subgroup_and_in_uncompressed_form);
          test_case
            "signature_of_bytes_opt does not accept points in the subgroup and \
             in uncompressed form"
            `Quick
            (Utils.repeat
               10
               test_signature_of_bytes_opt_does_not_accept_points_in_the_subgroup_and_in_uncompressed_form);
          test_case
            "unsafe_signature_of_bytes copies the input"
            `Quick
            (Utils.repeat 10 test_unsafe_signature_of_bytes_copies_the_input);
          test_case
            "signature_of_bytes_exn copies the input"
            `Quick
            (Utils.repeat 10 test_signature_of_bytes_exn_copies_the_input);
          test_case
            "signature_of_bytes_opt copies the input"
            `Quick
            (Utils.repeat 10 test_signature_of_bytes_opt_copies_the_input);
          test_case
            "unsafe_signature_of_bytes accepts points not in the subgroup"
            `Quick
            test_unsafe_signature_of_bytes_accepts_points_not_in_the_subgroup;
          test_case
            "signature_of_bytes_exn does check the input"
            `Quick
            test_signature_of_bytes_exn_does_check_the_input;
          test_case
            "signature_of_bytes_opt and signature_to_bytes are inverse \
             functions on valid inputs"
            `Quick
            (Utils.repeat
               10
               test_signature_to_bytes_of_bytes_opt_are_inverse_functions_on_valid_inputs);
          test_case
            "unsafe_signature_of_bytes and signature_to_bytes are inverse \
             functions on valid inputs"
            `Quick
            (Utils.repeat
               10
               test_signature_to_bytes_unsafe_of_bytes_are_inverse_functions_on_valid_inputs);
          test_case
            "signature_of_bytes_exn and signature_to_bytes are inverse \
             functions on valid inputs"
            `Quick
            (Utils.repeat
               10
               test_signature_to_bytes_of_bytes_exn_are_inverse_functions_on_valid_inputs);
          test_case
            "unsafe_signature_of_bytes and signature_to_bytes are inverse \
             functions on any valid input"
            `Quick
            (Utils.repeat
               10
               test_signature_to_bytes_unsafe_of_bytes_are_inverse_functions_on_any_input);
          test_case
            "signature_of_bytes_exn does verify the input represents a point \
             in the subgroup"
            `Quick
            test_signature_of_bytes_exn_does_verify_the_input_represents_a_point_in_the_subgroup;
          test_case
            "signature_of_bytes_opt does verify the input represents a point \
             in the subgroup"
            `Quick
            test_signature_of_bytes_opt_does_verify_the_input_represents_a_point_in_the_subgroup;
          test_case
            "signature_of_bytes_opt does check the input"
            `Quick
            test_signature_of_bytes_opt_does_check_the_input;
        ] );
      BasicProperties.get_tests ();
      AugProperties.get_tests ();
      PopProperties.get_tests ();
      ( "Proof of possession proof/verify properties and test vectors",
        [
          test_case
            "Pop G2 from file"
            `Quick
            test_pop_g2_from_blst_sigs_ref_files;
          test_case
            "Prove and verify with correct keys"
            `Quick
            (Utils.repeat 10 test_pop_prove_verify_with_correct_keys);
          test_case
            "Prove and verify with different pk for verify"
            `Quick
            (Utils.repeat 10 test_pop_prove_verify_with_different_pk_for_verify);
          test_case
            "Verify random proof"
            `Quick
            (Utils.repeat 10 test_pop_verify_random_proof);
        ] );
    ]
end

let path_test_vectors name =
  project_root // Filename.dirname __FILE__ // "test_vectors" // name

let () =
  let open Alcotest in
  let module TestMinPk =
    MakeTestsForInstantiation
      (struct
        let sig_basic_filenames =
          [
            path_test_vectors "sig_g2_basic"
            // "sig_g2_basic_fips_186_3_B233_blst";
            path_test_vectors "sig_g2_basic"
            // "sig_g2_basic_fips_186_3_B283_blst";
            path_test_vectors "sig_g2_basic"
            // "sig_g2_basic_fips_186_3_B409_blst";
            path_test_vectors "sig_g2_basic"
            // "sig_g2_basic_fips_186_3_B571_blst";
            path_test_vectors "sig_g2_basic"
            // "sig_g2_basic_fips_186_3_K233_blst";
            path_test_vectors "sig_g2_basic"
            // "sig_g2_basic_fips_186_3_K409_blst";
            path_test_vectors "sig_g2_basic"
            // "sig_g2_basic_fips_186_3_K571_blst";
            path_test_vectors "sig_g2_basic"
            // "sig_g2_basic_fips_186_3_P224_blst";
            path_test_vectors "sig_g2_basic"
            // "sig_g2_basic_fips_186_3_P256_blst";
            path_test_vectors "sig_g2_basic"
            // "sig_g2_basic_fips_186_3_P384_blst";
            path_test_vectors "sig_g2_basic"
            // "sig_g2_basic_fips_186_3_P521_blst";
            path_test_vectors "sig_g2_basic" // "sig_g2_basic_rfc6979_blst";
          ]

        let sig_aug_filenames =
          [
            path_test_vectors "sig_g2_aug" // "sig_g2_aug_fips_186_3_B233_blst";
            path_test_vectors "sig_g2_aug" // "sig_g2_aug_fips_186_3_B283_blst";
            path_test_vectors "sig_g2_aug" // "sig_g2_aug_fips_186_3_B409_blst";
            path_test_vectors "sig_g2_aug" // "sig_g2_aug_fips_186_3_B571_blst";
            path_test_vectors "sig_g2_aug" // "sig_g2_aug_fips_186_3_K233_blst";
            path_test_vectors "sig_g2_aug" // "sig_g2_aug_fips_186_3_K409_blst";
            path_test_vectors "sig_g2_aug" // "sig_g2_aug_fips_186_3_K571_blst";
            path_test_vectors "sig_g2_aug" // "sig_g2_aug_fips_186_3_P224_blst";
            path_test_vectors "sig_g2_aug" // "sig_g2_aug_fips_186_3_P256_blst";
            path_test_vectors "sig_g2_aug" // "sig_g2_aug_fips_186_3_P384_blst";
            path_test_vectors "sig_g2_aug" // "sig_g2_aug_fips_186_3_P521_blst";
            path_test_vectors "sig_g2_aug" // "sig_g2_aug_rfc6979_blst";
          ]

        let sig_pop_filenames =
          [
            path_test_vectors "sig_g2_pop" // "sig_g2_pop_fips_186_3_B233_blst";
            path_test_vectors "sig_g2_pop" // "sig_g2_pop_fips_186_3_B283_blst";
            path_test_vectors "sig_g2_pop" // "sig_g2_pop_fips_186_3_B409_blst";
            path_test_vectors "sig_g2_pop" // "sig_g2_pop_fips_186_3_B571_blst";
            path_test_vectors "sig_g2_pop" // "sig_g2_pop_fips_186_3_K233_blst";
            path_test_vectors "sig_g2_pop" // "sig_g2_pop_fips_186_3_K409_blst";
            path_test_vectors "sig_g2_pop" // "sig_g2_pop_fips_186_3_K571_blst";
            path_test_vectors "sig_g2_pop" // "sig_g2_pop_fips_186_3_P224_blst";
            path_test_vectors "sig_g2_pop" // "sig_g2_pop_fips_186_3_P256_blst";
            path_test_vectors "sig_g2_pop" // "sig_g2_pop_fips_186_3_P384_blst";
            path_test_vectors "sig_g2_pop" // "sig_g2_pop_fips_186_3_P521_blst";
            path_test_vectors "sig_g2_pop" // "sig_g2_pop_rfc6979_blst";
          ]

        (* These elements have been generated by bls12-381-unix-blst, commit
           ffa2ad5f1c882f05d64c7cb1633c6256b08513bf, by removing the
           multiplication by the cofactor in the random generator. It can be
           verified the elements are not in the prime subgroup by checking
           multiplying by Fr.order does not give zero. *)
        let pk_not_in_subgroup =
          [
            "a4c9678ad327129f4388e7f7ff781fc8e98d181add820b79d15facdca422b3ee7fb20f7082a7f9b7c7915053191cb013";
            "97ae9b4dc6a05cda8bc833dfb983e41423d224bbf6954ce4721a50364a2b37643e18a276ce19b07b83a333f90e2de6c2";
            "b1be8c9f94c1435227b9a18fb57a6d9932c1670d16c514d2d9d67839cc0cc19afdcd114d6e06bf8eb8394061bf880bd4";
            "b173357ce7e2340dc64c6a5633e6800683fb0a6c0f4af92b383425bd76d915819252ac9459e79a1bae530ea0145338cb";
            "a53944773013669c2722949399322703c0b92d877e52b95e0309bdf286d8290314763d61952d6812da50c1826bcaf4c3";
            "8fd2557441f4076917ffe8dfb0e12270994351661600e72f48fe654198199f6cc625a041ce3c9b7c765b32cb53e77192";
          ]

        (* These points are on the curve, but not in the subgroup, see
           test_g2.ml to see how it has been generated. It is given in the
           compressed form as required for the signature. *)
        let signature_not_in_subgroup =
          [
            "a7da246233ad216e60ee03070a0916154ae9f9dc23310c1191dfb4e277fc757f58a5cf5bdf7a9f322775143c37539cb90798205fd56217b682d5656f7ac7bc0da111dee59d3f863f1b040be659eda7941afb9f1bc5d0fe2beb5e2385e2cfe9ee";
            "b112717bbcd089ea99e8216eab455ea5cd462b0b3e3530303b83477f8e1bb7abca269fec10b3eb998f7f6fd1799d58ff11ed0a53bf75f91d2bf73d11bd52d061f401ac6a6ec0ef4a163e480bac85e75b97cb556f500057b9ef4b28bfe196791d";
            "86e5fa411047d9632c95747bea64d973757904c935ac0741b9eeefa2c7c4e439baf1d2c1e8633ba6c884ed9fdf1ffbdd129a32c046f355c5126254973115d6df32904498db6ca959d5bf1869f235be4c0e60fc334ed493f864476907cadfef2c";
            "88c83e90520a5ea31733cc01e3589e10b2ed755e2faade29199f97645fbf73f52b29297c22a3b1c4fcd3379bceeec832091df6fb3b9d23f04e8267fc41e578002484155562e70f488c2a4c6b11522c66736bc977755c257478f3022656abb630";
            "a25099811f52ad463c762197466c476a03951afdb3f0a457efa2b9475376652fba7b2d56f3184dad540a234d471c53a113203f73dd661694586c75d9c418d34cd16504356253e3ba4618f61cbee02880a43efeacb8f9fe1fdc84ceec4f780ba2";
            "990f5e1d200d1b9ab842c516ce50992730917a8b2e95ee1a4b830d7d9507c6846ace7a0eed8831a8d1f1e233cd24581215fe8fe85a99f4ca3fe046dba8ac6377fc3c10d73fa94b25c2d534d7a587a507b498754a2534cd85777b2a7f2978eec6";
            "a29415562a1d18b11ec8ab2e0b347a9417f9e904cf25f9b1dc40f235507814371fb4568cc1070a0b8c7baf39e0039d1e0b49d4352b095883ccc262e23d8651c49c39c06d0a920d40b2765d550a78c4c1940c8a2b6843a0063402c169f079f0ae";
            "8a257ed6d95cb226c3eb57218bd075ba27164fc1b972c4230ee70c7b81c89d38253ccf7ed2896aa5eb3d9fd6021fac000e368080e705f2a65c919539e2d28e6dd1117296b4210fd56db8d96891f8586bd333e9c47f838ed436659a1dafaee16c";
          ]

        let pop_filenames =
          [
            path_test_vectors "pop_g2" // "pop_g2_fips_186_3_B233_blst";
            path_test_vectors "pop_g2" // "pop_g2_fips_186_3_B283_blst";
            path_test_vectors "pop_g2" // "pop_g2_fips_186_3_B409_blst";
            path_test_vectors "pop_g2" // "pop_g2_fips_186_3_B571_blst";
            path_test_vectors "pop_g2" // "pop_g2_fips_186_3_K233_blst";
            path_test_vectors "pop_g2" // "pop_g2_fips_186_3_K409_blst";
            path_test_vectors "pop_g2" // "pop_g2_fips_186_3_K571_blst";
            path_test_vectors "pop_g2" // "pop_g2_fips_186_3_P224_blst";
            path_test_vectors "pop_g2" // "pop_g2_fips_186_3_P256_blst";
            path_test_vectors "pop_g2" // "pop_g2_fips_186_3_P384_blst";
            path_test_vectors "pop_g2" // "pop_g2_fips_186_3_P521_blst";
            path_test_vectors "pop_g2" // "pop_g2_rfc6979_blst";
          ]
      end)
      (Bls12_381.G1)
      (Bls12_381.G2)
      (Bls12_381_signature.MinPk)
  in
  let module TestMinSig =
    MakeTestsForInstantiation
      (struct
        let sig_basic_filenames =
          [
            path_test_vectors "sig_g1_basic"
            // "sig_g1_basic_fips_186_3_B233_blst";
            path_test_vectors "sig_g1_basic"
            // "sig_g1_basic_fips_186_3_B283_blst";
            path_test_vectors "sig_g1_basic"
            // "sig_g1_basic_fips_186_3_B409_blst";
            path_test_vectors "sig_g1_basic"
            // "sig_g1_basic_fips_186_3_B571_blst";
            path_test_vectors "sig_g1_basic"
            // "sig_g1_basic_fips_186_3_K233_blst";
            path_test_vectors "sig_g1_basic"
            // "sig_g1_basic_fips_186_3_K409_blst";
            path_test_vectors "sig_g1_basic"
            // "sig_g1_basic_fips_186_3_K571_blst";
            path_test_vectors "sig_g1_basic"
            // "sig_g1_basic_fips_186_3_P224_blst";
            path_test_vectors "sig_g1_basic"
            // "sig_g1_basic_fips_186_3_P256_blst";
            path_test_vectors "sig_g1_basic"
            // "sig_g1_basic_fips_186_3_P384_blst";
            path_test_vectors "sig_g1_basic"
            // "sig_g1_basic_fips_186_3_P521_blst";
            path_test_vectors "sig_g1_basic" // "sig_g1_basic_rfc6979_blst";
          ]

        let sig_aug_filenames =
          [
            path_test_vectors "sig_g1_aug" // "sig_g1_aug_fips_186_3_B233_blst";
            path_test_vectors "sig_g1_aug" // "sig_g1_aug_fips_186_3_B283_blst";
            path_test_vectors "sig_g1_aug" // "sig_g1_aug_fips_186_3_B409_blst";
            path_test_vectors "sig_g1_aug" // "sig_g1_aug_fips_186_3_B571_blst";
            path_test_vectors "sig_g1_aug" // "sig_g1_aug_fips_186_3_K233_blst";
            path_test_vectors "sig_g1_aug" // "sig_g1_aug_fips_186_3_K409_blst";
            path_test_vectors "sig_g1_aug" // "sig_g1_aug_fips_186_3_K571_blst";
            path_test_vectors "sig_g1_aug" // "sig_g1_aug_fips_186_3_P224_blst";
            path_test_vectors "sig_g1_aug" // "sig_g1_aug_fips_186_3_P256_blst";
            path_test_vectors "sig_g1_aug" // "sig_g1_aug_fips_186_3_P384_blst";
            path_test_vectors "sig_g1_aug" // "sig_g1_aug_fips_186_3_P521_blst";
            path_test_vectors "sig_g1_aug" // "sig_g1_aug_rfc6979_blst";
          ]

        let sig_pop_filenames =
          [
            path_test_vectors "sig_g1_pop" // "sig_g1_pop_fips_186_3_B233_blst";
            path_test_vectors "sig_g1_pop" // "sig_g1_pop_fips_186_3_B283_blst";
            path_test_vectors "sig_g1_pop" // "sig_g1_pop_fips_186_3_B409_blst";
            path_test_vectors "sig_g1_pop" // "sig_g1_pop_fips_186_3_B571_blst";
            path_test_vectors "sig_g1_pop" // "sig_g1_pop_fips_186_3_K233_blst";
            path_test_vectors "sig_g1_pop" // "sig_g1_pop_fips_186_3_K409_blst";
            path_test_vectors "sig_g1_pop" // "sig_g1_pop_fips_186_3_K571_blst";
            path_test_vectors "sig_g1_pop" // "sig_g1_pop_fips_186_3_P224_blst";
            path_test_vectors "sig_g1_pop" // "sig_g1_pop_fips_186_3_P256_blst";
            path_test_vectors "sig_g1_pop" // "sig_g1_pop_fips_186_3_P384_blst";
            path_test_vectors "sig_g1_pop" // "sig_g1_pop_fips_186_3_P521_blst";
            path_test_vectors "sig_g1_pop" // "sig_g1_pop_rfc6979_blst";
          ]

        (* These elements have been generated by bls12-381-unix-blst, commit
           ffa2ad5f1c882f05d64c7cb1633c6256b08513bf, by removing the
           multiplication by the cofactor in the random generator. It can be
           verified the elements are not in the prime subgroup by checking
           multiplying by Fr.order does not give zero. *)
        let signature_not_in_subgroup =
          [
            "a4c9678ad327129f4388e7f7ff781fc8e98d181add820b79d15facdca422b3ee7fb20f7082a7f9b7c7915053191cb013";
            "97ae9b4dc6a05cda8bc833dfb983e41423d224bbf6954ce4721a50364a2b37643e18a276ce19b07b83a333f90e2de6c2";
            "b1be8c9f94c1435227b9a18fb57a6d9932c1670d16c514d2d9d67839cc0cc19afdcd114d6e06bf8eb8394061bf880bd4";
            "b173357ce7e2340dc64c6a5633e6800683fb0a6c0f4af92b383425bd76d915819252ac9459e79a1bae530ea0145338cb";
            "a53944773013669c2722949399322703c0b92d877e52b95e0309bdf286d8290314763d61952d6812da50c1826bcaf4c3";
            "8fd2557441f4076917ffe8dfb0e12270994351661600e72f48fe654198199f6cc625a041ce3c9b7c765b32cb53e77192";
          ]

        (* These points are on the curve, but not in the subgroup, see
           test_g2.ml to see how it has been generated. It is given in the
           compressed form as required for the signature. *)
        let pk_not_in_subgroup =
          [
            "a7da246233ad216e60ee03070a0916154ae9f9dc23310c1191dfb4e277fc757f58a5cf5bdf7a9f322775143c37539cb90798205fd56217b682d5656f7ac7bc0da111dee59d3f863f1b040be659eda7941afb9f1bc5d0fe2beb5e2385e2cfe9ee";
            "b112717bbcd089ea99e8216eab455ea5cd462b0b3e3530303b83477f8e1bb7abca269fec10b3eb998f7f6fd1799d58ff11ed0a53bf75f91d2bf73d11bd52d061f401ac6a6ec0ef4a163e480bac85e75b97cb556f500057b9ef4b28bfe196791d";
            "86e5fa411047d9632c95747bea64d973757904c935ac0741b9eeefa2c7c4e439baf1d2c1e8633ba6c884ed9fdf1ffbdd129a32c046f355c5126254973115d6df32904498db6ca959d5bf1869f235be4c0e60fc334ed493f864476907cadfef2c";
            "88c83e90520a5ea31733cc01e3589e10b2ed755e2faade29199f97645fbf73f52b29297c22a3b1c4fcd3379bceeec832091df6fb3b9d23f04e8267fc41e578002484155562e70f488c2a4c6b11522c66736bc977755c257478f3022656abb630";
            "a25099811f52ad463c762197466c476a03951afdb3f0a457efa2b9475376652fba7b2d56f3184dad540a234d471c53a113203f73dd661694586c75d9c418d34cd16504356253e3ba4618f61cbee02880a43efeacb8f9fe1fdc84ceec4f780ba2";
            "990f5e1d200d1b9ab842c516ce50992730917a8b2e95ee1a4b830d7d9507c6846ace7a0eed8831a8d1f1e233cd24581215fe8fe85a99f4ca3fe046dba8ac6377fc3c10d73fa94b25c2d534d7a587a507b498754a2534cd85777b2a7f2978eec6";
            "a29415562a1d18b11ec8ab2e0b347a9417f9e904cf25f9b1dc40f235507814371fb4568cc1070a0b8c7baf39e0039d1e0b49d4352b095883ccc262e23d8651c49c39c06d0a920d40b2765d550a78c4c1940c8a2b6843a0063402c169f079f0ae";
            "8a257ed6d95cb226c3eb57218bd075ba27164fc1b972c4230ee70c7b81c89d38253ccf7ed2896aa5eb3d9fd6021fac000e368080e705f2a65c919539e2d28e6dd1117296b4210fd56db8d96891f8586bd333e9c47f838ed436659a1dafaee16c";
          ]

        let pop_filenames =
          [
            path_test_vectors "pop_g1" // "pop_g1_fips_186_3_B233_blst";
            path_test_vectors "pop_g1" // "pop_g1_fips_186_3_B283_blst";
            path_test_vectors "pop_g1" // "pop_g1_fips_186_3_B409_blst";
            path_test_vectors "pop_g1" // "pop_g1_fips_186_3_B571_blst";
            path_test_vectors "pop_g1" // "pop_g1_fips_186_3_K233_blst";
            path_test_vectors "pop_g1" // "pop_g1_fips_186_3_K409_blst";
            path_test_vectors "pop_g1" // "pop_g1_fips_186_3_K571_blst";
            path_test_vectors "pop_g1" // "pop_g1_fips_186_3_P224_blst";
            path_test_vectors "pop_g1" // "pop_g1_fips_186_3_P256_blst";
            path_test_vectors "pop_g1" // "pop_g1_fips_186_3_P384_blst";
            path_test_vectors "pop_g1" // "pop_g1_fips_186_3_P521_blst";
            path_test_vectors "pop_g1" // "pop_g1_rfc6979_blst";
          ]
      end)
      (Bls12_381.G2)
      (Bls12_381.G1)
      (Bls12_381_signature.MinSig)
  in
  let min_pk_tests = TestMinPk.get_tests () in
  let min_pk_tests =
    List.map (fun (s, tests) -> ("MinPk " ^ s, tests)) min_pk_tests
  in
  let min_sig_tests = TestMinSig.get_tests () in
  let min_sig_tests =
    List.map (fun (s, tests) -> ("MinSig " ^ s, tests)) min_sig_tests
  in
  let all_tests = List.concat [min_pk_tests; min_sig_tests] in
  run
    ~__FILE__
    "BLS Signature"
    (( "Common features to both instanciations",
       [
         test_case "Size in bytes of sk" `Quick test_sk_size_in_bytes;
         test_case
           "sk_of_bytes_opt and sk_to_bytes are inverse functions"
           `Quick
           test_sk_of_bytes_opt_and_to_bytes_are_inverse_functions;
         test_case
           "sk_of_bytes_exn and sk_to_bytes are inverse functions"
           `Quick
           test_sk_of_bytes_exn_and_to_bytes_are_inverse_functions;
         test_case
           "sk_of_bytes_opt valid values"
           `Quick
           test_sk_of_bytes_opt_valid_values;
         test_case
           "sk_of_bytes_exn does not accept more than 32 bytes"
           `Quick
           test_sk_of_bytes_exn_does_not_accept_more_than_32_bytes;
         test_case
           "sk_of_bytes_opt does not accept more than 32 bytes"
           `Quick
           test_sk_of_bytes_opt_does_not_accept_more_than_32_bytes;
         test_case
           "sk_of_bytes_opt does not accept values higher than Fr modules but \
            still on 32 bytes"
           `Quick
           test_sk_of_bytes_opt_does_not_accept_elements_higher_than_the_modulus_but_still_on_32_bytes;
         test_case
           "sk_of_bytes_exn does not accept values higher than Fr modules but \
            still on 32 bytes"
           `Quick
           test_sk_of_bytes_exn_does_not_accept_elements_higher_than_the_modulus_but_still_on_32_bytes;
         test_case
           "sk_of_bytes_exn valid values"
           `Quick
           test_sk_of_bytes_exn_valid_values;
         test_case
           "generate_sk raises Invalid_argument is ikm is smaller than 32 bytes"
           `Quick
           test_keygen_raise_invalid_argument_if_ikm_too_small;
       ] )
    :: all_tests)
