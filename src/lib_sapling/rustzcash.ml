(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(* Positions and amounts in librustzcash are a uint64 but we cast them to int64
   for simplicity. Both values are only provided as arguments and not returned by
   any function.
   Amounts are bounded by [-max_amount, +max_amount] so it fits in a int64.
   The positions we use are bounded by [ 0 ; 4294967295], which is an uint32, but
   for simplicity they are stored in a int64.
   Functions in librustzcash return [false] when their arguments are malformed,
   and return [true] otherwise (in which case result buffer contains the
   result). Because we lean on the OCaml type system to enforce that arguments
   are well-formed, we simply [assert] on the return value of the rust bindings.
*)

(* Ctypes binding. We encapsulate the binding in a specific module *)
module RS = Rustzcash_ctypes_bindings.Bindings (Rustzcash_ctypes_stubs)

module T : Rustzcash_sig.T = struct
  type ask = Bytes.t

  type ak = Bytes.t

  type nsk = Bytes.t

  type nk = Bytes.t

  type ovk = Bytes.t

  type diversifier = Bytes.t

  type pkd = Bytes.t

  type nullifier = Bytes.t

  type commitment = Bytes.t

  type epk = Bytes.t

  type symkey = Bytes.t

  type sighash = Bytes.t

  type spend_sig = Bytes.t

  type hash = Bytes.t

  type cv = Bytes.t

  type rk = Bytes.t

  type spend_proof = Bytes.t

  type binding_sig = Bytes.t

  type output_proof = Bytes.t

  type ar = Bytes.t

  type rcm = Bytes.t

  type esk = Bytes.t

  type ivk = Bytes.t

  type diversifier_index = Bytes.t

  let compare_diversifier_index = Bytes.compare

  (* 96 bytes *)
  type expanded_spending_key = {ask : ask; nsk : nsk; ovk : ovk}

  (* 169 bytes *)
  (* this is an extended_spending_key that can be used to derive more
     keys using zip32*)
  type zip32_expanded_spending_key = {
    depth : Bytes.t;
    (*  1 byte  *)
    parent_fvk_tag : Bytes.t;
    (*  4 bytes *)
    child_index : Bytes.t;
    (*  4 bytes *)
    chain_code : Bytes.t;
    (* 32 bytes *)
    expsk : expanded_spending_key;
    (* 96 bytes *)
    dk : Bytes.t; (* 32 bytes *)
  }

  (* 96 bytes *)
  type full_viewing_key = {ak : ak; nk : nk; ovk : ovk}

  (* 169 bytes *)
  (* this is an extended_full_viewing_key that can be used to derive more
     keys using zip32, not implemented for now *)
  type zip32_full_viewing_key = {
    depth : Bytes.t;
    (*  1 byte  *)
    parent_fvk_tag : Bytes.t;
    (*  4 bytes *)
    child_index : Bytes.t;
    (*  4 bytes *)
    chain_code : Bytes.t;
    (* 32 bytes *)
    fvk : full_viewing_key;
    (* 96 bytes *)
    dk : Bytes.t; (* 32 bytes *)
  }

  let to_nk x =
    assert (Bytes.length x = 32) ;
    x

  let to_ak x =
    assert (Bytes.length x = 32) ;
    x

  let to_nsk x =
    (* A scalar of Jubjub *)
    assert (Bytes.length x = 32) ;
    x

  let to_ask x =
    (* A scalar of Jubjub *)
    assert (Bytes.length x = 32) ;
    x

  let to_pkd x =
    assert (Bytes.length x = 32) ;
    x

  let to_ovk x =
    (* Can be any random 32 bytes, 5.6.6 *)
    assert (Bytes.length x = 32) ;
    x

  let to_nullifier x =
    assert (Bytes.length x = 32) ;
    x

  let to_commitment x =
    assert (Bytes.length x = 32) ;
    x

  let to_symkey x =
    assert (Bytes.length x = 32) ;
    x

  let to_epk x =
    assert (Bytes.length x = 32) ;
    x

  let to_spend_sig x =
    assert (Bytes.length x = 64) ;
    x

  let to_hash x =
    assert (Bytes.length x = 32) ;
    x

  let to_cv x =
    assert (Bytes.length x = 32) ;
    x

  let to_spend_proof x =
    assert (Bytes.length x = 48 + 96 + 48) ;
    x

  let to_output_proof x =
    assert (Bytes.length x = 48 + 96 + 48) ;
    x

  let to_rk x =
    assert (Bytes.length x = 32) ;
    x

  let to_sighash x =
    assert (Bytes.length x = 32) ;
    x

  let to_binding_sig x =
    assert (Bytes.length x = 64) ;
    x

  let check_diversifier_bytes diversifier =
    RS.check_diversifier (Ctypes.ocaml_bytes_start diversifier)

  let to_diversifier x =
    assert (Bytes.length x = 11) ;
    if check_diversifier_bytes x then Some x else None

  let to_diversifier_index x =
    assert (Bytes.length x = 11) ;
    x

  let to_ar x =
    assert (Bytes.length x = 32) ;
    x

  let to_rcm x =
    assert (Bytes.length x = 32) ;
    x

  let to_esk x =
    assert (Bytes.length x = 32) ;
    x

  let to_ivk x =
    assert (Bytes.length x = 32) ;
    (* The first 5 bits of the last byte must be zero because it is a number on 251 bits *)
    assert (int_of_char (Bytes.get x (32 - 1)) <= 7) ;
    x

  let to_expanded_spending_key x =
    assert (Bytes.length x = 96) ;
    let ask = Bytes.create 32 in
    Bytes.blit x 0 ask 0 32 ;
    let nsk = Bytes.create 32 in
    Bytes.blit x 32 nsk 0 32 ;
    let ovk = Bytes.create 32 in
    Bytes.blit x 64 ovk 0 32 ;
    {ask = to_ask @@ ask; nsk = to_nsk @@ nsk; ovk = to_ovk @@ ovk}

  let to_zip32_expanded_spending_key x =
    assert (Bytes.length x = 169) ;
    let depth = Bytes.create 1 in
    Bytes.blit x 0 depth 0 1 ;
    let parent_fvk_tag = Bytes.create 4 in
    Bytes.blit x 1 parent_fvk_tag 0 4 ;
    let child_index = Bytes.create 4 in
    Bytes.blit x 5 child_index 0 4 ;
    let chain_code = Bytes.create 32 in
    Bytes.blit x 9 chain_code 0 32 ;
    let expsk = to_expanded_spending_key @@ Bytes.sub x 41 96 in
    let dk = Bytes.create 32 in
    Bytes.blit x 137 dk 0 32 ;
    {depth; parent_fvk_tag; child_index; chain_code; expsk; dk}

  let to_full_viewing_key x =
    assert (Bytes.length x = 96) ;
    let ak = Bytes.create 32 in
    let nk = Bytes.create 32 in
    let ovk = Bytes.create 32 in
    Bytes.blit x 0 ak 0 32 ;
    Bytes.blit x 32 nk 0 32 ;
    Bytes.blit x 64 ovk 0 32 ;
    {ak = to_ak ak; nk = to_nk nk; ovk = to_ovk ovk}

  let to_zip32_full_viewing_key x =
    assert (Bytes.length x = 169) ;
    let depth = Bytes.create 1 in
    Bytes.blit x 0 depth 0 1 ;
    let parent_fvk_tag = Bytes.create 4 in
    Bytes.blit x 1 parent_fvk_tag 0 4 ;
    let child_index = Bytes.create 4 in
    Bytes.blit x 5 child_index 0 4 ;
    let chain_code = Bytes.create 32 in
    Bytes.blit x 9 chain_code 0 32 ;
    let dk = Bytes.create 32 in
    Bytes.blit x 137 dk 0 32 ;
    let fvk = to_full_viewing_key @@ Bytes.sub x 41 96 in
    {depth; parent_fvk_tag; child_index; chain_code; fvk; dk}

  let of_nk x = x

  let of_ak x = x

  let of_nsk x = x

  let of_ask x = x

  let of_pkd x = x

  let of_ovk x = x

  let of_nullifier x = x

  let of_commitment x = x

  let of_symkey x = x

  let of_epk x = x

  let of_spend_sig x = x

  let of_hash x = x

  let of_cv x = x

  let of_spend_proof x = x

  let of_output_proof x = x

  let of_rk x = x

  let of_sighash x = x

  let of_binding_sig x = x

  let of_diversifier x = x

  let of_diversifier_index x = x

  let of_ar x = x

  let of_rcm x = x

  let of_esk x = x

  let of_ivk x = x

  let of_expanded_spending_key exspk =
    Bytes.concat
      Bytes.empty
      [of_ask exspk.ask; of_nsk exspk.nsk; of_ovk exspk.ovk]

  let of_zip32_expanded_spending_key (sk : zip32_expanded_spending_key) =
    Bytes.concat
      Bytes.empty
      [
        sk.depth;
        sk.parent_fvk_tag;
        sk.child_index;
        sk.chain_code;
        of_expanded_spending_key sk.expsk;
        sk.dk;
      ]

  let of_full_viewing_key fvk =
    Bytes.concat Bytes.empty [of_ak fvk.ak; of_nk fvk.nk; of_ovk fvk.ovk]

  let of_zip32_full_viewing_key xfvk =
    Bytes.concat
      Bytes.empty
      [
        xfvk.depth;
        xfvk.parent_fvk_tag;
        xfvk.child_index;
        xfvk.chain_code;
        of_full_viewing_key xfvk.fvk;
        xfvk.dk;
      ]

  let hash_compare = Stdlib.compare

  let hash_of_commitment x = x

  let commitment_of_hash x = x
end

(* This trick allows to hide the Bytes.t implementation of type t *)
include T

let max_amount =
  let coin = 1_0000_0000L in
  let max_money = Int64.mul 21_000_000L coin in
  max_money

let valid_amount a = Compare.Int64.(a >= 0L && a <= max_amount)

(* The spec requires balance to be in
   {−38913406623490299131842 .. 96079866507916199586728}
   but we restrict it further. *)
let valid_balance b =
  Compare.Int64.(Int64.neg max_amount <= b && b <= max_amount)

let valid_position pos =
  let max_uint32 = 4294967295L in
  Compare.Int64.(pos >= 0L && pos <= max_uint32)

(* We don't load sprout's parameters.
   Parameters of type Rust `usize` are converted to OCaml `int` because they
   are only file paths. NULL is a void pointer.
*)
let init_zksnark_params ~spend_path ~output_path =
  let spend_path = Bytes.of_string spend_path in
  let output_path = Bytes.of_string output_path in
  let spend_path_len = Bytes.length spend_path in
  let output_path_len = Bytes.length output_path in
  RS.init_zksnark_params
    (Ctypes.ocaml_bytes_start spend_path)
    (Unsigned.Size_t.of_int spend_path_len)
    (Ctypes.ocaml_bytes_start output_path)
    (Unsigned.Size_t.of_int output_path_len)
    (* Getting a NULL pointer of type uchar. Causing the warning saying we
       convert a void * to unsigned char* *)
    Ctypes.(from_voidp uchar null)
    Unsigned.Size_t.zero

let nsk_to_nk nsk =
  let nk = Bytes.create 32 in
  RS.nsk_to_nk
    (Ctypes.ocaml_bytes_start (of_nsk nsk))
    (Ctypes.ocaml_bytes_start nk) ;
  to_nk nk

let ask_to_ak ask =
  let ak = Bytes.create 32 in
  RS.ask_to_ak
    (Ctypes.ocaml_bytes_start (of_ask ask))
    (Ctypes.ocaml_bytes_start ak) ;
  to_ak ak

let crh_ivk ak nk =
  let ivk = Bytes.create 32 in
  RS.crh_ivk
    (Ctypes.ocaml_bytes_start (of_ak ak))
    (Ctypes.ocaml_bytes_start (of_nk nk))
    (Ctypes.ocaml_bytes_start ivk) ;
  to_ivk ivk

let check_diversifier diversifier =
  RS.check_diversifier (Ctypes.ocaml_bytes_start (of_diversifier diversifier))

let ivk_to_pkd ivk diversifier =
  let pkd = Bytes.create 32 in
  let res =
    RS.ivk_to_pkd
      (Ctypes.ocaml_bytes_start (of_ivk ivk))
      (Ctypes.ocaml_bytes_start (of_diversifier diversifier))
      (Ctypes.ocaml_bytes_start pkd)
  in
  assert res ;
  to_pkd pkd

let generate_r () =
  let res = Bytes.create 32 in
  RS.sapling_generate_r (Ctypes.ocaml_bytes_start res) ;
  res

let compute_nf diversifier pk_d ~amount r ak nk ~position =
  if not (valid_amount amount) then invalid_arg "amount"
  else if not (valid_position position) then invalid_arg "position"
  else
    let nf = Bytes.create 32 in
    let res =
      RS.sapling_compute_nf
        (Ctypes.ocaml_bytes_start (of_diversifier diversifier))
        (Ctypes.ocaml_bytes_start (of_pkd pk_d))
        (Unsigned.UInt64.of_int64 amount)
        (Ctypes.ocaml_bytes_start (of_rcm r))
        (Ctypes.ocaml_bytes_start (of_ak ak))
        (Ctypes.ocaml_bytes_start (of_nk nk))
        (Unsigned.UInt64.of_int64 position)
        (Ctypes.ocaml_bytes_start nf)
    in
    assert res ;
    to_nullifier nf

let compute_cm diversifier pk_d ~amount rcm =
  if not (valid_amount amount) then invalid_arg "amount"
  else
    let cm = Bytes.create 32 in
    let res =
      RS.sapling_compute_cm
        false
        (Ctypes.ocaml_bytes_start (of_diversifier diversifier))
        (Ctypes.ocaml_bytes_start (of_pkd pk_d))
        (Unsigned.UInt64.of_int64 amount)
        (Ctypes.ocaml_bytes_start (of_rcm rcm))
        (Ctypes.ocaml_bytes_start cm)
    in
    assert res ;
    to_commitment cm

let ka_agree (p : Bytes.t) (sk : Bytes.t) =
  let ka = Bytes.create 32 in
  let res =
    RS.sapling_ka_agree
      false
      (Ctypes.ocaml_bytes_start p)
      (Ctypes.ocaml_bytes_start sk)
      (Ctypes.ocaml_bytes_start ka)
  in
  assert res ;
  to_symkey ka

let ka_agree_sender (p : pkd) (sk : esk) = ka_agree (of_pkd p) (of_esk sk)

let ka_agree_receiver (p : epk) (sk : ivk) = ka_agree (of_epk p) (of_ivk sk)

let ka_derivepublic diversifier esk =
  let epk = Bytes.create 32 in
  let res =
    RS.sapling_ka_derivepublic
      (Ctypes.ocaml_bytes_start (of_diversifier diversifier))
      (Ctypes.ocaml_bytes_start (of_esk esk))
      (Ctypes.ocaml_bytes_start epk)
  in
  assert res ;
  to_epk epk

let spend_sig ask ar sighash =
  let signature = Bytes.create 64 in
  let res =
    RS.sapling_spend_sig
      (Ctypes.ocaml_bytes_start (of_ask ask))
      (Ctypes.ocaml_bytes_start (of_ar ar))
      (Ctypes.ocaml_bytes_start (of_sighash sighash))
      (Ctypes.ocaml_bytes_start signature)
  in
  assert res ;
  to_spend_sig signature

type proving_ctx = unit Ctypes_static.ptr

let proving_ctx_init () = RS.proving_ctx_init ()

let proving_ctx_free ctx = RS.proving_ctx_free ctx

type verification_ctx = unit Ctypes_static.ptr

let verification_ctx_init () = RS.verification_ctx_init false

let verification_ctx_free ctx = RS.verification_ctx_free ctx

let tree_uncommitted =
  to_hash
    (Hex.to_bytes_exn
       (`Hex "0100000000000000000000000000000000000000000000000000000000000000"))

let merkle_hash ~height a b =
  (* TODO: Change height to size_t. It is an int for the moment *)
  let result = Bytes.create 32 in
  RS.merkle_hash
    (Unsigned.Size_t.of_int height)
    (Ctypes.ocaml_bytes_start (of_hash a))
    (Ctypes.ocaml_bytes_start (of_hash b))
    (Ctypes.ocaml_bytes_start result) ;
  to_hash result

let spend_proof ctx ak nsk diversifier rcm ar ~amount ~root ~witness =
  if not (valid_amount amount) then invalid_arg "amount"
  else
    let cv = Bytes.create 32 in
    let rk = Bytes.create 32 in
    let zkproof = Bytes.create (48 + 96 + 48) in
    let res =
      RS.sapling_spend_proof
        ctx
        (Ctypes.ocaml_bytes_start (of_ak ak))
        (Ctypes.ocaml_bytes_start (of_nsk nsk))
        (Ctypes.ocaml_bytes_start (of_diversifier diversifier))
        (Ctypes.ocaml_bytes_start (of_rcm rcm))
        (Ctypes.ocaml_bytes_start (of_ar ar))
        (Unsigned.UInt64.of_int64 amount)
        (Ctypes.ocaml_bytes_start (of_hash root))
        (Ctypes.ocaml_bytes_start witness)
        (Ctypes.ocaml_bytes_start cv)
        (Ctypes.ocaml_bytes_start rk)
        (Ctypes.ocaml_bytes_start zkproof)
    in
    assert res ;
    (to_cv cv, to_rk rk, to_spend_proof zkproof)

let check_spend verification_ctx cv root nullifier rk spend_proof spend_sig
    sighash =
  RS.sapling_check_spend
    verification_ctx
    (Ctypes.ocaml_bytes_start (of_cv cv))
    (Ctypes.ocaml_bytes_start (of_hash root))
    (Ctypes.ocaml_bytes_start (of_nullifier nullifier))
    (Ctypes.ocaml_bytes_start (of_rk rk))
    (Ctypes.ocaml_bytes_start (of_spend_proof spend_proof))
    (Ctypes.ocaml_bytes_start (of_spend_sig spend_sig))
    (Ctypes.ocaml_bytes_start (of_sighash sighash))

let make_binding_sig ctx ~balance sighash =
  if not (valid_balance balance) then invalid_arg "balance"
  else
    let binding_sig = Bytes.create 64 in
    let res =
      RS.sapling_binding_sig
        ctx
        balance
        (Ctypes.ocaml_bytes_start (of_sighash sighash))
        (Ctypes.ocaml_bytes_start binding_sig)
    in
    assert res ;
    to_binding_sig binding_sig

let output_proof ctx esk diversifier pk_d rcm ~amount =
  if not (valid_amount amount) then invalid_arg "amount"
  else
    let address = Bytes.create 43 in
    Bytes.blit (of_diversifier diversifier) 0 address 0 11 ;
    Bytes.blit (of_pkd pk_d) 0 address 11 32 ;
    let cv = Bytes.create 32 in
    let zkproof = Bytes.create (48 + 96 + 48) in
    let res =
      RS.sapling_output_proof
        ctx
        (Ctypes.ocaml_bytes_start (of_esk esk))
        (Ctypes.ocaml_bytes_start address)
        (Ctypes.ocaml_bytes_start (of_rcm rcm))
        (Unsigned.UInt64.of_int64 amount)
        (Ctypes.ocaml_bytes_start cv)
        (Ctypes.ocaml_bytes_start zkproof)
    in
    assert res ;
    (to_cv cv, to_output_proof zkproof)

let check_output verification_ctx cv commitment epk output_proof =
  RS.sapling_check_output
    verification_ctx
    (Ctypes.ocaml_bytes_start (of_cv cv))
    (Ctypes.ocaml_bytes_start (of_commitment commitment))
    (Ctypes.ocaml_bytes_start (of_epk epk))
    (Ctypes.ocaml_bytes_start (of_output_proof output_proof))

let final_check ctx balance binding_sig sighash =
  if not (valid_balance balance) then invalid_arg "balance"
  else
    RS.sapling_final_check
      ctx
      balance
      (Ctypes.ocaml_bytes_start (of_binding_sig binding_sig))
      (Ctypes.ocaml_bytes_start (of_sighash sighash))

let zip32_xsk_master seed =
  assert (Bytes.length seed = 32) ;
  let res = Bytes.create 169 in
  RS.zip32_xsk_master
    (Ctypes.ocaml_bytes_start seed)
    (Unsigned.Size_t.of_int 32)
    (Ctypes.ocaml_bytes_start res) ;
  to_zip32_expanded_spending_key res

let zip32_xfvk_address xfvk j =
  let j_ret = Bytes.create 11 in
  let addr = Bytes.create 43 in
  let fvk = of_full_viewing_key xfvk.fvk in
  let dk = xfvk.dk in
  let res =
    RS.zip32_xfvk_address
      (Ctypes.ocaml_bytes_start fvk)
      (Ctypes.ocaml_bytes_start dk)
      (Ctypes.ocaml_bytes_start (of_diversifier_index j))
      (Ctypes.ocaml_bytes_start j_ret)
      (Ctypes.ocaml_bytes_start addr)
  in
  assert (Bytes.length (of_diversifier_index j) = 11) ;
  if res then (
    let diversifier = Bytes.create 11 in
    let pkd = Bytes.create 32 in
    Bytes.blit addr 0 diversifier 0 11 ;
    Bytes.blit addr 11 pkd 0 32 ;
    let diversifier =
      (* This value is returned from the lib, it is a valid diversifier *)
      WithExceptions.Option.get ~loc:__LOC__ @@ to_diversifier diversifier
    in
    Some (to_diversifier_index j_ret, diversifier, to_pkd pkd))
  else None

let to_scalar input =
  assert (Bytes.length input = 64) ;
  let res = Bytes.create 32 in
  RS.to_scalar (Ctypes.ocaml_bytes_start input) (Ctypes.ocaml_bytes_start res) ;
  res

let zip32_xsk_derive parent index =
  let bytes_parent = of_zip32_expanded_spending_key parent in
  assert (Bytes.length bytes_parent = 169) ;
  assert (Int32.compare index Int32.zero >= 0) ;
  let res = Bytes.create 169 in
  RS.zip32_xsk_derive
    (Ctypes.ocaml_bytes_start bytes_parent)
    (Unsigned.UInt32.of_int32 index)
    (Ctypes.ocaml_bytes_start res) ;
  to_zip32_expanded_spending_key res

let zip32_xfvk_derive parent index =
  let bytes_parent = of_zip32_full_viewing_key parent in
  assert (Bytes.length bytes_parent = 169) ;
  assert (Int32.compare index Int32.zero >= 0) ;
  let derived = Bytes.create 169 in
  let res =
    RS.zip32_xfvk_derive
      (Ctypes.ocaml_bytes_start bytes_parent)
      (Unsigned.UInt32.of_int32 index)
      (Ctypes.ocaml_bytes_start derived)
  in
  assert res ;
  to_zip32_full_viewing_key derived

exception Params_not_found of string list

let () =
  Printexc.register_printer @@ function
  | Params_not_found locations ->
      (* We tend to look at the same location several times,
         but there is no need to confuse the user about it. *)
      let locations = List.sort_uniq String.compare locations in
      Some
        (Format.asprintf
           "@[<v>cannot find Zcash params in any of:@,\
            %a@ You may download them using \
            https://raw.githubusercontent.com/zcash/zcash/713fc761dd9cf4c9087c37b078bdeab98697bad2/zcutil/fetch-params.sh@]@."
           (Format.pp_print_list (fun fmt -> Format.fprintf fmt "- %s"))
           locations)
  | _ -> None

type parameter_files = {spend_path : string; output_path : string}

(* Find Zcash parameter files.
   This function is parameterized by system functions in order to be able to test it
   with a mock. *)
let find_params ?(getenv_opt = Sys.getenv_opt) ?(getcwd = Sys.getcwd)
    ?(file_exists = Sys.file_exists) () =
  let ( // ) = Filename.concat in
  (* [env name path] looks up the value of environment variable [name]
     and concatenates it with [path].
     If [split] is specified, the environment variable is expected
     to contain a list of paths separated by character [split].
     Otherwise the environment variable is expected to contain a single path. *)
  let env ?split name path =
    match getenv_opt name with
    | None -> []
    | Some value -> (
        match split with
        | None -> [Filename.concat value path]
        | Some char ->
            List.map (fun dir -> dir // path) (String.split_on_char char value))
  in
  (* [cwd path] is the current directory concatenated to [path]. *)
  let cwd path = try [getcwd () // path] with Sys_error _ -> [] in
  (* List directories where we could find zcash parameter files.
     Directories with higher priority come first. *)
  let candidate_directories =
    env "XDG_DATA_HOME" ".local/share/zcash-params"
    @ env ~split:':' "XDG_DATA_DIRS" "zcash-params"
    @ env "OPAM_SWITCH_PREFIX" "share/zcash-params"
    @ env "PWD" "_opam/share/zcash-params"
    @ cwd "_opam/share/zcash-params"
    @ env "HOME" ".zcash-params"
    @ env "HOME" ".local/share/zcash-params"
    @ env "HOMEBREW_PREFIX" "share/zcash-params"
    @ ["/usr/local/share/zcash-params"; "/usr/share/zcash-params"]
  in
  (* Files we are looking for. *)
  let spend_path = "sapling-spend.params" in
  let output_path = "sapling-output.params" in
  (* Find the first candidate directory that contains the expected files. *)
  let directory =
    let contains_zcash_files directory =
      file_exists (directory // spend_path)
      && file_exists (directory // output_path)
    in
    match List.find_opt contains_zcash_files candidate_directories with
    | None -> raise (Params_not_found candidate_directories)
    | Some directory -> directory
  in
  let spend_path = directory // spend_path in
  let output_path = directory // output_path in
  {spend_path; output_path}

let init_params () =
  let {spend_path; output_path} = find_params () in
  init_zksnark_params ~spend_path ~output_path

let init_params_lazy = Lazy.from_fun init_params

let with_proving_ctx f =
  let () = Lazy.force init_params_lazy in
  let ctx = proving_ctx_init () in
  Fun.protect ~finally:(fun () -> proving_ctx_free ctx) (fun () -> f ctx)

let with_verification_ctx f =
  let () = Lazy.force init_params_lazy in
  let ctx = verification_ctx_init () in
  Fun.protect ~finally:(fun () -> verification_ctx_free ctx) (fun () -> f ctx)
