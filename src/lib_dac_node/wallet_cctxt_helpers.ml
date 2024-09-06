(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

let get_keys cctxt (pkh : Tezos_crypto.Aggregate_signature.public_key_hash) =
  let open Lwt_result_syntax in
  let open Tezos_client_base.Client_keys in
  let (Bls12_381 bls_pkh) = pkh in
  let simple_pkh : Tezos_crypto.Signature.public_key_hash =
    Signature.Bls bls_pkh
  in
  let* alias = Public_key_hash.rev_find cctxt simple_pkh in
  match alias with
  | None -> return (pkh, None, None)
  | Some alias -> (
      let* keys_opt = alias_keys cctxt alias in
      match keys_opt with
      | Some (_pkh, Some (Bls pk), sk_uri_opt) ->
          let (aggregate_pk : Tezos_crypto.Aggregate_signature.public_key) =
            Tezos_crypto.Aggregate_signature.Bls12_381 pk
          in
          return (pkh, Some aggregate_pk, sk_uri_opt)
      | _ ->
          (* none or not bls key *)
          let*! () = Event.(emit committee_member_not_in_wallet pkh) in
          return (pkh, None, None))

let get_public_key cctxt address =
  let open Lwt_result_syntax in
  let+ _, pk_opt, _ = get_keys cctxt address in
  pk_opt

let can_verify (_, pk_opt, _) = Option.is_some pk_opt

let can_sign (_, _, sk_uri_opt) = Option.is_some sk_uri_opt
