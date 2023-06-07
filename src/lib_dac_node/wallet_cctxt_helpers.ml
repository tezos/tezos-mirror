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

module Aggregate_signature = Tezos_crypto.Aggregate_signature

let get_keys cctxt pkh =
  let open Lwt_result_syntax in
  let open Tezos_client_base.Client_keys in
  let* alias = Aggregate_alias.Public_key_hash.rev_find cctxt pkh in
  match alias with
  | None -> return (pkh, None, None)
  | Some alias -> (
      let* keys_opt = alias_aggregate_keys cctxt alias in
      match keys_opt with
      | None ->
          let*! () = Event.(emit committee_member_not_in_wallet pkh) in
          return (pkh, None, None)
      | Some (pkh, pk_opt, sk_uri_opt) -> return (pkh, pk_opt, sk_uri_opt))

let get_public_key cctxt address =
  let open Lwt_result_syntax in
  let+ _, pk_opt, _ = get_keys cctxt address in
  pk_opt

let can_verify (_, pk_opt, _) = Option.is_some pk_opt

let can_sign (_, _, sk_uri_opt) = Option.is_some sk_uri_opt
