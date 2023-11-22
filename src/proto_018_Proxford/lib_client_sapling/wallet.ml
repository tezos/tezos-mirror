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

open Client_keys
open Tezos_sapling.Core.Client

(* Transform a spending key to an uri, encrypted or not. *)
let to_uri unencrypted cctxt sapling_key =
  let open Lwt_result_syntax in
  if unencrypted then
    let*? sapling_uri =
      Tezos_signer_backends.Unencrypted.make_sapling_key sapling_key
    in
    return sapling_uri
  else Tezos_signer_backends.Encrypted.encrypt_sapling_key cctxt sapling_key

(** Transform an uri into a spending key, asking for a password if the uri was
    encrypted. *)
let from_uri (cctxt : #Client_context.full) uri =
  Tezos_signer_backends.Encrypted.decrypt_sapling_key cctxt uri

let register (cctxt : #Client_context.full) ?(force = false)
    ?(unencrypted = false) mnemonic name =
  let open Lwt_result_syntax in
  let sk = Spending_key.of_seed @@ Mnemonic.to_32_bytes mnemonic in
  let* sk_uri = to_uri unencrypted cctxt sk in
  let key =
    {
      sk = sk_uri;
      path = [Spending_key.child_index sk];
      address_index = Viewing_key.default_index;
    }
  in
  let* () = Sapling_key.add ~force cctxt name key in
  return @@ Viewing_key.of_sk sk

let derive (cctxt : #Client_context.full) ?(force = false)
    ?(unencrypted = false) src_name dst_name child_index =
  let open Lwt_result_syntax in
  let* k = Sapling_key.find cctxt src_name in
  let* src_sk = from_uri cctxt k.sk in
  let child_index = Int32.of_int child_index in
  let dst_sk = Spending_key.derive_key src_sk child_index in
  let* dst_sk_uri = to_uri unencrypted cctxt dst_sk in
  let dst_key =
    {
      sk = dst_sk_uri;
      path = child_index :: k.path;
      address_index = Viewing_key.default_index;
    }
  in
  (* TODO check this force *)
  let _ = force in
  let* () = Sapling_key.add ~force:true cctxt dst_name dst_key in
  let path =
    String.concat "/" (List.map Int32.to_string (List.rev dst_key.path))
  in
  return (path, Viewing_key.of_sk dst_sk)

let find_vk cctxt name =
  let open Lwt_result_syntax in
  let* k = Sapling_key.find cctxt name in
  let* sk = from_uri cctxt k.sk in
  return (Viewing_key.of_sk sk)

let new_address (cctxt : #Client_context.full) name index_opt =
  let open Lwt_result_syntax in
  let* k = Sapling_key.find cctxt name in
  let index =
    match index_opt with
    | None -> k.address_index
    | Some i -> Viewing_key.index_of_int64 (Int64.of_int i)
  in
  let* sk = from_uri cctxt k.sk in
  let* vk = return (Viewing_key.of_sk sk) in
  (* Viewing_key.new_address finds the smallest index greater or equal to
     [index] that generates a correct address. *)
  let corrected_index, address = Viewing_key.new_address vk index in
  let* () =
    Sapling_key.update
      cctxt
      name
      {k with address_index = Viewing_key.index_succ corrected_index}
  in
  return (sk, corrected_index, address)

let export_vk cctxt name =
  let open Lwt_result_syntax in
  let* vk = find_vk cctxt name in
  return (Data_encoding.Json.construct Viewing_key.encoding vk)
