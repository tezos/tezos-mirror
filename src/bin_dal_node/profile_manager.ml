(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4443
   Node profiles should be stored into the memory as well
   so that we can cache them *)

let add_profile proto_parameters node_store profile =
  let open Lwt_result_syntax in
  let*! () = Store.Legacy.add_profile proto_parameters node_store profile in
  return_unit

let get_profiles node_store = Store.Legacy.get_profiles node_store

let get_attestable_slots ctxt store proto_parameters pkh ~attested_level =
  let open Lwt_result_syntax in
  let* shard_indexes =
    Node_context.fetch_assigned_shard_indices ctxt ~pkh ~level:attested_level
    |> Errors.other_lwt_result
  in
  let expected_number_of_shards = List.length shard_indexes in
  if expected_number_of_shards = 0 then return Services.Types.Not_in_committee
  else
    let published_level =
      (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4612
         Correctly compute [published_level] in case of protocol changes, in
         particular a change of the value of [attestation_lag]. *)
      Int32.(
        sub attested_level (of_int proto_parameters.Dal_plugin.attestation_lag))
    in
    let are_shards_stored slot_index =
      let*! r =
        Slot_manager.get_commitment_by_published_level_and_index
          ~level:published_level
          ~slot_index
          store
      in
      let open Errors in
      match r with
      | Error `Not_found -> return false
      | Error (#decoding as e) -> fail (e :> [Errors.decoding | Errors.other])
      | Ok commitment ->
          Store.Shards.are_shards_available
            store.shard_store
            commitment
            shard_indexes
          |> Errors.other_lwt_result
    in
    let all_slot_indexes =
      Utils.Infix.(0 -- (proto_parameters.number_of_slots - 1))
    in
    let* flags = List.map_es are_shards_stored all_slot_indexes in
    return (Services.Types.Attestable_slots flags)
