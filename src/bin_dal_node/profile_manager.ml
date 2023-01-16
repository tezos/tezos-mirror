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

type error +=
  | Dal_attestor_not_in_committee of {
      attestor : Tezos_crypto.Signature.Public_key_hash.t;
      level : int32;
    }

let () =
  register_error_kind
    `Permanent
    ~id:"DAL_node.attestor_not_in_committee"
    ~title:"The attestor is not part of the DAL committee for this level"
    ~description:"The attestor is not part of the DAL committee for this level"
    ~pp:(fun ppf (attestor, level) ->
      Format.fprintf
        ppf
        "The attestor %a is not part of the DAL committee for the level %ld"
        Tezos_crypto.Signature.Public_key_hash.pp
        attestor
        level)
    Data_encoding.(
      obj2
        (req "attestor" Tezos_crypto.Signature.Public_key_hash.encoding)
        (req "level" int32))
    (function
      | Dal_attestor_not_in_committee {attestor; level} -> Some (attestor, level)
      | _ -> None)
    (fun (attestor, level) -> Dal_attestor_not_in_committee {attestor; level})

let add_profile node_store profile =
  let open Lwt_result_syntax in
  let*! () = Store.Legacy.add_profile node_store profile in
  return_unit

let get_profiles node_store =
  let open Lwt_result_syntax in
  let*! profiles = Store.Legacy.get_profiles node_store in
  return profiles

let get_attestable_slots ctxt store cryptobox proto_parameters pkh
    ~attested_level =
  let open Lwt_result_syntax in
  let* shard_indexes =
    Node_context.fetch_assigned_shard_indices ctxt ~pkh ~level:attested_level
  in
  let expected_number_of_shards = List.length shard_indexes in
  let* () =
    fail_when
      (expected_number_of_shards = 0)
      (Dal_attestor_not_in_committee {attestor = pkh; level = attested_level})
  in
  let published_level =
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4612
       Correctly compute [published_level] in case of protocol changes, in
       particular a change of the value of [attestation_lag]. *)
    Int32.(
      sub attested_level (of_int proto_parameters.Dal_plugin.attestation_lag))
  in
  let share_size = Cryptobox.encoded_share_size cryptobox in
  let are_shards_stored slot_index =
    let*! r =
      Slot_manager.get_commitment_by_published_level_and_index
        ~level:published_level
        ~slot_index
        store
    in
    match r with
    | Error (Ok `Not_found) -> return false
    | Error (Error e) -> fail e
    | Ok commitment ->
        Shard_store.are_shards_available
          ~share_size
          store.shard_store
          commitment
          shard_indexes
  in
  let all_slot_indexes =
    Utils.Infix.(0 -- (proto_parameters.number_of_slots - 1))
  in
  List.map_es are_shards_stored all_slot_indexes
