(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

include Slot_manager_legacy

type error +=
  | Invalid_slot_size of {provided : int; expected : int}
  | Invalid_degree of string

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.invalid_slot_size"
    ~title:"Invalid slot size"
    ~description:"The size of the given slot is not as expected"
    ~pp:(fun ppf (provided, expected) ->
      Format.fprintf
        ppf
        "The size (%d) of the given slot is not as expected (%d)"
        provided
        expected)
    Data_encoding.(obj2 (req "provided" int31) (req "expected" int31))
    (function
      | Invalid_slot_size {provided; expected} -> Some (provided, expected)
      | _ -> None)
    (fun (provided, expected) -> Invalid_slot_size {provided; expected}) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.invalid_degree_string"
    ~title:"Invalid degree"
    ~description:"The degree of the polynomial is too high"
    ~pp:(fun ppf msg -> Format.fprintf ppf "%s" msg)
    Data_encoding.(obj1 (req "msg" string))
    (function Invalid_degree msg -> Some msg | _ -> None)
    (fun msg -> Invalid_degree msg)

(* Used wrapper functions on top of Cryptobox. *)

let polynomial_from_slot cryptobox slot =
  let open Result_syntax in
  match Cryptobox.polynomial_from_slot cryptobox slot with
  | Ok r -> return r
  | Error (`Slot_wrong_size _) ->
      let open Cryptobox in
      let provided = Bytes.length slot in
      let {slot_size = expected; _} = parameters cryptobox in
      Error (Errors.other [Invalid_slot_size {provided; expected}])

let commit cryptobox polynomial =
  let open Result_syntax in
  match Cryptobox.commit cryptobox polynomial with
  | Ok cm -> return cm
  | Error (`Invalid_degree_strictly_less_than_expected _ as commit_error) ->
      Error
        (Errors.other
           [Invalid_degree (Cryptobox.string_of_commit_error commit_error)])

let commitment_should_exist node_store cryptobox commitment =
  let open Lwt_result_syntax in
  let*! exists =
    Store.Legacy.exists_slot_by_commitment node_store cryptobox commitment
  in
  if not exists then fail `Not_found else return_unit

(* Main functions *)

let add_commitment node_store slot cryptobox =
  let open Lwt_result_syntax in
  let*? polynomial = polynomial_from_slot cryptobox slot in
  let*? commitment = commit cryptobox polynomial in
  let*! exists =
    Store.Legacy.exists_slot_by_commitment node_store cryptobox commitment
  in
  let*! () =
    if exists then Lwt.return_unit
    else
      Store.Legacy.add_slot_by_commitment node_store cryptobox slot commitment
  in
  return commitment

let associate_slot_id_with_commitment node_store cryptobox commitment slot_id =
  let open Lwt_result_syntax in
  let* () = commitment_should_exist node_store cryptobox commitment in
  let*! () =
    Store.Legacy.associate_slot_id_with_commitment node_store commitment slot_id
  in
  return_unit

let get_commitment_slot node_store cryptobox commitment =
  let open Lwt_result_syntax in
  let* slot_opt =
    Store.Legacy.find_slot_by_commitment node_store cryptobox commitment
  in
  match slot_opt with
  | None -> fail `Not_found
  | Some slot_content -> return slot_content

let add_commitment_shards ~shards_proofs_precomputation node_store cryptobox
    commitment ~with_proof =
  let open Lwt_result_syntax in
  let* slot = get_commitment_slot node_store cryptobox commitment in
  let*? polynomial = polynomial_from_slot cryptobox slot in
  let shards = Cryptobox.shards_from_polynomial cryptobox polynomial in
  let* () =
    Store.(
      Shards.save_and_notify
        node_store.shard_store
        node_store.shards_watcher
        commitment
        shards)
  in
  if with_proof then
    let shard_proofs =
      Cryptobox.prove_shards
        cryptobox
        ~polynomial
        ~precomputation:shards_proofs_precomputation
    in
    Store.Legacy.save_shard_proofs node_store commitment shard_proofs |> return
  else return_unit

let get_opt array i =
  if i >= 0 && i < Array.length array then Some array.(i) else None

(** [shards_to_attesters committee] takes a committee [Committee_cache.committee]
      and returns a function that, given a shard index, yields the pkh of its
      attester for that level. *)
let shards_to_attesters committee =
  let rec do_n ~n f acc = if n <= 0 then acc else do_n ~n:(n - 1) f (f acc) in
  let to_array committee =
    (* We transform the map to a list *)
    Tezos_crypto.Signature.Public_key_hash.Map.bindings committee
    (* We sort the list in decreasing order w.r.t. to start_indices. *)
    |> List.fast_sort (fun (_pkh1, shard_indices1) (_pkh2, shard_indices2) ->
           shard_indices2.Committee_cache.start_index
           - shard_indices1.Committee_cache.start_index)
       (* We fold on the sorted list, starting from bigger start_indices. *)
    |> List.fold_left
         (fun accu (pkh, Committee_cache.{start_index = _; offset}) ->
           (* We put in the accu list as many [pkh] occurrences as the number
              of shards this pkh should attest, namely, [offset]. *)
           do_n ~n:offset (fun acc -> pkh :: acc) accu)
         []
    (* We build an array from the list. The array indices coincide with shard
       indices. *)
    |> Array.of_list
  in
  let committee = to_array committee in
  fun index -> get_opt committee index

(** This function publishes the shards of a commitment that is waiting for
    attestion on L1 if this node has those shards on disk and their proofs in
    memory. *)
let publish_slot_data ~level_committee (node_store : Store.node_store) gs_worker
    cryptobox proto_parameters commitment published_level slot_index =
  let open Lwt_result_syntax in
  match
    Store.Shard_proofs_cache.find_opt
      node_store.in_memory_shard_proofs
      commitment
  with
  | None ->
      (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5676

          If this happens, we should probably tell the user that
          something bad happened. Either:

          1. The proofs where not stored properly (an invariant is broken)

          2. The node was restarted (unlikely to happen given the time frame)

          3. The cache was full (unlikely to happen if
          [shards_proofs_cache_size] is set properly. *)
      return_unit
  | Some shard_proofs ->
      let attestation_level =
        Int32.(
          pred
          @@ add
               published_level
               (of_int proto_parameters.Dal_plugin.attestation_lag))
      in
      let* committee = level_committee ~level:attestation_level in
      let attester_of_shard = shards_to_attesters committee in
      let Cryptobox.{number_of_shards; _} = Cryptobox.parameters cryptobox in
      Store.Shards.read_all node_store.shard_store commitment ~number_of_shards
      |> Seq_s.iter_ep (function
             | _, _, Error [Stored_data.Missing_stored_data s] ->
                 let*! () =
                   Event.(
                     emit loading_shard_data_failed ("Missing stored data " ^ s))
                 in
                 return_unit
             | _, _, Error err ->
                 let*! () =
                   Event.(
                     emit
                       loading_shard_data_failed
                       (Format.asprintf "%a" pp_print_trace err))
                 in
                 return_unit
             | commitment, shard_index, Ok share -> (
                 match
                   ( attester_of_shard shard_index,
                     get_opt shard_proofs shard_index )
                 with
                 | None, _ ->
                     failwith
                       "Invariant broken: no attester found for shard %d"
                       shard_index
                 | _, None ->
                     failwith
                       "Invariant broken: no shard proof found for shard %d"
                       shard_index
                 | Some pkh, Some shard_proof ->
                     let message = Types.Message.{share; shard_proof} in
                     let topic = Types.Topic.{slot_index; pkh} in
                     let message_id =
                       Types.Message_id.
                         {
                           commitment;
                           level = published_level;
                           slot_index;
                           shard_index;
                           pkh;
                         }
                     in
                     Gossipsub.Worker.(
                       Publish_message {message; topic; message_id}
                       |> app_input gs_worker) ;
                     return_unit))

let store_slot_headers ~number_of_slots ~block_level slot_headers node_store =
  Store.Legacy.add_slot_headers
    ~number_of_slots
    ~block_level
    slot_headers
    node_store

let update_selected_slot_headers_statuses ~block_level ~attestation_lag
    ~number_of_slots attested_slots node_store =
  Store.Legacy.update_selected_slot_headers_statuses
    ~block_level
    ~attestation_lag
    ~number_of_slots
    attested_slots
    node_store

let get_commitment_by_published_level_and_index ~level ~slot_index node_store =
  Store.Legacy.get_commitment_by_published_level_and_index
    ~level
    ~slot_index
    node_store

let get_commitment_headers commitment ?slot_level ?slot_index node_store =
  Store.Legacy.get_commitment_headers
    commitment
    ?slot_level
    ?slot_index
    node_store

let get_published_level_headers ~published_level ?header_status store =
  Store.Legacy.get_published_level_headers ~published_level ?header_status store
