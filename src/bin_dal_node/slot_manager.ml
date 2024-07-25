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

type error +=
  | Merging_failed of string
  | Missing_shards of {provided : int; required : int}
  | Illformed_pages
  | Invalid_slot_size of {provided : int; expected : int}
  | Invalid_degree of string
  | No_prover_SRS

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.merge_failed"
    ~title:"Merge failed"
    ~description:"Merging the slot failed"
    ~pp:(fun ppf msg -> Format.fprintf ppf "%s" msg)
    Data_encoding.(obj1 (req "msg" string))
    (function Merging_failed parameter -> Some parameter | _ -> None)
    (fun parameter -> Merging_failed parameter) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.missing_shards"
    ~title:"Missing shards"
    ~description:"Some shards are missing"
    ~pp:(fun ppf (provided, required) ->
      Format.fprintf
        ppf
        "Some shards are missing, expected at least %d, found %d. Store is \
         invalid."
        provided
        required)
    Data_encoding.(obj2 (req "provided" int31) (req "required" int31))
    (function
      | Missing_shards {provided; required} -> Some (provided, required)
      | _ -> None)
    (fun (provided, required) -> Missing_shards {provided; required}) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.illformed_pages"
    ~title:"Illformed pages"
    ~description:"Illformed pages found in the store"
    ~pp:(fun ppf () -> Format.fprintf ppf "Illformed pages found in the store")
    Data_encoding.unit
    (function Illformed_pages -> Some () | _ -> None)
    (fun () -> Illformed_pages) ;
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
    (fun msg -> Invalid_degree msg) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.no_prover_srs"
    ~title:"No prover SRS"
    ~description:"The prover SRS has not been loaded."
    ~pp:(fun ppf _ ->
      Format.fprintf
        ppf
        "The prover SRS must be loaded before using proving functions. Change \
         the current profile of your DAL node to slot producer or observer to \
         be able to compute proofs.")
    Data_encoding.empty
    (function No_prover_SRS -> Some () | _ -> None)
    (fun () -> No_prover_SRS)

type slot = bytes

let polynomial_from_shards cryptobox shards =
  match Cryptobox.polynomial_from_shards cryptobox shards with
  | Ok p -> Lwt.return_ok p
  | Error
      ( `Not_enough_shards msg
      | `Shard_index_out_of_range msg
      | `Invalid_shard_length msg ) ->
      Lwt.return_error (Errors.other [Merging_failed msg])

let get_slot_content_from_shards cryptobox store slot_id =
  let open Lwt_result_syntax in
  let {Cryptobox.number_of_shards; redundancy_factor; slot_size; _} =
    Cryptobox.parameters cryptobox
  in
  let minimal_number_of_shards = number_of_shards / redundancy_factor in
  let rec loop acc shard_id remaining =
    if remaining <= 0 then return acc
    else if shard_id >= number_of_shards then
      let provided = minimal_number_of_shards - remaining in
      fail
        (Errors.other
           [Missing_shards {provided; required = minimal_number_of_shards}])
    else
      let*! res = Store.Shards.read store.Store.shards slot_id shard_id in
      match res with
      | Ok res -> loop (Seq.cons res acc) (shard_id + 1) (remaining - 1)
      | Error _ -> loop acc (shard_id + 1) remaining
  in
  let* shards = loop Seq.empty 0 minimal_number_of_shards in
  let* polynomial = polynomial_from_shards cryptobox shards in
  let slot = Cryptobox.polynomial_to_slot cryptobox polynomial in
  (* Store the slot so that next calls don't require a reconstruction. *)
  let* () = Store.Slots.add_slot store.Store.slots ~slot_size slot slot_id in
  let*! () = Event.(emit fetched_slot (Bytes.length slot, Seq.length shards)) in
  return slot

let get_slot_content ~reconstruct_if_missing ctxt slot_id =
  let open Lwt_result_syntax in
  (* First attempt to get the slot from the slot store. *)
  let store = Node_context.get_store ctxt in
  let cryptobox = Node_context.get_cryptobox ctxt in
  let Cryptobox.{slot_size; _} = Cryptobox.parameters cryptobox in
  let*! res_slot_store =
    Store.Slots.find_slot store.Store.slots ~slot_size slot_id
  in
  match res_slot_store with
  | Ok slot -> return slot
  | Error _ ->
      if reconstruct_if_missing then
        (* The slot could not be obtained from the slot store, attempt a
           reconstruction. *)
        let*! res_shard_store =
          Node_context.may_reconstruct
            ~reconstruct:(get_slot_content_from_shards cryptobox store)
            slot_id
            ctxt
        in
        match res_shard_store with
        | Ok slot -> return slot
        | Error _ -> Lwt.return res_slot_store
      else Lwt.return res_slot_store

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
  | Error `Prover_SRS_not_loaded -> Error (Errors.other [No_prover_SRS])

(* Main functions *)

let add_commitment_shards ~shards_proofs_precomputation node_store cryptobox
    commitment slot polynomial =
  let open Lwt_result_syntax in
  let shards = Cryptobox.shards_from_polynomial cryptobox polynomial in
  let*? precomputation =
    match shards_proofs_precomputation with
    | None -> Error (`Other [No_prover_SRS])
    | Some precomputation -> Ok precomputation
  in
  let shard_proofs =
    Cryptobox.prove_shards cryptobox ~polynomial ~precomputation
  in
  let shares =
    Array.map (fun Cryptobox.{index = _; share} -> share) (Array.of_seq shards)
  in
  Store.cache_entry node_store commitment slot shares shard_proofs ;
  return_unit

let get_opt array i =
  if i >= 0 && i < Array.length array then Some array.(i) else None

module IndexMap = Map.Make (struct
  type t = int

  let compare = compare
end)

(** [shards_to_attesters committee] takes a committee
    [Committee_cache.committee] and returns a function that, given a shard
    index, yields the pkh of its attester for that level. *)
let shards_to_attesters committee =
  let to_array committee =
    Tezos_crypto.Signature.Public_key_hash.Map.fold
      (fun pkh indexes index_map ->
        List.fold_left
          (fun index_map index -> IndexMap.add index pkh index_map)
          index_map
          indexes)
      committee
      IndexMap.empty
    |> IndexMap.bindings |> List.map snd |> Array.of_list
  in
  let committee = to_array committee in
  fun index -> get_opt committee index

(** This function publishes the shards of a commitment that is waiting
    for attestion on L1 if this node has those shards and their proofs
    in memory. *)
let publish_proved_shards (slot_id : Types.slot_id) ~level_committee
    proto_parameters commitment shards shard_proofs gs_worker =
  let open Lwt_result_syntax in
  let attestation_level =
    Int32.(
      pred
      @@ add
           slot_id.slot_level
           (of_int proto_parameters.Dal_plugin.attestation_lag))
  in
  let* committee = level_committee ~level:attestation_level in
  let attester_of_shard = shards_to_attesters committee in
  shards
  |> Seq.iter_ep (fun Cryptobox.{index = shard_index; share} ->
         match
           (attester_of_shard shard_index, get_opt shard_proofs shard_index)
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
             let topic : Types.Topic.t =
               {slot_index = slot_id.slot_index; pkh}
             in
             let message_id =
               Types.Message_id.
                 {
                   commitment;
                   level = slot_id.slot_level;
                   slot_index = slot_id.slot_index;
                   shard_index;
                   pkh;
                 }
             in
             Gossipsub.Worker.(
               Publish_message {message; topic; message_id}
               |> app_input gs_worker) ;
             return_unit)

(** This function publishes the shards of a commitment that is waiting
    for attestion on L1 if this node has those shards and their proofs
    in memory. *)
let publish_slot_data ~level_committee (node_store : Store.t) ~slot_size
    gs_worker proto_parameters commitment slot_id =
  let open Lwt_result_syntax in
  match Store.Commitment_indexed_cache.find_opt node_store.cache commitment with
  | None ->
      (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5676

          If this happens, we should probably tell the user that
          something bad happened. Either:

          1. The proofs where not stored properly (an invariant is broken)

          2. The node was restarted (unlikely to happen given the time frame)

          3. The cache was full (unlikely to happen if
          [shards_proofs_cache_size] is set properly. *)
      return_unit
  | Some (slot, shares, shard_proofs) ->
      let shards =
        shares |> Array.to_seq
        |> Seq.mapi (fun index share -> Cryptobox.{index; share})
      in
      let* () =
        Store.(Shards.write_all node_store.shards slot_id shards)
        |> Errors.to_tzresult
      in
      let* () =
        Store.Slots.add_slot ~slot_size node_store.slots slot slot_id
        |> Errors.to_tzresult
      in
      publish_proved_shards
        slot_id
        ~level_committee
        proto_parameters
        commitment
        shards
        shard_proofs
        gs_worker

let store_slot_headers ~number_of_slots ~block_level slot_headers node_store =
  Store.(add_slot_headers ~number_of_slots ~block_level slot_headers node_store)

let update_selected_slot_headers_statuses ~block_level ~attestation_lag
    ~number_of_slots attested_slots node_store =
  Store.(
    Statuses.update_selected_slot_headers_statuses
      ~block_level
      ~attestation_lag
      ~number_of_slots
      attested_slots
      node_store.slot_header_statuses)

let get_slot_status ~slot_id node_store =
  Store.(Statuses.get_slot_status ~slot_id node_store.slot_header_statuses)

let get_slot_shard (store : Store.t) (slot_id : Types.slot_id) shard_index =
  Store.Shards.read store.shards slot_id shard_index

let get_slot_pages ~reconstruct_if_missing node_context slot_id =
  let open Lwt_result_syntax in
  let proto_parameters = Node_context.get_proto_parameters node_context in
  let page_size = proto_parameters.cryptobox_parameters.page_size in
  let* slot = get_slot_content ~reconstruct_if_missing node_context slot_id in
  (* The slot size `Bytes.length slot` should be an exact multiple of `page_size`.
     If this is not the case, we throw an `Illformed_pages` error.
  *)
  let*? pages =
    Bytes.chunk_bytes
      page_size
      slot
      ~error_on_partial_chunk:(Errors.other @@ TzTrace.make Illformed_pages)
  in
  return pages
