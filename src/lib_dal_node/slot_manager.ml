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
         the current profile of your DAL node to a slot producer one (operator \
         or observer) to be able to compute proofs.")
    Data_encoding.empty
    (function No_prover_SRS -> Some () | _ -> None)
    (fun () -> No_prover_SRS)

type slot = bytes

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
      let*! res = Store.Shards.read (Store.shards store) slot_id shard_id in
      match res with
      | Ok res -> loop (Seq.cons res acc) (shard_id + 1) (remaining - 1)
      | Error _ -> loop acc (shard_id + 1) remaining
  in
  let* shards = loop Seq.empty 0 minimal_number_of_shards in
  let* polynomial = polynomial_from_shards cryptobox shards in
  let slot = Cryptobox.polynomial_to_slot cryptobox polynomial in
  (* Store the slot so that next calls don't require a reconstruction. *)
  let* () = Store.Slots.add_slot (Store.slots store) ~slot_size slot slot_id in
  let*! () =
    Event.emit_reconstructed_slot
      ~size:(Bytes.length slot)
      ~shards:(Seq.length shards)
  in
  return slot

let fetch_slot_from_http_uri ~slot_size ~published_level ~slot_index
    http_backup_uri =
  let open Lwt_syntax in
  let url =
    Uri.with_path
      http_backup_uri
      String.(
        concat
          "/"
          [
            "v0";
            "slots";
            "by_published_level";
            Format.sprintf "%ld_%d_%d" published_level slot_index slot_size;
          ])
  in
  let* resp, body = Cohttp_lwt_unix.Client.get url in
  match resp.status with
  | `OK ->
      let* body_str = Cohttp_lwt.Body.to_string body in
      return_some (Bytes.of_string body_str)
  | #Cohttp.Code.status_code as status ->
      (* Consume the body of the request in case of failure to avoid leaking stream!
         See https://github.com/mirage/ocaml-cohttp/issues/730 *)
      let* _ = Cohttp_lwt.Body.drain_body body in
      let* () =
        Event.emit_fetching_slot_from_http_backup_failed
          ~published_level
          ~slot_index
          ~http_backup_uri
          ~status
      in
      return_none

let try_fetch_slot_from_http_backup ~slot_size ~published_level ~slot_index
    cryptobox expected_commitment_hash http_backup_uri =
  let open Lwt_result_syntax in
  let fetch_and_sanitize_slot_content () =
    let open Lwt_syntax in
    (* /!\ Warning: We are fetching the slot content as stored by another DAL
       node on disk into its store/slot_store/ directory. Currently the
       home-made KVS we use appends extra bytes at the beginning of each
       "file" to chech if values are present. We should takes them into
       account to:
       - compute the expected size of the data
       - fetch the exact slot content, without encoding artifacts when written
         to disk. *)
    let* slot_opt =
      fetch_slot_from_http_uri
        ~slot_size
        ~published_level
        ~slot_index
        http_backup_uri
    in
    match slot_opt with
    | None -> return_none
    | Some slot_bytes ->
        let expected_size =
          slot_size + Key_value_store.file_prefix_bitset_size
        in
        let obtained_size = Bytes.length slot_bytes in
        if expected_size != obtained_size then
          let* () =
            Event.emit_slot_from_http_backup_has_unexpected_size
              ~published_level
              ~slot_index
              ~http_backup_uri
              ~expected_size
              ~obtained_size
          in
          return_none
        else
          return_some
          @@ Bytes.sub
               slot_bytes
               Key_value_store.file_prefix_bitset_size
               slot_size
  in
  let*! slot_opt = fetch_and_sanitize_slot_content () in
  match (slot_opt, expected_commitment_hash) with
  | None, _ -> return_none
  | Some slot, None ->
      (* We trust the http server, no extra checks to do. *)
      return_some slot
  | Some slot, Some expected_commitment ->
      let*? polynomial = polynomial_from_slot cryptobox slot in
      let*? obtained_commitment = commit cryptobox polynomial in
      if Cryptobox.Commitment.equal expected_commitment obtained_commitment then
        return_some slot
      else
        let*! () =
          Event.emit_slot_from_http_backup_has_unexpected_commitment
            ~published_level
            ~slot_index
            ~http_backup_uri
            ~expected_commitment
            ~obtained_commitment
        in
        return_none

let get_commitment_from_slot_id _ctxt _slot_id =
  (* TODO in follow-up MRs *)
  assert false

let fetch_slot_from_http_backups ctxt cryptobox ~slot_size slot_id =
  let open Lwt_result_syntax in
  let config : Configuration_file.t = Node_context.get_config ctxt in
  let Types.Slot_id.{slot_index; slot_level = published_level} = slot_id in
  match config.http_backup_uris with
  | [] ->
      (* Fail if no http backup URI is configured. *)
      fail Errors.not_found
  | http_backup_uris -> (
      (* We fetch the expected commitment hash from the published slot header on
         L1 if [trust_http_backup_uris] is false. *)
      let* expected_commitment_hash =
        if config.trust_http_backup_uris then return_none
        else get_commitment_from_slot_id ctxt slot_id
      in
      let* slot_opt =
        List.find_map_es
          (fun uri ->
            try_fetch_slot_from_http_backup
              cryptobox
              ~slot_size
              ~published_level
              ~slot_index
              expected_commitment_hash
              uri)
          http_backup_uris
      in
      match slot_opt with
      | None -> fail Errors.not_found
      | Some slot -> return slot)

let get_slot_content ~reconstruct_if_missing ctxt slot_id =
  let open Lwt_result_syntax in
  (* First attempt to get the slot from the slot store. *)
  let store = Node_context.get_store ctxt in
  let cryptobox = Node_context.get_cryptobox ctxt in
  let Cryptobox.{slot_size; _} = Cryptobox.parameters cryptobox in
  let*! res_slot_store =
    Store.Slots.find_slot (Store.slots store) ~slot_size slot_id
  in
  match res_slot_store with
  | Ok slot -> return slot
  | Error _ -> (
      let*! res_shard_store =
        if reconstruct_if_missing then
          (* The slot could not be obtained from the slot store, attempt a
             reconstruction. *)
          let*! res_shard_store =
            Node_context.may_reconstruct
              ~reconstruct:(get_slot_content_from_shards cryptobox store)
              slot_id
              ctxt
          in
          Lwt.return_some res_shard_store
        else Lwt.return_none
      in
      match res_shard_store with
      | Some (Ok slot) -> return slot
      | Some (Error _) | None ->
          fetch_slot_from_http_backups ctxt cryptobox ~slot_size slot_id)

(* Main functions *)

let maybe_register_trap traps_store ~traps_fraction message_id message =
  let delegate = message_id.Types.Message_id.pkh in
  let Types.Message.{share; shard_proof} = message in
  let Types.Message_id.{slot_index; level; shard_index; _} = message_id in
  let trap_res = Trap.share_is_trap ~traps_fraction delegate share in
  match trap_res with
  | Ok true ->
      let slot_id = Types.Slot_id.{slot_index; slot_level = level} in
      let () =
        Event.emit_dont_wait__register_trap
          ~delegate
          ~published_level:slot_id.slot_level
          ~slot_index:slot_id.slot_index
          ~shard_index
      in
      Store.Traps.add
        traps_store
        ~slot_id
        ~shard_index
        ~delegate
        ~share
        ~shard_proof
  | Ok false -> ()
  | Error _ ->
      Lwt.dont_wait
        (fun () ->
          Event.emit_trap_check_failure
            ~delegate
            ~published_level:level
            ~slot_index
            ~shard_index)
        (fun exc -> raise exc)

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
    Signature.Public_key_hash.Map.fold
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
    for attestation on L1 if this node has those shards and their proofs
    in memory. *)
let publish_proved_shards ctxt (slot_id : Types.slot_id) ~level_committee
    proto_parameters commitment shards shard_proofs gs_worker =
  let open Lwt_result_syntax in
  let attestation_level =
    Int32.(
      pred
      @@ add slot_id.slot_level (of_int proto_parameters.Types.attestation_lag))
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
             let store = Node_context.get_store ctxt in
             let traps_store = Store.traps store in
             (* TODO: https://gitlab.com/tezos/tezos/-/issues/7742
                The [proto_parameters] are those for the last known finalized
                level, which may differ from those of the slot level. This will
                be an issue when the value of the [traps_fraction] changes.*)
             let traps_fraction = proto_parameters.traps_fraction in
             let () =
               maybe_register_trap
                 traps_store
                 ~traps_fraction
                 message_id
                 message
             in
             Gossipsub.Worker.(
               Publish_message {message; topic; message_id}
               |> app_input gs_worker) ;
             return_unit)

(** This function publishes the shards of a commitment that is waiting
    for attestion on L1 if this node has those shards and their proofs
    in memory. *)
let publish_slot_data ctxt ~level_committee ~slot_size gs_worker
    proto_parameters commitment slot_id =
  let open Lwt_result_syntax in
  let node_store = Node_context.get_store ctxt in
  let cache = Store.cache node_store in
  match Store.Commitment_indexed_cache.find_opt cache commitment with
  | None ->
      let*! () =
        if
          Profile_manager.can_publish_on_slot_index
            slot_id.Types.Slot_id.slot_index
            (Node_context.get_profile_ctxt ctxt)
        then
          (* This is unexpected. Either:
             1. The proofs where not stored properly (an invariant is broken)
             2. The node was restarted (unlikely to happen given the time frame)
             3. The cache was full (unlikely to happen if
             [shards_proofs_cache_size] is set properly. *)
          Event.emit_commitment_not_found_in_cache ~commitment
        else
          (* The node is likely not concerned with the publication of the shards
             of this commit. *)
          Lwt.return_unit
      in
      return_unit
  | Some (slot, shares, shard_proofs) ->
      let shards =
        shares |> Array.to_seq
        |> Seq.mapi (fun index share -> Cryptobox.{index; share})
      in
      let* () =
        Store.Shards.write_all (Store.shards node_store) slot_id shards
        |> Errors.to_tzresult
      in
      let* () =
        Store.Slots.add_slot ~slot_size (Store.slots node_store) slot slot_id
        |> Errors.to_tzresult
      in
      publish_proved_shards
        ctxt
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
  let slot_header_statuses_store = Store.slot_header_statuses node_store in
  Store.Statuses.update_selected_slot_headers_statuses
    ~block_level
    ~attestation_lag
    ~number_of_slots
    attested_slots
    slot_header_statuses_store

let get_slot_status ~slot_id node_store =
  let slot_header_statuses_store = Store.slot_header_statuses node_store in
  Store.Statuses.get_slot_status ~slot_id slot_header_statuses_store

let get_slot_shard (store : Store.t) (slot_id : Types.slot_id) shard_index =
  Store.Shards.read (Store.shards store) slot_id shard_index

let get_slot_pages ~reconstruct_if_missing node_context slot_id =
  let open Lwt_result_syntax in
  let* proto_parameters =
    Node_context.get_proto_parameters
      node_context
      ~level:(`Level slot_id.Types.Slot_id.slot_level)
    |> Lwt.return
    |> lwt_map_error (fun e -> `Other e)
  in
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
