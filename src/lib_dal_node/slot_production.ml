(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type error +=
  | Cryptobox_proof_error of string
  | Post_slot_too_large of {expected : int; got : int}
  | No_prover_profile

let () =
  register_error_kind
    `Permanent
    ~id:"cryptobox_proof_error"
    ~title:"cryptobox error"
    ~description:"A wrapper around an error raised by the cryptobox of the DAL."
    ~pp:(fun fmt msg ->
      Format.fprintf
        fmt
        "The DAL cryptobox proof_from_polynomial failed with:@.'%s'"
        msg)
    Data_encoding.(obj1 (req "explanation" string))
    (function Cryptobox_proof_error msg -> Some msg | _ -> None)
    (fun msg -> Cryptobox_proof_error msg) ;
  register_error_kind
    `Permanent
    ~id:"post_slot_too_large"
    ~title:"Post slot too large"
    ~description:
      "The length of posted data exceeds the expected size of DAL slots."
    ~pp:(fun fmt (expected, got) ->
      Format.fprintf
        fmt
        "The RPC expects a slot_size of at most '%d'. Got: '%d' expected got"
        expected
        got)
    Data_encoding.(obj2 (req "expected" int31) (req "got" int31))
    (function
      | Post_slot_too_large {expected; got} -> Some (expected, got) | _ -> None)
    (fun (expected, got) -> Post_slot_too_large {expected; got}) ;
  register_error_kind
    `Permanent
    ~id:"no_prover_profile"
    ~title:"No prover profile"
    ~description:
      "The DAL node does not have a prover profile to accept slots injection."
    Data_encoding.unit
    (function No_prover_profile -> Some () | _ -> None)
    (fun () -> No_prover_profile)

(* This module is used below to maintain a bounded cache of recently injected
     slots to quickly return the commitment and commitment_proof of an already
     known slot. *)
module Injected_slots_cache =
  Aches.Vache.Map (Aches.Vache.FIFO_Precise) (Aches.Vache.Strong)
    (struct
      type t = string

      let equal = String.equal

      let hash = Hashtbl.hash
    end)

let commitment_proof_from_polynomial cryptobox polynomial =
  let open Result_syntax in
  match Cryptobox.prove_commitment cryptobox polynomial with
  (* [polynomial] was produced with the parameters from
       [cryptobox], thus we can always compute the proof from
       [polynomial] except if an error happens with the loading of the SRS. *)
  | Error
      (`Invalid_degree_strictly_less_than_expected _ | `Prover_SRS_not_loaded)
    ->
      Error
        (Errors.other
           [
             Cryptobox_proof_error
               "Unexpected error. Maybe an issue with the SRS from the DAL \
                node.";
           ])
  | Ok proof -> return proof

let produce_commitment_and_proof =
  let slots_cache =
    Injected_slots_cache.create Constants.not_yet_published_cache_size
  in
  fun ctxt padding slot ->
    let open Lwt_result_syntax in
    let store = Node_context.get_store ctxt in
    match Injected_slots_cache.find_opt slots_cache slot with
    | Some (commitment, commitment_proof)
      when Option.is_some
             (Store.Commitment_indexed_cache.find_opt
                (Store.not_yet_published_cache store)
                commitment) ->
        return (commitment, commitment_proof)
    | _ ->
        let*? cryptobox, shards_proofs_precomputation =
          Node_context.get_cryptobox_and_precomputations ctxt
          |> Errors.other_result
        in
        let profile = Node_context.get_profile_ctxt ctxt in
        let* () =
          if not (Profile_manager.is_prover_profile profile) then
            fail (Errors.other [No_prover_profile])
          else return_unit
        in
        let*? proto_parameters =
          (Node_context.get_proto_parameters ctxt ~level:`Head
          |> Errors.other_result)
          [@profiler.wrap_f
            {driver_ids = [Opentelemetry]}
              (Opentelemetry_helpers.trace_slot_no_commitment
                 ~slot
                 ~name:"get_proto_parameters")]
        in
        let slot_size = proto_parameters.cryptobox_parameters.slot_size in
        let slot_length = String.length slot in
        let*? slot_bytes =
          (if slot_length > slot_size then
             Error
               (Errors.other
                  [
                    Post_slot_too_large {expected = slot_size; got = slot_length};
                  ])
           else if slot_length = slot_size then Ok (Bytes.of_string slot)
           else
             let padding = String.make (slot_size - slot_length) padding in
             Ok (Bytes.of_string (slot ^ padding)))
          [@profiler.wrap_f
            {driver_ids = [Opentelemetry]}
              (Opentelemetry_helpers.trace_slot_no_commitment
                 ~attrs:[]
                 ~slot
                 ~name:"padding_slot")]
        in
        let*? polynomial =
          (Slot_manager.polynomial_from_slot
             cryptobox
             slot_bytes
           [@profiler.wrap_f
             {driver_ids = [Opentelemetry]}
               (Opentelemetry_helpers.trace_slot_no_commitment
                  ~slot
                  ~name:"computing_polynomial")])
        in
        let*? commitment =
          (Slot_manager.commit
             cryptobox
             polynomial
           [@profiler.wrap_f
             {driver_ids = [Opentelemetry]}
               (Opentelemetry_helpers.trace_slot_no_commitment
                  ~slot
                  ~name:"computing_commitment")])
        in
        let*? commitment_proof =
          (commitment_proof_from_polynomial
             cryptobox
             polynomial
           [@profiler.wrap_f
             {driver_ids = [Opentelemetry]}
               (Opentelemetry_helpers.trace_slot_no_commitment
                  ~slot
                  ~name:"computing_commitment_proof")])
        in
        let* () =
          (Slot_manager.add_commitment_shards
             ~shards_proofs_precomputation
             store
             cryptobox
             commitment
             slot_bytes
             polynomial
           [@profiler.wrap_f
             {driver_ids = [Opentelemetry]}
               (Opentelemetry_helpers.trace_slot_no_commitment
                  ~slot
                  ~name:"add_commitment_s_shards")])
        in
        let () =
          (Injected_slots_cache.replace
             slots_cache
             slot
             (commitment, commitment_proof)
           [@profiler.wrap_f
             {driver_ids = [Opentelemetry]}
               (Opentelemetry_helpers.trace_slot_no_commitment
                  ~slot
                  ~name:"store_shards_in_cache")])
        in
        return (commitment, commitment_proof)

module Tests = struct
  let publish_slot_using_client ctxt cctxt block_level slot_index secret_key
      slot_content (module Plugin : Dal_plugin.T) =
    let open Lwt_result_syntax in
    let* commitment, commitment_proof =
      produce_commitment_and_proof ctxt '\000' slot_content
    in
    let source =
      Signature.Public_key.hash @@ Signature.Secret_key.to_public_key secret_key
    in
    let*! res =
      Plugin.publish
        cctxt
        ~block_level
        ~source
        ~slot_index
        ~commitment
        ~commitment_proof
        ~src_sk:secret_key
        ()
    in
    let*! () =
      match res with
      | Ok op_hash -> Event.emit_publication ~block_level ~op_hash
      | Error error -> Event.emit_publication_failed ~block_level ~error
    in
    return_unit
end
