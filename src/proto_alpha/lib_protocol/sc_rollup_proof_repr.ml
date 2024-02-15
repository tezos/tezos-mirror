(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type error += Sc_rollup_proof_check of string

type error += Sc_rollup_invalid_serialized_inbox_proof

let () =
  register_error_kind
    `Permanent
    ~id:"smart_rollup_proof_check"
    ~title:"Invalid proof"
    ~description:"An invalid proof has been submitted"
    ~pp:(fun fmt msg -> Format.fprintf fmt "Invalid proof: %s" msg)
    Data_encoding.(obj1 @@ req "reason" (string Plain))
    (function Sc_rollup_proof_check msg -> Some msg | _ -> None)
    (fun msg -> Sc_rollup_proof_check msg) ;

  register_error_kind
    `Permanent
    ~id:"smart_rollup_invalid_serialized_inbox_proof"
    ~title:"Invalid serialized inbox proof"
    ~description:"The serialized inbox proof can not be de-serialized"
    ~pp:(fun fmt () -> Format.fprintf fmt "Invalid serialized inbox proof")
    Data_encoding.unit
    (function Sc_rollup_invalid_serialized_inbox_proof -> Some () | _ -> None)
    (fun () -> Sc_rollup_invalid_serialized_inbox_proof)

type reveal_proof =
  | Raw_data_proof of string
  | Metadata_proof
  | Dal_page_proof of {
      page_id : Dal_slot_repr.Page.t;
      proof : Dal_slot_repr.History.proof;
    }
  | Dal_parameters_proof

let reveal_proof_encoding =
  let open Data_encoding in
  let case_raw_data =
    case
      ~title:"raw data proof"
      (Tag 0)
      (obj2
         (req "reveal_proof_kind" (constant "raw_data_proof"))
         (req
            "raw_data"
            Bounded.(
              string
                ~length_kind:`Uint16
                Hex
                Constants_repr.sc_rollup_message_size_limit)))
      (function Raw_data_proof s -> Some ((), s) | _ -> None)
      (fun ((), s) -> Raw_data_proof s)
  and case_metadata_proof =
    case
      ~title:"metadata proof"
      (Tag 1)
      (obj1 (req "reveal_proof_kind" (constant "metadata_proof")))
      (function Metadata_proof -> Some () | _ -> None)
      (fun () -> Metadata_proof)
  in
  let case_dal_page =
    case
      ~title:"dal page proof"
      (Tag 2)
      (obj3
         (req "reveal_proof_kind" (constant "dal_page_proof"))
         (req "dal_page_id" Dal_slot_repr.Page.encoding)
         (req "dal_proof" Dal_slot_repr.History.proof_encoding))
      (function
        | Dal_page_proof {page_id; proof} -> Some ((), page_id, proof)
        | _ -> None)
      (fun ((), page_id, proof) -> Dal_page_proof {page_id; proof})
  in
  let case_dal_parameters =
    case
      ~title:"dal parameters proof"
      (Tag 3)
      (obj1 (req "reveal_proof_kind" (constant "dal_parameters_proof")))
      (function Dal_parameters_proof -> Some () | _ -> None)
      (fun () -> Dal_parameters_proof)
  in
  union [case_raw_data; case_metadata_proof; case_dal_page; case_dal_parameters]

type input_proof =
  | Inbox_proof of {
      level : Raw_level_repr.t;
      message_counter : Z.t;
      proof : Sc_rollup_inbox_repr.serialized_proof;
    }
  | Reveal_proof of reveal_proof
  | First_inbox_message

let input_proof_encoding =
  let open Data_encoding in
  let proof_kind kind = req "input_proof_kind" (constant kind) in
  let case_inbox_proof =
    case
      ~title:"inbox proof"
      (Tag 0)
      (obj4
         (proof_kind "inbox_proof")
         (req "level" Raw_level_repr.encoding)
         (req "message_counter" Data_encoding.n)
         (req "serialized_proof" Sc_rollup_inbox_repr.serialized_proof_encoding))
      (function
        | Inbox_proof {level; message_counter; proof} ->
            Some ((), level, message_counter, proof)
        | _ -> None)
      (fun ((), level, message_counter, proof) ->
        Inbox_proof {level; message_counter; proof})
  in
  let case_reveal_proof =
    case
      ~title:"reveal proof"
      (Tag 1)
      (obj2
         (proof_kind "reveal_proof")
         (req "reveal_proof" reveal_proof_encoding))
      (function Reveal_proof s -> Some ((), s) | _ -> None)
      (fun ((), s) -> Reveal_proof s)
  in
  let first_input =
    case
      ~title:"first input"
      (Tag 2)
      (obj1 (proof_kind "first_input"))
      (function First_inbox_message -> Some () | _ -> None)
      (fun () -> First_inbox_message)
  in
  union [case_inbox_proof; case_reveal_proof; first_input]

type 'proof t = {pvm_step : 'proof; input_proof : input_proof option}

type serialized = string

let serialize_pvm_step (type state proof output)
    ~(pvm : (state, proof, output) Sc_rollups.PVM.implementation)
    (proof : proof) : serialized tzresult =
  let open Result_syntax in
  let (module PVM) = pvm in
  match Data_encoding.Binary.to_string_opt PVM.proof_encoding proof with
  | Some p -> return p
  | None -> tzfail (Sc_rollup_proof_check "Cannot serialize proof")

let unserialize_pvm_step (type state proof output)
    ~(pvm : (state, proof, output) Sc_rollups.PVM.implementation)
    (proof : string) : proof tzresult =
  let open Result_syntax in
  let (module PVM) = pvm in
  match Data_encoding.Binary.of_string_opt PVM.proof_encoding proof with
  | Some p -> return p
  | None -> tzfail (Sc_rollup_proof_check "Cannot unserialize proof")

let serialized_encoding = Data_encoding.string Hex

let encoding =
  let open Data_encoding in
  conv
    (fun {pvm_step; input_proof} -> (pvm_step, input_proof))
    (fun (pvm_step, input_proof) -> {pvm_step; input_proof})
    (obj2
       (req "pvm_step" serialized_encoding)
       (opt "input_proof" input_proof_encoding))

let pp ppf _ = Format.fprintf ppf "Refutation game proof"

let start_of_pvm_step (type state proof output)
    ~(pvm : (state, proof, output) Sc_rollups.PVM.implementation)
    (proof : proof) =
  let (module P) = pvm in
  P.proof_start_state proof

let stop_of_pvm_step (type state proof output)
    ~(pvm : (state, proof, output) Sc_rollups.PVM.implementation)
    (proof : proof) =
  let (module P) = pvm in
  P.proof_stop_state proof

(* This takes an [input] and checks if it is above the given level,
   and if it is at or below the origination level for this rollup.
   It returns [None] if this is the case.

   We use this to check that the PVM proof is obeying [commit_inbox_level]
   correctly---if the message obtained from the inbox proof is above
   [commit_inbox_level] the [input_given] in the PVM proof should be [None]. *)
let cut_at_level ~origination_level ~commit_inbox_level
    (input : Sc_rollup_PVM_sig.input) =
  match input with
  | Inbox_message {inbox_level = input_level; _} ->
      if
        Raw_level_repr.(
          input_level <= origination_level || commit_inbox_level < input_level)
      then None
      else Some input
  | Reveal _data -> Some input

let proof_error reason =
  let open Lwt_result_syntax in
  tzfail (Sc_rollup_proof_check reason)

let check p reason =
  let open Lwt_result_syntax in
  if p then return_unit else proof_error reason

let check_inbox_proof snapshot serialized_inbox_proof (level, counter) =
  match Sc_rollup_inbox_repr.of_serialized_proof serialized_inbox_proof with
  | None -> Result_syntax.tzfail Sc_rollup_invalid_serialized_inbox_proof
  | Some inbox_proof ->
      Sc_rollup_inbox_repr.verify_proof (level, counter) snapshot inbox_proof

module Dal_helpers = struct
  (* FIXME/DAL: https://gitlab.com/tezos/tezos/-/issues/3997
     The current DAL refutation integration is not resilient to DAL parameters
     changes when upgrading the protocol. The code needs to be adapted. *)

  let valid_slot_id ~dal_number_of_slots ~dal_activation_level
      ~dal_attestation_lag ~origination_level ~commit_inbox_level
      Dal_slot_repr.Header.{published_level; index}
      ~dal_attested_slots_validity_lag =
    (* [dal_attestation_lag] is supposed to be positive. *)
    let open Raw_level_repr in
    let dal_was_activated =
      match dal_activation_level with
      | None -> false
      | Some dal_activation_level -> published_level >= dal_activation_level
    in
    let slot_published_after_origination =
      published_level > origination_level
    in
    let not_too_recent =
      add published_level dal_attestation_lag <= commit_inbox_level
    in
    let index_is_valid =
      Result.is_ok
      @@ Dal_slot_index_repr.check_is_in_range
           ~number_of_slots:dal_number_of_slots
           index
    in
    (* An attested slot is not expired if its attested level (equal to
       [published_level + dal_attestation_lag]) is not further than
       [dal_attested_slots_validity_lag] from the given inbox level. *)
    let ttl_not_expired =
      Raw_level_repr.(
        add
          (add published_level dal_attestation_lag)
          dal_attested_slots_validity_lag
        >= commit_inbox_level)
    in
    dal_was_activated && slot_published_after_origination && not_too_recent
    && index_is_valid && ttl_not_expired

  let page_id_is_valid ~dal_number_of_slots ~dal_activation_level
      ~dal_attestation_lag ~origination_level ~commit_inbox_level
      cryptobox_parameters
      Dal_slot_repr.Page.{slot_id = {published_level; index}; page_index}
      ~dal_attested_slots_validity_lag =
    let open Dal_slot_repr in
    Result.is_ok
      (Page.Index.check_is_in_range
         ~number_of_pages:(Page.pages_per_slot cryptobox_parameters)
         page_index)
    && Result.is_ok
         (Dal_slot_index_repr.check_is_in_range
            ~number_of_slots:dal_number_of_slots
            index)
    && valid_slot_id
         ~dal_number_of_slots
         ~dal_activation_level
         ~dal_attestation_lag
         ~origination_level
         ~commit_inbox_level
         Dal_slot_repr.Header.{published_level; index}
         ~dal_attested_slots_validity_lag

  let verify ~metadata ~dal_activation_level ~dal_attestation_lag
      ~dal_number_of_slots ~commit_inbox_level dal_parameters page_id
      dal_snapshot proof ~dal_attested_slots_validity_lag =
    let open Result_syntax in
    if
      page_id_is_valid
        dal_parameters
        ~dal_activation_level
        ~origination_level:metadata.Sc_rollup_metadata_repr.origination_level
        ~dal_attestation_lag
        ~commit_inbox_level
        ~dal_number_of_slots
        page_id
        ~dal_attested_slots_validity_lag
    then
      let* input =
        Dal_slot_repr.History.verify_proof
          dal_parameters
          page_id
          dal_snapshot
          proof
      in
      return_some (Sc_rollup_PVM_sig.Reveal (Dal_page input))
    else return_none

  let produce ~metadata ~dal_activation_level ~dal_attestation_lag
      ~dal_number_of_slots ~commit_inbox_level dal_parameters page_id ~page_info
      ~get_history confirmed_slots_history ~dal_attested_slots_validity_lag =
    let open Lwt_result_syntax in
    if
      page_id_is_valid
        dal_parameters
        ~dal_number_of_slots
        ~dal_activation_level
        ~origination_level:metadata.Sc_rollup_metadata_repr.origination_level
        ~dal_attestation_lag
        ~commit_inbox_level
        page_id
        ~dal_attested_slots_validity_lag
    then
      let* proof, content_opt =
        Dal_slot_repr.History.produce_proof
          dal_parameters
          page_id
          ~page_info
          ~get_history
          confirmed_slots_history
      in
      return
        ( Some (Reveal_proof (Dal_page_proof {proof; page_id})),
          Some (Sc_rollup_PVM_sig.Reveal (Dal_page content_opt)) )
    else return (None, None)
end

let valid (type state proof output)
    ~(pvm : (state, proof, output) Sc_rollups.PVM.implementation) ~metadata
    snapshot commit_inbox_level dal_snapshot dal_parameters
    ~dal_activation_level ~dal_attestation_lag ~dal_number_of_slots
    ~is_reveal_enabled ~dal_attested_slots_validity_lag (proof : proof t) =
  let open Lwt_result_syntax in
  let (module P) = pvm in
  let origination_level = metadata.Sc_rollup_metadata_repr.origination_level in
  let* input =
    match proof.input_proof with
    | None -> return_none
    | Some (Inbox_proof {level; message_counter; proof}) ->
        let*? inbox_message =
          check_inbox_proof snapshot proof (level, Z.succ message_counter)
        in
        return
        @@ Option.map (fun i -> Sc_rollup_PVM_sig.Inbox_message i) inbox_message
    | Some First_inbox_message ->
        let*? payload =
          Sc_rollup_inbox_message_repr.(serialize (Internal Start_of_level))
        in
        let inbox_level = Raw_level_repr.succ origination_level in
        let message_counter = Z.zero in
        return_some
          Sc_rollup_PVM_sig.(
            Inbox_message {inbox_level; message_counter; payload})
    | Some (Reveal_proof (Raw_data_proof data)) ->
        return_some (Sc_rollup_PVM_sig.Reveal (Raw_data data))
    | Some (Reveal_proof Metadata_proof) ->
        return_some (Sc_rollup_PVM_sig.Reveal (Metadata metadata))
    | Some (Reveal_proof (Dal_page_proof {proof; page_id})) ->
        Dal_helpers.verify
          ~dal_number_of_slots
          ~metadata
          ~dal_activation_level
          ~dal_attested_slots_validity_lag
          dal_parameters
          ~dal_attestation_lag
          ~commit_inbox_level
          page_id
          dal_snapshot
          proof
        |> Lwt.return
    | Some (Reveal_proof Dal_parameters_proof) ->
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/6562
           Support revealing historical DAL parameters.

           Currently, we do not support revealing DAL parameters for the past.
           We ignore the given [published_level] and use the DAL parameters. *)
        return_some
          (Sc_rollup_PVM_sig.Reveal
             (Dal_parameters
                Sc_rollup_dal_parameters_repr.
                  {
                    number_of_slots = Int64.of_int dal_number_of_slots;
                    attestation_lag = Int64.of_int dal_attestation_lag;
                    slot_size = Int64.of_int dal_parameters.slot_size;
                    page_size = Int64.of_int dal_parameters.page_size;
                  }))
  in
  let input =
    Option.bind input (cut_at_level ~origination_level ~commit_inbox_level)
  in
  let* input_requested =
    P.verify_proof ~is_reveal_enabled input proof.pvm_step
  in
  let* () =
    match (proof.input_proof, input_requested) with
    | None, No_input_required -> return_unit
    | Some First_inbox_message, Initial ->
        (* If the state is [Initial], we don't need a proof of the input,
           we know it's the [Start_of_level] after the origination. *)
        return_unit
    | Some (Inbox_proof {level; message_counter; proof = _}), First_after (l, n)
      ->
        check
          (Raw_level_repr.(level = l) && Z.(equal message_counter n))
          "Level and index of inbox proof are not equal to the one expected in \
           input request."
    | ( Some (Reveal_proof (Raw_data_proof data)),
        Needs_reveal (Reveal_raw_data expected_hash) ) ->
        let scheme = Sc_rollup_reveal_hash.scheme_of_hash expected_hash in

        let data_hash = Sc_rollup_reveal_hash.hash_string ~scheme [data] in
        check
          (Sc_rollup_reveal_hash.equal data_hash expected_hash)
          "Invalid reveal"
    | Some (Reveal_proof Metadata_proof), Needs_reveal Reveal_metadata ->
        return_unit
    | ( Some (Reveal_proof (Dal_page_proof {page_id; proof = _})),
        Needs_reveal (Request_dal_page pid) ) ->
        check
          (Dal_slot_repr.Page.equal page_id pid)
          "Dal proof's page ID is not the one expected in input request."
    | ( Some (Reveal_proof Dal_parameters_proof),
        Needs_reveal Reveal_dal_parameters ) ->
        return_unit
    | None, (Initial | First_after _ | Needs_reveal _)
    | Some _, No_input_required
    | Some (Inbox_proof _), Needs_reveal _
    | _ ->
        proof_error "Inbox proof and input request are dissociated."
  in
  return (input, input_requested)

module type PVM_with_context_and_state = sig
  include Sc_rollups.PVM.S

  val context : context

  val state : state

  val proof_encoding : proof Data_encoding.t

  val reveal : Sc_rollup_reveal_hash.t -> string option Lwt.t

  module Inbox_with_history : sig
    val inbox : Sc_rollup_inbox_repr.history_proof

    val get_history :
      Sc_rollup_inbox_repr.Hash.t ->
      Sc_rollup_inbox_repr.history_proof option Lwt.t

    val get_payloads_history :
      Sc_rollup_inbox_merkelized_payload_hashes_repr.Hash.t ->
      Sc_rollup_inbox_merkelized_payload_hashes_repr.History.t Lwt.t
  end

  module Dal_with_history : sig
    val confirmed_slots_history : Dal_slot_repr.History.t

    val get_history :
      Dal_slot_repr.History.hash -> Dal_slot_repr.History.t option Lwt.t

    val page_info :
      (Dal_slot_repr.Page.content * Dal_slot_repr.Page.proof) option

    val dal_parameters : Dal_slot_repr.parameters

    val dal_attestation_lag : int

    val dal_number_of_slots : int

    val dal_activation_level : Raw_level_repr.t option

    val dal_attested_slots_validity_lag : int
  end
end

let produce ~metadata pvm_and_state commit_inbox_level ~is_reveal_enabled =
  let open Lwt_result_syntax in
  let (module P : PVM_with_context_and_state) = pvm_and_state in
  let open P in
  let*! (request : Sc_rollup_PVM_sig.input_request) =
    P.is_input_state ~is_reveal_enabled P.state
  in
  let origination_level = metadata.Sc_rollup_metadata_repr.origination_level in
  let* input_proof, input_given =
    match request with
    | No_input_required -> return (None, None)
    | Initial ->
        (* The first input of a rollup is the [Start_of_level] after its
           origination. *)
        let* input =
          let*? payload =
            Sc_rollup_inbox_message_repr.(serialize (Internal Start_of_level))
          in
          let inbox_level = Raw_level_repr.succ origination_level in
          let message_counter = Z.zero in
          return_some
            Sc_rollup_PVM_sig.(
              Inbox_message {inbox_level; message_counter; payload})
        in
        let inbox_proof = First_inbox_message in
        return (Some inbox_proof, input)
    | First_after (level, message_counter) ->
        let* inbox_proof, input =
          Inbox_with_history.(
            Sc_rollup_inbox_repr.produce_proof
              ~get_payloads_history
              ~get_history
              inbox
              (level, Z.succ message_counter))
        in
        let input =
          Option.map (fun msg -> Sc_rollup_PVM_sig.Inbox_message msg) input
        in
        let inbox_proof =
          Inbox_proof
            {
              level;
              message_counter;
              proof = Sc_rollup_inbox_repr.to_serialized_proof inbox_proof;
            }
        in
        return (Some inbox_proof, input)
    | Needs_reveal (Reveal_raw_data h) -> (
        let*! res = reveal h in
        match res with
        | None -> proof_error "No reveal"
        | Some data ->
            return
              ( Some (Reveal_proof (Raw_data_proof data)),
                Some (Sc_rollup_PVM_sig.Reveal (Raw_data data)) ))
    | Needs_reveal Reveal_metadata ->
        return
          ( Some (Reveal_proof Metadata_proof),
            Some Sc_rollup_PVM_sig.(Reveal (Metadata metadata)) )
    | Needs_reveal (Request_dal_page page_id) ->
        let open Dal_with_history in
        Dal_helpers.produce
          ~dal_number_of_slots
          ~metadata
          ~dal_activation_level
          dal_parameters
          ~dal_attestation_lag
          ~commit_inbox_level
          page_id
          ~page_info
          ~get_history
          ~dal_attested_slots_validity_lag
          confirmed_slots_history
    | Needs_reveal Reveal_dal_parameters ->
        let open Dal_with_history in
        return
          ( Some (Reveal_proof Dal_parameters_proof),
            Some
              Sc_rollup_PVM_sig.(
                Reveal
                  (Dal_parameters
                     Sc_rollup_dal_parameters_repr.
                       {
                         number_of_slots = Int64.of_int dal_number_of_slots;
                         attestation_lag = Int64.of_int dal_attestation_lag;
                         slot_size = Int64.of_int dal_parameters.slot_size;
                         page_size = Int64.of_int dal_parameters.page_size;
                       })) )
  in
  let input_given =
    Option.bind
      input_given
      (cut_at_level ~origination_level ~commit_inbox_level)
  in
  let* pvm_step_proof =
    P.produce_proof P.context ~is_reveal_enabled input_given P.state
  in
  let*? pvm_step = serialize_pvm_step ~pvm:(module P) pvm_step_proof in
  return {pvm_step; input_proof}

module Internal_for_tests = struct
  let cut_at_level = cut_at_level
end
