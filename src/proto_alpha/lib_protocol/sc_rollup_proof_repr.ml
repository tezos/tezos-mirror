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
    ~id:"Sc_rollup_proof_check"
    ~title:"Invalid proof"
    ~description:"An invalid proof has been submitted"
    ~pp:(fun fmt msg -> Format.fprintf fmt "Invalid proof: %s" msg)
    Data_encoding.(obj1 @@ req "reason" string)
    (function Sc_rollup_proof_check msg -> Some msg | _ -> None)
    (fun msg -> Sc_rollup_proof_check msg) ;

  register_error_kind
    `Permanent
    ~id:"Sc_rollup_invalid_serialized_inbox_proof"
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
            (check_size Constants_repr.sc_rollup_message_size_limit bytes)))
      (function Raw_data_proof s -> Some ((), Bytes.of_string s) | _ -> None)
      (fun ((), s) -> Raw_data_proof (Bytes.to_string s))
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
  union [case_raw_data; case_metadata_proof; case_dal_page]

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

type t = {pvm_step : Sc_rollups.wrapped_proof; input_proof : input_proof option}

let encoding =
  let open Data_encoding in
  conv
    (fun {pvm_step; input_proof} -> (pvm_step, input_proof))
    (fun (pvm_step, input_proof) -> {pvm_step; input_proof})
    (obj2
       (req "pvm_step" Sc_rollups.wrapped_proof_encoding)
       (opt "input_proof" input_proof_encoding))

let pp ppf _ = Format.fprintf ppf "Refutation game proof"

let start proof =
  let (module P) = Sc_rollups.wrapped_proof_module proof.pvm_step in
  P.proof_start_state P.proof

let stop proof =
  let (module P) = Sc_rollups.wrapped_proof_module proof.pvm_step in
  P.proof_stop_state P.proof

(* This takes an [input] and checks if it is at or above the given level,
   and if it is at or below the origination level for this rollup.
   It returns [None] if this is the case.

   We use this to check that the PVM proof is obeying [commit_level]
   correctly---if the message obtained from the inbox proof is at or
   above [commit_level] the [input_given] in the PVM proof should be
   [None]. *)
let cut_at_level ~origination_level ~commit_level
    (input : Sc_rollup_PVM_sig.input) =
  match input with
  | Inbox_message {inbox_level = input_level; _} ->
      if
        Raw_level_repr.(
          input_level <= origination_level || commit_level <= input_level)
      then None
      else Some input
  | Reveal _data -> Some input

let proof_error reason =
  let open Lwt_tzresult_syntax in
  fail (Sc_rollup_proof_check reason)

let check p reason =
  let open Lwt_tzresult_syntax in
  if p then return () else proof_error reason

let check_inbox_proof snapshot serialized_inbox_proof (level, counter) =
  match Sc_rollup_inbox_repr.of_serialized_proof serialized_inbox_proof with
  | None -> fail Sc_rollup_invalid_serialized_inbox_proof
  | Some inbox_proof ->
      Sc_rollup_inbox_repr.verify_proof (level, counter) snapshot inbox_proof

module Dal_proofs = struct
  (* FIXME/DAL: https://gitlab.com/tezos/tezos/-/issues/3997
     The current DAL refutation integration is not resilient to DAL parameters
     changes when upgrading the protocol. The code needs to be adapted. *)
  (** Given a page, identified by its ID, we accept to produce or verify a
      proof for it if, and only if, the page's level [page_published_level]
      is in the following boundaries:
      - page_published_level > origination_level: this means that the slot
        of the page was published after the rollup origination ;
      - page_published_level + dal_endorsement_lag < commit_level: this
        means that the slot of the page has been confirmed before the
        [commit_level]. According to the definition in
        {!Sc_rollup_commitment_repr}, [commit_level] (aka inbox_level
        in that module) is the level (excluded) up to which the PVM consumed
        all messages and DAL/DAC inputs before producing the related commitment.
  *)
  let page_level_is_valid ~dal_endorsement_lag ~origination_level ~commit_level
      page_id =
    (* [dal_endorsement_lag] is supposed to be positive. *)
    let page_published_level =
      Dal_slot_repr.(page_id.Page.slot_id.Header.published_level)
    in
    let open Raw_level_repr in
    let not_too_old = page_published_level > origination_level in
    let not_too_recent =
      add page_published_level dal_endorsement_lag < commit_level
    in
    not_too_old && not_too_recent

  let verify ~metadata ~dal_endorsement_lag ~commit_level dal_parameters page_id
      dal_snapshot proof =
    let open Tzresult_syntax in
    if
      page_level_is_valid
        ~origination_level:metadata.Sc_rollup_metadata_repr.origination_level
        ~dal_endorsement_lag
        ~commit_level
        page_id
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

  let produce ~metadata ~dal_endorsement_lag ~commit_level dal_parameters
      page_id ~page_info confirmed_slots_history history_cache =
    let open Tzresult_syntax in
    if
      page_level_is_valid
        ~origination_level:metadata.Sc_rollup_metadata_repr.origination_level
        ~dal_endorsement_lag
        ~commit_level
        page_id
    then
      let* proof, content_opt =
        Dal_slot_repr.History.produce_proof
          dal_parameters
          page_id
          ~page_info
          confirmed_slots_history
          history_cache
      in
      return
        ( Some (Reveal_proof (Dal_page_proof {proof; page_id})),
          Some (Sc_rollup_PVM_sig.Reveal (Dal_page content_opt)) )
    else return (None, None)
end

let valid ~metadata snapshot commit_level dal_snapshot dal_parameters
    ~dal_endorsement_lag ~pvm_name proof =
  let open Lwt_tzresult_syntax in
  let (module P) = Sc_rollups.wrapped_proof_module proof.pvm_step in
  let* () = check (String.equal P.name pvm_name) "Incorrect PVM kind" in
  let origination_level = metadata.Sc_rollup_metadata_repr.origination_level in
  let* input =
    match proof.input_proof with
    | None -> return_none
    | Some (Inbox_proof {level; message_counter; proof}) ->
        let+ inbox_message =
          check_inbox_proof snapshot proof (level, Z.succ message_counter)
        in
        Option.map (fun i -> Sc_rollup_PVM_sig.Inbox_message i) inbox_message
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
        Dal_proofs.verify
          ~metadata
          dal_parameters
          ~dal_endorsement_lag
          ~commit_level
          page_id
          dal_snapshot
          proof
        |> Lwt.return
  in
  let input =
    Option.bind input (cut_at_level ~origination_level ~commit_level)
  in
  let* input_requested = P.verify_proof input P.proof in
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
        let data_hash = Sc_rollup_PVM_sig.Reveal_hash.hash_string [data] in
        check
          (Sc_rollup_PVM_sig.Reveal_hash.equal data_hash expected_hash)
          "Invalid reveal"
    | Some (Reveal_proof Metadata_proof), Needs_reveal Reveal_metadata ->
        return_unit
    | ( Some (Reveal_proof (Dal_page_proof {page_id; proof = _})),
        Needs_reveal (Request_dal_page pid) ) ->
        check
          (Dal_slot_repr.Page.equal page_id pid)
          "Dal proof's page ID is not the one expected in input request."
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

  val reveal : Sc_rollup_PVM_sig.Reveal_hash.t -> string option

  module Inbox_with_history : sig
    include
      Sc_rollup_inbox_repr.Merkelized_operations
        with type inbox_context = context

    val inbox : Sc_rollup_inbox_repr.history_proof

    val history : Sc_rollup_inbox_repr.History.t
  end

  module Dal_with_history : sig
    val confirmed_slots_history : Dal_slot_repr.History.t

    val history_cache : Dal_slot_repr.History.History_cache.t

    val page_info :
      (Dal_slot_repr.Page.content * Dal_slot_repr.Page.proof) option

    val dal_parameters : Dal_slot_repr.parameters

    val dal_endorsement_lag : int
  end
end

let produce ~metadata pvm_and_state commit_level =
  let open Lwt_tzresult_syntax in
  let (module P : PVM_with_context_and_state) = pvm_and_state in
  let open P in
  let*! (request : Sc_rollup_PVM_sig.input_request) =
    P.is_input_state P.state
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
            produce_proof context history inbox (level, Z.succ message_counter))
        in
        let input =
          Option.map (fun msg -> Sc_rollup_PVM_sig.Inbox_message msg) input
        in
        let inbox_proof =
          Inbox_proof
            {
              level;
              message_counter;
              proof = Inbox_with_history.to_serialized_proof inbox_proof;
            }
        in
        return (Some inbox_proof, input)
    | Needs_reveal (Reveal_raw_data h) -> (
        match reveal h with
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
        Dal_proofs.produce
          ~metadata
          dal_parameters
          ~dal_endorsement_lag
          ~commit_level
          page_id
          ~page_info
          confirmed_slots_history
          history_cache
        |> Lwt.return
  in
  let input_given =
    Option.bind input_given (cut_at_level ~origination_level ~commit_level)
  in
  let* pvm_step_proof = P.produce_proof P.context input_given P.state in
  let module P_with_proof = struct
    include P

    let proof = pvm_step_proof
  end in
  match Sc_rollups.wrap_proof (module P_with_proof) with
  | Some pvm_step -> return {pvm_step; input_proof}
  | None -> proof_error "Could not wrap proof"
