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

type reveal_proof = Raw_data_proof of string

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
      (function Raw_data_proof s -> Some ((), Bytes.of_string s))
      (fun ((), s) -> Raw_data_proof (Bytes.to_string s))
  in
  union [case_raw_data]

type input_proof =
  | Inbox_proof of {
      level : Raw_level_repr.t;
      message_counter : Z.t;
      proof : Sc_rollup_inbox_repr.serialized_proof;
    }
  | Reveal_proof of reveal_proof

let input_proof_encoding =
  let open Data_encoding in
  let case_inbox_proof =
    case
      ~title:"inbox proof"
      (Tag 0)
      (obj4
         (req "input_proof_kind" (constant "inbox_proof"))
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
         (req "input_proof_kind" (constant "reveal_proof"))
         (req "reveal_proof" reveal_proof_encoding))
      (function Reveal_proof s -> Some ((), s) | _ -> None)
      (fun ((), s) -> Reveal_proof s)
  in
  union [case_inbox_proof; case_reveal_proof]

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

(* This takes an [input] and checks if it is at or above the given level.
   It returns [None] if this is the case.

   We use this to check that the PVM proof is obeying [commit_level]
   correctly---if the message obtained from the inbox proof is at or
   above [commit_level] the [input_given] in the PVM proof should be
   [None]. *)
let cut_at_level level (input : Sc_rollup_PVM_sig.input) =
  match input with
  | Inbox_message {inbox_level = input_level; _} ->
      if Raw_level_repr.(level <= input_level) then None else Some input
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

let valid snapshot commit_level ~pvm_name proof =
  let open Lwt_tzresult_syntax in
  let (module P) = Sc_rollups.wrapped_proof_module proof.pvm_step in
  let* () = check (String.equal P.name pvm_name) "Incorrect PVM kind" in
  let* input =
    match proof.input_proof with
    | None -> return_none
    | Some (Inbox_proof {level; message_counter; proof}) ->
        let+ inbox_message =
          check_inbox_proof snapshot proof (level, Z.succ message_counter)
        in
        Option.map (fun i -> Sc_rollup_PVM_sig.Inbox_message i) inbox_message
    | Some (Reveal_proof (Raw_data_proof data)) ->
        return_some (Sc_rollup_PVM_sig.Reveal (Raw_data data))
  in
  let input = Option.bind input (cut_at_level commit_level) in
  let* input_requested = P.verify_proof input P.proof in
  let* () =
    match (proof.input_proof, input_requested) with
    | None, No_input_required -> return_unit
    | Some (Inbox_proof {level; message_counter; proof = _}), Initial ->
        check
          (Raw_level_repr.(level = root) && Z.(equal message_counter zero))
          "Inbox proof is not about the initial input request."
    | Some (Inbox_proof {level; message_counter; proof = _}), First_after (l, n)
      ->
        check
          (Raw_level_repr.(level = l) && Z.(equal message_counter n))
          "Level and index of inbox proof are not equal to the one expected in \
           input request."
    | ( Some (Reveal_proof (Raw_data_proof data)),
        Needs_reveal (Reveal_raw_data expected_hash) ) ->
        let data_hash = Sc_rollup_PVM_sig.Input_hash.hash_string [data] in
        check
          (Sc_rollup_PVM_sig.Input_hash.equal data_hash expected_hash)
          "Invalid reveal"
    | None, (Initial | First_after _ | Needs_reveal _)
    | Some _, No_input_required
    | Some (Inbox_proof _), Needs_reveal _
    | Some (Reveal_proof _), (Initial | First_after _) ->
        proof_error "Inbox proof and input request are dissociated."
  in
  return (input, input_requested)

module type PVM_with_context_and_state = sig
  include Sc_rollups.PVM.S

  val context : context

  val state : state

  val proof_encoding : proof Data_encoding.t

  val reveal : Sc_rollup_PVM_sig.Input_hash.t -> string option

  module Inbox_with_history : sig
    include
      Sc_rollup_inbox_repr.Merkelized_operations
        with type inbox_context = context

    val inbox : Sc_rollup_inbox_repr.history_proof

    val history : Sc_rollup_inbox_repr.History.t
  end
end

let produce pvm_and_state commit_level =
  let open Lwt_tzresult_syntax in
  let (module P : PVM_with_context_and_state) = pvm_and_state in
  let open P in
  let*! (request : Sc_rollup_PVM_sig.input_request) =
    P.is_input_state P.state
  in
  let* input_proof, input_given =
    match request with
    | No_input_required -> return (None, None)
    | Initial ->
        let level = Raw_level_repr.root in
        let message_counter = Z.zero in
        let* inbox_proof, input =
          Inbox_with_history.(
            produce_proof context history inbox (level, message_counter))
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
  in
  let input_given = Option.bind input_given (cut_at_level commit_level) in
  let* pvm_step_proof = P.produce_proof P.context input_given P.state in
  let module P_with_proof = struct
    include P

    let proof = pvm_step_proof
  end in
  match Sc_rollups.wrap_proof (module P_with_proof) with
  | Some pvm_step -> return {pvm_step; input_proof}
  | None -> proof_error "Could not wrap proof"
