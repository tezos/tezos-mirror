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

type inbox_proof = {
  level : Raw_level_repr.t;
  message_counter : Z.t;
  proof : Sc_rollup_inbox_repr.serialized_proof;
}

let inbox_proof_encoding =
  let open Data_encoding in
  conv
    (fun {level; message_counter; proof} -> (level, message_counter, proof))
    (fun (level, message_counter, proof) -> {level; message_counter; proof})
    (obj3
       (req "level" Raw_level_repr.encoding)
       (req "message_counter" Data_encoding.n)
       (req "proof" Sc_rollup_inbox_repr.serialized_proof_encoding))

type t = {pvm_step : Sc_rollups.wrapped_proof; inbox : inbox_proof option}

let encoding =
  let open Data_encoding in
  conv
    (fun {pvm_step; inbox} -> (pvm_step, inbox))
    (fun (pvm_step, inbox) -> {pvm_step; inbox})
    (obj2
       (req "pvm_step" Sc_rollups.wrapped_proof_encoding)
       (opt "inbox" inbox_proof_encoding))

let pp ppf _ = Format.fprintf ppf "Refutation game proof"

let start proof =
  let (module P) = Sc_rollups.wrapped_proof_module proof.pvm_step in
  P.proof_start_state P.proof

let stop input proof =
  let (module P) = Sc_rollups.wrapped_proof_module proof.pvm_step in
  P.proof_stop_state input P.proof

(* This takes an [input] and checks if it is at or above the given level.
   It returns [None] if this is the case.

   We use this to check that the PVM proof is obeying [commit_level]
   correctly---if the message obtained from the inbox proof is at or
   above [commit_level] the [input_given] in the PVM proof should be
   [None]. *)
let cut_at_level level input =
  let input_level = Sc_rollup_PVM_sig.(input.inbox_level) in
  if Raw_level_repr.(level <= input_level) then None else Some input

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

let pp_proof fmt {proof; _} =
  match Sc_rollup_inbox_repr.of_serialized_proof proof with
  | None -> Format.pp_print_string fmt "<invalid-proof-serialization>"
  | Some proof -> Sc_rollup_inbox_repr.pp_proof fmt proof

let valid snapshot commit_level ~pvm_name proof =
  let open Lwt_tzresult_syntax in
  let (module P) = Sc_rollups.wrapped_proof_module proof.pvm_step in
  let* () = check (String.equal P.name pvm_name) "Incorrect PVM kind" in
  let (input_requested : Sc_rollup_PVM_sig.input_request) =
    P.proof_input_requested P.proof
  in
  let* input =
    match (input_requested, proof.inbox) with
    | No_input_required, None -> return None
    | Initial, Some {proof = inbox_proof; _} ->
        check_inbox_proof snapshot inbox_proof (Raw_level_repr.root, Z.zero)
    | First_after (level, counter), Some {proof = inbox_proof; _} ->
        check_inbox_proof snapshot inbox_proof (level, Z.succ counter)
    | No_input_required, Some _ | Initial, None | First_after _, None ->
        proof_error
          (Format.asprintf
             "input_requested is %a, inbox proof is %a"
             Sc_rollup_PVM_sig.pp_input_request
             input_requested
             (Format.pp_print_option pp_proof)
             proof.inbox)
  in
  let input = Option.bind input (cut_at_level commit_level) in
  let*! res = P.verify_proof input P.proof in
  return (Result.is_ok res, input)

module type PVM_with_context_and_state = sig
  include Sc_rollups.PVM.S

  val context : context

  val state : state

  val proof_encoding : proof Data_encoding.t

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
  let* proof, input =
    match request with
    | No_input_required -> return (None, None)
    | Initial ->
        let level = Raw_level_repr.root in
        let message_counter = Z.zero in
        let* proof, input =
          Inbox_with_history.(
            produce_proof context history inbox (level, message_counter))
        in
        let proof = Inbox_with_history.to_serialized_proof proof in
        return (Some {level; message_counter; proof}, input)
    | First_after (level, message_counter) ->
        let* proof, input =
          Inbox_with_history.(
            produce_proof context history inbox (level, Z.succ message_counter))
        in
        let proof = Inbox_with_history.to_serialized_proof proof in
        return (Some {level; message_counter; proof}, input)
  in
  let input_given = Option.bind input (cut_at_level commit_level) in
  let* pvm_step_proof = P.produce_proof P.context input_given P.state in
  let module P_with_proof = struct
    include P

    let proof = pvm_step_proof
  end in
  match Sc_rollups.wrap_proof (module P_with_proof) with
  | Some pvm_step -> return {pvm_step; inbox = proof}
  | None -> proof_error "Could not wrap proof"
