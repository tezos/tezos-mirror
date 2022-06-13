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

type t = {
  pvm_step : Sc_rollups.wrapped_proof;
  inbox : Sc_rollup_inbox_repr.Proof.t option;
}

let encoding =
  let open Data_encoding in
  conv
    (fun {pvm_step; inbox} -> (pvm_step, inbox))
    (fun (pvm_step, inbox) -> {pvm_step; inbox})
    (obj2
       (req "pvm_step" Sc_rollups.wrapped_proof_encoding)
       (req "inbox" (option Sc_rollup_inbox_repr.Proof.encoding)))

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
let cut_at_level level input =
  let input_level = Sc_rollup_PVM_sem.(input.inbox_level) in
  if Raw_level_repr.(level <= input_level) then None else Some input

type error += Sc_rollup_proof_check of string

let proof_error reason =
  let open Lwt_result_syntax in
  fail (Sc_rollup_proof_check reason)

let check p reason =
  let open Lwt_result_syntax in
  if p then return () else proof_error reason

let valid snapshot commit_level ~pvm_name proof =
  let (module P) = Sc_rollups.wrapped_proof_module proof.pvm_step in
  let open Lwt_result_syntax in
  let* _ = check (String.equal P.name pvm_name) "Incorrect PVM kind" in
  let input_requested = P.proof_input_requested P.proof in
  let input_given = P.proof_input_given P.proof in
  let* input =
    match (input_requested, proof.inbox) with
    | Sc_rollup_PVM_sem.No_input_required, None -> return None
    | Sc_rollup_PVM_sem.Initial, Some inbox_proof ->
        Sc_rollup_inbox_repr.Proof.valid
          {inbox_level = Raw_level_repr.root; message_counter = Z.zero}
          snapshot
          inbox_proof
    | Sc_rollup_PVM_sem.First_after (level, counter), Some inbox_proof ->
        Sc_rollup_inbox_repr.Proof.valid
          {inbox_level = level; message_counter = Z.succ counter}
          snapshot
          inbox_proof
    | _ ->
        proof_error
          (Format.asprintf
             "input_requested is %a, inbox proof is %a"
             Sc_rollup_PVM_sem.pp_input_request
             input_requested
             (Format.pp_print_option Sc_rollup_inbox_repr.Proof.pp)
             proof.inbox)
  in
  let* _ =
    check
      (Option.equal
         Sc_rollup_PVM_sem.input_equal
         (Option.bind input (cut_at_level commit_level))
         input_given)
      "Input given is not what inbox proof expects"
  in
  Lwt.map Result.ok (P.verify_proof P.proof)

module type PVM_with_context_and_state = sig
  include Sc_rollups.PVM.S

  val context : context

  val state : state
end

type error += Proof_cannot_be_wrapped

let produce pvm_and_state inbox commit_level =
  let open Lwt_result_syntax in
  let (module P : PVM_with_context_and_state) = pvm_and_state in
  let*! request = P.is_input_state P.state in
  let* inbox, input_given =
    match request with
    | Sc_rollup_PVM_sem.No_input_required -> return (None, None)
    | Sc_rollup_PVM_sem.Initial ->
        let* p, i =
          Sc_rollup_inbox_repr.Proof.produce_proof
            inbox
            (Raw_level_repr.root, Z.zero)
        in
        return (Some p, i)
    | Sc_rollup_PVM_sem.First_after (l, n) ->
        let* p, i = Sc_rollup_inbox_repr.Proof.produce_proof inbox (l, n) in
        return (Some p, i)
  in
  let input_given = Option.bind input_given (cut_at_level commit_level) in
  let* pvm_step_proof = P.produce_proof P.context input_given P.state in
  let module P_with_proof = struct
    include P

    let proof = pvm_step_proof
  end in
  match Sc_rollups.wrap_proof (module P_with_proof) with
  | Some pvm_step -> return {pvm_step; inbox}
  | None -> fail Proof_cannot_be_wrapped
