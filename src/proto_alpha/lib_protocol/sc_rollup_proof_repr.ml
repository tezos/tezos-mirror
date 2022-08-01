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
  inbox : Sc_rollup_inbox_repr.serialized_proof option;
}

let encoding =
  let open Data_encoding in
  conv
    (fun {pvm_step; inbox} -> (pvm_step, inbox))
    (fun (pvm_step, inbox) -> {pvm_step; inbox})
    (obj2
       (req "pvm_step" Sc_rollups.wrapped_proof_encoding)
       (opt "inbox" Sc_rollup_inbox_repr.serialized_proof_encoding))

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

let () =
  register_error_kind
    `Permanent
    ~id:"Sc_rollup_proof_check"
    ~title:"Invalid proof"
    ~description:"An invalid proof has been submitted"
    ~pp:(fun fmt msg -> Format.fprintf fmt "Invalid proof: %s" msg)
    Data_encoding.(obj1 @@ req "reason" string)
    (function Sc_rollup_proof_check msg -> Some msg | _ -> None)
    (fun msg -> Sc_rollup_proof_check msg)

let proof_error reason =
  let open Lwt_tzresult_syntax in
  fail (Sc_rollup_proof_check reason)

let check p reason =
  let open Lwt_tzresult_syntax in
  if p then return () else proof_error reason

let check_inbox_proof snapshot serialized_inbox_proof (level, counter) =
  match Sc_rollup_inbox_repr.of_serialized_proof serialized_inbox_proof with
  | None -> return None
  | Some inbox_proof ->
      Sc_rollup_inbox_repr.verify_proof (level, counter) snapshot inbox_proof

let pp_proof fmt serialized_inbox_proof =
  match Sc_rollup_inbox_repr.of_serialized_proof serialized_inbox_proof with
  | None -> Format.pp_print_string fmt "<invalid-proof-serialization>"
  | Some proof -> Sc_rollup_inbox_repr.pp_proof fmt proof

let valid snapshot commit_level ~pvm_name proof =
  let open Lwt_tzresult_syntax in
  let (module P) = Sc_rollups.wrapped_proof_module proof.pvm_step in
  let* () = check (String.equal P.name pvm_name) "Incorrect PVM kind" in
  let input_requested = P.proof_input_requested P.proof in
  let input_given = P.proof_input_given P.proof in
  let* input =
    match (input_requested, proof.inbox) with
    | Sc_rollup_PVM_sem.No_input_required, None -> return None
    | Sc_rollup_PVM_sem.Initial, Some inbox_proof ->
        check_inbox_proof snapshot inbox_proof (Raw_level_repr.root, Z.zero)
    | Sc_rollup_PVM_sem.First_after (level, counter), Some inbox_proof ->
        check_inbox_proof snapshot inbox_proof (level, Z.succ counter)
    | _ ->
        proof_error
          (Format.asprintf
             "input_requested is %a, inbox proof is %a"
             Sc_rollup_PVM_sem.pp_input_request
             input_requested
             (Format.pp_print_option pp_proof)
             proof.inbox)
  in
  let* () =
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

  val proof_encoding : proof Data_encoding.t

  module Inbox_with_history : sig
    include
      Sc_rollup_inbox_repr.Merkelized_operations
        with type inbox_context = context

    val inbox : Sc_rollup_inbox_repr.history_proof

    val history : Sc_rollup_inbox_repr.history
  end
end

let of_lwt_result result =
  let open Lwt_tzresult_syntax in
  let*! r = result in
  match r with Ok x -> return x | Error e -> fail e

let produce pvm_and_state commit_level =
  let open Lwt_tzresult_syntax in
  let (module P : PVM_with_context_and_state) = pvm_and_state in
  let open P in
  let*! request = P.is_input_state P.state in
  let* inbox, input_given =
    match request with
    | Sc_rollup_PVM_sem.No_input_required -> return (None, None)
    | Sc_rollup_PVM_sem.Initial ->
        let* p, i =
          Inbox_with_history.(
            produce_proof context history inbox (Raw_level_repr.root, Z.zero))
        in
        return (Some (Inbox_with_history.to_serialized_proof p), i)
    | Sc_rollup_PVM_sem.First_after (l, n) ->
        let* p, i =
          Inbox_with_history.(produce_proof context history inbox (l, Z.succ n))
        in
        return (Some (Inbox_with_history.to_serialized_proof p), i)
  in
  let input_given = Option.bind input_given (cut_at_level commit_level) in
  let* pvm_step_proof =
    of_lwt_result (P.produce_proof P.context input_given P.state)
  in
  let module P_with_proof = struct
    include P

    let proof = pvm_step_proof
  end in
  match Sc_rollups.wrap_proof (module P_with_proof) with
  | Some pvm_step -> return {pvm_step; inbox}
  | None -> proof_error "Could not wrap proof"
