(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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
  | Tx_rollup_already_exists of Tx_rollup_repr.t
  | Tx_rollup_does_not_exist of Tx_rollup_repr.t

let init : Raw_context.t -> Tx_rollup_repr.t -> Raw_context.t tzresult Lwt.t =
 fun ctxt tx_rollup ->
  Storage.Tx_rollup.State.mem ctxt tx_rollup >>=? fun (ctxt, already_exists) ->
  fail_when already_exists (Tx_rollup_already_exists tx_rollup) >>=? fun () ->
  Storage.Tx_rollup.State.init ctxt tx_rollup Tx_rollup_state_repr.initial_state
  >|=? fst

let find :
    Raw_context.t ->
    Tx_rollup_repr.t ->
    (Raw_context.t * Tx_rollup_state_repr.t option) tzresult Lwt.t =
  Storage.Tx_rollup.State.find

let get :
    Raw_context.t ->
    Tx_rollup_repr.t ->
    (Raw_context.t * Tx_rollup_state_repr.t) tzresult Lwt.t =
 fun ctxt tx_rollup ->
  find ctxt tx_rollup >>=? fun (ctxt, state) ->
  match state with
  | Some state -> return (ctxt, state)
  | None -> fail (Tx_rollup_does_not_exist tx_rollup)

let assert_exist :
    Raw_context.t -> Tx_rollup_repr.t -> Raw_context.t tzresult Lwt.t =
 fun ctxt tx_rollup ->
  Storage.Tx_rollup.State.mem ctxt tx_rollup
  >>=? fun (ctxt, tx_rollup_exists) ->
  fail_unless tx_rollup_exists (Tx_rollup_does_not_exist tx_rollup)
  >>=? fun () -> return ctxt

let update :
    Raw_context.t ->
    Tx_rollup_repr.t ->
    Tx_rollup_state_repr.t ->
    Raw_context.t tzresult Lwt.t =
 fun ctxt tx_rollup t ->
  Storage.Tx_rollup.State.update ctxt tx_rollup t >>=? fun (ctxt, _) ->
  return ctxt

(* ------ Error registration ------------------------------------------------ *)

let () =
  let open Data_encoding in
  (* Tx_rollup_already_exists *)
  register_error_kind
    `Permanent
    ~id:"tx_rollup_already_exists"
    ~title:"Transaction rollup was already created"
    ~description:
      "The protocol tried to originate the same transaction rollup twice"
    ~pp:(fun ppf addr ->
      Format.fprintf
        ppf
        "Transaction rollup %a is already used for an existing transaction \
         rollup. This should not happen, and indicates there is a bug in the \
         protocol. If you can, please report this bug \
         (https://gitlab.com/tezos/tezos/-/issues.)"
        Tx_rollup_repr.pp
        addr)
    (obj1 (req "rollup_address" Tx_rollup_repr.encoding))
    (function Tx_rollup_already_exists rollup -> Some rollup | _ -> None)
    (fun rollup -> Tx_rollup_already_exists rollup) ;
  (* Tx_rollup_does_not_exist *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_does_not_exist"
    ~title:"Transaction rollup does not exist"
    ~description:"An invalid transaction rollup address was submitted"
    ~pp:(fun ppf addr ->
      Format.fprintf
        ppf
        "Invalid transaction rollup address %a"
        Tx_rollup_repr.pp
        addr)
    (obj1 (req "rollup_address" Tx_rollup_repr.encoding))
    (function Tx_rollup_does_not_exist rollup -> Some rollup | _ -> None)
    (fun rollup -> Tx_rollup_does_not_exist rollup)

let first_unfinalized_level :
    Raw_context.t ->
    Tx_rollup_repr.t ->
    (Raw_context.t * Raw_level_repr.t option) tzresult Lwt.t =
 fun ctxt tx_rollup ->
  Storage.Tx_rollup.State.get ctxt tx_rollup >>=? fun (ctxt, state) ->
  return (ctxt, Tx_rollup_state_repr.first_unfinalized_level state)
