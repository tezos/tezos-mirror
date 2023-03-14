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

open Tx_rollup_errors_repr

let init : Raw_context.t -> Tx_rollup_repr.t -> Raw_context.t tzresult Lwt.t =
 fun ctxt tx_rollup ->
  Storage.Tx_rollup.State.mem ctxt tx_rollup >>=? fun (ctxt, already_exists) ->
  fail_when already_exists (Tx_rollup_already_exists tx_rollup) >>=? fun () ->
  let pre_allocated_storage =
    Z.of_int @@ Constants_storage.tx_rollup_origination_size ctxt
  in
  Storage.Tx_rollup.State.init ctxt tx_rollup
  @@ Tx_rollup_state_repr.initial_state ~pre_allocated_storage
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
  | None -> tzfail (Tx_rollup_does_not_exist tx_rollup)

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
