(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** Functions to setup storage. Used by [Alpha_context.prepare].

    If you have defined a new type of storage, you should add relevant
    setups here.
  *)

(* This is the genesis protocol: initialise the state *)
val prepare_first_block :
  Chain_id.t ->
  Context.t ->
  typecheck_smart_contract:
    (Raw_context.t ->
    Script_repr.t ->
    ((Script_repr.t * Lazy_storage_diff.diffs option) * Raw_context.t)
    Error_monad.tzresult
    Lwt.t) ->
  typecheck_smart_rollup:
    (Raw_context.t -> Script_repr.expr -> Raw_context.t tzresult) ->
  level:int32 ->
  timestamp:Time.t ->
  predecessor:Block_hash.t ->
  (Raw_context.t, Error_monad.error Error_monad.trace) Pervasives.result Lwt.t

val prepare :
  Context.t ->
  level:Int32.t ->
  predecessor_timestamp:Time.t ->
  timestamp:Time.t ->
  (Raw_context.t
  * Receipt_repr.balance_updates
  * Migration_repr.origination_result list)
  Error_monad.tzresult
  Lwt.t
