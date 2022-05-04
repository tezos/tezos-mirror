(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** transactional Rollup client state *)
type t

(** [create ?name ?path ?base_dir ?path node] returns a fresh client
   identified by a specified [name], logging in [color], executing the
   program at [path], storing local information in [base_dir], and
   communicating with the specified [node]. *)
val create :
  ?name:string ->
  ?path:string ->
  ?base_dir:string ->
  ?color:Log.Color.t ->
  Tx_rollup_node.t ->
  t

val get_balance :
  ?block:string -> t -> tz4_address:string -> ticket_id:string -> int Lwt.t

val get_inbox : ?block:string -> t -> string Lwt.t

val get_block : t -> block:string -> string Lwt.t

val craft_tx_transaction :
  t ->
  signer:string ->
  ?counter:int64 ->
  Rollup.Tx_rollup.transfer_content ->
  string Lwt.t

val craft_tx_transfers : t -> Rollup.Tx_rollup.transfer -> string Lwt.t

val craft_tx_withdraw :
  ?counter:Int64.t ->
  t ->
  qty:Int64.t ->
  signer:string ->
  dest:string ->
  ticket:string ->
  string Lwt.t

val craft_tx_batch : t -> batch:string -> signatures:string -> string Lwt.t

val get_batcher_queue : t -> string Lwt.t

val get_batcher_transaction : t -> transaction_hash:string -> string Lwt.t

val inject_batcher_transaction :
  t -> ?expect_failure:bool -> string -> (string * string) Lwt.t
