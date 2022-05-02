(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

val launch : #Tezos_client_base.Client_context.wallet -> string -> unit Lwt.t

val stop : unit -> unit

(* [add_received ?unaccurate level ops] adds informations about the
   list of received consensus operations [ops], all at level
   [level]. [unaccurate] is true iff the [level] is the same as the
   current head's level. [endorsements] is an association list of
   tuples [(delegate, ops)], where [ops] is a list of tuples
   [(op_kind, round_opt, errors_opt, reception_time)], all signed by
   [delegate]. *)
val add_received :
  ?unaccurate:bool ->
  Int32.t ->
  (Signature.Public_key_hash.t
  * (Operation_kind.t * Int32.t option * error list option * Time.System.t) list)
  list ->
  unit

(* [add_block level hash round ts reception_time baker pkhs] adds
   information about a newly received block: its level, hash, round,
   its timestamp, its reception time, its baker, and its endorsers
   (the ones whose endorsements are actually included). *)
val add_block :
  level:Int32.t ->
  Block_hash.t ->
  round:Int32.t ->
  Time.Protocol.t ->
  Time.System.t ->
  Signature.Public_key_hash.t ->
  ?endorsements_round:Int32.t ->
  Signature.Public_key_hash.t list ->
  unit
