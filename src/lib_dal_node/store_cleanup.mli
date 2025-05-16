(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** [clean_up_store_and_catch_up ctxt cctxt ~last_processed_level
    ~first_seen_level ~head_level proto_parameters] performs the store clean-up
    and catch-up operations needed when starting or restarting the DAL node.

    Depending on whether the node supports refutation games, it removes outdated
    slot/shard data from the DAL store to reduce disk usage and ensure only
    relevant data is kept. It may also populate the skip list store for levels
    required by refutation mechanisms.

    - [last_processed_level] is the most recent level processed by the L1
    crawler.

    - [first_seen_level] is the first L1 level the node has seen (used to
    determine profile-specific retention).

    - [head_level] is the current L1 head level used to compute the clean-up
    range.

    This function emits events to track progress and updates the last processed
    level in the store.
*)
val clean_up_store_and_catch_up :
  Node_context.t ->
  Rpc_context.t ->
  last_processed_level:int32 ->
  first_seen_level:int32 option ->
  head_level:int32 ->
  Types.proto_parameters ->
  unit tzresult Lwt.t
