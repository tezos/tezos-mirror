(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
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
open Protocol.Alpha_context

(** The RPC server and the Daemon main loop are sharing a variable of the
    type stored in the Irmin store. The [State] module allows access to this stored
    data. *)

type t

(** [init ~data_dir ~context ~rollup ~block_origination_hash]
    checks that the rollup [rollup_id] is created inside the block
    identified by the hash [block_origination_hash], and creates an
    initial state for the rollup node if that is the case. *)
val init :
  data_dir:string ->
  context:#Protocol_client_context.full ->
  rollup:Tx_rollup.t ->
  rollup_genesis:Block_hash.t ->
  t tzresult Lwt.t

(** [set_new_head state hash] saves the Tezos head that has just been processed in a
    reference cell. *)
val set_new_head : t -> Block_hash.t -> unit tzresult Lwt.t

(** [get_head state] returns the head that has just been processed from the reference cell. *)
val get_head : t -> Block_hash.t option Lwt.t

(** [block_already_seen state block] returns [true] iff [block] has already been processed. *)
val block_already_seen : t -> Block_hash.t -> bool Lwt.t

(** [save_inbox state hash inbox] saves the inbox relative to the block referenced by
    the hash given as
    an argument. *)
val save_inbox : t -> Block_hash.t -> Inbox.t -> unit tzresult Lwt.t

(** [find_inbox state hash] Find the inbox stored at [hash]. *)
val find_inbox : t -> Block_hash.t -> Inbox.t option Lwt.t

(** [rollup_operation_index] returns the index where are rollup operation (currently
    as manager operation) stored into a [Block_info.t]. *)
val rollup_operation_index : int
