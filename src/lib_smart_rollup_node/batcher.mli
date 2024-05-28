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

(** The type for the status of messages in the batcher.  *)
type status =
  | Pending_batch  (** The message is in the queue of the batcher. *)
  | Batched of Injector.Inj_operation.id
      (** The message has already been batched and sent to the injector in an L1
          operation whose id is given. *)

(** [init plugin node_ctxt] initializes and starts the batcher for
    [signer].  [plugin] is the protocol plugin with which the batcher
    is started, but it will automatically change plugins on protocol
    migrations. The batcher worker is launched only if the current
    rollup node mode supports batching L2 operations. *)
val init :
  (module Protocol_plugin_sig.S) -> _ Node_context.t -> unit tzresult Lwt.t

(** Create L2 batches of operations from the queue and pack each batch
    in an L1 operation. The L1 operations (i.e. L2 batches) are queued
    in the injector for injection on the Tezos node. *)
val produce_batches : unit -> unit tzresult Lwt.t

(** Shutdown the batcher, waiting for the ongoing request to be processed. *)
val shutdown : unit -> unit Lwt.t

(** Return [true] if the batcher was started for this node. *)
val active : unit -> bool

(** Retrieve an L2 message from the queue. *)
val find_message : L2_message.id -> L2_message.t option tzresult

(** List all queued messages in the order they appear in the queue, i.e. the
    message that were added first to the queue are at the end of list. *)
val get_queue : unit -> (L2_message.id * L2_message.t) list tzresult

(** [register_messages messages] registers new L2 [messages] in the
    queue of the batcher for future injection on L1. In this case,
    when the application fails, the messages are not queued.  *)
val register_messages : string list -> L2_message.id list tzresult Lwt.t

(** The status of a message in the batcher. Returns [None] if the message is not
    known by the batcher (the batcher only keeps the batched status of the last
    500000 messages). *)
val message_status : L2_message.id -> (status * string) option tzresult
