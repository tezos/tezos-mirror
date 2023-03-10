(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech, <contact@trili.tech>                       *)
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

(** A value of type [t] represent a handler that specifies a procedure that is
    invoked by the DAC daemon. In most cases, it never resolves unless the DAC
    node terminates, in which case the underlying
    [Tezos_rpc__RPC_context.stopper] is called. *)
type t = unit tzresult Lwt.t * Tezos_rpc__RPC_context.stopper

(** Monitor heads and try resolve the DAC protocol plugin corresponding to
     the protocol of the targeted node. *)
val resolve_plugin_and_set_ready : Node_context.t -> t tzresult Lwt.t

(** [handlers ctxt] returns the handlers to be executed in the main loop of the
    daemon. The set of handlers is different according to the operating mode
    indicated in [ctxt.mode]. All operating modes have one handler for
    tracking new heads from the Layer 1. Additionally
    {ul
      {li If [ctx.mode] is [Coordinator _], then no other handlers are
          present, }
      {li If [ctxt.mode] is [Committee_member _], then the DAC node also has
          one handler for monitoring root hashes streamed by a [Coordinator].
          Upon detecting a new root hash, the associated pages are downloaded
          from the [Coordinator], after which the root hash is signed and the
          signature posted to the [Coordinator], }
      {li If [ctxt.mode] is [Observer _], then the DAC node also has
          one handler for monitoring root hashes streamed by a [Coordinator].
          Upon detecting a new root hash, the associated pages are downloaded
          from the [Coordinator]. Differently from the case of
          [Committee_member _], root hashes are not signed, }
      {li If [ctxt.mode] is [Legacy _], then a handler to monitor root hashes
          from a coordinator is present only if the RPC address of a
          [Coordindator] node is specified in the DAC node configuration.
          Additionally, if the configuration also contains a [committee_member]
          address field specified, then this handler behave as in the case for
          [Committee_member _]. Otherwise, it behaves as in the case for
          [Observer _].}
    } *)
val handlers : Node_context.t -> t tzresult Lwt.t list tzresult Lwt.t
