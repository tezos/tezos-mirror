(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 Trili Tech, <contact@trili.tech>                  *)
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

(** A [ready_ctx] value contains globally needed informations for a running dac
    node. It is available when the DAC plugin has been loaded. *)
type ready_ctxt = {
  dac_plugin : (module Dac_plugin.T);
  data_streamer : (module Data_streamer.S);
}

(** The status of the dac node. *)
type status = Ready of ready_ctxt | Starting

(** A [t] value contains both the status and the dac node configuration. Its
    fields are available through accessors. *)
type t

(** [init config cctx] creates a [t] with a status set to [Starting]
    using the given dac node configuration [config],
    and tezos node client context [cctx]. *)
val init : Configuration.t -> Client_context.full -> t

(** Raised by [set_ready] when the status is already [Ready _] *)
exception Status_already_ready

(** [set_ready ctxt ~dac_plugin] updates
    in place the status value to [Ready], and initializes the inner
    [ready_ctxt] value with the given parameters.

    @raise Status_already_ready when the status is already [Ready _] *)
val set_ready : t -> dac_plugin:(module Tezos_dac_node_lib.Dac_plugin.T) -> unit

type error += Node_not_ready

(** [get_ready ctxt] extracts the [ready_ctxt] value from a context [t]. It
    propagates [Node_not_ready] if status is not ready yet. If called multiple
    times, it replaces current values for [ready_ctxt] with new one. *)
val get_ready : t -> ready_ctxt tzresult

(** [get_config ctxt] returns the dac node configuration. *)
val get_config : t -> Configuration.t

(** [get_status ctxt] returns the dac node status. *)
val get_status : t -> status

(** [get_tezos_node_cctxt ctxt] returns the Tezos node's client context. *)
val get_tezos_node_cctxt : t -> Client_context.full
