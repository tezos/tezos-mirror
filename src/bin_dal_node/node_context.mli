(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** A [ready_ctx] value contains globally needed informations for a running dal
    node. It is available when both cryptobox is initialized and dal plugin is
    loaded. *)
type ready_ctxt = {
  dal_constants : Cryptobox.t;
  dal_parameters : Cryptobox.parameters;
  plugin : (module Dal_plugin.T);
  slot_header_store : Slot_headers_store.t;
}

(** The status of the dal node *)
type status = Ready of ready_ctxt | Starting

(** A [t] value contains both the status and the dal node configuration. It's
    field are available through accessors *)
type t

(** [init config] creates a [t] with a status set to [Starting] with a given dal
    node configuration.*)
val init : Configuration.t -> t

(** Raised by [set_ready] when the status is already [Ready _] *)
exception Status_already_ready

(** [set_ready ctxt slot_header_store plugin dal_constants dal_params] updates
    in place the status value to ready, and initializes the inner [ready_ctxt]
    value with the given parameters.

    @raise Status_already_ready when the status is already [Ready _] *)
val set_ready :
  t ->
  Slot_headers_store.t ->
  (module Dal_plugin.T) ->
  Cryptobox.t ->
  Cryptobox.parameters ->
  unit

type error += Node_not_ready

(** [get_ready ctxt] extracts the [ready_ctxt] value from a context [t]. It
    propagates [Node_not_ready] if status is not ready yet. If called multiple
    times, it replaces current values for [ready_ctxt] with new ones *)
val get_ready : t -> ready_ctxt tzresult

(** [get_config ctxt] returns the dal node configuration *)
val get_config : t -> Configuration.t

(** [get_status ctxt] returns the dal node status *)
val get_status : t -> status
