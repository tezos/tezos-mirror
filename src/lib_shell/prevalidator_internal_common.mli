(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** Common elements used by {!Prevalidator_internal} and also included
    in {!Prevalidator}. *)

(** Minimal delay between two mempool advertisements *)
val advertisement_delay : float

(** Argument that will be provided to {!Worker.MakeGroup} to create
    the prevalidator worker. *)
module Name :
  Tezos_base.Worker_intf.NAME with type t = Chain_id.t * Protocol_hash.t

open Prevalidator_worker_state

(** A prevalidator instance, tailored to a specific protocol (even if
    it is not visible in this module type.

    The creation of such prevalidator instances is implemented in
    {!Prevalidator_internal} and wrapped in {!Prevalidator.create}. *)
module type T = sig
  type types_state

  val get_rpc_directory :
    types_state -> types_state Tezos_rpc.Directory.t lazy_t

  val name : Name.t

  module Types : Worker_intf.TYPES with type state = types_state

  module Worker :
    Worker.T
      with type ('a, 'b) Request.t = ('a, 'b) Request.t
       and type Request.view = Request.view
       and type Types.state = types_state

  type worker = Worker.infinite Worker.queue Worker.t

  val worker : worker Lazy.t
end

(** Documented in {!Prevalidator}. *)
type t = (module T)
