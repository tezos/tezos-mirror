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

open Protocol_client_context
open Protocol
open Alpha_context
open Common

(** The type of an injector worker *)
type t

(** Initializes the injector with a client context, for a rollup and a list of
    signers. Each signer has its own pending queue of operations to inject. *)
val init :
  #Client_context.full ->
  rollup:Tx_rollup.t ->
  signers:(public_key_hash * Injector_worker_types.tag list) list ->
  t tzresult Lwt.t

(** Add an operation as pending injection in the injector. *)
val add_pending_operation : t -> L1_operation.t -> unit Lwt.t

(** Add multiple operations as pending injection in the injector. *)
val add_pending_operations : t -> L1_operation.t trace -> unit Lwt.t

(** Notify the injector of a new Tezos head. The injector marks the operations
    appropriately (for instance reverted operations that are part of a
    reorganization are put back in the pending queue). When an operation is
    considered as {e confirmed}, it disappears from the injector. *)
val new_tezos_head :
  t ->
  Alpha_block_services.block_info ->
  Alpha_block_services.block_info reorg ->
  unit Lwt.t

(** Trigger an injection of the pending operations. *)
val inject : ?tags:Injector_worker_types.tag list -> t -> unit Lwt.t
