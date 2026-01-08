(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** Functions for RPC service registration, using [Updater.rpc_context] and
    [RPC_service.t] from the Protocol Environment.

    This module is a frontend to a mutable service directory. The various
    [register] functions update the directory as a side-effect.

    The [get_rpc_services] function returns the resulting [RPC_context]. It is
    parameterized by [Updater.rpc_context] which acts as the service prefix (in
    practice meaning this type will be passed to each handler). Hence,
    Protocol RPC services provide a {i read-only} view of the Ledger state.
  *)

open Protocol
open Environment
open Alpha_context

type rpc_context = {
  block_hash : Block_hash.t;
  block_header : Block_header.shell_header;
  context : t;
}

(** [rpc_init rpc_context mode] allows to instantiate an [rpc_context]
   using the [Alpha_context] representation from a raw context
   representation (the one the shell knows).

    If [mode = `Head_level], the [Alpha_context] uses the same level
   as the head of the chain (given by [rpc_context.block_header]).

    If [mode= `Successor_level], the [Alpha_context] uses the
   successor level of the head.

    This function aims to be used by RPCs, in particular by RPCs which
   simulate an operation to determine the fees/gas of an
   operation. Using the [`Head_level] can be dangerous if some storage
   paths depend on the level. Using the successor level allows to
   ensure that the simulation is done on a fresh level. *)
val rpc_init :
  Updater.rpc_context ->
  [`Head_level | `Successor_level] ->
  rpc_context Error_monad.tzresult Lwt.t

val register0 :
  chunked:bool ->
  ( [< RPC_service.meth],
    Updater.rpc_context,
    Updater.rpc_context,
    'a,
    'b,
    'c )
  RPC_service.t ->
  (t -> 'a -> 'b -> 'c Error_monad.tzresult Lwt.t) ->
  unit

val register0_noctxt :
  chunked:bool ->
  ([< RPC_service.meth], Updater.rpc_context, 'a, 'b, 'c, 'd) RPC_service.t ->
  ('b -> 'c -> 'd Error_monad.tzresult Lwt.t) ->
  unit

val register1 :
  chunked:bool ->
  ( [< RPC_service.meth],
    Updater.rpc_context,
    Updater.rpc_context * 'a,
    'b,
    'c,
    'd )
  RPC_service.t ->
  (t -> 'a -> 'b -> 'c -> 'd Error_monad.tzresult Lwt.t) ->
  unit

val register2 :
  chunked:bool ->
  ( [< RPC_service.meth],
    Updater.rpc_context,
    (Updater.rpc_context * 'a) * 'b,
    'c,
    'd,
    'e )
  RPC_service.t ->
  (t -> 'a -> 'b -> 'c -> 'd -> 'e Error_monad.tzresult Lwt.t) ->
  unit

val opt_register0 :
  chunked:bool ->
  ( [< RPC_service.meth],
    Updater.rpc_context,
    Updater.rpc_context,
    'a,
    'b,
    'c )
  RPC_service.t ->
  (t -> 'a -> 'b -> 'c option Error_monad.tzresult Lwt.t) ->
  unit

val opt_register1 :
  chunked:bool ->
  ( [< RPC_service.meth],
    Updater.rpc_context,
    Updater.rpc_context * 'a,
    'b,
    'c,
    'd )
  RPC_service.t ->
  (t -> 'a -> 'b -> 'c -> 'd option Error_monad.tzresult Lwt.t) ->
  unit

val opt_register2 :
  chunked:bool ->
  ( [< RPC_service.meth],
    Updater.rpc_context,
    (Updater.rpc_context * 'a) * 'b,
    'c,
    'd,
    'e )
  RPC_service.t ->
  (t -> 'a -> 'b -> 'c -> 'd -> 'e option Error_monad.tzresult Lwt.t) ->
  unit

val get_rpc_services : unit -> Updater.rpc_context RPC_directory.directory
