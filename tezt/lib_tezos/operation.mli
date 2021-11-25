(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** This module allows to construct, forge, sign and inject (manager)
    operations. *)

(** [manager_operation_content] is an abstract type for
    manager operations. Values of this type can be constructed using the
    smart constructors below *)
type manager_operation_content

(** {2 Smart constructors} *)

(** [mk_transfer] allows to construct a manager operation representing a simple
    transfer between implicit accounts [source] and [dest].

    - Default [counter] is the successor of the counter of [source].
    - Default [amount] is [1] tez.
    - Default [fee] is [1000] mutez.
    - Default [gas_limit] is [1040] gas.
    - Default [storage_limit] is [257].
*)
val mk_transfer :
  source:Account.key ->
  ?counter:int ->
  ?fee:int ->
  ?gas_limit:int ->
  ?storage_limit:int ->
  dest:Account.key ->
  ?amount:int ->
  Client.t ->
  manager_operation_content Lwt.t

(** [mk_reveal] allows to construct a manager operation representing a
    public key revelation of an implicit account [source].

    - Default [counter] is the successor of the counter of [source].
    - Default [fee] is [1000] mutez.
    - Default [gas_limit] is [1040] gas.
    - Default [storage_limit] is [0]. *)
val mk_reveal :
  source:Account.key ->
  ?counter:int ->
  ?fee:int ->
  ?gas_limit:int ->
  ?storage_limit:int ->
  Client.t ->
  manager_operation_content Lwt.t

(** [mk_call] allows to construct a manager operation representing a call
    to a smart contract [entrypoint] with a given parameter [arg] from an
    implicit account [source].

    - Default [counter] is the successor of the counter of [source].
    - Default [amount] is [0] tez.
    - Default [fee] is [30_000] mutez.
    - Default [gas_limit] is [30_000] gas.
    - Default [storage_limit] is [1_500]. *)
val mk_call :
  source:Account.key ->
  ?counter:int ->
  ?fee:int ->
  ?gas_limit:int ->
  ?storage_limit:int ->
  dest:string ->
  ?amount:int ->
  entrypoint:string ->
  arg:Ezjsonm.value ->
  Client.t ->
  manager_operation_content Lwt.t

(** [mk_origination] allows to construct a manager operation representing a
    smart contract origination from an implicit account [source].

    - Default [counter] is the successor of the counter of [source].
    - Default [init_balance] is [0] tez.
    - Default [fee] is [1_000_000] mutez.
    - Default [gas_limit] is [100_000] gas.
    - Default [storage_limit] is [10_000]. *)
val mk_origination :
  source:Account.key ->
  ?counter:int ->
  ?fee:int ->
  ?gas_limit:int ->
  ?storage_limit:int ->
  code:Ezjsonm.value ->
  init_storage:Ezjsonm.value ->
  ?init_balance:int ->
  Client.t ->
  manager_operation_content Lwt.t

(** {2 Forging, signing and injecting operations} *)

(** [sign_manager_op_bytes ~signer op_bytes] signs [op_bytes] with
    [signer]'s secret key and the watermark [Generic_operation]. *)
val sign_manager_op_bytes :
  signer:Account.key -> bytes -> Tezos_crypto.Signature.t

(** Same as [sign_manager_op_bytes], but the input operation content
    and output signature are strings containing hexadecimal data. *)
val sign_manager_op_hex : signer:Account.key -> string -> string

(** [forge_and_inject_operation] allows to forge, sign and inject to a
    node, via the provided [client], the list [batch] of managed operations.
    The forged operation is signed by the given [signer] account.
    Default [branch] is the one returned by [RPC.get_branch client].

    If the [protocol] argument is supplied, the operation is forged locally
    (using [tezos-codec]), otherwise we call an RPC
    ([.../helpers/forge/operations]).

    If the [force] argument (by default [false]) is [true], then Tezt
    uses the RPC [/private/injection/operation] to allow for the injection of
    operations (which would be rejected by the default injection RPC).

    The [async] argument, whose default value is [false], is passed to
    the RPC during injection.

    On success, the function returns the injected operation's hash.
 *)
val forge_and_inject_operation :
  ?protocol:Protocol.t ->
  ?branch:string ->
  ?async:bool ->
  ?force:bool ->
  batch:manager_operation_content list ->
  signer:Account.key ->
  Client.t ->
  string Lwt.t

(** {2 High-level transfers injection} *)

(** [inject_transfer] is a high-level wrapper around
    [inject_operation] RPC for injecting a transfer operation.

    See {!forge_and_inject_operation} for argument [protocol].

    - Default [branch] is the current branch.
    - Default [counter] is the successor of the counter of [source].
    - Default [amount] is [1] tez.
    - Default [fee] is [1000] mutez.
    - Default [gas_limit] is [1040] gas.
    - Default [source] is [Constant.bootstrap1].
    - Default [destination] is [Constant.bootstrap2].
    - Default [force] is [false].
 *)
val inject_transfer :
  ?protocol:Protocol.t ->
  ?branch:string ->
  ?counter:int ->
  ?amount:int ->
  ?fee:int ->
  ?gas_limit:int ->
  ?source:Account.key ->
  ?destination:Account.key ->
  ?force:bool ->
  Client.t ->
  string Lwt.t

(** [inject_transfers] is a wrapper around [inject_transfer] to inject
    [number_of_operations] transfers with the same parameters.
    See {!inject_transfer} for optional arguments.
 *)
val inject_transfers :
  ?protocol:Protocol.t ->
  ?amount:int ->
  ?fee:int ->
  ?gas_limit:int ->
  ?source:Account.key ->
  ?destination:Account.key ->
  ?force:bool ->
  node:Node.t ->
  number_of_operations:int ->
  Client.t ->
  string list Lwt.t
