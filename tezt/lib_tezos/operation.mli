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

(** Michelson scripts and data in different representations.

    Used when originating or calling contracts.
    Depending on the test, the user can provide:

    - an EzJsonm value directly
    - a Michelson value in a string
    - a Michelson value in a file (mainly for Michelson scripts). In this
      case, files should have extension `.tz`, `.tez` or `.mic`
    - a file storing directly the JSON representation. This is for instance
      useful to test contracts that fail at type-checking. Instead of
      Michelson, the user can generate the JSON and store it a file with
      a '.json' extension.
*)
type micheline =
  [ `Json of Ezjsonm.value  (** EzJsonm value *)
  | `Michelson of string  (** Michelson string *)
  | `File of string  (** file with ext .tz or .json for Ezjsonm *) ]

(** {2 Smart constructors} *)

(** [mk_transfer] allows to construct a manager operation representing a simple
    transfer between implicit accounts [source] and [dest].

    - Default [counter] is the successor of the counter of [source].
    - Default [amount] is [1] tez.
    - Default [fee] is [1000] mutez.
    - Default [gas_limit] is [1040] gas. Use a greater limit (e.g. ~1500) if the
      destination is not allocated, and ~1900 if the source will additionnaly be
      emptied.
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

(** [mk_delegation] allows to construct a manager operation representing a
    delegation to the [delegate] account.

    - Default [counter] is the successor of the counter of [source].
    - Default [fee] is [1000] mutez.
    - Default [gas_limit] is [1040] gas.
    - Default [storage_limit] is [0]. *)
val mk_delegation :
  source:Account.key ->
  ?counter:int ->
  ?fee:int ->
  ?gas_limit:int ->
  ?storage_limit:int ->
  delegate:string ->
  Client.t ->
  manager_operation_content Lwt.t

(** [mk_rejection] allows to construct a mangager operation
   representing a tx_rollup rejection operation.

    - Default [counter] is the successor of the counter of [source].
    - Default [fee] is [1_000_000] mutez.
    - Default [gas_limit] is [1_000_000] gas.
    - Default [storage_limit] is [0]. *)
val mk_rejection :
  source:Account.key ->
  ?counter:int ->
  ?fee:int ->
  ?gas_limit:int ->
  ?storage_limit:int ->
  tx_rollup:string ->
  proof:string ->
  level:int ->
  message:Rollup.Tx_rollup.message ->
  message_position:int ->
  message_path:string list ->
  message_result_hash:string ->
  message_result_path:JSON.u ->
  previous_message_result_path:JSON.u ->
  previous_message_context_hash:string ->
  previous_message_withdraw_list_hash:string ->
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
  arg:micheline ->
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
  code:micheline ->
  init_storage:micheline ->
  ?init_balance:int ->
  Client.t ->
  manager_operation_content Lwt.t

(** {2 Helper functions to build manager operations} *)

(** Returns the current counter of the given implicit account *)
val get_counter : source:Account.key -> Client.t -> int Lwt.t

(** Returns the next counter of the given implicit account *)
val get_next_counter : source:Account.key -> Client.t -> int Lwt.t

(** Returns [branch] if supplied as argument, or fetches and returns
    the default injection branch, i.e. [head - 2]. *)
val get_injection_branch : ?branch:string -> Client.t -> string Lwt.t

(** {2 Forging, signing and injecting operations} *)

(** [sign_manager_op_bytes ~signer op_bytes] signs [op_bytes] with
    [signer]'s secret key and the watermark [Generic_operation]. *)
val sign_manager_op_bytes :
  signer:Account.key -> bytes -> Tezos_crypto.Signature.t

(** Same as [sign_manager_op_bytes], but the input operation is given
   in hexadecimal representation and returns a signature in
   hexadecimal representation. *)
val sign_manager_op_hex : signer:Account.key -> Hex.t -> Hex.t

(** Forge an operation and returns the hexadecimal binary representation.

    If the [protocol] argument is supplied, the operation is forged locally
    (using [tezos-codec]), otherwise we call an RPC
    ([.../helpers/forge/operations]).
 *)
val forge_operation :
  ?protocol:Protocol.t ->
  branch:string ->
  batch:manager_operation_content list ->
  Client.t ->
  Hex.t Lwt.t

(** Inject a forged operation with its signature.

    If the [force] argument (by default [false]) is [true], then Tezt
    uses the RPC [/private/injection/operation] to allow for the injection of
    operations (which would be rejected by the default injection RPC).

    The [async] argument, whose default value is [false], is passed to
    the RPC during injection.

    On success, the function returns the injected operation's hash.
  *)
val inject_operation :
  ?async:bool ->
  ?force:bool ->
  ?wait_for_injection:Node.t ->
  unsigned_op:Hex.t ->
  signature:Hex.t ->
  Client.t ->
  [`OpHash of string] Lwt.t

val runnable_inject_operation :
  ?async:bool ->
  ?force:bool ->
  unsigned_op:Hex.t ->
  signature:Hex.t ->
  Client.t ->
  JSON.t Process.runnable

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
  ?wait_for_injection:Node.t ->
  batch:manager_operation_content list ->
  signer:Account.key ->
  Client.t ->
  [`OpHash of string] Lwt.t

val runnable_forge_and_inject_operation :
  ?protocol:Protocol.t ->
  ?branch:string ->
  ?async:bool ->
  ?force:bool ->
  batch:manager_operation_content list ->
  signer:Account.key ->
  Client.t ->
  JSON.t Process.runnable Lwt.t

(** {2 High-level injection functions} *)

(** [inject_origination] is a high-level wrapper that allows to construct
    an origination operation and inject it using the given client. The [signer]
    can be different from the [source] to be able to inject missigned operations.

    See {!mk_origination} and {!forge_and_inject_operation} for the list of
    parameters and their default values. *)
val inject_origination :
  ?protocol:Protocol.t ->
  ?async:bool ->
  ?force:bool ->
  ?wait_for_injection:Node.t ->
  ?branch:string ->
  source:Account.key ->
  ?signer:Account.key ->
  ?counter:int ->
  ?fee:int ->
  ?gas_limit:int ->
  ?storage_limit:int ->
  code:micheline ->
  init_storage:micheline ->
  ?init_balance:int ->
  Client.t ->
  [`OpHash of string] Lwt.t

(** [inject_public_key_revelation] is a high-level wrapper that allows to build
    public key reveal operations and to inject them using the given client. The
    [signer] can be different from the [source] to be able to inject missigned
    operations. Also, it is possible to provide a [public_key] that does not
    match the one of the [signer] or of the [source].

    See {!mk_reveal} and {!forge_and_inject_operation} for the list of
    parameters and their default values. *)
val inject_public_key_revelation :
  ?protocol:Protocol.t ->
  ?async:bool ->
  ?force:bool ->
  ?wait_for_injection:Node.t ->
  ?branch:string ->
  source:Account.key ->
  ?signer:Account.key ->
  ?counter:int ->
  ?fee:int ->
  ?gas_limit:int ->
  ?storage_limit:int ->
  ?public_key:string ->
  Client.t ->
  [`OpHash of string] Lwt.t

(** [inject_delegation] is a high-level wrapper that allows to build
    delegation operations and to inject them using the given client. The
    [signer] can be different from the [source] to be able to inject missigned
    operations.

    See {!mk_delegation} and {!forge_and_inject_operation} for the list of
    parameters and their default values. *)
val inject_delegation :
  ?protocol:Protocol.t ->
  ?async:bool ->
  ?force:bool ->
  ?wait_for_injection:Node.t ->
  ?branch:string ->
  source:Account.key ->
  ?signer:Account.key ->
  ?counter:int ->
  ?fee:int ->
  ?gas_limit:int ->
  ?storage_limit:int ->
  delegate:string ->
  Client.t ->
  [`OpHash of string] Lwt.t

(** [inject_transfer] is a high-level wrapper that allows to build XTZ transfer
    operations and to inject it using the given client. The
    [signer] can be different from the [source] to be able to inject missigned
    operations.

    See {!mk_transfer} and {!forge_and_inject_operation} for the list of
    parameters and their default values. *)
val inject_transfer :
  ?protocol:Protocol.t ->
  ?async:bool ->
  ?force:bool ->
  ?wait_for_injection:Node.t ->
  ?branch:string ->
  source:Account.key ->
  ?signer:Account.key ->
  ?counter:int ->
  ?fee:int ->
  ?gas_limit:int ->
  ?storage_limit:int ->
  dest:Account.key ->
  ?amount:int ->
  Client.t ->
  [`OpHash of string] Lwt.t

(** [inject_contract_call] is a high-level wrapper that allows to build smart
    contracts calls operations and to inject it using the given client. The
    [signer] can be different from the [source] to be able to inject missigned
    operations.

    See {!mk_call} and {!forge_and_inject_operation} for the list of
    parameters and their default values. *)
val inject_contract_call :
  ?protocol:Protocol.t ->
  ?async:bool ->
  ?force:bool ->
  ?wait_for_injection:Node.t ->
  ?branch:string ->
  source:Account.key ->
  ?signer:Account.key ->
  ?counter:int ->
  ?fee:int ->
  ?gas_limit:int ->
  ?storage_limit:int ->
  dest:string ->
  ?amount:int ->
  entrypoint:string ->
  arg:micheline ->
  Client.t ->
  [`OpHash of string] Lwt.t

(** [inject_transfers] is a wrapper around [inject_transfer] to inject
    [number_of_operations] transfers with the same parameters.
    See {!inject_transfer} for optional arguments.
 *)
val inject_transfers :
  ?protocol:Protocol.t ->
  ?async:bool ->
  ?force:bool ->
  ?wait_for_injection:Node.t ->
  ?amount:int ->
  ?fee:int ->
  ?gas_limit:int ->
  ?source:Account.key ->
  ?destination:Account.key ->
  node:Node.t ->
  number_of_operations:int ->
  Client.t ->
  [`OpHash of string] list Lwt.t

(** [inject_rejection] is a high-level wrapper around the tx_rollup
   rejection operation (see {!mk_rejection}). *)
val inject_rejection :
  ?protocol:Protocol.t ->
  ?async:bool ->
  ?force:bool ->
  ?wait_for_injection:Node.t ->
  ?branch:string ->
  source:Account.key ->
  ?signer:Account.key ->
  ?counter:int ->
  ?fee:int ->
  ?gas_limit:int ->
  ?storage_limit:int ->
  tx_rollup:string ->
  proof:string ->
  level:int ->
  message:Rollup.Tx_rollup.message ->
  message_position:int ->
  message_path:string list ->
  message_result_hash:string ->
  message_result_path:JSON.u ->
  previous_message_result_path:JSON.u ->
  previous_message_context_hash:string ->
  previous_message_withdraw_list_hash:string ->
  Client.t ->
  [`OpHash of string] Lwt.t

(** [inject_transfer_ticket] constructs and injects a mangager operation
    representing a transfer ticket operation. *)
val inject_transfer_ticket :
  ?protocol:Protocol.t ->
  ?async:bool ->
  ?force:bool ->
  ?wait_for_injection:Node.t ->
  ?branch:string ->
  source:Account.key ->
  ?signer:Account.key ->
  ?counter:int ->
  ?fee:int ->
  ?gas_limit:int ->
  ?storage_limit:int ->
  contents:micheline ->
  ty:micheline ->
  ticketer:string ->
  amount:int ->
  destination:string ->
  entrypoint:string ->
  Client.t ->
  [`OpHash of string] Lwt.t
