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

(** Protocols we may want to test with. *)
type t = Hangzhou | Ithaca | Alpha

(** Protocol parameters.

    These values denote which file to use from the ["parameters"] directory. *)
type constants = Constants_sandbox | Constants_mainnet | Constants_test

(** The default constants used by tests: [Constants_sandbox]. *)
val default_constants : constants

(** Get the name of a protocol, capitalized (e.g. ["Edo"]). *)
val name : t -> string

(** Get the number of a protocol, e.g. 012 for Ithaca.

    The number for [Alpha] is the number it will have once snapshotted.

    Use this to specify constraints that should not change once Alpha is snapshotted.
    For instance, [number protocol >= 012] meant "at least Ithaca" even when
    Ithaca was still Alpha, while [number protocol >= number Alpha] stopped being true
    for Ithaca after it was snapshotted. *)
val number : t -> int

(** Get the name of a protocol as a tag, for use when registering tests (e.g. ["edo"]). *)
val tag : t -> string

(** Get the full hash of a protocol. *)
val hash : t -> string

(** Hash of protocol genesis *)
val genesis_hash : string

(** Get the location of the parameter file.

    This returns the path to one of the parameter files of the ["parameters"]
    directory of the protocol, relative to the root of the repository. *)
val parameter_file : ?constants:constants -> t -> string

(** Get the path of the accuser of a protocol, such as ["./tezos-accuser-alpha"]. *)
val accuser : t -> string

(** Get the path of the baker of a protocol, such as ["./tezos-baker-alpha"]. *)
val baker : t -> string

(** Get the part of the daemon name that is specific to a protocol (e.g. ["008-PtEdo2Zk"]). *)
val daemon_name : t -> string

(** Get the part which is added at the beginning of all encoding names.

    It turns out this is equal to what the [daemon_name] function returns. *)
val encoding_prefix : t -> string

(** Values to override in protocol parameters.

    The are pairs of JSON paths and optional values that can be used
    to override or remove (when the value is [None]) the default parameters
    when activating protocol. *)
type parameter_overrides = (string list * string option) list

(** Write a protocol parameter file.

    This function first builds a default parameter file from the [base]
    parameter. If [base] is a [string] {!Either.Left}, the string denotes a path
    to a parameter file like ["src/proto_alpha/parameters/sandbox-parameters.json"],
    which is taken as the base parameters. If [base] is a {!t * constants option} {!Either.Right},
    the default parameters of the given protocol are the base parameters.

    Then, the base parameters are tweaked with:
    - [parameters_overrides]
    - [additional_bootstrap_accounts] (with their optional default balance) are
      added to the list of bootstrap accounts of the protocol.
    *)
val write_parameter_file :
  ?additional_bootstrap_accounts:(Account.key * int option) list ->
  base:(string, t * constants option) Either.t ->
  parameter_overrides ->
  string Lwt.t

(** Get the successor of a protocol.

    WARNING: use of this function is discouraged, because:
    - a protocol may have several possible successors;
    - it prevents the type-checker from telling you that your test can no longer
      run when removing a protocol. *)
val next_protocol : t -> t option

(** Get the predecessor of a protocol.

    WARNING: use of this function is discouraged, because it prevents the type-checker
    from telling you that your test can no longer run when removing a protocol. *)
val previous_protocol : t -> t option

(** Get the list of all protocols.

    WARNING: use of this function is discouraged, because:
    - if we add protocols such as demo-noop or demo-counter to type [t],
      it would be awkward to add them to [all] if [all] only contained "real" protocols
      up to now, and it would also be awkward not to add them to [all];
    - it is easier for maintainers if the list of protocols a test runs on is clearly
      defined where the test is registered. *)
val all : t list

(** Constraints on the set protocols that a test supports.

    Do not use this to decide which protocols the test should run on, this is the role
    of the last argument of [register_test] and [register_regression_test].
    Instead, declare what the test *could* run on.

    - [Any_protocol]: the test can run on all active protocols. This is the default.
    - [From_protocol n]: the test can run on protocols [p] such that [number p >= n].
    - [Until_protocol n]: the test can run on protocols [p] such that [number p <= n].
    - [Between_protocols (a, b)]: the test can run on protocols [p]
      such that [a <= number p <= b].

    Always write the number itself, do not compute it.
    For instance, writing [Until_protocol (number Alpha)] would make your test
    not support the snapshotted version Alpha after Alpha is snapshotted,
    even though it actually does.

    It is recommended to write protocol numbers with leading zeros.
    For instance, write [From_protocol 008] instead of [From_protocol 8].*)
type supported_protocols =
  | Any_protocol
  | From_protocol of int
  | Until_protocol of int
  | Between_protocols of int * int

(** Register a test that uses the protocol.

    This is the same as [Test.register] except that:
    - the name of the protocol is automatically added at the beginning of [title];
    - the name of the protocol is automatically added as a tag in [tags];
    - the function that implements the test takes a protocol as parameter;
    - an additional parameter gives the list of protocols to run the test on.

    The list of protocol is the last parameter because this function is intended
    to be applied partially, without this list; the test function being
    the parameter of type [t -> unit Lwt.t] before the list. The list of protocols is
    specified in [main.ml], next to the registration of all other tests, so that it
    is easy to see at a glance which tests run on which protocols.

    If your test involves several protocols (for instance to test migration),
    use [Test.register] directly. *)
val register_test :
  __FILE__:string ->
  title:string ->
  tags:string list ->
  ?supports:supported_protocols ->
  (t -> unit Lwt.t) ->
  t list ->
  unit

(** Register a long-test that uses the propocol.

    This is the same as [Long_test.register], with the same differences
    as [Protocol.register_test] compared to [Test.register]. *)
val register_long_test :
  __FILE__:string ->
  title:string ->
  tags:string list ->
  ?supports:supported_protocols ->
  ?team:string ->
  executors:Long_test.executor list ->
  timeout:Long_test.timeout ->
  (t -> unit Lwt.t) ->
  t list ->
  unit

(** Register a regression test that uses the protocol.

    This is the same as [Regression.register], with the same differences
    as [Protocol.register_test] compared to [Test.register]. *)
val register_regression_test :
  __FILE__:string ->
  title:string ->
  tags:string list ->
  ?supports:supported_protocols ->
  output_file:string ->
  (t -> unit Lwt.t) ->
  t list ->
  unit
