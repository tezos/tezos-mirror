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
  (t -> unit Lwt.t) ->
  protocols:t list ->
  unit

(** Register a long-test that uses the propocol.

    This is the same as [Long_test.register], with the same differences
    as [Protocol.register_test] compared to [Test.register]. *)
val register_long_test :
  __FILE__:string ->
  title:string ->
  tags:string list ->
  ?team:string ->
  executors:Long_test.executor list ->
  timeout:Long_test.timeout ->
  (t -> unit Lwt.t) ->
  protocols:t list ->
  unit

(** Register a regression test that uses the protocol.

    This is the same as [Regression.register], with the same differences
    as [Protocol.register_test] compared to [Test.register]. *)
val register_regression_test :
  __FILE__:string ->
  title:string ->
  tags:string list ->
  output_file:string ->
  ?regression_output_path:string ->
  (t -> unit Lwt.t) ->
  protocols:t list ->
  unit
