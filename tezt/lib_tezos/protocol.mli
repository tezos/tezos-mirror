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
type t = Nairobi | Oxford | Alpha

val encoding : t Data_encoding.t

(** Protocol parameters.

    These values denote which file to use from the ["parameters"] directory. *)
type constants =
  | Constants_sandbox
  | Constants_mainnet
  | Constants_mainnet_with_chain_id
  | Constants_test

val constants_to_string : constants -> string

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

(** Get the directory of a protocol (e.g. ["proto_012_Psithaca"]). *)
val directory : t -> string

(** Get the name of a protocol as a tag, for use when registering tests (e.g. ["edo"]). *)
val tag : t -> string

(** Get the full hash of a protocol. *)
val hash : t -> string

(** Hash of protocol genesis *)
val genesis_hash : string

(** Hash of protocol demo_noops *)
val demo_noops_hash : string

(** Hash of protocol demo_counter *)
val demo_counter_hash : string

(** Hash of protocol zero *)
val protocol_zero_hash : string

(** Get the location of the parameter file.

    This returns the path to one of the parameter files of the ["parameters"]
    directory of the protocol, relative to the root of the repository. *)
val parameter_file : ?constants:constants -> t -> string

(** Get the path of the accuser of a protocol, such as ["./octez-accuser-alpha"]. *)
val accuser : t -> Uses.t

(** Get the path of the baker of a protocol, such as ["./octez-baker-alpha"]. *)
val baker : t -> Uses.t

(** Get the path of the smart rollup client of a protocol, such as
    ["./octez-smart-rollup-client-alpha"]. *)
val sc_rollup_client : t -> string

(** Get the part of the daemon name that is specific to a protocol (e.g. ["PtEdo2Zk"]).

    This should not be used for anything except to compute the name of executables. *)
val daemon_name : t -> string

(** Get the part which is added at the beginning of all encoding names. *)
val encoding_prefix : t -> string

(** Values to override in protocol parameters.

    They are pairs of JSON paths and optional values that can be used
    to override or remove (when the value is [`None]) the default parameters
    when activating a protocol.

    [`Int i] is a short-hand for [`Float (float i)] and
    [`String_of_int i] is a short-hand for [`String (string_of_int i)]. *)
type parameter_overrides =
  (string list * [`None | `Int of int | `String_of_int of int | JSON.u]) list

type bootstrap_smart_rollup = {
  address : string;
  pvm_kind : string;
  boot_sector : string;
  parameters_ty : Ezjsonm.value;
  whitelist : string list option;
}

type bootstrap_contract = {
  delegate : string option;
  amount : Tez.t;
  script : Ezjsonm.value;
  hash : string option;
}

(** The value is the same as the one in src/proto_alpha/lib_parameters/default_parameters.ml. *)
val default_bootstrap_balance : int

(** Write a protocol parameter file.

    This function first builds a default parameter file from the [base]
    parameter. If [base] is a [string] {!Either.Left}, the string denotes a path
    to a parameter file like ["src/proto_alpha/parameters/sandbox-parameters.json"],
    which is taken as the base parameters. If [base] is a {!t * constants option} {!Either.Right},
    the default parameters of the given protocol are the base parameters.

    Then, the base parameters are tweaked with:
    - [bootstrap_accounts], when given these accounts are used instead of
      [Account.Bootstrap.keys]
    - [parameters_overrides]
    - [additional_bootstrap_accounts] is a list of bootstrap accounts to
      add to activation parameters. Each account is a triplet
      [(key, balance, revealed)]. If [revealed] the public key is added,
      else the public key hash is added. Revealed keys are expected to bake
      from the start. Default [balance] is 4000000 tez.
    - [bootstrap_smart_rollups] when given.
    - [bootstrap_contracts] when given.
    - [output_file] the path where to write the protocol parameter file,
      a [Temp.file] temporary file "parameters.json" by default.
*)
val write_parameter_file :
  ?bootstrap_accounts:(Account.key * int option) list ->
  ?additional_bootstrap_accounts:(Account.key * int option * bool) list ->
  ?bootstrap_smart_rollups:bootstrap_smart_rollup list ->
  ?bootstrap_contracts:bootstrap_contract list ->
  ?output_file:string ->
  base:(string, t * constants option) Either.t ->
  parameter_overrides ->
  string Lwt.t

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
    - [Has_predecessor]: the test can run on protocols which have a predecessor
      according to [previous_protocol].
    - [And l]: all predicates of [l] hold.
    - [Or l]: at least one predicate of [l] hold.
    - [Not p]: predicate [p] does not hold.

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
  | Has_predecessor
  | And of supported_protocols list
  | Or of supported_protocols list
  | Not of supported_protocols

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
  ?uses:(t -> Uses.t list) ->
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
  ?uses:(t -> Uses.t list) ->
  ?supports:supported_protocols ->
  ?team:string ->
  executors:Long_test.executor list ->
  timeout:Long_test.timeout ->
  (t -> unit Lwt.t) ->
  t list ->
  unit

(** Register a regression test that uses the protocol.

    This is the same as [Regression.register], with the same differences
    as [Protocol.register_test] compared to [Test.register], and where
    [output_file] is parameterized by the protocol. *)
val register_regression_test :
  __FILE__:string ->
  title:string ->
  tags:string list ->
  ?uses:(t -> Uses.t list) ->
  ?supports:supported_protocols ->
  (t -> unit Lwt.t) ->
  t list ->
  unit

(** Convert a function that expects two successive protocols
    into a function that expects only one.

    The resulting function causes the test to fail if called on a protocol
    which does not have a predecessor. Use [~supports:Has_predecessor]
    to prevent this.

    Typical usage:
    {[
      Protocol.register_test ...
        ~supports:Has_predecessor
      @@ Protocol.with_predecessor
      @@ fun ~previous_protocol ~protocol ->
      ...
    ]}*)
val with_predecessor : (previous_protocol:t -> protocol:t -> 'a) -> t -> 'a
