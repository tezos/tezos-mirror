(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Wrapper around Tezt to declare test dependencies. *)

include module type of Tezt

module Uses : sig
  (** Test dependencies.

      [Test.register] and [Regression.register] take an optional argument [?uses]
      that allows to declare that the test uses a given file.

      For instance, you can define:
      {[
        let data = Uses.make ~tag:"data" ~path:"data/file.dat"
      ]}
      You would then:
      - declare your test with [~uses:[data]];
      - use [Uses.path data] to get the path to your file.

      [~uses:[data]] adds the tag of [data] to the test tags.

      [Uses.path data] checks that the current test was declared with [data]
      in its [~uses]. And when you declare a test with [~uses:[data]],
      the test checks, at the end, that [Uses.path data] was called.
      This helps to maintain the invariant that a test that uses a given file
      has a given tag.

      Note that some uses are added by default to all tests.
      See section {!section:default} below. *)

  (** Test dependencies. *)
  type t

  (** Make a test dependency.

      Multiple paths can be associated with the same tag,
      and the same paths can be associated with different tags. *)
  val make : tag:string -> path:string -> t

  (** Get the path of a test dependency. *)
  val path : t -> string

  (** Get the tag of a test dependency. *)
  val tag : t -> string

  (** Get the first [Uses.t] that was created using [make] for a given path.

      Paths are considered equal modulo some inconsequential variations.
      For instance, ["./bin//./octez-node"] and ["bin/octez-node"]
      are considered equivalent. *)
  val lookup : string -> t option

  (** {2:default Default Uses} *)

  (** The following uses are added by default, but can be removed by specifying
      [~uses_node:false] or [~uses_client:false]. *)

  (** ["./octez-node"], with tag ["node"]. *)
  val octez_node : t

  (** ["./octez-client"], with tag ["client"]. *)
  val octez_client : t

  (** ["./octez-admin-client"], with tag ["admin_client"]. *)
  val octez_admin_client : t
end

module Test : sig
  include module type of Tezt.Test

  (** Wrapper over [Tezt.Test.register] that checks test dependencies ([?uses]). *)
  val register :
    __FILE__:string ->
    title:string ->
    tags:string list ->
    ?uses:Uses.t list ->
    ?uses_node:bool ->
    ?uses_client:bool ->
    ?uses_admin_client:bool ->
    ?seed:seed ->
    (unit -> unit Lwt.t) ->
    unit
end

module Regression : sig
  include module type of Tezt.Regression

  (** Wrapper over [Tezt.Regression.register] that checks test dependencies ([?uses]). *)
  val register :
    __FILE__:string ->
    title:string ->
    tags:string list ->
    ?uses:Uses.t list ->
    ?uses_node:bool ->
    ?uses_client:bool ->
    ?uses_admin_client:bool ->
    ?file:string ->
    (unit -> unit Lwt.t) ->
    unit
end

(** Error modes.

    - [Ignore]: do not warn, do not fail.
    - [Warn]: warn, but do not fail.
    - [Fail]: output an error and fail the test. *)
type error_mode = Ignore | Warn | Fail

(** What to do if a test uses something without having it declared in its [~uses].

    Recommended setting for tests that are intended to run in the CI is [Fail]. *)
val error_mode_for_missing_use : error_mode ref

(** What to do if a test declares something in its [~uses] and does not use it.

    Recommended setting for tests that are intended to run in the CI is [Warn],
    which is the default.

    Using [Fail] is possible but:
    - if a test is non-deterministic and only sometimes uses something,
      one needs to make sure [Uses.path] is always called anyway;
    - if a test actually uses something but not through [Uses.path],
      the wrapper will not be able to detect it, so one will have to call
      [Uses.path] just to suppress the error. *)
val error_mode_for_useless_use : error_mode ref
