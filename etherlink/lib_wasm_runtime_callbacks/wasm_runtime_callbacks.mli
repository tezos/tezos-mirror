(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type scope

val root_scope : ?trace_host_funs:bool -> Opentelemetry.Scope.t option -> scope

val register : unit -> unit

val store_get_hash :
  Irmin_context.tree -> string -> (bytes, Error_code.t) result

module Internal_for_tests : sig
  val read_durable_value :
    Irmin_context.tree -> string -> (bytes, Error_code.t) result

  val store_delete :
    scope ->
    Irmin_context.tree ->
    string ->
    bool ->
    (Irmin_context.tree, Error_code.t) result

  val store_copy :
    scope ->
    Irmin_context.tree ->
    string ->
    string ->
    (Irmin_context.tree, Error_code.t) result

  val store_move :
    scope ->
    Irmin_context.tree ->
    string ->
    string ->
    (Irmin_context.tree, Error_code.t) result

  val mem_tree : Irmin_context.tree -> string -> (bool, Error_code.t) result

  val store_has :
    scope -> Irmin_context.tree -> string -> (int, Error_code.t) result

  val store_get_hash :
    Irmin_context.tree -> string -> (bytes, Error_code.t) result

  val store_list_size :
    scope -> Irmin_context.tree -> string -> (int, Error_code.t) result

  val store_value_size :
    scope -> Irmin_context.tree -> string -> (int, Error_code.t) result

  val store_read :
    scope ->
    Irmin_context.tree ->
    string ->
    int ->
    int ->
    (bytes, Error_code.t) result

  val store_write :
    scope ->
    Irmin_context.tree ->
    string ->
    int ->
    bytes ->
    (Irmin_context.tree * int, Error_code.t) result

  val store_write_all :
    scope ->
    Irmin_context.tree ->
    string ->
    bytes ->
    (Irmin_context.tree, Error_code.t) result

  (** [check_reboot_flag tree] returns [true] if the reboot flag was set in the
      durable storage of [tree], [false] otherwise. Besides, it returns a new
      [tree] where the reboot flag as be removed (if it was absent, [tree] is
      returned unchanged). *)
  val check_reboot_flag : Irmin_context.tree -> bool * Irmin_context.tree

  module Vector : module type of Vector

  module Error_code : module type of Error_code
end
