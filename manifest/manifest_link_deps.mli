(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>               *)
(*                                                                           *)
(*****************************************************************************)

(** Module holding types related to handling [link_deps] of targets. *)
module LinkTypes : sig
  type rust_dep = Rustzcash | Wasmer | Riscv_pvm | Etherlink_wasm_runtime

  type t = RustDep of rust_dep
end

(** Type used to specify [link_deps] when declaring manifest targets. *)
type t = LinkTypes.t

(** Types & functions used by the [LinkDeps] module. Workaround for circular dependencies. *)
module type TargetUtilsSig = sig
  type target

  type internal_target

  type target_kind

  val get_internal : target -> internal_target option

  val get_link_deps : internal_target -> t list

  val is_internal_kind_lib : target_kind -> bool

  val path_of_internal_target : internal_target -> string

  val debug_name : target -> string

  val debug_kind : target_kind -> string

  val error : ('a, unit, string, unit) format4 -> 'a
end

(** Module holding the logic of handling the linking dependencies provided in the
    [link_deps] argument of manifest targets. *)
module LinkDeps : functor (TU : TargetUtilsSig) -> sig
  (** Describes which target options can be modified by applying the rules for linking dependencies *)
  type computed_options = {
    deps : TU.target list;
    link_deps : t list;
    inline_tests_libraries : TU.target option list option;
    inline_tests_link_flags : string list option;
  }

  (** Generate a manifest target with the given [kind], [deps], [link_deps] and inline tests options
      compute the resulting [deps], [link_deps] and inline tests options taking into account 
      unification of linking dependencies and type of target (e.g. library/executable) *)
  val compute_opts :
    kind:TU.target_kind ->
    deps:TU.target list ->
    link_deps:t list ->
    inline_tests:'a option ->
    inline_tests_libraries:TU.target option list option ->
    inline_tests_link_flags:string list option ->
    computed_options

  (** Register an internal target and associated options corresponding to a list of link deps.
      Returns the same list of link deps to be used in end-user manifest targets. *)
  val register_archive :
    t list -> ?inline_tests_link_flags:string list -> TU.target -> t list
end
