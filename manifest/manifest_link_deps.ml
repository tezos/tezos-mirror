(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>               *)
(*                                                                           *)
(*****************************************************************************)

module LinkTypes = struct
  type rust_dep = Rustzcash | Wasmer | Riscv_pvm | Etherlink_wasm_runtime

  type archive = Rustzcash_deps | Rust_deps | Etherlink_wasm_runtime

  (* A sum type was chosen to signal currently only rust deps are supported. *)
  type t = RustDep of rust_dep

  (** Hardcoded path of Rust lib targets used "behind the scenes"
      to avoid managing them explicitly in manifest targets *)
  let rust_lib_of_path path =
    match path with
    | "src/rustzcash_deps" -> Some Rustzcash_deps
    | "src/rust_deps" -> Some Rust_deps
    | "etherlink/lib_wasm_runtime" -> Some Etherlink_wasm_runtime
    | _ -> None

  let id link_dep =
    match link_dep with
    | RustDep dep -> (
        match dep with
        | Rustzcash -> 1
        | Wasmer -> 2
        | Riscv_pvm -> 3
        | Etherlink_wasm_runtime -> 4)

  let compare lhs rhs = Int.compare (id lhs) (id rhs)
end

type t = LinkTypes.t

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

module LinkDeps (TU : TargetUtilsSig) = struct
  open LinkTypes
  module Key = Set.Make (LinkTypes)
  module Dep_map = Map.Make (Key)

  let registered_mappings : (TU.target * string list option) Dep_map.t ref =
    ref Dep_map.empty

  let target_implementing_archive archive =
    match Dep_map.find_opt archive !registered_mappings with
    | None -> (None, None)
    | Some (target, link_deps) -> (Some target, Some link_deps)

  let insert_mapping link_deps inline_tests_link_flags target =
    let key = Key.of_list link_deps in
    match target_implementing_archive key with
    | Some old_target, _ ->
        print_string @@ "WARNING: Trying to register target "
        ^ TU.debug_name target
        ^ " for a linking deps set. However, that set is already registered by \
           target: " ^ TU.debug_name old_target
    | None, _ ->
        registered_mappings :=
          Dep_map.add key (target, inline_tests_link_flags) !registered_mappings

  module RustDeps = struct end

  type computed_options = {
    deps : TU.target list;
    link_deps : t list;
    inline_tests_libraries : TU.target option list option;
    inline_tests_link_flags : string list option;
  }

  let compute_inline archive inline_tests inline_tests_libraries
      inline_tests_link_flags =
    (* This check is needed in order to not add inline_tests options for targets
       which do not define inline_tests, causing a crash in later checks in the manifest *)
    if Option.is_none inline_tests then
      (inline_tests_libraries, inline_tests_link_flags)
    else
      let inline_tests_link_flags : string list =
        Option.value ~default:[] inline_tests_link_flags
      in
      let inline_tests_libraries =
        Option.value ~default:[] inline_tests_libraries
      in
      let target, link_flags = target_implementing_archive archive in
      let link_flags =
        link_flags |> Option.join |> Option.to_list |> List.flatten
      in
      ( Some (inline_tests_libraries @ [target]),
        Some (inline_tests_link_flags @ link_flags) )

  let compute_link_deps kind link_deps deps =
    let reduced_link_deps =
      deps
      |> List.filter_map TU.get_internal
      |> List.map TU.get_link_deps |> List.flatten |> List.append link_deps
      |> List.sort_uniq compare
    in
    let archive = Key.of_list reduced_link_deps in
    (* Only need to add to concrete dependencies if target is an executable *)
    let deps =
      if TU.is_internal_kind_lib kind then deps
      else
        let target, _ = target_implementing_archive archive in
        Option.to_list target @ deps
    in
    (deps, reduced_link_deps, archive)

  let compute_opts ~kind ~deps ~link_deps ~inline_tests ~inline_tests_libraries
      ~inline_tests_link_flags =
    let is_linking_dep t =
      Option.is_some @@ Option.join
      @@ Option.map LinkTypes.rust_lib_of_path
      @@ Option.map TU.path_of_internal_target
      @@ TU.get_internal t
    in
    List.iter
      (fun d ->
        if is_linking_dep d then
          TU.error
            "The linking dependecy %s for target %s was specified as a normal \
             dependency. This is not allowed, they instead should only be \
             specified as linking dependencies using 'link_deps'"
            (TU.debug_name d)
            (TU.debug_kind kind))
      deps ;
    let deps, link_deps, rust_lib = compute_link_deps kind link_deps deps in
    let inline_tests_libraries, inline_tests_link_flags =
      compute_inline
        rust_lib
        inline_tests
        inline_tests_libraries
        inline_tests_link_flags
    in
    {deps; link_deps; inline_tests_libraries; inline_tests_link_flags}

  let register_archive link_deps ?inline_tests_link_flags target =
    insert_mapping link_deps inline_tests_link_flags target ;
    link_deps
end
