(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Utils.Infix

type protocol = {name : string; version : int option; hash : string option}

type t = {git_dir : string; commits : int; source : protocol; target : protocol}

let get_git_dir {git_dir; _} = git_dir

let eq_source_target {source; target; _} = source = target

let incr_commit_count state = {state with commits = state.commits + 1}

module type Dir_sig = sig
  val get_src_proto : t -> string

  val get_lib_protocol : t -> string

  val get_doc : t -> string
end

module type Protocol_sig = sig
  module Dir : sig
    module Relative : Dir_sig

    module Absolute : Dir_sig
  end

  val get_name : t -> string

  val get_version_value : t -> string

  val get_constructor : t -> string

  val get_hash : __LOC__:string -> ?short:unit -> t -> string

  val get_bin_suffix : __LOC__:string -> t -> string

  val get_module_name_root : __LOC__:string -> t -> string

  val get_dune_name : __LOC__:string -> t -> string

  val get_version : __LOC__:string -> t -> int
end

module type Get_protocol = sig
  val get_protocol : t -> protocol
end

let get_src_proto_dir proto =
  match (proto.version, proto.hash) with
  | None, _ -> ("src" // "proto_") ^ proto.name
  | Some version, None ->
      Format.asprintf "%s_%03d_%s" ("src" // "proto_") version proto.name
  | Some version, Some hash ->
      Format.asprintf "%s_%03d_%s" ("src" // "proto_") version hash

let get_doc_dir proto = "docs" // proto.name

let exit ~__LOC__ (state : t) =
  let open Log in
  error "Exiting because of an error at %s" __LOC__ ;
  if state.commits > 0 then
    printfln
      "@[<v 2>@{<green>To clean up git commits run:@}@,\
       git reset --hard  HEAD~%i"
      state.commits ;
  printfln
    "@[<v 0>@[<v 2>@{<green>To clean up other created files:@}@,\
     rm -rf %s@,\
     rm -rf src/proto_%s@]@,\
     exiting ...@]"
    (get_doc_dir state.target)
    (get_src_proto_dir state.target) ;
  exit 1

let make_absolute relative state = state.git_dir // relative

module Protocol (G : Get_protocol) = struct
  module Dir = struct
    module Relative = struct
      let get_src_proto state = get_src_proto_dir (G.get_protocol state)

      let get_lib_protocol state = get_src_proto state // "lib_protocol"

      let get_doc state = get_doc_dir (G.get_protocol state)
    end

    module Absolute = struct
      let get_src_proto state =
        make_absolute (get_src_proto_dir (G.get_protocol state)) state

      let get_lib_protocol state = get_src_proto state // "lib_protocol"

      let get_doc state =
        make_absolute (get_doc_dir (G.get_protocol state)) state
    end
  end

  let get_name state = (G.get_protocol state).name

  let get_version_value state =
    let proto = G.get_protocol state in
    match proto.version with
    | None -> proto.name
    | Some version -> Format.asprintf "%s_%03d" proto.name version

  let get_constructor state =
    String.capitalize_ascii (G.get_protocol state).name

  let get_hash ~__LOC__ ?short state =
    let proto = G.get_protocol state in
    match proto.hash with
    | Some hash -> (
        match short with None -> hash | Some () -> String.sub hash 0 8)
    | None ->
        Log.error "Protocol %s does not contain a hash" proto.name ;
        exit ~__LOC__ state

  let get_bin_suffix ~__LOC__ state =
    let proto = G.get_protocol state in
    match (proto.version, proto.hash) with
    | None, _ -> proto.name
    | Some version, Some hash -> Format.asprintf "%03d_%s" version hash
    | Some _, None ->
        Log.error "Protocol %s does not contain a hash" proto.name ;
        exit ~__LOC__ state

  let get_module_name_root ~__LOC__ state =
    let proto = G.get_protocol state in
    match (proto.version, proto.hash) with
    | None, _ -> "protocol_" ^ proto.name
    | Some version, Some hash -> Format.asprintf "protocol_%03d_%s" version hash
    | Some _, None ->
        Log.error "Protocol %s does not contain a hash" proto.name ;
        exit ~__LOC__ state

  let get_dune_name ~__LOC__ state =
    let proto = G.get_protocol state in
    match (proto.version, proto.hash) with
    | None, _ -> proto.name
    | Some version, Some hash -> Format.asprintf "%03d_%s" version hash
    | Some _, None ->
        Log.error "Protocol %s does not contain a hash" proto.name ;
        exit ~__LOC__ state

  let get_version ~__LOC__ state =
    let proto = G.get_protocol state in
    match proto.version with
    | None ->
        Log.error "%s protocol does not contain a version" proto.name ;
        exit ~__LOC__ state
    | Some version -> version
end

module Source = Protocol (struct
  let get_protocol {source; _} = source
end)

module Target = Protocol (struct
  let get_protocol {target; _} = target
end)

let create ~git_dir ~source ~target ~target_pattern =
  let parse_protocol proto =
    match String.split_on_char '_' proto with
    | [name; version] -> (
        match int_of_string_opt version with
        | Some _ as version -> {name; version; hash = None}
        | None ->
            Log.error "Cannot parse protocol target version." ;
            Log.error "Protocol target should be of form '%s'." target_pattern ;
            Stdlib.exit 1)
    | [name] -> {name; version = None; hash = None}
    | _ ->
        Log.error "Cannot parse protocol target." ;
        Log.error "Protocol target should be of form '%s'." target_pattern ;
        Stdlib.exit 1
  in
  {
    git_dir;
    commits = 0;
    source = parse_protocol source;
    target = parse_protocol target;
  }

let set_target_hash ~hash state = {state with target = {state.target with hash}}
