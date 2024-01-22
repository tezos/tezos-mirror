(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

let default_warnings = Defaults.warnings

let default_warn_error = "-a+8"

(** Override the default 'Env.Persistent_signature.load'
    with a lookup in locally defined hashtable.
*)

let preloaded_cmis : Persistent_env.Persistent_signature.t String.Hashtbl.t =
  String.Hashtbl.create ~random:true 42

let default_load = !Persistent_env.Persistent_signature.load

(* Set hook *)
let () =
  Persistent_env.Persistent_signature.load :=
    fun ~unit_name ->
      String.Hashtbl.find preloaded_cmis (String.capitalize_ascii unit_name)

let load_cmi_from_file file =
  String.Hashtbl.add
    preloaded_cmis
    (String.capitalize_ascii Filename.(basename (chop_extension file)))
    {filename = file; cmi = Cmi_format.read_cmi file}

let load_embedded_cmi (unit_name, content) =
  let content = Bytes.of_string content in
  (* Read cmi magic *)
  let magic_len = String.length Config.cmi_magic_number in
  let magic = Bytes.sub content 0 magic_len in
  assert (magic = Bytes.of_string Config.cmi_magic_number) ;
  (* Read cmi_name and cmi_sign *)
  let pos = magic_len in
  let cmi_name, cmi_sign = Marshal.from_bytes content pos in
  let pos = pos + Marshal.total_size content pos in
  (* Read cmi_crcs *)
  let cmi_crcs = Marshal.from_bytes content pos in
  let pos = pos + Marshal.total_size content pos in
  (* Read cmi_flags *)
  let cmi_flags = Marshal.from_bytes content pos in
  (* TODO check crcrs... *)
  String.Hashtbl.add
    preloaded_cmis
    (String.capitalize_ascii unit_name)
    {
      filename = unit_name ^ ".cmi";
      cmi = {cmi_name; cmi_sign; cmi_crcs; cmi_flags};
    }

let load_embedded_cmis cmis = List.iter load_embedded_cmi cmis

(** Compilation environment.

    [tezos_protocol_env] defines the list of [cmi] available while
    compiling the protocol version. The [cmi] are packed into the
    [octez-node] binary by using [ocp-ocamlres], see the Makefile.

    [register_env] defines a complementary list of [cmi] available
    while compiling the generated [register.ml] file (that register
    the protocol first-class module into the [Updater.versions]
    hashtable).

*)

let all_files l =
  List.map
    (fun (`File (fname, content)) ->
      (String.capitalize_ascii (Filename.chop_suffix fname ".cmi"), content))
    l

let tezos_protocol_env = all_files Embedded_cmis_env.root

let register_env = all_files Embedded_cmis_register.root

(** Helpers *)

open Filename.Infix

let create_file ?(perm = 0o644) name content =
  let open Unix in
  let fd = openfile name [O_TRUNC; O_CREAT; O_WRONLY] perm in
  ignore (write_substring fd content 0 (String.length content)) ;
  close fd

let safe_unlink file =
  try Unix.unlink file with Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let unlink_cmi dir (file, _) = safe_unlink ((dir // file) ^ ".cmi")

let unlink_object obj =
  safe_unlink obj ;
  safe_unlink (Filename.chop_suffix obj ".cmx" ^ ".cmi") ;
  safe_unlink (Filename.chop_suffix obj ".cmx" ^ ".o")

let debug_flag = ref false

let debug fmt =
  if !debug_flag then Format.eprintf fmt
  else Format.ifprintf Format.err_formatter fmt

let mktemp_dir () =
  Filename.get_temp_dir_name ()
  // Printf.sprintf "tezos-protocol-build-%06X" (Random.int 0xFFFFFF)

(** Main *)

type driver = {
  compile_ml : ?for_pack:string -> string -> string;
  pack_objects : string -> string list -> string;
  link_shared : string -> string list -> unit;
}

let parse_options errflag s =
  Option.iter Location.(prerr_alert none) (Warnings.parse_options errflag s)

let main {compile_ml; pack_objects; link_shared} version =
  Random.self_init () ;
  parse_options false default_warnings ;
  parse_options true default_warn_error ;
  let anonymous = ref []
  and static = ref false
  and register = ref false
  and build_dir = ref None
  and output_file = ref None
  and output_dep = ref false
  and hash_only = ref false
  and check_protocol_hash = ref true in
  let args_spec =
    [
      ("-o", Arg.String (fun s -> output_file := Some s), "");
      ( "-hash-only",
        Arg.Set hash_only,
        " Only display the hash of the protocol and don't compile" );
      ( "-no-hash-check",
        Arg.Clear check_protocol_hash,
        " Don't check that TEZOS_PROTOCOL declares the expected protocol hash \
         (if existent)" );
      ("-static", Arg.Set static, " Only build the static library (no .cmxs)");
      ("-register", Arg.Set register, " Generate the `Registerer` module");
      ("-bin-annot", Arg.Set Clflags.binary_annotations, " (see ocamlopt)");
      ("-g", Arg.Set Clflags.debug, " (see ocamlopt)");
      ("-output-dep", Arg.Set output_dep, " ...");
      ( "-build-dir",
        Arg.String (fun s -> build_dir := Some s),
        "use custom build directory and preserve build artifacts" );
      ( "--version",
        Unit
          (fun () ->
            Format.printf "%s\n" version ;
            Stdlib.exit 0),
        " Display version information" );
      ( "-warning",
        Arg.String (fun s -> parse_options false s),
        " <list> Enable or disable ocaml warnings according to <list>. This \
         extends the default: " ^ default_warnings );
      ( "-warn-error",
        Arg.String (fun s -> parse_options true s),
        " <list> Enable or disable ocaml error status according to <list>. \
         This extends the default: " ^ default_warn_error );
    ]
  in
  let usage_msg =
    Printf.sprintf "Usage: %s [options] <srcdir>\nOptions are:" Sys.argv.(0)
  in
  Arg.parse args_spec (fun s -> anonymous := s :: !anonymous) usage_msg ;
  let source_dir =
    match !anonymous with
    | [protocol_dir] -> protocol_dir
    | _ ->
        Arg.usage args_spec usage_msg ;
        Stdlib.exit 1
  in
  let stored_hash_opt, protocol =
    match Lwt_main.run (Tezos_base_unix.Protocol_files.read_dir source_dir) with
    | Ok (hash, proto) -> (hash, proto)
    | Error err ->
        Format.eprintf "Failed to read TEZOS_PROTOCOL: %a" pp_print_trace err ;
        exit 2
  in
  let computed_hash = Protocol.hash protocol in
  if !hash_only then (
    Format.printf "%a@." Protocol_hash.pp computed_hash ;
    exit 0) ;
  let hash =
    match stored_hash_opt with
    | None -> computed_hash
    | Some stored_hash
      when !check_protocol_hash
           && not (Protocol_hash.equal computed_hash stored_hash) ->
        Format.eprintf
          "Inconsistent hash for protocol in TEZOS_PROTOCOL.@\n\
           Computed hash: %a@\n\
           Stored in TEZOS_PROTOCOL: %a@."
          Protocol_hash.pp
          computed_hash
          Protocol_hash.pp
          stored_hash ;
        exit 2
    | Some hash -> hash
  in
  let build_dir =
    match !build_dir with
    | None ->
        let dir = mktemp_dir () in
        at_exit (fun () -> Lwt_main.run (Lwt_utils_unix.remove_dir dir)) ;
        dir
    | Some dir -> dir
  in
  let output =
    match !output_file with
    | Some output -> output
    | None -> Format.asprintf "proto_%a" Protocol_hash.pp hash
  in
  Lwt_main.run (Lwt_utils_unix.create_dir ~perm:0o755 build_dir) ;
  Lwt_main.run (Lwt_utils_unix.create_dir ~perm:0o755 (Filename.dirname output)) ;
  (* Generate the 'functor' *)
  let functor_file = build_dir // "functor.ml" in
  let version = Protocol.module_name_of_env_version protocol.expected_env in
  let oc = open_out functor_file in
  Packer.dump
    oc
    version
    hash
    (Array.map
       (fun {Protocol.name; _} ->
         let name_lowercase = String.uncapitalize_ascii name in
         (source_dir // name_lowercase) ^ ".ml")
       (Array.of_list protocol.components)) ;
  close_out oc ;
  (* Compile the protocol *)
  let proto_cmi = Filename.chop_extension functor_file ^ ".cmi" in
  let functor_unit =
    String.capitalize_ascii Filename.(basename (chop_extension functor_file))
  in
  let for_pack = String.capitalize_ascii (Filename.basename output) in
  (* Initialize the compilers *)
  Compenv.(readenv Format.err_formatter Before_args) ;
  Clflags.nopervasives := true ;
  Clflags.no_std_include := true ;
  Clflags.include_dirs := [Filename.dirname functor_file] ;
  load_embedded_cmis tezos_protocol_env ;
  let packed_protocol_object = compile_ml ~for_pack functor_file in
  let register_objects =
    if not !register then []
    else (
      load_embedded_cmis register_env ;
      load_cmi_from_file proto_cmi ;
      (* Compiler the 'registering module' *)
      let register_file = Filename.dirname functor_file // "register.ml" in
      create_file
        register_file
        (Printf.sprintf
           "module Name = struct let name = %S end\n\
           \ let () = Tezos_protocol_registerer.register Name.name (%s (module \
            %s.Make))"
           (Protocol_hash.to_b58check hash)
           (Protocol.module_name_of_env_version protocol.expected_env)
           functor_unit) ;
      let register_object = compile_ml ~for_pack register_file in
      [register_object])
  in
  let resulting_object =
    pack_objects output (packed_protocol_object :: register_objects)
  in
  (* Create the final [cmxs] *)
  if not !static then (
    Clflags.link_everything := true ;
    link_shared (output ^ ".cmxs") [resulting_object]) ;
  if !output_dep then (
    let dsrc = Digest.file functor_file in
    let dimpl = Digest.file resulting_object in
    let dintf =
      Digest.file (Filename.chop_extension resulting_object ^ ".cmi")
    in
    Format.printf "module Toto = struct include %s end ;; \n" for_pack ;
    Format.printf "let src_digest = %S ;;\n" (Digest.to_hex dsrc) ;
    Format.printf "let impl_digest = %S ;;\n" (Digest.to_hex dimpl) ;
    Format.printf "let intf_digest = %S ;;\n" (Digest.to_hex dintf)) ;
  Format.printf "Success: %a.@." Protocol_hash.pp hash
