(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Utils.Infix

let error ~__LOC__ state () = State.exit ~__LOC__ state

let tmp_proto_snapshot_dir =
  Filename.get_temp_dir_name () // "tezos_proto_snapshot"

let tezt_protocol_file (state : State.t) =
  State.make_absolute ("tezt" // "lib_tezos" // "protocol.ml") state

type pattern = {string : string; regex : Re.re}

let pp_pattern fmt {string; _} = Format.pp_print_string fmt string

let protocol_pattern =
  let string = {|[a-z]+[0-9]*|} in
  {string; regex = Re.Perl.("^" ^ string ^ "$" |> re |> compile)}

let check_proto_name proto_name pattern =
  if not (Re.execp pattern.regex proto_name) then (
    Log.error
      "Protocol name '%s' should be of the form '%a'"
      proto_name
      pp_pattern
      pattern ;
    Stdlib.exit 1)

let check_arguments =
  let commands =
    Command.FIFO.empty
    |>
    (* Check source and target *)
    Command.register
      (Command.create
         ~desc:(fun state ->
           Log.printfln "check that protocol source and target are different" ;
           state)
         (fun state ->
           if State.eq_source_target state then (
             Log.error "protocol source and target should be different" ;
             Stdlib.exit 1) ;
           state))
    |> Command.register
         (Command.File.check_exists (fun state ->
              State.Source.Dir.Absolute.get_src_proto state))
    |> Command.register
         (Command.File.check_exists (fun state ->
              State.Source.Dir.Absolute.get_doc state))
    |> Command.register
         (Command.File.check_not_exist (fun state ->
              State.Target.Dir.Absolute.get_src_proto state))
    |> Command.register
         (Command.File.check_not_exist (fun state ->
              State.Target.Dir.Absolute.get_doc state))
    |>
    (* Check git directory *)
    Command.register
      (Command.File.check_exists
         ~error_msg:(fun _state ->
           "octez-protocol-compiler not found, compile it")
         (fun state -> State.make_absolute "octez-protocol-compiler" state))
    |> Command.register (Command.Git.check_no_uncommitted_changes ~__LOC__)
  in
  Command.of_fifo
    ~desc:(fun state ->
      Log.printfln "Check the arguments" ;
      state)
    commands

let clean_tmp_dir =
  Command.Shell.create ~__LOC__ (fun _state ->
      "rm -rf " ^ tmp_proto_snapshot_dir)

(** Copy src/proto_<source> to src/proto_<target> and remove auto-generated
    dune files. *)
let raw_copy =
  let commands =
    Command.FIFO.empty
    |> Command.register
         (Command.Log.printfln (fun state ->
              Format.asprintf
                "Copying %s to %s"
                (State.Source.Dir.Relative.get_src_proto state)
                (State.Target.Dir.Relative.get_src_proto state)))
    |> Command.register
         (Command.create
            ~desc:(fun state ->
              Log.printfln "create temp directory" ;
              state)
            (fun state ->
              Sys.mkdir tmp_proto_snapshot_dir 0o700 ;
              state))
    |> Command.register
         (Command.Git.cp
            ~__LOC__
            (fun state -> State.Source.Dir.Absolute.get_src_proto state)
            (fun _state -> tmp_proto_snapshot_dir))
    |> Command.register
         (Command.create
            ~desc:(fun state ->
              Log.printfln "move sources from temp to git repository." ;
              state)
            (fun state ->
              let _ =
                Utils.Process.shell
                  ~error:(error ~__LOC__ state)
                  (Format.asprintf
                     "mv %s %s"
                     (tmp_proto_snapshot_dir
                     // State.Source.Dir.Relative.get_src_proto state)
                     (State.Target.Dir.Absolute.get_src_proto state))
              in
              state))
    |> Command.register
         (Command.Shell.create ~__LOC__ (fun _state ->
              "rm -rf " ^ tmp_proto_snapshot_dir))
    |> Command.register
         (Command.create
            ~desc:(fun state ->
              Log.printfln {|delete all auto generated dune files.|} ;
              state)
            (fun state ->
              let files =
                Utils.File.find
                  ~error:(error ~__LOC__ state)
                  ~dir:(State.Target.Dir.Absolute.get_src_proto state)
                  ~opts:
                    {|-name dune -exec grep -l "; This file was automatically generated, do not edit." {} \;|}
              in
              (match files with
              | [] -> Log.warning "No auto-generated dune file found."
              | _ -> ()) ;
              List.iter Sys.remove files ;
              state))
    |> Command.register
         (Command.Git.commit ~__LOC__ ~no_verify:() (fun state ->
              "src: copy from " ^ State.Source.Dir.Relative.get_src_proto state))
  in
  Command.of_fifo
    ~desc:(fun state ->
      Log.printfln "Copy and clean protocol sources" ;
      state)
    commands

(** Update the version value of target protocol. *)
let set_current_version =
  let commands =
    (* set current version
       Starting from 018 the version value moved to `constants_repr`. To be
       able to snapshot older protocol the `raw_context` file is kept even
       if it is not strictly needed anymore. *)
    Command.FIFO.empty
    |> Command.register
         (Command.Log.printfln (fun _state ->
              "Setting current version in raw_context and proxy"))
    |> Command.register
         (Command.create
            ~desc:(fun state ->
              Log.printfln "" ;
              state)
            (fun state ->
              let files =
                [
                  State.Target.Dir.Absolute.get_lib_protocol state
                  // "constants_repr.ml";
                  State.Target.Dir.Absolute.get_lib_protocol state
                  // "raw_context.ml";
                  State.Target.Dir.Absolute.get_src_proto state
                  // "lib_client" // "proxy.ml";
                ]
              in
              let regex =
                let open Re in
                seq
                  [
                    str {|let version_value = "|};
                    State.Source.get_version_value state |> str;
                    [set {|"|}] |> compl |> rep;
                    str {|"|};
                  ]
                |> compile
              in
              Utils.File.Content.(
                check_modif_count_all
                  ~warn:(Msg "No version_value has been updated.")
                  (replace_string_all
                     ~regex
                     ~by:
                       (Format.asprintf
                          {|let version_value = "%s"|}
                          (State.Target.get_version_value state)))
                  files) ;
              state))
    |> Command.register
         (Command.Git.commit ~__LOC__ ~no_verify:() (fun _state ->
              "src: set current version"))
  in
  Command.of_fifo
    ~desc:(fun state ->
      Log.printfln "set current version" ;
      state)
    commands

(** Add the predecessor of target protocol. *)
let add_predecessor =
  let commands =
    let raw_context_file state =
      State.Target.Dir.Absolute.get_lib_protocol state // "raw_context.ml"
    in
    let files (state : State.t) =
      [
        raw_context_file state;
        State.Target.Dir.Absolute.get_lib_protocol state // "raw_context.mli";
        State.Target.Dir.Absolute.get_lib_protocol state // "init_storage.ml";
      ]
    in
    Command.FIFO.empty
    |> Command.register
         (Command.create
            ~desc:(fun state ->
              Log.printfln "add predecessor of target protocol" ;
              state)
            (fun state ->
              let by =
                Format.asprintf
                  {|else if Compare.String.(s = "%s") then return (%s, ctxt)|}
                  (State.Target.get_name state)
                  (State.Target.get_constructor state)
              in
              let regex =
                let open Re in
                seq
                  [
                    rep notnl;
                    str "return (";
                    State.Source.get_constructor state |> str;
                    str ", ctxt)";
                    rep notnl;
                  ]
                |> compile
              in
              let replacements =
                Utils.File.Content.replace_string
                  ~regex
                  ~by
                  (raw_context_file state)
              in
              if replacements <> 1 then Log.warning "No predecessor added" ;
              state))
    |> Command.register
         (Command.create
            ~desc:(fun state ->
              Log.printfln "Replace constructor occurences in files" ;
              state)
            (fun state ->
              let regex =
                Re.(State.Source.get_constructor state |> str |> compile)
              in
              let by = State.Target.get_constructor state in
              Utils.File.Content.(
                check_modif_count_all
                  ~warn:(Msg "No constructor has been updated")
                  (replace_string_all ~regex ~by)
                  (files state)) ;
              state))
    |> Command.register
         (Command.File.ocamlformat ~__LOC__ (fun state -> files state))
    |> Command.register
         (Command.Git.commit ~__LOC__ ~no_verify:() (fun state ->
              Format.asprintf
                "src: adapt %s predecessors"
                (State.Target.get_constructor state)))
  in
  Command.of_fifo
    ~desc:(fun state ->
      Log.printfln "add predecessors" ;
      state)
    commands

(** Hash the target protocol and use this hash to update TEZOS_PROTOCOL file. *)
let compute_and_replace_hash =
  let commands =
    Command.FIFO.empty
    |> Command.register (Command.Log.cyan (fun _state -> "Computing hash"))
    |> Command.register
         (Command.create
            ~desc:(fun state ->
              Log.printfln
                "Hash %s"
                (State.Target.Dir.Relative.get_src_proto state) ;
              State.set_target_hash ~hash:(Some "HASH") state)
            (fun state ->
              let bin = "octez-protocol-compiler" in
              let output =
                Utils.Process.exec
                  ~error:(error ~__LOC__ state)
                  ~dir:(State.get_git_dir state)
                  ~bin
                  ~args:
                    [
                      "-hash-only";
                      State.Target.Dir.Absolute.get_lib_protocol state;
                    ]
              in
              let hash = Utils.Process.stdout_by_line output in
              let hash =
                match hash with
                | [hash] -> hash
                | _ ->
                    Log.error "Invalid hash computed by %s" bin ;
                    State.exit ~__LOC__ state
              in
              let state = State.set_target_hash ~hash:(Some hash) state in
              Log.magenta
                "Hash computed: %s"
                (State.Target.get_hash ~__LOC__ state) ;
              Log.magenta
                "Short hash: %s"
                (State.Target.get_hash ~__LOC__ ~short:() state) ;
              state))
    |> Command.register
         (Command.create
            ~desc:(fun state ->
              Log.printfln "replace fake hash with real hash in TEZOS_PROTOCOL" ;
              state)
            (fun state ->
              let regex =
                let open Re in
                seq [str {|"hash": "|}; [set {|"|}] |> compl |> rep; str {|",|}]
                |> compile
              in
              let by =
                Format.asprintf
                  {|"hash": "%s",|}
                  (State.Target.get_hash ~__LOC__ state)
              in
              let replacements =
                Utils.File.Content.replace_string
                  ~regex
                  ~by
                  (State.Target.Dir.Absolute.get_lib_protocol state
                  // "TEZOS_PROTOCOL")
              in
              if replacements = 0 then
                Log.warning {|No field "hash" found in "TEZOS_PROTOCOL"|}
              else if replacements <> 1 then
                Log.warning {|Multiple fields "hash" found in "TEZOS_PROTOCOL"|} ;
              state))
    |> Command.register
         (Command.Git.commit ~__LOC__ ~no_verify:() (fun state ->
              Format.asprintf
                "src: replace %s hash with %s hash"
                (State.Source.get_name state)
                (State.Target.get_name state)))
  in
  Command.of_fifo
    ~desc:(fun state ->
      Log.printfln "Compute and update hash" ;
      state)
    commands

(** Rename binaries of target protocol (baker and accuser). *)
let rename_binaries =
  let commands =
    Command.FIFO.empty
    |> Command.register
         (Command.create
            ~desc:(fun state ->
              Log.printfln "rename main_*.ml{,i} files of the binaries" ;
              state)
            (fun state ->
              let opts =
                Format.asprintf
                  {|-name main_\*_%s.ml -or -name main_\*_%s.mli|}
                  (State.Source.get_bin_suffix ~__LOC__ state)
                  (State.Source.get_bin_suffix ~__LOC__ state)
              in
              let files =
                Utils.File.find
                  ~error:(error ~__LOC__ state)
                  ~dir:(State.Target.Dir.Absolute.get_src_proto state)
                  ~opts
              in
              let regex =
                let open Re in
                "_" ^ State.Source.get_bin_suffix ~__LOC__ state
                |> str |> compile
              in
              let new_files =
                List.map
                  (Re.replace_string
                     regex
                     ~by:("_" ^ State.Target.get_bin_suffix ~__LOC__ state))
                  files
              in
              List.iter2
                (fun file new_file ->
                  let cmd =
                    Command.Git.git
                      state
                      (Format.asprintf "mv %s %s" file new_file)
                  in
                  let _ =
                    Utils.Process.shell ~error:(error ~__LOC__ state) cmd
                  in
                  ())
                files
                new_files ;
              Log.magenta
                "Files: %a"
                Format.(pp_print_list pp_print_string)
                files ;
              state))
    |> Command.register
         (Command.Git.commit ~__LOC__ ~no_verify:() (fun _state ->
              "src: rename binaries main_*.ml{,i} files"))
  in
  Command.of_fifo
    ~desc:(fun state ->
      Log.printfln "rename binaries" ;
      state)
    commands

(** Replace protocol references of source protocol in target protocol. *)
let replace_protocol_occurences =
  let commands =
    Command.FIFO.empty
    |> Command.register
         (Command.create
            ~desc:(fun state ->
              Log.eprintfln "replace protocol references in lib_protocol code" ;
              state)
            (fun state ->
              let files =
                Utils.File.find
                  ~error:(error ~__LOC__ state)
                  ~dir:(State.Target.Dir.Absolute.get_lib_protocol state)
                  ~opts:"-type f"
              in
              let regex =
                let open Re in
                [
                  State.Source.get_module_name_root ~__LOC__ state
                  |> str |> group ~name:"full";
                  "protocol-" ^ State.Source.get_dune_name ~__LOC__ state
                  |> str |> group ~name:"dash";
                  "protocol-functor-"
                  ^ State.Source.get_dune_name ~__LOC__ state
                  |> str |> group ~name:"functor";
                ]
                |> alt |> compile
              in
              let assoc =
                [
                  ("full", State.Target.get_module_name_root ~__LOC__ state);
                  ( "dash",
                    "protocol-" ^ State.Target.get_dune_name ~__LOC__ state );
                  ( "functor",
                    "protocol-functor-"
                    ^ State.Target.get_dune_name ~__LOC__ state );
                ]
              in
              Utils.File.Content.(
                check_modif_count_all
                  ~warn:(Msg "No protocol references has been updated.")
                  (replace_assoc_all
                     ~error:(error ~__LOC__ state)
                     ~regex
                     ~assoc)
                  files) ;
              state))
    |> Command.register
         (Command.create
            ~desc:(fun state ->
              Log.printfln "Format modified files." ;
              state)
            (fun state ->
              let files =
                Utils.File.find
                  ~error:(error ~__LOC__ state)
                  ~dir:(State.Target.Dir.Absolute.get_lib_protocol state)
                  ~opts:{|-type f -name "*.ml"|}
              in
              let files =
                files
                @ Utils.File.find
                    ~error:(error ~__LOC__ state)
                    ~dir:(State.Target.Dir.Absolute.get_lib_protocol state)
                    ~opts:{|-type f -name "*.mli"|}
              in
              Utils.File.Content.ocamlformat ~error:(error ~__LOC__ state) files ;
              state))
    |> Command.register
         (Command.Git.commit ~__LOC__ ~no_verify:() (fun state ->
              Format.asprintf
                "src: replace %s with %s"
                (State.Source.get_module_name_root ~__LOC__ state)
                (State.Target.get_module_name_root ~__LOC__ state)))
  in
  Command.of_fifo
    ~desc:(fun state ->
      Log.printfln "Replace protocol references in protocol source files" ;
      state)
    commands

(** Add target protocol hash to protocol compiler lib. *)
let add_to_final_protocol_versions =
  let commands =
    Command.FIFO.empty
    |> Command.register
         (Command.create
            ~desc:(fun state ->
              Log.printfln "Add this protocol to the immutable list." ;
              state)
            (fun state ->
              Utils.File.Content.append
                ~content:
                  (Format.asprintf
                     "%s\n"
                     (State.Target.get_hash ~__LOC__ state))
                (State.make_absolute
                   ("src" // "lib_protocol_compiler"
                  // "final_protocol_versions")
                   state) ;
              state))
    |> Command.register
         (Command.Git.commit ~__LOC__ ~no_verify:() (fun _state ->
              "src: add protocol to final_protocol_versions"))
  in
  Command.of_fifo
    ~desc:(fun state ->
      Log.printfln "Add protocom to final_protocol_versions" ;
      state)
    commands

(** Update target protocol README file. *)
let update_readme =
  let commands =
    Command.FIFO.empty
    |> Command.register
         (Command.create
            ~desc:(fun state ->
              Log.printfln "Update README.md" ;
              state)
            (fun state ->
              let regex = Re.(State.Source.get_name state |> str |> compile) in
              let by = State.Target.get_name state in
              Utils.File.Content.(
                check_modif_count
                  ~warn:(Msg "No modification in README.md has been done")
                  (replace_string ~regex ~by)
                  (State.Target.Dir.Absolute.get_src_proto state // "README.md")) ;
              state))
    |> Command.register
         (Command.Git.commit ~__LOC__ ~no_verify:() (fun _state ->
              "src: rename protocol in the README"))
  in
  Command.of_fifo
    ~desc:(fun state ->
      Log.printfln "rename protocol in the README" ;
      state)
    commands

(** Link target protocol in the manifest. *)
let link =
  let commands =
    let file (state : State.t) =
      State.make_absolute ("manifest" // "product_octez.ml") state
    in
    Command.FIFO.empty
    |> Command.register
         (Command.Log.yellow (fun _state ->
              "Linking protocol in the node, client and codec"))
    |> Command.register
         (Command.create
            ~desc:(fun state ->
              Log.printfln "link protocol in the node, client and codec" ;
              state)
            (fun state ->
              let regex =
                let open Re in
                seq
                  [
                    str "let";
                    str " " |> rep1;
                    State.Source.get_name state |> str;
                    Utils.str_of_fmt
                      {| = active (Name.dev "%s")|}
                      (State.Source.get_name state);
                  ]
                |> group |> compile
              in
              let f gp =
                match Re.Group.get_opt gp 1 with
                | None ->
                    Log.error "Group not found in manifest" ;
                    State.exit ~__LOC__ state
                | Some matched ->
                    Format.asprintf
                      {|let _%s = active (Name.dev "%s")@.%s@.|}
                      (State.Target.get_name state)
                      (State.Target.get_name state)
                      matched
              in
              Utils.File.Content.(
                check_modif_count
                  ~warn:(Msg "No modifications in manifest has been done.")
                  (replace_f ~regex ~f)
                  (file state)) ;
              state))
    |> Command.register
         (Command.File.ocamlformat ~__LOC__ (fun state -> [file state]))
    |> Command.register
         (Command.Git.commit ~__LOC__ ~no_verify:() (fun _state ->
              "manifest: link protocol in the node, client and codec"))
  in
  Command.of_fifo
    ~desc:(fun state ->
      Log.printfln "link protocol in manifest" ;
      state)
    commands

(** Execute make -C manifest. *)
let make_manifest =
  let commands =
    Command.FIFO.empty
    |> Command.register (Command.Log.blue (fun _state -> "Make manifest"))
    |> Command.register
         (Command.Shell.create ~__LOC__ (fun _state -> "make -C manifest"))
    |> Command.register
         (Command.create
            ~desc:(fun state ->
              Log.printfln "Format devtools ocaml source files" ;
              state)
            (fun state ->
              let files =
                Utils.File.find
                  ~error:(error ~__LOC__ state)
                  ~dir:(State.make_absolute "devtools" state)
                  ~opts:"-name '*.ml'"
              in
              Utils.File.Content.ocamlformat ~error:(error ~__LOC__ state) files ;
              state))
    |> Command.register
         (Command.Git.commit ~__LOC__ ~no_verify:() (fun _state ->
              "manifest: make manifest"))
  in
  Command.of_fifo
    ~desc:(fun state ->
      Log.printfln "make manifest" ;
      state)
    commands

let copy_source =
  let commands =
    Command.FIFO.empty |> Command.register raw_copy
    |> Command.register set_current_version
    |> Command.register
         (Command.Log.printfln (fun state -> State.Source.get_name state))
    |> Command.register add_predecessor
    |>
    (* All modifications that impact target protocol hash has been done, it
          is time to compute the target protocol hash. *)
    Command.register compute_and_replace_hash
    |> Command.register rename_binaries
    |> Command.register replace_protocol_occurences
    |> Command.register add_to_final_protocol_versions
    |> Command.register update_readme
    |> Command.register link
    |> Command.register make_manifest
  in
  Command.of_fifo commands

let update_protocol_tests =
  let commands = Command.FIFO.empty in
  (* TODO *) Command.of_fifo commands

let update_source =
  let commands = Command.FIFO.empty in
  (* TODO *) Command.of_fifo commands

let update_tezt_tests =
  let commands = Command.FIFO.empty in
  (* TODO *) Command.of_fifo commands

let misc_updates =
  let commands = Command.FIFO.empty in
  (* TODO *) Command.of_fifo commands

let generate_doc =
  let commands = Command.FIFO.empty in
  (* TODO *) Command.of_fifo commands

let snapshot_protocol =
  let commands =
    Command.FIFO.empty
    |> Command.register copy_source
    |> Command.register update_protocol_tests
    |> Command.register update_source
    |> Command.register update_tezt_tests
    |> Command.register misc_updates
    |> Command.register generate_doc
  in
  Command.of_fifo commands

module Stabilise = struct
  let run git_dir source target =
    check_proto_name source protocol_pattern ;
    check_proto_name target protocol_pattern ;
    let state =
      State.create
        ~git_dir
        ~source
        ~target
        ~target_pattern:protocol_pattern.string
    in
    let commands =
      Command.FIFO.empty
      |> Command.register check_arguments
      |> Command.register clean_tmp_dir
      |> Command.register
           (Command.create
              ~desc:(fun state ->
                Log.printfln
                  "Will stabilise protocol from '%s'"
                  (State.Source.Dir.Relative.get_src_proto state) ;
                state)
              (fun state ->
                Log.blue
                  "Will stabilise protocol from '%s'"
                  (State.Source.Dir.Relative.get_src_proto state) ;
                state))
      |> Command.register snapshot_protocol
    in
    Log.cyan "Describe commands" ;
    let _state = Command.FIFO.describe commands state in
    Log.printfln "" ;
    Log.cyan "Execute commands" ;
    let _state = Command.FIFO.execute commands state in
    ()
end

module Snapshot = struct
  let target_pattern =
    let string = {|[a-z]+_[0-9][0-9][0-9]|} in
    {string; regex = Re.Perl.("^" ^ string ^ "$" |> re |> compile)}

  let check_version_coherency : Command.t =
    Command.create
      ~desc:(fun state ->
        Log.printfln
          "Extract version corresponding to label from tezt and check \
           coherency with arguments" ;
        state)
      (fun state ->
        let pattern =
          {
            string = State.Source.get_constructor state ^ " -> [0-9]+";
            regex =
              (let open Re in
               seq
                 [
                   State.Source.get_constructor state |> str;
                   str " -> ";
                   digit |> rep1 |> group;
                 ]
               |> compile);
          }
        in
        let groups =
          Utils.File.Content.search
            ~regex:pattern.regex
            (tezt_protocol_file state)
        in
        let expected_version =
          match groups with
          | [group] -> (
              match Re.Group.get_opt group 1 with
              | Some expected_version -> expected_version
              | None ->
                  Log.error
                    "Cannot parse protocol version from %s."
                    (tezt_protocol_file state) ;
                  Log.error
                    "This file should contain a line matching '%a'."
                    pp_pattern
                    pattern ;
                  Stdlib.exit 1)
          | _ ->
              Log.error
                "Cannot parse protocol version from %s."
                (tezt_protocol_file state) ;
              Log.error
                "This file should contain a line matching '%a'."
                pp_pattern
                pattern ;
              Stdlib.exit 1
        in
        let expected_version =
          match int_of_string_opt expected_version with
          | Some version -> version
          | None ->
              Log.error
                "Cannot parse protocol version from %s"
                (tezt_protocol_file state) ;
              Log.error
                "This file should contain a line matching '%a'."
                pp_pattern
                pattern ;
              Stdlib.exit 1
        in
        let target_version = State.Target.get_version ~__LOC__ state in
        if expected_version <> target_version then (
          Log.error "Wrong protocol version: %03d" target_version ;
          Log.error "Expected version: %03d" expected_version ;
          Stdlib.exit 1) ;
        state)

  let run git_dir source target =
    check_proto_name source protocol_pattern ;
    check_proto_name target target_pattern ;
    let state =
      State.create
        ~git_dir
        ~source
        ~target
        ~target_pattern:target_pattern.string
    in
    let commands =
      Command.FIFO.empty
      |> Command.register check_arguments
      |> Command.register check_version_coherency
      |> Command.register clean_tmp_dir
      |> Command.register
           (Command.create
              ~desc:(fun state ->
                Log.printfln
                  "Will snapshot protocol from '%s'"
                  (State.Source.Dir.Relative.get_src_proto state) ;
                state)
              (fun state ->
                Log.blue
                  "Will snapshot protocol from '%s'"
                  (State.Source.Dir.Relative.get_src_proto state) ;
                state))
      |> Command.register snapshot_protocol
    in
    Log.cyan "Describe commands" ;
    let _state = Command.FIFO.describe commands state in
    Log.printfln "" ;
    Log.cyan "Execute commands" ;
    let _state = Command.FIFO.execute commands state in
    ()
end

module Hash = struct
  let run _git_dir _from = Log.warning "Updating hash is under development"
end
