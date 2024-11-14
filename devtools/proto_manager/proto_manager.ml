(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let ( // ) = Filename.concat

let tmp_proto_snapshot_dir =
  Filename.get_temp_dir_name () // "tezos_proto_snapshot"

let tezt_protocol_file (state : State.t) =
  state.git_dir // "tezt" // "lib_tezos" // "protocol.ml"

type pattern = {string : string; regex : Re.re}

let pp_pattern fmt {string; _} = Format.pp_print_string fmt string

let protocol_pattern =
  let string = {|[a-z]+[0-9]*|} in
  {string; regex = Re.Perl.(compile @@ re ("^" ^ string ^ "$"))}

let check_proto_name get_proto_name pattern =
  Command.
    {
      describe =
        (fun state ->
          Log.printfln
            "Check that the protocol name '%s' is of form '%a'"
            (get_proto_name state)
            pp_pattern
            pattern ;
          state);
      execute =
        (fun state ->
          if not (Re.execp pattern.regex (get_proto_name state)) then (
            Log.error
              "Protocol name '%s' should be of the form '%a'"
              (get_proto_name state)
              pp_pattern
              pattern ;
            Stdlib.exit 1) ;
          state);
    }

let check_arguments target_pattern =
  let open Command.FIFO in
  let commands =
    empty
    |> (* Check source and target *)
    register
      {
        describe =
          (fun state ->
            Log.printfln "check that protocol source and target are different" ;
            state);
        execute =
          (fun state ->
            if state.protocol_source = state.protocol_target then (
              Log.error "protocol source and target should be different" ;
              Stdlib.exit 1) ;
            state);
      }
    |> register
         (check_proto_name
            (fun state -> state.protocol_source)
            protocol_pattern)
    |> register
         (check_proto_name (fun state -> state.protocol_target) target_pattern)
    |> register
         (Command.File.check_exists (fun state ->
              (state.git_dir // "src" // "proto_") ^ state.protocol_source))
    |> register
         (Command.File.check_exists (fun state ->
              state.git_dir // "docs" // state.protocol_source))
    |> register
         (Command.File.check_not_exist (fun state ->
              state.git_dir // "src"
              // Format.asprintf "proto_%a" State.pp_label state.target_label))
    |> register
         (Command.File.check_not_exist (fun state ->
              state.git_dir // "docs"
              // Format.asprintf "%a" State.pp_label_name state.target_label))
    |> (* Check git directory *)
    register
      (Command.File.check_exists
         ~error_msg:(fun _state ->
           "octez-protocol-compiler not found, compile it")
         (fun state -> state.git_dir // "octez-protocol-compiler"))
    |> register Command.Git.check_no_uncommitted_changes
  in
  to_command
    ~desc:(fun state ->
      Log.printfln "Check the arguments" ;
      state)
    commands

let clean_tmp_dir =
  Command.Shell.create ~__LOC__ (fun _state ->
      "rm -rf " ^ tmp_proto_snapshot_dir)

let copy_source =
  let commands = Command.FIFO.empty in
  (* TODO *) Command.FIFO.to_command commands

let update_protocol_tests =
  let commands = Command.FIFO.empty in
  (* TODO *) Command.FIFO.to_command commands

let update_source =
  let commands = Command.FIFO.empty in
  (* TODO *) Command.FIFO.to_command commands

let update_tezt_tests =
  let commands = Command.FIFO.empty in
  (* TODO *) Command.FIFO.to_command commands

let misc_updates =
  let commands = Command.FIFO.empty in
  (* TODO *) Command.FIFO.to_command commands

let generate_doc =
  let commands = Command.FIFO.empty in
  (* TODO *) Command.FIFO.to_command commands

let snapshot_protocol =
  let open Command.FIFO in
  let commands =
    empty |> register copy_source
    |> register update_protocol_tests
    |> register update_source |> register update_tezt_tests
    |> register misc_updates |> register generate_doc
  in
  to_command commands

module Stabilise = struct
  let run git_dir protocol_source protocol_target =
    let open Command.FIFO in
    let state =
      State.create
        ~git_dir
        ~protocol_source
        ~protocol_target
        ~target_pattern:protocol_pattern.string
    in
    let commands =
      Command.FIFO.empty
      |> register (check_arguments protocol_pattern)
      |> register clean_tmp_dir
      |> register
           {
             describe =
               (fun state ->
                 Log.printfln
                   "Will stabilise protocol from 'src/proto_%s'"
                   state.protocol_source ;
                 state);
             execute =
               (fun state ->
                 Log.blue
                   "Will stabilise protocol from 'src/proto_%s'"
                   state.protocol_source ;
                 state);
           }
      |> register snapshot_protocol
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
    {string; regex = Re.Perl.(compile @@ re ("^" ^ string ^ "$"))}

  let check_version_coherency =
    Command.
      {
        describe =
          (fun state ->
            Log.printfln
              "Extract version corresponding to label from tezt and check \
               coherency with arguments" ;
            state);
        execute =
          (fun state ->
            let target_label =
              match state.target_label with
              | Snapshot label -> label
              | _ ->
                  Log.error "Wrong target label type." ;
                  Stdlib.exit 1
            in
            let pattern =
              {
                string = state.capitalize_label ^ " -> [0-9]+";
                regex =
                  Re.(
                    compile
                      (seq
                         [
                           str state.capitalize_label;
                           str " -> ";
                           group (rep1 digit);
                         ]));
              }
            in
            let in_channel = open_in (tezt_protocol_file state) in
            let content = In_channel.input_all in_channel in
            In_channel.close in_channel ;
            let group = Re.exec_opt pattern.regex content in
            let expected_version =
              match group with
              | None ->
                  Log.error
                    "Cannot parse protocol version from %s."
                    (tezt_protocol_file state) ;
                  Log.error
                    "This file should contain a line matching '%a'."
                    pp_pattern
                    pattern ;
                  Stdlib.exit 1
              | Some group -> (
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
            if expected_version <> target_label.version then (
              Log.error "Wrong protocol version: %03d" target_label.version ;
              Log.error "Expected version: %03d" expected_version ;
              Stdlib.exit 1) ;
            state);
      }

  let run git_dir protocol_source protocol_target =
    let open Command.FIFO in
    let state =
      State.create
        ~git_dir
        ~protocol_source
        ~protocol_target
        ~target_pattern:target_pattern.string
    in
    let commands =
      Command.FIFO.empty
      |> register (check_arguments target_pattern)
      |> register check_version_coherency
      |> register clean_tmp_dir
      |> register
           {
             describe =
               (fun state ->
                 Log.printfln
                   "Will snapshot protocol from 'src/proto_%s'"
                   state.protocol_source ;
                 state);
             execute =
               (fun state ->
                 Log.blue
                   "Will snapshot protocol from 'src/proto_%s'"
                   state.protocol_source ;
                 state);
           }
      |> register snapshot_protocol
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
