(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Commands *)

let show (args : Shared_arg.t) =
  let run =
    let open Lwt_result_syntax in
    let*! () = Tezos_base_unix.Internal_event_unix.init_with_defaults () in
    if not @@ Sys.file_exists args.config_file then
      Format.eprintf
        "@[<v>@[<v 9>Warning: no configuration file found at %s@,\
         displaying the default configuration@]@]@."
        args.config_file ;
    let* config = Shared_arg.read_and_patch_config_file args in
    print_endline @@ Config_file.to_string config ;
    return_unit
  in
  Shared_arg.process_command run

let reset (args : Shared_arg.t) =
  let run =
    let open Lwt_result_syntax in
    let*! () = Tezos_base_unix.Internal_event_unix.init_with_defaults () in
    if Sys.file_exists args.config_file then
      Format.eprintf
        "Ignoring previous configuration file: %s.@."
        args.config_file ;
    let* current_config = Shared_arg.read_config_file args in
    (* Here we set the network of the default config to the current network
       to prevent overriding it. *)
    let* default_config =
      Shared_arg.patch_network current_config.blockchain_network
    in
    let* config = Shared_arg.patch_config ~cfg:default_config args in
    let* () = Config_validation.check config in
    Config_file.write args.config_file config
  in
  Shared_arg.process_command run

let init (args : Shared_arg.t) =
  let run =
    let open Lwt_result_syntax in
    let*! () = Tezos_base_unix.Internal_event_unix.init_with_defaults () in
    if Sys.file_exists args.config_file then
      failwith
        "Pre-existing configuration file at %s, use `reset`."
        args.config_file
    else
      let* config =
        Shared_arg.read_and_patch_config_file ~may_override_network:true args
      in
      let* () = Config_validation.check config in
      let* () = Config_file.write args.config_file config in
      let default = if args.network = None then " default" else "" in
      let alias =
        match config.blockchain_network.alias with
        | None ->
            (* Cannot happen, as --network cannot take custom networks as arguments. *)
            ""
        | Some alias -> ": " ^ alias
      in
      Format.eprintf
        "Created %s for%s network%s.@."
        args.config_file
        default
        alias ;
      return_unit
  in
  Shared_arg.process_command run

let update (args : Shared_arg.t) =
  let run =
    let open Lwt_result_syntax in
    let*! () = Tezos_base_unix.Internal_event_unix.init_with_defaults () in
    if not (Sys.file_exists args.config_file) then
      failwith
        "Missing configuration file at %s. Use `%s config init [options]` to \
         generate a new file"
        args.config_file
        Sys.argv.(0)
    else
      let* config = Shared_arg.read_and_patch_config_file args in
      let* () = Config_validation.check config in
      Config_file.write args.config_file config
  in
  Shared_arg.process_command run

let validate (args : Shared_arg.t) =
  let run =
    let open Lwt_result_syntax in
    let*! () = Tezos_base_unix.Internal_event_unix.init_with_defaults () in
    if not (Sys.file_exists args.config_file) then
      Format.eprintf
        "@[<v>@[<v 9>Warning: no configuration file found at %s@,\
         validating the default configuration@]@]@."
        args.config_file ;
    let* config = Shared_arg.read_and_patch_config_file args in
    let*! r = Config_validation.check config in
    match r with
    (* Here we do not consider the node configuration file
       being invalid as a failure. *)
    | Error (Config_validation.Invalid_node_configuration :: _) | Ok () ->
        return_unit
    | err -> Lwt.return err
  in
  Shared_arg.process_command run

(** Main *)
module Term = struct
  let cmds =
    let open Cmdliner in
    [
      Cmd.v
        (Cmd.info
           ~doc:
             "reads, parses and displays Tezos current config file. Use this \
              command to see exactly what config file will be used by Tezos. \
              If additional command-line arguments are provided, the displayed \
              configuration will be amended accordingly. This is the default \
              operation"
           "show")
        Term.(ret (const show $ Shared_arg.Term.args));
      Cmd.v
        (Cmd.info
           ~doc:
             "will overwrite the current configuration file with a factory \
              default one. If additional command-line arguments are provided, \
              they will amend the generated file. It assumes that a \
              configuration file already exists and will abort otherwise"
           "reset")
        Term.(ret (const reset $ Shared_arg.Term.args));
      Cmd.v
        (Cmd.info
           ~doc:
             "is like reset but assumes that no configuration file is present \
              and will abort otherwise"
           "init")
        Term.(ret (const init $ Shared_arg.Term.args));
      Cmd.v
        (Cmd.info
           ~doc:
             "is the main option to edit the configuration file of Tezos. It \
              will parse command line arguments and add or replace \
              corresponding entries in the Tezos configuration file"
           "update")
        Term.(ret (const update $ Shared_arg.Term.args));
      Cmd.v
        (Cmd.info
           ~doc:
             "verifies that the configuration file parses correctly and \
              performs some sanity checks on its values"
           "validate")
        Term.(ret (const validate $ Shared_arg.Term.args));
    ]
end

module Manpage = struct
  let command_description =
    "The $(b,config) command is meant to inspect and amend the configuration \
     of the Tezos node. This command is complementary to manually editing the \
     tezos node configuration file. Its arguments are a subset of the $(i,run) \
     command ones."

  let options =
    let schema = Data_encoding.Json.schema Config_file.encoding in
    let schema = Format.asprintf "@[%a@]" Json_schema.pp schema in
    let schema = String.concat "\\$" (String.split_no_empty '$' schema) in
    [`S "OPTIONS"; `P "All options available in the config file"; `Pre schema]

  let man = Shared_arg.Manpage.args @ options @ Shared_arg.Manpage.bugs

  let info = Cmdliner.Cmd.info ~doc:"Manage node configuration" ~man "config"
end

let cmd = Cmdliner.Cmd.group Manpage.info Term.cmds
