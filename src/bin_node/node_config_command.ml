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

let show (args : Node_shared_arg.t) =
  let open Lwt_result_syntax in
  let*! () = Internal_event_unix.init () in
  if not @@ Sys.file_exists args.config_file then
    Format.eprintf
      "@[<v>@[<v 9>Warning: no configuration file found at %s@,\
       displaying the default configuration@]@]@."
      args.config_file ;
  let* config = Node_shared_arg.read_and_patch_config_file args in
  print_endline @@ Node_config_file.to_string config ;
  return_unit

let reset (args : Node_shared_arg.t) =
  let open Lwt_result_syntax in
  let*! () = Internal_event_unix.init () in
  if Sys.file_exists args.config_file then
    Format.eprintf
      "Ignoring previous configuration file: %s.@."
      args.config_file ;
  let* config = Node_shared_arg.read_and_patch_config_file args in
  let* () = Node_config_validation.check config in
  Node_config_file.write args.config_file config

let init (args : Node_shared_arg.t) =
  let open Lwt_result_syntax in
  let*! () = Internal_event_unix.init () in
  if Sys.file_exists args.config_file then
    failwith
      "Pre-existing configuration file at %s, use `reset`."
      args.config_file
  else
    let* config =
      Node_shared_arg.read_and_patch_config_file ~may_override_network:true args
    in
    let* () = Node_config_validation.check config in
    let* () = Node_config_file.write args.config_file config in
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

let update (args : Node_shared_arg.t) =
  let open Lwt_result_syntax in
  let*! () = Internal_event_unix.init () in
  if not (Sys.file_exists args.config_file) then
    failwith
      "Missing configuration file at %s. Use `%s config init [options]` to \
       generate a new file"
      args.config_file
      Sys.argv.(0)
  else
    let* config = Node_shared_arg.read_and_patch_config_file args in
    let* () = Node_config_validation.check config in
    Node_config_file.write args.config_file config

let validate (args : Node_shared_arg.t) =
  let open Lwt_result_syntax in
  let*! () = Internal_event_unix.init () in
  if not (Sys.file_exists args.config_file) then
    Format.eprintf
      "@[<v>@[<v 9>Warning: no configuration file found at %s@,\
       validating the default configuration@]@]@."
      args.config_file ;
  let* config = Node_shared_arg.read_and_patch_config_file args in
  let*! r = Node_config_validation.check config in
  match r with
  (* Here we do not consider the node configuration file
     being invalid as a failure. *)
  | Error (Node_config_validation.Invalid_node_configuration :: _) | Ok () ->
      return_unit
  | err -> Lwt.return err

(** Main *)
module Term = struct
  type subcommand = Show | Reset | Init | Update | Validate

  let process subcommand args =
    let res =
      match subcommand with
      | Show -> show args
      | Reset -> reset args
      | Init -> init args
      | Update -> update args
      | Validate -> validate args
    in
    match Lwt_main.run @@ Lwt_exit.wrap_and_exit res with
    | Ok () -> `Ok ()
    | Error err -> `Error (false, Format.asprintf "%a" pp_print_trace err)

  let subcommand_arg =
    let parser = function
      | "show" -> `Ok Show
      | "reset" -> `Ok Reset
      | "init" -> `Ok Init
      | "update" -> `Ok Update
      | "validate" -> `Ok Validate
      | s -> `Error ("invalid argument: " ^ s)
    and printer ppf = function
      | Show -> Format.fprintf ppf "show"
      | Reset -> Format.fprintf ppf "reset"
      | Init -> Format.fprintf ppf "init"
      | Update -> Format.fprintf ppf "update"
      | Validate -> Format.fprintf ppf "validate"
    in
    let open Cmdliner.Arg in
    let doc =
      "Operation to perform. Possible values: $(b,show), $(b,reset), \
       $(b,init), $(b,update), $(b,validate)."
    in
    value & pos 0 (parser, printer) Show & info [] ~docv:"OPERATION" ~doc

  let term =
    let open Cmdliner.Term in
    ret (const process $ subcommand_arg $ Node_shared_arg.Term.args)
end

module Manpage = struct
  let command_description =
    "The $(b,config) command is meant to inspect and amend the configuration \
     of the Tezos node. This command is complementary to manually editing the \
     tezos node configuration file. Its arguments are a subset of the $(i,run) \
     command ones."

  let description =
    [
      `S "DESCRIPTION";
      `P (command_description ^ " Several operations are possible: ");
      `P
        "$(b,show) reads, parses and displays Tezos current config file. Use \
         this command to see exactly what config file will be used by Tezos. \
         If additional command-line arguments are provided, the displayed \
         configuration will be amended accordingly. This is the default \
         operation.";
      `P
        "$(b,reset) will overwrite the current configuration file with a \
         factory default one. If additional command-line arguments are \
         provided, they will amend the generated file. It assumes that a \
         configuration file already exists and will abort otherwise.";
      `P
        "$(b,init) is like reset but assumes that no configuration file is \
         present and will abort otherwise.";
      `P
        "$(b,update) is the main option to edit the configuration file of \
         Tezos. It will parse command line arguments and add or replace \
         corresponding entries in the Tezos configuration file.";
      `P
        "$(b,validate) verifies that the configuration file parses correctly \
         and performs some sanity checks on its values.";
    ]

  let options =
    let schema = Data_encoding.Json.schema Node_config_file.encoding in
    let schema = Format.asprintf "@[%a@]" Json_schema.pp schema in
    let schema = String.concat "\\$" (String.split '$' schema) in
    [`S "OPTIONS"; `P "All options available in the config file"; `Pre schema]

  let man =
    description @ Node_shared_arg.Manpage.args @ options
    @ Node_shared_arg.Manpage.bugs

  let info = Cmdliner.Term.info ~doc:"Manage node configuration" ~man "config"
end

let cmd = (Term.term, Manpage.info)
