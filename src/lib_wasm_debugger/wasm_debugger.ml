(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Make (Wasm : Wasm_utils_intf.S) = struct
  module Commands = Commands.Make (Wasm)

  let parse_binary_module name module_ =
    let bytes = Tezos_lazy_containers.Chunked_byte_vector.of_string module_ in
    Tezos_webassembly_interpreter.Decode.decode ~allow_floats:false ~name ~bytes

  (* [typecheck_module module_ast] runs the typechecker on the module, which is
     not done by the PVM. *)
  let typecheck_module module_ =
    Repl_helpers.trap_exn (fun () ->
        Tezos_webassembly_interpreter.Valid.check_module module_)

  (* [import_pvm_host_functions ~version ()] registers the host
     functions of the PVM. *)
  let import_pvm_host_functions ~version () =
    let lookup name =
      Lwt.return (Tezos_scoru_wasm.Host_funcs.lookup ~version name)
    in
    Repl_helpers.trap_exn (fun () ->
        Lwt.return
          (Tezos_webassembly_interpreter.Import.register
             ~module_name:"smart_rollup_core"
             lookup))

  (* [link module_ast] checks a module actually uses the host functions with their
     correct type, outside of the PVM. *)
  let link module_ =
    Repl_helpers.trap_exn (fun () ->
        Tezos_webassembly_interpreter.Import.link module_)

  let set_durable_value tree key value =
    let open Lwt_syntax in
    let open Tezos_scoru_wasm.Durable in
    let* durable_storage = Wasm.wrap_as_durable_storage tree in
    let durable = Tezos_scoru_wasm.Durable.of_storage_exn durable_storage in
    let key = key_of_string_exn key in
    let* durable = set_value_exn durable key value in
    let durable_storage = Tezos_scoru_wasm.Durable.to_storage durable in
    let wrapped_tree = Durable_storage.to_tree_exn durable_storage in
    Wasm.Tree_encoding_runner.encode
      Tezos_tree_encoding.(scope ["durable"] wrapped_tree)
      wrapped_tree
      tree

  let handle_installer_config_instr durable
      Octez_smart_rollup.Installer_config.(Set {value; to_}) =
    let open Tezos_scoru_wasm.Durable in
    let key = key_of_string_exn to_ in
    set_value_exn durable key value

  let handle_installer_config installer_config tree =
    let open Lwt_syntax in
    let* durable_storage = Wasm.wrap_as_durable_storage tree in
    let durable = Tezos_scoru_wasm.Durable.of_storage_exn durable_storage in
    let* durable =
      List.fold_left_s handle_installer_config_instr durable installer_config
    in
    let durable_storage = Tezos_scoru_wasm.Durable.to_storage durable in
    let wrapped_tree = Durable_storage.to_tree_exn durable_storage in
    Wasm.Tree_encoding_runner.encode
      Tezos_tree_encoding.(scope ["durable"] wrapped_tree)
      wrapped_tree
      tree

  (* Starting point of the module after reading the kernel file: parsing,
     typechecking and linking for safety before feeding kernel to the PVM, then
     installation into a tree for the PVM interpreter. *)
  let handle_module ?installer_config ?tree version binary name module_ =
    let open Lwt_result_syntax in
    let open Tezos_protocol_alpha.Protocol.Alpha_context.Sc_rollup in
    let* ast =
      Repl_helpers.trap_exn (fun () ->
          if binary then parse_binary_module name module_
          else Lwt.return (Wasm_utils.parse_module module_))
    in
    let* () = typecheck_module ast in
    let* () = import_pvm_host_functions ~version () in
    let* _ = link ast in
    let*! tree =
      Wasm.initial_tree
        ~version
        ?tree
        ~ticks_per_snapshot:(Z.to_int64 Wasm_2_0_0PVM.ticks_per_snapshot)
        ~outbox_validity_period:Wasm_2_0_0PVM.outbox_validity_period
        ~outbox_message_limit:Wasm_2_0_0PVM.outbox_message_limit
        ~from_binary:binary
        module_
    in
    let*! tree =
      match installer_config with
      | None -> Lwt.return tree
      | Some installer_config -> handle_installer_config installer_config tree
    in
    let*! tree = Wasm.eval_until_input_requested tree in
    return tree

  let start ?installer_config ?tree version file =
    let open Lwt_result_syntax in
    let module_name = Filename.(file |> basename |> chop_extension) in
    let* buffer, binary =
      if Filename.(check_suffix file ".hex") then
        let*! content = Repl_helpers.read_file file in
        let*? content =
          match Hex.to_string (`Hex content) with
          | Some content -> Ok content
          | None -> error_with "%S is not a valid hexadecimal file" file
        in
        return (content, true)
      else
        let*! content = Repl_helpers.read_file file in
        let*? binary =
          if Filename.check_suffix file ".wasm" then Ok true
          else if Filename.check_suffix file ".wast" then Ok false
          else error_with "Kernels should have .wasm or .wast file extension"
        in
        return (content, binary)
    in
    handle_module ?installer_config ?tree version binary module_name buffer

  (* REPL main loop: reads an input, does something out of it, then loops. *)
  let repl tree inboxes level config =
    let open Lwt_result_syntax in
    let rec loop tree inboxes level =
      let*! () = Lwt_io.printf "> " in
      let*! input = Option.catch_s (fun () -> Lwt_io.read_line Lwt_io.stdin) in
      match input with
      | Some command ->
          let* ctx =
            Commands.handle_command command config tree inboxes level
          in
          Option.fold_f
            ~none:(fun () -> return tree)
            ~some:(fun (tree, inboxes, level) -> loop tree inboxes level)
            ctx
      | None -> return tree
    in
    loop tree (List.to_seq inboxes) level

  let file_parameter =
    Tezos_clic.parameter (fun _ filename ->
        Repl_helpers.(trap_exn (fun () -> read_file filename)))

  let dir_parameter =
    Tezos_clic.parameter (fun _ dirpath ->
        if Sys.file_exists dirpath && Sys.is_directory dirpath then
          Lwt.return_ok dirpath
        else Error_monad.failwith "%s is not a valid directory" dirpath)

  let wasm_parameter =
    Tezos_clic.parameter (fun _ filename ->
        if Sys.file_exists filename then Lwt_result.return filename
        else Error_monad.failwith "%s is not a valid file" filename)

  let wasm_arg =
    let open Tezos_clic in
    arg
      ~doc:"kernel file"
      ~long:"kernel"
      ~placeholder:"kernel.wasm"
      wasm_parameter

  let input_arg =
    let open Tezos_clic in
    arg
      ~doc:"input file"
      ~long:"inputs"
      ~placeholder:"inputs.json"
      file_parameter

  let installer_config_parameter =
    let open Lwt_result_syntax in
    Tezos_clic.parameter (fun _ filename ->
        let* kind =
          if Filename.check_suffix filename ".yaml" then return `Yaml
          else if Filename.check_suffix filename ".json" then return `Json
          else Error_monad.failwith "Expecting either a .yaml of .json file"
        in
        let+ content = Repl_helpers.(trap_exn (fun () -> read_file filename)) in
        (kind, content))

  let installer_config_arg =
    let open Tezos_clic in
    arg
      ~doc:"installer configuration file"
      ~long:"installer-config"
      ~placeholder:"installer-config.yaml"
      installer_config_parameter

  let rollup_parameter =
    let open Lwt_result_syntax in
    Tezos_clic.(
      parameter (fun _ hash ->
          let hash_opt =
            Tezos_protocol_alpha.Protocol.Alpha_context.Sc_rollup.Address
            .of_b58check_opt
              hash
          in
          match hash_opt with
          | Some hash -> return hash
          | None ->
              failwith
                "Parameter '%s' is an invalid smart rollup address encoded in \
                 a base58 string."
                hash))

  let rollup_arg =
    let open Tezos_clic in
    arg
      ~doc:
        (Format.asprintf
           "The rollup address representing the current kernel. It is used on \
            the reveal metadata channel and as the default destination for \
            internal messages. If absent, it defaults to `%a`."
           Tezos_protocol_alpha.Protocol.Alpha_context.Sc_rollup.Address.pp
           Config.default_destination)
      ~long:"rollup"
      ~placeholder:"rollup address"
      rollup_parameter

  let preimage_directory_arg =
    let open Tezos_clic in
    arg
      ~doc:
        (Format.sprintf
           "Directory where the preimages can be read. If not specified, it \
            defaults to `%s`."
           Config.default_preimage_directory)
      ~long:"preimage-dir"
      ~placeholder:"preimage-dir"
      dir_parameter

  let dal_pages_directory_arg =
    let open Tezos_clic in
    arg
      ~doc:
        (Format.sprintf
           "Directory where the DAL pages can be read. If not specified, it \
            defaults to `%s`."
           Config.default_dal_pages_directory)
      ~long:"dal-pages-dir"
      ~placeholder:"dal-pages-dir"
      dir_parameter

  let version_parameter =
    Tezos_clic.parameter (fun _ v ->
        let open Tezos_scoru_wasm.Wasm_pvm_state in
        match Data_encoding.Binary.of_string_opt version_encoding v with
        | Some v -> Lwt_result_syntax.return v
        | None ->
            Error_monad.failwith
              "%s is not a valid WASM PVM version. Expected one of: %a."
              v
              Format.(
                pp_print_list
                  ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
                  (fun fmt str -> fprintf fmt "'%s'" str))
              (List.map fst versions))

  let version_arg =
    let open Tezos_clic in
    arg
      ~doc:"The initial version of the WASM PVM"
      ~short:'p'
      ~long:"pvm-version"
      ~placeholder:"VERSION"
      version_parameter

  let no_kernel_debug_flag =
    let open Tezos_clic in
    switch ~doc:"Hides the kernel debug messages." ~long:"no-kernel-debug" ()

  let flamecharts_directory_arg =
    let open Tezos_clic in
    arg
      ~doc:
        (Format.sprintf
           "Directory where the profiler output its flamecharts. If not \
            specified, it defaults to `%s`."
           Config.default_flamecharts_directory)
      ~long:"flamecharts-dir"
      ~placeholder:"flamecharts-dir"
      dir_parameter

  let plugins_parameter =
    Tezos_clic.parameter (fun _ filenames ->
        let filenames = String.split_on_char ',' filenames in
        List.map_es
          (fun filename ->
            if Sys.file_exists filename then Lwt_result.return filename
            else Error_monad.failwith "%s is not a valid file" filename)
          filenames)

  let plugins_arg =
    let open Tezos_clic in
    arg
      ~doc:"The list of plugins separated by commas"
      ~long:"plugins"
      ~placeholder:"plugin1.cmxs,plugin2.cmxs"
      plugins_parameter

  let global_options =
    Tezos_clic.(
      args10
        wasm_arg
        input_arg
        rollup_arg
        preimage_directory_arg
        dal_pages_directory_arg
        version_arg
        no_kernel_debug_flag
        plugins_arg
        installer_config_arg
        flamecharts_directory_arg)

  let handle_plugin_file f =
    try Dynlink.loadfile f with
    | Dynlink.Error err ->
        Format.printf
          "Loading plugin `%s` failed with\n%s \n%!"
          f
          (Dynlink.error_message err)
    | _ -> Format.printf "Loading the plugin %s failed\n%!" f

  let handle_plugins files = List.iter handle_plugin_file files

  let dispatch args =
    let open Lwt_result_syntax in
    let* ( ( wasm_file,
             inputs,
             rollup_arg,
             preimage_directory,
             dal_pages_directory,
             version,
             no_kernel_debug_flag,
             plugins,
             installer_config,
             flamecharts_directory ),
           _ ) =
      Tezos_clic.parse_global_options global_options () args
    in
    let version =
      Option.value
        ~default:
          Tezos_protocol_alpha.Protocol.Sc_rollup_wasm.V2_0_0.current_version
        version
    in
    let config =
      Config.config
        ?destination:rollup_arg
        ?preimage_directory
        ?dal_pages_directory
        ?flamecharts_directory
        ~kernel_debug:(not no_kernel_debug_flag)
        ()
    in
    let*? wasm_file =
      match wasm_file with
      | Some wasm_file -> Ok wasm_file
      | None -> error_with "A kernel file must be provided"
    in
    let parse_json_config content =
      match Data_encoding.Json.from_string content with
      | Ok json -> (
          try
            Ok
              (Data_encoding.Json.destruct
                 Octez_smart_rollup.Installer_config.encoding
                 json)
          with exn ->
            error_with
              "Config is invalid:\n%a"
              (Data_encoding.Json.print_error ?print_unknown:None)
              exn)
      | Error msg -> error_with "Config is not a valid JSON: %s" msg
    in
    let*? installer_config =
      Option.map_e
        (function
          | `Yaml, content ->
              Octez_smart_rollup.Installer_config.parse_yaml content
          | `Json, content -> parse_json_config content)
        installer_config
    in
    let* tree = start ?installer_config version wasm_file in
    let* inboxes =
      match inputs with
      | Some inputs -> Messages.parse_inboxes inputs config
      | None -> return_nil
    in
    Option.iter handle_plugins plugins ;
    let+ _tree = repl tree inboxes 0l config in
    ()

  let main () =
    ignore
      Tezos_clic.(
        setup_formatter
          Format.std_formatter
          (if Unix.isatty Unix.stdout then Ansi else Plain)
          Short) ;
    let args = Array.to_list Sys.argv |> List.tl |> Option.value ~default:[] in
    let result = Lwt_main.run (dispatch args) in
    match result with
    | Ok _ -> ()
    | Error [Tezos_clic.Version] ->
        let version = Tezos_version_value.Bin_version.octez_version_string in
        Format.printf "%s\n" version ;
        exit 0
    | Error [Tezos_clic.Help command] ->
        Tezos_clic.usage
          Format.std_formatter
          ~executable_name:(Filename.basename Sys.executable_name)
          ~global_options
          (match command with None -> [] | Some c -> [c]) ;
        exit 0
    | Error e ->
        Format.eprintf
          "%a\n%!"
          Tezos_clic.(
            fun ppf errs ->
              pp_cli_errors
                ppf
                ~executable_name:(Filename.basename Sys.executable_name)
                ~global_options
                ~default:pp
                errs)
          e ;
        exit 1
end

include Make (Wasm_utils)
