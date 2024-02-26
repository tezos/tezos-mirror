(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
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

open Repl_helpers
open Tezos_scoru_wasm
open Custom_section

(* Possible step kinds. *)
type eval_step =
  | Tick  (** Tick per tick *)
  | Result  (** Up to Eval (Result (SK_Result _ | SK_Trap _)) *)
  | Kernel_run  (** Up to the end of the current `kernel_run` *)
  | Inbox  (** Until input requested *)

type profile_options = {collapse : bool; with_time : bool; no_reboot : bool}

(* Possible commands for the REPL. *)
type commands =
  | Time of commands
  | Show_inbox
  | Show_outbox of int32
  | Show_status
  | Show_durable_storage
  | Show_subkeys of string
  | Show_key of string * printable_value_kind
  | Show_memory of int32 * int * printable_value_kind
  | Dump_function_symbols
  | Step of eval_step
  | Load_inputs
  | Reveal_preimage of string option
  | Reveal_metadata
  | Profile of profile_options
  | Unknown of string
  | Help
  | Stop

let parse_eval_step = function
  | "tick" -> Some Tick
  | "result" -> Some Result
  | "kernel_run" -> Some Kernel_run
  | "inbox" -> Some Inbox
  | _ -> None

let parse_memory_commands = function
  | ["at"; address; "for"; length; "bytes"] -> (
      match (Int32.of_string_opt address, int_of_string_opt length) with
      | Some address, Some length -> Some (Show_memory (address, length, `Hex))
      | _, _ -> None)
  | ["at"; address; "for"; length; "bytes"; "as"; "string"] -> (
      match (Int32.of_string_opt address, int_of_string_opt length) with
      | Some address, Some length ->
          Some (Show_memory (address, length, `String))
      | _, _ -> None)
  | ["at"; address; "as"; kind] -> (
      match
        (Int32.of_string_opt address, integer_value_kind_of_string kind)
      with
      | Some address, Some kind ->
          Some (Show_memory (address, value_kind_length kind, kind))
      | _, _ -> None)
  | _ -> None

let parse_profile_options =
  let set_option profile_options = function
    | "--collapsed" ->
        Option.map (fun opts -> {opts with collapse = true}) profile_options
    | "--without-time" ->
        Option.map (fun opts -> {opts with with_time = false}) profile_options
    | "--no-reboot" ->
        Option.map (fun opts -> {opts with no_reboot = true}) profile_options
    | _ -> None
  in
  List.fold_left
    set_option
    (Some {collapse = false; with_time = true; no_reboot = false})

(* Documentation for commands type *)
type command_description = {
  parse : string list -> commands option;
  documentation : string;
}

(* Parsing and documentation of each possible command *)
let rec commands_docs =
  [
    {
      parse =
        (function
        | "time" :: rst -> try_parse rst |> Option.map (fun x -> Time x)
        | _ -> None);
      documentation =
        "time <command>: Executes <command> and returns the time required for \
         its execution.";
    };
    {
      parse = (function ["show"; "inbox"] -> Some Show_inbox | _ -> None);
      documentation =
        "show inbox: Prints the current input buffer and the number of \
         messages it contains.";
    };
    {
      parse =
        (function
        | ["show"; "outbox"; "at"; "level"; level] ->
            Int32.of_string_opt level |> Option.map (fun l -> Show_outbox l)
        | _ -> None);
      documentation =
        "show outbox at level <level>: Prints the outbox messages for <level>.";
    };
    {
      parse = (function ["show"; "status"] -> Some Show_status | _ -> None);
      documentation = "show status: Shows the state of the PVM.";
    };
    {
      parse =
        (function
        | ["show"; "durable"; "storage"] -> Some Show_durable_storage
        | _ -> None);
      documentation =
        "show durable storage: Prints the durable storage from the root of the \
         tree.";
    };
    {
      parse =
        (function
        | ["show"; "subkeys"; path] | ["ls"; path] -> Some (Show_subkeys path)
        | ["show"; "subkeys"] | ["ls"] -> Some (Show_subkeys "/")
        | _ -> None);
      documentation =
        "show subkeys [<path>] | ls [<path>]: Prints the direct subkeys under \
         <path> (default '/')";
    };
    {
      parse =
        (function
        | ["show"; "key"; key] -> Some (Show_key (key, `Hex))
        | ["show"; "key"; key; "as"; kind] ->
            printable_value_kind_of_string kind
            |> Option.map (fun kind -> Show_key (key, kind))
        | _ -> None);
      documentation =
        "show key <key> [as <kind>]: Looks for given <key> in durable storage \
         and prints its value in <kind> format. Prints errors if <key> is \
         invalid or not existing.";
    };
    {
      parse =
        (function
        | ["dump"; "function"; "symbols"] -> Some Dump_function_symbols
        | _ -> None);
      documentation =
        "dump function symbols: Pretty-prints the parsed functions custom \
         section. The result will be empty if the kernel is in `wast` format \
         or has been stripped of its custom sections.";
    };
    {
      parse =
        (function
        | ["step"; step] -> parse_eval_step step |> Option.map (fun s -> Step s)
        | _ -> None);
      documentation =
        "step <eval_step>: Depending on <eval_step>:\n\
        \          - tick: Run for a tick.\n\
        \          - result: Run up to the end of `kernel_run` but before \
         rebooting of waiting for reboot, before the memory is flushed.\n\
        \          - kernel_run: Run up to the end of the current `kernel_run` \
         with padding up to the maximum ticks per reboot for the current \
         version of the PVM.\n\
        \          - inbox: Run until the next inbox level. Loads the next \
         inbox if the interpreter is waiting for inputs.";
    };
    {
      parse = (function ["load"; "inputs"] -> Some Load_inputs | _ -> None);
      documentation =
        "load inputs: If the kernel is waiting for inputs, go the the next \
         level and load the next inbox.";
    };
    {
      parse =
        (function
        | ["reveal"; "preimage"] -> Some (Reveal_preimage None)
        | ["reveal"; "preimage"; hex_encoded_preimage] ->
            Some (Reveal_preimage (Some hex_encoded_preimage))
        | _ -> None);
      documentation =
        "reveal preimage [<hex_encoded_preimage>]: Checks the current state is \
         waiting for a preimage, parses <hex_encoded_preimage> as an \
         hexadecimal representation of the data or uses the builtin if none is \
         given, and does a reveal step.";
    };
    {
      parse =
        (function ["reveal"; "metadata"] -> Some Reveal_metadata | _ -> None);
      documentation =
        "reveal metadata: While in a reveal step, print the current metadata \
         from PVM.";
    };
    {
      parse = (function ["stop"] -> Some Stop | _ -> None);
      documentation = "stop: Stops the execution of the current session.";
    };
    {
      parse =
        (function
        | "profile" :: options ->
            parse_profile_options options
            |> Option.map (fun opts -> Profile opts)
        | _ -> None);
      documentation =
        "profile <option>: Profiles the execution of a full inbox and outputs \
         a flamegraph compatible stack fill in the temporary directory of the \
         system. \n\
         Options:\n\
        \ - `--collapsed`: reduces the size of the resulting file by merging \
         the identical stacks, at the cost of not being able to track the call \
         stack on a time basis.\n\
        \ - `--without-time`: does not profile the time (can have an impact on \
         performance if the kernel does too many function calls).\n\
        \ - `--no-reboot`: profile a single `kernel_run`, not a full inbox.";
    };
    {
      parse =
        (function
        | "show" :: "memory" :: rest -> parse_memory_commands rest | _ -> None);
      documentation =
        "show memory at <address> for <length> bytes [as string] | show memory \
         at <address> as <kind>: Loads the <length> bytes at <address> in the \
         memory, and prints it in its hexadecimal (or string) representation.\n\
         The command is only available if the memory hasn't been flushed and \
         during the evaluation of the kernel, i.e. after `step result` or \
         `step tick`.";
    };
    {
      parse = (function ["help"] | ["man"] -> Some Help | _ -> None);
      documentation =
        "help: Provide documentation about the available commands.";
    };
  ]

and try_parse command =
  let rec go = function
    | [] -> None
    | command_docs :: commands_docs -> (
        match command_docs.parse command with
        | None -> go commands_docs
        | command -> command)
  in
  go commands_docs

let parse_commands s =
  let commands = try_parse (String.split_no_empty ' ' (String.trim s)) in
  match commands with None -> Unknown s | Some cmd -> cmd

let build_metadata config =
  Tezos_protocol_alpha.Protocol.Alpha_context.Sc_rollup.Metadata.(
    {
      address = config.Config.destination;
      origination_level =
        Tezos_protocol_alpha.Protocol.Alpha_context.Raw_level.of_int32_exn 0l;
    }
    |> Data_encoding.Binary.to_string_exn encoding)

let read_data_from_file path =
  let ch = open_in path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ;
  s

let read_data_from_stdin retries =
  let open Lwt_syntax in
  let rec input_data retries : string Lwt.t =
    if retries <= 0 then Stdlib.failwith "Too many tries, aborting."
    else
      let* () = Lwt_fmt.printf "> " in
      let* input = Option.catch_s (fun () -> Lwt_io.read_line Lwt_io.stdin) in
      match Option.bind input (fun bytes -> Hex.to_string (`Hex bytes)) with
      | Some data -> Lwt.return data
      | None ->
          let* () =
            Lwt_fmt.printf
              "Error: the data is not a valid hexadecimal value.\n%!"
          in
          input_data (pred retries)
  in
  input_data retries

let read_data ~kind ~directory ~filename ?fetch_from_remote retries =
  let read_from_file () =
    let path = Filename.concat directory filename in
    let s = read_data_from_file path in
    Lwt.return s
  in
  let read_from_remote =
    match fetch_from_remote with
    | None -> fun () -> raise Not_found
    | Some fetch_from_remote ->
        fun () ->
          let open Lwt_syntax in
          let* s = fetch_from_remote filename in
          let ch = open_out (Filename.concat directory filename) in
          output_string ch s ;
          close_out ch ;
          return s
  in
  let read_from_stdin () =
    let open Lwt_syntax in
    let* () =
      Lwt_fmt.printf
        "%s %s not found in %s, or there was a reading error. Please provide \
         it manually.\n\
         %!"
        kind
        filename
        directory
    in
    read_data_from_stdin retries
  in
  Lwt.catch read_from_file @@ fun _ ->
  Lwt.catch read_from_remote @@ fun _ -> read_from_stdin ()

let fetch_preimage_from_remote endpoint hash_hex =
  let open Lwt_syntax in
  let url =
    Uri.with_path endpoint String.(concat "/" [Uri.path endpoint; hash_hex])
  in
  let* resp, body = Cohttp_lwt_unix.Client.get url in
  let* body_str = Cohttp_lwt.Body.to_string body in
  match resp.status with
  | `OK ->
      let contents_hash_hex =
        let open Tezos_protocol_alpha.Protocol.Sc_rollup_reveal_hash in
        hash_string ~scheme:Blake2B [body_str] |> to_hex
      in
      if not (String.equal contents_hash_hex hash_hex) then
        Format.ksprintf
          Stdlib.failwith
          "The kernel fetched pre-image with hash %s instead of %s."
          contents_hash_hex
          hash_hex ;
      return body_str
  | #Cohttp.Code.status_code ->
      let* () =
        Lwt_fmt.printf
          "Could not fetch %s from %s. Response status code %d\n%!"
          hash_hex
          (Uri.path endpoint)
          (Cohttp.Code.code_of_status resp.status)
      in
      raise Not_found

let reveal_preimage_builtin config retries hash =
  let hex = Tezos_protocol_alpha.Protocol.Sc_rollup_reveal_hash.to_hex hash in
  let fetch_from_remote =
    Option.map fetch_preimage_from_remote config.Config.preimage_endpoint
  in
  read_data
    ~kind:"Preimage"
    ~directory:config.Config.preimage_directory
    ~filename:hex
    ?fetch_from_remote
    retries

let request_dal_page config retries published_level slot_index page_index =
  let open Tezos_protocol_alpha.Protocol in
  let filename =
    Format.asprintf
      "%a-%a-%a"
      Alpha_context.Raw_level.pp
      published_level
      Alpha_context.Dal.Slot_index.pp
      slot_index
      Dal_slot_repr.Page.Index.pp
      page_index
  in
  read_data
    ~kind:"DAL page"
    ~directory:config.Config.dal_pages_directory
    ~filename
    retries

let reveals config request =
  let open Tezos_protocol_alpha.Protocol.Alpha_context.Sc_rollup in
  let num_retries = 3 in
  match Wasm_2_0_0PVM.decode_reveal request with
  | Reveal_raw_data hash -> reveal_preimage_builtin config num_retries hash
  | Reveal_metadata -> Lwt.return (build_metadata config)
  | Reveal_dal_parameters ->
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/6547
         Support reveal_dal_parameters in the WASM debugger. *)
      Stdlib.failwith
        "The kernel tried to request DAL parameters, but this is not yet \
         supported by the debugger."
  | Request_dal_page {slot_id = {published_level; index}; page_index} ->
      request_dal_page config num_retries published_level index page_index

let write_debug config =
  if config.Config.kernel_debug then
    Tezos_scoru_wasm.Builtins.Printer (fun msg -> Lwt_fmt.printf "%s%!" msg)
  else Tezos_scoru_wasm.Builtins.Noop

module Make (Wasm_utils : Wasm_utils_intf.S) = struct
  module Prof = Profiling.Make (Wasm_utils)
  open Wasm_utils

  (* [compute_step tree] is a wrapper around [Wasm_pvm.compute_step] that also
     returns the number of ticks elapsed (whi is always 1). *)
  let compute_step config tree =
    let open Lwt_syntax in
    trap_exn (fun () ->
        let+ tree =
          Wasm.compute_step_with_debug
            ~wasm_entrypoint:Constants.wasm_entrypoint
            ~write_debug:(write_debug config)
            tree
        in
        (tree, 1L))

  (** [eval_to_result tree] tries to evaluates the PVM until the next `SK_Result`
    or `SK_Trap`, and stops in case of reveal tick or input tick. It has the
    property that the memory hasn't been flushed yet and can be inspected. *)
  let eval_to_result config tree =
    trap_exn (fun () ->
        eval_to_result
          ~write_debug:(write_debug config)
          ~reveal_builtins:(reveals config)
          tree)

  (* [eval_kernel_run tree] evals up to the end of the current `kernel_run` (or
     starts a new one if already at snapshot point). *)
  let eval_kernel_run ~wasm_entrypoint config tree =
    let open Lwt_syntax in
    trap_exn (fun () ->
        let* info_before = Wasm.get_info tree in
        let* tree, _ =
          Wasm_fast.compute_step_many
            ~wasm_entrypoint
            ~reveal_builtins:(reveals config)
            ~write_debug:(write_debug config)
            ~stop_at_snapshot:true
            ~max_steps:Int64.max_int
            tree
        in
        let+ info_after = Wasm.get_info tree in
        ( tree,
          Z.to_int64 @@ Z.sub info_after.current_tick info_before.current_tick
        ))

  (* Wrapper around {Wasm_utils.eval_until_input_requested}. *)
  let eval_until_input_requested ~wasm_entrypoint config tree =
    let open Lwt_syntax in
    trap_exn (fun () ->
        let* info_before = Wasm.get_info tree in
        let* tree =
          eval_until_input_requested
            ~wasm_entrypoint
            ~fast_exec:true
            ~reveal_builtins:(Some (reveals config))
            ~write_debug:(write_debug config)
            ~max_steps:Int64.max_int
            tree
        in
        let+ info_after = Wasm.get_info tree in
        ( tree,
          Z.to_int64 @@ Z.sub info_after.current_tick info_before.current_tick
        ))

  let produce_flamegraph ~collapse ~max_depth kernel_runs =
    let filename =
      Time.System.(
        Format.asprintf "wasm-debugger-profiling-%a.out" pp_hum (now ()))
    in
    let path = Filename.(concat (get_temp_dir_name ()) filename) in
    let file = open_out path in
    let pp_kernel_run ppf = function
      | Some run ->
          Profiling.pp_flamegraph ~collapse ~max_depth Profiling.pp_call ppf run
      | None -> ()
    in
    let pp_kernel_runs ppf runs =
      Format.pp_print_list ~pp_sep:(fun _ _ -> ()) pp_kernel_run ppf runs
    in
    Format.fprintf
      (Format.formatter_of_out_channel file)
      "%a"
      pp_kernel_runs
      kernel_runs ;
    close_out file ;
    Format.printf "Profiling result can be found in %s\n%!" path

  let profiling_results = function
    | Some run ->
        let pvm_steps = Profiling.aggregate_toplevel_time_and_ticks run in
        let full_ticks, full_time = Profiling.full_ticks_and_time pvm_steps in
        Format.printf
          "----------------------\n\
           Detailed results for a `kernel_run`:\n\
           %a\n\n\
           Full execution: %a ticks%a\n\
           %!"
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf "\n")
             Profiling.pp_ticks_and_time)
          pvm_steps
          Z.pp_print
          full_ticks
          Profiling.pp_time_opt
          full_time
    | None ->
        Format.printf
          "----------------------\n\
           The resulting call graph is inconsistent for this specific \
           `kernel_run`, please open an issue.\n\
           %!"

  let eval_and_profile ~collapse ~with_time ~no_reboot config function_symbols
      tree =
    let open Lwt_syntax in
    trap_exn (fun () ->
        Format.printf
          "Starting the profiling until new messages are expected. Please note \
           that it will take some time and does not reflect a real computation \
           time.\n\
           %!" ;
        let+ tree, ticks, graph =
          Prof.eval_and_profile
            ~write_debug:(write_debug config)
            ~reveal_builtins:(reveals config)
            ~with_time
            ~no_reboot
            function_symbols
            tree
        in
        produce_flamegraph ~collapse ~max_depth:100 graph ;
        List.iter profiling_results graph ;
        Format.printf
          "----------------------\nFull execution with padding: %a ticks\n%!"
          Z.pp_print
          ticks ;
        tree)

  let set_raw_message_input_step level counter encoded_message tree =
    Wasm.set_input_step (input_info level counter) encoded_message tree

  (* [check_input_request tree] checks the PVM is waiting for inputs, and returns
     an hint to go to the next input request state otherwise. *)
  let check_input_request tree =
    let open Lwt_syntax in
    let* info = Wasm.get_info tree in
    match info.Wasm_pvm_state.input_request with
    | Input_required -> return_ok_unit
    | No_input_required ->
        return_error
          "The PVM is not expecting inputs yet. You can `step inbox` to \
           evaluate the current inbox."
    | Reveal_required _ ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/4208 *)
        return_error
          "The PVM is expecting a reveal. This command is not implemented yet."

  (* [load_inputs_gen inboxes level tree] reads the next inbox from [inboxes], set the
     messages at [level] in the [tree], and returns the remaining inbox, the next
     level and the tree. *)
  let load_inputs_gen inboxes level tree =
    let open Lwt_result_syntax in
    match Seq.uncons inboxes with
    | Some (inputs, inboxes) ->
        let* tree =
          trap_exn (fun () ->
              set_full_input_step_gen
                set_raw_message_input_step
                inputs
                level
                tree)
        in
        let*! () =
          Lwt_fmt.printf
            "Loaded %d inputs at level %ld\n%!"
            (List.length inputs)
            level
        in
        return (tree, inboxes, Int32.succ level)
    | None ->
        let*! () = Lwt_fmt.printf "No more inputs at level %ld\n%!" level in
        return (tree, inboxes, level)

  let load_inputs inboxes level tree =
    let open Lwt_result_syntax in
    let*! status = check_input_request tree in
    match status with
    | Ok () -> load_inputs_gen inboxes level tree
    | Error msg ->
        Format.printf "%s\n%!" msg ;
        return (tree, inboxes, level)

  (* Eval dispatcher. *)
  let eval ~wasm_entrypoint level inboxes config step tree =
    let open Lwt_result_syntax in
    let return' ?(inboxes = inboxes) f =
      let* tree, count = f in
      return (tree, count, inboxes, level)
    in
    match step with
    | Tick -> return' (compute_step config tree)
    | Result -> return' (eval_to_result config tree)
    | Kernel_run -> return' (eval_kernel_run ~wasm_entrypoint config tree)
    | Inbox -> (
        let*! status = check_input_request tree in
        match status with
        | Ok () ->
            let* tree, inboxes, level = load_inputs inboxes level tree in
            let* tree, ticks =
              eval_until_input_requested ~wasm_entrypoint config tree
            in
            return (tree, ticks, inboxes, level)
        | Error _ ->
            return' (eval_until_input_requested ~wasm_entrypoint config tree))

  let profile ~collapse ~with_time ~no_reboot level inboxes config
      function_symbols tree =
    let open Lwt_result_syntax in
    let*! pvm_state =
      Wasm_utils.Tree_encoding_runner.decode Wasm_pvm.pvm_state_encoding tree
    in
    let*! status = check_input_request tree in
    let is_profilable =
      match pvm_state.tick_state with
      | Collect | Snapshot
      | Decode Tezos_webassembly_interpreter.Decode.{module_kont = MKStart; _}
        ->
          true
      | _ -> false
    in

    match status with
    | Ok () when is_profilable ->
        let* tree, inboxes, level = load_inputs inboxes level tree in
        let* tree =
          eval_and_profile
            ~collapse
            ~with_time
            ~no_reboot
            config
            function_symbols
            tree
        in
        return (tree, inboxes, level)
    | Error _ when is_profilable ->
        let* tree =
          eval_and_profile
            ~collapse
            ~with_time
            ~no_reboot
            config
            function_symbols
            tree
        in
        return (tree, inboxes, level)
    | _ ->
        let*! () =
          Lwt_fmt.printf
            "Profiling can only be done from a snapshotable state or when \
             waiting for input. You can use `step kernel_run` to go to the \
             next snapshotable state.\n\
             %!"
        in
        return (tree, inboxes, level)

  let pp_input_request ppf = function
    | Wasm_pvm_state.No_input_required -> Format.fprintf ppf "Evaluating"
    | Input_required -> Format.fprintf ppf "Waiting for input"
    | Reveal_required req ->
        let open Tezos_protocol_alpha.Protocol.Alpha_context.Sc_rollup in
        let req = Wasm_2_0_0PVM.decode_reveal req in
        Format.fprintf ppf "Waiting for reveal: %a" pp_reveal req

  (* [show_status tree] show the state of the PVM. *)
  let show_status tree =
    let open Lwt_syntax in
    let* state = Wasm.Internal_for_tests.get_tick_state tree in
    let* info = Wasm.get_info tree in
    Lwt_fmt.printf
      "%s\n%!"
      (Format.asprintf
         "Status: %a\nInternal_status: %a"
         pp_input_request
         info.Wasm_pvm_state.input_request
         pp_state
         state)

  (* [step level inboxes config kind tree] evals according to the step kind and
     prints the number of ticks elapsed and the new status. *)
  let step ~wasm_entrypoint level inboxes config kind tree =
    let open Lwt_result_syntax in
    let* tree, ticks, inboxes, level =
      eval ~wasm_entrypoint level inboxes config kind tree
    in
    let*! () = Lwt_fmt.printf "Evaluation took %Ld ticks so far\n" ticks in
    let*! () = show_status tree in
    return_some (tree, inboxes, level)

  (* [show_inbox tree] prints the current input buffer and the number of messages
     it contains. *)
  let show_inbox tree =
    let open Lwt_syntax in
    let* input_buffer = Wasm.Internal_for_tests.get_input_buffer tree in
    let* messages =
      Tezos_lazy_containers.Lazy_vector.(
        Mutable.ZVector.snapshot input_buffer |> ZVector.to_list)
    in
    let messages_sorted =
      List.sort Messages.compare_input_buffer_messages messages
    in
    let pp_message ppf
        Tezos_webassembly_interpreter.Input_buffer.
          {raw_level; message_counter; payload} =
      Format.fprintf
        ppf
        {|{ raw_level: %ld;
  counter: %s
  payload: %a }|}
        raw_level
        (Z.to_string message_counter)
        Messages.pp_input
        payload
    in
    let pp_messages () =
      Format.asprintf
        "%a"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "\n")
           pp_message)
        messages_sorted
    in

    let size =
      Tezos_lazy_containers.Lazy_vector.Mutable.ZVector.num_elements
        input_buffer
    in
    Lwt_fmt.printf
      "Inbox has %s messages:\n%s\n%!"
      (Z.to_string size)
      (pp_messages ())

  (* [show_outbox_gen tree level] prints the outbox messages for a given level. *)
  let show_outbox_gen tree level =
    let open Lwt_syntax in
    let* output_buffer = Wasm.Internal_for_tests.get_output_buffer tree in
    let* level_vector =
      Tezos_webassembly_interpreter.Output_buffer.get_outbox output_buffer level
    in
    let* messages =
      Tezos_lazy_containers.Lazy_vector.(level_vector |> ZVector.to_list)
    in
    let pp_messages () =
      Format.asprintf
        "%a"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "\n")
           Messages.pp_output)
        messages
    in
    let size =
      Tezos_lazy_containers.Lazy_vector.ZVector.num_elements level_vector
    in
    Lwt_fmt.printf
      "Outbox has %s messages:\n%s\n%!"
      (Z.to_string size)
      (pp_messages ())

  (* [show_outbox tree level] prints the outbox messages for a given level. *)
  let show_outbox tree level =
    Lwt.catch
      (fun () -> show_outbox_gen tree level)
      (fun _ -> Lwt_fmt.printf "No outbox found at level %ld\n%!" level)

  (* [find_key_in_durable] retrieves the given [key] from the durable storage in
     the tree. Returns `None` if the key does not exists. *)
  let find_key_in_durable tree key =
    let open Lwt_syntax in
    let* durable = Wasm_utils.wrap_as_durable_storage tree in
    let durable = Tezos_scoru_wasm.Durable.of_storage_exn durable in
    Tezos_scoru_wasm.Durable.find_value durable key

  (* [print_durable ~depth ~show_values ~path tree] prints the keys in the durable
     storage from the given path and their values in their hexadecimal
     representation. By default, it prints from the root of the durable
     storage. *)
  let print_durable ?(depth = 10) ?(show_values = true) ?(path = []) tree =
    let open Lwt_syntax in
    let durable_path = "durable" :: path in
    let* path_exists = Wasm_utils.Ctx.Tree.mem_tree tree durable_path in
    if path_exists then
      Wasm_utils.Ctx.Tree.fold
        ~depth:(`Le depth)
        tree
        ("durable" :: path)
        ~order:`Sorted
        ~init:()
        ~f:(fun key tree () ->
          let full_key = String.concat "/" key in
          (* If we need to show the values, we show every keys, even the root and
             '@'. *)
          if show_values then
            let+ value = Wasm_utils.Ctx.Tree.find tree [] in
            let value = Option.value ~default:(Bytes.create 0) value in
            Format.printf "/%s\n  %a\n%!" full_key Hex.pp (Hex.of_bytes value)
          else if key <> [] && key <> ["@"] then
            return (Format.printf "/%s\n%!" full_key)
          else return_unit)
    else
      Lwt.return
      @@ Format.printf
           "The path /%s is not available in the durable storage\n%!"
           (String.concat "/" path)

  (* [show_durable] prints the durable storage from the tree. *)
  let show_durable tree = print_durable ~depth:10 tree

  (* [show_subkeys tree path] prints the direct subkeys under the given path. *)
  let show_subkeys tree path =
    let invalid_path () =
      Lwt.return @@ Format.printf "Invalid path, it must start with '/'\n%!"
    in
    match String.index_opt path '/' with
    | Some i when i <> 0 -> invalid_path ()
    | None -> invalid_path ()
    | Some _ ->
        print_durable
          ~depth:1
          ~path:(String.split_no_empty '/' path)
          ~show_values:false
          tree

  let show_value kind value =
    let printable = Repl_helpers.print_wasm_encoded_value kind value in
    match printable with
    | Ok s -> s
    | Error err ->
        Format.asprintf
          "Error: %s. Defaulting to hexadecimal value\n%a"
          err
          Hex.pp
          (Hex.of_string value)

  (* [show_key_gen tree key] looks for the given [key] in the durable storage and
     print its value in hexadecimal format. *)
  let show_key_gen tree key kind =
    let open Lwt_syntax in
    let* value = find_key_in_durable tree key in
    match value with
    | None ->
        Format.printf "Key not found\n%!" ;
        return_unit
    | Some v ->
        let+ str_value =
          Tezos_lazy_containers.Chunked_byte_vector.to_string v
        in
        Format.printf "%s\n%!" @@ show_value kind str_value

  (* [show_key tree key] looks for the given [key] in the durable storage and
     print its value in hexadecimal format. Prints errors in case the key is
     invalid or not existing. *)
  let show_key tree key kind =
    Lwt.catch
      (fun () ->
        let key = Tezos_scoru_wasm.Durable.key_of_string_exn key in
        show_key_gen tree key kind)
      (function
        | Tezos_scoru_wasm.Durable.Invalid_key _ ->
            Lwt_fmt.printf "Invalid key\n%!"
        | Tezos_scoru_wasm.Durable.Value_not_found ->
            Lwt_fmt.printf "No value found for key\n%!"
        | Tezos_scoru_wasm.Durable.Tree_not_found ->
            Lwt_fmt.printf "No tree found for key\n%!"
        | exn ->
            Lwt_fmt.printf "Unknown exception: %s\n%!" (Printexc.to_string exn))

  exception Cannot_inspect_memory of string

  (* [load_memory tree] finds the memory module 0 from the tree, only and only if
     the PVM is in an Init or Eval state. *)
  let load_memory tree =
    let open Lwt_syntax in
    let* state = Wasm.Internal_for_tests.get_tick_state tree in
    let* module_inst =
      match state with
      | Eval _ | Init _ -> Wasm.Internal_for_tests.get_module_instance_exn tree
      | _ -> raise (Cannot_inspect_memory (Format.asprintf "%a" pp_state state))
    in
    Lwt.catch
      (fun () ->
        Tezos_lazy_containers.Lazy_vector.Int32Vector.get
          0l
          module_inst.memories)
      (fun _ ->
        raise Tezos_webassembly_interpreter.Eval.Missing_memory_0_export)

  (* [show_memory tree address length] loads the [length] bytes at address
     [address] in the memory, and prints it in its hexadecimal representation. *)
  let show_memory tree address length kind =
    let open Lwt_syntax in
    Lwt.catch
      (fun () ->
        let* memory = load_memory tree in
        let* value =
          Tezos_webassembly_interpreter.Memory.load_bytes memory address length
        in
        Lwt_fmt.printf "%s\n%!" @@ show_value kind value)
      (function
        | Cannot_inspect_memory state ->
            Lwt_fmt.printf
              "Error: Cannot inspect memory during internal state %s\n%!"
              state
        | exn -> Lwt_fmt.printf "Error: %s\n%!" (Printexc.to_string exn))

  let dump_function_symbols function_symbols =
    let functions =
      Format.asprintf
        "%a"
        Custom_section.pp_function_subsection
        function_symbols
    in
    Lwt_fmt.printf "Functions:\n%s\n" functions

  (* [reveal_preimage config hex tree] checks the current state is waiting for a
     preimage, parses [hex] as an hexadecimal representation of the data or use
     the builtin if none is given, and does a reveal step. *)
  let reveal_preimage config bytes tree =
    let open Lwt_syntax in
    let* info = Wasm.get_info tree in
    match info.Tezos_scoru_wasm.Wasm_pvm_state.input_request with
    | Reveal_required req -> (
        match
          ( Tezos_protocol_alpha.Protocol.Alpha_context.Sc_rollup.Wasm_2_0_0PVM
            .decode_reveal
              req,
            Option.bind bytes (fun bytes -> Hex.to_bytes (`Hex bytes)) )
        with
        | Reveal_raw_data _, Some preimage -> Wasm.reveal_step preimage tree
        | Reveal_raw_data hash, None ->
            Lwt.catch
              (fun () ->
                let* preimage = reveal_preimage_builtin config 1 hash in
                Wasm.reveal_step (String.to_bytes preimage) tree)
              (fun _ -> return tree)
        | _ ->
            let+ () =
              Lwt_fmt.printf "Error: not the expected reveal step\n%!"
            in
            tree)
    | _ ->
        let+ () = Lwt_fmt.printf "Error: not a reveal step\n%!" in
        tree

  let reveal_metadata config tree =
    let open Lwt_syntax in
    let* info = Wasm.get_info tree in
    match info.Tezos_scoru_wasm.Wasm_pvm_state.input_request with
    | Tezos_scoru_wasm.Wasm_pvm_state.(Reveal_required req) -> (
        match
          Tezos_protocol_alpha.Protocol.Alpha_context.Sc_rollup.Wasm_2_0_0PVM
          .decode_reveal
            req
        with
        | Reveal_metadata ->
            let data = build_metadata config in
            Wasm.reveal_step (Bytes.of_string data) tree
        | _ ->
            let+ () =
              Lwt_fmt.printf "Error: Not the expected reveal step\n%!"
            in
            tree)
    | _ ->
        let+ () = Lwt_fmt.printf "Error: Not in a reveal step\n%!" in
        tree

  let get_function_symbols tree =
    let open Lwt_result_syntax in
    let*! durable = Wasm_utils.wrap_as_durable_storage tree in
    let durable = Tezos_scoru_wasm.Durable.of_storage_exn durable in
    let*! module_ =
      Tezos_scoru_wasm.(Durable.find_value_exn durable Constants.kernel_key)
    in
    let*! module_string =
      Tezos_lazy_containers.Chunked_byte_vector.to_string module_
    in
    let* function_symbols =
      Repl_helpers.trap_exn (fun () ->
          parse_custom_sections "kernel" module_string)
    in
    return function_symbols

  (* [handle_command command tree inboxes level] dispatches the commands to their
     actual implementation. *)
  let handle_command c config tree inboxes level =
    let open Lwt_result_syntax in
    let command = parse_commands c in
    let return ?(tree = tree) ?(inboxes = inboxes) () =
      return_some (tree, inboxes, level)
    in
    let rec go = function
      | Time cmd ->
          let t = Time.System.now () in
          let* res = go cmd in
          let t' = Time.System.now () in
          let*! () =
            Lwt_fmt.printf
              "took %s\n%!"
              (Format.asprintf "%a" Ptime.Span.pp (Ptime.diff t' t))
          in
          Lwt_result_syntax.return res
      | Load_inputs ->
          let+ ctx = load_inputs inboxes level tree in
          Some ctx
      | Show_status ->
          let*! () = show_status tree in
          return ()
      | Step kind ->
          step
            ~wasm_entrypoint:Constants.wasm_entrypoint
            level
            inboxes
            config
            kind
            tree
      | Show_inbox ->
          let*! () = show_inbox tree in
          return ()
      | Show_outbox level ->
          let*! () = show_outbox tree level in
          return ()
      | Show_durable_storage ->
          let*! () = show_durable tree in
          return ()
      | Show_subkeys path ->
          let*! () = show_subkeys tree path in
          return ()
      | Show_key (key, kind) ->
          let*! () = show_key tree key kind in
          return ()
      | Show_memory (address, length, kind) ->
          let*! () = show_memory tree address length kind in
          return ()
      | Dump_function_symbols ->
          let* function_symbols = get_function_symbols tree in
          let*! () = dump_function_symbols function_symbols in
          return ()
      | Reveal_preimage bytes ->
          let*! tree = reveal_preimage config bytes tree in
          return ~tree ()
      | Reveal_metadata ->
          let*! tree = reveal_metadata config tree in
          return ~tree ()
      | Profile {collapse; with_time; no_reboot} ->
          let* function_symbols = get_function_symbols tree in
          let* tree, inboxes, level =
            profile
              ~collapse
              ~with_time
              ~no_reboot
              level
              inboxes
              config
              function_symbols
              tree
          in
          return_some (tree, inboxes, level)
      | Unknown s ->
          let*! () = Lwt_fmt.eprintf "Unknown command `%s`\n%!" s in
          return ()
      | Help ->
          let*! () =
            List.iter_s
              (fun command_docs ->
                Lwt_fmt.printf "  %s\n" command_docs.documentation)
              commands_docs
          in
          return ()
      | Stop -> return_none
    in
    go command
end
