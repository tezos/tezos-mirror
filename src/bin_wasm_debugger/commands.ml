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

open Wasm_utils
open Repl_helpers
open Tezos_scoru_wasm

(* Possible step kinds. *)
type eval_step =
  | Tick  (** Tick per tick *)
  | Result  (** Up to Eval (Result (SK_Result _ | SK_Trap _)) *)
  | Kernel_run  (** Up to the end of the current `kernal_run` *)
  | Inbox  (** Until input requested *)

(* Possible commands for the REPL. *)
type commands =
  | Bench
  | Time of commands
  | Show_inbox
  | Show_outbox of int32
  | Show_status
  | Show_durable_storage
  | Show_subkeys of string
  | Show_key of string * printable_value_kind
  | Show_memory of int32 * int * printable_value_kind
  | Step of eval_step
  | Load_inputs
  | Reveal_preimage of string option
  | Reveal_metadata
  | Unknown of string
  | Stop

let parse_eval_step = function
  | "tick" -> Some Tick
  | "result" -> Some Result
  | "kernel_run" -> Some Kernel_run
  | "inbox" -> Some Inbox
  | _ -> None

let parse_memory_commands full_command = function
  | ["at"; address; "for"; length; "bytes"] -> (
      match (Int32.of_string_opt address, int_of_string_opt length) with
      | Some address, Some length -> Show_memory (address, length, `Hex)
      | _, _ -> Unknown full_command)
  | ["at"; address; "for"; length; "bytes"; "as"; "string"] -> (
      match (Int32.of_string_opt address, int_of_string_opt length) with
      | Some address, Some length -> Show_memory (address, length, `String)
      | _, _ -> Unknown full_command)
  | ["at"; address; "as"; kind] -> (
      match
        (Int32.of_string_opt address, integer_value_kind_of_string kind)
      with
      | Some address, Some kind ->
          Show_memory (address, value_kind_length kind, kind)
      | _, _ -> Unknown full_command)
  | _ -> Unknown full_command

let parse_commands s =
  let command = String.split_no_empty ' ' (String.trim s) in
  let rec go = function
    | "time" :: rst ->
        let cmd = go rst in
        Time cmd
    | ["show"; "inbox"] -> Show_inbox
    | ["show"; "outbox"; "at"; "level"; level] -> (
        match Int32.of_string_opt level with
        | Some l -> Show_outbox l
        | None -> Unknown s)
    | ["show"; "status"] -> Show_status
    | ["show"; "durable"; "storage"] -> Show_durable_storage
    | ["show"; "subkeys"; path] -> Show_subkeys path
    | ["show"; "key"; key] -> Show_key (key, `Hex)
    | ["show"; "key"; key; "as"; kind] -> (
        match printable_value_kind_of_string kind with
        | Some kind -> Show_key (key, kind)
        | None -> Unknown s)
    | "show" :: "memory" :: rest -> parse_memory_commands s rest
    | ["step"; step] -> (
        match parse_eval_step step with Some s -> Step s | None -> Unknown s)
    | ["load"; "inputs"] -> Load_inputs
    | ["reveal"; "preimage"] -> Reveal_preimage None
    | ["reveal"; "preimage"; hex_encoded_preimage] ->
        Reveal_preimage (Some hex_encoded_preimage)
    | ["reveal"; "metadata"] -> Reveal_metadata
    | ["stop"] -> Stop
    | ["bench"] -> Bench
    | _ -> Unknown s
  in
  go command

let build_metadata config =
  Tezos_protocol_alpha.Protocol.Alpha_context.Sc_rollup.Metadata.(
    {
      address = config.Config.destination;
      origination_level =
        Tezos_protocol_alpha.Protocol.Alpha_context.Raw_level.of_int32_exn 0l;
    }
    |> Data_encoding.Binary.to_string_exn encoding)

let reveal_preimage_manually retries hash =
  let open Lwt_syntax in
  let* () = Lwt_io.printf "Preimage for hash %s not found.\n%!" hash in
  let rec input_preimage retries : string Lwt.t =
    if retries <= 0 then Stdlib.failwith "Too much retry, aborting"
    else
      let* () = Lwt_io.printf "> " in
      let* input =
        Lwt.catch
          (fun () ->
            let* i = Lwt_io.read_line Lwt_io.stdin in
            return_some i)
          (fun _ -> return_none)
      in
      match Option.bind input (fun bytes -> Hex.to_string (`Hex bytes)) with
      | Some preimage -> Lwt.return preimage
      | None ->
          let* () =
            Lwt_io.printf
              "Error: the preimage is not a valid hexadecimal value.\n%!"
          in
          input_preimage (pred retries)
  in
  input_preimage retries

let reveal_preimage_builtin config retries hash =
  let (`Hex hex) = Hex.of_string hash in
  Lwt.catch
    (fun () ->
      let path = Filename.concat config.Config.preimage_directory hex in
      let ch = open_in path in
      let s = really_input_string ch (in_channel_length ch) in
      close_in ch ;
      Lwt.return s)
    (fun _ -> reveal_preimage_manually retries hex)

let reveal_builtins config =
  Tezos_scoru_wasm.Builtins.
    {
      (* We retry 3 times to reveal the preimage manually, this should be
         configurable at some point. *)
      reveal_preimage = reveal_preimage_builtin config 3;
      reveal_metadata = (fun () -> Lwt.return (build_metadata config));
    }

let write_debug =
  Tezos_scoru_wasm.Builtins.Printer (fun msg -> Lwt_io.printf "%s%!" msg)

(* [compute_step tree] is a wrapper around [Wasm_pvm.compute_step] that also
   returns the number of ticks elapsed (whi is always 1). *)
let compute_step tree =
  let open Lwt_syntax in
  trap_exn (fun () ->
      let+ tree = Wasm.compute_step_with_debug ~write_debug tree in
      (tree, 1L))

(** [eval_to_result tree] tries to evaluates the PVM until the next `SK_Result`
    or `SK_Trap`, and stops in case of reveal tick or input tick. It has the
    property that the memory hasn't been flushed yet and can be inspected. *)
let eval_to_result config tree =
  trap_exn (fun () ->
      eval_to_result ~write_debug ~reveal_builtins:(reveal_builtins config) tree)

(* [eval_kernel_run tree] evals up to the end of the current `kernel_run` (or
   starts a new one if already at snapshot point). *)
let eval_kernel_run config tree =
  let open Lwt_syntax in
  trap_exn (fun () ->
      let* info_before = Wasm.get_info tree in
      let* tree, _ =
        Wasm_fast.compute_step_many
          ~reveal_builtins:(reveal_builtins config)
          ~write_debug
          ~stop_at_snapshot:true
          ~max_steps:Int64.max_int
          tree
      in
      let+ info_after = Wasm.get_info tree in
      ( tree,
        Z.to_int64 @@ Z.sub info_after.current_tick info_before.current_tick ))

(* Wrapper around {Wasm_utils.eval_until_input_requested}. *)
let eval_until_input_requested config tree =
  let open Lwt_syntax in
  trap_exn (fun () ->
      let* info_before = Wasm.get_info tree in
      let* tree =
        eval_until_input_requested
          ~fast_exec:true
          ~reveal_builtins:(Some (reveal_builtins config))
          ~write_debug
          ~max_steps:Int64.max_int
          tree
      in
      let+ info_after = Wasm.get_info tree in
      ( tree,
        Z.to_int64 @@ Z.sub info_after.current_tick info_before.current_tick ))

(* Eval dispatcher. *)
let eval config = function
  | Tick -> compute_step
  | Result -> eval_to_result config
  | Kernel_run -> eval_kernel_run config
  | Inbox -> eval_until_input_requested config

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
        "The PVM is not expecting inputs yet. You can `step inbox` to evaluate \
         the current inbox."
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
            set_full_input_step_gen set_raw_message_input_step inputs level tree)
      in
      let*! () =
        Lwt_io.printf
          "Loaded %d inputs at level %ld\n%!"
          (List.length inputs)
          level
      in
      return (tree, inboxes, Int32.succ level)
  | None ->
      let*! () = Lwt_io.printf "No more inputs at level %ld\n%!" level in
      return (tree, inboxes, level)

let load_inputs inboxes level tree =
  let open Lwt_result_syntax in
  let*! status = check_input_request tree in
  match status with
  | Ok () -> load_inputs_gen inboxes level tree
  | Error msg ->
      Format.printf "%s\n%!" msg ;
      return (tree, inboxes, level)

let pp_input_request ppf = function
  | Wasm_pvm_state.No_input_required -> Format.fprintf ppf "Evaluating"
  | Input_required -> Format.fprintf ppf "Waiting for input"
  | Reveal_required Reveal_metadata -> Format.fprintf ppf "Waiting for metadata"
  | Reveal_required (Reveal_raw_data hash) ->
      Format.fprintf ppf "Waiting for reveal: %s" hash

(* [show_status tree] show the state of the PVM. *)
let show_status tree =
  let open Lwt_syntax in
  let* state = Wasm.Internal_for_tests.get_tick_state tree in
  let* info = Wasm.get_info tree in
  Lwt_io.printf
    "%s\n%!"
    (Format.asprintf
       "Status: %a\nInternal_status: %a"
       pp_input_request
       info.Wasm_pvm_state.input_request
       pp_state
       state)

(* [step kind tree] evals according to the step kind and prints the number of
   ticks elapsed and the new status. *)
let step config kind tree =
  let open Lwt_result_syntax in
  let* tree, ticks = eval config kind tree in
  let*! () = Lwt_io.printf "Evaluation took %Ld ticks so far\n" ticks in
  let*! () = show_status tree in
  return tree

let bench config tree =
  let open Lwt_syntax in
  let action =
    Octez_smart_rollup_wasm_benchmark_lib.Scenario.exec_slow
      ~reveal_builtins:(reveal_builtins config)
  in
  let step =
    Octez_smart_rollup_wasm_benchmark_lib.Scenario.make_scenario_step "" action
  in
  let eval = Octez_smart_rollup_wasm_benchmark_lib.Scenario.apply_step step in
  let* benchmark, tree = eval tree in
  let* () =
    Lwt_io.printf
      "%s\n%!"
      (Format.asprintf
         "%a"
         Octez_smart_rollup_wasm_benchmark_lib.Data.pp_analysis
         benchmark)
  in
  return tree

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
    Tezos_lazy_containers.Lazy_vector.Mutable.ZVector.num_elements input_buffer
  in
  Lwt_io.printf
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
  Lwt_io.printf
    "Outbox has %s messages:\n%s\n%!"
    (Z.to_string size)
    (pp_messages ())

(* [show_outbox tree level] prints the outbox messages for a given level. *)
let show_outbox tree level =
  Lwt.catch
    (fun () -> show_outbox_gen tree level)
    (fun _ -> Lwt_io.printf "No outbox found at level %ld\n%!" level)

(* [show_durable] prints the durable storage from the tree. *)
let show_durable tree = Repl_helpers.print_durable ~depth:10 tree

(* [show_subkeys tree path] prints the direct subkeys under the given path. *)
let show_subkeys tree path =
  let invalid_path () =
    Lwt.return @@ Format.printf "Invalid path, it must start with '/'\n%!"
  in
  match String.index_opt path '/' with
  | Some i when i <> 0 -> invalid_path ()
  | None -> invalid_path ()
  | Some _ ->
      Repl_helpers.print_durable
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
  let* value = Repl_helpers.find_key_in_durable tree key in
  match value with
  | None ->
      Format.printf "Key not found\n%!" ;
      return_unit
  | Some v ->
      let+ str_value = Tezos_lazy_containers.Chunked_byte_vector.to_string v in
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
          Lwt_io.printf "Invalid key\n%!"
      | Tezos_scoru_wasm.Durable.Value_not_found ->
          Lwt_io.printf "No value found for key\n%!"
      | Tezos_scoru_wasm.Durable.Tree_not_found ->
          Lwt_io.printf "No tree found for key\n%!"
      | exn ->
          Lwt_io.printf "Unknown exception: %s\n%!" (Printexc.to_string exn))

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
      Tezos_lazy_containers.Lazy_vector.Int32Vector.get 0l module_inst.memories)
    (fun _ -> raise Tezos_webassembly_interpreter.Eval.Missing_memory_0_export)

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
      Lwt_io.printf "%s\n%!" @@ show_value kind value)
    (function
      | Cannot_inspect_memory state ->
          Lwt_io.printf
            "Error: Cannot inspect memory during internal state %s\n%!"
            state
      | exn -> Lwt_io.printf "Error: %s\n%!" (Printexc.to_string exn))

(* [reveal_preimage config hex tree] checks the current state is waiting for a
   preimage, parses [hex] as an hexadecimal representation of the data or use
   the builtin if none is given, and does a reveal step. *)
let reveal_preimage config bytes tree =
  let open Lwt_syntax in
  let* info = Wasm.get_info tree in
  match
    ( info.Tezos_scoru_wasm.Wasm_pvm_state.input_request,
      Option.bind bytes (fun bytes -> Hex.to_bytes (`Hex bytes)) )
  with
  | ( Tezos_scoru_wasm.Wasm_pvm_state.(Reveal_required (Reveal_raw_data _)),
      Some preimage ) ->
      Wasm.reveal_step preimage tree
  | Reveal_required (Reveal_raw_data hash), None ->
      Lwt.catch
        (fun () ->
          let* preimage = reveal_preimage_builtin config 1 hash in
          Wasm.reveal_step (String.to_bytes preimage) tree)
        (fun _ -> return tree)
  | _ ->
      let+ () = Lwt_io.print "Error: Not in a reveal step\n%!" in
      tree

let reveal_metadata config tree =
  let open Lwt_syntax in
  let* info = Wasm.get_info tree in
  match info.Tezos_scoru_wasm.Wasm_pvm_state.input_request with
  | Tezos_scoru_wasm.Wasm_pvm_state.(Reveal_required Reveal_metadata) ->
      let data = build_metadata config in
      Wasm.reveal_step (Bytes.of_string data) tree
  | _ ->
      let+ () = Lwt_io.print "Error: Not in a reveal step\n%!" in
      tree

(* [handle_command command tree inboxes level] dispatches the commands to their
   actual implementation. *)
let handle_command c config tree inboxes level =
  let open Lwt_result_syntax in
  let command = parse_commands c in
  let return ?(tree = tree) () = return (tree, inboxes, level) in
  let rec go = function
    | Bench ->
        let*! tree = bench config tree in
        return ~tree ()
    | Time cmd ->
        let t = Time.System.now () in
        let* res = go cmd in
        let t' = Time.System.now () in
        let*! () =
          Lwt_io.printf
            "took %s\n%!"
            (Format.asprintf "%a" Ptime.Span.pp (Ptime.diff t' t))
        in
        Lwt_result_syntax.return res
    | Load_inputs -> load_inputs inboxes level tree
    | Show_status ->
        let*! () = show_status tree in
        return ()
    | Step kind ->
        let* tree = step config kind tree in
        return ~tree ()
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
    | Reveal_preimage bytes ->
        let*! tree = reveal_preimage config bytes tree in
        return ~tree ()
    | Reveal_metadata ->
        let*! tree = reveal_metadata config tree in
        return ~tree ()
    | Unknown s ->
        let*! () = Lwt_io.eprintf "Unknown command `%s`\n%!" s in
        return ()
    | Stop -> return ()
  in
  go command
