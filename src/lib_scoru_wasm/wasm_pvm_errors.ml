(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

open Tezos_webassembly_interpreter

type truncated_string = Truncated of string [@@ocaml.unboxed]

let messages_maximum_size = 128

let truncate_message msg =
  if String.length msg > messages_maximum_size then
    Truncated (String.sub msg 0 messages_maximum_size)
  else Truncated msg

type interpreter_error = {
  raw_exception : truncated_string;
  explanation : truncated_string option;
}

type fallback_cause =
  | Decode_cause of interpreter_error
  | Link_cause of truncated_string
  | Init_cause of interpreter_error

type t =
  | Decode_error of interpreter_error
  | Link_error of truncated_string
  | Init_error of interpreter_error
  | Eval_error of interpreter_error
  | Invalid_state of truncated_string
  | Unknown_error of truncated_string
  | Too_many_ticks
  | Too_many_reboots
  | No_fallback_kernel of fallback_cause

let decode_state_to_string_raw = function
  | Decode.Byte_vector_step -> "Byte_vector_step"
  | Instr_step -> "Instr_step"
  | Instr_block_step -> "Instr_block_step"
  | Block_step -> "Block_step"
  | Name_step -> "Name_step"
  | Func_type_step -> "Func_type_step"
  | Import_step -> "Import_step"
  | Export_step -> "Export_step"
  | Code_step -> "Code_step"
  | Elem_step -> "Elem_step"
  | Data_step -> "Data_step"
  | Module_step -> "Module_step"

let decode_state_to_string exn =
  decode_state_to_string_raw exn |> truncate_message

let init_state_to_string_raw = function
  | Eval.Init_step -> "Init_step"
  | Map_step -> "Map_step"
  | Map_concat_step -> "Map_concat_step"
  | Join_step -> "Join_step"
  | Section_step -> "Section_step"
  | Eval_const -> "Eval_const"
  | Create_global_step -> "Create_global_step"
  | Run_data_step -> "Run_data_step"

let init_state_to_string exn = init_state_to_string_raw exn |> truncate_message

let durable_exn_explanation_raw = function
  | Durable.Invalid_key path -> Some ("Invalid_key: " ^ path)
  | Durable.Value_not_found -> Some "Value not found"
  | Durable.Tree_not_found -> Some "Tree not found"
  | Durable.Durable_empty | Durable_storage.Durable_empty ->
      Some "Empty durable storage"
  | _ -> None

let durable_exn_explanation exn =
  durable_exn_explanation_raw exn |> Option.map truncate_message

let eval_state_to_string_raw = function
  | Eval.Invoke_step msg -> "Invoke_step: " ^ msg
  | Label_step -> "Label_step"
  | Frame_step -> "Frame_step"
  | Eval_step -> "Eval_step"

let eval_state_to_string exn = eval_state_to_string_raw exn |> truncate_message

let reveal_error_to_string_raw = function
  | Eval.Reveal_step -> "Reveal_step"
  | Reveal_hash_decoding msg ->
      Format.sprintf "Unknown error during hash decoding: %s" msg
  | Reveal_payload_decoding msg ->
      Format.sprintf "Unknown error during payload decoding: %s" msg

let reveal_error_to_string exn =
  reveal_error_to_string_raw exn |> truncate_message

let extract_interpreter_error exn =
  let open Tezos_lazy_containers in
  let raw_exception = Printexc.to_string exn |> truncate_message in
  match exn with
  (* The locations are removed during encoding, they won't be usable in practice. *)
  | Binary_exn.Decode_error.Error (_, explanation)
  | Binary_exn.Encode_error.Error (_, explanation)
  | Binary_exn.Floating_point.Error (_, explanation)
  | Valid.Invalid (_, explanation)
  | Eval.Link (_, explanation)
  | Eval.Trap (_, explanation)
  | Eval.Crash (_, explanation)
  | Eval.Exhaustion (_, explanation)
  | Import.Unknown (_, explanation) ->
      `Interpreter
        {raw_exception; explanation = Some (truncate_message explanation)}
  | Values.TypeError _ | Binary_exn.EOS | Binary_exn.Utf8
  | Lazy_map.UnexpectedAccess | Lazy_vector.Bounds | Lazy_vector.SizeOverflow
  | Chunked_byte_vector.Bounds | Chunked_byte_vector.SizeOverflow | Table.Type
  | Table.SizeLimit | Table.OutOfMemory | Table.Bounds | Table.SizeOverflow
  | Memory.Type | Memory.SizeLimit | Memory.OutOfMemory | Memory.Bounds
  | Memory.SizeOverflow | Global.Type | Global.NotMutable | Ixx.Overflow
  | Ixx.DivideByZero | Ixx.InvalidConversion | Input_buffer.Bounds
  | Input_buffer.SizeOverflow | Input_buffer.Cannot_store_an_earlier_message
  | Input_buffer.Dequeue_from_empty_queue ->
      `Interpreter {raw_exception; explanation = None}
  | Decode.Step_error state ->
      `Interpreter
        {raw_exception; explanation = Some (decode_state_to_string state)}
  | Eval.Init_step_error state ->
      `Interpreter
        {raw_exception; explanation = Some (init_state_to_string state)}
  | Eval.Evaluation_step_error state ->
      `Interpreter
        {raw_exception; explanation = Some (eval_state_to_string state)}
  | Eval.Reveal_error state ->
      `Interpreter
        {raw_exception; explanation = Some (reveal_error_to_string state)}
  | Eval.Missing_memory_0_export ->
      `Interpreter
        {
          raw_exception;
          explanation = Some (truncate_message "Module must export memory 0");
        }
  | Durable.Invalid_key _ | Durable.Value_not_found | Durable.Tree_not_found
  | Durable.Durable_empty | Durable_storage.Durable_empty ->
      `Interpreter {raw_exception; explanation = durable_exn_explanation exn}
  | _ -> `Unknown raw_exception

let invalid_state m = Invalid_state (truncate_message m)

let truncated_string_encoding =
  Data_encoding.(conv (fun (Truncated s) -> s) (fun s -> Truncated s) string)

let interpreter_error_encoding prefix =
  let open Data_encoding in
  conv
    (fun {raw_exception; explanation} -> (raw_exception, explanation))
    (fun (raw_exception, explanation) -> {raw_exception; explanation})
    (obj2
       (req (prefix ^ "_raw_exception") truncated_string_encoding)
       (req (prefix ^ "_explanation") (option truncated_string_encoding)))

let fallback_cause_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"Decode_cause"
        (interpreter_error_encoding "decode")
        (function Decode_cause cause -> Some cause | _ -> None)
        (fun cause -> Decode_cause cause);
      case
        (Tag 1)
        ~title:"Link_cause"
        (obj1 (req "link" truncated_string_encoding))
        (function Link_cause cause -> Some cause | _ -> None)
        (fun cause -> Link_cause cause);
      case
        (Tag 2)
        ~title:"Init_cause"
        (interpreter_error_encoding "init")
        (function Init_cause cause -> Some cause | _ -> None)
        (fun cause -> Init_cause cause);
    ]

let encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"Decode_error"
        (interpreter_error_encoding "decode")
        (function Decode_error err -> Some err | _ -> None)
        (fun err -> Decode_error err);
      case
        (Tag 1)
        ~title:"Link_error"
        (obj1 (req "link" truncated_string_encoding))
        (function Link_error err -> Some err | _ -> None)
        (fun err -> Link_error err);
      case
        (Tag 2)
        ~title:"Init_error"
        (interpreter_error_encoding "init")
        (function Init_error err -> Some err | _ -> None)
        (fun err -> Init_error err);
      case
        (Tag 3)
        ~title:"Eval_error"
        (interpreter_error_encoding "eval")
        (function Eval_error err -> Some err | _ -> None)
        (fun err -> Eval_error err);
      case
        (Tag 4)
        ~title:"Invalid_state"
        (obj1 (req "invalid_state" truncated_string_encoding))
        (function Invalid_state msg -> Some msg | _ -> None)
        (fun msg -> Invalid_state msg);
      case
        (Tag 5)
        ~title:"Unknown_error"
        (obj1 (req "unknown_error" truncated_string_encoding))
        (function Unknown_error exn -> Some exn | _ -> None)
        (fun exn -> Unknown_error exn);
      case
        (Tag 6)
        ~title:"Too_many_ticks"
        (constant "too_many_ticks")
        (function Too_many_ticks -> Some () | _ -> None)
        (fun () -> Too_many_ticks);
      case
        (Tag 7)
        ~title:"Too_many_reboots"
        (constant "too_many_reboots")
        (function Too_many_reboots -> Some () | _ -> None)
        (fun () -> Too_many_reboots);
      case
        (Tag 8)
        ~title:"No_fallback_kernel"
        (obj1 (req "no_kernel_fallback" fallback_cause_encoding))
        (function No_fallback_kernel cause -> Some cause | _ -> None)
        (fun cause -> No_fallback_kernel cause);
    ]

let link_error kind ~module_name ~item_name =
  match kind with
  | `Item ->
      Link_error
        (Format.sprintf "Unexpected import: %s.%s" module_name item_name
        |> truncate_message)
  | `Module ->
      Link_error
        (Format.sprintf "Unexpected module import: %s" module_name
        |> truncate_message)
