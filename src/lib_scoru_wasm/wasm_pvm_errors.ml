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

open Tezos_webassembly_interpreter

type interpreter_error = {raw_exception : string; explanation : string option}

type t =
  | Decode_error of interpreter_error
  | Init_error of interpreter_error
  | Eval_error of interpreter_error
  | Invalid_state of string
  | Unknown_error of string

let decode_state_to_string = function
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

let eval_state_to_string = function
  | Eval.Init_step -> "Init_step"
  | Map_step -> "Map_step"
  | Map_concat_step -> "Map_concat_step"
  | Join_step -> "Join_step"
  | Section_step -> "Section_step"

let extract_interpreter_error exn =
  let open Lazy_containers in
  let raw_exception = Printexc.to_string exn in
  match exn with
  (* The locations are removed during encoding, they won't be usable in practice. *)
  | Binary_exn.Decode_error.Error (_, explanation)
  | Binary_exn.Encode_error.Error (_, explanation)
  | Valid.Invalid (_, explanation)
  | Eval.Link (_, explanation)
  | Eval.Trap (_, explanation)
  | Eval.Crash (_, explanation)
  | Eval.Exhaustion (_, explanation)
  | Import.Unknown (_, explanation) ->
      `Interpreter {raw_exception; explanation = Some explanation}
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
        {raw_exception; explanation = Some (eval_state_to_string state)}
  | Eval.Missing_memory_0_export ->
      `Interpreter
        {raw_exception; explanation = Some "Module must export memory 0"}
  | _ -> `Unknown raw_exception

let encoding =
  let open Data_encoding in
  let interpreter_error_encoding prefix =
    conv
      (fun {raw_exception; explanation} -> (raw_exception, explanation))
      (fun (raw_exception, explanation) -> {raw_exception; explanation})
      (obj2
         (req (prefix ^ "_raw_exception") string)
         (req (prefix ^ "_explanation") (option string)))
  in
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
        ~title:"Init_error"
        (interpreter_error_encoding "init")
        (function Init_error err -> Some err | _ -> None)
        (fun err -> Init_error err);
      case
        (Tag 2)
        ~title:"Eval_error"
        (interpreter_error_encoding "eval")
        (function Eval_error err -> Some err | _ -> None)
        (fun err -> Eval_error err);
      case
        (Tag 3)
        ~title:"Invalid_state"
        (obj1 (req "invalid_state" string))
        (function Invalid_state msg -> Some msg | _ -> None)
        (fun msg -> Invalid_state msg);
      case
        (Tag 4)
        ~title:"Unknown_error"
        (obj1 (req "unknown_error" string))
        (function Unknown_error exn -> Some exn | _ -> None)
        (fun exn -> Unknown_error exn);
    ]

let link_error kind ~module_name ~item_name =
  match kind with
  | `Item ->
      Invalid_state
        (Format.sprintf "Unexpected import: %s.%s" module_name item_name)
  | `Module ->
      Invalid_state (Format.sprintf "Unexpected module import: %s" module_name)
