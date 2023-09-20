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

(* [error loc category msg] fails with the location of an error and a message,
   returned by either the parser of the typechecker of the WASM reference
   interpreter. *)
let error at category msg =
  failwith "%s: %s: %s\n%!" (Source.string_of_region at) category msg

(* [trap_exn f] runs [f] and lifts it into the Lwt error monad. It traps any
   exception. *)
let trap_exn f =
  let open Lwt_result_syntax in
  let open Tezos_webassembly_interpreter in
  let open Tezos_webassembly_interpreter_extra in
  let print_unknown ppf e =
    Format.fprintf ppf "Unknown exception: %s" (Printexc.to_string e)
  in
  Lwt.catch
    (fun () ->
      let*! v = f () in
      return v)
    (function
      | Decode.Code (at, msg) -> error at "decoding error" msg
      | Parse.Syntax (at, msg) -> error at "syntax error" msg
      | Valid.Invalid (at, msg) -> error at "invalid module" msg
      | Import.Unknown (at, msg) -> error at "link failure" msg
      | Eval.Link (at, msg) -> error at "link failure" msg
      | Eval.Trap (at, msg) -> error at "runtime trap" msg
      | Eval.Exhaustion (at, msg) -> error at "resource exhaustion" msg
      | Eval.Crash (at, msg) -> error at "runtime crash" msg
      | Encode.Code (at, msg) -> error at "encoding error" msg
      | exn -> failwith "%a" (Data_encoding.Json.print_error ~print_unknown) exn)

(* [read_file file] reads a file and returns its content. This version prevent
   channel manipulation and takes care of opening and closing it. *)
let read_file file =
  let open Lwt_syntax in
  Lwt_io.(
    with_file ~mode:Input file (fun ic ->
        let* len = length ic in
        let buffer = Bytes.make (Int64.to_int len) '\000' in
        let+ () = read_into_exactly ic buffer 0 (Int64.to_int len) in
        Bytes.to_string buffer))

type integer_value_kind = [`U16 | `I32 | `I64 | `U128 | `U256]

type printable_value_kind = [integer_value_kind | `Hex | `String]

let integer_value_kind_of_string = function
  | "uint16" | "u16" -> Some `U16
  | "int32" | "i32" -> Some `I32
  | "int64" | "i64" -> Some `I64
  | "uint128" | "u128" -> Some `U128
  | "uint256" | "u256" -> Some `U256
  | _ -> None

let integer_value_kind_to_string = function
  | `U16 -> "u16"
  | `I32 -> "i32"
  | `I64 -> "i64"
  | `U128 -> "u128"
  | `U256 -> "u256"

let value_kind_length = function
  | `U16 -> 2
  | `I32 -> 4
  | `I64 -> 8
  | `U128 -> 16
  | `U256 -> 32

let printable_value_kind_of_string = function
  | "hex" -> Some `Hex
  | "string" -> Some `String
  | k -> integer_value_kind_of_string k

let print_wasm_encoded_value (kind : printable_value_kind) value =
  let open Result_syntax in
  match kind with
  | `Hex -> return (Format.asprintf "%a" Hex.pp (Hex.of_string value))
  | `String -> return value
  | #integer_value_kind as kind ->
      let kind_length = value_kind_length kind in
      if kind_length <> String.length value then
        Error
          (Format.sprintf
             "%s values must be %d bytes long"
             (integer_value_kind_to_string kind)
             kind_length)
      else return (Z.of_bits value |> Z.to_string)
