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
  let* path_exists = Encodings_util.Context.Tree.mem_tree tree durable_path in
  if path_exists then
    Encodings_util.Context.Tree.fold
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
          let+ value = Encodings_util.Context.Tree.find tree [] in
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

type printable_value_kind = [`I32 | `I64 | `Hex | `String]

let integer_value_kind_of_string = function
  | "int32" | "i32" -> Some `I32
  | "int64" | "i64" -> Some `I64
  | _ -> None

let value_kind_length = function `I32 -> 4 | `I64 -> 8

let printable_value_kind_of_string = function
  | "hex" -> Some `Hex
  | "string" -> Some `String
  | k -> integer_value_kind_of_string k

module type NUMERICAL = sig
  type t

  val zero : t

  val name : string

  (* Number of bytes to contain the value *)
  val bytes_length : int

  val of_int : int -> t

  val shift_left : t -> int -> t

  val shift_right_logical : t -> int -> t

  val logor : t -> t -> t
end

module Int32 = struct
  include Int32

  let name = "int32"

  let bytes_length = 4
end

module Int64 = struct
  include Int64

  let name = "int64"

  let bytes_length = 8
end

(* [integer_from_small_endian (module Num) bytes] reads each [char] of bytes and
   returns the corresponding integer according to its representation in
   [Num].
*)
let integer_from_small_endian (type t) (module N : NUMERICAL with type t = t)
    bytes : (t, string) result =
  if N.bytes_length <> String.length bytes then
    Error
      (Format.sprintf "%s values must be %d bytes long" N.name N.bytes_length)
  else
    Ok
      (String.fold_left
         (fun value byte ->
           let v = Char.code byte |> N.of_int in
           N.(
             logor
               (shift_right_logical value 8)
               (shift_left v ((bytes_length - 1) * 8))))
         N.zero
         bytes)

let print_wasm_encoded_value kind value =
  let open Result_syntax in
  match kind with
  | `I32 ->
      let+ i = integer_from_small_endian (module Int32) value in
      Int32.to_string i
  | `I64 ->
      let+ i = integer_from_small_endian (module Int64) value in
      Int64.to_string i
  | `Hex -> return (Format.asprintf "%a" Hex.pp (Hex.of_string value))
  | `String -> return value
