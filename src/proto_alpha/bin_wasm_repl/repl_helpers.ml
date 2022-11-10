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

(* [print_durable ~depth tree] prints the keys from the durable storage and
   their value in their hexadecimal representation. *)
let print_durable ?(depth = 10) tree =
  let open Lwt_syntax in
  Test_encodings_util.Context.Tree.fold
    ~depth:(`Le depth)
    tree
    ["durable"]
    ~order:`Sorted
    ~init:()
    ~f:(fun key tree () ->
      let+ value = Test_encodings_util.Context.Tree.find tree [] in
      let value = Option.value ~default:(Bytes.create 0) value in
      Format.printf
        "/%s\n  %a\n%!"
        (String.concat "/" key)
        Hex.pp
        (Hex.of_bytes value))
