(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let spawn_command ?(path = Uses.path Constant.octez_codec) ?hooks arguments =
  Process.spawn path ?hooks arguments

let spawn_encode ?path ?hooks ~name json =
  spawn_command ?path ?hooks ["encode"; name; "from"; JSON.encode_u json]

let encode ?path ?hooks ~name json =
  let open Lwt.Infix in
  spawn_encode ?path ?hooks ~name json
  |> Process.check_and_read_stdout >|= String.trim

let spawn_decode ?path ?hooks ~name binary =
  spawn_command ?path ?hooks ["decode"; name; "from"; binary]

let decode ?path ?hooks ~name binary =
  let* json =
    String.trim binary
    |> spawn_decode ?path ?hooks ~name
    |> Process.check_and_read_stdout
  in
  return (JSON.parse ~origin:("octez-codec encode " ^ name) json)

let spawn_dump_encodings ?path () = spawn_command ?path ["dump"; "encodings"]

let dump_encodings ?path () =
  let* json = spawn_dump_encodings ?path () |> Process.check_and_read_stdout in
  return (JSON.parse ~origin:"octez-codec dump encodings" json)

let spawn_dump_encoding ?path ~id () =
  spawn_command ?path ["dump"; "encoding"; id]

let dump_encoding ?path ~id () =
  let* json =
    spawn_dump_encoding ?path ~id () |> Process.check_and_read_stdout
  in
  return (JSON.parse ~origin:"octez-codec dump encoding" json)

let spawn_describe_binary_schema ?path ~id () =
  spawn_command ?path ["describe"; id; "binary"; "schema"]

let spawn_describe_json_schema ?path ~id () =
  spawn_command ?path ["describe"; id; "json"; "schema"]

let describe_binary_schema ?path ~id () =
  let* output =
    spawn_describe_binary_schema ?path ~id () |> Process.check_and_read_stdout
  in
  return output

let describe_json_schema ?path ~id () =
  let* output =
    spawn_describe_json_schema ?path ~id () |> Process.check_and_read_stdout
  in
  return output
