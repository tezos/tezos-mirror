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

let spawn_command ?(path = Uses.path Constant.octez_protocol_compiler) ?hooks
    arguments =
  Process.spawn path ?hooks arguments

let spawn_compile ?path ?hooks ?(hash_only = false) proto_dir =
  spawn_command
    ?path
    ?hooks
    ((if hash_only then ["-hash-only"] else []) @ [proto_dir])

let compile ?path ?hooks ?(hash_only = false) proto_dir =
  let* output =
    spawn_compile ?path ?hooks ~hash_only proto_dir
    |> Process.check_and_read_stdout
  in
  if hash_only then return (String.trim output)
  else
    match output =~* rex "Success: ([a-zA-Z0-9]+)" with
    | Some hash -> return hash
    | None ->
        Test.fail "Couldn't parse output from compiling %s: %s" proto_dir output
