(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type t = {
  name : string;
  path : string;
  sc_node : Sc_rollup_node.t;
  base_dir : string;
  color : Log.Color.t;
}

let next_name = ref 1

let fresh_name () =
  let index = !next_name in
  incr next_name ;
  "client" ^ string_of_int index

let () = Test.declare_reset_function @@ fun () -> next_name := 1

let create ?name ?path ?base_dir ?(color = Log.Color.FG.green) sc_node =
  let name = match name with None -> fresh_name () | Some name -> name in
  let path =
    match path with None -> Constant.sc_rollup_client | Some p -> p
  in
  let base_dir =
    match base_dir with None -> Temp.dir name | Some dir -> dir
  in
  {name; path; sc_node; base_dir; color}

let base_dir_arg sc_client = ["--base-dir"; sc_client.base_dir]

let endpoint_arg sc_client =
  ["--endpoint"; Sc_rollup_node.endpoint sc_client.sc_node]

let spawn_command ?hooks sc_client command =
  Process.spawn
    ~name:sc_client.name
    ~color:sc_client.color
    ?hooks
    sc_client.path
    (base_dir_arg sc_client @ endpoint_arg sc_client @ command)

let sc_rollup_address sc_client =
  let* out =
    spawn_command sc_client ["get"; "sc"; "rollup"; "address"]
    |> Process.check_and_read_stdout
  in
  return (String.trim out)
