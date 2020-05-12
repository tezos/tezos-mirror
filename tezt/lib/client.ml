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

open Base

type t = {
  name : string;
  color : Log.Color.t;
  base_dir : string;
  node : Node.t option;
}

let next_name = ref 1

let fresh_name () =
  let index = !next_name in
  incr next_name ;
  "client" ^ string_of_int index

let () = Test.declare_reset_function @@ fun () -> next_name := 1

let create ?name ?(color = Log.Color.FG.blue) ?base_dir ?node () =
  let name = match name with None -> fresh_name () | Some name -> name in
  let base_dir =
    match base_dir with None -> Temp.dir name | Some dir -> dir
  in
  {name; color; base_dir; node}

let check_node_is_specified ?node command client =
  match (node, client.node) with
  | (None, None) ->
      Test.fail
        "%s has no node, cannot run command: %s"
        client.name
        (String.concat " " command)
  | (Some _, _) | (_, Some _) ->
      ()

let base_args ?node client =
  let node =
    match (node, client.node) with
    | (Some node, _) | (None, Some node) ->
        Some node
    | (None, None) ->
        None
  in
  ( match node with
  | None ->
      []
  | Some node ->
      ["-P"; string_of_int (Node.rpc_port node)] )
  @ ["--base-dir"; client.base_dir]

let run_command ?(needs_node = true) ?(admin = false) ?node client command =
  if needs_node then check_node_is_specified ?node command client ;
  Process.run
    ~name:client.name
    ~color:client.color
    (if admin then Constant.tezos_admin_client else Constant.tezos_client)
    (base_args ?node client @ command)

module Admin = struct
  let run_command = run_command ~admin:true

  let connect_address ?node ~peer client =
    run_command
      client
      ?node
      ["connect"; "address"; "[::1]:" ^ string_of_int (Node.net_port peer)]
end

let import_secret_key ?node client (key : Constant.key) =
  run_command
    ~needs_node:false
    ?node
    client
    ["import"; "secret"; "key"; key.alias; key.secret]

let activate_protocol ?node ?(protocol = Constant.alpha) ?(fitness = 1)
    ?(key = Constant.activator.alias) ?timestamp
    ?(timestamp_delay = 3600. *. 24. *. 365.) client =
  let timestamp =
    match timestamp with
    | Some timestamp ->
        timestamp
    | None ->
        let tm = Unix.gmtime (Unix.time () -. timestamp_delay) in
        Printf.sprintf
          "%04d-%02d-%02dT%02d:%02d:%02dZ"
          (tm.tm_year + 1900)
          (tm.tm_mon + 1)
          tm.tm_mday
          tm.tm_hour
          tm.tm_min
          tm.tm_sec
  in
  run_command
    client
    ?node
    [ "activate";
      "protocol";
      protocol.hash;
      "with";
      "fitness";
      string_of_int fitness;
      "and";
      "key";
      key;
      "and";
      "parameters";
      protocol.parameter_file;
      "--timestamp";
      timestamp ]

let bake_for ?node ?(key = Constant.bootstrap1.alias)
    ?(minimal_timestamp = true) client =
  run_command
    client
    ?node
    ( "bake" :: "for" :: key
    :: (if minimal_timestamp then ["--minimal-timestamp"] else []) )

let init ?name ?color ?base_dir ?node () =
  let client = create ?name ?color ?base_dir ?node () in
  let* () =
    Lwt_list.iter_s (import_secret_key client) Constant.all_secret_keys
  in
  return client
