(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
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

open Runnable.Syntax

type t = {
  name : string;
  path : string;
  dac_node : Dac_node.t;
  base_dir : string;
  color : Log.Color.t;
  runner : Runner.t option;
}

let next_name = ref 1

let path = "./octez-dac-client"

let fresh_name () =
  let index = !next_name in
  incr next_name ;
  "dac_client" ^ string_of_int index

let () = Test.declare_reset_function @@ fun () -> next_name := 1

let create ?runner ?name ?base_dir ?(color = Log.Color.FG.green) dac_node =
  let name = match name with None -> fresh_name () | Some name -> name in
  let base_dir =
    match base_dir with None -> Temp.dir ?runner name | Some dir -> dir
  in
  {name; path; dac_node; base_dir; color; runner}

let base_dir_arg dac_client = ["--base-dir"; dac_client.base_dir]

let spawn_command ?hooks dac_client command =
  let process =
    Process.spawn
      ?runner:dac_client.runner
      ~name:dac_client.name
      ~color:dac_client.color
      ?hooks
      dac_client.path
      (base_dir_arg dac_client @ command)
  in
  Runnable.{value = process; run = Process.check_and_read_stdout}

type output = Root_hash of Hex.t | Certificate of Hex.t

let send_payload_output raw_output =
  match raw_output =~* rex "Payload stored under root hash: ([0-9A-Fa-f]+)" with
  | Some hex -> Root_hash (`Hex hex)
  | None -> (
      match
        raw_output
        =~* rex
              "No certificate could be obtained.\n\
               Payload stored under root hash: ([0-9A-Fa-f]+)\n"
      with
      | Some hex -> Root_hash (`Hex hex)
      | None -> (
          match raw_output =~* rex "Certificate received: ([0-9A-Fa-f]+)\n" with
          | Some hex -> Certificate (`Hex hex)
          | None -> assert false))

let get_certificate_output raw_output =
  match raw_output =~* rex "No certificate known for ([0-9A-Fa-f]+)\n" with
  | Some _hex -> None
  | None -> (
      match raw_output =~* rex "Certificate received: ([0-9A-Fa-f]+)\n" with
      | Some hex -> Some (Certificate (`Hex hex))
      | None -> assert false)

let send_payload ?hooks ?threshold dac_client hex_payload =
  let coordinator_endpoint =
    Printf.sprintf
      "%s:%d"
      (Dac_node.rpc_host dac_client.dac_node)
      (Dac_node.rpc_port dac_client.dac_node)
  in
  let threshold_arg =
    match threshold with
    | None -> []
    | Some n -> ["--wait-for-threshold"; string_of_int n]
  in
  let*? process =
    spawn_command
      ?hooks
      dac_client
      ([
         "send";
         "payload";
         "to";
         "coordinator";
         coordinator_endpoint;
         "with";
         "content";
         Hex.show hex_payload;
       ]
      @ threshold_arg)
  in
  let* raw_output = Process.check_and_read_stdout process in
  Lwt.return @@ send_payload_output raw_output

let get_certificate ?hooks dac_client hex_root_hash =
  let coordinator_endpoint =
    Printf.sprintf
      "%s:%d"
      (Dac_node.rpc_host dac_client.dac_node)
      (Dac_node.rpc_port dac_client.dac_node)
  in
  let*? process =
    spawn_command
      ?hooks
      dac_client
      [
        "get";
        "certificate";
        "from";
        "coordinator";
        coordinator_endpoint;
        "for";
        "root";
        "hash";
        Hex.show hex_root_hash;
      ]
  in
  let* raw_output = Process.check_and_read_stdout process in
  Lwt.return @@ get_certificate_output raw_output
