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

(* Testing
   -------
   Component:    Client
   Invocation:   dune exec tezt/tests/main.exe -- monitor operations
   Subject:      Test monitor_operations RPC
*)

let get_monitor_operations_hashes output_monitor =
  let open JSON in
  let output_monitor_length = String.length output_monitor in
  (* Format the output_monitor of the RPC as a valid json list *)
  let output_monitor =
    String.mapi
      (fun i c ->
        if c = '\n' then if i = output_monitor_length - 1 then ' ' else ','
        else c)
      output_monitor
  in
  let output_monitor = Format.sprintf "[%s]" output_monitor in
  let parsed_output =
    parse ~origin:"monitor_operation" output_monitor |> as_list
  in
  List.fold_left
    (fun ophs json ->
      let l = as_list json in
      List.fold_left
        (fun ophs op -> (op |-> "hash" |> as_string) :: ophs)
        ophs
        l)
    []
    parsed_output

(** Test the monitor operations RPC

   Scenario:

   + One node activates a protocol

   + Monitor_operations is called

   + Two transfers are done

   + Bake one block

   + Recover the hashes from the RPC response

   + Compare hashes from RPC and from mempool
 *)
let monitor_operations =
  Protocol.register_test
    ~__FILE__
    ~title:"Test monitor_operations RPC"
    ~tags:["monitor"; "operations"]
  @@ fun protocol ->
  (* Step 1 *)
  (* initialize the node and the client *)
  let* node, client = Client.init_with_protocol `Client ~protocol () in
  (* Step 2 *)
  (* call the monitor_operations RPC *)
  let monitor_path =
    sf
      "http://%s:%d/chains/main/mempool/monitor_operations"
      Constant.default_host
    @@ Node.rpc_port node
  in
  let proc_monitor = Process.spawn "curl" [monitor_path] in
  (* Step 3 *)
  (* Inject two transfer and recover their hashes *)
  let* () =
    Node_event_level.transfer_and_wait_for_injection
      node
      client
      10
      Constant.bootstrap1
      Constant.bootstrap2
  in
  let* () =
    Node_event_level.transfer_and_wait_for_injection
      node
      client
      20
      Constant.bootstrap2
      Constant.bootstrap3
  in
  let* ophs = Node_event_level.get_validated_operation_hash_list client in
  (* Step 4 *)
  (* Bake a block *)
  let* () = Node_event_level.bake_wait_log node client in
  (* Step 5 *)
  (* Recover the hashes from the RPC response *)
  let* output_monitor = Process.check_and_read_stdout proc_monitor in
  let monitor_ophs = get_monitor_operations_hashes output_monitor in
  (* Step 6 *)
  (* Compare hashes from RPC and from mempool *)
  let not_included_operations =
    List.filter (fun oph -> not @@ List.mem oph monitor_ophs) ophs
  in
  if not_included_operations = [] then unit
  else
    let error_msg =
      Format.asprintf
        "Some operations in mempool have not been monitored: %a"
        (Format.pp_print_list Format.pp_print_string)
        not_included_operations
    in
    Test.fail "%s" error_msg

let register ~protocols = monitor_operations protocols
