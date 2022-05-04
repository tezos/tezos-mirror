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
  tx_node : Tx_rollup_node.t;
  base_dir : string;
  color : Log.Color.t;
}

let next_name = ref 1

let fresh_name () =
  let index = !next_name in
  incr next_name ;
  "client" ^ string_of_int index

let () = Test.declare_reset_function @@ fun () -> next_name := 1

let create ?name ?path ?base_dir ?(color = Log.Color.FG.green) tx_node =
  let name = match name with None -> fresh_name () | Some name -> name in
  let path =
    match path with None -> Constant.tx_rollup_client | Some p -> p
  in
  let base_dir =
    match base_dir with None -> Temp.dir name | Some dir -> dir
  in
  {name; path; tx_node; base_dir; color}

let base_dir_arg tx_client = ["--base-dir"; tx_client.base_dir]

let endpoint_arg tx_client =
  ["--endpoint"; Tx_rollup_node.endpoint tx_client.tx_node]

let optional_arg ~name f =
  Option.fold ~none:[] ~some:(fun x -> ["--" ^ name; f x])

let spawn_command ?hooks tx_client command =
  Process.spawn
    ~name:tx_client.name
    ~color:tx_client.color
    ?hooks
    tx_client.path
    (base_dir_arg tx_client @ endpoint_arg tx_client @ command)

let get_balance ?block tx_client ~tz4_address ~ticket_id =
  let* out =
    spawn_command
      tx_client
      (["get"; "balance"; "for"; tz4_address; "of"; ticket_id]
      @ optional_arg ~name:"block" Fun.id block)
    |> Process.check_and_read_stdout
  in
  let json = JSON.parse ~origin:"tx_client_get_balance" out in
  match JSON.(json |> as_int_opt) with
  | Some level -> Lwt.return level
  | None -> Test.fail "Cannot retrieve balance of tz4 address %s" tz4_address

let get_inbox ?(block = "head") tx_client =
  let* out =
    spawn_command tx_client ["get"; "inbox"; "for"; block]
    |> Process.check_and_read_stdout
  in
  Lwt.return out

let get_block tx_client ~block =
  let* out =
    spawn_command tx_client ["get"; "block"; block]
    |> Process.check_and_read_stdout
  in
  Lwt.return out

let craft_tx_transaction tx_client ~signer ?counter
    Rollup.Tx_rollup.{qty; destination; ticket} =
  let qty = Int64.to_string qty in
  let* out =
    spawn_command
      tx_client
      ([
         "craft";
         "tx";
         "transferring";
         qty;
         "from";
         signer;
         "to";
         destination;
         "for";
         ticket;
       ]
      @ optional_arg ~name:"counter" Int64.to_string counter)
    |> Process.check_and_read_stdout
  in
  Lwt.return out

let craft_tx_transfers tx_client Rollup.Tx_rollup.{counter; signer; contents} =
  let contents_json =
    let open Data_encoding in
    Json.construct (list Rollup.Tx_rollup.transfer_content_encoding) contents
    |> Json.to_string
  in
  let* out =
    spawn_command
      tx_client
      (["craft"; "tx"; "transfers"; "from"; signer; "using"; contents_json]
      @ optional_arg ~name:"counter" Int64.to_string counter)
    |> Process.check_and_read_stdout
  in
  Lwt.return out

let craft_tx_withdraw ?counter tx_client ~qty ~signer ~dest ~ticket =
  let qty = Int64.to_string qty in
  let* out =
    spawn_command
      tx_client
      ([
         "craft";
         "tx";
         "withdrawing";
         qty;
         "from";
         signer;
         "to";
         dest;
         "for";
         ticket;
       ]
      @ optional_arg ~name:"counter" Int64.to_string counter)
    |> Process.check_and_read_stdout
  in
  Lwt.return out

let craft_tx_batch tx_client ~batch ~signatures =
  let* out =
    spawn_command tx_client ["craft"; "batch"; "with"; batch; "for"; signatures]
    |> Process.check_and_read_stdout
  in
  Lwt.return out

let get_batcher_queue tx_client =
  let* out =
    spawn_command tx_client ["get"; "batcher"; "queue"]
    |> Process.check_and_read_stdout
  in
  Lwt.return out

let get_batcher_transaction tx_client ~transaction_hash =
  let* out =
    spawn_command tx_client ["get"; "batcher"; "transaction"; transaction_hash]
    |> Process.check_and_read_stdout
  in
  Lwt.return out

let inject_batcher_transaction tx_client ?expect_failure signed_tx_json =
  let* out =
    spawn_command tx_client ["inject"; "batcher"; "transaction"; signed_tx_json]
    |> Process.check_and_read_both ?expect_failure
  in
  Lwt.return out
