(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

type mode = Accuser | Batcher | Custom | Maintenance | Observer | Operator

module Parameters = struct
  type persistent_state = {
    tezos_node : Node.t;
    client : Client.t;
    data_dir : string;
    runner : Runner.t option;
    rollup_id : string;
    operator : string option;
    batch_signer : string option;
    finalize_commitment_signer : string option;
    remove_commitment_signer : string option;
    dispatch_withdrawals_signer : string option;
    rejection_signer : string option;
    rollup_genesis : string;
    rpc_addr : string;
    allow_deposit : bool;
    dormant_mode : bool;
    mode : mode;
    mutable pending_ready : unit option Lwt.u list;
    mutable pending_level : (int * int option Lwt.u) list;
  }

  type 'a known = Unknown | Known of 'a

  type session_state = {mutable ready : bool; mutable level : int known}

  let base_default_name = "tx-rollup-node"

  let default_colors = Log.Color.[|FG.magenta; FG.yellow; FG.green; FG.cyan|]
end

open Parameters
include Daemon.Make (Parameters)

let rpc_addr node = node.persistent_state.rpc_addr

let data_dir node = node.persistent_state.data_dir

let endpoint node = "http://" ^ rpc_addr node

let operator node = node.persistent_state.operator

let spawn_command node =
  Process.spawn ~name:node.name ~color:node.color node.path

let add_option flag str_opt command =
  command @ match str_opt with None -> [] | Some o -> [flag; o]

let string_of_mode = function
  | Observer -> "observer"
  | Accuser -> "accuser"
  | Batcher -> "batcher"
  | Maintenance -> "maintenance"
  | Operator -> "operator"
  | Custom -> "custom"

let spawn_init_config node =
  spawn_command
    node
    (([
        "init";
        string_of_mode node.persistent_state.mode;
        "config";
        "for";
        node.persistent_state.rollup_id;
        "--data-dir";
        data_dir node;
        "--rollup-genesis";
        node.persistent_state.rollup_genesis;
        "--rpc-addr";
        rpc_addr node;
      ]
     @ if node.persistent_state.allow_deposit then ["--allow-deposit"] else [])
    |> add_option "--operator" @@ operator node
    |> add_option "--batch-signer" node.persistent_state.batch_signer
    |> add_option
         "--finalize-commitment-signer"
         node.persistent_state.finalize_commitment_signer
    |> add_option
         "--remove-commitment-signer"
         node.persistent_state.remove_commitment_signer
    |> add_option
         "--dispatch-withdrawals-signer"
         node.persistent_state.dispatch_withdrawals_signer
    |> add_option "--rejection-signer" node.persistent_state.rejection_signer)

let init_config node =
  let process = spawn_init_config node in
  let* output = Process.check_and_read_stdout process in
  match output =~* rex "Configuration written in ([^\n]*)" with
  | None -> failwith "Configuration initialization failed"
  | Some filename -> return filename

let check_event ?where node name promise =
  let* result = promise in
  match result with
  | None ->
      raise (Terminated_before_event {daemon = node.name; event = name; where})
  | Some x -> return x

let trigger_ready node value =
  let pending = node.persistent_state.pending_ready in
  node.persistent_state.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let set_ready node =
  (match node.status with
  | Not_running -> ()
  | Running status -> status.session_state.ready <- true) ;
  trigger_ready node (Some ())

let update_level node current_level =
  (match node.status with
  | Not_running -> ()
  | Running status -> (
      match status.session_state.level with
      | Unknown -> status.session_state.level <- Known current_level
      | Known old_level ->
          status.session_state.level <- Known (max old_level current_level))) ;
  let pending = node.persistent_state.pending_level in
  node.persistent_state.pending_level <- [] ;
  List.iter
    (fun ((level, resolver) as pending) ->
      if current_level >= level then
        Lwt.wakeup_later resolver (Some current_level)
      else
        node.persistent_state.pending_level <-
          pending :: node.persistent_state.pending_level)
    pending

let handle_event node {name; value} =
  match name with
  | "tx_rollup_node_is_ready.v0" -> set_ready node
  | "tx_rollup_node_tezos_block_processed.v0" -> (
      match JSON.(value |-> "level" |> as_int_opt) with
      | Some level -> update_level node level
      | None -> ())
  | _ -> ()

let wait_for_ready node =
  match node.status with
  | Running {session_state = {ready = true; _}; _} -> unit
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      let (promise, resolver) = Lwt.task () in
      node.persistent_state.pending_ready <-
        resolver :: node.persistent_state.pending_ready ;
      check_event node "tx_rollup_node_is_ready.v0" promise

let process node =
  match node.status with
  | Running {process; _} -> Some process
  | Not_running -> None

let wait_for_tezos_level node level =
  match node.status with
  | Running {session_state = {level = Known current_level; _}; _}
    when current_level >= level ->
      return current_level
  | Not_running | Running _ ->
      let (promise, resolver) = Lwt.task () in
      node.persistent_state.pending_level <-
        (level, resolver) :: node.persistent_state.pending_level ;
      check_event
        node
        "tx_rollup_node_tezos_block_processed.v0"
        ~where:("level >= " ^ string_of_int level)
        promise

let wait_for_full ?where node name filter =
  let (promise, resolver) = Lwt.task () in
  let current_events =
    String_map.find_opt name node.one_shot_event_handlers
    |> Option.value ~default:[]
  in
  node.one_shot_event_handlers <-
    String_map.add
      name
      (Event_handler {filter; resolver} :: current_events)
      node.one_shot_event_handlers ;
  let* result = promise in
  match result with
  | None ->
      raise (Terminated_before_event {daemon = node.name; event = name; where})
  | Some x -> return x

let event_from_full_event_filter filter json =
  let raw = get_event_from_full_event json in
  (* If [json] does not match the correct JSON structure, it
     will be filtered out, which will result in ignoring
     the current event.
     @see raw_event_from_event *)
  Option.bind raw (fun {value; _} -> filter value)

let wait_for ?where node name filter =
  wait_for_full ?where node name (event_from_full_event_filter filter)

let create ?(path = Constant.tx_rollup_node) ?runner ?data_dir
    ?(addr = "127.0.0.1") ?(dormant_mode = false) ?color ?event_pipe ?name mode
    ~rollup_id ~rollup_genesis ?operator ?batch_signer
    ?finalize_commitment_signer ?remove_commitment_signer
    ?dispatch_withdrawals_signer ?rejection_signer ?(allow_deposit = false)
    client tezos_node =
  let name = match name with None -> fresh_name () | Some name -> name in
  let rpc_addr =
    match String.rindex_opt addr ':' with
    | Some _ -> addr
    | None -> Printf.sprintf "%s:%d" addr (Port.fresh ())
  in
  let data_dir =
    match data_dir with None -> Temp.dir name | Some dir -> dir
  in
  let tx_node =
    create
      ?runner
      ~path
      ~name
      ?color
      ?event_pipe
      {
        tezos_node;
        data_dir;
        rollup_id;
        rpc_addr;
        rollup_genesis;
        runner;
        operator;
        batch_signer;
        finalize_commitment_signer;
        remove_commitment_signer;
        dispatch_withdrawals_signer;
        rejection_signer;
        allow_deposit;
        client;
        pending_ready = [];
        pending_level = [];
        dormant_mode;
        mode;
      }
  in
  on_event tx_node (handle_event tx_node) ;
  tx_node

let make_arguments node =
  [
    "--endpoint";
    Printf.sprintf
      "http://%s:%d"
      (Node.rpc_host node.persistent_state.tezos_node)
      (Node.rpc_port node.persistent_state.tezos_node);
  ]

let do_runlike_command node arguments =
  if node.status <> Not_running then
    Test.fail "Transaction rollup node %s is already running" node.name ;
  let on_terminate _status =
    trigger_ready node None ;
    unit
  in
  let arguments = make_arguments node @ arguments in
  run node {ready = false; level = Unknown} arguments ~on_terminate

let run node =
  do_runlike_command
    node
    [
      "--base-dir";
      Client.base_dir node.persistent_state.client;
      "run";
      "--data-dir";
      node.persistent_state.data_dir;
    ]

let change_signers ?operator ?batch_signer ?finalize_commitment_signer
    ?remove_commitment_signer ?dispatch_withdrawals_signer ?rejection_signer
    ?mode ?allow_deposit (tx_node : t) =
  let operator =
    Option.value operator ~default:tx_node.persistent_state.operator
  in
  let batch_signer =
    Option.value batch_signer ~default:tx_node.persistent_state.batch_signer
  in
  let finalize_commitment_signer =
    Option.value
      finalize_commitment_signer
      ~default:tx_node.persistent_state.finalize_commitment_signer
  in
  let remove_commitment_signer =
    Option.value
      remove_commitment_signer
      ~default:tx_node.persistent_state.remove_commitment_signer
  in
  let dispatch_withdrawals_signer =
    Option.value
      dispatch_withdrawals_signer
      ~default:tx_node.persistent_state.dispatch_withdrawals_signer
  in
  let rejection_signer =
    Option.value
      rejection_signer
      ~default:tx_node.persistent_state.rejection_signer
  in
  let mode = Option.value mode ~default:tx_node.persistent_state.mode in
  let allow_deposit =
    Option.value allow_deposit ~default:tx_node.persistent_state.allow_deposit
  in
  let tmp_tx_node =
    {
      tx_node with
      persistent_state =
        {
          tx_node.persistent_state with
          operator;
          batch_signer;
          finalize_commitment_signer;
          remove_commitment_signer;
          dispatch_withdrawals_signer;
          rejection_signer;
          mode;
          allow_deposit;
        };
    }
  in
  let* _ = init_config tmp_tx_node in
  unit

module Inbox = struct
  type l2_context_hash = {irmin_hash : string; tree_hash : string}

  type message = {
    message : JSON.t;
    result : JSON.t;
    l2_context_hash : l2_context_hash;
  }

  type t = {contents : message list; cumulated_size : int}
end

module Client = struct
  let raw_tx_node_rpc node ~url =
    let* rpc = RPC.Curl.get () in
    match rpc with
    | None -> assert false
    | Some curl ->
        let url = Printf.sprintf "%s/%s" (rpc_addr node) url in
        curl ~url

  let get_inbox ~tx_node ~block =
    let parse_l2_context_hash json =
      let irmin_hash = JSON.(json |-> "irmin_hash" |> as_string) in
      let tree_hash = JSON.(json |-> "tree_hash" |> as_string) in
      Inbox.{irmin_hash; tree_hash}
    in
    let parse_message json =
      let message = JSON.(json |-> "message") in
      let result = JSON.(json |-> "result") in
      let l2_context_hash =
        parse_l2_context_hash JSON.(json |-> "l2_context_hash")
      in
      Inbox.{message; result; l2_context_hash}
    in
    let parse_json json =
      let cumulated_size = JSON.(json |-> "cumulated_size" |> as_int) in
      let contents =
        JSON.(json |-> "contents" |> as_list) |> List.map parse_message
      in
      Inbox.{cumulated_size; contents}
    in
    let* json = raw_tx_node_rpc tx_node ~url:("block/" ^ block ^ "/inbox") in
    return (parse_json json)

  let get_balance ~tx_node ~block ~ticket_id ~tz4_address =
    let parse_json json =
      match JSON.(json |> as_int_opt) with
      | Some level -> level
      | None ->
          Test.fail "Cannot retrieve balance of tz4 address %s" tz4_address
    in
    let* json =
      raw_tx_node_rpc
        tx_node
        ~url:
          ("context/" ^ block ^ "/tickets/" ^ ticket_id ^ "/balance/"
         ^ tz4_address)
    in
    return (parse_json json)

  let get_queue ~tx_node = raw_tx_node_rpc tx_node ~url:"queue"

  let get_transaction_in_queue ~tx_node txh =
    raw_tx_node_rpc tx_node ~url:("queue/transaction/" ^ txh)

  let get_block ~tx_node ~block = raw_tx_node_rpc tx_node ~url:("block/" ^ block)

  let get_merkle_proof ~tx_node ~block ~message_pos =
    raw_tx_node_rpc
      tx_node
      ~url:("block/" ^ block ^ "/proof/message/" ^ message_pos)

  let get_ticket ~tx_node ~block ~ticket_id =
    raw_tx_node_rpc tx_node ~url:("context/" ^ block ^ "/tickets/" ^ ticket_id)

  let get_ticket_index ~tx_node ~block ~ticket_id =
    let parse_json json =
      match JSON.(json |> as_int_opt) with
      | Some level -> level
      | None -> Test.fail "Cannot retrieve ticket of %s" ticket_id
    in
    let* json =
      raw_tx_node_rpc
        tx_node
        ~url:("context/" ^ block ^ "/tickets/" ^ ticket_id ^ "/index")
    in
    return (parse_json json)
end
