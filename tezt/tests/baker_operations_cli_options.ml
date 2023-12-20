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

(* Testing
   -------
   Component:    Baker daemon
   Invocation:   dune exec tezt/tests/main.exe -- --file baker_operations_cli_options.ml
   Subject:      Test CLI flags of the baker daemon
*)

(* Simple tests to check support for the following operations-related options
   for baking
   - --ignore-node-mempool
   - --operations-pool [file|uri]
   - --record-state *)

type http_path_mapping = Present | Absent

(* Straight from:
   https://github.com/mirage/ocaml-cohttp#compile-and-execute-with-dune-1 *)
let http_server ~port ~stop file presence () =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let color = Log.Color.BG.cyan in
  let file_basename = Filename.basename file in
  let callback _conn req _body =
    let uri = req |> Request.uri in
    let path = Uri.path uri in
    let meth = req |> Request.meth in
    Log.debug
      ~color
      "[http_server] >>> %s %s (path: %s)"
      (Code.string_of_method meth)
      (Uri.to_string uri)
      path ;
    match meth with
    | `GET -> (
        (* remove '/' prefix *)
        let path =
          if String.get path 0 = '/' then String.(sub path 1 (length path - 1))
          else path
        in
        match (String.equal path file_basename, presence) with
        | true, Present ->
            let body = Base.read_file file in
            Log.debug ~color "[http_server] <<< OK: %s" body ;
            Server.respond_string ~status:`OK ~body ()
        | true, Absent ->
            Log.debug ~color "[http_server] <<< Not found" ;
            Server.respond_string ~status:`Not_found ~body:"" ()
        | _ ->
            let body =
              sf "Unexpected HTTP GET call for unmapped path %s" path
            in
            Log.debug ~color "[http_server] <<< Error: %s" body ;
            Server.respond_string ~status:`Internal_server_error ~body ())
    | _ ->
        let body =
          sf "Unexpected HTTP call with method %s" (Code.string_of_method meth)
        in
        Log.debug ~color "[http_server] <<< Error: %s" body ;
        Server.respond_string ~status:`Internal_server_error ~body ()
  in
  Log.debug
    ~color
    "[http_server] starting, mapping %s to %s"
    file_basename
    (match presence with Present -> "200" | Absent -> "404") ;
  let stop =
    let* () = stop in
    Log.debug ~color "[http_server] stopping" ;
    unit
  in
  let* () =
    Server.create ~stop ~mode:(`TCP (`Port port)) (Server.make ~callback ())
  in
  unit

(* Check that a transfer injected into the node is dutifully ignored
   when baking with --ignore-node-mempool *)
let test_ignore_node_mempool =
  Protocol.register_test
    ~__FILE__
    ~title:"Ignore node mempool"
    ~tags:[Tag.layer1; "ignore"; "node"; "mempool"]
  @@ fun protocol ->
  let* node, client = Client.init_with_protocol ~protocol `Client () in
  let sender = Constant.bootstrap4 in
  let* balance0 = Client.get_balance_for ~account:sender.alias client in
  let amount = Tez.of_int 2 in
  let fee = Tez.of_int 1 in
  let* (`OpHash oph) =
    Operation.Manager.(
      inject
        [
          make
            ~fee:(Tez.to_mutez fee)
            ~source:sender
            (transfer
               ~dest:Constant.bootstrap5
               ~amount:(Tez.to_mutez amount)
               ());
        ])
      client
  in
  let* () = Client.bake_for_and_wait ~ignore_node_mempool:true client in
  let* mempool = Mempool.get_mempool client in
  Mempool.check_mempool ~validated:[oph] mempool ;
  let* balance1 = Client.get_balance_for ~account:sender.alias client in
  Check.(
    (balance1 = balance0)
      Tez.typ
      ~__LOC__
      ~error_msg:
        "Expected the balance to be unchanged, but got %L instead of %R") ;
  (* Check that a transfer injected, then ignored, can be injected at the
     next block *)
  let* () = Client.bake_for_and_wait ~ignore_node_mempool:false client in
  let* mempool = Mempool.get_mempool client in
  Mempool.check_mempool ~validated:[] mempool ;
  let* balance2 = Client.get_balance_for ~account:sender.alias client in
  let* level = Node.get_level node in
  Check.(
    (balance2 = Tez.(balance0 - amount - fee))
      Tez.typ
      ~__LOC__
      ~error_msg:"Expected the balance of sender to be %R, but got %L" ;
    (level = 3) int ~__LOC__ ~error_msg:"Expected level %R, got %L") ;
  unit

let with_http_server file presence f =
  let port = Port.fresh () in
  let http_server_stop, http_server_stopper = Lwt.wait () in
  let (http_server_p : unit Lwt.t) =
    http_server ~port ~stop:http_server_stop file presence ()
  in
  Lwt.finalize
    (fun () ->
      let base_uri = sf "http://localhost:%d/" port in
      f base_uri)
    (fun () ->
      Lwt.wakeup_later http_server_stopper () ;
      let* () = http_server_stop in
      let* () = http_server_p in
      unit)

let register_external_mempool ~title ~tags ~mempool body protocols =
  let title = "external operations, " ^ title in
  let tags = ["baker"; "external_operations"] @ tags in
  Protocol.register_test
    ~__FILE__
    ~title:(title ^ " file")
    ~tags:(tags @ ["file"])
    (fun protocol ->
      let* node, client = Client.init_with_protocol ~protocol `Client () in
      let* mempool_file, _presence = mempool node client in
      body node client mempool_file)
    protocols ;
  Protocol.register_test
    ~__FILE__
    ~title:(title ^ " http")
    ~tags:(tags @ ["http"])
    (fun protocol ->
      let* node, client = Client.init_with_protocol ~protocol `Client () in
      let* mempool_file, presence = mempool node client in
      let basename = Filename.basename mempool_file in
      with_http_server mempool_file presence @@ fun base_uri ->
      body node client (base_uri ^ basename))
    protocols

let all_empty block =
  let open JSON in
  block |-> "operations" |> as_list
  |> List.for_all @@ fun l ->
     l |> as_list |> function [] -> true | _ -> false

let only_has_consensus block =
  let open JSON in
  List.for_all
    Fun.id
    (block |-> "operations" |> as_list
    |> List.mapi @@ fun i l ->
       if i = 0 then true
       else l |> as_list |> function [] -> true | _ -> false)

let check_block_all_empty ~__LOC__ client =
  let* head = Client.RPC.call client @@ RPC.get_chain_block () in
  Check.is_true
    (all_empty head)
    ~__LOC__
    ~error_msg:"Expected an empty operation list." ;
  unit

let check_block_only_has_consensus ?block ~__LOC__ client =
  let* head = Client.RPC.call client @@ RPC.get_chain_block ?block () in
  Check.is_true
    (only_has_consensus head)
    ~__LOC__
    ~error_msg:"Expected an empty operation list." ;
  unit

let test_bake_empty_operations protocols =
  [
    ( "empty",
      fun _node _client -> return (Client.empty_mempool_file (), Present) );
    ( "absent",
      fun _node _client ->
        let absent_operations_file = "absent_operations_file.json" in
        Check.file_not_exists ~__LOC__ absent_operations_file ;
        return (absent_operations_file, Absent) );
  ]
  |> List.iter @@ fun (description, mempool) ->
     register_external_mempool
       ~title:("missing operations (" ^ description ^ ")")
       ~mempool
       ~tags:[Tag.layer1; "empty"]
       (fun node client mempool ->
         let* level = Node.get_level node in
         let* () = Client.bake_for_and_wait ~mempool client in
         let* new_level = Node.get_level node in
         Check.(
           (new_level = level + 1)
             int
             ~__LOC__
             ~error_msg:"Expected level %R, got %L") ;
         let* () = check_block_all_empty ~__LOC__ client in
         unit)
       protocols

type mempool_op = {branch : string; contents : JSON.t list; signature : string}

let get_operations client =
  let to_op validated_op =
    JSON.
      {
        branch = validated_op |-> "branch" |> as_string;
        contents = validated_op |-> "contents" |> as_list;
        signature = validated_op |-> "signature" |> as_string;
      }
  in
  let* mempool =
    Client.RPC.call client
    @@ RPC.get_chain_mempool_pending_operations ~version:"2" ()
  in
  return JSON.(mempool |-> "validated" |> as_list |> List.map to_op)

let encode_operations ops =
  let encode_operation op =
    `O
      [
        ("branch", `String op.branch);
        ("contents", `A (List.map JSON.unannotate op.contents));
        ("signature", `String op.signature);
      ]
  in
  let json_u = `A (List.map encode_operation ops) in
  JSON.annotate ~origin:"operations" json_u

(* Construct a transaction over the current state, put it into a
   file, and bake it into the chain through --operations-pool
   option. *)
let test_bake_singleton_operations =
  let amount = Tez.of_int 2 in
  let fee = Tez.of_int 1 in
  let sender = Constant.bootstrap4.alias in
  let mempool _node client =
    (* Construct a transaction over the current state, put it into a
       file. *)
    Log.info "Create transaction" ;
    let* () =
      Client.transfer
        ~amount
        ~fee
        ~giver:sender
        ~receiver:Constant.bootstrap3.alias
        client
    in
    let* pending_ops = get_operations client in
    Check.(
      (List.length pending_ops = 1)
        int
        ~__LOC__
        ~error_msg:"Expected exactly one pending ops, got %L") ;
    Check.(
      (List.length (List.nth pending_ops 0).contents = 1)
        int
        ~__LOC__
        ~error_msg:"Expected contents to contain exactly one element, got %L") ;
    let singleton_operations = Temp.file "singleton_operations.json" in
    JSON.encode_to_file singleton_operations (encode_operations pending_ops) ;
    return (singleton_operations, Present)
  in
  register_external_mempool
    ~title:"singleton operation"
    ~tags:[Tag.layer1; "singleton"]
    ~mempool
  @@ fun _node client mempool ->
  (* Bake the file obtained through mempool, and note the balance diff it generates *)
  Log.info "Bake singleton file" ;
  let* balance0 = Client.get_balance_for ~account:sender client in
  let* () =
    Client.bake_for_and_wait ~ignore_node_mempool:false ~mempool client
  in
  let* balance1 = Client.get_balance_for ~account:sender client in
  Check.(
    (balance1 = Tez.(balance0 - amount - fee))
      Tez.typ
      ~__LOC__
      ~error_msg:"Expected the new balance difference (%L) to be equal (%R)") ;
  unit

(* Test adding an external operations source (file) to a baker
   daemon *)
let test_baker_external_operations =
  Protocol.register_test
    ~__FILE__
    ~title:"Baker external operations"
    ~tags:[Tag.layer1; "baker"; "external"; "operations"]
    ~uses:(fun protocol -> [Protocol.baker protocol])
  @@ fun protocol ->
  Log.info "Init" ;
  let node_args = Node.[Synchronisation_threshold 0] in
  (* We define minimal_block_delay and delay_increment_per_round to 1
     as it is the lowest value that is accepted by the protocol that
     minimizes the test duration. *)
  let minimal_block_delay = 1 in
  let delay_increment_per_round = 1 in
  let parameters =
    [
      (["minimal_block_delay"], `String_of_int minimal_block_delay);
      (["delay_increment_per_round"], `String_of_int delay_increment_per_round);
    ]
  in
  let* parameter_file =
    Protocol.write_parameter_file ~base:(Right (protocol, None)) parameters
  in
  let* node, client =
    Client.init_with_protocol
      ~nodes_args:node_args
      ~parameter_file
      ~timestamp:Now
      ~protocol
      `Client
      ()
  in
  (* Generate a transfer operation and save it to a file *)
  Log.info "Generate operations" ;
  let keys =
    Array.map
      (fun Account.{public_key_hash; _} -> public_key_hash)
      Account.Bootstrap.keys
    |> Array.to_list
  in
  let* () = Client.bake_for_and_wait ~keys client in
  let* () =
    Client.transfer
      ~amount:(Tez.of_int 3)
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap3.alias
      client
  in
  let* () = Client.bake_for_and_wait ~keys client in
  let* () = Client.bake_for_and_wait ~keys client in
  let* level = Node.get_level node in
  Check.((level = 4) int ~__LOC__ ~error_msg:"Expected level %R, got %L") ;
  let* pending_ops = get_operations client in
  Check.(
    (List.length pending_ops = 0)
      int
      ~__LOC__
      ~error_msg:"Expected exactly zero pending ops, got %L") ;
  let transfer_value = Tez.of_int 2 in
  let* () =
    Client.transfer
      ~amount:transfer_value
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap3.alias
      client
  in
  let* pending_ops = get_operations client in
  let operations_pool = Temp.file "operations.json" in
  JSON.encode_to_file operations_pool (encode_operations pending_ops) ;
  (* Cleanup the node's mempool. Forget about the last transfer *)
  Log.info "Terminate sandbox" ;
  let* () = Node.terminate node in
  (* Restart the node and add a baker daemon *)
  Log.info "Start baker" ;
  let* () = Node.run node node_args in
  (* The baker may propose at level 4 or 5 depending on whether the
     round 0 at level 4 has ended or not. We make sure that round 0
     has ended by waiting the duration of round 0 (and a bit more) --
     that is minimal_block_delay + delay_increment_per_round (2
     second). In this way we are sure the operation will be in the
     block (re-proposed) at level 4. *)
  let* () =
    Lwt_unix.sleep
      (float_of_int (minimal_block_delay + delay_increment_per_round))
  in
  let* _baker = Baker.init ~protocol ~operations_pool node client in
  (* Wait until we have seen enough blocks. This should not take much time. *)
  Log.info "Wait until high enough level" ;
  let* (_ : int) = Node.wait_for_level node (level + 1) in
  (* Check that block exactly contains the operations that we put into
     our operations file *)
  Log.info "Check block baked" ;
  let* block =
    Client.RPC.call client
    @@ RPC.get_chain_block ~block:(string_of_int level) ()
  in
  let manager_ops = JSON.(block |-> "operations" |=> 3) in
  Check.(
    (manager_ops |> JSON.as_list |> List.length = 1)
      int
      ~__LOC__
      ~error_msg:"Expected exactly %R manager operations, got %L") ;
  (* Check that block is empty of operations *)
  let amount =
    JSON.(manager_ops |=> 0 |-> "contents" |=> 0 |-> "amount" |> as_int)
  in
  Check.(
    (Tez.of_mutez_int amount = transfer_value)
      Tez.typ
      ~__LOC__
      ~error_msg:"Expected baked block to have an amount of %R, got %L") ;
  Log.info "Check block after baked" ;
  let level_succ = level + 1 in
  let* (_ : int) = Node.wait_for_level node level_succ in
  let* () =
    check_block_only_has_consensus
      ~__LOC__
      ~block:(string_of_int level_succ)
      client
  in
  unit

(* Test that baking a block results in the generation of the
   "record_state" file in the client directory or not, depending
   on the [state_recorder] input argument. *)
let test_baker_state_recorder protocol state_recorder =
  let* node, client =
    Client.init_with_protocol ~timestamp:Now ~protocol `Client ()
  in
  let keys =
    Array.map
      (fun Account.{public_key_hash; _} -> public_key_hash)
      Account.Bootstrap.keys
    |> Array.to_list
  in
  (* Bake an empty block *)
  let* () = Client.bake_for_and_wait ~keys client in
  let* level = Node.get_level node in
  (* Initialise a baker *)
  let* _baker =
    if state_recorder then Baker.init ~protocol ~state_recorder node client
    else Baker.init ~protocol node client
  in
  (* Wait for chain to process the empty block *)
  let* (_ : int) = Node.wait_for_level node (level + 1) in
  (* Obtain the "record_state" file name *)
  let* chain_id = Client.RPC.call client (RPC.get_chain_chain_id ()) in
  let chain_id =
    Tezos_crypto.Hashed.Chain_id.to_short_b58check
      (Tezos_crypto.Hashed.Chain_id.of_b58check_exn chain_id)
  in
  let state_recorder_file = chain_id ^ "_baker_state" in
  let base_dir = Client.base_dir client in
  let dir_contents = Array.to_list @@ Sys.readdir base_dir in
  Check.(
    (List.mem state_recorder_file dir_contents = state_recorder)
      bool
      ~__LOC__
      ~error_msg:
        "The state_recorder file presence: %L, but its presence should have \
         been: %R") ;
  unit

let test_baker_state_recorder_memory =
  Protocol.register_test
    ~__FILE__
    ~title:"Baker state recorder - memory case"
    ~tags:[Tag.layer1; "baker"; "state"; "recorder"; "memory"]
    ~uses:(fun protocol -> [Protocol.baker protocol])
  @@ fun protocol -> test_baker_state_recorder protocol false

let test_baker_state_recorder_filesystem =
  Protocol.register_test
    ~__FILE__
    ~title:"Baker state recorder - filesystem case"
    ~tags:[Tag.layer1; "baker"; "state"; "recorder"; "filesystem"]
    ~uses:(fun protocol -> [Protocol.baker protocol])
  @@ fun protocol -> test_baker_state_recorder protocol true

let register ~protocols =
  test_ignore_node_mempool protocols ;
  test_bake_empty_operations protocols ;
  test_bake_singleton_operations protocols ;
  test_baker_external_operations protocols ;
  test_baker_state_recorder_memory protocols ;
  test_baker_state_recorder_filesystem protocols
