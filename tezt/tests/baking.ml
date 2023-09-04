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

(* Testing
   -------
   Component:    Baking
   Invocation:   dune exec tezt/tests/main.exe -- --file baking.ml
   Subject:      Test the baker
*)

(* ------------------------------------------------------------------------- *)
(* Typedefs *)

open Tezos_crypto
open Tezos_crypto.Hashed

type operation = {shell_header : branch; protocol_data : protocol_data}

and branch = {branch : Block_hash.t}

and protocol_data = {
  contents : operation_content list;
  signature : string option;
}

and operation_content = {
  kind : string;
  source : string;
  fee : string;
  counter : string;
  gas_limit : string;
  storage_limit : string;
  amount : string;
  destination : string;
}

type mempool_operation =
  | Mempool_operation of {
      protocol : Protocol_hash.t;
      shell_header : branch;
      protocol_data : protocol_data;
    }

type mempool = (Operation_hash.t * mempool_operation) list

(* ------------------------------------------------------------------------- *)
(* Operation-related encodings *)

let branch_encoding : branch Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {branch} -> branch)
    (fun branch -> {branch})
    (obj1 (req "branch" Block_hash.encoding))

let signature_encoding : string Data_encoding.t = Data_encoding.string

let operation_content_encoding : operation_content Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {
           kind;
           source;
           fee;
           counter;
           gas_limit;
           storage_limit;
           amount;
           destination;
         } ->
      (kind, source, fee, counter, gas_limit, storage_limit, amount, destination))
    (fun ( kind,
           source,
           fee,
           counter,
           gas_limit,
           storage_limit,
           amount,
           destination ) ->
      {
        kind;
        source;
        fee;
        counter;
        gas_limit;
        storage_limit;
        amount;
        destination;
      })
    (obj8
       (req "kind" string)
       (req "source" string)
       (req "fee" string)
       (req "counter" string)
       (req "gas_limit" string)
       (req "storage_limit" string)
       (req "amount" string)
       (req "destination" string))

let protocol_data_encoding : protocol_data Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {contents; signature} -> (contents, signature))
    (fun (contents, signature) -> {contents; signature})
    (obj2
       (req "contents" (list operation_content_encoding))
       (opt "signature" signature_encoding))

let unsigned_operation_encoding : operation Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {shell_header; protocol_data} -> (shell_header, protocol_data))
    (fun (branch, protocol_data) -> {shell_header = branch; protocol_data})
    (merge_objs branch_encoding protocol_data_encoding)

(* ------------------------------------------------------------------------- *)
(* Mempool-related encodings *)

(* This encoding is a protocol-independent version of
   [Tezos_shell_services.Block_services.Mempool.encoding] *)

let mempool_operation_encoding : mempool_operation Data_encoding.t =
  let open Data_encoding in
  conv
    (function
      | Mempool_operation {protocol = _; shell_header; protocol_data} ->
          (shell_header.branch, protocol_data.contents, protocol_data.signature))
    (fun _ -> assert false)
    (obj3
       (req "branch" Block_hash.encoding)
       (req "contents" (list operation_content_encoding))
       (opt "signature" signature_encoding))

(* ------------------------------------------------------------------------- *)
(* Operation-related helpers *)

let unsigned_operation_to_json op =
  Data_encoding.Json.construct unsigned_operation_encoding op

(* ------------------------------------------------------------------------- *)

type state = {
  protocol : Protocol_hash.t;
  sandbox_client : Tezt_tezos.Client.t;
  sandbox_node : Tezt_tezos.Node.t;
  counters : (Account.key, int) Hashtbl.t;
}

let bootstraps =
  let open Constant in
  [|bootstrap1; bootstrap2; bootstrap3; bootstrap4; bootstrap5|]

let sample_bootstrap () =
  let index = Random.int (Array.length bootstraps) in
  bootstraps.(index)

let get_next_counter state key =
  match Hashtbl.find_opt state.counters key with
  | None ->
      Hashtbl.add state.counters key 2 ;
      1
  | Some c ->
      Hashtbl.add state.counters key (c + 1) ;
      c

(* ------------------------------------------------------------------------- *)

(*
  Process to convert a mockup operation to an injectable one

  mockup mempool operation
  -> set signature to None and save signature aside for later
  -> encode to binary via forge rpc
  -> watermark binary operation
  -> inject (hex_of (watermarked_op ^ signature))
 *)

let remove_signature (op : operation) : operation =
  {op with protocol_data = {op.protocol_data with signature = None}}

let encode_unsigned_operation_to_binary client op =
  let op = remove_signature op in
  let op = unsigned_operation_to_json op in
  let* json_hex =
    Client.(
      rpc
        POST
        ["chains"; "main"; "blocks"; "head"; "helpers"; "forge"; "operations"]
        ~data:(Data op)
        client)
  in
  return Hex.(to_bytes (`Hex (JSON.as_string json_hex)))

let mempool_operation_from_op client protocol signer op :
    (mempool_operation * bytes) Lwt.t =
  let* bin = encode_unsigned_operation_to_binary client op in
  let signature = Operation.sign_manager_op_bytes ~signer bin in
  let signature = Signature.to_b58check signature in
  return
    ( Mempool_operation
        {
          protocol;
          shell_header = op.shell_header;
          protocol_data = {op.protocol_data with signature = Some signature};
        },
      bin )

let mempool_from_list_of_ops client protocol operations =
  let rec loop operations acc =
    match operations with
    | [] -> return (List.rev acc)
    | (account, op) :: tl ->
        let* mempool_op, binary_proto_data =
          mempool_operation_from_op client protocol account op
        in
        let shell_op =
          {
            Tezos_base.Operation.shell = {branch = op.shell_header.branch};
            proto = binary_proto_data;
          }
        in
        let hash = Tezos_base.Operation.hash shell_op in
        loop tl ((hash, mempool_op) :: acc)
  in
  loop operations []

(* ------------------------------------------------------------------------- *)

let fees = [|1_000; 2_000; 3_000|]

type fee = Fee_auto | Fee_mutez of int [@@warning "-37"]

let sample_next_transfer_for state ~fee ~branch ~account =
  let receiver = sample_bootstrap () in
  let fee =
    match fee with
    | Fee_auto -> fees.(Random.int (Array.length fees))
    | Fee_mutez fee -> fee
  in
  let amount = 1 + Random.int 500 in
  return
    {
      shell_header = {branch};
      protocol_data =
        {
          contents =
            [
              {
                kind = "transaction";
                source = account.Account.public_key_hash;
                fee = string_of_int fee;
                counter = string_of_int @@ get_next_counter state account;
                gas_limit = string_of_int 2000;
                storage_limit = "0";
                amount = string_of_int amount;
                destination = receiver.Account.public_key_hash;
              };
            ];
          signature = None;
        };
    }

let mutez_of_string s = Tez.of_mutez_int @@ int_of_string s

let bake_with_mempool ?protocol node client mempool =
  let* () =
    Lwt_list.iter_s
      (fun op ->
        let t = Stdlib.List.hd op.protocol_data.contents in
        let fee = mutez_of_string t.fee
        and gas_limit = int_of_string t.gas_limit
        and amount = mutez_of_string t.amount
        and giver = t.source
        and receiver = t.destination
        and endpoint = Client.Node node
        and storage_limit = int_of_string t.storage_limit in
        Client.transfer
          ~endpoint
          ~fee
          ~gas_limit
          ~amount
          ~giver
          ~receiver
          ~storage_limit
          client)
      mempool
  in
  (* Use --context's client argument to prevent the node from sorting
     the operations. *)
  Client.bake_for_and_wait
    ?protocol
    ~force:true
    ~context_path:(Node.data_dir node // "context")
    client

let get_current_head_hash state =
  let* head =
    Client.(
      rpc GET ["chains"; "main"; "blocks"; "head"; "hash"] state.sandbox_client)
  in
  return (Block_hash.of_b58check_exn (JSON.as_string head))

type op_info = {
  source : string;
  fee : int;
  gas_limit : int;
  storage_limit : int;
  counter : int;
}

(* We deliberately ignore the [gas_limit] and [storage_limit] for now, but
   ultimately we might want to check that the [weight] (as defined in the baker)
   is correctly taken into account when sorting transactions. *)
let compare_info info1 info2 =
  if String.equal info1.source info2.source then
    Int.compare info1.counter info2.counter
  else Int.compare info2.fee info1.fee

let get_fees_manager_and_counter op_json =
  let open JSON in
  let contents_list = as_list (op_json |-> "contents") in
  match contents_list with
  | [contents] ->
      let source = as_string (contents |-> "source") in
      let fee = as_int (contents |-> "fee") in
      let gas_limit = as_int (contents |-> "gas_limit") in
      let storage_limit = as_int (contents |-> "storage_limit") in
      let counter = as_int (contents |-> "counter") in
      {source; fee; gas_limit; storage_limit; counter}
  | _ -> Test.fail "unexpected packed operation"

let check_ordering ops =
  let ops' = List.sort compare_info ops in
  assert (ops = ops')

let assert_block_is_well_baked block expected_number_manager_op =
  match JSON.(as_list (block |-> "operations")) with
  | [consensus_ops; vote_ops; anonymous_ops; manager_ops] ->
      (* There very well might be attestation operations *)
      Log.debug
        "%d consensus operations"
        (List.length (JSON.as_list consensus_ops)) ;
      List.iter
        (fun l -> assert (JSON.as_list l = []))
        [vote_ops; anonymous_ops] ;
      let manager_ops = JSON.as_list manager_ops in
      let number_manager_ops = List.length manager_ops in
      Check.(
        (number_manager_ops = expected_number_manager_op)
          int
          ~error_msg:
            "Expected %R operations in the baked block, got %L operations.") ;
      let fees_managers_and_counters =
        List.map (fun json -> get_fees_manager_and_counter json) manager_ops
      in
      check_ordering fees_managers_and_counters
  | _ -> Test.fail "ill-formed operation list list"

(* ------------------------------------------------------------------------- *)
(* Random mempools *)

let random_permutation list =
  assert (list <> []) ;
  let rng = Random.State.make_self_init () in
  Tezos_base.TzPervasives.List.shuffle ~rng list

let single_baker_increasing_fees state ~account =
  let* branch = get_current_head_hash state in
  let fees = Array.of_list [1_000; 2_000; 3_000] in
  let* op1 =
    sample_next_transfer_for state ~fee:(Fee_mutez fees.(0)) ~branch ~account
  in
  let* _op2 =
    sample_next_transfer_for state ~fee:(Fee_mutez fees.(1)) ~branch ~account
  in
  let* _op3 =
    sample_next_transfer_for state ~fee:(Fee_mutez fees.(2)) ~branch ~account
  in
  Lwt.return @@ [op1; _op2; _op3]

let distinct_bakers_increasing_fees state sources =
  let* branch = get_current_head_hash state in
  let fees = random_permutation [1_000; 2_000; 3_000; 4_000; 5_000] in
  let accounts = random_permutation sources in
  Lwt_list.map_s
    (fun (account, fee) ->
      let* op =
        sample_next_transfer_for state ~fee:(Fee_mutez fee) ~branch ~account
      in
      return op)
    (List.combine accounts fees)

(* ------------------------------------------------------------------------- *)
(* Test entrypoints *)

let bake_and_check ?expected_baked_operations state ~protocol ~mempool ~sources
    =
  let* () =
    bake_with_mempool ~protocol state.sandbox_node state.sandbox_client mempool
  in
  let* block = Client.RPC.call state.sandbox_client @@ RPC.get_chain_block () in
  let expected_number_manager_op =
    match expected_baked_operations with
    | None ->
        List.length
          (List.sort_uniq
             (fun src1 src2 ->
               String.compare src1.Account.public_key_hash src2.public_key_hash)
             sources)
    | Some n -> n
  in
  assert_block_is_well_baked block expected_number_manager_op ;
  return ()

let init ~protocol =
  let* sandbox_node = Node.init [Synchronisation_threshold 0; Private_mode] in
  let* sandbox_client = Client.init ~endpoint:(Node sandbox_node) () in
  let* () = Client.activate_protocol ~protocol sandbox_client in
  Log.info "Activated protocol." ;
  return
    {
      protocol = Protocol_hash.of_b58check_exn (Protocol.hash protocol);
      sandbox_node;
      sandbox_client;
      counters = Hashtbl.create 11;
    }

let test_ordering =
  Protocol.register_test
    ~__FILE__
    ~title:"baking ordering"
    ~tags:["baking"; "ordering"]
  @@ fun protocol ->
  let* state = init ~protocol in
  Log.info "Testing ordering by counter" ;
  let account = Constant.bootstrap1 in
  let* mempool = single_baker_increasing_fees state ~account in
  let* () =
    bake_and_check
      state
      ~expected_baked_operations:1
        (* We're going through the mempool, with
           operations from the same source, so that only 1 will be baked in *)
      ~protocol
      ~mempool
      ~sources:[account]
  in
  (* Because of the 1m restriction, only one operation has been added
     to the previous block, hence the next counter for bootstrap1 is
     the successor of one. *)
  Hashtbl.add state.counters account 2 ;
  Log.info "Testing ordering by fees" ;
  let sources = Array.to_list bootstraps in
  let* mempool = distinct_bakers_increasing_fees state sources in
  bake_and_check state ~protocol ~mempool ~sources

let check_op_not_in_baked_block client op =
  let* ops = Client.RPC.call client @@ RPC.get_chain_block_operations () in
  let open JSON in
  let ops_list = ops |=> 3 |> as_list in
  let res = List.exists (fun e -> e |-> "hash" |> as_string = op) ops_list in
  if res then Test.fail "%s found in Baked block" op ;
  unit

let wrong_branch_operation_dismissal =
  Protocol.register_test
    ~__FILE__
    ~title:"wrong branch operation dismissal"
    ~tags:["baking"; "branch"]
  @@ fun protocol ->
  let* node = Node.init [Synchronisation_threshold 0; Private_mode] in
  let* client = Client.init ~endpoint:(Node node) () in
  let minimal_block_delay = 1 in
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Either.Right (protocol, None))
      [
        (["consensus_threshold"], `Int 1);
        (["minimal_block_delay"], `String_of_int minimal_block_delay);
        (["delay_increment_per_round"], `String "1");
      ]
  in
  let* () =
    Client.activate_protocol ~timestamp:Now ~protocol ~parameter_file client
  in
  Log.info "Activated protocol." ;
  Log.info "Baking a first proposal." ;
  let* () = Client.propose_for ~minimal_timestamp:false ~key:[] client in
  (* We inject an operation where the branch is the head (instead of
     head~2). Such operation should not be included. *)
  let* branch = Client.RPC.call client @@ RPC.get_chain_block_hash () in
  Log.info "Injecting a transfer branched on the current head." ;
  let* (`OpHash oph) =
    Operation.Manager.(inject ~branch [make @@ transfer ()] client)
  in
  let* current_mempool = Mempool.get_mempool client in
  Check.(
    (current_mempool <> Tezt_tezos.Mempool.empty)
      Tezt_tezos.Mempool.typ
      ~error_msg:"unexpected empty mempool") ;
  Log.info "Wait a bit in order to propose on a different round." ;
  let* () = Lwt_unix.sleep (float minimal_block_delay) in
  Log.info "Bake a second proposal at a different round." ;
  let* () = Client.propose_for ~minimal_timestamp:false ~key:[] client in
  Log.info "Checking that the transfer is dismissed from the current mempool." ;
  let* current_mempool = Mempool.get_mempool client in
  Check.(
    (current_mempool = Tezt_tezos.Mempool.empty)
      Tezt_tezos.Mempool.typ
      ~error_msg:"unexpected non-empty mempool") ;
  Log.info "Checking that the transfer is not included in the current head." ;
  check_op_not_in_baked_block client oph

let baking_operation_exception =
  Protocol.register_test
    ~__FILE__
    ~title:"ensure we can still bake with a faulty operation"
    ~tags:["baking"; "exception"]
  @@ fun protocol ->
  let* node, client = Client.init_with_protocol `Client ~protocol () in
  let data_dir = Node.data_dir node in
  let wait_injection = Node.wait_for_request ~request:`Inject node in
  let* new_account = Client.gen_and_show_keys client in
  let* () =
    Client.transfer
      ~giver:"bootstrap1"
      ~receiver:new_account.alias
      ~amount:(Tez.of_int 10)
      ~burn_cap:Tez.one
      client
  in
  let* () = wait_injection in
  (* We use [context_path] to ensure the baker will not use the
     preapply RPC. Indeed, this test was introduced because of a bug
     that happens when the baker does not use the preapply RPC. *)
  let* () =
    Client.bake_for_and_wait ~context_path:(data_dir // "context") client
  in
  let wait_injection = Node.wait_for_request ~request:`Inject node in
  let*! () = Client.reveal ~fee:Tez.one ~src:new_account.alias client in
  let* () = wait_injection in
  let* () =
    Client.bake_for_and_wait ~context_path:(data_dir // "context") client
  in
  let* _ =
    Operation.Manager.(
      inject
        [
          make
            ~source:new_account
            ~fee:9_000_000
              (* Emptying an account costs gas: we add 400 to the
                 minimal manager operation gas cost to cover this
                 possibility. See issue
                 https://gitlab.com/tezos/tezos/-/issues/3188 *)
            ~gas_limit:(Constant.manager_operation_gas_cost ~protocol + 400)
          @@ delegation ~delegate:new_account ();
        ]
        client)
  in
  Client.bake_for_and_wait ~context_path:(data_dir // "context") client

let init ?(overrides = []) protocol =
  let* sandbox_node = Node.init [Synchronisation_threshold 0; Private_mode] in
  let sandbox_endpoint = Client.Node sandbox_node in
  let* sandbox_client = Client.init ~endpoint:sandbox_endpoint () in
  let* parameter_file =
    let base = Either.Right (protocol, None) in
    Protocol.write_parameter_file ~base overrides
  in
  let* () =
    (* activate in the past - let timestamp_delay be the default value of 1 year *)
    Client.activate_protocol ~protocol sandbox_client ~parameter_file
  in
  return @@ (sandbox_endpoint, sandbox_client)

let test_operation_pool_ordering
    ?(accounts = Array.map (fun k -> k.Account.alias) Account.Bootstrap.keys)
    operations n_transfers =
  Protocol.register_test
    ~__FILE__
    ~title:
      (Printf.sprintf
         "External operations are ordered (%d transfers, max %d operations)"
         n_transfers
         operations)
    ~tags:["operations_pool"; "baking"]
  @@ fun protocol ->
  let* endpoint, client = init protocol in
  (* Test preparation *)
  Log.info "Evaluating gas cost of a single transfer" ;
  let* () =
    Client.transfer
      ~amount:(Tez.of_int 2)
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      client
  in

  let* () = Client.bake_for_and_wait ~endpoint client in
  let* gas_limit =
    let* block = Client.RPC.call_json client (RPC.get_chain_block ()) in
    JSON.get "operations" block
    |> JSON.geti 3 |> JSON.geti 0 |> JSON.get "contents" |> JSON.geti 0
    |> JSON.get "gas_limit" |> JSON.as_int |> Lwt.return
  in

  let* () =
    Node.terminate (match endpoint with Node n -> n | _ -> assert false)
  in

  (* The test in itself *)
  Log.info "Restarting protocol to test --operations option" ;
  let expected_n_ops = operations in
  (* Create a new protocol instance with a small limit of gas per block, so
     that a number of transfers cannot be included. *)
  let* _endpoint, client =
    init
      ~overrides:
        [
          ( ["hard_gas_limit_per_block"],
            `String_of_int (expected_n_ops * gas_limit) );
        ]
      protocol
  in
  let rec do_transfer i =
    let len = Array.length accounts in
    if i < 0 || i > len || i >= n_transfers then Lwt.return_unit
    else
      let giver = accounts.(i) and receiver = accounts.((i + 1) mod len) in
      let* () = Client.transfer ~amount:Tez.one ~giver ~receiver client in
      do_transfer (i + 1)
  in
  let* () = do_transfer 0 in

  (* Create a valid operation pool from current mempool *)
  let* mempool =
    Client.RPC.call client @@ RPC.get_chain_mempool_pending_operations ()
  in
  let mgmt_ops = JSON.get "validated" mempool in

  let filename = Filename.temp_file "opool_" ".json" in
  let json = JSON.as_list mgmt_ops in
  let to_op o =
    let of_field name = (name, JSON.get name o |> JSON.unannotate) in
    ( JSON.get "hash" o,
      `O [of_field "contents"; of_field "branch"; of_field "signature"] )
  in
  let hashes, pool =
    let l = List.map to_op json in
    (List.map fst l, `A (List.map snd l))
  in

  Log.info "Writing external operations pool (%s)" filename ;
  let oc = open_out_bin filename in
  Printf.fprintf oc "%s%!" (JSON.encode (JSON.annotate ~origin:"mempool" pool)) ;
  close_out oc ;

  let* () =
    Client.bake_for_and_wait ~ignore_node_mempool:true ~mempool:filename client
  in
  let* op_hashes =
    let* json =
      Client.RPC.call_json client (RPC.get_chain_block_operation_hashes ())
    in
    JSON.as_list (JSON.geti 3 json) |> Lwt.return
  in
  Check.(List.length op_hashes = expected_n_ops)
    Check.int
    ~error_msg:"Expected %R operations, got %L" ;
  (* Check that the order of hash in the block is the same as the order of
     hashe in the external pool *)
  let check_hashes l1 l2 =
    let len = List.length l1 in
    let should_be_in =
      Tezos_base__TzPervasives.List.take_n len l2 |> List.map JSON.encode
    in
    List.iter
      (fun oph ->
        let oph_s = JSON.encode oph in
        Check.(List.mem oph_s should_be_in = true)
          Check.bool
          ~error_msg:
            (Printf.sprintf
               "operation hash %s not in first %d elements of initial pool"
               oph_s
               len))
      l1
  in
  check_hashes op_hashes hashes ;
  Lwt.return_unit

(** This test activates a protocol with a timestamps [Now]. It then bakes a
    block with the given [minimal_timestamp] flag. *)
let baking_with_given_minimal_timestamp ~minimal_timestamp =
  Protocol.register_test
    ~supports:Protocol.(From_protocol (number Nairobi))
    ~__FILE__
    ~title:(sf "Baking minimal timestamp (%b)" minimal_timestamp)
    ~tags:["baking"; "timestamp"]
  @@ fun protocol ->
  let* _node, client =
    Client.init_with_protocol
      ~timestamp:(Ago (Tezos_base.Time.System.Span.of_seconds_exn 60.))
      ~nodes_args:[Synchronisation_threshold 0]
      ~protocol
      `Client
      ()
  in
  Client.bake_for_and_wait ~minimal_timestamp client

let register ~protocols =
  test_ordering protocols ;
  wrong_branch_operation_dismissal protocols ;
  baking_operation_exception protocols ;
  baking_with_given_minimal_timestamp ~minimal_timestamp:false protocols ;
  baking_with_given_minimal_timestamp ~minimal_timestamp:true protocols

let register_operations_pool ~protocols =
  List.iter
    (fun n ->
      List.iter
        (fun max_operations ->
          test_operation_pool_ordering max_operations n protocols)
        (Base.range 1 n))
    (Base.range 1 (Array.length Account.Bootstrap.keys))
