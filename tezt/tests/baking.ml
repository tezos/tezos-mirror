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
      protocol : Tezos_crypto.Protocol_hash.t;
      shell_header : branch;
      protocol_data : protocol_data;
    }

type mempool = (Tezos_crypto.Operation_hash.t * mempool_operation) list

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
      | Mempool_operation {protocol; shell_header; protocol_data} ->
          ( protocol,
            shell_header.branch,
            protocol_data.contents,
            protocol_data.signature ))
    (fun (protocol, branch, contents, signature) ->
      Mempool_operation
        {
          protocol;
          shell_header = {branch};
          protocol_data = {contents; signature};
        })
    (obj4
       (req "protocol" Tezos_crypto.Protocol_hash.encoding)
       (req "branch" Block_hash.encoding)
       (req "contents" (list operation_content_encoding))
       (opt "signature" signature_encoding))

let mempool_encoding : mempool Data_encoding.t =
  let open Data_encoding in
  let is_empty = function [] -> true | _ -> false in
  conv
    (fun operations ->
      let applied = [] in
      let refused = [] in
      let outdated = [] in
      let branch_refused = [] in
      let branch_delayed = [] in
      let unprocessed = operations in
      (applied, refused, outdated, branch_refused, branch_delayed, unprocessed))
    (fun ( applied,
           refused,
           outdated,
           branch_refused,
           branch_delayed,
           unprocessed ) ->
      assert (is_empty applied) ;
      assert (is_empty refused) ;
      assert (is_empty outdated) ;
      assert (is_empty branch_refused) ;
      assert (is_empty branch_delayed) ;
      unprocessed)
    (obj6
       (* We put [unit] as a stub *)
       (req "applied" (list (dynamic_size unit)))
       (req "refused" (list (dynamic_size unit)))
       (req "outdated" (list (dynamic_size unit)))
       (req "branch_refused" (list (dynamic_size unit)))
       (req "branch_delayed" (list (dynamic_size unit)))
       (req
          "unprocessed"
          (list
             (tup2
                Tezos_crypto.Operation_hash.encoding
                mempool_operation_encoding))))

(* ------------------------------------------------------------------------- *)
(* Operation-related helpers *)

let unsigned_operation_to_json op =
  Data_encoding.Json.construct unsigned_operation_encoding op

(* ------------------------------------------------------------------------- *)

type state = {
  protocol : Tezos_crypto.Protocol_hash.t;
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

let encode_unsigned_operation_to_binary state op =
  let op = remove_signature op in
  let op = unsigned_operation_to_json op in
  let* json_hex =
    Client.(
      rpc
        POST
        ["chains"; "main"; "blocks"; "head"; "helpers"; "forge"; "operations"]
        ~data:op
        state.sandbox_client)
  in
  return Hex.(to_bytes (`Hex (JSON.as_string json_hex)))

let mempool_operation_from_op state signer op :
    (mempool_operation * bytes) Lwt.t =
  let* bin = encode_unsigned_operation_to_binary state op in
  let signature = Operation.sign_manager_op_bytes ~signer bin in
  let signature = Tezos_crypto.Signature.to_b58check signature in
  return
    ( Mempool_operation
        {
          protocol = state.protocol;
          shell_header = op.shell_header;
          protocol_data = {op.protocol_data with signature = Some signature};
        },
      bin )

let mempool_from_list_of_ops state operations =
  let rec loop state operations acc =
    match operations with
    | [] -> return (List.rev acc)
    | (account, op) :: tl ->
        let* (mempool_op, binary_proto_data) =
          mempool_operation_from_op state account op
        in
        let shell_op =
          {
            Tezos_base.Operation.shell = {branch = op.shell_header.branch};
            proto = binary_proto_data;
          }
        in
        let hash = Tezos_base.Operation.hash shell_op in
        loop state tl ((hash, mempool_op) :: acc)
  in
  loop state operations []

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
  let operation =
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
                counter = string_of_int (get_next_counter state account);
                gas_limit = string_of_int 2000;
                storage_limit = string_of_int 0;
                amount = string_of_int amount;
                destination = receiver.Account.public_key_hash;
              };
            ];
          signature = None;
        };
    }
  in
  return operation

let bake_with_mempool ?protocol state mempool =
  let mempool_json = Data_encoding.Json.construct mempool_encoding mempool in
  let mempool_str = Ezjsonm.value_to_string mempool_json in
  let mempool = Temp.file "mempool.json" in
  let* _ =
    Lwt_io.with_file ~mode:Lwt_io.Output mempool (fun oc ->
        Lwt_io.write oc mempool_str)
  in
  (* Use --context's client argument to prevent the node from sorting
     the operations. *)
  Client.bake_for
    ?protocol
    ~mempool
    ~force:true
    ~context_path:(Node.data_dir state.sandbox_node // "context")
    state.sandbox_client

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

let assert_block_is_well_baked block =
  match JSON.(as_list (block |-> "operations")) with
  | [endorsement_ops; vote_ops; anonymous_ops; manager_ops] ->
      (* There very well might be endorsement operations *)
      Log.debug
        "%d endorsement operations"
        (List.length (JSON.as_list endorsement_ops)) ;
      List.iter
        (fun l -> assert (JSON.as_list l = []))
        [vote_ops; anonymous_ops] ;
      let fees_managers_and_counters =
        List.map
          (fun json -> get_fees_manager_and_counter json)
          (JSON.as_list manager_ops)
      in
      check_ordering fees_managers_and_counters
  | _ -> Test.fail "ill-formed operation list list"

(* ------------------------------------------------------------------------- *)
(* Random mempools *)

let random_permutation list =
  assert (list <> []) ;
  Tezos_stdlib.TzList.shuffle list

let single_baker_increasing_fees state ~account : mempool Lwt.t =
  let* branch = get_current_head_hash state in
  let fees = Array.of_list (random_permutation [1_000; 2_000; 3_000]) in
  let* op1 =
    sample_next_transfer_for state ~fee:(Fee_mutez fees.(0)) ~branch ~account
  in
  let* op2 =
    sample_next_transfer_for state ~fee:(Fee_mutez fees.(1)) ~branch ~account
  in
  let* op3 =
    sample_next_transfer_for state ~fee:(Fee_mutez fees.(2)) ~branch ~account
  in
  let ops =
    random_permutation [(account, op1); (account, op2); (account, op3)]
  in
  mempool_from_list_of_ops state ops

let distinct_bakers_increasing_fees state : mempool Lwt.t =
  let* branch = get_current_head_hash state in
  let fees = random_permutation [1_000; 2_000; 3_000; 4_000; 5_000] in
  let accounts =
    random_permutation
      [
        Constant.bootstrap1;
        Constant.bootstrap2;
        Constant.bootstrap3;
        Constant.bootstrap4;
        Constant.bootstrap5;
      ]
  in
  let* ops =
    Lwt_list.map_s
      (fun (account, fee) ->
        let* op =
          sample_next_transfer_for state ~fee:(Fee_mutez fee) ~branch ~account
        in
        return (account, op))
      (List.combine accounts fees)
  in
  mempool_from_list_of_ops state ops

(* ------------------------------------------------------------------------- *)
(* Test entrypoints *)

let bake_and_check state ~protocol ~mempool =
  let* () = bake_with_mempool ~protocol state mempool in
  let* block =
    Client.(rpc GET ["chains"; "main"; "blocks"; "head"] state.sandbox_client)
  in
  assert_block_is_well_baked block ;
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
  let* mempool =
    single_baker_increasing_fees state ~account:Constant.bootstrap1
  in
  let* () = bake_and_check state ~protocol ~mempool in
  Log.info "Testing ordering by fees" ;
  let* mempool = distinct_bakers_increasing_fees state in
  bake_and_check state ~protocol ~mempool

let check_op_not_in_baked_block client op =
  let* ops = RPC.get_operations client in
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
        (["consensus_threshold"], Some "1");
        ( ["minimal_block_delay"],
          Some (Printf.sprintf "\"%d\"" minimal_block_delay) );
        (["delay_increment_per_round"], Some "\"1\"");
      ]
  in
  let* () =
    Client.activate_protocol
      ~timestamp_delay:0.
      ~protocol
      ~parameter_file
      client
  in
  Log.info "Activated protocol." ;
  Log.info "Baking a first proposal." ;
  let* () = Client.propose_for ~minimal_timestamp:false ~key:[] client in
  (* Retrieve head's hash *)
  let* head_hash = RPC.get_block_hash client in
  Log.info "Injecting a transfer branched on the current head." ;
  let* (`OpHash oph) =
    Operation.inject_transfer
      ~branch:head_hash
      ~amount:1
      ~source:Constant.bootstrap1
      ~dest:Constant.bootstrap2
      client
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

let register ~protocols =
  test_ordering ~protocols ;
  wrong_branch_operation_dismissal ~protocols
