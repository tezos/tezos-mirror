(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Testing
    -------
    Component:    Shell (state)
    Invocation:   dune exec src/lib_shell/test/test.exe test "state"
    Subject:      On states.
*)

module Proto = Shell_test_helpers.Genesis_proto

let incr_fitness fitness =
  let new_fitness =
    match fitness with
    | [fitness] ->
        Data_encoding.Binary.of_bytes_opt Data_encoding.int64 fitness
        |> Option.value ~default:0L |> Int64.succ
        |> Data_encoding.Binary.to_bytes_exn Data_encoding.int64
    | _ ->
        Data_encoding.Binary.to_bytes_exn Data_encoding.int64 1L
  in
  [new_fitness]

let incr_timestamp timestamp =
  Time.Protocol.add timestamp (Int64.add 1L (Random.int64 10L))

let operation op =
  let op : Operation.t =
    {
      shell = {branch = Shell_test_helpers.genesis_block_hash};
      proto = Bytes.of_string op;
    }
  in
  (Operation.hash op, op, Data_encoding.Binary.to_bytes Operation.encoding op)

let block_header_data_encoding =
  Data_encoding.(obj1 (req "proto_block_header" string))

let block _state ?(context = Context_hash.zero) ?(operations = [])
    (pred : State.Block.t) name : Block_header.t =
  let operations_hash =
    Operation_list_list_hash.compute [Operation_list_hash.compute operations]
  in
  let pred_header = State.Block.shell_header pred in
  let fitness = incr_fitness pred_header.fitness in
  let timestamp = incr_timestamp pred_header.timestamp in
  let protocol_data =
    Data_encoding.Binary.to_bytes_exn block_header_data_encoding name
  in
  {
    shell =
      {
        level = Int32.succ pred_header.level;
        proto_level = pred_header.proto_level;
        predecessor = State.Block.hash pred;
        validation_passes = 1;
        timestamp;
        operations_hash;
        fitness;
        context;
      };
    protocol_data;
  }

let parsed_block ({shell; protocol_data} : Block_header.t) =
  let protocol_data =
    Data_encoding.Binary.of_bytes_exn
      Proto.block_header_data_encoding
      protocol_data
  in
  ({shell; protocol_data} : Proto.block_header)

let zero = Bytes.create 0

let build_valid_chain state vtbl pred names =
  List.fold_left_s
    (fun pred name ->
      State.Block.context_exn pred
      >>= fun predecessor_context ->
      let max_trials = 100 in
      let rec attempt trials context =
        (let (oph, op, _bytes) = operation name in
         let block = block ?context state ~operations:[oph] pred name in
         let hash = Block_header.hash block in
         let pred_header = State.Block.header pred in
         (let predecessor_context =
            Shell_context.wrap_disk_context predecessor_context
          in
          Proto.begin_application
            ~chain_id:Chain_id.zero
            ~predecessor_context
            ~predecessor_timestamp:pred_header.shell.timestamp
            ~predecessor_fitness:pred_header.shell.fitness
            (parsed_block block)
          >>=? fun vstate ->
          (* no operations *)
          Proto.finalize_block vstate)
         >>=? fun (result, _metadata) ->
         let context = Shell_context.unwrap_disk_context result.context in
         Context.commit ~time:block.shell.timestamp context
         >>= fun context_hash ->
         let validation_store =
           ( {
               context_hash;
               message = result.message;
               max_operations_ttl = 1;
               last_allowed_fork_level = result.last_allowed_fork_level;
             }
             : Block_validation.validation_store )
         in
         let block_metadata = zero in
         let block_metadata_hash =
           Option.some @@ Block_metadata_hash.hash_bytes [block_metadata]
         in
         let operations_metadata = [[zero]] in
         let operations_metadata_hashes =
           Some [[Operation_metadata_hash.hash_bytes [zero]]]
         in
         State.Block.store
           state
           block
           block_metadata
           [[op]]
           operations_metadata
           block_metadata_hash
           operations_metadata_hashes
           ( {
               context_hash;
               message = validation_store.message;
               max_operations_ttl = 1;
               last_allowed_fork_level =
                 validation_store.last_allowed_fork_level;
             }
             : Block_validation.validation_store )
           ~forking_testchain:false
         >>=? fun _vblock ->
         State.Block.read state hash
         >>=? fun vblock ->
         String.Hashtbl.add vtbl name vblock ;
         return vblock)
        >>= function
        | Ok v ->
            if trials < max_trials then
              Format.eprintf
                "Took %d trials to build valid chain"
                (max_trials - trials + 1) ;
            Lwt.return v
        | Error (Validation_errors.Inconsistent_hash (got, _) :: _) ->
            (* Kind of a hack, but at least it tests idempotence to some extent. *)
            if trials <= 0 then assert false
            else (
              Format.eprintf
                "Inconsistent context hash: got %a, retrying (%d)\n"
                Context_hash.pp
                got
                trials ;
              attempt (trials - 1) (Some got) )
        | Error err ->
            Format.eprintf "Error: %a\n" Error_monad.pp_print_error err ;
            assert false
      in
      attempt max_trials None)
    pred
    names
  >>= fun _ -> Lwt.return_unit

type state = {
  vblock : State.Block.t String.Hashtbl.t;
  state : State.t;
  chain : State.Chain.t;
}

let vblock s k =
  WithExceptions.Option.get ~loc:__LOC__ @@ String.Hashtbl.find s.vblock k

exception Found of string

let vblocks s =
  String.Hashtbl.fold (fun k v acc -> (k, v) :: acc) s.vblock []
  |> List.sort Stdlib.compare

(*******************************************************)
(*

    Genesis - A1 - A2 - A3 - A4 - A5 - A6 - A7 - A8
                         \
                          B1 - B2 - B3 - B4 - B5 - B6 - B7 - B8
*)

let build_example_tree chain =
  let vtbl = String.Hashtbl.create 23 in
  Chain.genesis chain
  >>= fun genesis ->
  String.Hashtbl.add vtbl "Genesis" genesis ;
  let c = ["A1"; "A2"; "A3"; "A4"; "A5"; "A6"; "A7"; "A8"] in
  build_valid_chain chain vtbl genesis c
  >>= fun () ->
  let a3 =
    WithExceptions.Option.get ~loc:__LOC__ @@ String.Hashtbl.find vtbl "A3"
  in
  let c = ["B1"; "B2"; "B3"; "B4"; "B5"; "B6"; "B7"; "B8"] in
  build_valid_chain chain vtbl a3 c >>= fun () -> Lwt.return vtbl

let wrap_state_init f base_dir =
  let open Filename.Infix in
  let store_root = base_dir // "store" in
  let context_root = base_dir // "context" in
  State.init
    ~store_mapsize:4_096_000_000L
    ~store_root
    ~context_root
    Shell_test_helpers.genesis
  >>=? fun (state, chain, _index, _history_mode) ->
  build_example_tree chain >>= fun vblock -> f {state; chain; vblock}

(** Initializes the store. *)
let test_init (_ : state) = return_unit

(** State.Block.read *)

let test_read_block (s : state) =
  List.iter_s
    (fun (name, vblock) ->
      let hash = State.Block.hash vblock in
      State.Block.read s.chain hash
      >>= function
      | Error _ ->
          Assert.fail_msg "Error while reading valid block %s" name
      | Ok _vblock' ->
          (* FIXME COMPARE read operations ??? *)
          Lwt.return_unit)
    (vblocks s)
  >>= fun () -> return_unit

(****************************************************************************)

(** Chain.set_checkpoint_then_purge_full *)

let test_set_checkpoint_then_purge_full (s : state) =
  State.Chain.checkpoint s.chain
  >>= fun checkpoint ->
  let checkpoint_lvl = checkpoint.shell.level in
  let checkpoint_hash = Block_header.hash checkpoint in
  (* At the beginning the checkpoint is the genesis. *)
  State.Block.read s.chain Shell_test_helpers.genesis_block_hash
  >>=? fun read_genesis ->
  let read_genesis_hash =
    Block_header.hash (State.Block.header read_genesis)
  in
  assert (Block_hash.equal checkpoint_hash read_genesis_hash) ;
  assert (checkpoint_lvl = Int32.zero) ;
  let a1 = vblock s "A1" in
  let ha1 = State.Block.hash a1 in
  let b1 = vblock s "B1" in
  let hb1 = State.Block.hash b1 in
  let b2 = vblock s "B2" in
  let hb2 = State.Block.hash b2 in
  let la1 = State.Block.level a1 in
  let lb1 = State.Block.level b1 in
  let lb2 = State.Block.level b2 in
  assert (Int32.compare checkpoint_lvl la1 = -1) ;
  assert (Int32.compare checkpoint_lvl lb1 = -1) ;
  assert (Int32.compare checkpoint_lvl lb2 = -1) ;
  State.Chain.store s.chain
  >>= fun chain_store ->
  let chain_store = Store.Chain.get chain_store (State.Chain.id s.chain) in
  let block_store = Store.Block.get chain_store in
  (* Let us set a new checkpoint "B1" whose level is greater than the genesis. *)
  State.Chain.set_checkpoint_then_purge_full s.chain (State.Block.header b2)
  >>=? fun () ->
  (* Assert b2 does still exist and is the new checkpoint. *)
  State.Block.known s.chain hb2
  >|= (fun b -> assert b)
  >>= fun () ->
  State.Chain.checkpoint s.chain
  >|= (fun b ->
        assert (Block_hash.equal (Block_header.hash b) hb2) ;
        assert (Int32.equal b.shell.level lb2))
  >>= fun () ->
  (* Assert b1 has been pruned.. *)
  Store.Block.Contents.known (block_store, hb1)
  >|= (fun b -> assert (not b))
  >>= fun () ->
  (* pruned, so we can still access its header. *)
  State.Block.read_opt s.chain hb1
  >|= (function Some _header -> assert true | None -> assert false)
  >>= fun () ->
  (* Assert a1 has also been pruned .. *)
  Store.Block.Contents.known (block_store, ha1)
  >|= (fun b -> assert (not b))
  >>= fun () ->
  (* and we can also access its header. *)
  State.Block.read_opt s.chain ha1
  >|= (function Some _header -> assert true | None -> assert false)
  >>= fun () ->
  (* and is accessible in Store.Block.Header *)
  Store.Block.Pruned_contents.known (block_store, ha1)
  >|= (fun b -> assert b)
  >>= fun () -> return_unit

(** Chain.set_checkpoint_then_purge_rolling *)

let test_set_checkpoint_then_purge_rolling (s : state) =
  State.Chain.checkpoint s.chain
  >>= fun checkpoint ->
  let checkpoint_lvl = checkpoint.shell.level in
  let checkpoint_hash = Block_header.hash checkpoint in
  (* At the beginning the checkpoint is the genesis. *)
  State.Block.read s.chain Shell_test_helpers.genesis_block_hash
  >>=? fun read_genesis ->
  let read_genesis_hash =
    Block_header.hash (State.Block.header read_genesis)
  in
  assert (Block_hash.equal checkpoint_hash read_genesis_hash) ;
  assert (checkpoint_lvl = Int32.zero) ;
  let a1 = vblock s "A1" in
  let ha1 = State.Block.hash a1 in
  let b1 = vblock s "B1" in
  let hb1 = State.Block.hash b1 in
  let b2 = vblock s "B2" in
  let hb2 = State.Block.hash b2 in
  let la1 = State.Block.level a1 in
  let lb1 = State.Block.level b1 in
  let lb2 = State.Block.level b2 in
  assert (Int32.compare checkpoint_lvl la1 = -1) ;
  assert (Int32.compare checkpoint_lvl lb1 = -1) ;
  assert (Int32.compare checkpoint_lvl lb2 = -1) ;
  State.Block.max_operations_ttl b2
  >>=? fun max_op_ttl ->
  assert (max_op_ttl > 0) ;
  let ilb1 = Int32.to_int lb1 in
  let ilb2 = Int32.to_int lb2 in
  (* Assert b1 is in the to-prune range. *)
  assert (ilb2 - ilb1 <= min max_op_ttl ilb2) ;
  (* Assert a1 is in the to-delete range. *)
  let ila1 = Int32.to_int la1 in
  assert (ilb2 - ila1 > min max_op_ttl ilb2) ;
  (* Assert b1 is not yet in Store.Block.Header since not pruned *)
  State.Chain.store s.chain
  >>= fun chain_store ->
  let chain_store = Store.Chain.get chain_store (State.Chain.id s.chain) in
  let block_store = Store.Block.get chain_store in
  Store.Block.Pruned_contents.known (block_store, hb1)
  >|= (fun b -> assert (not b))
  >>= fun () ->
  (* But accessible with State.Block.Header *)
  State.Block.known s.chain hb1
  >|= (fun b -> assert b)
  (* And Store.Block.Contents *)
  >>= fun () ->
  Store.Block.Contents.known (block_store, hb1)
  >|= (fun b -> assert b)
  (* Let us set a new checkpoint "B1" whose level is greater than the genesis. *)
  >>= fun () ->
  State.Chain.set_checkpoint_then_purge_rolling s.chain (State.Block.header b2)
  >>=? fun () ->
  (* Assert b2 does still exist and is the new checkpoint. *)
  State.Block.known s.chain hb2
  >|= (fun b -> assert b)
  >>= fun () ->
  State.Chain.checkpoint s.chain
  >|= (fun b ->
        assert (Block_hash.equal (Block_header.hash b) hb2) ;
        assert (Int32.equal b.shell.level lb2))
  >>= fun () ->
  (* Assert b1 has been pruned.. *)
  Store.Block.Contents.known (block_store, hb1)
  >|= (fun b -> assert (not b))
  >>= fun () ->
  (* pruned, so we can still access its header. *)
  State.Block.read_opt s.chain hb1
  >|= (function Some _block -> assert true | None -> assert false)
  >>= fun () ->
  (* Assert b1 is now in Store.Block.Header since it has been pruned *)
  Store.Block.Pruned_contents.known (block_store, hb1)
  >|= (fun b -> assert b)
  >>= fun () ->
  (* And also accessible with State.Block.Header *)
  State.Block.Header.known (block_store, hb1)
  >|= (fun b -> assert b)
  (* But not in Store.Block.Contents *)
  >>= fun () ->
  Store.Block.Contents.known (block_store, hb1)
  >|= (fun b -> assert (not b))
  >>= fun () ->
  (* Assert a1 has been deleted.. *)
  State.Block.known s.chain ha1
  >|= (fun b -> assert (not b))
  >>= fun () ->
  (* deleted, so we can not access its header anymore. *)
  State.Block.read_opt s.chain ha1
  >|= (function Some _header -> assert false | None -> assert true)
  >>= fun () ->
  (* Assert b1 is now in Store.Block.Header since it has been pruned *)
  Store.Block.Pruned_contents.known (block_store, ha1)
  >|= (fun b -> assert (not b))
  >>= fun () ->
  (* And not in State.Block.Header *)
  State.Block.Header.known (block_store, ha1)
  >|= (fun b -> assert (not b))
  (* Neither in Store.Block.Contents *)
  >>= fun () ->
  Store.Block.Contents.known (block_store, hb1)
  >|= (fun b -> assert (not b))
  (*  *)
  >>= fun () -> return_unit

(****************************************************************************)

(** Chain_traversal.path *)

let rec compare_path p1 p2 =
  match (p1, p2) with
  | ([], []) ->
      true
  | (h1 :: p1, h2 :: p2) ->
      Block_hash.equal h1 h2 && compare_path p1 p2
  | _ ->
      false

(** Various path traversal checks on [chain_store] (using
    [Store.Chain_traversal.path]). *)
let test_path (s : state) =
  let check_path h1 h2 p2 =
    Chain_traversal.path (vblock s h1) (vblock s h2)
    >>= function
    | None ->
        Assert.fail_msg "cannot compute path %s -> %s" h1 h2
    | Some (p : State.Block.t list) ->
        let p = List.map State.Block.hash p in
        let p2 = List.map (fun b -> State.Block.hash (vblock s b)) p2 in
        if not (compare_path p p2) then
          Assert.fail_msg "bad path %s -> %s" h1 h2 ;
        Lwt.return_unit
  in
  check_path "Genesis" "Genesis" []
  >>= fun () ->
  check_path "A1" "A1" []
  >>= fun () ->
  check_path "A2" "A6" ["A3"; "A4"; "A5"; "A6"]
  >>= fun () ->
  check_path "B2" "B6" ["B3"; "B4"; "B5"; "B6"]
  >>= fun () ->
  check_path "A1" "B3" ["A2"; "A3"; "B1"; "B2"; "B3"] >>= fun () -> return_unit

(****************************************************************************)

(** Checks that two blocks admits some same ancestor (using
    [Store.Chain_traversal.common_ancestor]). *)
let test_ancestor s =
  let check_ancestor h1 h2 expected =
    Chain_traversal.common_ancestor (vblock s h1) (vblock s h2)
    >>= fun a ->
    if not (Block_hash.equal (State.Block.hash a) (State.Block.hash expected))
    then Assert.fail_msg "bad ancestor %s %s" h1 h2 ;
    Lwt.return_unit
  in
  check_ancestor "Genesis" "Genesis" (vblock s "Genesis")
  >>= fun () ->
  check_ancestor "Genesis" "A3" (vblock s "Genesis")
  >>= fun () ->
  check_ancestor "A3" "Genesis" (vblock s "Genesis")
  >>= fun () ->
  check_ancestor "A1" "A1" (vblock s "A1")
  >>= fun () ->
  check_ancestor "A1" "A3" (vblock s "A1")
  >>= fun () ->
  check_ancestor "A3" "A1" (vblock s "A1")
  >>= fun () ->
  check_ancestor "A6" "B6" (vblock s "A3")
  >>= fun () ->
  check_ancestor "B6" "A6" (vblock s "A3")
  >>= fun () ->
  check_ancestor "A4" "B1" (vblock s "A3")
  >>= fun () ->
  check_ancestor "B1" "A4" (vblock s "A3")
  >>= fun () ->
  check_ancestor "A3" "B1" (vblock s "A3")
  >>= fun () ->
  check_ancestor "B1" "A3" (vblock s "A3")
  >>= fun () ->
  check_ancestor "A2" "B1" (vblock s "A2")
  >>= fun () ->
  check_ancestor "B1" "A2" (vblock s "A2") >>= fun () -> return_unit

(****************************************************************************)

let seed =
  let receiver_id =
    P2p_peer.Id.of_string_exn (String.make P2p_peer.Id.size 'r')
  in
  let sender_id =
    P2p_peer.Id.of_string_exn (String.make P2p_peer.Id.size 's')
  in
  {Block_locator.receiver_id; sender_id}

(** Checks that the locator of some block (i.e., a number of
    predecessors) corresponds to an expected length and value. *)
let test_locator s =
  let check_locator length h1 expected =
    State.compute_locator s.chain ~max_size:length (vblock s h1) seed
    >>= fun l ->
    let (_, l) = (l : Block_locator.t :> _ * _) in
    match
      List.iter2
        ~when_different_lengths:()
        (fun h h2 ->
          if not (Block_hash.equal h (State.Block.hash @@ vblock s h2)) then
            Assert.fail_msg "Invalid locator %s (expected: %s)" h1 h2)
        l
        expected
    with
    | Error () ->
        Assert.fail_msg
          "Invalid locator length %s (found: %d, expected: %d)"
          h1
          (List.length l)
          (List.length expected)
    | Ok () ->
        Lwt.return_unit
  in
  check_locator 6 "A8" ["A7"; "A6"; "A5"; "A4"; "A3"; "A2"]
  >>= fun () ->
  check_locator 8 "B8" ["B7"; "B6"; "B5"; "B4"; "B3"; "B2"; "B1"; "A3"]
  >>= fun () ->
  check_locator 4 "B8" ["B7"; "B6"; "B5"; "B4"]
  >>= fun () ->
  check_locator 0 "A5" []
  >>= fun () ->
  check_locator 100 "A5" ["A4"; "A3"; "A2"; "A1"; "Genesis"]
  >>= fun () -> return_unit

(****************************************************************************)

(** Chain.known_heads *)

let compare s name heads l =
  if List.length heads <> List.length l then
    Assert.fail_msg
      "unexpected known_heads size (%s: %d %d)"
      name
      (List.length heads)
      (List.length l) ;
  List.iter
    (fun bname ->
      let hash = State.Block.hash (vblock s bname) in
      if
        not
          (List.exists
             (fun b -> Block_hash.equal hash (State.Block.hash b))
             heads)
      then Assert.fail_msg "missing block in known_heads (%s: %s)" name bname)
    l

(** Asserts that the initial chain heads of the example chain are A8
    and B8, (using [Store.Chain.known_heads]). *)
let test_known_heads s =
  Chain.known_heads s.chain
  >>= fun heads ->
  compare s "initial" heads ["A8"; "B8"] ;
  return_unit

(****************************************************************************)

(** First, verifies that the chain head is set to genesis. Then, sets
    chain head to A6 then checks if head is A6 (using
    [Chain.head/set_head]). *)
let test_head s =
  Chain.head s.chain
  >>= fun head ->
  if
    not
      (Block_hash.equal
         (State.Block.hash head)
         Shell_test_helpers.genesis_block_hash)
  then Assert.fail_msg "unexpected head" ;
  Chain.set_head s.chain (vblock s "A6")
  >>= fun _ ->
  Chain.head s.chain
  >>= fun head ->
  if
    not
      (Block_hash.equal
         (State.Block.hash head)
         (State.Block.hash @@ vblock s "A6"))
  then Assert.fail_msg "unexpected head" ;
  return_unit

(****************************************************************************)

(** Checks whether some block belongs (or not) to the current mainchain
    (using Store.Chain.is_in_chain).

    Genesis - A1 - A2 - A3 - A4 - A5 - A6 - A7 - A8
                             \
                              B1 - B2 - B3 - B4 - B5 - B6 - B7 - B8
*)
let test_mem s =
  let mem s x = Chain.mem s.chain (State.Block.hash @@ vblock s x) in
  let test_mem s x =
    mem s x
    >>= function
    | true -> Lwt.return_unit | false -> Assert.fail_msg "mem %s" x
  in
  let test_not_mem s x =
    mem s x
    >>= function
    | false -> Lwt.return_unit | true -> Assert.fail_msg "not (mem %s)" x
  in
  test_not_mem s "A3"
  >>= fun () ->
  test_not_mem s "A6"
  >>= fun () ->
  test_not_mem s "A8"
  >>= fun () ->
  test_not_mem s "B1"
  >>= fun () ->
  test_not_mem s "B6"
  >>= fun () ->
  test_not_mem s "B8"
  >>= fun () ->
  Chain.set_head s.chain (vblock s "A8")
  >>= fun _ ->
  test_mem s "A3"
  >>= fun () ->
  test_mem s "A6"
  >>= fun () ->
  test_mem s "A8"
  >>= fun () ->
  test_not_mem s "B1"
  >>= fun () ->
  test_not_mem s "B6"
  >>= fun () ->
  test_not_mem s "B8"
  >>= fun () ->
  Chain.set_head s.chain (vblock s "A6")
  >>= fun _ ->
  test_mem s "A3"
  >>= fun () ->
  test_mem s "A6"
  >>= fun () ->
  test_not_mem s "A8"
  >>= fun () ->
  test_not_mem s "B1"
  >>= fun () ->
  test_not_mem s "B6"
  >>= fun () ->
  test_not_mem s "B8"
  >>= fun () ->
  Chain.set_head s.chain (vblock s "B6")
  >>= fun _ ->
  test_mem s "A3"
  >>= fun () ->
  test_not_mem s "A4"
  >>= fun () ->
  test_not_mem s "A6"
  >>= fun () ->
  test_not_mem s "A8"
  >>= fun () ->
  test_mem s "B1"
  >>= fun () ->
  test_mem s "B6"
  >>= fun () ->
  test_not_mem s "B8"
  >>= fun () ->
  Chain.set_head s.chain (vblock s "B8")
  >>= fun _ ->
  test_mem s "A3"
  >>= fun () ->
  test_not_mem s "A4"
  >>= fun () ->
  test_not_mem s "A6"
  >>= fun () ->
  test_not_mem s "A8"
  >>= fun () ->
  test_mem s "B1"
  >>= fun () ->
  test_mem s "B6" >>= fun () -> test_mem s "B8" >>= fun () -> return_unit

(****************************************************************************)

(** Asserts the expected common ancestor and path as obtained by
    [Store.Chain_traversal.new_blocks] for a series of block pairs in
    the example chain. *)
let test_new_blocks s =
  let test s head h expected_ancestor expected =
    let to_block = vblock s head and from_block = vblock s h in
    Chain_traversal.new_blocks ~from_block ~to_block
    >>= fun (ancestor, blocks) ->
    if
      not
        (Block_hash.equal
           (State.Block.hash ancestor)
           (State.Block.hash @@ vblock s expected_ancestor))
    then
      Assert.fail_msg
        "Invalid ancestor %s -> %s (expected: %s)"
        head
        h
        expected_ancestor ;
    match
      List.iter2
        ~when_different_lengths:()
        (fun h1 h2 ->
          if
            not
              (Block_hash.equal
                 (State.Block.hash h1)
                 (State.Block.hash @@ vblock s h2))
          then
            Assert.fail_msg
              "Invalid new blocks %s -> %s (expected: %s)"
              head
              h
              h2)
        blocks
        expected
    with
    | Error () ->
        Assert.fail_msg
          "Invalid locator length %s (found: %d, expected: %d)"
          h
          (List.length blocks)
          (List.length expected)
    | Ok () ->
        Lwt.return_unit
  in
  test s "A6" "A6" "A6" []
  >>= fun () ->
  test s "A8" "A6" "A6" ["A7"; "A8"]
  >>= fun () ->
  test s "A8" "B7" "A3" ["A4"; "A5"; "A6"; "A7"; "A8"]
  >>= fun () -> return_unit

(****************************************************************************)

let tests : (string * (state -> unit tzresult Lwt.t)) list =
  [ ("init", test_init);
    ("read_block", test_read_block);
    ("path", test_path);
    ("ancestor", test_ancestor);
    ("locator", test_locator);
    ("known_heads", test_known_heads);
    ("head", test_head);
    ("mem", test_mem);
    ("new_blocks", test_new_blocks);
    ( "set_checkpoint_then_purge_rolling",
      test_set_checkpoint_then_purge_rolling );
    ("set_checkpoint_then_purge_full", test_set_checkpoint_then_purge_full) ]

let wrap (n, f) =
  Alcotest_lwt.test_case n `Quick (fun _ () ->
      Lwt_utils_unix.with_tempdir "tezos_test_" (fun dir ->
          wrap_state_init f dir
          >>= function
          | Ok () ->
              Lwt.return_unit
          | Error error ->
              Format.kasprintf Stdlib.failwith "%a" pp_print_error error))

let tests = List.map wrap tests
