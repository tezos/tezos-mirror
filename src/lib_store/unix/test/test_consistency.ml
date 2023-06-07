(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    _______

    Component: Store
    Invocation: dune exec src/lib_store/unix/test/main.exe \
                -- --file test_consistency.ml
    Subject: Store consistency tests
*)

open Test_utils

let nb_protocols = 5

let register_protocol ~hash ~sources =
  let module M = struct
    include
      Registered_protocol.Register_embedded_V9
        (Tezos_protocol_environment_demo_noops)
        (Tezos_protocol_demo_noops.Protocol)
        (struct
          let hash = Some hash

          let sources = sources
        end)
  end in
  ()

let test_protocols =
  let mk i =
    let s = string_of_int i in
    let proto =
      {
        Protocol.expected_env = V9;
        components = [{name = s; interface = None; implementation = s}];
      }
    in
    let proto_hash = Protocol.hash proto in
    register_protocol ~hash:proto_hash ~sources:proto ;
    (proto_hash, proto)
  in
  Stdlib.List.init nb_protocols mk

let init_protocols store history_mode =
  let open Lwt_result_syntax in
  let chain_store = Store.main_chain_store store in
  assert (History_mode.equal (Store.Chain.history_mode chain_store) history_mode) ;
  let*! () =
    List.iter_s
      (fun (h, p) ->
        let*! r = Store.Protocol.store store h p in
        assert (r <> None) ;
        (* register protocol *)
        Lwt.return_unit)
      test_protocols
  in
  assert (List.for_all (Store.Protocol.mem store) (List.map fst test_protocols)) ;
  let blocks_proto = List.mapi (fun i p -> (succ i, fst p)) test_protocols in
  (* 10 blocks per cycle *)
  let nb_blocks_per_cycle = 10 in
  let constants =
    {
      Test_utils.default_protocol_constants with
      blocks_per_cycle = Int32.of_int nb_blocks_per_cycle;
      preserved_cycles = 1;
    }
  in
  let* () =
    List.iter_es
      (fun (protocol_level, proto_hash) ->
        let* _ =
          append_blocks
            ~constants
            ~max_operations_ttl:8
            ~kind:`Full
            ~should_set_head:true
            ~protocol_level
            ~set_protocol:proto_hash
            chain_store
            nb_blocks_per_cycle
        in
        let*! () =
          Block_store.await_merging (Store.Unsafe.get_block_store chain_store)
        in
        return_unit)
      blocks_proto
  in
  let*! all_proto_levels = Store.Chain.all_protocol_levels chain_store in
  let open Store_types in
  let*! _savepoint, savepoint_level = Store.Chain.savepoint chain_store in
  let* () =
    Protocol_levels.iter_es
      (fun proto_level {Protocol_levels.activation_block; _} ->
        let activation_block_hash, activation_block_level = activation_block in
        (* We cannot consider gc-ed blocks *)
        if activation_block_level < Int32.succ savepoint_level then return_unit
        else
          let* activation_block =
            Store.Block.read_block chain_store activation_block_hash
          in
          let* pred =
            Store.Block.read_predecessor chain_store activation_block
          in
          assert (Store.Block.proto_level activation_block = proto_level) ;
          assert (
            Store.Block.proto_level pred = Int.pred proto_level
            || proto_level = 0) ;
          return_unit)
      all_proto_levels
  in
  return all_proto_levels

let test_protocol_level_consistency_drop_one history_mode nth
    (store_dir, context_dir) store =
  let open Lwt_result_syntax in
  assert (nth < 5) ;
  let* _ = init_protocols store history_mode in
  let chain_store = Store.main_chain_store store in
  (* Close the store and remove a protocol level between savepoint and head *)
  let chain_id = Store.Chain.chain_id chain_store in
  let protocol_level_file =
    let dir = store_dir in
    let open Naming in
    store_dir ~dir_path:dir |> fun d ->
    chain_dir d chain_id |> protocol_levels_file
  in
  let*! () = Lwt_unix.unlink (Naming.encoded_file_path protocol_level_file) in
  let*! all_proto_levels = Store.Chain.all_protocol_levels chain_store in
  let*! _ =
    let proto_l = Store_types.Protocol_levels.bindings all_proto_levels in
    let protos =
      Store_types.Protocol_levels.of_seq
        (List.to_seq (List.take_n nth proto_l @ List.drop_n (nth + 1) proto_l))
    in
    Stored_data.init protocol_level_file ~initial_data:protos
  in
  let*! () = Store.close_store store in
  let* store' =
    Store.init
      ~patch_context:dummy_patch_context
      ~history_mode
      ~store_dir
      ~context_dir
      ~allow_testchains:true
      genesis
  in
  (* Check that between the savepoint and the head, all protocol
     levels are known and correct. *)
  let chain_store = Store.main_chain_store store' in
  let*! current_head = Store.Chain.current_head chain_store in
  let*! _savepoint, savepoint_level = Store.Chain.savepoint chain_store in
  let* () =
    List.iter_es
      (fun l ->
        let* b = Store.Block.read_block_by_level chain_store (Int32.of_int l) in
        let*! proto_opt =
          Store.Chain.find_protocol
            chain_store
            ~protocol_level:(Store.Block.proto_level b)
        in
        let* proto =
          match proto_opt with
          | Some proto -> return proto
          | None ->
              failwith
                "unexpected missing proto level %d in the protocol levels"
                (Store.Block.proto_level b)
        in
        let* ctxt = Store.Block.context chain_store b in
        let*! proto_block = Context_ops.get_protocol ctxt in
        Assert.equal
          ~eq:Protocol_hash.equal
          ~pp:Protocol_hash.pp
          ~loc:__LOC__
          proto
          proto_block ;
        return_unit)
      (Int32.to_int savepoint_level
      -- Int32.to_int (Store.Block.level current_head))
  in
  let*! () = Store.close_store store' in
  return_unit

let check_protocol_levels_availability chain_store ~expected_protocols
    ~recovered_protocols =
  let open Lwt_result_syntax in
  let open Store_types in
  let*! savepoint_hash, _ = Store.Chain.savepoint chain_store in
  let* savepoint = Store.Block.read_block chain_store savepoint_hash in
  let savepoint_proto_level = Store.Block.proto_level savepoint in
  Protocol_levels.iter
    (fun proto_level _ ->
      if proto_level < savepoint_proto_level then
        assert (not (Protocol_levels.mem proto_level recovered_protocols))
      else assert (Protocol_levels.mem proto_level recovered_protocols))
    expected_protocols ;
  return_unit

let test_protocol_level_consistency_remove_file history_mode
    (store_dir, context_dir) store =
  let open Lwt_result_syntax in
  let* expected_protocols = init_protocols store history_mode in
  let open Store_types in
  let chain_store = Store.main_chain_store store in
  (* Close the store and remove a protocol level between savepoint and head *)
  let chain_id = Store.Chain.chain_id chain_store in
  let protocol_level_file =
    let dir = store_dir in
    let open Naming in
    store_dir ~dir_path:dir |> fun d ->
    chain_dir d chain_id |> protocol_levels_file
  in
  let*! () = Lwt_unix.unlink (Naming.encoded_file_path protocol_level_file) in
  let*! _ =
    Stored_data.init protocol_level_file ~initial_data:Protocol_levels.empty
  in
  let*! () = Store.close_store store in
  let* store' =
    Store.init
      ~patch_context:dummy_patch_context
      ~history_mode
      ~store_dir
      ~context_dir
      ~allow_testchains:true
      genesis
  in
  (* Check that between the savepoint and the head, all protocol
     levels are known and correct. *)
  let chain_store = Store.main_chain_store store' in
  let*! current_head = Store.Chain.current_head chain_store in
  let*! _savepoint, savepoint_level = Store.Chain.savepoint chain_store in
  let* () =
    List.iter_es
      (fun l ->
        let* b = Store.Block.read_block_by_level chain_store (Int32.of_int l) in
        let*! proto_opt =
          Store.Chain.find_protocol
            chain_store
            ~protocol_level:(Store.Block.proto_level b)
        in
        let* proto =
          match proto_opt with
          | Some proto -> return proto
          | None ->
              failwith
                "unexpected missing proto level %d in the protocol levels"
                (Store.Block.proto_level b)
        in
        let*! ctxt = Store.Block.context_exn chain_store b in
        let*! proto_block = Context_ops.get_protocol ctxt in
        assert (Protocol_hash.(proto = proto_block)) ;
        return_unit)
      (Int32.to_int savepoint_level
      -- Int32.to_int (Store.Block.level current_head))
  in
  let*! recovered_protocols = Store.Chain.all_protocol_levels chain_store in
  let* () =
    check_protocol_levels_availability
      chain_store
      ~expected_protocols
      ~recovered_protocols
  in
  let*! () = Store.close_store store' in
  return_unit

let make_tests =
  let history_modes = History_mode.[Rolling (Some {offset = 1}); Archive] in
  let tests =
    Stdlib.List.init 5 (fun i history_mode ->
        ( Format.asprintf
            "protocol level consistency (%a): drop protocol #%d"
            History_mode.pp
            history_mode
            (i + 1),
          test_protocol_level_consistency_drop_one history_mode i ))
    @ [
        (fun history_mode ->
          ( Format.asprintf
              "protocol level consistency (%a): remove file"
              History_mode.pp
              history_mode,
            test_protocol_level_consistency_remove_file history_mode ));
      ]
  in
  List.map (fun (hm, test) -> (hm, test hm)) List.(product history_modes tests)

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run
       ~__FILE__
       "tezos-store"
       [
         ( "consistency",
           List.map
             (fun (history_mode, test) ->
               wrap_test
                 ~block_cache_limit:1
                 ~history_mode
                 ~manual_close:true
                 test)
             make_tests );
       ]
