(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) Nomadic Labs, <contact@nomadic-labs.com>.                   *)
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

let uid = ref 0

(* Create a block hash that depends deterministically on the level and messages
   content. *)
let make_block_hash level messages =
  let h_msgs = Hashtbl.hash messages in
  (* Rudimentary hash for level and messages. *)
  let hash_string = Z.of_int (Int32.to_int level + (7 * h_msgs)) |> Z.to_bits in
  let len = String.length hash_string in
  (* Left pad hash_string with null bytes *)
  let s =
    String.init Block_hash.size (fun i ->
        if i >= len then '\000' else hash_string.[i])
  in
  Block_hash.of_string_exn s

let default_constants =
  (* Same as default test constants for alpha excepted for
     commitment_period_in_block. *)
  Rollup_constants.
    {
      minimal_block_delay = 1L;
      delay_increment_per_round = 1L;
      sc_rollup =
        {
          challenge_window_in_blocks = 4032;
          commitment_period_in_blocks = 3;
          reveal_activation_level =
            Some
              {
                blake2B = 0l;
                metadata = 0l;
                dal_page = 0l;
                dal_parameters = 0l;
                dal_attested_slots_validity_lag = Int32.max_int;
              };
          max_number_of_stored_cemented_commitments = 5;
        };
      dal =
        {
          feature_enable = false;
          attestation_lag = 4;
          number_of_slots = 16;
          cryptobox_parameters =
            {
              redundancy_factor = 8;
              page_size = 4096;
              slot_size = 32768;
              number_of_shards = 64;
            };
        };
    }

let add_l2_genesis_block (node_ctxt : _ Node_context.t) ~boot_sector =
  let open Lwt_result_syntax in
  let head =
    Layer1.{hash = Block_hash.zero; level = node_ctxt.genesis_info.level}
  in
  let* () = Node_context.save_level node_ctxt head in
  let predecessor =
    Layer1.
      {
        hash =
          Block_hash.of_hex_exn
            (`Hex
              "0000000000000000000000000000000000000000000000000000000000000001");
        level = Int32.pred head.level;
      }
  in
  let predecessor_timestamp = Time.Protocol.epoch in
  let*? (module Plugin) =
    Protocol_plugins.proto_plugin_for_protocol
      (Reference.get node_ctxt.current_protocol).hash
  in
  let inbox =
    (* The global inbox starts at level 0, with the origination. *)
    Plugin.Inbox.init
      ~predecessor_timestamp
      ~predecessor:predecessor.hash
      ~level:node_ctxt.genesis_info.level
  in
  let* inbox_hash = Node_context.save_inbox node_ctxt inbox in
  let inbox_witness = Inbox.current_witness inbox in
  let* () =
    Node_context.save_messages node_ctxt inbox_witness ~level:head.level []
  in
  let ctxt = Context.empty node_ctxt.context in
  let num_ticks = 0L in
  let initial_tick = Z.zero in
  let*! initial_state = Plugin.Pvm.initial_state node_ctxt.kind in
  let*! state =
    Plugin.Pvm.install_boot_sector node_ctxt.kind initial_state boot_sector
  in
  let*! genesis_state_hash = Plugin.Pvm.state_hash node_ctxt.kind state in
  let*! ctxt = Context.PVMState.set ctxt state in
  let*! context_hash = Context.commit ctxt in
  let commitment =
    Commitment.genesis_commitment
      ~origination_level:node_ctxt.genesis_info.level
      ~genesis_state_hash
  in
  let* commitment_hash = Node_context.save_commitment node_ctxt commitment in
  let previous_commitment_hash = node_ctxt.genesis_info.commitment_hash in
  let header =
    Sc_rollup_block.
      {
        block_hash = head.hash;
        level = node_ctxt.genesis_info.level;
        predecessor = predecessor.hash;
        commitment_hash = Some commitment_hash;
        previous_commitment_hash;
        context = context_hash;
        inbox_witness;
        inbox_hash;
      }
  in
  let l2_block =
    Sc_rollup_block.{header; content = (); num_ticks; initial_tick}
  in
  let* () = Node_context.save_l2_block node_ctxt l2_block in
  let* () = Node_context.set_l2_head node_ctxt l2_block in
  return l2_block

let initialize_node_context ?data_dir protocol ?(constants = default_constants)
    kind ~boot_sector =
  let open Lwt_result_syntax in
  incr uid ;
  (* To avoid any conflict with previous runs of this test. *)
  let data_dir =
    match data_dir with
    | Some d -> d
    | None ->
        Tezt.Temp.dir
          (Format.asprintf
             "sc-rollup-node-test-%a-%d"
             Protocol_hash.pp_short
             protocol
             !uid)
  in
  let base_dir =
    Tezt.Temp.dir
      (Format.asprintf
         "sc-rollup-node-test-%a-base-%d"
         Protocol_hash.pp_short
         protocol
         !uid)
  in
  let filesystem = String.Hashtbl.create 10 in
  let cctxt = new Faked_client_context.unix_faked ~base_dir ~filesystem in
  let current_protocol =
    {Node_context.hash = protocol; proto_level = 0; constants}
  in
  let* ctxt =
    Node_context_loader.Internal_for_tests.create_node_context
      cctxt
      current_protocol
      ~data_dir
      kind
  in
  let* genesis = add_l2_genesis_block ctxt ~boot_sector in
  let commitment_hash =
    WithExceptions.Option.get ~loc:__LOC__ genesis.header.commitment_hash
  in
  let ctxt =
    {ctxt with genesis_info = {ctxt.genesis_info with commitment_hash}}
  in
  return (ctxt, genesis)

let with_node_context ?data_dir ?constants kind protocol ~boot_sector f =
  let open Lwt_result_syntax in
  let* node_ctxt, genesis =
    initialize_node_context ?data_dir protocol ?constants kind ~boot_sector
  in
  Lwt.finalize (fun () -> f node_ctxt ~genesis) @@ fun () ->
  let open Lwt_syntax in
  let* _ = Node_context_loader.close node_ctxt in
  return_unit

let make_header ~predecessor level messages =
  let hash = make_block_hash level messages in
  let timestamp = Time.Protocol.of_seconds (Int64.of_int32 level) in
  let header : Block_header.shell_header =
    {
      level;
      predecessor;
      timestamp;
      (* dummy values below *)
      proto_level = 0;
      validation_passes = 3;
      operations_hash = Tezos_crypto.Hashed.Operation_list_list_hash.zero;
      fitness = [];
      context = Tezos_crypto.Hashed.Context_hash.zero;
    }
  in
  {Layer1.hash; level; header}

let header_of_block (block : Sc_rollup_block.t) =
  let hash = block.header.block_hash in
  let level = block.header.level in
  let timestamp = Time.Protocol.of_seconds (Int64.of_int32 level) in
  let header : Block_header.shell_header =
    {
      level;
      predecessor = block.header.predecessor;
      timestamp;
      (* dummy values below *)
      proto_level = 0;
      validation_passes = 3;
      operations_hash = Tezos_crypto.Hashed.Operation_list_list_hash.zero;
      fitness = [];
      context = Tezos_crypto.Hashed.Context_hash.zero;
    }
  in
  {Layer1.hash; level; header}

let add_l2_block (node_ctxt : _ Node_context.t) ?(is_first_block = false)
    ~(predecessor_l2_block : Sc_rollup_block.t) messages =
  let open Lwt_result_syntax in
  let* () =
    Node_context.save_level
      node_ctxt
      {
        Layer1.hash = predecessor_l2_block.header.block_hash;
        level = predecessor_l2_block.header.level;
      }
  in
  let* old_head = Node_context.last_processed_head_opt node_ctxt in
  let* () =
    match old_head with
    | Some o
      when Block_hash.(
             o.header.block_hash = predecessor_l2_block.header.block_hash) ->
        return_unit
    | _ -> Node_context.set_l2_head node_ctxt predecessor_l2_block
  in
  let pred_level = predecessor_l2_block.header.level in
  let head =
    make_header
      ~predecessor:predecessor_l2_block.header.block_hash
      (Int32.succ pred_level)
      messages
  in
  let predecessor = header_of_block predecessor_l2_block in
  let*? plugin =
    Protocol_plugins.proto_plugin_for_protocol
      (Reference.get node_ctxt.current_protocol).hash
  in
  Rollup_node_daemon.Internal_for_tests.process_messages
    plugin
    node_ctxt
    ~is_first_block
    ~predecessor
    head
    messages

let append_l2_block (node_ctxt : _ Node_context.t) ?(is_first_block = false)
    messages =
  let open Lwt_result_syntax in
  let*! () = Lwt.pause () in
  let* predecessor_l2_block = Node_context.last_processed_head_opt node_ctxt in
  let* predecessor_l2_block =
    match predecessor_l2_block with
    | Some b -> return b
    | None ->
        failwith "No genesis block, please add one with add_l2_genesis_block"
  in
  add_l2_block node_ctxt ~is_first_block ~predecessor_l2_block messages

let append_l2_blocks node_ctxt message_batches =
  List.map_es (append_l2_block node_ctxt) message_batches

let append_dummy_l2_chain node_ctxt ~length =
  let open Lwt_result_syntax in
  let* head = Node_context.last_processed_head_opt node_ctxt in
  let head_level =
    match head with None -> 0 | Some h -> h.header.level |> Int32.to_int
  in
  let batches =
    Stdlib.List.init length (fun i ->
        ["\001" (* External tag *) ^ Z.to_bits (Z.of_int (i + head_level + 1))])
  in
  append_l2_blocks node_ctxt batches

module V4 = struct
  open Octez_smart_rollup_node_store

  let migrate_back_block (v4_store : Store_v4.rw) (node_ctxt : _ Node_context.t)
      (block : Sc_rollup_block.t) =
    let open Lwt_result_syntax in
    let* () =
      Store_v4.Levels_to_hashes.add
        v4_store.levels_to_hashes
        block.header.level
        block.header.block_hash
    in
    let* () =
      Option.iter_es
        (fun commitment_hash ->
          let* commitment =
            Node_context.get_commitment node_ctxt commitment_hash
          in
          Store_v4.Commitments.append
            v4_store.commitments
            ~key:commitment_hash
            ~value:commitment)
        block.header.commitment_hash
    in
    let* inbox = Node_context.get_inbox node_ctxt block.header.inbox_hash in
    let* () =
      Store_v4.Inboxes.append
        v4_store.inboxes
        ~key:block.header.inbox_hash
        ~value:inbox
    in
    let* messages =
      Node_context.find_messages node_ctxt block.header.inbox_witness
    in
    let* () =
      Store_v4.Messages.append
        v4_store.messages
        ~key:block.header.inbox_witness
        ~header:block.header.predecessor
        ~value:(Option.value messages ~default:[])
    in
    let* () =
      Store_v4.L2_blocks.append
        v4_store.l2_blocks
        ~key:block.header.block_hash
        ~header:block.header
        ~value:{block with header = ()}
    in
    let* () = Store_v4.L2_head.write v4_store.l2_head block in
    return_unit

  let migrate_back v4_store node_ctxt chain =
    List.iter_es (migrate_back_block v4_store node_ctxt) chain

  let with_store_migration_node_context ?constants kind protocol ~boot_sector
      ~chain_size f =
    let open Lwt_result_syntax in
    let* data_dir, cctxt, current_protocol, genesis, chain =
      with_node_context ?constants kind protocol ~boot_sector
      @@ fun node_ctxt ~genesis ->
      let* chain = append_dummy_l2_chain node_ctxt ~length:chain_size in
      let* v4_store =
        Store_v4.load
          Read_write
          ~index_buffer_size:1000
          ~l2_blocks_cache_size:1
          (Configuration.default_storage_dir node_ctxt.data_dir)
      in
      let chain = genesis :: chain in
      let* () = migrate_back v4_store node_ctxt chain in
      let* () = Store_v4.close v4_store in
      return
        ( node_ctxt.data_dir,
          node_ctxt.cctxt,
          Reference.get node_ctxt.current_protocol,
          genesis,
          chain )
    in
    (* Clean up V5 store *)
    let*! () =
      List.iter_s
        (fun f ->
          Lwt.catch
            (fun () -> Lwt_unix.unlink (Filename.concat data_dir f))
            (fun _ -> Lwt.return_unit))
        Sql_store.(sqlite_file_name :: extra_sqlite_files)
    in
    let* () =
      Store_version.write_version_file
        ~dir:(Configuration.default_storage_dir data_dir)
        Store_v4.version
    in
    let* ctxt =
      Node_context_loader.Internal_for_tests.create_node_context
        cctxt
        current_protocol
        ~data_dir
        kind
    in
    let commitment_hash =
      WithExceptions.Option.get ~loc:__LOC__ genesis.header.commitment_hash
    in
    let ctxt =
      {ctxt with genesis_info = {ctxt.genesis_info with commitment_hash}}
    in
    Lwt.finalize (fun () -> f ctxt chain) @@ fun () ->
    let open Lwt_syntax in
    let* _ = Node_context_loader.close ctxt in
    return_unit
end

module Assert = struct
  module Make_with_encoding (X : sig
    type t

    val encoding : t Data_encoding.t
  end) =
  Assert.Make_equalities (struct
    type t = X.t

    let eq (b1 : X.t) (b2 : X.t) =
      Bytes.equal
        (Data_encoding.Binary.to_bytes_exn X.encoding b1)
        (Data_encoding.Binary.to_bytes_exn X.encoding b2)

    let pp ppf (b : X.t) =
      Data_encoding.Json.pp ppf (Data_encoding.Json.construct X.encoding b)
  end)

  module L2_block = Make_with_encoding (Sc_rollup_block)
  module Commitment = Make_with_encoding (Commitment)
  module Commitment_hash =
    Make_with_encoding (Octez_smart_rollup.Commitment.Hash)
  module State_hash = Make_with_encoding (State_hash)
end

let alcotest ?name speed ?constants kind protocol ~boot_sector f =
  let name =
    Format.asprintf
      "%s%a %a"
      (match name with None -> "" | Some n -> n ^ " - ")
      Protocol_hash.pp_short
      protocol
      Kind.pp
      kind
  in
  Alcotest_lwt.test_case name speed @@ fun _lwt_switch () ->
  let open Lwt_result_syntax in
  let*! r = with_node_context ?constants kind protocol ~boot_sector f in
  match r with
  | Ok () -> Lwt.return_unit
  | Error err ->
      Format.printf "@\n%a@." pp_print_trace err ;
      Lwt.fail Alcotest.Test_error

let store_migration_alcotest ?name speed ?constants kind protocol ~boot_sector
    ~chain_size f =
  let name =
    Format.asprintf
      "%s%a %a"
      (match name with None -> "" | Some n -> n ^ " - ")
      Protocol_hash.pp_short
      protocol
      Kind.pp
      kind
  in
  Alcotest_lwt.test_case name speed @@ fun _lwt_switch () ->
  let open Lwt_result_syntax in
  let*! r =
    V4.with_store_migration_node_context
      ?constants
      kind
      protocol
      ~boot_sector
      ~chain_size
      f
  in
  match r with
  | Ok () -> Lwt.return_unit
  | Error err ->
      Format.printf "@\n%a@." pp_print_trace err ;
      Lwt.fail Alcotest.Test_error
