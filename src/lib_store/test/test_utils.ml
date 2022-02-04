(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Alcotest_lwt
open Filename.Infix

let genesis_hash =
  Block_hash.of_b58check_exn
    "BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2"

let genesis =
  {
    Genesis.block = genesis_hash;
    time = Time.Protocol.epoch;
    protocol = Tezos_protocol_alpha.Protocol.hash;
  }

let default_protocol_constants = Default_parameters.constants_test

let default_max_operations_ttl = 1

let check_invariants ?(expected_checkpoint = None) ?(expected_savepoint = None)
    ?(expected_caboose = None) chain_store =
  Lwt.catch
    (fun () ->
      let open Store in
      Chain.current_head chain_store >>= fun current_head ->
      Chain.checkpoint chain_store >>= fun checkpoint ->
      (match expected_savepoint with
      | Some savepoint -> Lwt.return savepoint
      | None -> Chain.savepoint chain_store)
      >>= fun savepoint ->
      (match expected_caboose with
      | Some caboose -> Lwt.return caboose
      | None -> Chain.caboose chain_store)
      >>= fun caboose ->
      Format.eprintf
        "head %ld - savepoint %ld - checkpoint %ld - caboose %ld@."
        (Store.Block.level current_head)
        (snd savepoint)
        (snd checkpoint)
        (snd caboose) ;
      (Block.get_block_metadata_opt chain_store current_head >>= function
       | None ->
           Assert.fail_msg "check_invariant: could not find head's metadata"
       | Some metadata -> Lwt.return metadata)
      >>= fun head_metadata ->
      let expected_checkpoint_level =
        match expected_checkpoint with
        | Some l -> snd l
        | None -> Block.last_allowed_fork_level head_metadata
      in
      Assert.is_true
        ~msg:
          (Format.sprintf
             "check_invariant: checkpoint.level(%ld) < \
              head.last_allowed_fork_level(%ld)"
             (snd checkpoint)
             expected_checkpoint_level)
        Compare.Int32.(snd checkpoint >= expected_checkpoint_level) ;
      Block.read_block_opt chain_store (fst savepoint)
      >>= fun savepoint_b_opt ->
      Block.read_block_metadata chain_store (fst savepoint)
      >>=? fun savepoint_metadata_opt ->
      (match (savepoint_b_opt, savepoint_metadata_opt) with
      | (Some _, Some _) -> Lwt.return_unit
      | (Some _, None) ->
          Assert.fail_msg "check_invariant: could not find savepoint's metadata"
      | _ -> Assert.fail_msg "check_invariant: could not find savepoint block")
      >>= fun () ->
      Block.read_block_opt chain_store (fst caboose) >>= fun caboose_b_opt ->
      Block.read_block_metadata chain_store (fst caboose)
      >>=? fun caboose_metadata_opt ->
      match (caboose_b_opt, caboose_metadata_opt) with
      | (Some _, (Some _ | None)) -> return_unit
      | (None, _) ->
          Format.eprintf "caboose lvl : %ld@." (snd caboose) ;
          Assert.fail_msg "check_invariant: could not find the caboose block")
    (fun exn ->
      Store.make_pp_chain_store chain_store >>= fun pp ->
      Format.printf "DEBUG CHAIN STORE: %a@." pp () ;
      Lwt.fail exn)

let dummy_patch_context ctxt =
  let open Tezos_context in
  let open Tezos_protocol_alpha in
  Context.add ctxt ["version"] (Bytes.of_string "genesis") >>= fun ctxt ->
  let open Tezos_protocol_alpha_parameters in
  let proto_params =
    let json =
      Default_parameters.json_of_parameters
        Default_parameters.(parameters_of_constants constants_test)
    in
    Data_encoding.Binary.to_bytes_exn Data_encoding.json json
  in
  Context.add ctxt ["protocol_parameters"] proto_params >>= fun ctxt ->
  let ctxt = Tezos_shell_context.Shell_context.wrap_disk_context ctxt in
  Protocol.Main.init
    ctxt
    {
      level = 0l;
      proto_level = 0;
      predecessor = genesis.block;
      timestamp = genesis.time;
      validation_passes = 0;
      operations_hash = Operation_list_list_hash.empty;
      fitness = [];
      context = Context_hash.zero;
    }
  >>= fun res ->
  Lwt.return (Protocol.Environment.wrap_tzresult res) >>=? fun {context; _} ->
  return (Tezos_shell_context.Shell_context.unwrap_disk_context context)

let wrap_store_init ?(patch_context = dummy_patch_context)
    ?(history_mode = History_mode.Archive) ?(allow_testchains = true)
    ?(keep_dir = false) k _ () : unit Lwt.t =
  let prefix_dir = "tezos_indexed_store_test_" in
  let run f =
    if not keep_dir then Lwt_utils_unix.with_tempdir prefix_dir f
    else
      let base_dir = Filename.temp_file prefix_dir "" in
      Format.printf "temp dir: %s@." base_dir ;
      Lwt_unix.unlink base_dir >>= fun () ->
      Lwt_unix.mkdir base_dir 0o700 >>= fun () -> f base_dir
  in
  run (fun base_dir ->
      let store_dir = base_dir // "store" in
      let context_dir = base_dir // "context" in
      Store.init
        ~patch_context
        ~history_mode
        ~store_dir
        ~context_dir
        ~allow_testchains
        genesis
      >>=? fun store ->
      protect
        ~on_error:(fun err ->
          Store.make_pp_store store >>= fun pp_store ->
          Format.eprintf "@[<v>DEBUG:@ %a@]@." pp_store () ;
          Store.close_store store >>= fun () -> Lwt.return (Error err))
        (fun () ->
          k (store_dir, context_dir) store >>=? fun () ->
          Format.printf "Invariants check before closing@." ;
          check_invariants (Store.main_chain_store store) >>=? fun () ->
          Store.close_store store >>= fun () ->
          Store.init
            ~history_mode
            ~store_dir
            ~context_dir
            ~allow_testchains
            genesis
          >>=? fun store' ->
          Format.printf "Invariants check after reloading@." ;
          Lwt.finalize
            (fun () -> check_invariants (Store.main_chain_store store'))
            (fun () -> Store.close_store store' >>= fun _ -> Lwt.return_unit)
          >>=? fun () -> return_unit))
  >>= function
  | Error err ->
      Format.printf "@\nTest failed:@\n%a@." Error_monad.pp_print_trace err ;
      Lwt.fail Alcotest.Test_error
  | Ok r -> Lwt.return r

let wrap_test ?history_mode ?(speed = `Quick) ?patch_context ?keep_dir (name, f)
    =
  test_case
    name
    speed
    (wrap_store_init ?patch_context ?history_mode ?keep_dir f)

let wrap_simple_store_init ?(patch_context = dummy_patch_context)
    ?(history_mode = History_mode.default) ?(allow_testchains = true)
    ?(keep_dir = false) k _ () : unit Lwt.t =
  let prefix_dir = "tezos_indexed_store_test_" in
  let run f =
    if not keep_dir then Lwt_utils_unix.with_tempdir prefix_dir f
    else
      let base_dir = Filename.temp_file prefix_dir "" in
      Lwt_unix.unlink base_dir >>= fun () ->
      Lwt_unix.mkdir base_dir 0o700 >>= fun () -> f base_dir
  in
  run (fun base_dir ->
      let store_dir = base_dir // "store" in
      let context_dir = base_dir // "context" in
      Store.init
        ~history_mode
        ~patch_context
        ~store_dir
        ~context_dir
        ~allow_testchains
        genesis
      >>=? fun store ->
      protect
        ~on_error:(fun err ->
          Store.close_store store >>= fun () -> Lwt.return (Error err))
        (fun () ->
          Lwt.finalize
            (fun () -> k (store_dir, context_dir) store)
            (fun () -> Store.close_store store >>= fun _ -> Lwt.return_unit)))
  >>= function
  | Error err ->
      Format.printf "@\nTest failed:@\n%a@." Error_monad.pp_print_trace err ;
      Lwt.fail Alcotest.Test_error
  | Ok () -> Lwt.return_unit

let wrap_simple_store_init_test ?history_mode ?(speed = `Quick) ?patch_context
    ?keep_dir (name, f) =
  test_case
    name
    speed
    (wrap_simple_store_init ?history_mode ?patch_context ?keep_dir f)

let make_raw_block ?min_lafl ?(max_operations_ttl = default_max_operations_ttl)
    ?(constants = default_protocol_constants) ?(context = Context_hash.zero)
    (pred_block_hash, pred_block_level) =
  let level = Int32.succ pred_block_level in
  let header =
    {
      Block_header.shell =
        {
          level;
          proto_level = 0;
          predecessor = pred_block_hash;
          timestamp = Time.Protocol.(add epoch (Int64.of_int32 level));
          validation_passes =
            List.length Tezos_protocol_alpha.Protocol.Main.validation_passes;
          operations_hash = Operation_list_list_hash.zero;
          fitness = [Bytes.create 8];
          context;
        };
      protocol_data =
        (* make it random to be certain to obtain a different hash *)
        Data_encoding.(Binary.to_bytes_exn int31 Random.(int (bits ())));
    }
  in
  let hash = Block_header.hash header in
  let last_allowed_fork_level =
    let current_cycle = Int32.(div (pred level) constants.blocks_per_cycle) in
    Int32.(
      mul
        constants.blocks_per_cycle
        (Compare.Int32.max
           0l
           (sub current_cycle (of_int constants.preserved_cycles))))
  in
  let last_allowed_fork_level =
    match min_lafl with
    | Some min_lafl -> Compare.Int32.max min_lafl last_allowed_fork_level
    | None -> last_allowed_fork_level
  in
  let metadata =
    Some
      {
        Block_repr.message = Some "message";
        max_operations_ttl;
        last_allowed_fork_level;
        block_metadata = Bytes.create 1;
        operations_metadata =
          List.map
            (fun _ -> [])
            Tezos_protocol_alpha.Protocol.Main.validation_passes;
      }
  in
  let operations =
    List.map (fun _ -> []) Tezos_protocol_alpha.Protocol.Main.validation_passes
  in
  {
    Block_repr.hash;
    contents =
      {
        header;
        operations;
        block_metadata_hash = None;
        operations_metadata_hashes = None;
      };
    metadata;
  }

let prune_block block = block.Block_repr.metadata <- None

let pp_block fmt b =
  let (h, lvl) = Store.Block.descriptor b in
  Format.fprintf fmt "%a (%ld)" Block_hash.pp h lvl

let raw_descriptor b = (Block_repr.hash b, Block_repr.level b)

let pp_raw_block fmt b =
  let (h, lvl) = raw_descriptor b in
  Format.fprintf fmt "%a (%ld)" Block_hash.pp h lvl

let store_raw_block chain_store (raw_block : Block_repr.t) =
  let metadata =
    WithExceptions.Option.get ~loc:__LOC__ (Block_repr.metadata raw_block)
  in
  let ops_metadata =
    let operations_metadata = Block_repr.operations_metadata metadata in
    match Block_repr.operations_metadata_hashes raw_block with
    | Some metadata_hashes ->
        let res =
          WithExceptions.List.map2
            ~loc:__LOC__
            (WithExceptions.List.map2 ~loc:__LOC__ (fun x y -> (x, y)))
            operations_metadata
            metadata_hashes
        in
        Block_validation.Metadata_hash res
    | None -> Block_validation.(No_metadata_hash operations_metadata)
  in
  let validation_result =
    {
      Tezos_validation.Block_validation.validation_store =
        {
          context_hash = Block_repr.context raw_block;
          timestamp = Block_repr.timestamp raw_block;
          message = Block_repr.message metadata;
          max_operations_ttl = Block_repr.max_operations_ttl metadata;
          last_allowed_fork_level = Block_repr.last_allowed_fork_level metadata;
        };
      block_metadata =
        ( Block_repr.block_metadata metadata,
          Block_repr.block_metadata_hash raw_block );
      ops_metadata;
    }
  in
  Store.Block.store_block
    chain_store
    ~block_header:(Block_repr.header raw_block)
    ~operations:(Block_repr.operations raw_block)
    validation_result
  >>= function
  | Ok (Some block) -> return block
  | Ok None ->
      Alcotest.failf
        "store_raw_block: could not store block %a (%ld)"
        Block_hash.pp
        (Block_repr.hash raw_block)
        (Block_repr.level raw_block)
  | Error _ as err -> Lwt.return err

let set_block_predecessor blk pred_hash =
  let open Block_repr in
  let open Block_header in
  {
    blk with
    contents =
      {
        blk.contents with
        header =
          {
            blk.contents.header with
            shell = {blk.contents.header.shell with predecessor = pred_hash};
          };
      };
  }

let make_raw_block_list ?min_lafl ?constants ?max_operations_ttl ?(kind = `Full)
    (pred_hash, pred_level) n =
  List.fold_left
    (fun ((pred_hash, pred_level), acc) _ ->
      let raw_block =
        make_raw_block
          ?min_lafl
          ?constants
          ?max_operations_ttl
          (pred_hash, pred_level)
      in
      if kind = `Pruned then prune_block raw_block ;
      ((Block_repr.hash raw_block, Block_repr.level raw_block), raw_block :: acc))
    ((pred_hash, pred_level), [])
    (1 -- n)
  |> snd
  |> fun l ->
  let blk = List.hd l |> WithExceptions.Option.get ~loc:__LOC__ in
  Lwt.return (List.rev l, blk)

let append_blocks ?min_lafl ?constants ?max_operations_ttl ?root ?(kind = `Full)
    ?(should_set_head = false) ?(should_commit = false) chain_store n =
  (match root with
  | Some (bh, bl) -> Lwt.return (bh, bl)
  | None ->
      Store.Chain.current_head chain_store >>= fun block ->
      Lwt.return (Store.Block.descriptor block))
  >>= fun root ->
  Store.Block.read_block chain_store (fst root) >>=? fun root_b ->
  Tezos_context.Context.checkout
    (Store.context_index (Store.Chain.global_store chain_store))
    (Store.Block.context_hash root_b)
  >>= fun ctxt_opt ->
  make_raw_block_list ?min_lafl ?constants ?max_operations_ttl ~kind root n
  >>= fun (blocks, _last) ->
  List.fold_left_es
    (fun (ctxt_opt, last_opt, blocks) b ->
      (if should_commit then
       let open Tezos_context in
       let ctxt = WithExceptions.Option.get ~loc:__LOC__ ctxt_opt in
       Tezos_context.Context.add
         ctxt
         ["level"]
         (Bytes.of_string (Format.asprintf "%ld" (Block_repr.level b)))
       >>= fun ctxt ->
       Context.commit ~time:Time.Protocol.epoch ctxt >>= fun ctxt_hash ->
       let predecessor =
         Option.value ~default:(Block_repr.predecessor b) last_opt
       in
       let shell =
         {
           b.contents.header.Block_header.shell with
           context = ctxt_hash;
           predecessor;
         }
       in
       let header =
         {Block_header.shell; protocol_data = b.contents.header.protocol_data}
       in
       let hash = Block_header.hash header in
       return
         ( Some ctxt,
           Some hash,
           {
             Block_repr.hash;
             contents =
               {
                 header;
                 operations = b.contents.operations;
                 block_metadata_hash = None;
                 operations_metadata_hashes = None;
               };
             metadata = b.metadata;
           } )
      else return (ctxt_opt, last_opt, b))
      >>=? fun (ctxt, last_opt, b) ->
      store_raw_block chain_store b >>=? fun b ->
      (if should_set_head then
       Store.Chain.set_head chain_store b >>=? fun _ -> return_unit
      else return_unit)
      >>=? fun () -> return (ctxt, last_opt, b :: blocks))
    (ctxt_opt, None, [])
    blocks
  >>=? fun (_, _, blocks) ->
  let head = List.hd blocks |> WithExceptions.Option.get ~loc:__LOC__ in
  return (List.rev blocks, head)

let append_cycle ?(constants = default_protocol_constants) ?max_operations_ttl
    ?root ?(kind = `Full) ?(should_set_head = false) chain_store =
  append_blocks
    chain_store
    ~constants
    ?max_operations_ttl
    ?root
    ~kind
    ~should_set_head
    (Int32.to_int constants.blocks_per_cycle)

let assert_presence_in_store ?(with_metadata = false) chain_store blocks =
  Lwt_list.iter_s
    (fun b ->
      let hash = Store.Block.hash b in
      Store.Block.read_block_opt chain_store hash >>= function
      | None ->
          Format.eprintf "Block %a not present in store@." pp_block b ;
          Lwt.fail Alcotest.Test_error
      | Some b' ->
          let b_header = Store.Block.header b in
          let b'_header = Store.Block.header b' in
          Assert.equal_block
            ~msg:"assert_presence: different header"
            b_header
            b'_header ;
          (if with_metadata then (
           Store.Block.get_block_metadata_opt chain_store b
           >>= fun b_metadata ->
           Store.Block.get_block_metadata_opt chain_store b'
           >>= fun b'_metadata ->
           Assert.equal_metadata
             b_metadata
             b'_metadata
             ~msg:"assert_presence: different metadata" ;
           Lwt.return_unit)
          else Lwt.return_unit)
          >>= fun () -> Lwt.return_unit)
    blocks
  >>= fun () -> return_unit

let assert_absence_in_store chain_store blocks =
  Lwt_list.iter_s
    (fun b ->
      Store.Block.(read_block_opt chain_store (hash b)) >>= function
      | None -> Lwt.return_unit
      | Some b' ->
          Store.Chain.current_head chain_store >>= fun current_head ->
          Store.Chain.checkpoint chain_store >>= fun checkpoint ->
          Store.Chain.savepoint chain_store >>= fun savepoint ->
          Store.Chain.caboose chain_store >>= fun caboose ->
          Format.printf
            "head %ld - savepoint %ld - checkpoint %ld - caboose %ld@."
            (Store.Block.level current_head)
            (snd savepoint)
            (snd checkpoint)
            (snd caboose) ;
          if not (Store.Block.equal b b') then
            Alcotest.failf
              "assert_absence_in_store: found %a and got a different block %a@."
              pp_block
              b
              pp_block
              b'
          else
            Alcotest.failf
              "assert_absence_in_store: unexpected block found in store %a@."
              pp_block
              b')
    blocks
  >>= fun () -> return_unit

let set_head_by_level chain_store lvl =
  Store.Block.read_block_by_level chain_store lvl >>=? fun head ->
  Store.Chain.set_head chain_store head

module Example_tree = struct
  (**********************************************************

    Genesis (H) - A1 - A2 - A3 - A4 - A5 - A6 - A7 - A8
                         \
                          B1 - B2 - B3 - B4 - B5 - B6 - B7 - B8

   **********************************************************)

  module Nametbl = Hashtbl.MakeSeeded (struct
    type t = string

    let hash = Hashtbl.seeded_hash

    let equal = String.equal
  end)

  let build_example_tree store =
    let combine_exn l1 l2 =
      match
        List.combine ~when_different_lengths:(Failure "combine_exn") l1 l2
      with
      | Ok x -> x
      | Error _ -> assert false
    in
    let chain_store = Store.main_chain_store store in
    let main_chain = List.map (fun i -> Format.sprintf "A%d" i) (1 -- 8) in
    append_blocks chain_store ~kind:`Full (List.length main_chain)
    >>=? fun (blocks, _head) ->
    Lwt_list.filter_map_s
      (fun b -> Store.Block.read_block_opt chain_store (Store.Block.hash b))
      blocks
    >>= fun main_blocks ->
    let a2 = List.nth main_blocks 2 |> WithExceptions.Option.get ~loc:__LOC__ in
    let main_blocks = combine_exn main_chain main_blocks in
    let branch_chain = List.map (fun i -> Format.sprintf "B%d" i) (1 -- 8) in
    append_blocks
      chain_store
      ~root:(Store.Block.descriptor a2)
      ~kind:`Full
      (List.length branch_chain)
    >>=? fun (branch, _head) ->
    Lwt_list.filter_map_s
      (fun b -> Store.Block.read_block_opt chain_store (Store.Block.hash b))
      branch
    >>= fun branch_blocks ->
    let branch_blocks = combine_exn branch_chain branch_blocks in
    let vtbl = Nametbl.create 17 in
    Store.Chain.genesis_block chain_store >>= fun genesis ->
    Nametbl.add vtbl "Genesis" genesis ;
    List.iter (fun (k, b) -> Nametbl.add vtbl k b) (main_blocks @ branch_blocks) ;
    assert_presence_in_store chain_store (blocks @ branch) >>=? fun () ->
    return vtbl

  let rev_lookup block_hash tbl =
    Nametbl.fold
      (fun k b -> function
        | None ->
            if Block_hash.equal block_hash (Store.Block.hash b) then Some k
            else None
        | x -> x)
      tbl
      None
    |> WithExceptions.Option.to_exn ~none:Not_found

  let vblock tbl k = Nametbl.find tbl k

  let wrap_test ?keep_dir ?history_mode (name, g) =
    let f _ store =
      let chain_store = Store.main_chain_store store in
      build_example_tree store >>=? fun tbl -> g chain_store tbl
    in
    wrap_test ?keep_dir ?history_mode (name, f)
end
