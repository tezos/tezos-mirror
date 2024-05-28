(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Testing
    _______

    Component: Store
    Invocation: dune exec src/lib_store/unix/test/main.exe -- --match history_mode_switch
    Subject: Store tests ( history_mode_switch )
*)

open Test_utils
open History_mode

let invalid_history_mode_switch ~previous_mode ~target_mode =
  Alcotest.fail
    (Format.asprintf
       "Invalid switch from %a to %a"
       History_mode.pp
       previous_mode
       History_mode.pp
       target_mode)

let expected_savepoint chain_store current_head blocks_per_cycle ~previous_mode
    ~target_mode =
  let open Lwt_result_syntax in
  let* current_head_metadata =
    Store.Block.get_block_metadata chain_store current_head
  in
  let head_lpbl =
    Store.Block.last_preserved_block_level current_head_metadata
  in
  let max_op_ttl = Store.Block.max_operations_ttl current_head_metadata in
  match target_mode with
  | Archive when previous_mode <> Archive ->
      invalid_history_mode_switch ~previous_mode ~target_mode
  | Archive -> return 0l
  | Full offset -> (
      let target_offset =
        (Option.value offset ~default:History_mode.default_additional_cycles)
          .offset
      in
      match previous_mode with
      | Archive ->
          (* We can comply to every mode *)
          (* The preserved_level is the level to be kept to be able to
             export snapshots.*)
          let preserved_level = Int32.(sub head_lpbl (of_int max_op_ttl)) in
          let target_offset_window =
            Int32.(mul blocks_per_cycle (of_int target_offset))
          in
          (* The expected_savepoint is the savepoint we expect given an
             offset. We take the succ to be on a block of the end of a
             cycle. *)
          let expected_savepoint =
            Int32.(succ (sub head_lpbl target_offset_window))
          in
          let preserved_savepoint = min preserved_level expected_savepoint in
          return (max 0l preserved_savepoint)
      | Full offset ->
          let previous_offset =
            (Option.value
               offset
               ~default:History_mode.default_additional_cycles)
              .offset
          in
          (* We are limited by the previous savepoint available *)
          let preserved_level = Int32.(sub head_lpbl (of_int max_op_ttl)) in
          let target_offset_window =
            Int32.(mul blocks_per_cycle (of_int target_offset))
          in
          let previous_offset_window =
            Int32.(mul blocks_per_cycle (of_int previous_offset))
          in
          let expected_savepoint =
            Int32.(succ (sub head_lpbl target_offset_window))
          in
          let preserved_savepoint = min preserved_level expected_savepoint in
          (* The available savepoint is the savepoint available in the
             current store. Which is the one computed with the offset or
             the preserved one. *)
          let available_savepoint =
            Int32.(
              max
                0l
                (min
                   preserved_level
                   (succ (sub head_lpbl previous_offset_window))))
          in
          return (max available_savepoint preserved_savepoint)
      | Rolling _ -> invalid_history_mode_switch ~previous_mode ~target_mode)
  | Rolling offset -> (
      let target_offset =
        (Option.value offset ~default:History_mode.default_additional_cycles)
          .offset
      in
      match previous_mode with
      | Archive ->
          (* We can comply to every mode *)
          let preserved_level = Int32.(sub head_lpbl (of_int max_op_ttl)) in
          let target_offset_window =
            Int32.(mul blocks_per_cycle (of_int target_offset))
          in
          let expected_savepoint =
            Int32.(succ (sub head_lpbl target_offset_window))
          in
          let preserved_savepoint = min preserved_level expected_savepoint in
          return (max 0l preserved_savepoint)
      | Full offset ->
          let previous_offset =
            (Option.value
               offset
               ~default:History_mode.default_additional_cycles)
              .offset
          in
          (* We are limited by the previous savepoint available *)
          let preserved_level = Int32.(sub head_lpbl (of_int max_op_ttl)) in
          let target_offset_window =
            Int32.(mul blocks_per_cycle (of_int target_offset))
          in
          let previous_offset_window =
            Int32.(mul blocks_per_cycle (of_int previous_offset))
          in
          let expected_savepoint =
            Int32.(succ (sub head_lpbl target_offset_window))
          in
          let preserved_savepoint = min preserved_level expected_savepoint in
          let available_savepoint =
            Int32.(
              max
                0l
                (min
                   preserved_level
                   (succ (sub head_lpbl previous_offset_window))))
          in
          return (max available_savepoint preserved_savepoint)
      | Rolling offset ->
          let previous_offset =
            (Option.value
               offset
               ~default:History_mode.default_additional_cycles)
              .offset
          in
          (* We are limited by the previous savepoint available *)
          let preserved_level = Int32.(sub head_lpbl (of_int max_op_ttl)) in
          let target_offset_window =
            Int32.(mul blocks_per_cycle (of_int target_offset))
          in
          let previous_offset_window =
            Int32.(mul blocks_per_cycle (of_int previous_offset))
          in
          let expected_savepoint =
            Int32.(succ (sub head_lpbl target_offset_window))
          in
          let preserved_savepoint = min preserved_level expected_savepoint in
          let available_savepoint =
            Int32.(
              max
                0l
                (min
                   preserved_level
                   (succ (sub head_lpbl previous_offset_window))))
          in
          return (max available_savepoint preserved_savepoint))

let expected_caboose chain_store current_head blocks_per_cycle ~previous_mode
    ~target_mode savepoint =
  let open Lwt_result_syntax in
  match target_mode with
  | Archive -> return 0l
  | Full _ -> return 0l
  | Rolling offset -> (
      let target_offset =
        (Option.value offset ~default:History_mode.default_additional_cycles)
          .offset
      in
      match previous_mode with
      | Archive -> return savepoint
      | Full _ ->
          (* We can get everything *)
          let* current_head_metadata =
            Store.Block.get_block_metadata chain_store current_head
          in
          let head_lpbl =
            Store.Block.last_preserved_block_level current_head_metadata
          in
          let offset_window =
            Int32.(sub head_lpbl (mul blocks_per_cycle (of_int target_offset)))
          in
          let expected_caboose =
            (* When the offset window exceeds the savepoint, we take the
               succ of the expected caboose to be at the end of a cycle.*)
            if offset_window < savepoint then
              Int32.(succ (min offset_window savepoint))
            else min offset_window savepoint
          in
          return (max 0l expected_caboose)
      | Rolling _ ->
          (* savepoint = caboose *)
          return savepoint)

let check_consistency_after_switch descr chain_store ~previous_mode ~target_mode
    blocks =
  let open Lwt_result_syntax in
  let current_head =
    List.(hd (rev blocks)) |> WithExceptions.Option.get ~loc:__LOC__
  in
  let* Tezos_protocol_alpha.Protocol.Alpha_context.Constants.
         {parametric = {blocks_per_cycle; _}; _} =
    Alpha_utils.get_constants chain_store current_head
  in
  let stored_history_mode = Store.Chain.history_mode chain_store in
  Assert.equal
    ~eq:History_mode.equal
    ~pp:History_mode.pp
    ~msg:("expected history mode: " ^ descr)
    stored_history_mode
    target_mode ;
  let*! _, savepoint_level = Store.Chain.savepoint chain_store in
  let*! _, caboose_level = Store.Chain.caboose chain_store in
  let* () =
    match (previous_mode, target_mode) with
    | Archive, Archive
    | Archive, Rolling _
    | Archive, Full _
    | Full _, Full _
    | Full _, Rolling _
    | Rolling _, Rolling _ ->
        let* expected_savepoint_level =
          expected_savepoint
            chain_store
            current_head
            blocks_per_cycle
            ~previous_mode
            ~target_mode
        in
        Assert.Int32.equal
          ~msg:"savepoint consistency: "
          expected_savepoint_level
          savepoint_level ;
        let* expected_caboose_level =
          expected_caboose
            chain_store
            current_head
            blocks_per_cycle
            ~previous_mode
            ~target_mode
            expected_savepoint_level
        in
        Assert.Int32.equal
          ~msg:"caboose consistency: "
          expected_caboose_level
          caboose_level ;
        return_unit
    | _ -> Alcotest.fail "Should not happen in test"
  in
  match (previous_mode, target_mode) with
  | Archive, Full _ | Full _, Full _ ->
      let below_savepoint, above_savepoint =
        List.split_n (Int32.to_int savepoint_level) blocks
      in
      let* () =
        assert_presence_in_store
          ~with_metadata:false
          chain_store
          below_savepoint
      in
      let* () =
        assert_presence_in_store
          ~with_metadata:false
          chain_store
          above_savepoint
      in
      return_unit
  | Archive, Rolling _ | Full _, Rolling _ | Rolling _, Rolling _ ->
      let below_caboose, above_caboose =
        List.split_n Int32.(to_int (pred caboose_level)) blocks
      in
      let below_savepoint, above_savepoint =
        List.split_n (Int32.to_int savepoint_level) above_caboose
      in
      let* () = assert_absence_in_store chain_store below_caboose in
      let* () =
        assert_presence_in_store
          ~with_metadata:false
          chain_store
          below_savepoint
      in
      let* () =
        assert_presence_in_store
          ~with_metadata:false
          chain_store
          above_savepoint
      in
      return_unit
  | p, n when History_mode.equal p n -> return_unit
  | _ -> assert false

let test ~test_descr ~from_hm ~to_hm ~nb_blocks_to_bake (store_dir, context_dir)
    store ~patch_context =
  let open Lwt_result_syntax in
  let chain_store = Store.main_chain_store store in
  let*! genesis_block = Store.Chain.genesis_block chain_store in
  let* previously_baked_blocks, _current_head =
    Alpha_utils.bake_n chain_store nb_blocks_to_bake genesis_block
  in
  let*! () =
    Block_store.await_merging (Store.Unsafe.get_block_store chain_store)
  in
  let* expected_to_fail =
    Error_monad.protect
      (fun () ->
        let genesis = Store.Chain.genesis chain_store in
        let*! () = Store.close_store store in
        let* () =
          Store.may_switch_history_mode
            ~store_dir
            ~context_dir
            genesis
            ~new_history_mode:to_hm
        in
        return_false)
      ~on_error:(function
        | [Store_errors.Cannot_switch_history_mode _] ->
            return
              (match (from_hm, to_hm) with
              | _, Archive -> true
              | Rolling _, Full _ -> true
              | _ -> false)
        | err ->
            Format.printf
              "@\nTest failed:@\n%a@."
              Error_monad.pp_print_trace
              err ;
            Alcotest.fail "Should not fail")
  in
  if expected_to_fail then return_unit
  else
    (* TODO: avoid reloading the store *)
    let* store' =
      Store.init
        ~history_mode:to_hm
        ~patch_context
        ~store_dir
        ~context_dir
        ~allow_testchains:false
        genesis
    in
    Lwt.finalize
      (fun () ->
        let chain_store' = Store.main_chain_store store' in
        check_consistency_after_switch
          test_descr
          chain_store'
          ~previous_mode:from_hm
          ~target_mode:to_hm
          previously_baked_blocks)
      (fun () -> Store.close_store store')

let make_tests speed parameters =
  let patch_context ctxt =
    Alpha_utils.patch_context
      ctxt
      ~json:(Default_parameters.json_of_parameters parameters)
  in
  let history_modes =
    List.sort_uniq
      compare
      (match speed with
      | `Slow ->
          [
            Archive;
            Full (Some {offset = 0});
            Full (Some {offset = 3});
            default_full;
            Full (Some {offset = 7});
            Full (Some {offset = 10});
            Rolling (Some {offset = 0});
            Rolling (Some {offset = 3});
            default_rolling;
            Rolling (Some {offset = 7});
            Rolling (Some {offset = 10});
            default;
          ]
      | `Quick ->
          [
            Archive;
            Full (Some {offset = 0});
            default_full;
            Rolling (Some {offset = 0});
            default_rolling;
            Full (Some {offset = 5});
            Rolling (Some {offset = 5});
          ])
  in
  let blocks_per_cycle = parameters.constants.blocks_per_cycle in
  let nb_blocks_to_bake = [9 * Int32.to_int blocks_per_cycle] in
  let permutations =
    List.(product history_modes (product history_modes nb_blocks_to_bake))
    |> List.map (fun (a, (b, c)) -> (a, b, c))
  in
  List.filter_map
    (fun (from_hm, to_hm, nb_blocks_to_bake) ->
      let test_descr =
        Format.asprintf
          "switching from %a to %a with %d blocks baked and cycle length %ld"
          History_mode.pp
          from_hm
          History_mode.pp
          to_hm
          nb_blocks_to_bake
          blocks_per_cycle
      in
      Some
        (wrap_simple_store_init_test
           ~speed
           ~with_gc:true
           ~keep_dir:false
           ~history_mode:from_hm
           ~patch_context
           ( test_descr,
             fun store_path store ->
               test
                 ~test_descr
                 ~from_hm
                 ~to_hm
                 ~nb_blocks_to_bake
                 ~patch_context
                 store_path
                 store )))
    permutations

let make_test_drag speed parameters =
  let patch_context ctxt =
    Alpha_utils.patch_context
      ctxt
      ~json:(Default_parameters.json_of_parameters parameters)
  in
  let blocks_per_cycle = parameters.constants.blocks_per_cycle in
  let nb_blocks_to_bake = 9 * Int32.to_int blocks_per_cycle in
  let test_descr = "Check savepoint drag with cycles bigger that max_op_ttl" in
  let from_hm = default_full in
  wrap_simple_store_init_test
    ~speed
    ~with_gc:true
    ~keep_dir:false
    ~history_mode:from_hm
    ~patch_context
    ( test_descr,
      fun store_path store ->
        test
          ~test_descr
          ~from_hm
          ~to_hm:default_rolling
          ~nb_blocks_to_bake
          ~patch_context
          store_path
          store )

let tests speed =
  let default_parameters = Alpha_utils.default_genesis_parameters in
  let test_cases = make_tests speed default_parameters in
  let big_cycles_parameters =
    Alpha_utils.
      {
        default_genesis_parameters with
        constants =
          {default_genesis_parameters.constants with blocks_per_cycle = 256l};
      }
  in
  let test_cases_big_cycles = [make_test_drag speed big_cycles_parameters] in
  ("history_mode_switch", test_cases_big_cycles @ test_cases)
