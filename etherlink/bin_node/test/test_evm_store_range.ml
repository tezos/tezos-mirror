(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Bin_evm_node
    Invocation:   dune exec etherlink/bin_node/test/test_evm_store_range.exe
    Subject:      Check that [Evm_store.Blueprints.find_range_with_events]
                  returns the same blueprints and events as
                  [Evm_store.Blueprints.find_with_events] called on each
                  level.
*)

open Evm_node_lib_dev
open Evm_node_lib_dev_encoding

let register ?(tags = []) =
  Test.register
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
    ~__FILE__
    ~tags:("evm_store" :: "blueprints" :: "range" :: tags)

let expect_ok = function
  | Ok x -> x
  | Error err -> Test.fail "%a" pp_print_trace err

let qty n = Ethereum_types.Qty (Z.of_int n)

let timestamp n = Time.Protocol.of_seconds (Int64.of_int n)

let hash_of_char c = Ethereum_types.Hash (Hex (String.make 64 c))

let blueprint n =
  Blueprint_types.
    {
      number = qty n;
      timestamp = timestamp (1000 + n);
      payload = [`External (Format.sprintf "blueprint %d" n)];
    }

let kernel_upgrade c =
  Evm_events.Upgrade.{hash = hash_of_char c; timestamp = timestamp 42}

let sequencer_upgrade =
  Evm_events.Sequencer_upgrade.
    {
      sequencer =
        Signature.Public_key.of_b58check_exn
          "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav";
      pool_address = Ethereum_types.Address (Hex (String.make 40 'a'));
      timestamp = timestamp 43;
    }

let delayed_transaction c =
  Evm_events.Delayed_transaction.
    {kind = Deposit; hash = hash_of_char c; raw = String.make 8 c}

(* Levels 1 to 4 and 6 have a blueprint, level 5 is missing. Events are
   attached to existing levels, to the missing level, to a level after the
   gap and to a level before the first blueprint. *)
let populate conn =
  let open Lwt_result_syntax in
  let* () =
    List.iter_es
      (fun n -> Evm_store.Blueprints.store conn (blueprint n))
      [1; 2; 3; 4; 6]
  in
  let* () = Evm_store.Kernel_upgrades.store conn (qty 3) (kernel_upgrade '1') in
  let* () = Evm_store.Sequencer_upgrades.store conn (qty 2) sequencer_upgrade in
  List.iter_es
    (fun (level, c) ->
      Evm_store.Delayed_transactions.store
        conn
        (qty level)
        (delayed_transaction c))
    [(0, '0'); (2, 'b'); (2, 'a'); (4, 'c'); (5, 'd'); (6, 'e')]

let encode blueprints =
  List.map
    (fun b ->
      Hex.show
        (Hex.of_string
           (Data_encoding.Binary.to_string_exn
              Blueprint_types.with_events_encoding
              b)))
    blueprints

(* Reference implementation: [find_with_events] level by level, stopping
   at the first missing level. *)
let find_one_by_one conn ~from ~to_ =
  let open Lwt_result_syntax in
  let rec go rev_acc level =
    if level > to_ then return (List.rev rev_acc)
    else
      let* blueprint = Evm_store.Blueprints.find_with_events conn (qty level) in
      match blueprint with
      | Some blueprint -> go (blueprint :: rev_acc) (level + 1)
      | None -> return (List.rev rev_acc)
  in
  go [] from

let check_range conn ~from ~to_ ~expected_levels =
  let open Lwt_result_syntax in
  let* reference = find_one_by_one conn ~from ~to_ in
  let* range =
    Evm_store.Blueprints.find_range_with_events
      conn
      ~from:(qty from)
      ~to_:(qty to_)
  in
  let levels =
    List.map
      (fun Blueprint_types.{blueprint = {number = Qty n; _}; _} -> Z.to_int n)
      range
  in
  Check.(
    (levels = expected_levels)
      (list int)
      ~error_msg:
        (Format.sprintf "Range [%d, %d]: expected levels %%R, got %%L" from to_)) ;
  Check.(
    (encode range = encode reference)
      (list string)
      ~error_msg:
        (Format.sprintf
           "Range [%d, %d] differs from find_with_events: expected %%R, got %%L"
           from
           to_)) ;
  return range

let test_range_matches_find_with_events () =
  register ~title:"Blueprints range matches find_with_events level by level"
  @@ fun () ->
  let data_dir = Temp.dir "evm_store_range" in
  let* store =
    Lwt.map expect_ok
    @@ Evm_store.init ~chain_family:L2_types.EVM ~data_dir ~perm:Read_write ()
  in
  let* result =
    Evm_store.use store @@ fun conn ->
    let open Lwt_result_syntax in
    let* () = populate conn in
    (* Stops at the gap at level 5. *)
    let* full =
      check_range conn ~from:1 ~to_:10 ~expected_levels:[1; 2; 3; 4]
    in
    (* Sub-range: events outside of it are ignored. *)
    let* _ = check_range conn ~from:2 ~to_:3 ~expected_levels:[2; 3] in
    (* Missing first level. *)
    let* _ = check_range conn ~from:5 ~to_:6 ~expected_levels:[] in
    (* Empty range. *)
    let* _ = check_range conn ~from:3 ~to_:2 ~expected_levels:[] in
    return full
  in
  let full = expect_ok result in
  (* Check the events explicitly, so that the test does not only rely on the
     reference implementation. *)
  let delayed_hashes =
    List.map
      (fun Blueprint_types.{delayed_transactions; _} ->
        List.map
          (fun Evm_events.Delayed_transaction.{hash = Hash (Hex h); _} ->
            String.sub h 0 1)
          delayed_transactions)
      full
  in
  Check.(
    (delayed_hashes = [[]; ["b"; "a"]; []; ["c"]])
      (list (list string))
      ~error_msg:"Expected delayed transactions %R, got %L") ;
  let upgrades =
    List.map
      (fun Blueprint_types.{kernel_upgrade; sequencer_upgrade; _} ->
        (Option.is_some kernel_upgrade, Option.is_some sequencer_upgrade))
      full
  in
  Check.(
    (upgrades = [(false, false); (false, true); (true, false); (false, false)])
      (list (tuple2 bool bool))
      ~error_msg:"Expected (kernel, sequencer) upgrades %R, got %L") ;
  unit

let () =
  test_range_matches_find_with_events () ;
  Test.run ()
