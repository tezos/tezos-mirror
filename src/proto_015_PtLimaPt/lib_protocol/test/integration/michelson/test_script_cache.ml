(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:  Protocol (cache)
    Invocation: cd src/proto_alpha/lib_protocol/test/integration/michelson && \
                dune exec ./main.exe -- test "^script cache$"
    Subject:    These unit tests check basic behavior of script cache
*)

open Protocol
open Alpha_context
open Contract_helpers
open Script_ir_translator

exception Script_cache_test_error of string

let err x = Exn (Script_cache_test_error x)

(*

   The following value is hard-coded to detect change in the size
   model. It has been computed by a manual run of the test.

*)
let liquidity_baking_contract_size = 182272

let liquidity_baking_contract =
  Contract_hash.of_b58check_exn "KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5"

let make_block block f =
  Incremental.begin_construction block >>=? fun incr ->
  f (Incremental.alpha_ctxt incr) >>=? fun (ret, ctxt) ->
  let incr = Incremental.set_alpha_ctxt incr ctxt in
  Incremental.finalize_block incr >>=? fun block ->
  Block.bake block >>=? fun next_block -> return (ret, next_block)

let ( @! ) f g =
  f @@ fun ctxt ->
  g ctxt >>=? fun ret -> return (ret, ctxt)

let throw_block_away (x, _block) = return x

let equal_scripts (s1 : Script.t) (s2 : Script.t) =
  Script_repr.(
    force_bytes s1.code >>? fun code1 ->
    force_bytes s2.code >>? fun code2 ->
    force_bytes s1.storage >>? fun storage1 ->
    force_bytes s2.storage >>? fun storage2 ->
    ok (Bytes.equal code1 code2 && Bytes.equal storage1 storage2))
  |> Environment.wrap_tzresult

let find ctxt addr =
  Script_cache.find ctxt addr >|= Environment.wrap_tzresult
  >>=? fun (ctxt, identifier, result) ->
  match result with
  | None ->
      (* by [find_correctly_looks_up]. *)
      assert false
  | Some (script, Ex_script ir) ->
      return (ctxt, identifier, script, Ex_script ir)

let value_as_int :
    type a ac. (a, ac) Script_typed_ir.ty -> a -> Script_int.z Script_int.num =
 fun ty v -> match ty with Int_t -> v | _ -> Stdlib.failwith "value_as_int"

let path = project_root // Filename.dirname __FILE__

let add_some_contracts k src block baker =
  ( make_block block @@ fun ctxt ->
    find ctxt liquidity_baking_contract >>=? fun (ctxt, id, _, _) ->
    return (id, ctxt) )
  >>=? fun (liquidity_baking_contract_id, block) ->
  List.fold_left_es
    (fun (rev_contracts, block) _ ->
      let script = path // "contracts/int-store.tz" in
      originate_contract_hash script "31" src block baker
      >>=? fun (addr, block) ->
      Block.bake block >>=? fun block ->
      make_block block @@ fun ctxt ->
      find ctxt addr >>=? fun (ctxt, id, _, _) ->
      let contract = (id, addr) in
      return (contract :: rev_contracts, ctxt))
    ([], block)
    (1 -- k)
  >>=? fun (rev_contracts, block) ->
  let contracts =
    (* After each baking [liquidity_baking_contract] is the most
       recently used contract. *)
    let lb = (liquidity_baking_contract_id, liquidity_baking_contract) in
    List.rev (lb :: rev_contracts)
  in
  return (contracts, block)

(*

   The following value is hard-coded to detect change in the size
   model. It has been computed by a manual run of the test.

*)
let int_store_contract_size = 592

(*

   Check our assumptions regarding the size of reference scripts.

*)
let assert_cache_size expected_size ctxt =
  fail_unless
    (Script_cache.size ctxt = expected_size)
    (err
       (Printf.sprintf
          "Invalid script cache size, expecting %d, got %d"
          expected_size
          (Script_cache.size ctxt)))

let test_size_of_liquidity_baking_contract () =
  init () >>=? fun (block, _, _, _) ->
  make_block block @! assert_cache_size liquidity_baking_contract_size
  >>=? throw_block_away

let test_size_of_int_store_contract () =
  init () >>=? fun (block, baker, src, _) ->
  let script = path // "contracts/int-store.tz" in
  originate_contract_hash script "31" src block baker >>=? fun (addr, block) ->
  ( make_block block @! fun ctxt ->
    Script_cache.find ctxt addr >|= Environment.wrap_tzresult
    >>=? fun (ctxt, _, _) ->
    assert_cache_size
      (int_store_contract_size + liquidity_baking_contract_size)
      ctxt )
  >>=? throw_block_away

(*

   When a contract is in the context, [find] correctly returns it.

*)
let test_find_correctly_looks_up () =
  init () >>=? fun (block, baker, src, _) ->
  let script = path // "contracts/sapling_contract.tz" in
  originate_contract_hash script "{ }" src block baker >>=? fun (addr, block) ->
  ( make_block block @! fun ctxt ->
    (*
      Contract is present.
    *)
    Script_cache.find ctxt addr >|= Environment.wrap_tzresult
    >>=? fun (_, _, result) ->
    Contract.get_script ctxt addr >|= Environment.wrap_tzresult
    >>=? fun (ctxt, script) ->
    (match (result, script) with
    | None, _ -> ok false
    | Some _, None ->
        (* because we assume that get_script correctly behaves. *)
        assert false
    | Some (cached_script, _), Some script -> equal_scripts script cached_script)
    >>?= fun cond ->
    fail_unless
      cond
      (err "find should be able to retrieve an originated contract")
    (*
      Contract is absent.
    *)
    >>=? fun () ->
    let addr = Contract_helpers.fake_KT1 in
    Script_cache.find ctxt addr >|= Environment.wrap_tzresult
    >>=? fun (_, _, cached_contract) ->
    fail_unless
      (cached_contract = None)
      (err "find should return None for unbound contracts") )
  >>=? throw_block_away

(*

   [test_update] correctly modifies a cached contract in the context.

*)
let test_update_modifies_cached_contract () =
  init () >>=? fun (block, baker, src, _) ->
  let script = path // "contracts/int-store.tz" in
  originate_contract_hash script "36" src block baker >>=? fun (addr, block) ->
  ( make_block block @! fun ctxt ->
    find ctxt addr >>=? fun (ctxt, identifier, script, Ex_script (Script ir)) ->
    match ir.storage_type with
    | Int_t ->
        let storage' = Script_int.(add ir.storage (Script_int.of_int 1)) in
        let cached_contract' =
          (script, Ex_script (Script {ir with storage = storage'}))
        in
        Script_cache.update ctxt identifier cached_contract' 1
        |> Environment.wrap_tzresult
        >>?= fun ctxt ->
        find ctxt addr >>=? fun (_, _, _, Ex_script (Script ir')) ->
        let storage = value_as_int ir'.storage_type ir'.storage in
        fail_unless
          (Script_int.compare storage storage' = 0)
          (err
             (Format.sprintf
                "Update failed, expecting %s, got %s"
                (Script_int.to_string storage')
                (Script_int.to_string storage)))
    | _ ->
        (* by definition of int-store.tz. *)
        assert false )
  >>=? throw_block_away

(*

   [test_entries] returns the list of cached scripts in order of least
   modification date.

*)
let test_entries_returns_the_list_in_correct_order () =
  let ncontracts = 10 in
  init () >>=? fun (block, baker, src, _) ->
  add_some_contracts ncontracts src block baker >>=? fun (contracts, block) ->
  let addrs = snd @@ List.split contracts in
  ( make_block block @! fun ctxt ->
    Script_cache.entries ctxt |> Environment.wrap_tzresult >>?= fun entries ->
    let cached_contracts = fst (List.split entries) in
    fail_unless
      (addrs = cached_contracts)
      (err "entries must return cached contracts in order") )
  >>=? throw_block_away

(*

   [test_contract_rank] correctly computes LRU rank.

*)
let test_contract_rank_is_lru_rank () =
  let ncontracts = 10 in
  init () >>=? fun (block, baker, src, _) ->
  add_some_contracts ncontracts src block baker >>=? fun (contracts, block) ->
  let addrs = snd @@ List.split contracts in
  ( make_block block @! fun ctxt ->
    let rec check_rank k = function
      | [] -> return ()
      | addr :: addrs -> (
          match Script_cache.contract_rank ctxt addr with
          | None -> fail (err "Contract rank should find a cached contract")
          | Some rank ->
              fail_unless
                (k = rank)
                (err
                   (Printf.sprintf
                      "Invalid contract rank, expecting %d, got %d"
                      k
                      rank))
              >>=? fun () -> check_rank (k + 1) addrs)
    in
    check_rank 0 addrs )
  >>=? throw_block_away

(*

   [size] correctly returns the sums of the (declared) sizes.

*)
let test_size_adds_entries_sizes () =
  let ncontracts = 10 in
  init () >>=? fun (block, baker, src, _) ->
  add_some_contracts ncontracts src block baker >>=? fun (_, block) ->
  ( make_block block @! fun ctxt ->
    let expected_size =
      liquidity_baking_contract_size + (ncontracts * int_store_contract_size)
    in
    fail_unless
      (Script_cache.size ctxt = expected_size)
      (err
         (Printf.sprintf
            "Invalid script cache size, expecting %d, got %d"
            expected_size
            (Script_cache.size ctxt))) )
  >>=? throw_block_away

(*

   [test_size_limit] is the value found in [Constants_repr.cache_layout].

*)
let defined_size_limit =
  Tezos_protocol_015_PtLimaPt_parameters.Default_parameters.constants_mainnet
    .cache_script_size

let test_size_limit_is_in_constants_repr () =
  init () >>=? fun (block, _baker, _src, _) ->
  ( make_block block @! fun ctxt ->
    fail_unless
      (Script_cache.size_limit ctxt = defined_size_limit)
      (err
         (Printf.sprintf
            "Invalid size limit, expecting %d, got %d"
            defined_size_limit
            (Script_cache.size_limit ctxt))) )
  >>=? throw_block_away

(*

   [test_entries] are conform to an LRU strategy: when the cache is
   full the least recently used entries are removed.

*)
let test_entries_shows_lru () =
  let ncontracts = 10 in
  init () >>=? fun (block, baker, src, _) ->
  add_some_contracts ncontracts src block baker >>=? fun (contracts, block) ->
  (* We pretend that the contracts' sizes grow so much that they cannot all
     fit into the cache. *)
  let new_size = 2 * defined_size_limit / ncontracts in
  ( make_block block @@ fun ctxt ->
    List.fold_left_es
      (fun ctxt (_, addr) ->
        find ctxt addr >>=? fun (ctxt, id, script, cached_contract) ->
        Lwt.return
        @@ (Script_cache.update ctxt id (script, cached_contract) new_size
           |> Environment.wrap_tzresult))
      ctxt
      contracts
    >>=? fun ctxt -> return ((), ctxt) )
  >>=? fun ((), block) ->
  (* At this point, the cache should only contain the most recently modified
     contracts. *)
  ( make_block block @@ fun ctxt ->
    Script_cache.entries ctxt |> Environment.wrap_tzresult >>?= fun entries ->
    let rev_entries = List.rev entries in
    let rev_contracts = List.rev contracts in
    let rec aux rev_entries rev_contracts =
      Printf.eprintf
        "%d %d\n"
        (List.length rev_entries)
        (List.length rev_contracts) ;
      match (rev_entries, rev_contracts) with
      | [], _ ->
          (* We do not count liquidity baking contract. *)
          let removed_contracts = List.length rev_contracts - 1 in
          fail_unless
            (removed_contracts = ncontracts / 2)
            (err
               (Printf.sprintf
                  "Too few contracts have been removed from the cache while it \
                   is full, %d remaining while expecting %d"
                  removed_contracts
                  (ncontracts / 2)))
      | (contract, size) :: rev_entries, (_, contract') :: rev_contracts ->
          fail_unless
            (size = new_size || contract = liquidity_baking_contract)
            (err
               (Printf.sprintf
                  "A contract in the cache has not the right size, expecting \
                   %d, got %d"
                  new_size
                  size))
          >>=? fun () ->
          fail_unless
            (contract = contract')
            (err
               (Printf.sprintf
                  "entries do not return cached contracts in right order"))
          >>=? fun () -> aux rev_entries rev_contracts
      | _, [] ->
          (* There cannot be more entries than contracts. *)
          assert false
    in
    aux rev_entries rev_contracts >>=? fun () -> return ((), ctxt) )
  >>=? throw_block_away

let tests =
  let open Tztest in
  [
    tztest
      "assumption about size of liquidity baking holds"
      `Quick
      test_size_of_liquidity_baking_contract;
    tztest
      "assumption about size of 'int_store' contract holds"
      `Quick
      test_size_of_int_store_contract;
    tztest "find correctly looks up" `Quick test_find_correctly_looks_up;
    tztest
      "update correctly modifies"
      `Quick
      test_update_modifies_cached_contract;
    tztest
      "entries correctly list contracts in order"
      `Quick
      test_entries_returns_the_list_in_correct_order;
    tztest "contract_rank is LRU rank" `Quick test_contract_rank_is_lru_rank;
    tztest "size returns entries size" `Quick test_size_adds_entries_sizes;
    tztest
      "size limit is a protocol constant"
      `Quick
      test_size_limit_is_in_constants_repr;
    tztest "entries show LRU behavior" `Quick test_entries_shows_lru;
  ]
