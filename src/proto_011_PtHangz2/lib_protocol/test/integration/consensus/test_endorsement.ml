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
    Component:  Protocol (endorsement)
    Invocation: dune exec src/proto_011_PtHangz2/lib_protocol/test/integration/consensus/main.exe -- test "^endorsement$"
    Subject:    Endorsing a block adds an extra layer of confidence to the
                Tezos' PoS algorithm. The block endorsing operation must be
                included in the following block. Each endorser possesses a
                number of slots corresponding to their priority. After
                [preserved_cycles], a reward is given to the endorser. This
                reward depends on the priority of the block that contains
                the endorsements.
*)

open Protocol
open Alpha_context
open Test_tez

(****************************************************************)
(*                    Utility functions                         *)
(****************************************************************)

let get_hd_hd = function x :: y :: _ -> (x, y) | _ -> assert false

let get_expected_reward ctxt ~priority ~baker ~endorsing_power =
  (if baker then Context.get_baking_reward ctxt ~priority ~endorsing_power
  else return (Test_tez.Tez.of_int 0))
  >>=? fun baking_reward ->
  Context.get_endorsing_reward ctxt ~priority ~endorsing_power
  >>=? fun endorsing_reward ->
  Test_tez.Tez.(endorsing_reward +? baking_reward) >>?= fun reward ->
  return reward

let get_expected_deposit ctxt ~baker ~endorsing_power =
  Context.get_constants ctxt
  >>=? fun Constants.
             {
               parametric =
                 {endorsement_security_deposit; block_security_deposit; _};
               _;
             } ->
  let open Environment in
  let open Tez in
  let baking_deposit = if baker then block_security_deposit else of_int 0 in
  endorsement_security_deposit *? Int64.of_int endorsing_power
  >>?= fun endorsement_deposit ->
  endorsement_deposit +? baking_deposit >>?= fun deposit -> return deposit

(* [baker] is true if the [pkh] has also baked the current block, in
   which case corresponding deposit and reward should be adjusted *)
let assert_endorser_balance_consistency ~loc ?(priority = 0) ?(baker = false)
    ~endorsing_power ctxt pkh initial_balance =
  let contract = Contract.implicit_contract pkh in
  get_expected_reward ctxt ~priority ~baker ~endorsing_power >>=? fun reward ->
  get_expected_deposit ctxt ~baker ~endorsing_power >>=? fun deposit ->
  Assert.balance_was_debited ~loc ctxt contract initial_balance deposit
  >>=? fun () ->
  Context.Contract.balance ~kind:Rewards ctxt contract
  >>=? fun reward_balance ->
  Assert.equal_tez ~loc reward_balance reward >>=? fun () ->
  Context.Contract.balance ~kind:Deposit ctxt contract
  >>=? fun deposit_balance -> Assert.equal_tez ~loc deposit_balance deposit

let delegates_with_slots endorsers =
  List.map
    (fun (endorser : Plugin.RPC.Endorsing_rights.t) ->
      (endorser.delegate, endorser.slots))
    endorsers

let endorsing_power endorsers =
  List.fold_left
    (fun sum (endorser : Plugin.RPC.Endorsing_rights.t) ->
      sum + List.length endorser.slots)
    0
    endorsers

(****************************************************************)
(*                      Tests                                   *)
(****************************************************************)

(** Apply a single endorsement from the slot 0 endorser. *)
let test_simple_endorsement () =
  Context.init 5 >>=? fun (b, _) ->
  Context.get_endorser (B b) >>=? fun (delegate, slots) ->
  Op.endorsement_with_slot ~delegate:(delegate, slots) (B b) () >>=? fun op ->
  Context.Contract.balance (B b) (Contract.implicit_contract delegate)
  >>=? fun initial_balance ->
  let policy = Block.Excluding [delegate] in
  Block.get_next_baker ~policy b >>=? fun (_, priority, _) ->
  Block.bake ~policy ~operations:[Operation.pack op] b >>=? fun b2 ->
  assert_endorser_balance_consistency
    ~loc:__LOC__
    (B b2)
    ~priority
    ~endorsing_power:(List.length slots)
    delegate
    initial_balance

(** Apply a maximum number of endorsements. An endorser can be
    selected twice. *)
let test_max_endorsement () =
  let endorsers_per_block = 16 in
  Context.init ~endorsers_per_block 32 >>=? fun (b, _) ->
  Context.get_endorsers (B b) >>=? fun endorsers ->
  Assert.equal_bool
    ~loc:__LOC__
    Compare.List_length_with.(
      List.concat_map
        (fun {Plugin.RPC.Endorsing_rights.slots; _} -> slots)
        endorsers
      = endorsers_per_block)
    true
  >>=? fun () ->
  List.fold_left_es
    (fun (delegates, ops, balances) (endorser : Plugin.RPC.Endorsing_rights.t) ->
      let delegate = endorser.delegate in
      Context.Contract.balance (B b) (Contract.implicit_contract delegate)
      >>=? fun balance ->
      Op.endorsement_with_slot ~delegate:(delegate, endorser.slots) (B b) ()
      >|=? fun op ->
      ( delegate :: delegates,
        Operation.pack op :: ops,
        (List.length endorser.slots, balance) :: balances ))
    ([], [], [])
    endorsers
  >>=? fun (delegates, ops, previous_balances) ->
  Block.bake ~policy:(Excluding delegates) ~operations:(List.rev ops) b
  >>=? fun b ->
  (* One account can endorse more than one time per level, we must
     check that the bonds are summed up *)
  List.iter2_es
    ~when_different_lengths:(TzTrace.make (Exn (Failure __LOC__)))
    (fun endorser_account (endorsing_power, previous_balance) ->
      assert_endorser_balance_consistency
        ~loc:__LOC__
        (B b)
        ~endorsing_power
        endorser_account
        previous_balance)
    delegates
    previous_balances

(** Check that every endorsers' balances are consistent with different
    priorities. *)
let test_consistent_priorities () =
  let priorities = 0 -- 64 in
  Context.init 64 >>=? fun (b, _) ->
  List.fold_left_es
    (fun (b, used_pkhes) priority ->
      (* Choose an endorser that has not baked nor endorsed before *)
      Context.get_endorsers (B b) >>=? fun endorsers ->
      let endorser =
        List.find_opt
          (fun (e : Plugin.RPC.Endorsing_rights.t) ->
            not (Signature.Public_key_hash.Set.mem e.delegate used_pkhes))
          endorsers
      in
      match endorser with
      | None ->
          return (b, used_pkhes) (* not enough fresh endorsers; we "stop" *)
      | Some endorser ->
          Context.Contract.balance
            (B b)
            (Contract.implicit_contract endorser.delegate)
          >>=? fun balance ->
          Op.endorsement_with_slot
            ~delegate:(endorser.delegate, endorser.slots)
            (B b)
            ()
          >>=? fun operation ->
          let operation = Operation.pack operation in
          Block.get_next_baker ~policy:(By_priority priority) b
          >>=? fun (baker, _, _) ->
          let used_pkhes = Signature.Public_key_hash.Set.add baker used_pkhes in
          let used_pkhes =
            Signature.Public_key_hash.Set.add endorser.delegate used_pkhes
          in
          (* Bake with a specific priority *)
          Block.bake ~policy:(By_priority priority) ~operation b >>=? fun b ->
          let is_baker =
            Signature.Public_key_hash.(baker = endorser.delegate)
          in
          assert_endorser_balance_consistency
            ~loc:__LOC__
            ~priority
            ~baker:is_baker
            (B b)
            ~endorsing_power:(List.length endorser.slots)
            endorser.delegate
            balance
          >|=? fun () -> (b, used_pkhes))
    (b, Signature.Public_key_hash.Set.empty)
    priorities
  >>=? fun _b -> return_unit

(** Check that after [preserved_cycles] number of cycles the endorser
    gets his reward. *)
let test_reward_retrieval () =
  Context.init 5 >>=? fun (b, _) ->
  Context.get_constants (B b)
  >>=? fun Constants.{parametric = {preserved_cycles; _}; _} ->
  Context.get_endorser (B b) >>=? fun (endorser, slots) ->
  Context.Contract.balance (B b) (Contract.implicit_contract endorser)
  >>=? fun balance ->
  Op.endorsement_with_slot ~delegate:(endorser, slots) (B b) ()
  >>=? fun operation ->
  let operation = Operation.pack operation in
  let policy = Block.Excluding [endorser] in
  Block.get_next_baker ~policy b >>=? fun (_, priority, _) ->
  Block.bake ~policy ~operation b >>=? fun b ->
  (* Bake (preserved_cycles + 1) cycles *)
  List.fold_left_es
    (fun b _ -> Block.bake_until_cycle_end ~policy:(Excluding [endorser]) b)
    b
    (0 -- preserved_cycles)
  >>=? fun b ->
  get_expected_reward
    (B b)
    ~priority
    ~baker:false
    ~endorsing_power:(List.length slots)
  >>=? fun reward ->
  Assert.balance_was_credited
    ~loc:__LOC__
    (B b)
    (Contract.implicit_contract endorser)
    balance
    reward

(** Check that after [preserved_cycles] number of cycles endorsers get
    their reward. Two endorsers are used and they endorse in different
    cycles. *)
let test_reward_retrieval_two_endorsers () =
  Context.init 5 >>=? fun (b, _) ->
  Context.get_constants (B b)
  >>=? fun Constants.
             {
               parametric = {preserved_cycles; endorsement_security_deposit; _};
               _;
             } ->
  Context.get_endorsers (B b) >>=? fun endorsers ->
  let (endorser1, endorser2) = get_hd_hd endorsers in
  Context.Contract.balance (B b) (Contract.implicit_contract endorser1.delegate)
  >>=? fun balance1 ->
  Context.Contract.balance (B b) (Contract.implicit_contract endorser2.delegate)
  >>=? fun balance2 ->
  Tez.(
    endorsement_security_deposit *? Int64.of_int (List.length endorser1.slots))
  >>?= fun security_deposit1 ->
  (* endorser1 endorses the genesis block in cycle 0 *)
  Op.endorsement_with_slot
    ~delegate:(endorser1.delegate, endorser1.slots)
    (B b)
    ()
  >>=? fun operation1 ->
  let policy = Block.Excluding [endorser1.delegate; endorser2.delegate] in
  Block.get_next_baker ~policy b >>=? fun (_, priority, _) ->
  Context.get_endorsing_reward
    (B b)
    ~priority
    ~endorsing_power:(List.length endorser1.slots)
  >>=? fun reward1 ->
  (* bake next block, include endorsement of endorser1 *)
  Block.bake ~policy ~operation:(Operation.pack operation1) b >>=? fun b ->
  Assert.balance_was_debited
    ~loc:__LOC__
    (B b)
    (Contract.implicit_contract endorser1.delegate)
    balance1
    security_deposit1
  >>=? fun () ->
  Assert.balance_is
    ~loc:__LOC__
    (B b)
    (Contract.implicit_contract endorser2.delegate)
    balance2
  >>=? fun () ->
  (* complete cycle 0 *)
  Block.bake_until_cycle_end ~policy b >>=? fun b ->
  Assert.balance_was_debited
    ~loc:__LOC__
    (B b)
    (Contract.implicit_contract endorser1.delegate)
    balance1
    security_deposit1
  >>=? fun () ->
  Assert.balance_is
    ~loc:__LOC__
    (B b)
    (Contract.implicit_contract endorser2.delegate)
    balance2
  >>=? fun () ->
  (* get the slots of endorser2 for the current block *)
  Context.get_endorsers (B b) >>=? fun endorsers ->
  let same_endorser2 endorser =
    Signature.Public_key_hash.(
      endorser.Plugin.RPC.Endorsing_rights.delegate = endorser2.delegate)
  in
  let endorser2 =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.find same_endorser2 endorsers
  in
  (* No exception raised: in sandboxed mode endorsers do not change between blocks *)
  Tez.(
    endorsement_security_deposit *? Int64.of_int (List.length endorser2.slots))
  >>?= fun security_deposit2 ->
  (* endorser2 endorses the last block in cycle 0 *)
  Op.endorsement_with_slot
    ~delegate:(endorser2.delegate, endorser2.slots)
    (B b)
    ()
  >>=? fun operation2 ->
  (* bake first block in cycle 1, include endorsement of endorser2 *)
  Block.bake ~policy ~operation:(Operation.pack operation2) b >>=? fun b ->
  let priority = b.header.protocol_data.contents.priority in
  Context.get_endorsing_reward
    (B b)
    ~priority
    ~endorsing_power:(List.length endorser2.slots)
  >>=? fun reward2 ->
  Assert.balance_was_debited
    ~loc:__LOC__
    (B b)
    (Contract.implicit_contract endorser1.delegate)
    balance1
    security_deposit1
  >>=? fun () ->
  Assert.balance_was_debited
    ~loc:__LOC__
    (B b)
    (Contract.implicit_contract endorser2.delegate)
    balance2
    security_deposit2
  >>=? fun () ->
  (* bake [preserved_cycles] number of cycles *)
  List.fold_left_es
    (fun b _ ->
      Assert.balance_was_debited
        ~loc:__LOC__
        (B b)
        (Contract.implicit_contract endorser1.delegate)
        balance1
        security_deposit1
      >>=? fun () ->
      Assert.balance_was_debited
        ~loc:__LOC__
        (B b)
        (Contract.implicit_contract endorser2.delegate)
        balance2
        security_deposit2
      >>=? fun () -> Block.bake_until_cycle_end ~policy b)
    b
    (1 -- preserved_cycles)
  >>=? fun b ->
  Assert.balance_was_credited
    ~loc:__LOC__
    (B b)
    (Contract.implicit_contract endorser1.delegate)
    balance1
    reward1
  >>=? fun () ->
  Assert.balance_was_debited
    ~loc:__LOC__
    (B b)
    (Contract.implicit_contract endorser2.delegate)
    balance2
    security_deposit2
  >>=? fun () ->
  (* bake cycle [preserved_cycle + 1] *)
  Block.bake_until_cycle_end ~policy b >>=? fun b ->
  Assert.balance_was_credited
    ~loc:__LOC__
    (B b)
    (Contract.implicit_contract endorser1.delegate)
    balance1
    reward1
  >>=? fun () ->
  Assert.balance_was_credited
    ~loc:__LOC__
    (B b)
    (Contract.implicit_contract endorser2.delegate)
    balance2
    reward2

(****************************************************************)
(*  The following test scenarios are supposed to raise errors.  *)
(****************************************************************)

(** Apply an endorsement without its slot bearing wrapper. *)
let test_unwrapped_endorsement () =
  Context.init 5 >>=? fun (b, _) ->
  Context.get_endorser (B b) >>=? fun (delegate, _slots) ->
  Op.endorsement ~delegate (B b) () >>=? fun op ->
  let policy = Block.Excluding [delegate] in
  Block.bake ~policy ~operations:[Operation.pack op] b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Apply.Unwrapped_endorsement -> true
      | _ -> false)

(** Apply an endorsement with an invalid slot in its slot bearing wrapper. *)
let test_bad_slot_wrapper () =
  Context.init 5 >>=? fun (b, _) ->
  Context.get_endorser (B b) >>=? fun (delegate, _slots) ->
  Op.endorsement_with_slot ~delegate:(delegate, [2000]) (B b) () >>=? fun op ->
  let policy = Block.Excluding [delegate] in
  Block.bake ~policy ~operations:[Operation.pack op] b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Baking.Invalid_endorsement_slot _ -> true
      | _ -> false)

(** Apply an endorsement with a negative slot in its slot bearing wrapper. *)
let test_neg_slot_wrapper () =
  Context.init 5 >>=? fun (b, _) ->
  Context.get_endorser (B b) >>=? fun (delegate, _slots) ->
  Op.endorsement_with_slot ~delegate:(delegate, [-1]) (B b) () >>=? fun op ->
  let policy = Block.Excluding [delegate] in
  Lwt.catch
    (fun () ->
      Block.bake ~policy ~operations:[Operation.pack op] b >>= fun _ ->
      failwith
        "negative slot wrapper should not be accepted by the binary format")
    (function
      | Data_encoding.Binary.Write_error _ -> return_unit | e -> Lwt.fail e)

(** Apply an endorsement with a non-normalized slot in its slot bearing wrapper. *)
let test_non_normalized_slot_wrapper () =
  Context.init 5 >>=? fun (b, _) ->
  Context.get_endorsers (B b) >>=? fun endorsers ->
  (* find an endorsers with more than 1 slot *)
  let endorser =
    WithExceptions.Option.get ~loc:__LOC__
    @@ List.find
         (fun endorser ->
           Compare.List_length_with.(
             endorser.Plugin.RPC.Endorsing_rights.slots > 1))
         endorsers
  in
  let (delegate, slots) = (endorser.delegate, endorser.slots) in
  (* the first slot should be the smallest slot *)
  Assert.equal_int
    ~loc:__LOC__
    (WithExceptions.Option.get ~loc:__LOC__ @@ List.hd endorser.slots)
    (WithExceptions.Option.get ~loc:__LOC__
    @@ List.hd (List.sort compare endorser.slots))
  >>=? fun () ->
  Op.endorsement_with_slot ~delegate:(delegate, List.rev slots) (B b) ()
  >>=? fun op ->
  let policy = Block.Excluding [delegate] in
  Block.bake ~policy ~operations:[Operation.pack op] b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Baking.Unexpected_endorsement_slot _ -> true
      | _ -> false)

(** Wrong endorsement predecessor : apply an endorsement with an
    incorrect block predecessor. *)
let test_wrong_endorsement_predecessor () =
  Context.init 5 >>=? fun (b, _) ->
  Block.bake b >>=? fun b' ->
  Op.endorsement_with_slot ~signing_context:(B b) (B b') ()
  >>=? fun operation ->
  let operation = Operation.pack operation in
  Block.bake ~operation b' >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Apply.Wrong_endorsement_predecessor _ -> true
      | _ -> false)

(** Invalid_endorsement_level: apply an endorsement with an incorrect
    level (i.e. the predecessor level). *)
let test_invalid_endorsement_level () =
  Context.init 5 >>=? fun (b, _) ->
  Context.get_level (B b) >>?= fun genesis_level ->
  Block.bake b >>=? fun b' ->
  Context.get_endorser (B b) >>=? fun (genesis_endorser, slots) ->
  Op.endorsement_with_slot
    ~delegate:(genesis_endorser, slots)
    ~signing_context:(B b')
    ~level:genesis_level
    (B b')
    ()
  >>=? fun operation ->
  let operation = Operation.pack operation in
  Block.bake ~operation b' >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Apply.Invalid_endorsement_level -> true
      | _ -> false)

(** Duplicate endorsement : apply an endorsement that has already been
    done. *)
let test_duplicate_endorsement () =
  Context.init 5 >>=? fun (b, _) ->
  Incremental.begin_construction b >>=? fun inc ->
  Op.endorsement_with_slot (B b) () >>=? fun operation ->
  let operation = Operation.pack operation in
  Incremental.add_operation inc operation >>=? fun inc ->
  Op.endorsement_with_slot (B b) () >>=? fun operation ->
  let operation = Operation.pack operation in
  Incremental.add_operation inc operation >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Apply.Duplicate_endorsement _ -> true
      | _ -> false)

(** Apply a single endorsement from the slot 0 endorser. *)
let test_not_enough_for_deposit () =
  Context.init 5 ~endorsers_per_block:1 >>=? fun (b_init, contracts) ->
  List.map_es
    (fun c -> Context.Contract.manager (B b_init) c >|=? fun m -> (m, c))
    contracts
  >>=? fun managers ->
  Block.bake b_init >>=? fun b ->
  (* retrieve the level 2's endorser *)
  Context.get_endorser (B b) >>=? fun (endorser, slots) ->
  let (_, contract_other_than_endorser) =
    WithExceptions.Option.get ~loc:__LOC__
    @@ List.find
         (fun (c, _) ->
           not (Signature.Public_key_hash.equal c.Account.pkh endorser))
         managers
  in
  let (_, contract_of_endorser) =
    WithExceptions.Option.get ~loc:__LOC__
    @@ List.find
         (fun (c, _) -> Signature.Public_key_hash.equal c.Account.pkh endorser)
         managers
  in
  Context.Contract.balance (B b) (Contract.implicit_contract endorser)
  >>=? fun initial_balance ->
  (* Empty the future endorser account *)
  Op.transaction
    (B b_init)
    contract_of_endorser
    contract_other_than_endorser
    initial_balance
  >>=? fun op_trans ->
  Block.bake ~operation:op_trans b_init >>=? fun b ->
  (* Endorse with a zero balance *)
  Op.endorsement_with_slot ~delegate:(endorser, slots) (B b) ()
  >>=? fun op_endo ->
  Block.bake
    ~policy:(Excluding [endorser])
    ~operation:(Operation.pack op_endo)
    b
  >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Delegate_storage.Balance_too_low_for_deposit _ -> true
      | _ -> false)

(** Check that a block with not enough endorsement cannot be baked. *)
let test_endorsement_threshold () =
  let initial_endorsers = 28 in
  let num_accounts = 100 in
  Context.init ~initial_endorsers num_accounts >>=? fun (b, _) ->
  Context.get_endorsers (B b) >>=? fun endorsers ->
  let num_endorsers = List.length endorsers in
  (* we try to bake with more and more endorsers, but at each
     iteration with a timestamp smaller than required *)
  List.iter_es
    (fun i ->
      (* the priority is chosen rather arbitrarily *)
      let priority = num_endorsers - i in
      let crt_endorsers = List.take_n i endorsers in
      let endorsing_power = endorsing_power crt_endorsers in
      let delegates = delegates_with_slots crt_endorsers in
      List.map_es
        (fun x -> Op.endorsement_with_slot ~delegate:x (B b) ())
        delegates
      >>=? fun ops ->
      Context.get_minimal_valid_time (B b) ~priority ~endorsing_power
      >>=? fun timestamp ->
      (* decrease the timestamp by one second *)
      let seconds =
        Int64.(sub (of_string (Timestamp.to_seconds_string timestamp)) 1L)
      in
      match Timestamp.of_seconds_string (Int64.to_string seconds) with
      | None -> failwith "timestamp to/from string manipulation failed"
      | Some timestamp ->
          Block.bake
            ~timestamp
            ~policy:(By_priority priority)
            ~operations:(List.map Operation.pack ops)
            b
          >>= fun b2 ->
          Assert.proto_error ~loc:__LOC__ b2 (function
              | Baking.Timestamp_too_early _ -> true
              | _ -> false))
    (0 -- (num_endorsers - 1))
  >>=? fun () ->
  (* we bake with all endorsers endorsing, at the right time *)
  let priority = 0 in
  let endorsing_power = endorsing_power endorsers in
  let delegates = delegates_with_slots endorsers in
  List.map_es
    (fun delegate -> Op.endorsement_with_slot ~delegate (B b) ())
    delegates
  >>=? fun ops ->
  Context.get_minimal_valid_time (B b) ~priority ~endorsing_power
  >>=? fun timestamp ->
  Block.bake
    ~policy:(By_priority priority)
    ~timestamp
    ~operations:(List.map Operation.pack ops)
    b
  >>= fun _ -> return_unit

(** Fitness gap *)
let test_fitness_gap () =
  let num_accounts = 5 in
  Context.init num_accounts >>=? fun (b, _) ->
  (match Fitness_repr.to_int64 b.header.shell.fitness with
  | Ok fitness -> Int64.to_int fitness
  | Error _ -> assert false)
  |> fun fitness ->
  Context.get_endorser (B b) >>=? fun (delegate, slots) ->
  Op.endorsement_with_slot ~delegate:(delegate, slots) (B b) () >>=? fun op ->
  (* bake at priority 0 succeed thanks to enough endorsements *)
  Block.bake ~policy:(By_priority 0) ~operations:[Operation.pack op] b
  >>=? fun b ->
  (match Fitness_repr.to_int64 b.header.shell.fitness with
  | Ok new_fitness -> Int64.to_int new_fitness - fitness
  | Error _ -> assert false)
  |> fun res ->
  (* in Emmy+, the fitness increases by 1, so the difference between
     the fitness at level 1 and at level 0 is 1, independently if the
     number fo endorsements (here 1) *)
  Assert.equal_int ~loc:__LOC__ res 1 >>=? fun () -> return_unit

let tests =
  [
    Tztest.tztest "Simple endorsement" `Quick test_simple_endorsement;
    Tztest.tztest "Unwrapped endorsement" `Quick test_unwrapped_endorsement;
    Tztest.tztest
      "Endorsement wrapped with invalid slot"
      `Quick
      test_bad_slot_wrapper;
    Tztest.tztest
      "Endorsement wrapped with slot -1"
      `Quick
      test_neg_slot_wrapper;
    Tztest.tztest
      "Endorsement wrapped with non-normalized slot"
      `Quick
      test_non_normalized_slot_wrapper;
    Tztest.tztest "Maximum endorsement" `Quick test_max_endorsement;
    Tztest.tztest "Consistent priorities" `Quick test_consistent_priorities;
    Tztest.tztest "Reward retrieval" `Quick test_reward_retrieval;
    Tztest.tztest
      "Reward retrieval two endorsers"
      `Quick
      test_reward_retrieval_two_endorsers;
    Tztest.tztest "Endorsement threshold" `Quick test_endorsement_threshold;
    Tztest.tztest "Fitness gap" `Quick test_fitness_gap;
    (* Fail scenarios *)
    Tztest.tztest
      "Wrong endorsement predecessor"
      `Quick
      test_wrong_endorsement_predecessor;
    Tztest.tztest
      "Invalid endorsement level"
      `Quick
      test_invalid_endorsement_level;
    Tztest.tztest "Duplicate endorsement" `Quick test_duplicate_endorsement;
    Tztest.tztest "Not enough for deposit" `Quick test_not_enough_for_deposit;
  ]
