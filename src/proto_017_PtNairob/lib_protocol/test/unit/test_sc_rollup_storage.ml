(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:  Protocol Sc_rollup_storage
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
      -- test "^\[Unit\] sc rollup storage$"
    Subject:    Tests for the SCORU storage module
*)

open Protocol
module Commitment_repr = Sc_rollup_commitment_repr

(** [new_context_n n] creates a context with [n] accounts. *)
let new_context_n nb_stakers =
  let open Lwt_result_syntax in
  let* b, contracts = Context.init_n nb_stakers () in
  let+ inc = Incremental.begin_construction b in
  let ctxt = Incremental.alpha_ctxt inc in
  (* Necessary to originate rollups. *)
  let ctxt = Alpha_context.Origination_nonce.init ctxt Operation_hash.zero in
  let ctxt = Alpha_context.Internal_for_tests.to_raw ctxt in
  let accounts =
    List.map
      (function
        | Alpha_context.Contract.Implicit key -> key | _ -> assert false)
      contracts
  in
  (ctxt, accounts)

let new_context () =
  let open Lwt_result_syntax in
  (* A context needs at least one account to bake. *)
  let* ctxt, _accounts = new_context_n 1 in
  return ctxt

let new_context_1 () =
  let open Lwt_result_syntax in
  let* ctxt, accounts = new_context_n 1 in
  match accounts with [account] -> return (ctxt, account) | _ -> assert false

let new_context_2 () =
  let open Lwt_result_syntax in
  let* ctxt, accounts = new_context_n 2 in
  match accounts with
  | [account1; account2] -> return (ctxt, account1, account2)
  | _ -> assert false

let new_sc_rollup ctxt =
  let open Lwt_result_wrap_syntax in
  let {Michelson_v1_parser.expanded; _}, _ =
    Michelson_v1_parser.parse_expression "unit"
  in
  let parameters_ty = Alpha_context.Script.lazy_expr expanded in
  let boot_sector = "" in
  let kind = Sc_rollups.Kind.Example_arith in
  let*! genesis_commitment =
    Sc_rollup_helpers.genesis_commitment_raw
      ~boot_sector
      ~origination_level:(Raw_context.current_level ctxt).level
      kind
  in
  let* rollup, _size, genesis_hash, ctxt =
    Sc_rollup_storage.originate ctxt ~kind ~parameters_ty ~genesis_commitment
  in
  return (rollup, genesis_hash, ctxt)

let new_context_with_stakers_and_rollup nb_stakers =
  let open Lwt_result_wrap_syntax in
  let* ctxt, stakers = new_context_n nb_stakers in
  let+@ rollup, genesis_hash, ctxt = new_sc_rollup ctxt in
  (ctxt, rollup, genesis_hash, stakers)

let new_context_with_rollup () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = new_context () in
  let+@ rollup, genesis_hash, ctxt = new_sc_rollup ctxt in
  (ctxt, rollup, genesis_hash)

let equal_tez ~loc =
  Assert.equal ~loc Tez_repr.( = ) "Tez aren't equal" Tez_repr.pp

let assert_not_exist ~loc ~pp comp_lwt =
  let open Lwt_result_syntax in
  let* _ctxt, res_opt = comp_lwt in
  Assert.is_none ~loc ~pp res_opt

let assert_balance_changed op ctxt ctxt' account amount =
  let open Lwt_result_wrap_syntax in
  let*@ _, balance = Token.balance ctxt account in
  let*@ _, balance' = Token.balance ctxt' account in
  let*@ balance_op_amount = op balance amount in
  equal_tez balance' ~loc:__LOC__ balance_op_amount

let assert_balance_increased ctxt ctxt' account amount =
  let ( +? ) t1 t2 = Lwt.return Tez_repr.(t1 +? t2) in
  assert_balance_changed ( +? ) ctxt ctxt' account amount

let assert_balance_decreased ctxt ctxt' account amount =
  let ( -? ) t1 t2 = Lwt.return Tez_repr.(t1 -? t2) in
  assert_balance_changed ( -? ) ctxt ctxt' account amount

let perform_staking_action_and_check ctxt rollup staker do_and_check =
  let staker_contract = Contract_repr.Implicit staker in
  let stake = Constants_storage.sc_rollup_stake_amount ctxt in
  do_and_check ctxt rollup staker_contract stake

let number_of_ticks_exn n =
  match Sc_rollup_repr.Number_of_ticks.of_value n with
  | Some x -> x
  | None -> Stdlib.failwith "Bad Number_of_ticks"

module Zero = struct
  let staker = Sc_rollup_repr.Staker.zero

  let commitment =
    Commitment_repr.
      {
        compressed_state = Sc_rollup_repr.State_hash.zero;
        inbox_level = Raw_level_repr.root;
        predecessor = Hash.zero;
        number_of_ticks = number_of_ticks_exn 1L;
      }

  let commitment_hash = Commitment_repr.Hash.zero

  let rollup = Sc_rollup_repr.Address.zero
end

let deposit_stake_and_check_balances ctxt rollup staker =
  let open Lwt_result_wrap_syntax in
  perform_staking_action_and_check
    ctxt
    rollup
    staker
    (fun ctxt rollup staker_contract stake ->
      let*@ ctxt', _, _ =
        Sc_rollup_stake_storage.Internal_for_tests.deposit_stake
          ctxt
          rollup
          staker
      in
      let* () =
        assert_balance_decreased ctxt ctxt' (`Contract staker_contract) stake
      in
      let bond_id = Bond_id_repr.Sc_rollup_bond_id rollup in
      let bonds_account = `Frozen_bonds (staker_contract, bond_id) in
      let+ () = assert_balance_increased ctxt ctxt' bonds_account stake in
      ctxt')

(** Originate a rollup with [nb_stakers] stakers and make a deposit to the
    initial LCC. *)
let originate_rollup_and_deposit_with_n_stakers nb_stakers =
  let open Lwt_result_syntax in
  let* ctxt, rollup, genesis_hash, stakers =
    new_context_with_stakers_and_rollup nb_stakers
  in
  let deposit ctxt staker =
    deposit_stake_and_check_balances ctxt rollup staker
  in
  let+ ctxt = List.fold_left_es deposit ctxt stakers in
  (ctxt, rollup, genesis_hash, stakers)

(** Originate a rollup with one staker and make a deposit to the initial LCC. *)
let originate_rollup_and_deposit_with_one_staker () =
  let open Lwt_result_wrap_syntax in
  let* ctxt, staker = new_context_1 () in
  let*@ rollup, genesis_hash, ctxt = new_sc_rollup ctxt in
  let+ ctxt = deposit_stake_and_check_balances ctxt rollup staker in
  (ctxt, rollup, genesis_hash, staker)

(** Originate a rollup with two stakers and make a deposit to the initial LCC.
*)
let originate_rollup_and_deposit_with_two_stakers () =
  let open Lwt_result_wrap_syntax in
  let* ctxt, staker1, staker2 = new_context_2 () in
  let*@ rollup, genesis_hash, ctxt = new_sc_rollup ctxt in
  let* ctxt = deposit_stake_and_check_balances ctxt rollup staker1 in
  let+ ctxt = deposit_stake_and_check_balances ctxt rollup staker2 in
  (ctxt, rollup, genesis_hash, staker1, staker2)

(** Originate a rollup with three stakers and make a deposit to the initial LCC.
*)
let originate_rollup_and_deposit_with_three_stakers () =
  let open Lwt_result_syntax in
  let+ ctxt, rollup, genesis_hash, stakers =
    originate_rollup_and_deposit_with_n_stakers 3
  in
  match stakers with
  | [staker1; staker2; staker3] ->
      (ctxt, rollup, genesis_hash, staker1, staker2, staker3)
  | _ -> assert false

(** Trivial assertion.

    By convention, context is passed linearly as [ctxt].  This takes a context
    argument to allow this.
*)
let assert_true _ctxt = return ()

(** Assert that the computation fails with the given message. *)
let assert_fails_with ~loc k expected_err =
  let open Lwt_result_syntax in
  let*! res = k in
  let res = Environment.wrap_tzresult res in
  Assert.proto_error ~loc res (( = ) expected_err)

let assert_fails_with_f ~loc k pred =
  let open Lwt_result_syntax in
  let*! res = k in
  let res = Environment.wrap_tzresult res in
  Assert.proto_error ~loc res pred

let assert_fails ~loc k =
  let open Lwt_result_syntax in
  let*! res = k in
  let res = Environment.wrap_tzresult res in
  Assert.error ~loc res (fun _ -> true)

(** Assert operation fails because of missing rollup *)
let assert_fails_with_missing_rollup ~loc op =
  let open Lwt_result_syntax in
  let* ctxt = new_context () in
  assert_fails_with
    ~loc
    (op ctxt Zero.rollup)
    (Sc_rollup_errors.Sc_rollup_does_not_exist Zero.rollup)

(** Assert commitment hash equality. *)
let assert_commitment_hash_equal ~loc x y =
  Assert.equal
    ~loc
    Commitment_repr.Hash.equal
    "Compare commitment hash"
    Commitment_repr.Hash.pp
    x
    y

let assert_level_equal ~loc =
  Assert.equal ~loc Raw_level_repr.equal "Compare raw level" Raw_level_repr.pp

let commitment_equal
    Commitment_repr.
      {
        compressed_state = c1;
        inbox_level = l1;
        predecessor = p1;
        number_of_ticks = n1;
      }
    Commitment_repr.
      {
        compressed_state = c2;
        inbox_level = l2;
        predecessor = p2;
        number_of_ticks = n2;
      } =
  Sc_rollup_repr.State_hash.equal c1 c2
  && Raw_level_repr.equal l1 l2
  && Commitment_repr.Hash.equal p1 p2
  && Sc_rollup_repr.Number_of_ticks.equal n1 n2

let assert_commitment_equal ~loc x y =
  Assert.equal ~loc commitment_equal "Compare commitment" Commitment_repr.pp x y

let assert_commitments_with_levels_equal ~loc cs1 cs2 =
  let commitment_with_level_pp ppf (hash, level) =
    Format.fprintf
      ppf
      "(%a, %a)"
      Sc_rollup_commitment_repr.Hash.pp
      hash
      Raw_level_repr.pp
      level
  in
  Assert.assert_equal_list
    ~loc
    (fun (commitment1, level1) (commitment2, level2) ->
      Sc_rollup_commitment_repr.Hash.(commitment1 = commitment2)
      && Raw_level_repr.(level1 = level2))
    "Unexpected list of cemented commitments"
    commitment_with_level_pp
    cs1
    cs2

(* Artificially advance current level to make stake refinement possible.
   The commitment can be posted after the inbox level commited. For example,
   if you post a commitment for the inbox level 32, you will be able to
   publish the commitment at level 33.
*)
let advance_level_for_commitment ctxt (commitment : Commitment_repr.t) =
  let cur_level = Level_storage.(current ctxt).level in
  if cur_level > commitment.inbox_level then ctxt
  else
    let offset =
      let open Raw_level_repr in
      let open Int32 in
      succ @@ sub (to_int32 commitment.inbox_level) (to_int32 cur_level)
    in
    Raw_context.Internal_for_tests.add_level ctxt (Int32.to_int offset)

let advance_level_for_cement ctxt rollup (commitment : Commitment_repr.t) =
  let open Lwt_result_syntax in
  let* ctxt, commitment_added =
    Storage.Sc_rollup.Commitment_added.get
      (ctxt, rollup)
      (Commitment_repr.hash_uncarbonated commitment)
  in
  let challenge_window =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  let cur_level = Level_storage.(current ctxt).level in
  let target_level = Raw_level_repr.add commitment_added challenge_window in
  if cur_level > target_level then return ctxt
  else
    let offset = Raw_level_repr.diff target_level cur_level in
    return (Raw_context.Internal_for_tests.add_level ctxt (Int32.to_int offset))

let advance_level_n_refine_stake ctxt rollup staker commitment =
  let ctxt = advance_level_for_commitment ctxt commitment in
  Sc_rollup_stake_storage.Internal_for_tests.refine_stake
    ctxt
    rollup
    staker
    commitment

let valid_inbox_level ctxt =
  let root_level = Level_storage.(current ctxt).level in
  let commitment_freq =
    Constants_storage.sc_rollup_commitment_period_in_blocks ctxt
  in
  fun i ->
    Raw_level_repr.add
      root_level
      Int32.(to_int (mul (of_int commitment_freq) i))

(** A more precise version of {!valid_inbox_level}. Not used everywhere
    as it requires more information than {!valid_inbox_level} and is in
    the lwt tzresult monad. *)
let proper_valid_inbox_level (ctxt, rollup) i =
  let open Lwt_result_syntax in
  let+ _, {level = genesis_level; _} =
    Sc_rollup_storage.genesis_info ctxt rollup
  in
  let commitment_freq =
    Constants_storage.sc_rollup_commitment_period_in_blocks ctxt
  in
  Raw_level_repr.add genesis_level (commitment_freq * i)

let commitment ?compressed_state ?predecessor ?inbox_level ?number_of_ticks () =
  let commitment =
    Commitment_repr.
      {
        compressed_state =
          Option.value
            ~default:Zero.commitment.compressed_state
            compressed_state;
        predecessor =
          Option.value ~default:Zero.commitment.predecessor predecessor;
        inbox_level =
          Option.value ~default:Zero.commitment.inbox_level inbox_level;
        number_of_ticks =
          Option.value ~default:Zero.commitment.number_of_ticks number_of_ticks;
      }
  in
  (commitment, Commitment_repr.hash_uncarbonated commitment)

(** [commitments ~predecessor ?start_at_level ctxt n] creates a branch of
    commitment starting at [valid_inbox_level ctxt start_at_level] and with
    [predecessor] as commitment's predecessor. The branch is [n] elements
    deep. *)
let commitments ~predecessor ?(start_at = 1l) ctxt n =
  let n = Int32.(add (of_int n) start_at) in
  let rec go predecessor acc l =
    if n = l then acc
    else
      let inbox_level = valid_inbox_level ctxt l in
      let commitment, hash = commitment ~predecessor ~inbox_level () in
      go hash (commitment :: acc) (Int32.succ l)
  in
  List.rev (go predecessor [] start_at)

let publish_commitment ctxt rollup staker commitment =
  let open Lwt_result_syntax in
  let ctxt = advance_level_for_commitment ctxt commitment in
  let* _hash, _publication_level, ctxt, _balance_updates =
    Sc_rollup_stake_storage.publish_commitment ctxt rollup staker commitment
  in
  return ctxt

let publish_commitments ctxt rollup staker commitments =
  List.fold_left_es
    (fun ctxt commitment -> publish_commitment ctxt rollup staker commitment)
    ctxt
    commitments

let cement_commitment ctxt rollup commitment =
  let open Lwt_result_syntax in
  let* ctxt = advance_level_for_cement ctxt rollup commitment in
  let* ctxt, _commitment, _commitment_hash =
    Sc_rollup_stake_storage.cement_commitment ctxt rollup
  in
  return ctxt

let cement_commitments ctxt rollup commitments =
  List.fold_left_es
    (fun ctxt commitment -> cement_commitment ctxt rollup commitment)
    ctxt
    commitments

let publish_and_cement_commitment ctxt rollup staker commitment =
  let open Lwt_result_syntax in
  let* ctxt = publish_commitment ctxt rollup staker commitment in
  cement_commitment ctxt rollup commitment

let publish_and_cement_commitments ctxt rollup staker commitments =
  List.fold_left_es
    (fun ctxt commitment ->
      publish_and_cement_commitment ctxt rollup staker commitment)
    ctxt
    commitments

let withdraw ctxt rollup staker =
  let open Lwt_result_syntax in
  let* ctxt, _balance_updates =
    Sc_rollup_stake_storage.withdraw_stake ctxt rollup staker
  in
  return ctxt

let assert_staker_exists ctxt rollup staker =
  let open Lwt_result_wrap_syntax in
  (* Assert the stake was frozen from the balance. *)
  let stake = Constants_storage.sc_rollup_stake_amount ctxt in
  let*@ frozen_balance =
    Contract_storage.(get_frozen_bonds ctxt (Contract_repr.Implicit staker))
  in
  let* () = equal_tez ~loc:__LOC__ stake frozen_balance in
  (* Assert [staker] was given an index. *)
  let*@ ctxt, staker_index_opt =
    Sc_rollup_staker_index_storage.find_staker_index_unsafe ctxt rollup staker
  in
  let* staker_index = Assert.get_some ~loc:__LOC__ staker_index_opt in
  let*@ _ctxt, exists =
    Sc_rollup_staker_index_storage.is_active ctxt rollup staker_index
  in
  Assert.equal_bool ~loc:__LOC__ true exists

let assert_staker_dont_exists ctxt rollup staker =
  let open Lwt_result_wrap_syntax in
  (* Assert no stake was frozen from the balance. *)
  let*@ frozen_balance =
    Contract_storage.(get_frozen_bonds ctxt (Contract_repr.Implicit staker))
  in
  let* () = equal_tez ~loc:__LOC__ Tez_repr.zero frozen_balance in
  (* Assert [staker] has no index. *)
  let*@ _ctxt, staker_index_opt =
    Sc_rollup_staker_index_storage.find_staker_index_unsafe ctxt rollup staker
  in
  Assert.is_none ~loc:__LOC__ ~pp:Z.pp_print (staker_index_opt :> Z.t option)

let assert_staked_or_not ~staked ctxt rollup hash staker =
  let open Lwt_result_wrap_syntax in
  let*@ _ctxt, stakers_index =
    Storage.Sc_rollup.Commitment_stakers.get (ctxt, rollup) hash
  in
  let*@ _ctxt, staker_index =
    Sc_rollup_staker_index_storage.get_staker_index_unsafe ctxt rollup staker
  in
  Assert.equal_bool
    ~loc:__LOC__
    staked
    (List.mem ~equal:Z.equal (staker_index :> Z.t) (stakers_index :> Z.t list))

let assert_staked_on = assert_staked_or_not ~staked:true

let assert_not_staked_on = assert_staked_or_not ~staked:false

let assert_commitment_metadata_exists ?publication_level ctxt rollup commitment
    staker =
  let open Lwt_result_wrap_syntax in
  (* Assert the commitment's metadata exists. *)
  let hash = Commitment_repr.hash_uncarbonated commitment in
  let*@ _ctxt, actual_publication_level =
    Storage.Sc_rollup.Commitment_added.get (ctxt, rollup) hash
  in
  let* () =
    match publication_level with
    | None -> return_unit
    | Some publication_level ->
        assert_level_equal
          ~loc:__LOC__
          publication_level
          actual_publication_level
  in
  let* () = assert_staked_on ctxt rollup hash staker in
  let*@ _ctxt, commitments_hash =
    Storage.Sc_rollup.Commitments_per_inbox_level.get
      (ctxt, rollup)
      commitment.inbox_level
  in
  Assert.equal_bool
    ~loc:__LOC__
    true
    (List.mem ~equal:Commitment_repr.Hash.equal hash commitments_hash)

let assert_commitment_exists ctxt rollup commitment =
  let open Lwt_result_wrap_syntax in
  (* Assert commitment exists. *)
  let hash = Commitment_repr.hash_uncarbonated commitment in
  let*@ actual_commitment_opt, _ctxt =
    Sc_rollup_commitment_storage.get_commitment_opt_unsafe ctxt rollup hash
  in
  let* actual_commitment = Assert.get_some ~loc:__LOC__ actual_commitment_opt in
  let* () = assert_commitment_equal ~loc:__LOC__ commitment actual_commitment in
  return_unit

let assert_commitment_metadata_dont_exists ctxt rollup commitment =
  let open Lwt_result_wrap_syntax in
  (* Assert the commitment's metadata dont exists. *)
  let hash = Commitment_repr.hash_uncarbonated commitment in
  let*@ _ctxt, exists =
    Storage.Sc_rollup.Commitment_added.mem (ctxt, rollup) hash
  in
  let* () = Assert.equal_bool ~loc:__LOC__ false exists in
  let*@ _ctxt, exists =
    Storage.Sc_rollup.Commitment_stakers.mem (ctxt, rollup) hash
  in
  let* () = Assert.equal_bool ~loc:__LOC__ false exists in
  let*@ _ctxt, exists =
    Storage.Sc_rollup.Commitments_per_inbox_level.mem
      (ctxt, rollup)
      commitment.inbox_level
  in
  Assert.equal_bool ~loc:__LOC__ false exists

let assert_commitment_dont_exists ctxt rollup commitment =
  let open Lwt_result_wrap_syntax in
  let hash = Commitment_repr.hash_uncarbonated commitment in
  let*@ commitment_opt, _ctxt =
    Sc_rollup_commitment_storage.get_commitment_opt_unsafe ctxt rollup hash
  in
  Assert.is_none ~loc:__LOC__ ~pp:Commitment_repr.pp commitment_opt

(** {2. Tests} *)

module Stake_storage_tests = struct
  let test_last_cemented_commitment_hash_with_level_when_genesis () =
    let open Lwt_result_wrap_syntax in
    let* ctxt = new_context () in
    let*@ rollup, genesis_hash, ctxt = new_sc_rollup ctxt in
    let*@ c1, inbox_level, ctxt =
      Sc_rollup_commitment_storage.last_cemented_commitment_hash_with_level
        ctxt
        rollup
    in
    let* () = assert_commitment_hash_equal ~loc:__LOC__ genesis_hash c1 in
    Assert.equal_int32
      ~loc:__LOC__
      (Raw_level_repr.to_int32 (Raw_context.current_level ctxt).level)
      (Raw_level_repr.to_int32 inbox_level)

  (** {2. Deposit unit tests.} *)

  (** Test that deposit initializes the metadata for the staker. *)
  let test_deposit () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, staker = new_context_1 () in
    let contract_staker = Contract_repr.Implicit staker in
    let*@ rollup, _genesis_hash, ctxt = new_sc_rollup ctxt in
    let*@ ctxt_after_deposit, _balance_updates, _staker_index =
      Sc_rollup_stake_storage.Internal_for_tests.deposit_stake
        ctxt
        rollup
        staker
    in
    (* Assert [staker]'s metadata exists. *)
    let* () = assert_staker_exists ctxt_after_deposit rollup staker in
    let stake = Constants_storage.sc_rollup_stake_amount ctxt in
    (* Assert [staker]'s balance decreased of [stake]. *)
    let* () =
      assert_balance_decreased
        ctxt
        ctxt_after_deposit
        (`Contract contract_staker)
        stake
    in
    let bond_id = Bond_id_repr.Sc_rollup_bond_id rollup in
    let bonds_account = `Frozen_bonds (contract_staker, bond_id) in
    (* Assert [bond_account]'s balance increased of [stake]. *)
    let* () =
      assert_balance_increased ctxt ctxt_after_deposit bonds_account stake
    in
    return_unit

  (** Test that deposit fails if the staker is underfunded. *)
  let test_deposit_by_underfunded_staker () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, sc_rollup, _genesis_hash = new_context_with_rollup () in
    let staker =
      Sc_rollup_repr.Staker.of_b58check_exn
        "tz1hhNZvjed6McQQLWtR7MRzPHpgSFZTXxdW"
    in
    let stake = Constants_storage.sc_rollup_stake_amount ctxt in
    let* () =
      assert_fails_with
        ~loc:__LOC__
        (Sc_rollup_stake_storage.Internal_for_tests.deposit_stake
           ctxt
           sc_rollup
           staker)
        (Sc_rollup_errors.Sc_rollup_staker_funds_too_low
           {
             staker;
             sc_rollup;
             staker_balance = Tez_repr.zero;
             min_expected_balance = stake;
           })
    in
    let staker_balance = Tez_repr.div_exn stake 2 in
    let staker_contract = Contract_repr.Implicit staker in
    let*@ ctxt, _ =
      Token.transfer ctxt `Minted (`Contract staker_contract) staker_balance
    in
    assert_fails_with
      ~loc:__LOC__
      (Sc_rollup_stake_storage.Internal_for_tests.deposit_stake
         ctxt
         sc_rollup
         staker)
      (Sc_rollup_errors.Sc_rollup_staker_funds_too_low
         {staker; sc_rollup; staker_balance; min_expected_balance = stake})

  (** Test that a staker can deposit and then withdraw. *)
  let test_deposit_then_withdraw () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, _genesis_hash, staker =
      originate_rollup_and_deposit_with_one_staker ()
    in
    let* () = assert_staker_exists ctxt rollup staker in
    let*@ ctxt, _balance_updates =
      Sc_rollup_stake_storage.withdraw_stake ctxt rollup staker
    in
    assert_staker_dont_exists ctxt rollup staker

  (** Test that an account can stake on more than one rollup. *)
  let test_deposit_on_two_rollups () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, staker = new_context_1 () in
    let assert_frozen_balance ctxt staker expected =
      let*@ frozen_balance =
        Contract_storage.(get_frozen_bonds ctxt (Contract_repr.Implicit staker))
      in
      equal_tez ~loc:__LOC__ expected frozen_balance
    in
    let stake = Constants_storage.sc_rollup_stake_amount ctxt in
    let* () = assert_frozen_balance ctxt staker Tez_repr.zero in
    let*@ rollup1, _genesis_hash, ctxt = new_sc_rollup ctxt in
    let*@ ctxt, _balance_updates, _staker_index =
      Sc_rollup_stake_storage.Internal_for_tests.deposit_stake
        ctxt
        rollup1
        staker
    in
    let* () = assert_frozen_balance ctxt staker stake in
    let*@ rollup2, _genesis_hash, ctxt = new_sc_rollup ctxt in
    let*@ ctxt, _balance_updates, _staker_index =
      Sc_rollup_stake_storage.Internal_for_tests.deposit_stake
        ctxt
        rollup2
        staker
    in
    assert_frozen_balance ctxt staker (Tez_repr.mul_exn stake 2)

  (** Test that deposit twice on the same rollup fails. *)
  let test_deposit_twice_fails () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, staker = new_context_1 () in
    let*@ rollup, _genesis_hash, ctxt = new_sc_rollup ctxt in
    let*@ ctxt, _balance_updates, _staker_index =
      Sc_rollup_stake_storage.Internal_for_tests.deposit_stake
        ctxt
        rollup
        staker
    in
    assert_fails
      ~loc:__LOC__
      (Sc_rollup_stake_storage.Internal_for_tests.deposit_stake
         ctxt
         rollup
         staker)

  (** {2. Publish unit tests.} *)

  (** Test that the staker, the commitment and its metadata exist after
      the publish. *)
  let test_publish () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, staker = new_context_1 () in
    let*@ rollup, genesis_hash, ctxt = new_sc_rollup ctxt in
    let commitment, _hash =
      commitment
        ~predecessor:genesis_hash
        ~inbox_level:(valid_inbox_level ctxt 1l)
        ()
    in
    let*@ ctxt = publish_commitment ctxt rollup staker commitment in
    (* The staker exists as a stake was deposited. *)
    let* () = assert_staker_exists ctxt rollup staker in
    (* The commitment and its metadata exists in the storage. *)
    let* () = assert_commitment_exists ctxt rollup commitment in
    let* () = assert_commitment_metadata_exists ctxt rollup commitment staker in
    return_unit

  (** Test that publish twice to the same level is allowed unless the
      commitment changes. *)
  let test_publish_twice () =
    let open Lwt_result_wrap_syntax in
    (* In contrary to deposit, cement and withdraw, the same commitment
       can be published twice (and even more). *)
    let* ctxt, staker = new_context_1 () in
    let*@ rollup, genesis_hash, ctxt = new_sc_rollup ctxt in
    let commitment, _hash =
      commitment
        ~predecessor:genesis_hash
        ~inbox_level:(valid_inbox_level ctxt 1l)
        ()
    in
    let*@ ctxt = publish_commitment ctxt rollup staker commitment in
    let* () = assert_commitment_exists ctxt rollup commitment in
    let* () = assert_commitment_metadata_exists ctxt rollup commitment staker in
    (* Assert that publishing twice succeeds and the commitment still exist. *)
    let*@ ctxt = publish_commitment ctxt rollup staker commitment in
    let* () = assert_commitment_exists ctxt rollup commitment in
    let* () = assert_commitment_metadata_exists ctxt rollup commitment staker in

    (* However, you can publish at most one unique commitment per inbox level. *)
    let new_commitment =
      {
        commitment with
        compressed_state =
          Sc_rollup_repr.State_hash.context_hash_to_state_hash
            (Context_hash.hash_string ["honest"]);
      }
    in
    let* () =
      assert_fails_with
        ~loc:__LOC__
        (publish_commitment ctxt rollup staker new_commitment)
        Sc_rollup_errors.Sc_rollup_staker_double_stake
    in
    return_unit

  (** Test that the entrypoint [publish] fails with a nice error if
      the rollup is missing. *)
  let test_publish_to_missing_rollup () =
    assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
        Sc_rollup_stake_storage.publish_commitment
          ctxt
          rollup
          Zero.staker
          Zero.commitment)

  (** Test that publish to a wrong inbox level is forbidden. *)
  let test_publish_wrong_inbox_level () =
    let open Lwt_result_syntax in
    let* ctxt, rollup, genesis_hash, staker =
      originate_rollup_and_deposit_with_one_staker ()
    in
    let commitment, _hash =
      commitment
        ~predecessor:genesis_hash
        ~inbox_level:(Raw_level_repr.of_int32_exn 42l)
        ()
    in
    assert_fails_with
      ~loc:__LOC__
      (publish_commitment ctxt rollup staker commitment)
      Sc_rollup_errors.Sc_rollup_bad_inbox_level

  (** Test that two stakers can publish the same commitment. *)
  let test_publish_existing_commitment () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker1, staker2 =
      originate_rollup_and_deposit_with_two_stakers ()
    in
    let commitment, _hash =
      commitment
        ~predecessor:genesis_hash
        ~inbox_level:(valid_inbox_level ctxt 1l)
        ()
    in
    let*@ ctxt = publish_commitment ctxt rollup staker1 commitment in
    let*@ ctxt = publish_commitment ctxt rollup staker2 commitment in
    (* [staker2] does exist and stakes on [commitment]. *)
    let* () = assert_staker_exists ctxt rollup staker2 in
    let* () =
      assert_commitment_metadata_exists ctxt rollup commitment staker2
    in
    let* () = assert_commitment_exists ctxt rollup commitment in
    return_unit

  (** Test that publish returns the oldest level at which the commitment
      was published. *)
  let test_publish_returns_oldest_publish_level () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker1, staker2 =
      originate_rollup_and_deposit_with_two_stakers ()
    in
    let commitment, _hash =
      commitment
        ~predecessor:genesis_hash
        ~inbox_level:(valid_inbox_level ctxt 1l)
        ()
    in
    let ctxt = advance_level_for_commitment ctxt commitment in
    let*@ _hash, publish_level, ctxt, _balance_updates =
      Sc_rollup_stake_storage.publish_commitment ctxt rollup staker1 commitment
    in
    let ctxt = Raw_context.Internal_for_tests.add_level ctxt 42 in
    let*@ _hash, publish_level', _ctxt, _balance_updates =
      Sc_rollup_stake_storage.publish_commitment ctxt rollup staker2 commitment
    in
    Assert.equal_int32
      ~loc:__LOC__
      (Raw_level_repr.to_int32 publish_level)
      (Raw_level_repr.to_int32 publish_level')

  (** Test that a commitment can not be published from the future. *)
  let test_publish_fails_on_commitment_from_future () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker =
      originate_rollup_and_deposit_with_one_staker ()
    in
    let inbox_level = valid_inbox_level ctxt 1l in
    let commitment, _hash =
      commitment ~predecessor:genesis_hash ~inbox_level ()
    in
    let* () =
      assert_fails_with
        ~loc:__LOC__
        (Sc_rollup_stake_storage.publish_commitment
           ctxt
           rollup
           staker
           commitment)
        (Sc_rollup_errors.Sc_rollup_commitment_from_future
           {current_level = Raw_level_repr.of_int32_exn 1l; inbox_level})
    in
    let ctxt = advance_level_for_commitment ctxt commitment in
    let*@ _hash, _publish_level, ctxt, _balance_updates =
      Sc_rollup_stake_storage.publish_commitment ctxt rollup staker commitment
    in
    let* () = assert_commitment_exists ctxt rollup commitment in
    assert_commitment_metadata_exists ctxt rollup commitment staker

  (** Test that a staker can publish a commitment when its last commimtent was
      cemented. *)
  let test_publish_from_behind_lcc () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker =
      originate_rollup_and_deposit_with_one_staker ()
    in
    let level l = valid_inbox_level ctxt l in
    let level1 = level 1l in
    let level2 = level 2l in
    let commitment1, hash =
      commitment ~predecessor:genesis_hash ~inbox_level:level1 ()
    in
    let*@ ctxt = publish_commitment ctxt rollup staker commitment1 in
    let*@ ctxt = cement_commitment ctxt rollup commitment1 in
    let commitment2, _hash =
      commitment ~predecessor:hash ~inbox_level:level2 ()
    in
    let*@ ctxt = publish_commitment ctxt rollup staker commitment2 in
    let* () = assert_commitment_exists ctxt rollup commitment2 in
    assert_commitment_metadata_exists ctxt rollup commitment2 staker

  (** Test that a staker can join another staker's branch without
      staking explicitely on each commitment. *)
  let test_publish_anywhere_on_branch () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker1, staker2 =
      originate_rollup_and_deposit_with_two_stakers ()
    in
    (* staker1 creates a first branch. *)
    let commitments = commitments ~predecessor:genesis_hash ctxt 10 in
    let*@ ctxt = publish_commitments ctxt rollup staker1 commitments in

    (* staker2 will stake on only one commitment. *)
    let to_stake_commitment = Stdlib.List.nth commitments 6 in
    let not_staked_commitments =
      List.filter (( <> ) to_stake_commitment) commitments
    in
    let*@ ctxt = publish_commitment ctxt rollup staker2 to_stake_commitment in
    (* Assert [staker2] is staked on [to_stake_commitment] and not on
       other commitments. *)
    let* () =
      assert_staked_on
        ctxt
        rollup
        (Commitment_repr.hash_uncarbonated to_stake_commitment)
        staker2
    in
    let* () =
      List.iter_es
        (fun commitment ->
          assert_not_staked_on
            ctxt
            rollup
            (Commitment_repr.hash_uncarbonated commitment)
            staker2)
        not_staked_commitments
    in
    return_unit

  (** Test that a commitment needs to have a predecessor. *)
  let test_publish_without_predecessor_fails () =
    let open Lwt_result_syntax in
    let* ctxt, rollup, _genesis_hash, staker =
      originate_rollup_and_deposit_with_one_staker ()
    in
    let commitment, _hash =
      commitment ~inbox_level:(valid_inbox_level ctxt 42l) ()
    in
    assert_fails_with
      ~loc:__LOC__
      (publish_commitment ctxt rollup staker commitment)
      (Sc_rollup_errors.Sc_rollup_unknown_commitment commitment.predecessor)

  (** Test that publishing a commitment at the LCC or behind it fails. *)
  let test_publish_behind_or_at_lcc_fails () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker =
      originate_rollup_and_deposit_with_one_staker ()
    in
    let commitments = commitments ~predecessor:genesis_hash ctxt 10 in
    let*@ ctxt =
      publish_and_cement_commitments ctxt rollup staker commitments
    in
    let*@ _ctxt, last_cemented_inbox_level, _ =
      Sc_rollup_commitment_storage.last_cemented_commitment_hash_with_level
        ctxt
        rollup
    in
    (* Trying to publish a commitment behind or at lcc will fail. *)
    List.iter_es
      (fun commitment ->
        assert_fails_with
          ~loc:__LOC__
          (Sc_rollup_stake_storage.publish_commitment
             ctxt
             rollup
             staker
             commitment)
          (Sc_rollup_errors.Sc_rollup_commitment_too_old
             {
               commitment_inbox_level = commitment.inbox_level;
               last_cemented_inbox_level;
             }))
      commitments

  (** {2. Cement unit tests.} *)

  (** Test that cement cleans the commitment's metadata. *)
  let test_cement () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker =
      originate_rollup_and_deposit_with_one_staker ()
    in
    let commitment, _hash =
      commitment
        ~predecessor:genesis_hash
        ~inbox_level:(valid_inbox_level ctxt 1l)
        ()
    in
    let*@ ctxt = publish_commitment ctxt rollup staker commitment in
    let* () = assert_commitment_exists ctxt rollup commitment in
    let* () = assert_commitment_metadata_exists ctxt rollup commitment staker in
    let*@ ctxt = cement_commitment ctxt rollup commitment in
    assert_commitment_metadata_dont_exists ctxt rollup commitment

  (** Test that the entrypoint [cement] fails with a nice error if
      the rollup is missing. *)
  let test_cement_to_missing_rollup () =
    assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
        Sc_rollup_stake_storage.cement_commitment ctxt rollup)

  (** Test that if [n] stakers stake on the same commitment, it can be
      cemented. *)
  let test_cement_with_n_stakers () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, stakers =
      originate_rollup_and_deposit_with_n_stakers 10
    in
    let commitment, _hash =
      commitment
        ~predecessor:genesis_hash
        ~inbox_level:(valid_inbox_level ctxt 1l)
        ()
    in
    let*@ ctxt =
      List.fold_left_es
        (fun ctxt staker -> publish_commitment ctxt rollup staker commitment)
        ctxt
        stakers
    in
    let*@ ctxt = cement_commitment ctxt rollup commitment in
    assert_commitment_metadata_dont_exists ctxt rollup commitment

  (** Create and cement three commitments:

     [c3 -> c2 -> c1 -> Commitment_hash.zero]

     This is useful to catch potential issues with de-allocation of [c2],
     as we deallocate the old LCC when a new LCC is cemented.
  *)
  let test_cement_n_commitments () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker =
      originate_rollup_and_deposit_with_one_staker ()
    in
    let commitments = commitments ~predecessor:genesis_hash ctxt 10 in
    let*@ ctxt = publish_commitments ctxt rollup staker commitments in
    let*@ ctxt =
      advance_level_for_cement
        ctxt
        rollup
        (List.rev commitments |> Stdlib.List.hd)
    in
    let*@ _ctxt, cemented_commitments =
      List.fold_left_es
        (fun (ctxt, acc) _commitment ->
          let* ctxt, cemented_commitment, _cemented_commitment_hash =
            Sc_rollup_stake_storage.cement_commitment ctxt rollup
          in
          return (ctxt, cemented_commitment :: acc))
        (ctxt, [])
        commitments
    in
    let* () =
      List.iter2_es
        ~when_different_lengths:[]
        (fun commitment cemented_commitment ->
          assert_commitment_equal ~loc:__LOC__ commitment cemented_commitment)
        commitments
        (List.rev cemented_commitments)
    in
    return_unit

  (** Test that a number of commitments are saved in the storage after
      cementation, and other are completely removed.  Note that the
      metadata for both saved and removed commitments are removed. *)
  let test_cement_clean_commitments () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker =
      originate_rollup_and_deposit_with_one_staker ()
    in
    let max_number_of_stored_commitments =
      Constants_storage.max_number_of_stored_cemented_commitments ctxt
    in
    let commitments =
      commitments
        ~predecessor:genesis_hash
        ctxt
        (max_number_of_stored_commitments * 2)
    in
    let*@ ctxt = publish_commitments ctxt rollup staker commitments in
    let*@ ctxt = cement_commitments ctxt rollup commitments in
    (* Assert that [max_number_of_stored_commitments] cemented commitments
       exists, i.e. the commitment and not its metadata. The rest are cleaned. *)
    let* () =
      List.iter_es
        (assert_commitment_metadata_dont_exists ctxt rollup)
        commitments
    in
    let removed_commitments, saved_commitments =
      List.split_n max_number_of_stored_commitments commitments
    in
    let* () =
      List.iter_es
        (assert_commitment_dont_exists ctxt rollup)
        removed_commitments
    in
    let* () =
      List.iter_es
        (fun commitment -> assert_commitment_exists ctxt rollup commitment)
        saved_commitments
    in
    return_unit

  let test_cement_conflicted_branches () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker1, staker2, staker3 =
      originate_rollup_and_deposit_with_three_stakers ()
    in

    (* Create an honest and dishonest branch. *)
    let honest_commitment, honest_commitment_hash =
      let compressed_state =
        Sc_rollup_repr.State_hash.context_hash_to_state_hash
          (Context_hash.hash_string ["honest"])
      in
      commitment
        ~predecessor:genesis_hash
        ~compressed_state
        ~inbox_level:(valid_inbox_level ctxt 1l)
        ()
    in
    let honest_commitments =
      honest_commitment
      :: commitments ~predecessor:honest_commitment_hash ~start_at:2l ctxt 10
    in
    let dishonest_commitment, dishonest_commitment_hash =
      let compressed_state =
        Sc_rollup_repr.State_hash.context_hash_to_state_hash
          (Context_hash.hash_string ["dishonest"])
      in
      commitment
        ~predecessor:genesis_hash
        ~compressed_state
        ~inbox_level:(valid_inbox_level ctxt 1l)
        ()
    in
    let dishonest_commitments =
      dishonest_commitment
      :: commitments ~predecessor:dishonest_commitment_hash ~start_at:2l ctxt 10
    in
    (* Conflict begins. *)
    let*@ ctxt = publish_commitments ctxt rollup staker1 honest_commitments in
    let*@ ctxt =
      publish_commitments ctxt rollup staker2 dishonest_commitments
    in
    let*@ ctxt =
      publish_commitments ctxt rollup staker3 dishonest_commitments
    in

    (* No one can cement their branches. *)
    let cant_cement ctxt =
      List.iter_es (fun commitment ->
          assert_fails_with_f
            ~loc:__LOC__
            (cement_commitment ctxt rollup commitment)
            (let open Sc_rollup_errors in
            function
            | Sc_rollup_disputed | Sc_rollup_parent_not_lcc
            | Raw_context.Storage_error (Missing_key _) (* missing commitment *)
              ->
                true
            | _ -> false))
    in
    let* () = cant_cement ctxt honest_commitments in
    let* () = cant_cement ctxt dishonest_commitments in

    (* Simulate a conflict's resolution through [remove_staker]. *)
    let*@ ctxt, _balance_updates =
      Sc_rollup_stake_storage.remove_staker ctxt rollup staker2
    in
    (* [staker1] is not yet able to cement, [staker3] still stake on the
       dishonest branch. *)
    let* () = cant_cement ctxt honest_commitments in
    (* Simulate the second conflict's resolution. *)
    let*@ ctxt, _balance_updates =
      Sc_rollup_stake_storage.remove_staker ctxt rollup staker3
    in
    (* [staker1] can now cement its branch. The dishonest branch can not
       be cemented, before and after the honest branch was cemented. *)
    let*@ _ctxt = cement_commitments ctxt rollup honest_commitments in
    return_unit

  (** {2. Withdraw unit tests.} *)

  (** Test that the entrypoint [withdraw] fails with a nice error if
      the rollup is missing. *)
  let test_withdraw_to_missing_rollup () =
    assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
        Sc_rollup_stake_storage.withdraw_stake ctxt rollup Zero.staker)

  (* Test that withdraw fail if the account has not deposited. *)
  let test_withdraw_when_not_staked () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, account = new_context_1 () in
    let*@ rollup, _genesis_hash, ctxt = new_sc_rollup ctxt in
    assert_fails_with
      ~loc:__LOC__
      (Sc_rollup_stake_storage.withdraw_stake ctxt rollup account)
      Sc_rollup_errors.Sc_rollup_not_staked

  (** Test that you can withdraw only once (you need to deposit again). *)
  let test_withdraw_twice_fails () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, _genesis_hash, staker =
      originate_rollup_and_deposit_with_one_staker ()
    in
    let*@ ctxt, _balance_updates =
      Sc_rollup_stake_storage.withdraw_stake ctxt rollup staker
    in
    assert_fails_with
      ~loc:__LOC__
      (Sc_rollup_stake_storage.withdraw_stake ctxt rollup staker)
      Sc_rollup_errors.Sc_rollup_not_staked

  (** Test that withdraw when the stakers's newest staked commitment is
      after the LCC fails. *)
  let test_withdraw_fails_staked_after_lcc () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker =
      originate_rollup_and_deposit_with_one_staker ()
    in
    let commitment, _hash =
      commitment
        ~predecessor:genesis_hash
        ~inbox_level:(valid_inbox_level ctxt 1l)
        ()
    in
    let*@ ctxt = publish_commitment ctxt rollup staker commitment in
    assert_fails_with
      ~loc:__LOC__
      (Sc_rollup_stake_storage.withdraw_stake ctxt rollup staker)
      Sc_rollup_errors.Sc_rollup_not_staked_on_lcc_or_ancestor

  (** Test that withdraw succeeds when the the staker staked on a branch older
      than the LCC. *)
  let test_withdraw_staked_before_or_at_lcc () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker1, staker2 =
      originate_rollup_and_deposit_with_two_stakers ()
    in
    let commitments = commitments ~predecessor:genesis_hash ctxt 10 in
    let commitment, commitments =
      match commitments with x :: xs -> (x, xs) | _ -> assert false
    in
    let*@ ctxt = publish_commitment ctxt rollup staker1 commitment in
    let*@ ctxt = publish_commitments ctxt rollup staker2 commitments in
    let*@ ctxt = cement_commitment ctxt rollup commitment in
    let*@ ctxt = cement_commitments ctxt rollup commitments in
    let*@ _ctxt = withdraw ctxt rollup staker1 in
    return_unit

  (** Test that [remove_staker] cleans the stakers' metadata. *)
  let test_remove_staker () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, _genesis_hash, staker =
      originate_rollup_and_deposit_with_one_staker ()
    in
    let*@ ctxt, _balance_updates =
      Sc_rollup_stake_storage.remove_staker ctxt rollup staker
    in
    assert_staker_dont_exists ctxt rollup staker

  (** Test that a staker can come back after being slashed. *)
  let test_come_back_after_remove_staker () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker =
      originate_rollup_and_deposit_with_one_staker ()
    in
    let* () = assert_staker_exists ctxt rollup staker in
    let commitment, _hash =
      commitment
        ~predecessor:genesis_hash
        ~inbox_level:(valid_inbox_level ctxt 1l)
        ()
    in
    let*@ ctxt = publish_commitment ctxt rollup staker commitment in
    let* () = assert_commitment_exists ctxt rollup commitment in
    let* () = assert_commitment_metadata_exists ctxt rollup commitment staker in
    (* We simulate a conflict resolution through [remove_staker] *)
    let*@ ctxt, _balance_updates =
      Sc_rollup_stake_storage.remove_staker ctxt rollup staker
    in
    let* () = assert_staker_dont_exists ctxt rollup staker in
    (* The staker can come back through a new index. *)
    let new_commitment =
      {
        commitment with
        compressed_state =
          Sc_rollup_repr.State_hash.context_hash_to_state_hash
            (Context_hash.hash_string ["honest"]);
      }
    in
    let*@ ctxt = publish_commitment ctxt rollup staker new_commitment in
    let* () = assert_commitment_exists ctxt rollup new_commitment in
    let* () =
      assert_commitment_metadata_exists ctxt rollup new_commitment staker
    in
    let* () = assert_staker_exists ctxt rollup staker in
    (* Furthermore, the commitment can be cemented. *)
    let*@ _ctxt = cement_commitment ctxt rollup commitment in
    return_unit

  let assert_balance_unchanged ctxt ctxt' account =
    let open Lwt_result_wrap_syntax in
    let*@ _, balance = Token.balance ctxt account in
    let*@ _, balance' = Token.balance ctxt' account in
    equal_tez ~loc:__LOC__ balance' balance

  let remove_staker_and_check_balances ctxt rollup staker =
    let open Lwt_result_wrap_syntax in
    perform_staking_action_and_check
      ctxt
      rollup
      staker
      (fun ctxt rollup staker_contract stake ->
        let*@ ctxt', _ =
          Sc_rollup_stake_storage.remove_staker ctxt rollup staker
        in
        let* () =
          assert_balance_unchanged ctxt ctxt' (`Contract staker_contract)
        in
        let bond_id = Bond_id_repr.Sc_rollup_bond_id rollup in
        let bonds_account = `Frozen_bonds (staker_contract, bond_id) in
        let+ () = assert_balance_decreased ctxt ctxt' bonds_account stake in
        ctxt')

  let produce_and_refine ctxt ~number_of_commitments ?(start_at_level = 1)
      ~predecessor staker rollup =
    let open Lwt_result_syntax in
    let inbox_level = proper_valid_inbox_level (ctxt, rollup) in
    let rec aux ctxt n l predecessor result =
      if n = 0 then return @@ (List.rev result, ctxt)
      else
        let* inbox_level = inbox_level l in
        let commitment =
          Commitment_repr.
            {
              predecessor;
              inbox_level;
              number_of_ticks = number_of_ticks_exn 1232909L;
              compressed_state = Sc_rollup_repr.State_hash.zero;
            }
        in
        let* c, _level, ctxt =
          advance_level_n_refine_stake ctxt rollup staker commitment
        in
        aux ctxt (n - 1) (l + 1) c (c :: result)
    in
    aux ctxt number_of_commitments start_at_level predecessor []

  let rec cement_commitments ctxt commitments rollup =
    let open Lwt_result_syntax in
    match commitments with
    | [] -> return ctxt
    | _c :: commitments ->
        let* ctxt, _commitment, _commitment_hash =
          Sc_rollup_stake_storage.cement_commitment ctxt rollup
        in
        cement_commitments ctxt commitments rollup

  let test_cement_fail_too_recent () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker =
      originate_rollup_and_deposit_with_one_staker ()
    in
    let level = valid_inbox_level ctxt in
    let challenge_window =
      Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
    in
    let commitment =
      Commitment_repr.
        {
          predecessor = genesis_hash;
          inbox_level = level 1l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ _c1, level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker commitment
    in
    let min_cementation_level = Raw_level_repr.add level challenge_window in
    let* () =
      assert_fails_with
        ~loc:__LOC__
        (Sc_rollup_stake_storage.cement_commitment ctxt rollup)
        (Sc_rollup_errors.Sc_rollup_commitment_too_recent
           {current_level = level; min_level = min_cementation_level})
    in
    let ctxt =
      Raw_context.Internal_for_tests.add_level ctxt (challenge_window - 1)
    in
    let level = (Raw_context.current_level ctxt).level in
    assert_fails_with
      ~loc:__LOC__
      (Sc_rollup_stake_storage.cement_commitment ctxt rollup)
      (Sc_rollup_errors.Sc_rollup_commitment_too_recent
         {current_level = level; min_level = min_cementation_level})

  let test_cement_deadline_uses_oldest_add_time () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker1, staker2 =
      originate_rollup_and_deposit_with_two_stakers ()
    in
    let commitment =
      Commitment_repr.
        {
          predecessor = genesis_hash;
          inbox_level = valid_inbox_level ctxt 1l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ c1, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker1 commitment
    in
    let challenge_window =
      Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
    in
    let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in

    let*@ c2, _level, _ctxt =
      Sc_rollup_stake_storage.Internal_for_tests.refine_stake
        ctxt
        rollup
        staker2
        commitment
    in
    let*@ _ctxt = Sc_rollup_stake_storage.cement_commitment ctxt rollup in
    assert_commitment_hash_equal ~loc:__LOC__ c1 c2

  let test_last_cemented_commitment_hash_with_level () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker =
      originate_rollup_and_deposit_with_one_staker ()
    in
    let challenge_window =
      Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
    in
    let inbox_level = valid_inbox_level ctxt 1l in
    let commitment =
      Commitment_repr.
        {
          predecessor = genesis_hash;
          inbox_level;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ c1, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker commitment
    in
    let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
    let*@ ctxt, _, _ = Sc_rollup_stake_storage.cement_commitment ctxt rollup in
    let*@ c1', inbox_level', _ctxt =
      Sc_rollup_commitment_storage.last_cemented_commitment_hash_with_level
        ctxt
        rollup
    in
    let* () = assert_commitment_hash_equal ~loc:__LOC__ c1 c1' in
    Assert.equal_int32
      ~loc:__LOC__
      (Raw_level_repr.to_int32 inbox_level)
      (Raw_level_repr.to_int32 inbox_level')

  let test_cement_with_two_stakers () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker1, staker2 =
      originate_rollup_and_deposit_with_two_stakers ()
    in
    let level = valid_inbox_level ctxt in
    let commitment1 =
      Commitment_repr.
        {
          predecessor = genesis_hash;
          inbox_level = level 1l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ c1, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker1 commitment1
    in
    let commitment2 =
      Commitment_repr.
        {
          predecessor = c1;
          inbox_level = level 2l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ _node, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker2 commitment2
    in
    let challenge_window =
      Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
    in
    let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in

    let*@ ctxt = Sc_rollup_stake_storage.cement_commitment ctxt rollup in
    assert_true ctxt

  let test_can_remove_staker () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker1, staker2 =
      originate_rollup_and_deposit_with_two_stakers ()
    in
    let level = valid_inbox_level ctxt in
    let commitment1 =
      Commitment_repr.
        {
          predecessor = genesis_hash;
          inbox_level = level 1l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ c1, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker1 commitment1
    in
    let commitment2 =
      Commitment_repr.
        {
          predecessor = c1;
          inbox_level = level 2l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ _node, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker2 commitment2
    in
    let* ctxt = remove_staker_and_check_balances ctxt rollup staker1 in
    let challenge_window =
      Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
    in
    let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
    (* It needs to have a staker on [c1] to cement it, otherwise it's not
       an active commitment. *)
    assert_fails
      ~loc:__LOC__
      (Sc_rollup_stake_storage.cement_commitment ctxt rollup)

  let test_can_remove_staker2 () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker1, staker2 =
      originate_rollup_and_deposit_with_two_stakers ()
    in
    let level = valid_inbox_level ctxt in
    let commitment1 =
      Commitment_repr.
        {
          predecessor = genesis_hash;
          inbox_level = level 1l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ c1, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker1 commitment1
    in
    let commitment2 =
      Commitment_repr.
        {
          predecessor = c1;
          inbox_level = level 2l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ _node, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker2 commitment2
    in
    let* ctxt = remove_staker_and_check_balances ctxt rollup staker2 in
    let challenge_window =
      Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
    in
    let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
    let*@ ctxt = Sc_rollup_stake_storage.cement_commitment ctxt rollup in
    assert_true ctxt

  let test_removed_staker_can_not_withdraw () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker1, staker2 =
      originate_rollup_and_deposit_with_two_stakers ()
    in
    let level = valid_inbox_level ctxt in
    let commitment1 =
      Commitment_repr.
        {
          predecessor = genesis_hash;
          inbox_level = level 1l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ c1, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker1 commitment1
    in
    let commitment2 =
      Commitment_repr.
        {
          predecessor = c1;
          inbox_level = level 2l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ _node, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker2 commitment2
    in
    let*@ ctxt, _ = Sc_rollup_stake_storage.remove_staker ctxt rollup staker2 in
    assert_fails_with
      ~loc:__LOC__
      (Sc_rollup_stake_storage.withdraw_stake ctxt rollup staker2)
      Sc_rollup_errors.Sc_rollup_not_staked

  let test_no_cement_on_conflict () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker1, staker2 =
      originate_rollup_and_deposit_with_two_stakers ()
    in
    let level = valid_inbox_level ctxt in
    let commitment1 =
      Commitment_repr.
        {
          predecessor = genesis_hash;
          inbox_level = level 1l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ _c1, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker1 commitment1
    in
    let commitment2 =
      Commitment_repr.
        {
          predecessor = genesis_hash;
          inbox_level = level 1l;
          number_of_ticks = number_of_ticks_exn 44L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ _node, _level, ctxt =
      Sc_rollup_stake_storage.Internal_for_tests.refine_stake
        ctxt
        rollup
        staker2
        commitment2
    in
    let challenge_window =
      Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
    in
    let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
    assert_fails_with
      ~loc:__LOC__
      (Sc_rollup_stake_storage.cement_commitment ctxt rollup)
      Sc_rollup_errors.Sc_rollup_disputed

  let test_finds_conflict_point_at_lcc () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker1, staker2 =
      originate_rollup_and_deposit_with_two_stakers ()
    in
    let level = valid_inbox_level ctxt in
    let commitment1 =
      Commitment_repr.
        {
          predecessor = genesis_hash;
          inbox_level = level 1l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ c1, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker1 commitment1
    in
    let commitment2 =
      Commitment_repr.
        {
          predecessor = genesis_hash;
          inbox_level = level 1l;
          number_of_ticks = number_of_ticks_exn 55L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ _c2, _level, ctxt =
      Sc_rollup_stake_storage.Internal_for_tests.refine_stake
        ctxt
        rollup
        staker2
        commitment2
    in
    let*@ (left, _right), _ctxt =
      Sc_rollup_refutation_storage.Internal_for_tests.get_conflict_point
        ctxt
        rollup
        staker1
        staker2
    in
    assert_commitment_hash_equal ~loc:__LOC__ left.hash c1

  let test_finds_conflict_point_beneath_lcc () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker1, staker2 =
      originate_rollup_and_deposit_with_two_stakers ()
    in
    let level = valid_inbox_level ctxt in
    let commitment1 =
      Commitment_repr.
        {
          predecessor = genesis_hash;
          inbox_level = level 1l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ c1, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker1 commitment1
    in
    let commitment2 =
      Commitment_repr.
        {
          predecessor = c1;
          inbox_level = level 2l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ c2, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker1 commitment2
    in
    let commitment3 =
      Commitment_repr.
        {
          predecessor = c1;
          inbox_level = level 2l;
          number_of_ticks = number_of_ticks_exn 7373L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ c3, _level, ctxt =
      Sc_rollup_stake_storage.Internal_for_tests.refine_stake
        ctxt
        rollup
        staker2
        commitment3
    in
    let*@ (left, right), _ctxt =
      Sc_rollup_refutation_storage.Internal_for_tests.get_conflict_point
        ctxt
        rollup
        staker1
        staker2
    in
    let* () = assert_commitment_hash_equal ~loc:__LOC__ left.hash c2 in
    assert_commitment_hash_equal ~loc:__LOC__ right.hash c3

  let test_conflict_point_is_first_point_of_disagreement () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker1, staker2 =
      originate_rollup_and_deposit_with_two_stakers ()
    in
    let level = valid_inbox_level ctxt in
    let commitment1 =
      Commitment_repr.
        {
          predecessor = genesis_hash;
          inbox_level = level 1l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ c1, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker1 commitment1
    in
    let commitment2 =
      Commitment_repr.
        {
          predecessor = c1;
          inbox_level = level 2l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ c2, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker1 commitment2
    in
    let commitment3 =
      Commitment_repr.
        {
          predecessor = c1;
          inbox_level = level 2l;
          number_of_ticks = number_of_ticks_exn 7373L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ c3, _level, ctxt =
      Sc_rollup_stake_storage.Internal_for_tests.refine_stake
        ctxt
        rollup
        staker2
        commitment3
    in
    let commitment4 =
      Commitment_repr.
        {
          predecessor = c2;
          inbox_level = level 3l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ _c4, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker1 commitment4
    in
    let*@ (left, right), _ctxt =
      Sc_rollup_refutation_storage.Internal_for_tests.get_conflict_point
        ctxt
        rollup
        staker1
        staker2
    in
    let* () = assert_commitment_hash_equal ~loc:__LOC__ left.hash c2 in
    assert_commitment_hash_equal ~loc:__LOC__ right.hash c3

  let test_conflict_point_computation_fits_in_gas_limit () =
    let open Lwt_result_wrap_syntax in
    (* Worst case of conflict point computation: two branches of maximum
       length rooted just after the LCC. *)
    let* ctxt, rollup, genesis_hash, staker1, staker2 =
      originate_rollup_and_deposit_with_two_stakers ()
    in
    let level = valid_inbox_level ctxt in
    let max_commits =
      let commitment_freq =
        Constants_storage.sc_rollup_commitment_period_in_blocks ctxt
      in
      Int32.div
        (Constants_storage.sc_rollup_max_lookahead_in_blocks ctxt)
        (Int32.of_int commitment_freq)
    in
    let root_commitment =
      Commitment_repr.
        {
          predecessor = genesis_hash;
          inbox_level = level 1l;
          number_of_ticks = number_of_ticks_exn 1L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ root_commitment_hash, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker1 root_commitment
    in
    let*@ _node, _level, ctxt =
      Sc_rollup_stake_storage.Internal_for_tests.refine_stake
        ctxt
        rollup
        staker2
        root_commitment
    in
    let rec branch ctxt staker_id predecessor i max acc =
      let open Result_syntax in
      let commitment =
        Commitment_repr.
          {
            predecessor;
            inbox_level = level i;
            number_of_ticks = number_of_ticks_exn staker_id;
            compressed_state = Sc_rollup_repr.State_hash.zero;
          }
      in
      let* ctxt, commitment_hash =
        Sc_rollup_commitment_storage.hash ctxt commitment
      in
      if i = max then
        return (List.rev ((commitment, commitment_hash) :: acc), ctxt)
      else
        branch
          ctxt
          staker_id
          commitment_hash
          (Int32.succ i)
          max
          ((commitment, commitment_hash) :: acc)
    in
    let*?@ branch_1, ctxt =
      branch ctxt 1L root_commitment_hash 2l max_commits []
    in
    let*?@ branch_2, ctxt =
      branch ctxt 2L root_commitment_hash 2l max_commits []
    in
    let both_branches = List.combine_drop branch_1 branch_2 in
    let*@ ctxt =
      List.fold_left_es
        (fun ctxt ((c1, _c1h), (c2, _c2h)) ->
          let* _ch, _level, ctxt =
            advance_level_n_refine_stake ctxt rollup staker1 c1
          in
          let+ _ch, _level, ctxt =
            advance_level_n_refine_stake ctxt rollup staker2 c2
          in
          ctxt)
        ctxt
        both_branches
    in
    let ctxt =
      Raw_context.set_gas_limit
        ctxt
        (Constants_storage.hard_gas_limit_per_operation ctxt)
    in
    let*@ (left, right), _ctxt =
      Sc_rollup_refutation_storage.Internal_for_tests.get_conflict_point
        ctxt
        rollup
        staker1
        staker2
    in
    let head_hash branch =
      match List.hd branch with Some x -> snd x | None -> assert false
    in
    let* () =
      assert_commitment_hash_equal ~loc:__LOC__ left.hash (head_hash branch_1)
    in
    assert_commitment_hash_equal ~loc:__LOC__ right.hash (head_hash branch_2)

  let test_no_conflict_point_one_staker_at_lcc_preboot () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker1, staker2 =
      originate_rollup_and_deposit_with_two_stakers ()
    in
    let commitment =
      Commitment_repr.
        {
          predecessor = genesis_hash;
          inbox_level = valid_inbox_level ctxt 1l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ _, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker1 commitment
    in
    assert_fails_with
      ~loc:__LOC__
      (Sc_rollup_refutation_storage.Internal_for_tests.get_conflict_point
         ctxt
         rollup
         staker1
         staker2)
      Sc_rollup_errors.Sc_rollup_no_conflict

  let test_no_conflict_point_both_stakers_at_lcc_preboot () =
    let open Lwt_result_syntax in
    let* ctxt, rollup, _genesis_hash, staker1, staker2 =
      originate_rollup_and_deposit_with_two_stakers ()
    in
    assert_fails_with
      ~loc:__LOC__
      (Sc_rollup_refutation_storage.Internal_for_tests.get_conflict_point
         ctxt
         rollup
         staker1
         staker2)
      Sc_rollup_errors.Sc_rollup_no_conflict

  let test_no_conflict_point_one_staker_at_lcc () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker1, staker2 =
      originate_rollup_and_deposit_with_two_stakers ()
    in
    let level = valid_inbox_level ctxt in
    let commitment1 =
      Commitment_repr.
        {
          predecessor = genesis_hash;
          inbox_level = level 1l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ c1, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker1 commitment1
    in
    let commitment2 =
      Commitment_repr.
        {
          predecessor = c1;
          inbox_level = level 2l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ _node, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker2 commitment2
    in
    let challenge_window =
      Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
    in
    let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
    let*@ ctxt, _, _ = Sc_rollup_stake_storage.cement_commitment ctxt rollup in
    assert_fails_with
      ~loc:__LOC__
      (Sc_rollup_refutation_storage.Internal_for_tests.get_conflict_point
         ctxt
         rollup
         staker1
         staker2)
      Sc_rollup_errors.Sc_rollup_no_conflict

  let test_no_conflict_point_both_stakers_at_lcc () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker1, staker2 =
      originate_rollup_and_deposit_with_two_stakers ()
    in
    let commitment1 =
      Commitment_repr.
        {
          predecessor = genesis_hash;
          inbox_level = valid_inbox_level ctxt 1l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ _c1, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker1 commitment1
    in
    let*@ _node, _level, ctxt =
      Sc_rollup_stake_storage.Internal_for_tests.refine_stake
        ctxt
        rollup
        staker2
        commitment1
    in
    let challenge_window =
      Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
    in
    let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
    let*@ ctxt, _, _ = Sc_rollup_stake_storage.cement_commitment ctxt rollup in
    assert_fails_with
      ~loc:__LOC__
      (Sc_rollup_refutation_storage.Internal_for_tests.get_conflict_point
         ctxt
         rollup
         staker1
         staker2)
      Sc_rollup_errors.Sc_rollup_no_conflict

  let test_last_cemented_commitment_of_missing_rollup () =
    assert_fails_with_missing_rollup
      ~loc:__LOC__
      Sc_rollup_commitment_storage.last_cemented_commitment

  let test_last_cemented_commitment_hash_with_level_of_missing_rollup () =
    assert_fails_with_missing_rollup
      ~loc:__LOC__
      Sc_rollup_commitment_storage.last_cemented_commitment_hash_with_level

  let test_get_commitment_of_missing_rollup () =
    assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
        Sc_rollup_commitment_storage.get_commitment
          ctxt
          rollup
          Commitment_repr.Hash.zero)

  let test_get_missing_commitment () =
    let open Lwt_result_wrap_syntax in
    let* ctxt = new_context () in
    let*@ rollup, _genesis_hash, ctxt = new_sc_rollup ctxt in
    let commitment_hash = Commitment_repr.Hash.zero in
    assert_fails_with
      ~loc:__LOC__
      (Sc_rollup_commitment_storage.get_commitment ctxt rollup commitment_hash)
      (Sc_rollup_errors.Sc_rollup_unknown_commitment commitment_hash)

  let test_genesis_info_of_missing_rollup () =
    assert_fails_with_missing_rollup ~loc:__LOC__ Sc_rollup_storage.genesis_info

  let test_concurrent_refinement_point_of_conflict () =
    let open Lwt_result_wrap_syntax in
    let* before_ctxt, rollup, genesis_hash, staker1, staker2 =
      originate_rollup_and_deposit_with_two_stakers ()
    in
    let level = valid_inbox_level before_ctxt in
    let commitment1 =
      Commitment_repr.
        {
          predecessor = genesis_hash;
          inbox_level = level 1l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let commitment2 =
      Commitment_repr.
        {
          predecessor = genesis_hash;
          inbox_level = level 1l;
          number_of_ticks = number_of_ticks_exn 7373L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ (c1, c2), _ctxt =
      let* _c1, _level, ctxt =
        advance_level_n_refine_stake before_ctxt rollup staker1 commitment1
      in
      let* _c2, _level, ctxt =
        advance_level_n_refine_stake ctxt rollup staker2 commitment2
      in
      Sc_rollup_refutation_storage.Internal_for_tests.get_conflict_point
        ctxt
        rollup
        staker1
        staker2
    in
    let*@ (c1', c2'), _ctxt =
      let* _c2, _level, ctxt =
        advance_level_n_refine_stake before_ctxt rollup staker2 commitment2
      in
      let* _c1, _level, ctxt =
        advance_level_n_refine_stake ctxt rollup staker1 commitment1
      in
      Sc_rollup_refutation_storage.Internal_for_tests.get_conflict_point
        ctxt
        rollup
        staker1
        staker2
    in
    let* () = assert_commitment_hash_equal ~loc:__LOC__ c1.hash c1'.hash in
    assert_commitment_hash_equal ~loc:__LOC__ c2.hash c2'.hash

  let test_concurrent_refinement_cement () =
    let open Lwt_result_wrap_syntax in
    let* before_ctxt, rollup, genesis_hash, staker1, staker2 =
      originate_rollup_and_deposit_with_two_stakers ()
    in
    let commitment =
      Commitment_repr.
        {
          predecessor = genesis_hash;
          inbox_level = valid_inbox_level before_ctxt 1l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ c1, _ctxt =
      let* _c1, _level, ctxt =
        advance_level_n_refine_stake before_ctxt rollup staker1 commitment
      in
      let* _c2, _level, ctxt =
        Sc_rollup_stake_storage.Internal_for_tests.refine_stake
          ctxt
          rollup
          staker2
          commitment
      in
      let challenge_window =
        Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
      in
      let ctxt =
        Raw_context.Internal_for_tests.add_level ctxt challenge_window
      in
      let* ctxt, _, _ = Sc_rollup_stake_storage.cement_commitment ctxt rollup in
      Sc_rollup_commitment_storage.last_cemented_commitment ctxt rollup
    in
    let*@ c2, _ctxt =
      let* _c2, _level, ctxt =
        advance_level_n_refine_stake before_ctxt rollup staker2 commitment
      in
      let* _c1, _level, ctxt =
        Sc_rollup_stake_storage.Internal_for_tests.refine_stake
          ctxt
          rollup
          staker1
          commitment
      in
      let challenge_window =
        Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
      in
      let ctxt =
        Raw_context.Internal_for_tests.add_level ctxt challenge_window
      in
      let* ctxt, _, _ = Sc_rollup_stake_storage.cement_commitment ctxt rollup in
      Sc_rollup_commitment_storage.last_cemented_commitment ctxt rollup
    in
    assert_commitment_hash_equal ~loc:__LOC__ c1 c2

  let record ctxt rollup level message_index =
    Sc_rollup_outbox_storage.record_applied_message
      ctxt
      rollup
      (Raw_level_repr.of_int32_exn @@ Int32.of_int level)
      ~message_index

  (* Recreating the indexing logic to make sure messages are applied. *)
  let assert_is_already_applied ~loc ctxt rollup level message_index =
    let open Lwt_result_wrap_syntax in
    let level = Raw_level_repr.of_int32_exn (Int32.of_int level) in
    let level_index =
      let max_active_levels =
        Constants_storage.sc_rollup_max_active_outbox_levels ctxt
      in
      Int32.rem (Raw_level_repr.to_int32 level) max_active_levels
    in
    let*@ _ctxt, level_and_bitset_opt =
      Storage.Sc_rollup.Applied_outbox_messages.find (ctxt, rollup) level_index
    in
    match level_and_bitset_opt with
    | Some (existing_level, bitset) when Raw_level_repr.(existing_level = level)
      ->
        let*?@ is_set = Bitset.mem bitset message_index in
        Assert.equal_bool ~loc is_set true
    | _ -> Stdlib.failwith "Expected a bitset and a matching level."

  (** Test outbox for applied messages. *)
  let test_storage_outbox () =
    let open Lwt_result_wrap_syntax in
    let* ctxt = new_context () in
    let*@ rollup1, _genesis_hash, ctxt = new_sc_rollup ctxt in
    let level1 = 100 in
    (* Test that is-applied is false for non-recorded messages. *)
    let*@ _size_diff, ctxt = record ctxt rollup1 level1 1 in
    let* () = assert_is_already_applied ~loc:__LOC__ ctxt rollup1 level1 1 in
    (* Record the same level and message twice should fail. *)
    let* () =
      assert_fails_with
        ~loc:__LOC__
        (record ctxt rollup1 level1 1)
        Sc_rollup_errors.Sc_rollup_outbox_message_already_applied
    in
    let*@ _size_diff, ctxt = record ctxt rollup1 level1 2 in
    let* () = assert_is_already_applied ~loc:__LOC__ ctxt rollup1 level1 2 in
    (* Record messages for new level. *)
    let level2 = level1 + 3 in
    let*@ _size_diff, ctxt = record ctxt rollup1 level2 47 in
    let* () = assert_is_already_applied ~loc:__LOC__ ctxt rollup1 level2 47 in
    let* () = assert_is_already_applied ~loc:__LOC__ ctxt rollup1 level1 1 in
    (* Record for a new rollup. *)
    let*@ rollup2, _genesis_hash, ctxt = new_sc_rollup ctxt in
    let*@ _size_diff, ctxt = record ctxt rollup2 level1 1 in
    let*@ _size_diff, ctxt = record ctxt rollup2 level1 3 in
    let* () = assert_is_already_applied ~loc:__LOC__ ctxt rollup2 level1 1 in
    let* () = assert_is_already_applied ~loc:__LOC__ ctxt rollup2 level1 3 in
    assert_is_already_applied ~loc:__LOC__ ctxt rollup1 level1 1

  (** Test limits for applied outbox messages. *)
  let test_storage_outbox_exceed_limits () =
    let open Lwt_result_wrap_syntax in
    let level = 1234 in
    let* ctxt = new_context () in
    let*@ rollup, _genesis_hash, ctxt = new_sc_rollup ctxt in
    (* Assert that recording a message index that exceeds max outbox messages per
       level fails. *)
    let* () =
      let max_message_index =
        Constants_storage.sc_rollup_max_outbox_messages_per_level ctxt
      in
      assert_fails_with
        ~loc:__LOC__
        (record ctxt rollup level max_message_index)
        Sc_rollup_errors.Sc_rollup_invalid_outbox_message_index
    in
    let* () =
      assert_fails_with
        ~loc:__LOC__
        (record ctxt rollup level (-1))
        Sc_rollup_errors.Sc_rollup_invalid_outbox_message_index
    in
    let max_active_levels =
      Int32.to_int @@ Constants_storage.sc_rollup_max_active_outbox_levels ctxt
    in
    (* Record message 42 at level 15 *)
    let*@ _size_diff, ctxt = record ctxt rollup 15 42 in
    let* () = assert_is_already_applied ~loc:__LOC__ ctxt rollup 15 42 in
    (* Record message 42 at level [max_active_levels + 15] *)
    let*@ _size_diff, ctxt = record ctxt rollup (max_active_levels + 15) 42 in
    (* Record message 42 at level 15 again should fail as it's expired. *)
    let* () =
      assert_fails_with
        ~loc:__LOC__
        (record ctxt rollup 15 42)
        Sc_rollup_errors.Sc_rollup_outbox_level_expired
    in
    return ()

  (** Test storage outbox size diff. Note that these tests depend on the constant.
    [sc_rollup_max_outbox_messages_per_level] which controls the maximum size
    of bitsets required to store applied messages per level.

    Here's a breakdown of the size for applied-outbox-messages storage:
    - [size_of_level = 4]
    - [max_size_per_level < (2 * (sc_rollup_max_outbox_messages_per_level / 8))]
    - [max_size_per_level < size_of_level + size_of_bitset]
    - [total_size < sc_rollup_max_active_outbox_levels * max_size_per_level]
  *)
  let test_storage_outbox_size_diff () =
    let open Lwt_result_wrap_syntax in
    (* This is the maximum additional storage space required to store one message.
       It depends on [sc_rollup_max_outbox_messages_per_level]. *)
    let max_size_diff = 19 in
    let* ctxt = new_context () in
    let*@ rollup, _genesis_hash, ctxt = new_sc_rollup ctxt in
    let level = 15 in
    let max_message_index =
      Constants_storage.sc_rollup_max_outbox_messages_per_level ctxt - 1
    in
    let max_active_levels =
      Int32.to_int @@ Constants_storage.sc_rollup_max_active_outbox_levels ctxt
    in
    (* Record a new message. *)
    let*@ size_diff, ctxt = record ctxt rollup level 1 in
    (* Size diff is 11 bytes. 4 bytes for level and 7 bytes for a new Z.t *)
    let* () = Assert.equal_int ~loc:__LOC__ (Z.to_int size_diff) 5 in
    let*@ size_diff, ctxt = record ctxt rollup level 2 in
    (* Recording a new message in the bitset at a lower index does not occupy
       any additional space. *)
    let* () = Assert.equal_int ~loc:__LOC__ (Z.to_int size_diff) 0 in
    (* Record a new message at the highest index at an existing level. This
       expands the bitset but does not charge for the level. *)
    let*@ size_diff, ctxt = record ctxt rollup level max_message_index in
    let* () = Assert.equal_int ~loc:__LOC__ (Z.to_int size_diff) 14 in
    (* Record a new message at the highest index at a new level. This charges for
       space for level and maximum bitset. *)
    let*@ size_diff, ctxt = record ctxt rollup (level + 1) max_message_index in
    let* () =
      Assert.equal_int ~loc:__LOC__ (Z.to_int size_diff) max_size_diff
    in
    (* Record a new message for a level that resets an index. This replaces the
       bitset with a smaller one. Hence we get a negative size diff. *)
    let*@ size_diff, _ctxt = record ctxt rollup (level + max_active_levels) 0 in
    let* () = Assert.equal_int ~loc:__LOC__ (Z.to_int size_diff) (-14) in
    return ()

  let test_get_cemented_commitments_with_levels_of_missing_rollup () =
    assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
        Sc_rollup_commitment_storage.Internal_for_tests
        .get_cemented_commitments_with_levels
          ctxt
          rollup)

  let test_get_cemented_commitments_with_levels () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, c0, staker =
      originate_rollup_and_deposit_with_one_staker ()
    in
    let level = valid_inbox_level ctxt in
    let challenge_window =
      Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
    in
    (* Produce and stake on n commitments, each on top of the other. *)
    (* Fetch number of stored commitments in context. *)
    let max_num_stored_cemented_commitments =
      (Raw_context.constants ctxt).sc_rollup
        .max_number_of_stored_cemented_commitments
    in
    (* Produce and stake more commitments than the number of cemented
       commitments that can be stored. *)
    let number_of_commitments = max_num_stored_cemented_commitments + 5 in
    let*@ commitments, ctxt =
      produce_and_refine
        ~number_of_commitments
        ~predecessor:c0
        ctxt
        staker
        rollup
    in
    let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
    (* Cement all commitments that have been produced. *)
    let*@ ctxt = cement_commitments ctxt commitments rollup in
    (* Add genesis commitment to list of produced commitments. *)
    let commitments = c0 :: commitments in
    let number_of_cemented_commitments = List.length commitments in
    (* Fetch cemented commitments that are kept in context. *)
    let*@ cemented_commitments_with_levels, _ctxt =
      Sc_rollup_commitment_storage.Internal_for_tests
      .get_cemented_commitments_with_levels
        ctxt
        rollup
    in
    (* Check that only ctxt.sc_rollup.max_number_of_stored_cemented_commitments
       are kept in context. *)
    let* () =
      Assert.equal_int
        ~loc:__LOC__
        (List.length cemented_commitments_with_levels)
        max_num_stored_cemented_commitments
    in
    (* Check that the commitments that are kept in context are the
       last [ctxt.sc_rollup.max_number_of_stored_cemented_commitments].
       commitments that have been cemented. *)
    let dropped_commitments =
      number_of_cemented_commitments - max_num_stored_cemented_commitments
    in
    let expected_commitments_with_levels =
      commitments
      |> List.drop_n dropped_commitments
      |> List.mapi (fun i c ->
             (c, level @@ Int32.of_int (i + dropped_commitments)))
    in
    assert_commitments_with_levels_equal
      ~loc:__LOC__
      cemented_commitments_with_levels
      expected_commitments_with_levels

  (* Produces [max_num_stored_cemented_commitments] number of commitments and
     verifies that each of them is an ancestor of the last cemented commitment. *)
  let test_are_commitments_related_when_related () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, c0, staker =
      originate_rollup_and_deposit_with_one_staker ()
    in
    let challenge_window =
      Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
    in
    (* Produce and stake on n commitments, each on top of the other. *)
    (* Fetch number of stored commitments in context. *)
    let max_num_stored_cemented_commitments =
      (Raw_context.constants ctxt).sc_rollup
        .max_number_of_stored_cemented_commitments
    in
    (* Produce and store a number of commitments equal to the maximum number of
       cemented commitments that can be stored. *)
    let number_of_commitments = max_num_stored_cemented_commitments in
    let*@ commitments, ctxt =
      produce_and_refine
        ~number_of_commitments
        ~predecessor:c0
        ctxt
        staker
        rollup
    in
    let ctxt = Raw_context.Internal_for_tests.add_level ctxt challenge_window in
    (* Cement all commitments that have been produced. *)
    let*@ ctxt = cement_commitments ctxt commitments rollup in
    (* Check that check_if_commitments_are_related detects that each
       cemented commitment is an ancestor of the last cemented commitment. *)
    let*@ lcc, ctxt =
      Sc_rollup_commitment_storage.last_cemented_commitment ctxt rollup
    in
    commitments
    |> List.iter_es (fun commitment ->
           let*@ is_commitment_cemented, _ctxt =
             Sc_rollup_commitment_storage.check_if_commitments_are_related
               ctxt
               rollup
               ~descendant:lcc
               ~ancestor:commitment
           in
           Assert.equal_bool ~loc:__LOC__ is_commitment_cemented true)

  (** Tests that [check_if_commitments_are_related] returns false for two
    unrelated commitments. *)
  let test_unrelated_commitments () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash, staker1, staker2 =
      originate_rollup_and_deposit_with_two_stakers ()
    in
    let level = valid_inbox_level ctxt in
    let commitment1 =
      Commitment_repr.
        {
          predecessor = genesis_hash;
          inbox_level = level 1l;
          number_of_ticks = number_of_ticks_exn 1232909L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ c1, _level, ctxt =
      advance_level_n_refine_stake ctxt rollup staker1 commitment1
    in
    let commitment2 =
      Commitment_repr.
        {
          predecessor = genesis_hash;
          inbox_level = level 1l;
          number_of_ticks = number_of_ticks_exn 44L;
          compressed_state = Sc_rollup_repr.State_hash.zero;
        }
    in
    let*@ c2, _level, ctxt =
      Sc_rollup_stake_storage.Internal_for_tests.refine_stake
        ctxt
        rollup
        staker2
        commitment2
    in
    let*@ are_commitments_related, _ctxt =
      Sc_rollup_commitment_storage.check_if_commitments_are_related
        ctxt
        rollup
        ~descendant:c1
        ~ancestor:c2
    in
    Assert.equal_bool ~loc:__LOC__ are_commitments_related false

  let test_fresh_index_correctly_increment () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, _genesis_hash, staker1, staker2 =
      originate_rollup_and_deposit_with_two_stakers ()
    in
    let assert_staker_index ~__LOC__ ctxt staker expected_index =
      let*@ _, (found_index : Sc_rollup_staker_index_repr.t) =
        Storage.Sc_rollup.Staker_index.get (ctxt, rollup) staker
      in
      Assert.equal_z ~loc:__LOC__ (found_index :> Z.t) expected_index
    in
    let* () = assert_staker_index ~__LOC__ ctxt staker1 Z.zero in
    let* () = assert_staker_index ~__LOC__ ctxt staker2 Z.one in
    let Account.{pkh; _} = Account.new_account () in
    let*@ ctxt, fresh_staker3_index =
      Sc_rollup_staker_index_storage.fresh_staker_index ctxt rollup pkh
    in
    let* () = assert_staker_index ~__LOC__ ctxt pkh Z.(succ one) in
    Assert.equal_z ~loc:__LOC__ (fresh_staker3_index :> Z.t) Z.(succ one)

  let tests =
    [
      (* Deposit tests: *)
      Tztest.tztest "deposit" `Quick test_deposit;
      Tztest.tztest
        "deposit by underfunded staker"
        `Quick
        test_deposit_by_underfunded_staker;
      Tztest.tztest "deposit then withdraw" `Quick test_deposit_then_withdraw;
      Tztest.tztest "deposit on two rollups" `Quick test_deposit_on_two_rollups;
      Tztest.tztest "deposit twice fails" `Quick test_deposit_twice_fails;
      (* Publish tests: *)
      Tztest.tztest "publish" `Quick test_publish;
      Tztest.tztest "publish twice" `Quick test_publish_twice;
      Tztest.tztest
        "publish to missing rollup fails"
        `Quick
        test_publish_to_missing_rollup;
      Tztest.tztest
        "publish to wrong inbox level"
        `Quick
        test_publish_wrong_inbox_level;
      Tztest.tztest
        "publish existing commitment"
        `Quick
        test_publish_existing_commitment;
      Tztest.tztest
        "publish commitment returns level when commitment was first published"
        `Quick
        test_publish_returns_oldest_publish_level;
      Tztest.tztest
        "publish from the future is not allowed"
        `Quick
        test_publish_fails_on_commitment_from_future;
      Tztest.tztest
        "publish from behind lcc is allowed"
        `Quick
        test_publish_from_behind_lcc;
      Tztest.tztest
        "publish to any commitment of a branch"
        `Quick
        test_publish_anywhere_on_branch;
      Tztest.tztest
        "publish needs a predecessor"
        `Quick
        test_publish_without_predecessor_fails;
      Tztest.tztest
        "publish behind or at LCC"
        `Quick
        test_publish_behind_or_at_lcc_fails;
      (* Cement tests: *)
      Tztest.tztest "cement" `Quick test_cement;
      Tztest.tztest
        "cement to missing rollup fails"
        `Quick
        test_cement_to_missing_rollup;
      Tztest.tztest "cement with n stakers" `Quick test_cement_with_n_stakers;
      Tztest.tztest "cement n commitments" `Quick test_cement_n_commitments;
      Tztest.tztest
        "cement clean commitment(s)"
        `Quick
        test_cement_clean_commitments;
      Tztest.tztest
        "cement conflicted branches"
        `Quick
        test_cement_conflicted_branches;
      (* Withdraw tests: *)
      Tztest.tztest
        "withdraw to missing rollup fails"
        `Quick
        test_withdraw_to_missing_rollup;
      Tztest.tztest
        "withdraw when not staked fails"
        `Quick
        test_withdraw_when_not_staked;
      Tztest.tztest "withdraw twice fails" `Quick test_withdraw_twice_fails;
      Tztest.tztest
        "withdraw fails when staked after LCC"
        `Quick
        test_withdraw_fails_staked_after_lcc;
      Tztest.tztest
        "withdraw when staked before or at LCC"
        `Quick
        test_withdraw_staked_before_or_at_lcc;
      (* Remove staker tests: *)
      Tztest.tztest "remove staker" `Quick test_remove_staker;
      (* Misc tests: *)
      Tztest.tztest
        "staker come back after being slashed"
        `Quick
        test_come_back_after_remove_staker;
    ]

  (* These tests were written for the old mechanism of staking (calling it
     the old design would be a too strong statement).
     They need to polished if necessary.
  *)
  let tests =
    tests
    @ [
        Tztest.tztest
          "cement fails when too recent"
          `Quick
          test_cement_fail_too_recent;
        Tztest.tztest
          "cement deadline uses oldest add time"
          `Quick
          test_cement_deadline_uses_oldest_add_time;
        Tztest.tztest
          "last cemented commitment hash and level returns correct information"
          `Quick
          test_last_cemented_commitment_hash_with_level;
        Tztest.tztest
          "cement with two stakers"
          `Quick
          test_cement_with_two_stakers;
        Tztest.tztest "no cement on conflict" `Quick test_no_cement_on_conflict;
        Tztest.tztest
          "finds conflict point at LCC"
          `Quick
          test_finds_conflict_point_at_lcc;
        Tztest.tztest
          "finds conflict point beneath LCC"
          `Quick
          test_finds_conflict_point_beneath_lcc;
        Tztest.tztest
          "finds first point of disagreement when as point of conflict"
          `Quick
          test_conflict_point_is_first_point_of_disagreement;
        Tztest.tztest
          "finds no conflict point with two stakers, one of which is at LCC \
           (PVM in preboot)"
          `Quick
          test_no_conflict_point_one_staker_at_lcc_preboot;
        Tztest.tztest
          "finds no conflict point when both stakers commit to LCC (PVM in \
           preboot)"
          `Quick
          test_no_conflict_point_both_stakers_at_lcc_preboot;
        Tztest.tztest
          "finds no conflict point with two stakers, one of which is at LCC"
          `Quick
          test_no_conflict_point_one_staker_at_lcc;
        Tztest.tztest
          "finds no conflict point when both stakers commit to LCC"
          `Quick
          test_no_conflict_point_both_stakers_at_lcc;
        Tztest.tztest
          "test_conflict_point_computation_fits_in_gas_limit"
          `Quick
          test_conflict_point_computation_fits_in_gas_limit;
        Tztest.tztest "can remove staker 1" `Quick test_can_remove_staker;
        Tztest.tztest "can remove staker 2" `Quick test_can_remove_staker2;
        Tztest.tztest
          "removed staker can not withdraw"
          `Quick
          test_removed_staker_can_not_withdraw;
        Tztest.tztest
          "fetching last final commitment of missing rollup fails"
          `Quick
          test_last_cemented_commitment_of_missing_rollup;
        Tztest.tztest
          "fetching last final commitment hash and level of missing rollup \
           fails"
          `Quick
          test_last_cemented_commitment_hash_with_level_of_missing_rollup;
        Tztest.tztest
          "fetching commitment of missing rollup fails"
          `Quick
          test_get_commitment_of_missing_rollup;
        Tztest.tztest
          "fetching non-existing commitment of rollup fails"
          `Quick
          test_get_missing_commitment;
        Tztest.tztest
          "initial level of missing rollup fails"
          `Quick
          test_genesis_info_of_missing_rollup;
        Tztest.tztest
          "Refinement operations are commutative (point of conflict)"
          `Quick
          test_concurrent_refinement_point_of_conflict;
        Tztest.tztest
          "Refinement operations are commutative (cement)"
          `Quick
          test_concurrent_refinement_cement;
        Tztest.tztest
          "Record messages in storage outbox"
          `Quick
          test_storage_outbox;
        Tztest.tztest
          "Record messages in storage outbox limits"
          `Quick
          test_storage_outbox_exceed_limits;
        Tztest.tztest
          "Record messages size diffs"
          `Quick
          test_storage_outbox_size_diff;
        Tztest.tztest
          "Originating a rollup creates a genesis commitment"
          `Quick
          test_last_cemented_commitment_hash_with_level_when_genesis;
        Tztest.tztest
          "Getting cemented commitments with levels of missing rollups fails"
          `Quick
          test_get_cemented_commitments_with_levels_of_missing_rollup;
        Tztest.tztest
          "Getting cemented commitments returns multiple cemented commitments"
          `Quick
          test_get_cemented_commitments_with_levels;
        Tztest.tztest
          "All cemented commitments are ancestors of last cemented commitment"
          `Quick
          test_are_commitments_related_when_related;
        Tztest.tztest
          "Unrelated commitments are classified as such"
          `Quick
          test_unrelated_commitments;
        Tztest.tztest
          "test fresh index is correcly incremented"
          `Quick
          test_fresh_index_correctly_increment;
      ]
end

module Rollup_storage_tests = struct
  let test_genesis_info_of_rollup () =
    let open Lwt_result_wrap_syntax in
    let* ctxt = new_context () in
    let level_before_rollup = (Raw_context.current_level ctxt).level in
    let*@ rollup, _genesis_hash, ctxt = new_sc_rollup ctxt in
    let ctxt = Raw_context.Internal_for_tests.add_level ctxt 10 in
    let*@ _ctxt, genesis_info = Sc_rollup_storage.genesis_info ctxt rollup in
    let initial_level = genesis_info.level in
    Assert.equal_int32
      ~loc:__LOC__
      (Raw_level_repr.to_int32 level_before_rollup)
      (Raw_level_repr.to_int32 initial_level)

  let test_initial_state_is_pre_boot () =
    let open Lwt_result_wrap_syntax in
    let* ctxt, rollup, genesis_hash = new_context_with_rollup () in
    let*@ lcc, _ctxt =
      Sc_rollup_commitment_storage.last_cemented_commitment ctxt rollup
    in
    assert_commitment_hash_equal ~loc:__LOC__ lcc genesis_hash

  let test_kind_of_missing_rollup () =
    assert_fails_with_missing_rollup ~loc:__LOC__ (fun ctxt rollup ->
        Sc_rollup_storage.kind ctxt rollup)

  let tests =
    [
      Tztest.tztest
        "initial_level returns correct level"
        `Quick
        test_genesis_info_of_rollup;
      Tztest.tztest
        "rollup starts in pre-boot state"
        `Quick
        test_initial_state_is_pre_boot;
      Tztest.tztest
        "kind of missing rollup is None"
        `Quick
        test_kind_of_missing_rollup;
    ]
end

let tests = Stake_storage_tests.tests @ Rollup_storage_tests.tests

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/2460
   Further tests to be added.
*)
