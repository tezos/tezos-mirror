(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trilitech <contact@trili.tech>                         *)
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
    Component:  Protocol (baking)
    Invocation: dune exec src/proto_024_PtTALLiN/lib_protocol/test/integration/main.exe \
                 -- --file test_constants.ml
    Subject:    the consistency of parametric constants
 *)

open Tez_helpers

let register_test =
  Tezt_helpers.register_test_es ~__FILE__ ~file_tags:["constants"]

let () =
  register_test ~title:"sc_rollup constants consistency" @@ fun () ->
  let open Protocol.Alpha_context in
  let to_string c =
    Data_encoding.Json.(
      to_string ~minify:true
      @@ construct Constants.Parametric.Internal_for_tests.sc_rollup_encoding c)
  in
  let open Lwt_result_syntax in
  (* We do not necessarily need to update this value when the block time
     changes. The goal is to witness the consistency of the “symbolic”
     computations in [Default_parameters] and [Raw_context].. *)
  let block_time = 10 in
  let quarter_more x = Int32.(div (mul 5l x) 4l) in
  let sc_rollup =
    Default_parameters.Internal_for_tests.make_sc_rollup_parameter
      ~dal_attested_slots_validity_lag:241_920
        (* 4 weeks with a 10 secs block time. *)
      ~dal_activation_level:Raw_level.root
      block_time
  in
  (* Check no update *)
  let sc_rollup' =
    Constants.Parametric.Internal_for_tests.update_sc_rollup_parameter
      Fun.id
      sc_rollup
  in
  let* () =
    Assert.equal
      ~loc:__LOC__
      (fun s1 s2 -> String.equal (to_string s1) (to_string s2))
      "sc_rollup_parameter update"
      (fun fmt sc_rollup -> Format.pp_print_string fmt @@ to_string sc_rollup)
      sc_rollup
      sc_rollup'
  in
  (* Check with update *)
  let sc_rollup_expected_constants =
    Default_parameters.Internal_for_tests.make_sc_rollup_parameter
      ~dal_attested_slots_validity_lag:241_920
        (* 4 weeks with a 10 secs block time. *)
      ~dal_activation_level:Raw_level.root
      8
  in
  let sc_rollup_updated_constants =
    Constants.Parametric.Internal_for_tests.update_sc_rollup_parameter
      quarter_more
      sc_rollup
  in
  Assert.equal
    ~loc:__LOC__
    (fun s1 s2 -> String.equal (to_string s1) (to_string s2))
    "sc_rollup_parameter update"
    (fun fmt sc_rollup -> Format.pp_print_string fmt @@ to_string sc_rollup)
    sc_rollup_expected_constants
    sc_rollup_updated_constants

let () =
  register_test ~title:"constants consistency" @@ fun () ->
  let open Default_parameters in
  List.iter_es
    Block.check_constants_consistency
    [constants_mainnet; constants_sandbox; constants_test]

let () =
  register_test ~title:"max_operations_ttl is 1 hour" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let open Protocol in
  (* We check the rationale that the value for [max_operations_time_to_live] is the following:

     [minimal_time_between_blocks *  max_operations_time_to_live = 3600] *)
  let constants = Default_parameters.constants_mainnet in
  let*?@ result =
    Alpha_context.Period.mult
      (Int32.of_int constants.max_operations_time_to_live)
      constants.minimal_block_delay
  in
  Assert.equal
    ~loc:__LOC__
    (fun x y -> Alpha_context.Period.compare x y = 0)
    "max_operations_ttl"
    Alpha_context.Period.pp
    Alpha_context.Period.one_hour
    result

let cycles_to_period
    ~(constants : Protocol.Alpha_context.Constants.Parametric.t) c =
  let open Lwt_result_wrap_syntax in
  let open Protocol in
  let*?@ one_cycle_period =
    Alpha_context.Period.mult
      constants.blocks_per_cycle
      constants.minimal_block_delay
  in
  let*?@ res = Alpha_context.Period.mult c one_cycle_period in
  return res

type duration = Days of int32 | Hours of int32

let check_protocol_time_correlation
    ~(constants : Protocol.Alpha_context.Constants.Parametric.t) ~cycles
    ~duration =
  let open Lwt_result_wrap_syntax in
  let open Protocol in
  let duration_to_period x =
    let x, period =
      match x with
      | Days x -> (x, Protocol.Alpha_context.Period.one_day)
      | Hours x -> (x, Protocol.Alpha_context.Period.one_hour)
    in
    let*?@ res = Protocol.Alpha_context.Period.mult x period in
    return res
  in
  let* duration = duration_to_period duration in
  let* constant = cycles_to_period ~constants cycles in
  Assert.equal
    ~loc:__LOC__
    (fun x y -> Alpha_context.Period.compare x y = 0)
    "constant in cycles is not equal to given period"
    Alpha_context.Period.pp
    constant
    duration

let () =
  register_test
    ~title:"delay increment per round is greater than or equal to one second"
  @@ fun () ->
  let constants = Default_parameters.constants_mainnet in
  Assert.is_true
    ~loc:__LOC__
    Protocol.Alpha_context.(
      constants.delay_increment_per_round >= Period.one_second)

let () =
  register_test ~title:"one cycle is 1 day" @@ fun () ->
  let constants = Default_parameters.constants_mainnet in
  check_protocol_time_correlation ~constants ~cycles:1l ~duration:(Days 1l)

let () =
  register_test ~title:"voting period is 14 days" @@ fun () ->
  let constants = Default_parameters.constants_mainnet in
  check_protocol_time_correlation
    ~constants
    ~cycles:constants.cycles_per_voting_period
    ~duration:(Days 14l)

let () =
  register_test ~title:"delegate parameters activation delay is 5 days"
  @@ fun () ->
  let constants = Default_parameters.constants_mainnet in
  check_protocol_time_correlation
    ~constants
    ~cycles:(Int32.of_int constants.delegate_parameters_activation_delay)
    ~duration:(Days 5l)

let () =
  register_test ~title:"tolerated inactivity period is 2 days" @@ fun () ->
  let constants = Default_parameters.constants_mainnet in
  check_protocol_time_correlation
    ~constants
    ~cycles:(Int32.of_int constants.tolerated_inactivity_period)
    ~duration:(Days 2l)

let () =
  register_test ~title:"Nonce commitment per cycle is above 128" @@ fun () ->
  let constants = Default_parameters.constants_mainnet in
  Assert.leq
    ~loc:__LOC__
    Int32.compare
    "blocks_per_commitment is too large wrt blocks_per_cycle. Not enough \
     nonces will be produced per cycle."
    (fun ppf -> Format.fprintf ppf "%ld")
    128l
    Int32.(div constants.blocks_per_cycle constants.blocks_per_commitment)

(* For the rational of VDF related constants, see the description of
   [vdf_difficulty] in
   {!val:Tezos_protocol_024_PtTALLiN_parameters.Default_parameters.constants_mainnet}. *)

(* Security factor ensures that even with the most powerful CPU in the market,
   you cannot compute the VDF result within the time granted for nonce
   revelation. *)
let vdf_security_factor = 5l

(* This factor is to ensure that after nonce revelation and VDF computation, we
   have a comfortable margin to inject the VDF computation result.

   [margin_in_cycle_factor=2] means that the cycle should last two times more
   than nonce revelation + VDF computation. ie half of the cycle remains for
   including the VDF result in a block . *)
let margin_in_cycle_factor = 2l

(* Number of modular squaring per second on benchmark machine *)
let vdf_modular_squaring_per_second = 200_000L

let () =
  register_test
    ~title:"Nonce revelation period is short enough for VDF to fit in a cycle "
  @@ fun () ->
  let constants = Default_parameters.constants_mainnet in
  Assert.leq
    ~loc:__LOC__
    Int32.compare
    "nonce_revelation_threshold is too short wrt blocks_per_cycle"
    (fun ppf -> Format.fprintf ppf "%ld")
    Int32.(
      (* time of reveal + vdf comp + VDF injection *)
      mul
        margin_in_cycle_factor
        ((* time for revelation AND vdf computation *)
         mul
           (add 1l vdf_security_factor)
           constants.nonce_revelation_threshold))
    constants.blocks_per_cycle

let () =
  register_test ~title:"VDF difficulty fits into a cycle" @@ fun () ->
  let constants = Default_parameters.constants_mainnet in
  let open Lwt_result_syntax in
  let* cycle_period = cycles_to_period ~constants 1l in
  Assert.leq
    ~loc:__LOC__
    Int64.compare
    "nonce_revelation_threshold is too short wrt blocks_per_cycle"
    (fun ppf -> Format.fprintf ppf "%Ld")
    Int64.(mul (of_int32 margin_in_cycle_factor) constants.vdf_difficulty)
    Int64.(
      (* time of a cycle translated to VDF difficulty *)
      mul
        vdf_modular_squaring_per_second
        (Protocol.Alpha_context.Period.to_seconds cycle_period))

let () =
  register_test ~title:"VDF difficulty is secure wrt revelation period"
  @@ fun () ->
  let constants = Default_parameters.constants_mainnet in
  let open Lwt_result_wrap_syntax in
  let*?@ revelation_period =
    Protocol.Alpha_context.Period.mult
      constants.nonce_revelation_threshold
      constants.minimal_block_delay
  in
  Assert.leq
    ~loc:__LOC__
    Int64.compare
    "vdf_difficulty is too easy wrt nonce_revelation_threshold"
    (fun ppf -> Format.fprintf ppf "%Ld")
    Int64.(
      (* [vdf_security_factor] times revelation period translated to VDF difficulty *)
      mul
        (of_int32 vdf_security_factor)
        (mul
           vdf_modular_squaring_per_second
           (Protocol.Alpha_context.Period.to_seconds revelation_period)))
    constants.vdf_difficulty

(* Check that
    [sc_rollup_challenge_window_in_blocks < sc_rollup_max_lookahead_in_blocks]

    Otherwise committers would be forced to commit at an artificially slow rate, affecting
    the throughput of the rollup. *)
let () =
  register_test ~title:"sc rollup challenge window less than max lookahead"
  @@ fun () ->
  let constants = Default_parameters.constants_mainnet in
  let max_lookahead = constants.sc_rollup.max_lookahead_in_blocks in
  let challenge_window =
    Int32.of_int constants.sc_rollup.challenge_window_in_blocks
  in
  Assert.lt_int32 ~loc:__LOC__ challenge_window max_lookahead

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4481
   Improve this to catch more regressions in term of storage consumption *)

(* Check that
    [commitment_storage_cost * max_lookahead / commitment_period < stake_amount]

   Otherwise storage could be overallocated - since backtracking is not allowed, a staker
   can allocated at most [d] nodes (where [d] is the tree depth) - the maximum storage cost
   of these commitments must be at most the size of the staker's deposit. *)
let () =
  register_test ~title:"sc rollup max commitment storage cost less than deposit"
  @@ fun () ->
  let constants = Default_parameters.constants_mainnet in
  let open Protocol in
  let cost_per_byte_mutez =
    Alpha_context.Tez.to_mutez constants.cost_per_byte
  in
  let commitment_storage_size =
    Int64.of_int
      Sc_rollup_stake_storage.Internal_for_tests
      .max_commitment_storage_size_in_bytes
  in
  let commitment_storage_cost =
    Int64.mul cost_per_byte_mutez commitment_storage_size
  in
  let max_lookahead =
    Int64.of_int32 constants.sc_rollup.max_lookahead_in_blocks
  in
  let commitment_period =
    Int64.of_int constants.sc_rollup.commitment_period_in_blocks
  in
  let stake_amount =
    Alpha_context.Tez.to_mutez constants.sc_rollup.stake_amount
  in
  Assert.leq_int64
    ~loc:__LOC__
    (Int64.mul
       commitment_storage_cost
       (Int64.div max_lookahead commitment_period))
    stake_amount

(* Check that
   [{!Sc_rollup_stake_storage.commitment_storage_size_in_bytes} =
   commitments_entry_size + commitment_stake_count_entry_size +
   commitment_added_entry_size]

   Required to ensure [sc_rollup_stake_amount] and [sc_rollup_max_lookahead] are
   correctly scaled with respect to each other - see
   {!test_sc_rollup_max_commitment_storage_cost_lt_deposit}
*)
let () =
  register_test ~title:"sc rollup commitment storage size correct" @@ fun () ->
  let open Lwt_result_syntax in
  let open Protocol in
  let* number_of_ticks =
    Assert.get_some
      ~loc:__LOC__
      (Sc_rollup_repr.Number_of_ticks.of_value 1232909L)
  in
  let commitment =
    Sc_rollup_commitment_repr.
      {
        predecessor = Sc_rollup_commitment_repr.Hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_ticks;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let versioned_commitment =
    Sc_rollup_commitment_repr.to_versioned commitment
  in
  let commitment_length =
    Data_encoding.Binary.length
      Sc_rollup_commitment_repr.versioned_encoding
      versioned_commitment
  in
  let commitment_hash =
    Sc_rollup_commitment_repr.hash_uncarbonated commitment
  in
  let level = Alpha_context.Raw_level.of_int32_exn 5l in
  (* One for the first publication level, and one for the published level. *)
  let levels_length =
    Data_encoding.Binary.length Alpha_context.Raw_level.encoding level * 2
  in
  let staker_index =
    Sc_rollup_staker_index_repr.Internal_for_tests.of_z (Z.of_int 94323442)
  in
  let stakers_index_length =
    Data_encoding.(
      Binary.length (list Sc_rollup_staker_index_repr.encoding) [staker_index])
  in
  let commitment_hashes_length =
    Data_encoding.(
      Binary.length
        (list Sc_rollup_commitment_repr.Hash.encoding)
        [commitment_hash])
  in
  let max_expected =
    Sc_rollup_stake_storage.Internal_for_tests
    .max_commitment_storage_size_in_bytes
  in
  let total_computed =
    commitment_length + levels_length + stakers_index_length
    + commitment_hashes_length
  in
  Assert.leq_int ~loc:__LOC__ total_computed max_expected

(** Test that the amount of the liquidity baking subsidy is epsilon smaller than
   1/16th of the maximum reward. *)
let () =
  register_test
    ~title:
      "liquidity_baking_subsidy parameter is 1/16th of total baking rewards"
  @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let constants = Default_parameters.constants_mainnet in
  let get_reward =
    Protocol.Alpha_context.Delegate.Rewards.For_RPC.reward_from_constants
      constants
  in
  let*?@ baking_reward_bonus =
    get_reward ~reward_kind:Baking_reward_bonus_per_block
  in
  let*?@ baking_reward_fixed_portion =
    get_reward ~reward_kind:Baking_reward_fixed_portion
  in
  let*? baking_rewards = baking_reward_fixed_portion +? baking_reward_bonus in
  let*?@ validators_rewards =
    get_reward ~reward_kind:Attesting_reward_per_block
  in
  let*?@ dal_attesting_reward_per_shard =
    get_reward ~reward_kind:Dal_attesting_reward_per_shard
  in
  let*? dal_rewards =
    if constants.dal.incentives_enable then
      dal_attesting_reward_per_shard
      *? Int64.of_int constants.dal.cryptobox_parameters.number_of_shards
    else Result_syntax.return (of_int 0)
  in
  let*? total_tb_rewards = baking_rewards +? validators_rewards in
  let*? total_rewards = total_tb_rewards +? dal_rewards in
  let expected_subsidy = total_rewards /! 16L in
  let*?@ liquidity_baking_subsidy =
    Protocol.Alpha_context.Delegate.Rewards.For_RPC
    .liquidity_baking_subsidy_from_constants
      constants
  in
  let*? diff = liquidity_baking_subsidy -? expected_subsidy in
  let max_diff =
    1000
    (* mutez *)
  in
  Assert.leq_int ~loc:__LOC__ (Int64.to_int (to_mutez diff)) max_diff
