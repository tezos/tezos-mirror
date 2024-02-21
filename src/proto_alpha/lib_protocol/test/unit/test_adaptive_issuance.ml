(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:  Protocol (rewards)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                 -- --file test_adaptive_issuance.ml
    Subject:    Test reward values under adaptive issuance
*)

open Protocol
open Alpha_context

let test_reward_coefficient () =
  let open Lwt_result_wrap_syntax in
  let csts = Default_parameters.constants_test in
  let*?@ default =
    Delegate.Rewards.For_RPC.(
      reward_from_constants csts ~reward_kind:Baking_reward_fixed_portion)
  in
  let*?@ default_times_4 =
    Delegate.Rewards.For_RPC.(
      reward_from_constants
        ~coeff:(Q.of_int 4)
        csts
        ~reward_kind:Baking_reward_fixed_portion)
  in
  Assert.equal_tez ~loc:__LOC__ Tez_helpers.(default *! 4L) default_times_4

let test_reward_coeff_ratio () =
  let open Delegate.Rewards.Internal_for_tests in
  let open Lwt_result_wrap_syntax in
  let assert_eq ~loc a b = Assert.equal ~loc Q.equal "" Q.pp_print a b in
  (* Curve tests *)
  let curve stake_ratio =
    compute_reward_coeff_ratio_without_bonus
      ~stake_ratio
      ~issuance_ratio_max:(Q.of_int 1_000_000)
      ~issuance_ratio_min:(Q.of_int (-1_000_000))
  in
  (* Test the curve on some staking points: 1, 1/10, 1/100, 1/2 *)
  let* () = assert_eq ~loc:__LOC__ (curve Q.one) Q.(1 // 1600) in
  let* () = assert_eq ~loc:__LOC__ (curve Q.(1 // 10)) Q.(1 // 16) in
  let* () = assert_eq ~loc:__LOC__ (curve Q.(1 // 100)) Q.(100 // 16) in
  let* () = assert_eq ~loc:__LOC__ (curve Q.(1 // 2)) Q.(1 // 400) in
  (* Test the curve on extreme values: ε and 1 - ε with ε small *)
  let epsilon = Q.(3 // 50000) in
  let one_m_e = Q.(one - epsilon) in
  let* () =
    assert_eq ~loc:__LOC__ (curve epsilon) Q.(1 // 1600 / (epsilon * epsilon))
  in
  let* () =
    assert_eq ~loc:__LOC__ (curve one_m_e) Q.(1 // 1600 / (one_m_e * one_m_e))
  in
  (* Test min max *)
  let bound = Q.(1 // 30) in
  (* curve(1/10) = 1/16 > 1/30 (max). Expected result: 1/30 *)
  let* () =
    assert_eq
      ~loc:__LOC__
      (compute_reward_coeff_ratio_without_bonus
         ~stake_ratio:Q.(1 // 10)
         ~issuance_ratio_max:bound
         ~issuance_ratio_min:(Q.of_int (-100)))
      bound
  in
  (* curve(1) = 1/1600 < 1/30 (min). Expected result: 1/30 *)
  let* () =
    assert_eq
      ~loc:__LOC__
      (compute_reward_coeff_ratio_without_bonus
         ~stake_ratio:Q.one
         ~issuance_ratio_max:(Q.of_int 100)
         ~issuance_ratio_min:bound)
      bound
  in
  return_unit

let test_compute_bonus () =
  let open Delegate.Rewards.Internal_for_tests in
  let open Lwt_result_wrap_syntax in
  let assert_fun ~loc ~f a b =
    let* a in
    Assert.equal ~loc f "" Q.pp_print a b
  in
  let assert_eq ~loc a b =
    let* a in
    Assert.equal ~loc Q.equal "" Q.pp_print a b
  in
  let reward_params =
    Default_parameters.constants_test.adaptive_issuance.adaptive_rewards_params
  in
  (* For simplicity, one cycle = one day *)
  let seconds_per_cycle = 86_400L in
  let compute_bonus ?(seconds_per_cycle = seconds_per_cycle) stake_ratio
      previous_bonus =
    let () = assert (Q.(stake_ratio <= one)) in
    let*?@ previous_bonus =
      Issuance_bonus_repr.of_Q ~max_bonus:reward_params.max_bonus previous_bonus
    in
    let issuance_ratio_max = reward_params.issuance_ratio_final_max in
    let issuance_ratio_min = reward_params.issuance_ratio_final_min in
    let base_reward_coeff_ratio =
      compute_reward_coeff_ratio_without_bonus
        ~stake_ratio
        ~issuance_ratio_max
        ~issuance_ratio_min
    in
    let*?@ bonus =
      compute_bonus
        ~issuance_ratio_max
        ~seconds_per_cycle
        ~stake_ratio
        ~base_reward_coeff_ratio
        ~previous_bonus
        ~reward_params
    in
    let full_reward_coeff = Q.add (bonus :> Q.t) base_reward_coeff_ratio in
    (* The full coeff should be within the bounds *)
    let* () =
      assert_fun
        ~loc:__LOC__
        ~f:Q.geq
        (return full_reward_coeff)
        issuance_ratio_min
    in
    let* () =
      assert_fun
        ~loc:__LOC__
        ~f:Q.leq
        (return full_reward_coeff)
        issuance_ratio_max
    in
    return (bonus :> Q.t)
  in
  let small_bonus = Q.(1 // 200) (* 0.5% *) in
  (* Test deadzone *)
  let* () =
    assert_eq ~loc:__LOC__ (compute_bonus Q.(48 // 100) small_bonus) small_bonus
  in
  let* () =
    assert_eq ~loc:__LOC__ (compute_bonus Q.(52 // 100) small_bonus) small_bonus
  in
  let* () =
    assert_fun
      ~loc:__LOC__
      ~f:Q.gt
      (compute_bonus Q.(47_9999 // 100_0000) small_bonus)
      small_bonus
  in
  let* () =
    assert_fun
      ~loc:__LOC__
      ~f:Q.lt
      (compute_bonus Q.(52_0001 // 100_0000) small_bonus)
      small_bonus
  in
  (* Test variation amplitude *)
  let variation = Q.(1 // 10_000) (* 0.01% *) in
  let* () =
    assert_eq
      ~loc:__LOC__
      (compute_bonus Q.(47 // 100) small_bonus)
      (Q.add small_bonus variation)
  in
  let* () =
    assert_eq
      ~loc:__LOC__
      (compute_bonus Q.(40 // 100) small_bonus)
      (Q.add small_bonus (Q.mul variation (Q.of_int 8)))
  in
  let* () =
    assert_eq
      ~loc:__LOC__
      (compute_bonus Q.(53 // 100) small_bonus)
      (Q.sub small_bonus variation)
  in
  let* () =
    assert_eq
      ~loc:__LOC__
      (compute_bonus Q.(60 // 100) small_bonus)
      (Q.sub small_bonus (Q.mul variation (Q.of_int 8)))
  in
  (* Test bounds *)
  let max_bonus = (reward_params.max_bonus :> Q.t) in
  let* () =
    assert_eq ~loc:__LOC__ (compute_bonus Q.(60 // 100) Q.zero) Q.zero
  in
  let* () =
    assert_fun
      ~loc:__LOC__
      ~f:Q.leq
      (compute_bonus Q.(40 // 100) max_bonus)
      max_bonus
  in
  (* Test linearity wrt seconds_per_cycle *)
  let compute_growth seconds_per_cycle =
    let* computed_bonus =
      compute_bonus ~seconds_per_cycle Q.(47 // 100) small_bonus
    in
    return Q.(sub computed_bonus small_bonus)
  in
  let* base_growth = compute_growth seconds_per_cycle in
  let* () =
    assert_eq
      ~loc:__LOC__
      (compute_growth (Int64.div seconds_per_cycle 2L))
      Q.(mul (1 // 2) base_growth)
  in
  let* () =
    assert_eq
      ~loc:__LOC__
      (compute_growth (Int64.mul seconds_per_cycle 2L))
      Q.(mul (2 // 1) base_growth)
  in
  return_unit

let test_compute_coeff () =
  let open Delegate.Rewards.Internal_for_tests in
  let open Lwt_result_wrap_syntax in
  let assert_eq ~loc a b = Assert.equal ~loc Q.equal "" Q.pp_print a b in
  let assert_eq_lwt ~loc a b =
    let* a in
    let* b in
    Assert.equal ~loc Q.equal "" Q.pp_print a b
  in
  let reward_params =
    Default_parameters.constants_test.adaptive_issuance.adaptive_rewards_params
  in
  let base_total_issued_per_minute =
    Default_parameters.constants_test.issuance_weights
      .base_total_issued_per_minute |> Tez.to_mutez |> Tez_repr.of_mutez_exn
  in
  let compute_coeff
      ?(base_total_issued_per_minute = base_total_issued_per_minute)
      ?(q_total_supply = Q.of_int64 1_000_000_000L) () =
    compute_coeff
      ~issuance_ratio_max:reward_params.issuance_ratio_final_max
      ~issuance_ratio_min:reward_params.issuance_ratio_final_min
      ~base_total_issued_per_minute
      ~q_total_supply
  in
  (* Test inverse linearity wrt base issuance
     Base issuance * Issuance coeff = constant (= issuance per unit of time)
     In other words, ∀α>0.αf(αx)=f(x), where x is the base issuance
     and f is compute_coeff *)
  let* () =
    let base_issuance = 20_000_000L in
    let compute_coeff base =
      compute_coeff
        ~base_total_issued_per_minute:(Tez_repr.of_mutez_exn base)
        ~base_reward_coeff_ratio:Q.(1 // 100)
        ~bonus:Issuance_bonus_repr.zero
        ()
    in
    let* () =
      assert_eq
        ~loc:__LOC__
        (compute_coeff (Int64.div base_issuance 2L))
        Q.(mul (2 // 1) (compute_coeff base_issuance))
    in
    let* () =
      assert_eq
        ~loc:__LOC__
        (compute_coeff (Int64.mul base_issuance 2L))
        Q.(mul (1 // 2) (compute_coeff base_issuance))
    in
    (* Test edge case base = 0 *)
    let* () = assert_eq ~loc:__LOC__ (compute_coeff 0L) Q.one in
    return_unit
  in
  (* Test linearity wrt total_supply *)
  let* () =
    let total_supply = Q.of_int64 1_000_000_000L in
    let compute_coeff q_total_supply =
      compute_coeff
        ~q_total_supply
        ~base_reward_coeff_ratio:Q.(1 // 100)
        ~bonus:Issuance_bonus_repr.zero
        ()
    in
    let* () =
      assert_eq
        ~loc:__LOC__
        (compute_coeff Q.(mul (1 // 2) total_supply))
        Q.(mul (1 // 2) (compute_coeff total_supply))
    in
    let* () =
      assert_eq
        ~loc:__LOC__
        (compute_coeff Q.(mul (2 // 1) total_supply))
        Q.(mul (2 // 1) (compute_coeff total_supply))
    in
    return_unit
  in
  let* () =
    let compute_coeff base_reward_coeff_ratio bonus =
      (* bonus must be <= 5% *)
      let*?@ bonus =
        Issuance_bonus_repr.of_Q ~max_bonus:reward_params.max_bonus bonus
      in
      return @@ compute_coeff ~base_reward_coeff_ratio ~bonus ()
    in
    let q_mul a b =
      let* b in
      return @@ Q.mul a b
    in
    let q_add a b =
      let* a in
      let* b in
      return @@ Q.add a b
    in
    (* Test linearity wrt base_reward_coeff_ratio *)
    let* () =
      assert_eq_lwt
        ~loc:__LOC__
        (compute_coeff Q.(2 // 99) Q.zero)
        (q_mul Q.(2 // 1) (compute_coeff Q.(1 // 99) Q.zero))
    in
    let* () =
      assert_eq_lwt
        ~loc:__LOC__
        (compute_coeff Q.(3 // 99) Q.zero)
        (q_add
           (compute_coeff Q.(2 // 99) Q.zero)
           (compute_coeff Q.(1 // 99) Q.zero))
    in
    (* Test symmetry *)
    let* () =
      assert_eq_lwt
        ~loc:__LOC__
        (compute_coeff Q.(1 // 99) Q.zero)
        (compute_coeff Q.zero Q.(1 // 99))
    in
    let* () =
      assert_eq_lwt
        ~loc:__LOC__
        (compute_coeff Q.(2 // 99) Q.(1 // 99))
        (compute_coeff Q.(1 // 99) Q.(2 // 99))
    in
    (* Test min *)
    let* () =
      assert_eq_lwt
        ~loc:__LOC__
        (compute_coeff Q.zero Q.zero)
        (compute_coeff reward_params.issuance_ratio_final_min Q.zero)
    in
    (* Test max *)
    let* () =
      assert_eq_lwt
        ~loc:__LOC__
        (compute_coeff
           (Q.add reward_params.issuance_ratio_final_max Q.one)
           Q.zero)
        (compute_coeff reward_params.issuance_ratio_final_max Q.zero)
    in
    let* () =
      assert_eq_lwt
        ~loc:__LOC__
        (compute_coeff reward_params.issuance_ratio_final_max Q.(1 // 100))
        (compute_coeff reward_params.issuance_ratio_final_max Q.zero)
    in
    return_unit
  in
  return_unit

let test_compute_min_max () =
  let open Delegate.Rewards.Internal_for_tests in
  let open Lwt_result_wrap_syntax in
  (* let assert_eq ~loc a b = Assert.equal ~loc Q.equal "" Q.pp_print a b in *)
  let assert_eq_list ~loc a b =
    Assert.assert_equal_list ~loc Q.equal "" Q.pp_print a b
  in
  let reward_params =
    Default_parameters.constants_test.adaptive_issuance.adaptive_rewards_params
  in
  let update_reward_params ?issuance_ratio_final_min ?issuance_ratio_final_max
      ?issuance_ratio_initial_min ?issuance_ratio_initial_max ?initial_period
      ?transition_period
      (reward_params : Constants.Parametric.adaptive_rewards_params) =
    let issuance_ratio_final_min =
      Option.value
        ~default:reward_params.issuance_ratio_final_min
        issuance_ratio_final_min
    in
    let issuance_ratio_final_max =
      Option.value
        ~default:reward_params.issuance_ratio_final_max
        issuance_ratio_final_max
    in
    let issuance_ratio_initial_min =
      Option.value
        ~default:reward_params.issuance_ratio_initial_min
        issuance_ratio_initial_min
    in
    let issuance_ratio_initial_max =
      Option.value
        ~default:reward_params.issuance_ratio_initial_max
        issuance_ratio_initial_max
    in
    let initial_period =
      Option.value ~default:reward_params.initial_period initial_period
    in
    let transition_period =
      Option.value ~default:reward_params.transition_period transition_period
    in
    {
      reward_params with
      issuance_ratio_final_min;
      issuance_ratio_final_max;
      issuance_ratio_initial_min;
      issuance_ratio_initial_max;
      initial_period;
      transition_period;
    }
  in
  let compute_aux ~f ?issuance_ratio_final_min ?issuance_ratio_final_max
      ?issuance_ratio_initial_min ?issuance_ratio_initial_max ?initial_period
      ?transition_period ?(launch_cycle = Some 0l) cycle =
    let launch_cycle = Option.map Cycle_repr.of_int32_exn launch_cycle in
    let new_cycle = Cycle_repr.of_int32_exn (Int32.of_int cycle) in
    let reward_params =
      update_reward_params
        ?issuance_ratio_final_min
        ?issuance_ratio_final_max
        ?issuance_ratio_initial_min
        ?issuance_ratio_initial_max
        ?initial_period
        ?transition_period
        reward_params
    in
    f ~reward_params ~launch_cycle ~new_cycle
  in
  let compute_min = compute_aux ~f:compute_min in
  let compute_max = compute_aux ~f:compute_max in
  let assert_eq_on_interval ~loc ~f ~from ~to_ expected =
    assert (List.length expected = to_ - from + 1) ;
    let actual = Stdlib.List.init (to_ - from + 1) (fun i -> f (i + from)) in
    assert_eq_list ~loc expected actual
  in
  (* Python-style list generation *)
  let ( *+ ) a b = Stdlib.List.init b (fun _ -> a) in
  (* Test before launch cycle *)
  let* () =
    assert_eq_on_interval
      ~loc:__LOC__
      ~f:(compute_min ~launch_cycle:(Some 10l))
      ~from:0
      ~to_:10
      (reward_params.issuance_ratio_initial_min *+ 11)
  in
  let* () =
    assert_eq_on_interval
      ~loc:__LOC__
      ~f:(compute_max ~launch_cycle:(Some 10l))
      ~from:0
      ~to_:10
      (reward_params.issuance_ratio_initial_max *+ 11)
  in
  (* Test no launch cycle *)
  let* () =
    assert_eq_on_interval
      ~loc:__LOC__
      ~f:(compute_min ~launch_cycle:None)
      ~from:0
      ~to_:10
      (reward_params.issuance_ratio_initial_min *+ 11)
  in
  let* () =
    assert_eq_on_interval
      ~loc:__LOC__
      ~f:(compute_max ~launch_cycle:None)
      ~from:0
      ~to_:10
      (reward_params.issuance_ratio_initial_max *+ 11)
  in
  (* From now on, launch_cycle = 0 *)
  (* Test initial period *)
  let* () =
    assert_eq_on_interval
      ~loc:__LOC__
      ~f:compute_min
      ~from:0
      ~to_:reward_params.initial_period
      (reward_params.issuance_ratio_initial_min
      *+ (reward_params.initial_period + 1))
  in
  let* () =
    assert_eq_on_interval
      ~loc:__LOC__
      ~f:compute_max
      ~from:0
      ~to_:reward_params.initial_period
      (reward_params.issuance_ratio_initial_max
      *+ (reward_params.initial_period + 1))
  in
  (* Test final period *)
  let* () =
    assert_eq_on_interval
      ~loc:__LOC__
      ~f:compute_min
      ~from:(reward_params.initial_period + reward_params.transition_period + 1)
      ~to_:(reward_params.initial_period + reward_params.transition_period + 10)
      (reward_params.issuance_ratio_final_min *+ 10)
  in
  let* () =
    assert_eq_on_interval
      ~loc:__LOC__
      ~f:compute_max
      ~from:(reward_params.initial_period + reward_params.transition_period + 1)
      ~to_:(reward_params.initial_period + reward_params.transition_period + 10)
      (reward_params.issuance_ratio_final_max *+ 10)
  in
  (* Test transition period *)
  let* () =
    let initial_period = 5 in
    let transition_period = 5 in
    let issuance_ratio_initial_min = Q.(7 // 100) in
    let issuance_ratio_final_min = Q.(1 // 100) in
    (* Min increases by 1/100th per cycle *)
    let issuance_ratio_initial_max = Q.(1 // 10) in
    let issuance_ratio_final_max = Q.(7 // 10) in
    (* Max increases by 1/10th per cycle *)
    let compute_min =
      compute_min
        ~initial_period
        ~transition_period
        ~issuance_ratio_initial_max
        ~issuance_ratio_initial_min
        ~issuance_ratio_final_max
        ~issuance_ratio_final_min
    in
    let compute_max =
      compute_max
        ~initial_period
        ~transition_period
        ~issuance_ratio_initial_max
        ~issuance_ratio_initial_min
        ~issuance_ratio_final_max
        ~issuance_ratio_final_min
    in
    let* () =
      assert_eq_on_interval
        ~loc:__LOC__
        ~f:compute_min
        ~from:0
        ~to_:15
        ((issuance_ratio_initial_min *+ 6)
        @ Q.[6 // 100; 5 // 100; 4 // 100; 3 // 100; 2 // 100]
        @ (issuance_ratio_final_min *+ 5))
    in
    let* () =
      assert_eq_on_interval
        ~loc:__LOC__
        ~f:compute_max
        ~from:0
        ~to_:15
        ((issuance_ratio_initial_max *+ 6)
        @ Q.[2 // 10; 3 // 10; 4 // 10; 5 // 10; 6 // 10]
        @ (issuance_ratio_final_max *+ 5))
    in
    return_unit
  in
  return_unit

let tests =
  Tztest.
    [
      tztest
        "adaptive issuance - application of coefficient to rewards"
        `Quick
        test_reward_coefficient;
      tztest
        "adaptive issuance - reward coeff ratio computation"
        `Quick
        test_reward_coeff_ratio;
      tztest
        "adaptive issuance - reward bonus computation"
        `Quick
        test_compute_bonus;
      tztest
        "adaptive issuance - reward coeff computation"
        `Quick
        test_compute_coeff;
      tztest
        "adaptive issuance - min/max coeff computation"
        `Quick
        test_compute_min_max;
    ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("adaptive issuance", tests)]
  |> Lwt_main.run
