(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Liquidity_baking_machine
open QCheck.Gen
open Lib_test

let total_xtz = 32_000_000_000_000L

let ten_subsidies = 25_000_000L

let rec remove_last_element = function
  | [_] -> []
  | x :: rst -> x :: remove_last_element rst
  | [] -> raise (Invalid_argument "remove_last_element")

(** Try to shrink a list by removing elements from the tail of said
    list.

    The elements themselves are not shrinked. *)
let shrink_list_spine_tail : 'a list QCheck.Shrink.t =
  let rec shrinked_list = function
    | [] -> []
    | l ->
        let l = remove_last_element l in
        l :: shrinked_list l
  in
  fun l -> QCheck.Iter.of_list (shrinked_list l)

let gen_balances : int64 -> int -> int -> balances QCheck.Gen.t =
 fun max_xtz max_tzbtc max_liquidity ->
  let+ xtz = Qcheck_helpers.int64_strictly_positive_gen max_xtz
  and+ tzbtc = Qcheck_helpers.int_strictly_positive_gen max_tzbtc
  and+ liquidity = Qcheck_helpers.int_strictly_positive_gen max_liquidity in
  {xtz; tzbtc; liquidity}

let gen_specs : int -> int -> specs QCheck.Gen.t =
 fun total_tzbtc total_liquidity ->
  (* 1. We pick a random number to decide how many implicit account we
        will set-up in the specs. Note that there will be one more
        implicit accounts, the [Holder], that we will use to reach the
        expected balances for the CPMM and the implicit accounts. *)
  let* accounts_numbers = int_range 10 20 in
  (* 2. To keep the generator simpler, we do not try to strictly reach
        the [total_tzbtc] and [total_liquidity] value, but rather we
        compute maxima for the implicit accounts balances from
        them. *)
  (* 2.1. We divide a fraction of the [total_xtz] that we need to
          share to the implicit accounts. The rationale is to provide
          a large amount to xtz to [Holder], so that we do not have to
          worry about it being “rich enough.” *)
  let max_xtz = Int64.(div total_xtz (of_int (50 * accounts_numbers))) in
  (* 2.2. We divide [total_tzbtc] between the implicit accounts *and*
          the CPMM contract. *)
  let max_tzbtc = total_tzbtc / (accounts_numbers + 1) in
  (* 2.2. We divide [total_liquidity] between the implicit accounts only.  *)
  let max_liquidity = total_liquidity / accounts_numbers in
  let+ cpmm_balance = gen_balances max_xtz max_tzbtc 1
  and+ accounts_balances =
    list_repeat accounts_numbers (gen_balances max_xtz max_tzbtc max_liquidity)
  in
  {
    cpmm_min_xtz_balance = cpmm_balance.xtz;
    cpmm_min_tzbtc_balance = cpmm_balance.tzbtc;
    accounts_balances;
  }

let arb_specs : tzbtc -> liquidity -> specs QCheck.arbitrary =
 fun total_tzbtc total_liquidity ->
  QCheck.make
    ~print:(fun specs -> Format.asprintf "%a" pp_specs specs)
    (gen_specs total_tzbtc total_liquidity)

type 'a optgen = 'a option QCheck.Gen.t

let ( let*? ) (m : 'a optgen) (f : 'a -> 'b optgen) =
  let* x = m in
  match x with None -> return None | Some x -> f x

(** [genopt_oneof l] tries to generate a value using the generators of
    [l], one at a time.

    First, the list [l] is randomized, then each generator is
    tried. The first one to return a result (not [None]) is picked. If
    all generators returns [None], the generators tries again with the
    whole list (at most 100 times). If no generator of [l] is able to
    return a result, then [genopt_oneof l] returns [None]. *)
let genopt_oneof (l : 'a optgen list) : 'a optgen =
  let* l = QCheck.Gen.shuffle_l l in
  let rec aux n = function
    | [] -> if n = 0 then pure None else aux (n - 1) l
    | g :: l -> (
        let* x = g in
        match x with None -> aux n l | Some x -> pure @@ Some x)
  in
  aux 100 l

let genopt_account ?choice ?(filter = Fun.const true) env : contract_id optgen =
  let l =
    List.filter
      filter
      (Option.fold ~none:env.implicit_accounts ~some:(fun x -> [x]) choice)
  in
  if l = [] then return None else map Option.some (oneofl l)

let genopt_account_with_tzbtc ?choice ?(min = 1) env state =
  genopt_account
    ?choice
    ~filter:(fun a -> SymbolicMachine.get_tzbtc_balance a env state >= min)
    env

let genopt_account_with_xtz ?choice ?(min = 1L) env state =
  genopt_account
    ?choice
    ~filter:(fun a -> SymbolicMachine.get_xtz_balance a state >= min)
    env

let genopt_account_with_liquidity ?choice ?(min = 1) env state =
  genopt_account
    ?choice
    ~filter:(fun a -> SymbolicMachine.get_liquidity_balance a env state >= min)
    env

let genopt_step_tzbtc_to_xtz :
    ?source:contract_id ->
    ?destination:contract_id ->
    contract_id env ->
    SymbolicMachine.t ->
    contract_id step optgen =
 fun ?source ?destination env state ->
  let*? source = genopt_account_with_tzbtc ?choice:source env state in
  let*? destination = genopt_account ?choice:destination env in
  let+ tzbtc_deposit =
    Qcheck_helpers.int_strictly_positive_gen
      (SymbolicMachine.get_tzbtc_balance source env state)
  in
  (* See note (2) *)
  if
    SymbolicMachine.get_tzbtc_balance env.cpmm_contract env state
    < Int.max_int - tzbtc_deposit
  then Some (SellTzBTC {source; destination; tzbtc_deposit})
  else None

let genopt_step_xtz_to_tzbtc :
    ?source:contract_id ->
    ?destination:contract_id ->
    contract_id env ->
    SymbolicMachine.t ->
    contract_id step optgen =
 fun ?source ?destination env state ->
  let*? source = genopt_account_with_xtz ?choice:source env state in
  let*? destination = genopt_account ?choice:destination env in
  let+ xtz_deposit =
    map
      Int64.of_int
      (int_range
         1
         (Int64.to_int @@ SymbolicMachine.get_xtz_balance source state))
  in
  (* See note (2) *)
  if
    SymbolicMachine.get_xtz_balance env.cpmm_contract state
    < Int64.(sub max_int (add ten_subsidies xtz_deposit))
  then Some (BuyTzBTC {source; destination; xtz_deposit})
  else None

let genopt_step_add_liquidity :
    ?source:contract_id ->
    ?destination:contract_id ->
    contract_id env ->
    SymbolicMachine.t ->
    contract_id step optgen =
 fun ?source ?destination env state ->
  let rec find_xtz_deposit candidate max_tzbtc_deposit =
    let tzbtc_deposit =
      SymbolicMachine.predict_required_tzbtc_deposit candidate env state
    in
    if tzbtc_deposit <= max_tzbtc_deposit then candidate
    else find_xtz_deposit (Int64.div candidate 2L) max_tzbtc_deposit
  in
  let*? source = genopt_account_with_xtz ?choice:source env state in
  let*? destination = genopt_account ?choice:destination env in
  let source_xtz_pool = SymbolicMachine.get_xtz_balance source state in
  (* the source needs at least one xtz *)
  if 1L < source_xtz_pool then
    let+ candidate =
      Qcheck_helpers.int64_strictly_positive_gen source_xtz_pool
    in
    let xtz_deposit =
      find_xtz_deposit
        candidate
        (SymbolicMachine.get_tzbtc_balance source env state)
    in
    (* See note (2) *)
    if
      SymbolicMachine.get_xtz_balance env.cpmm_contract state
      < Int64.(sub max_int (add ten_subsidies xtz_deposit))
    then Some (AddLiquidity {source; destination; xtz_deposit})
    else None
  else pure None

let genopt_step_remove_liquidity :
    ?source:contract_id ->
    ?destination:contract_id ->
    contract_id env ->
    SymbolicMachine.t ->
    contract_id step optgen =
 fun ?source ?destination env state ->
  let*? source = genopt_account_with_liquidity ?choice:source env state in
  let*? destination = genopt_account ?choice:destination env in
  let lqt_available = SymbolicMachine.get_liquidity_balance source env state in
  if 1 < lqt_available then
    let+ lqt_burned =
      int_range 1 (SymbolicMachine.get_liquidity_balance source env state)
    in
    Some (RemoveLiquidity {source; destination; lqt_burned})
  else return None

let genopt_step :
    ?source:contract_id ->
    ?destination:contract_id ->
    contract_id env ->
    SymbolicMachine.t ->
    contract_id step optgen =
 fun ?source ?destination env state ->
  genopt_oneof
    [
      genopt_step_tzbtc_to_xtz env state ?source ?destination;
      genopt_step_xtz_to_tzbtc env state ?source ?destination;
      genopt_step_add_liquidity env state ?source ?destination;
      genopt_step_remove_liquidity env state ?source ?destination;
    ]

let rec gen_steps :
    ?source:contract_id ->
    ?destination:contract_id ->
    contract_id env ->
    SymbolicMachine.t ->
    int ->
    contract_id step list QCheck.Gen.t =
 fun ?source ?destination env state size ->
  if size <= 0 then return []
  else
    let* h = genopt_step ?source ?destination env state in
    match h with
    | None -> pure []
    | Some h ->
        let state = SymbolicMachine.step h env state in
        let* rst = gen_steps ?source ?destination env state (size - 1) in
        pure (h :: rst)

let gen_scenario :
    tzbtc -> liquidity -> int -> (specs * contract_id step list) QCheck.Gen.t =
 fun total_tzbtc total_liquidity size ->
  let* specs = gen_specs total_tzbtc total_liquidity in
  let (state, env) = SymbolicMachine.build specs in
  let+ scenario = gen_steps env state size in
  (specs, scenario)

let pp_scenario fmt (specs, steps) =
  Format.(
    fprintf
      fmt
      "@[<v>{@ @[<v 2>  @[<v 2>specs@ = %a;@]@ @[<v 2>steps@ = @[<v>[ \
       %a]@]@]@]}@]"
      pp_specs
      specs
      (pp_print_list
         ~pp_sep:(fun fmt _ -> fprintf fmt "@ ; ")
         (pp_step pp_contract_id))
      steps)

let arb_scenario :
    tzbtc ->
    liquidity ->
    int ->
    (specs * contract_id step list) QCheck.arbitrary =
 fun total_tzbtc total_liquidity size ->
  QCheck.make
    ~print:(Format.asprintf "%a" pp_scenario)
    ~shrink:(fun (specs, steps) ->
      (* See note (1) *)
      QCheck.Iter.pair (QCheck.Iter.return specs) (shrink_list_spine_tail steps))
    (gen_scenario total_tzbtc total_liquidity size)

let gen_adversary_scenario :
    tzbtc ->
    liquidity ->
    int ->
    (specs * contract_id * contract_id step list) QCheck.Gen.t =
 fun total_tzbtc total_liquidity size ->
  let* specs = gen_specs total_tzbtc total_liquidity in
  let (state, env) = SymbolicMachine.build ~subsidy:0L specs in
  let* c = oneofl env.implicit_accounts in
  let+ scenario = gen_steps ~source:c ~destination:c env state size in
  (specs, c, scenario)

let arb_adversary_scenario :
    tzbtc ->
    liquidity ->
    int ->
    (specs * contract_id * contract_id step list) QCheck.arbitrary =
 fun total_tzbtc total_liquidity size ->
  QCheck.make
    ~print:(fun (specs, _, steps) ->
      Format.asprintf "%a" pp_scenario (specs, steps))
    ~shrink:(fun (specs, c, steps) ->
      (* see note (1) *)
      QCheck.Iter.triple
        (QCheck.Iter.return specs)
        (QCheck.Iter.return c)
        (shrink_list_spine_tail steps))
    (gen_adversary_scenario total_tzbtc total_liquidity size)

(* -------------------------------------------------------------------------- *)

(* Note (1)

   We shrink a valid scenario by removing steps from its tails,
   because a prefix of a valid scenario remains a valid
   scenario. Removing a random element of a scenario could lead to an
   invalid scenario.  *)

(* Note (2)

   If we are not being careful, it is possible to provoke an overflow
   in the xtzPool and tzbtcPool. We try to avoid that as much as
   possible by being very careful with the steps that are likely to
   add xtz to the contract. *)
