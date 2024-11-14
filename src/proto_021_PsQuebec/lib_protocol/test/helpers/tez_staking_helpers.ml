(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** [Tez_staking_helpers] defines different kinds of tez Modules that
    manipulate them in different ways. They involve more complicated operations,
    as they are related to staking, thus are represented as partial amounts,
    and are related to pseudotokens. *)

module Cycle = Protocol.Alpha_context.Cycle

module Tez = struct
  include Tez_helpers
  include Tez_helpers.Compare
end

(** Representation of Tez with non integer values *)
module Partial_tez = struct
  include Q

  let of_tez a = Tez.to_mutez a |> of_int64

  let to_tez_rem {num; den} =
    let tez, rem = Z.div_rem num den in
    (Tez.of_z tez, rem /// den)

  let to_tez ~round = Tez.of_q ~round

  let get_rem a = snd (to_tez_rem a)

  let pp fmt a =
    let tez, rem = to_tez_rem a in
    (* If rem = 0, we keep the (+ 0), to indicate that it's a partial tez *)
    Format.fprintf fmt "%a ( +%aµꜩ )" Tez.pp tez Q.pp_print rem
end

(** [Frozen_tez] represents frozen stake and frozen unstaked funds.
    Properties:
    - sum of all current partial tez is an integer
    - Can only add integer amounts
    - Can always subtract integer amount (if lower than frozen amount)
    - If subtracting partial amount, must be the whole frozen amount (for given contract).
      The remainder is then distributed equally amongst remaining accounts, to keep property 1.
    - All entries of current are positive, non zero.
*)
module Frozen_tez = struct
  (* The map in current maps the stakers' name with their staked value.
     It contains only delegators of the delegate which owns the frozen tez *)
  type t = {
    delegate : string;
    initial : Tez.t;
    self_current : Tez.t;
    co_current : Partial_tez.t String.Map.t;
  }

  let pp fmt {delegate; initial; self_current; co_current} =
    Format.fprintf
      fmt
      "Delegate: %s, Initial: %a, Self_current: %a, Co_current: %a"
      delegate
      Tez.pp
      initial
      Tez.pp
      self_current
      (fun fmt ->
        String.Map.iter (fun k v ->
            Format.fprintf fmt "%s: %a, " k Partial_tez.pp v))
      co_current

  let init amount account delegate =
    if account = delegate then
      {
        delegate;
        initial = amount;
        self_current = amount;
        co_current = String.Map.empty;
      }
    else
      {
        delegate;
        initial = amount;
        self_current = Tez.zero;
        co_current = String.Map.singleton account (Partial_tez.of_tez amount);
      }

  let union a b =
    assert (a.delegate = b.delegate) ;
    {
      delegate = a.delegate;
      initial = Tez.(a.initial +! b.initial);
      self_current = Tez.(a.self_current +! b.self_current);
      co_current =
        String.Map.union
          (fun _ x y -> Some Partial_tez.(x + y))
          a.co_current
          b.co_current;
    }

  let get account frozen_tez =
    if account = frozen_tez.delegate then
      Partial_tez.of_tez frozen_tez.self_current
    else
      match String.Map.find account frozen_tez.co_current with
      | None -> Partial_tez.zero
      | Some p -> p

  let total_co_current_q co_current =
    String.Map.fold
      (fun _ x acc -> Partial_tez.(x + acc))
      co_current
      Partial_tez.zero

  let total_co_current t =
    let r = total_co_current_q t.co_current in
    let tez, rem = Partial_tez.to_tez_rem r in
    assert (Q.(equal rem zero)) ;
    tez

  let total_current t = Tez.(t.self_current +! total_co_current t)

  let total_current_with_limits ~limit_of_staking_over_baking t =
    let max_co_current =
      Tez.mul_q t.self_current limit_of_staking_over_baking
      |> Tez.of_q ~round:`Down
    in
    let co_current = Tez.min (total_co_current t) max_co_current in
    Tez.(t.self_current +! co_current)

  (* Precondition: 0 <= quantity < 1 && co_current + quantity is int *)
  let add_q_to_all_co_current quantity co_current =
    let s = total_co_current_q co_current in
    if Q.(equal quantity zero) then co_current
    else
      let f p_amount =
        let q = Q.div p_amount s in
        Partial_tez.add p_amount (Q.mul quantity q)
      in
      String.Map.map f co_current

  (* For rewards, distribute equally *)
  let add_tez_to_all_current ~edge ~limit tez a =
    let total_current = total_current a in
    let total_current_with_limit =
      Tez.(
        mul_q a.self_current Q.(add one limit)
        |> of_q ~round:`Up |> min total_current)
    in
    let self_portion = Tez.ratio a.self_current total_current_with_limit in
    (* Baker's advantage for the mutez *)
    let self_quantity = Tez.mul_q tez self_portion |> Tez.of_q ~round:`Up in
    let remains = Tez.(tez -! self_quantity) in
    (* Baker's edge. Round up for the baker's advantage again *)
    let bakers_edge = Tez.mul_q remains edge |> Tez.of_q ~round:`Up in
    let self_quantity = Tez.(self_quantity +! bakers_edge) in
    (* The remains are distributed equally *)
    let co_quantity = Partial_tez.of_tez Tez.(tez -! self_quantity) in
    let co_current = add_q_to_all_co_current co_quantity a.co_current in
    {a with co_current; self_current = Tez.(a.self_current +! self_quantity)}

  (* For slashing, slash equally *)
  let sub_tez_from_all_current ~limit tez a =
    let total_current = total_current a in
    let total_current_with_limit =
      Tez.(
        mul_q a.self_current Q.(add one limit)
        |> of_q ~round:`Up |> min total_current)
    in
    let self_portion = Tez.ratio a.self_current total_current_with_limit in
    let self_quantity = Tez.mul_q tez self_portion |> Tez.of_q ~round:`Up in
    let self_current =
      if Tez.(self_quantity >= a.self_current) then Tez.zero
      else Tez.(a.self_current -! self_quantity)
    in
    let co_quantity = Tez.(tez -! self_quantity) in
    let s = total_co_current_q a.co_current in
    if Partial_tez.(geq (of_tez co_quantity) s) then
      ( {a with self_current; co_current = String.Map.empty},
        self_quantity,
        co_quantity )
    else
      let f p_amount =
        let q = Q.div p_amount s in
        Partial_tez.sub p_amount (Tez.mul_q co_quantity q)
        (* > 0 *)
      in
      ( {a with self_current; co_current = String.Map.map f a.co_current},
        self_quantity,
        co_quantity )

  (* Adds frozen to account. Happens each stake in frozen deposits *)
  let add_current_q amount account a =
    if account = a.delegate then (
      let amount, rem = Partial_tez.to_tez_rem amount in
      assert (Q.(equal rem zero)) ;
      {a with self_current = Tez.(a.self_current +! amount)})
    else
      {
        a with
        co_current =
          String.Map.update
            account
            (function
              | None -> Some amount | Some q -> Some Partial_tez.(add q amount))
            a.co_current;
      }

  let add_current amount account a =
    add_current_q (Partial_tez.of_tez amount) account a

  let add_self_current amount a =
    let self_current = Tez.(a.self_current +! amount) in
    {a with self_current}

  (* Adds frozen to account. Happens each unstake to unstaked frozen deposits *)
  let add_init amount account a = union a (init amount account a.delegate)

  (* Allows amount greater than current frozen amount.
     Happens each unstake in frozen deposits *)
  let sub_current amount account a =
    if account = a.delegate then
      let amount = Tez.min amount a.self_current in
      ({a with self_current = Tez.(a.self_current -! amount)}, amount)
    else
      match String.Map.find account a.co_current with
      | None -> (a, Tez.zero)
      | Some frozen ->
          let amount_q = Partial_tez.of_tez amount in
          if Q.(geq amount_q frozen) then
            let removed, remainder = Partial_tez.to_tez_rem frozen in
            let co_current = String.Map.remove account a.co_current in
            let co_current = add_q_to_all_co_current remainder co_current in
            ({a with co_current}, removed)
          else
            let co_current =
              String.Map.add account Q.(frozen - amount_q) a.co_current
            in
            ({a with co_current}, amount)

  (* Remove a partial amount from the co frozen tez table. *)
  let sub_current_q amount_q account a =
    if account = a.delegate then assert false
    else
      match String.Map.find account a.co_current with
      | None -> assert false
      | Some frozen ->
          if Q.(geq amount_q frozen) then
            let co_current = String.Map.remove account a.co_current in
            {a with co_current}
          else
            let co_current =
              String.Map.add account Q.(frozen - amount_q) a.co_current
            in
            {a with co_current}

  let sub_current_and_init amount account a =
    let a, amount = sub_current amount account a in
    ({a with initial = Tez.(a.initial -! amount)}, amount)

  let slash ~limit cst base_amount (pct : Protocol.Percentage.t) a =
    Log.info
      "Slashing frozen tez for delegate %s with percentage %a"
      a.delegate
      Q.pp_print
    @@ Protocol.Percentage.to_q pct ;
    let pct_q = Protocol.Percentage.to_q pct in
    let total_current = total_current a in
    let slashed_amount =
      Tez.mul_q base_amount pct_q
      |> Tez.of_q ~round:`Down |> Tez.min total_current
    in
    let a, burnt_amount, rewarded_amount =
      if total_current > Tez.zero then
        let a, slashed_baker, slashed_staker =
          sub_tez_from_all_current slashed_amount ~limit a
        in
        let rat =
          cst.Protocol.Alpha_context.Constants.Parametric.adaptive_issuance
            .global_limit_of_staking_over_baking + 2
        in
        let rewarded_baker =
          Tez.mul_q slashed_baker Q.(1 // rat) |> Tez.of_q ~round:`Down
        in

        let rewarded_staker =
          Tez.mul_q slashed_staker Q.(1 // rat) |> Tez.of_q ~round:`Down
        in
        let rewarded_amount = Tez.(rewarded_baker +! rewarded_staker) in

        let burnt_amount = Tez.(slashed_amount -! rewarded_amount) in
        (a, burnt_amount, rewarded_amount)
      else (a, Tez.zero, Tez.zero)
    in
    Log.info
      "Total current: %a, slashed amount: %a, rewarded amount: %a, burnt \
       amount: %a"
      Tez.pp
      total_current
      Tez.pp
      slashed_amount
      Tez.pp
      rewarded_amount
      Tez.pp
      burnt_amount ;
    (a, burnt_amount, rewarded_amount)
end

(** Representation of Unstaked frozen deposits *)
module Unstaked_frozen = struct
  type r = {
    cycle : Cycle.t;
    (* initial total requested amount (slash ∝ initial) *)
    initial : Tez.t;
    (* current amount, slashes applied here *)
    current : Tez.t;
    (* initial requests, don't apply slash unless finalize or balance query *)
    requests : Tez.t String.Map.t;
    (* slash pct memory for requests *)
    slash_pct : int;
  }

  type t = r list

  type get_info = {cycle : Cycle.t; request : Tez.t; current : Tez.t}

  type get_info_list = get_info list

  type finalizable_info = {
    amount : Tez.t;
    slashed_requests : Tez.t String.Map.t;
  }

  let zero = []

  let init_r cycle request account =
    {
      cycle;
      initial = request;
      current = request;
      requests = String.Map.singleton account request;
      slash_pct = 0;
    }

  let apply_slash_to_request slash_pct amount =
    let slashed_amount =
      Tez.mul_q amount Q.(slash_pct // 100) |> Tez.of_q ~round:`Up
    in
    Tez.(amount -! slashed_amount)

  let apply_slash_to_current cst slash_pct initial current =
    let slashed_amount =
      Tez.mul_q initial Q.(slash_pct // 100)
      |> Tez.of_q ~round:`Down |> Tez.min current
    in
    let rat =
      cst.Protocol.Alpha_context.Constants.Parametric.adaptive_issuance
        .global_limit_of_staking_over_baking + 2
    in
    let rewarded_amount =
      Tez.mul_q slashed_amount Q.(1 // rat) |> Tez.of_q ~round:`Down
    in
    let burnt_amount = Tez.(slashed_amount -! rewarded_amount) in
    let actual_slashed_amount = Tez.(rewarded_amount +! burnt_amount) in
    let remaining =
      Tez.sub_opt current actual_slashed_amount
      |> Option.value ~default:Tez.zero
    in
    (remaining, burnt_amount, rewarded_amount)

  let remove_zeros (a : t) : t =
    List.filter (fun ({current; _} : r) -> Tez.(current > zero)) a

  let get account unstaked : get_info_list =
    List.filter_map
      (fun {cycle; requests; slash_pct; _} ->
        String.Map.find account requests
        |> Option.map (fun request ->
               {
                 cycle;
                 request;
                 current = apply_slash_to_request slash_pct request;
               }))
      unstaked

  let get_total account unstaked =
    get account unstaked
    |> List.fold_left
         (fun acc ({current; _} : get_info) -> Tez.(acc +! current))
         Tez.zero

  let sum_current unstaked =
    List.fold_left
      (fun acc ({current; _} : r) -> Tez.(acc +! current))
      Tez.zero
      unstaked

  (* Happens each unstake operation *)
  let rec add_unstake cycle amount account : t -> t = function
    | [] -> [init_r cycle amount account]
    | ({cycle = c; requests; initial; current; slash_pct} as h) :: t ->
        let open Tez in
        if Cycle.equal c cycle then (
          assert (Int.equal slash_pct 0) ;
          {
            cycle;
            initial = initial +! amount;
            current = current +! amount;
            slash_pct;
            requests =
              String.Map.update
                account
                (function
                  | None -> Some amount | Some x -> Some Tez.(x +! amount))
                requests;
          }
          :: t)
        else h :: add_unstake cycle amount account t

  (* Happens in stake from unstake *)
  let sub_unstake amount account : r -> r =
   fun {cycle; requests; initial; current; slash_pct} ->
    assert (slash_pct = 0) ;
    let open Tez in
    {
      cycle;
      initial = initial -! amount;
      current = current -! amount;
      slash_pct;
      requests =
        String.Map.update
          account
          (function
            | None ->
                assert (Tez.(amount = zero)) ;
                None
            | Some x ->
                if Tez.(x = amount) then None else Some Tez.(x -! amount))
          requests;
    }

  (* Makes given cycle finalizable (and unslashable) *)
  let rec pop_cycle cycle : t -> finalizable_info * t = function
    | [] -> ({amount = Tez.zero; slashed_requests = String.Map.empty}, [])
    | ({cycle = c; requests; initial = _; current; slash_pct} as h) :: t ->
        if Cycle.(c = cycle) then
          let amount = current in
          let slashed_requests =
            String.Map.map (apply_slash_to_request slash_pct) requests
          in
          ({amount; slashed_requests}, t)
        else if Cycle.(c < cycle) then
          Stdlib.failwith
            "Unstaked_frozen: found unfinalized cycle before given [cycle]. \
             Make sure to call [apply_unslashable] every cycle"
        else
          let info, rest = pop_cycle cycle t in
          (info, h :: rest)

  let slash cst ~slashable_deposits_period slashed_cycle pct_times_100 a =
    remove_zeros a
    |> List.map
         (fun
           ({cycle; requests = _; initial; current; slash_pct = old_slash_pct}
            as r)
         ->
           if
             Cycle.(
               cycle > slashed_cycle
               || add cycle slashable_deposits_period < slashed_cycle)
           then (r, (Tez.zero, Tez.zero))
           else
             let new_current, burnt, rewarded =
               apply_slash_to_current cst pct_times_100 initial current
             in
             let slash_pct = min 100 (pct_times_100 + old_slash_pct) in
             ({r with slash_pct; current = new_current}, (burnt, rewarded)))
    |> List.split
end

(** Representation of unstaked finalizable tez *)
module Unstaked_finalizable = struct
  (* Slashing might put inaccessible tez in this container: they are represented in the remainder.
     They still count towards the total supply, but are currently owned by noone.
     At most one mutez per unstaking account per slashed cycle *)
  type t = {map : Tez.t String.Map.t; remainder : Tez.t}

  let zero = {map = String.Map.empty; remainder = Tez.zero}

  (* Called when unstaked frozen for some cycle becomes finalizable *)
  let add_from_poped_ufd
      ({amount; slashed_requests} : Unstaked_frozen.finalizable_info)
      {map; remainder} =
    let total_requested =
      String.Map.fold (fun _ x acc -> Tez.(x +! acc)) slashed_requests Tez.zero
    in
    let remainder = Tez.(remainder +! amount -! total_requested) in
    let map =
      String.Map.union (fun _ a b -> Some Tez.(a +! b)) map slashed_requests
    in
    {map; remainder}

  let total {map; remainder} =
    String.Map.fold (fun _ x acc -> Tez.(x +! acc)) map remainder

  let get account {map; _} =
    match String.Map.find account map with None -> Tez.zero | Some x -> x
end

(** Pseudotoken helpers *)
let tez_to_pseudo ~round amount staking_delegate_denominator frozen_deposits =
  let total_q = Frozen_tez.(total_co_current_q frozen_deposits.co_current) in
  let total, rem = Partial_tez.to_tez_rem total_q in
  assert (Q.(equal rem zero)) ;
  if Tez.(equal total zero) then Tez.to_z amount
  else
    let r = Tez.ratio amount total in
    let p = Q.(r * of_bigint staking_delegate_denominator) in
    Tez.(of_q ~round p |> to_z)

let pseudo_to_partial_tez amount_pseudo staking_delegate_denominator
    frozen_deposits =
  let total_q = Frozen_tez.(total_co_current_q frozen_deposits.co_current) in
  let total, rem = Partial_tez.to_tez_rem total_q in
  assert (Q.(equal rem zero)) ;
  if Z.(equal staking_delegate_denominator zero) then Q.of_bigint amount_pseudo
  else
    let q = Q.(amount_pseudo /// staking_delegate_denominator) in
    Tez.mul_q total q

(* tez_q <= amount *)
let stake_values_real amount staking_delegate_denominator frozen_deposits =
  let pseudo =
    tez_to_pseudo
      ~round:`Down
      amount
      staking_delegate_denominator
      frozen_deposits
  in
  let tez_q =
    pseudo_to_partial_tez pseudo staking_delegate_denominator frozen_deposits
  in
  (pseudo, tez_q)

(* returned_amount <= amount *)
let unstake_values_real amount staking_delegate_denominator frozen_deposits =
  let pseudo =
    tez_to_pseudo ~round:`Up amount staking_delegate_denominator frozen_deposits
  in
  let tez_q =
    pseudo_to_partial_tez pseudo staking_delegate_denominator frozen_deposits
  in
  if Tez.equal (Tez.of_q ~round:`Down tez_q) amount then (pseudo, tez_q)
  else
    let pseudo = Z.(pseudo - one) in
    ( pseudo,
      pseudo_to_partial_tez pseudo staking_delegate_denominator frozen_deposits
    )
