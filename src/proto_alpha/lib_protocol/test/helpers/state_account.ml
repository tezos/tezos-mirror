(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Adaptive_issuance_helpers
module Cycle = Protocol.Alpha_context.Cycle

module Tez = struct
  include Tez_helpers
  include Tez_helpers.Compare
end

let fail_account_not_found func_name account_name =
  Log.error "State_account.%s: account %s not found" func_name account_name ;
  assert false

let join_errors e1 e2 =
  let open Lwt_result_syntax in
  match (e1, e2) with
  | Ok (), Ok () -> return_unit
  | Error e, Ok () | Ok (), Error e -> fail e
  | Error e1, Error e2 -> fail (e1 @ e2)

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

  let zero =
    {
      delegate = "";
      initial = Tez.zero;
      self_current = Tez.zero;
      co_current = String.Map.empty;
    }

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

  let total_current a =
    let r = total_co_current_q a.co_current in
    let tez, rem = Partial_tez.to_tez_rem r in
    assert (Q.(equal rem zero)) ;
    Tez.(tez +! a.self_current)

  (* 0 <= quantity < 1 && co_current + quantity is int *)
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
  let add_tez_to_all_current ~edge tez a =
    let self_portion = Tez.ratio a.self_current (total_current a) in
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
  let sub_tez_from_all_current tez a =
    let self_portion = Tez.ratio a.self_current (total_current a) in
    let self_quantity = Tez.mul_q tez self_portion |> Tez.of_q ~round:`Down in
    let self_current =
      if Tez.(self_quantity >= a.self_current) then Tez.zero
      else Tez.(a.self_current -! self_quantity)
    in
    let co_quantity = Tez.(tez -! self_quantity) in
    let s = total_co_current_q a.co_current in
    if Partial_tez.(geq (of_tez co_quantity) s) then
      {a with self_current; co_current = String.Map.empty}
    else
      let f p_amount =
        let q = Q.div p_amount s in
        Partial_tez.sub p_amount (Tez.mul_q co_quantity q)
        (* > 0 *)
      in
      {a with self_current; co_current = String.Map.map f a.co_current}

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

  (* Remove a partial amount to the co frozen tez table. *)
  let sub_current_q amount_q account a =
    if account = a.delegate then assert false
    else
      match String.Map.find account a.co_current with
      | None -> a
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

  let slash base_amount (pct : Protocol.Percentage.t) a =
    let pct_q = Protocol.Percentage.to_q pct in
    let slashed_amount = Tez.mul_q base_amount pct_q |> Tez.of_q ~round:`Down in
    let total_current = total_current a in
    let slashed_amount_final = Tez.min slashed_amount total_current in
    (sub_tez_from_all_current slashed_amount a, slashed_amount_final)
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

  let apply_slash_to_current slash_pct initial current =
    let slashed_amount =
      Tez.mul_q initial Q.(slash_pct // 100) |> Tez.of_q ~round:`Down
    in
    Tez.sub_opt current slashed_amount |> Option.value ~default:Tez.zero

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

  let slash ~slashable_deposits_period slashed_cycle pct_times_100 a =
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
           then (r, Tez.zero)
           else
             let new_current =
               apply_slash_to_current pct_times_100 initial current
             in
             let slashed = Tez.(current -! new_current) in
             let slash_pct = min 100 (pct_times_100 + old_slash_pct) in
             ({r with slash_pct; current = new_current}, slashed))
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

module CycleMap = Map.Make (Cycle)

(** Abstract information of accounts *)
type account_state = {
  pkh : Signature.Public_key_hash.t;
  contract : Protocol.Alpha_context.Contract.t;
  delegate : string option;
  parameters : staking_parameters;
  liquid : Tez.t;
  bonds : Tez.t;
  (* The three following fields contain maps from the account's stakers to,
     respectively, their frozen stake, their unstaked frozen balance, and
     their unstaked finalizable funds. Additionally, [unstaked_frozen] indexes
     the maps with the cycle at which the unstake operation occurred. *)
  frozen_deposits : Frozen_tez.t;
  unstaked_frozen : Unstaked_frozen.t;
  unstaked_finalizable : Unstaked_finalizable.t;
  staking_delegator_numerator : Z.t;
  staking_delegate_denominator : Z.t;
  frozen_rights : Tez.t CycleMap.t;
  slashed_cycles : Cycle.t list;
}

let init_account ?delegate ~pkh ~contract ~parameters ?(liquid = Tez.zero)
    ?(bonds = Tez.zero) ?(frozen_deposits = Frozen_tez.zero)
    ?(unstaked_frozen = Unstaked_frozen.zero)
    ?(unstaked_finalizable = Unstaked_finalizable.zero)
    ?(staking_delegator_numerator = Z.zero)
    ?(staking_delegate_denominator = Z.zero) ?(frozen_rights = CycleMap.empty)
    ?(slashed_cycles = []) () =
  {
    pkh;
    contract;
    delegate;
    parameters;
    liquid;
    bonds;
    frozen_deposits;
    unstaked_frozen;
    unstaked_finalizable;
    staking_delegator_numerator;
    staking_delegate_denominator;
    frozen_rights;
    slashed_cycles;
  }

type account_map = account_state String.Map.t

(** Balance returned by RPCs. Partial tez are rounded down *)
type balance = {
  liquid_b : Tez.t;
  bonds_b : Tez.t;
  staked_b : Partial_tez.t;
  unstaked_frozen_b : Tez.t;
  unstaked_finalizable_b : Tez.t;
  staking_delegator_numerator_b : Z.t;
  staking_delegate_denominator_b : Z.t;
}

let balance_zero =
  {
    liquid_b = Tez.zero;
    bonds_b = Tez.zero;
    staked_b = Partial_tez.zero;
    unstaked_frozen_b = Tez.zero;
    unstaked_finalizable_b = Tez.zero;
    staking_delegator_numerator_b = Z.zero;
    staking_delegate_denominator_b = Z.zero;
  }

let balance_of_account account_name (account_map : account_map) =
  match String.Map.find account_name account_map with
  | None -> fail_account_not_found "balance_of_account.src" account_name
  | Some
      {
        pkh = _;
        contract = _;
        delegate;
        parameters = _;
        liquid;
        bonds;
        frozen_deposits = _;
        unstaked_frozen = _;
        unstaked_finalizable = _;
        staking_delegator_numerator;
        staking_delegate_denominator;
        frozen_rights = _;
        slashed_cycles = _;
      } ->
      let balance =
        {
          balance_zero with
          liquid_b = liquid;
          bonds_b = bonds;
          staking_delegator_numerator_b = staking_delegator_numerator;
          staking_delegate_denominator_b = staking_delegate_denominator;
        }
      in
      let balance =
        match delegate with
        | None -> balance
        | Some d -> (
            match String.Map.find d account_map with
            | None -> fail_account_not_found "balance_of_account.delegate" d
            | Some delegate_account ->
                {
                  balance with
                  staked_b =
                    Frozen_tez.get account_name delegate_account.frozen_deposits;
                })
      in
      (* Because an account can still have frozen or finalizable funds from a delegate
         that is not its own, we iterate over all of them *)
      let unstaked_frozen_b, unstaked_finalizable_b =
        String.Map.fold
          (fun _delegate_name delegate (frozen, finalzbl) ->
            let frozen =
              Tez.(
                frozen
                +! Unstaked_frozen.get_total
                     account_name
                     delegate.unstaked_frozen)
            in
            let finalzbl =
              Tez.(
                finalzbl
                +! Unstaked_finalizable.get
                     account_name
                     delegate.unstaked_finalizable)
            in
            (frozen, finalzbl))
          account_map
          (Tez.zero, Tez.zero)
      in
      {balance with unstaked_frozen_b; unstaked_finalizable_b}

let balance_pp fmt
    {
      liquid_b;
      bonds_b;
      staked_b;
      unstaked_frozen_b;
      unstaked_finalizable_b;
      staking_delegator_numerator_b;
      staking_delegate_denominator_b;
    } =
  Format.fprintf
    fmt
    "{@;\
     @[<v 2>  liquid : %a@;\
     bonds : %a@;\
     staked : %a@;\
     unstaked_frozen : %a@;\
     unstaked_finalizable : %a@;\
     staking_delegator_numerator : %a@;\
     staking_delegate_denominator : %a@;\
     }@."
    Tez.pp
    liquid_b
    Tez.pp
    bonds_b
    Partial_tez.pp
    staked_b
    Tez.pp
    unstaked_frozen_b
    Tez.pp
    unstaked_finalizable_b
    Z.pp_print
    staking_delegator_numerator_b
    Z.pp_print
    staking_delegate_denominator_b

let balance_update_pp fmt
    ( {
        liquid_b = a_liquid_b;
        bonds_b = a_bonds_b;
        staked_b = a_staked_b;
        unstaked_frozen_b = a_unstaked_frozen_b;
        unstaked_finalizable_b = a_unstaked_finalizable_b;
        staking_delegator_numerator_b = a_staking_delegator_numerator_b;
        staking_delegate_denominator_b = a_staking_delegate_denominator_b;
      },
      {
        liquid_b = b_liquid_b;
        bonds_b = b_bonds_b;
        staked_b = b_staked_b;
        unstaked_frozen_b = b_unstaked_frozen_b;
        unstaked_finalizable_b = b_unstaked_finalizable_b;
        staking_delegator_numerator_b = b_staking_delegator_numerator_b;
        staking_delegate_denominator_b = b_staking_delegate_denominator_b;
      } ) =
  Format.fprintf
    fmt
    "{@;\
     @[<v 2>  liquid : %a -> %a@;\
     bonds : %a -> %a@;\
     staked : %a -> %a@;\
     unstaked_frozen : %a -> %a@;\
     unstaked_finalizable : %a -> %a@;\
     staking_delegator_numerator : %a -> %a@;\
     staking_delegate_denominator : %a -> %a@;\
     }@."
    Tez.pp
    a_liquid_b
    Tez.pp
    b_liquid_b
    Tez.pp
    a_bonds_b
    Tez.pp
    b_bonds_b
    Partial_tez.pp
    a_staked_b
    Partial_tez.pp
    b_staked_b
    Tez.pp
    a_unstaked_frozen_b
    Tez.pp
    b_unstaked_frozen_b
    Tez.pp
    a_unstaked_finalizable_b
    Tez.pp
    b_unstaked_finalizable_b
    Z.pp_print
    a_staking_delegator_numerator_b
    Z.pp_print
    b_staking_delegator_numerator_b
    Z.pp_print
    a_staking_delegate_denominator_b
    Z.pp_print
    b_staking_delegate_denominator_b

let assert_balance_equal ~loc account_name
    {
      liquid_b = a_liquid_b;
      bonds_b = a_bonds_b;
      staked_b = a_staked_b;
      unstaked_frozen_b = a_unstaked_frozen_b;
      unstaked_finalizable_b = a_unstaked_finalizable_b;
      staking_delegator_numerator_b = a_staking_delegator_numerator_b;
      staking_delegate_denominator_b = a_staking_delegate_denominator_b;
    }
    {
      liquid_b = b_liquid_b;
      bonds_b = b_bonds_b;
      staked_b = b_staked_b;
      unstaked_frozen_b = b_unstaked_frozen_b;
      unstaked_finalizable_b = b_unstaked_finalizable_b;
      staking_delegator_numerator_b = b_staking_delegator_numerator_b;
      staking_delegate_denominator_b = b_staking_delegate_denominator_b;
    } =
  let open Lwt_result_syntax in
  let f s = Format.asprintf "%s: %s" account_name s in
  let* () =
    List.fold_left
      (fun a b ->
        let*! a in
        let*! b in
        join_errors a b)
      return_unit
      [
        Assert.equal
          ~loc
          Tez.equal
          (f "Liquid balances do not match")
          Tez.pp
          a_liquid_b
          b_liquid_b;
        Assert.equal
          ~loc
          Tez.equal
          (f "Bonds balances do not match")
          Tez.pp
          a_bonds_b
          b_bonds_b;
        Assert.equal
          ~loc
          Tez.equal
          (f "Staked balances do not match")
          Tez.pp
          (Partial_tez.to_tez ~round:`Down a_staked_b)
          (Partial_tez.to_tez ~round:`Down b_staked_b);
        Assert.equal
          ~loc
          Tez.equal
          (f "Unstaked frozen balances do not match")
          Tez.pp
          a_unstaked_frozen_b
          b_unstaked_frozen_b;
        Assert.equal
          ~loc
          Tez.equal
          (f "Unstaked finalizable balances do not match")
          Tez.pp
          a_unstaked_finalizable_b
          b_unstaked_finalizable_b;
        Assert.equal
          ~loc
          Z.equal
          (f "Staking delegator numerators do not match")
          Z.pp_print
          a_staking_delegator_numerator_b
          b_staking_delegator_numerator_b;
        Assert.equal
          ~loc
          Z.equal
          (f "Staking delegate denominators do not match")
          Z.pp_print
          a_staking_delegate_denominator_b
          b_staking_delegate_denominator_b;
      ]
  in
  return_unit

let update_account ~f account_name account_map =
  String.Map.update
    account_name
    (function
      | None -> fail_account_not_found "update_account" account_name
      | Some x -> Some (f x))
    account_map

let add_liquid_rewards amount account_name account_map =
  let f account =
    let liquid = Tez.(account.liquid +! amount) in
    {account with liquid}
  in
  update_account ~f account_name account_map

let add_frozen_rewards amount account_name account_map =
  let f account =
    let actual_edge =
      Q.(
        mul account.parameters.edge_of_baking_over_staking (1_000_000_000 // 1)
        |> to_int |> of_int
        |> mul (1 // 1_000_000_000))
    in
    let frozen_deposits =
      Frozen_tez.add_tez_to_all_current
        ~edge:actual_edge
        amount
        account.frozen_deposits
    in
    {account with frozen_deposits}
  in
  update_account ~f account_name account_map

let apply_burn amount src_name account_map =
  let f src = {src with liquid = Tez.(src.liquid -! amount)} in
  update_account ~f src_name account_map

let apply_transfer amount src_name dst_name account_map =
  match
    (String.Map.find src_name account_map, String.Map.find dst_name account_map)
  with
  | Some src, Some _ ->
      if Tez.(src.liquid < amount) then
        (* Invalid amount: operation will fail *)
        account_map
      else
        let f_src src =
          let liquid = Tez.(src.liquid -! amount) in
          {src with liquid}
        in
        let f_dst dst =
          let liquid = Tez.(dst.liquid +! amount) in
          {dst with liquid}
        in
        let account_map = update_account ~f:f_src src_name account_map in
        update_account ~f:f_dst dst_name account_map
  | None, _ -> fail_account_not_found "apply_transfer.src" src_name
  | _, None -> fail_account_not_found "apply_transfer.dst" dst_name

let stake_from_unstake amount current_cycle consensus_rights_delay delegate_name
    account_map =
  match String.Map.find delegate_name account_map with
  | None -> fail_account_not_found "stake_from_unstake" delegate_name
  | Some ({unstaked_frozen; frozen_deposits; slashed_cycles; _} as account) ->
      let oldest_slashable_cycle =
        Cycle.(sub current_cycle (consensus_rights_delay + 1))
        |> Option.value ~default:Cycle.root
      in
      if
        List.exists
          (fun x -> Cycle.(x >= oldest_slashable_cycle))
          slashed_cycles
      then (account_map, amount)
      else
        let unstaked_frozen =
          List.sort
            (fun (Unstaked_frozen.{cycle = cycle1; _} : Unstaked_frozen.r)
                 {cycle = cycle2; _} -> Cycle.compare cycle2 cycle1)
            unstaked_frozen
        in
        let rec aux acc_unstakes rem_amount rem_unstakes =
          match rem_unstakes with
          | [] -> (acc_unstakes, rem_amount)
          | (Unstaked_frozen.{initial; _} as h) :: t ->
              if Tez.(rem_amount = zero) then
                (acc_unstakes @ rem_unstakes, Tez.zero)
              else if Tez.(rem_amount >= initial) then
                let h = Unstaked_frozen.sub_unstake initial delegate_name h in
                let rem_amount = Tez.(rem_amount -! initial) in
                aux (acc_unstakes @ [h]) rem_amount t
              else
                let h =
                  Unstaked_frozen.sub_unstake rem_amount delegate_name h
                in
                (acc_unstakes @ [h] @ t, Tez.zero)
        in
        let unstaked_frozen, rem_amount = aux [] amount unstaked_frozen in
        let frozen_deposits =
          Frozen_tez.add_current
            Tez.(amount -! rem_amount)
            delegate_name
            frozen_deposits
        in
        let account = {account with unstaked_frozen; frozen_deposits} in
        let account_map =
          update_account ~f:(fun _ -> account) delegate_name account_map
        in
        (account_map, rem_amount)

let tez_to_pseudo ~round amount delegate_account =
  let {staking_delegate_denominator; frozen_deposits; _} = delegate_account in
  let total_q = Frozen_tez.total_co_current_q frozen_deposits.co_current in
  let total, rem = Partial_tez.to_tez_rem total_q in
  assert (Q.(equal rem zero)) ;
  if Tez.(equal total zero) then Tez.to_z amount
  else
    let r = Tez.ratio amount total in
    let p = Q.(r * of_bigint staking_delegate_denominator) in
    Tez.(of_q ~round p |> to_z)

let pseudo_to_partial_tez amount_pseudo delegate_account =
  let {staking_delegate_denominator; frozen_deposits; _} = delegate_account in
  let total_q = Frozen_tez.total_co_current_q frozen_deposits.co_current in
  let total, rem = Partial_tez.to_tez_rem total_q in
  assert (Q.(equal rem zero)) ;
  if Z.(equal staking_delegate_denominator zero) then Q.of_bigint amount_pseudo
  else
    let q = Q.(amount_pseudo /// staking_delegate_denominator) in
    Tez.mul_q total q

(* tez_q <= amount *)
let stake_values_real amount delegate_account =
  let pseudo = tez_to_pseudo ~round:`Down amount delegate_account in
  let tez_q = pseudo_to_partial_tez pseudo delegate_account in
  (pseudo, tez_q)

(* returned_amount <= amount *)
let unstake_values_real amount delegate_account =
  let pseudo = tez_to_pseudo ~round:`Up amount delegate_account in
  let tez_q = pseudo_to_partial_tez pseudo delegate_account in
  if Tez.equal (Tez.of_q ~round:`Down tez_q) amount then (pseudo, tez_q)
  else
    let pseudo = Z.(pseudo - one) in
    (pseudo, pseudo_to_partial_tez pseudo delegate_account)

let apply_stake amount current_cycle consensus_rights_delay staker_name
    account_map =
  match String.Map.find staker_name account_map with
  | None -> fail_account_not_found "apply_stake" staker_name
  | Some staker -> (
      match staker.delegate with
      | None ->
          (* Invalid operation: no delegate *)
          account_map
      | Some delegate_name ->
          let old_account_map = account_map in
          (* If self stake, then try to stake from unstake.
             Returns the amount that remains to be staked from liquid *)
          let account_map, amount =
            if delegate_name = staker_name then
              stake_from_unstake
                amount
                current_cycle
                consensus_rights_delay
                staker_name
                account_map
            else (account_map, amount)
          in
          if Tez.(staker.liquid < amount) then
            (* Not enough liquid balance: operation will fail *)
            old_account_map
          else if delegate_name = staker_name then
            (* If self stake: increase frozen deposits and decrease liquid balance.
               "add_current" is easy to resolve since there is no pseudotokens *)
            let f delegate =
              let frozen_deposits =
                Frozen_tez.add_current
                  amount
                  staker_name
                  delegate.frozen_deposits
              in
              let liquid = Tez.(delegate.liquid -! amount) in
              {delegate with frozen_deposits; liquid}
            in
            update_account ~f delegate_name account_map
          else
            (* If external stake: *)
            let delegate_account =
              String.Map.find delegate_name account_map
              |> Option.value_f ~default:(fun _ -> assert false)
            in
            (* Call stake_values_real to know the actual amount staked and the pseudotokens minted *)
            (* amount_q would be the effective stake on the delegate's side, while
               amount is the amount removed from the liquid balance *)
            let pseudo, amount_q = stake_values_real amount delegate_account in
            let f_staker staker =
              let liquid = Tez.(staker.liquid -! amount) in
              let staking_delegator_numerator =
                Z.add staker.staking_delegator_numerator pseudo
              in
              {staker with liquid; staking_delegator_numerator}
            in
            let f_delegate delegate =
              (* The difference between the actual amount and the effective amount is
                 "distributed" amongst current stake holders.
                 Indeed, when trading in "amount", the staker receives "pseudo" pseudotokens
                 valued at "amount_q". So the total amount of value is increased by "amount_q".
                 Then, "portion" is added to the total, so it must be distributed.
                 This means that the order is important: first you add_current_q, then
                 you add the portion to all *)
              let portion = Partial_tez.(of_tez amount - amount_q) in
              let frozen_deposits =
                Frozen_tez.add_current_q
                  amount_q
                  staker_name
                  delegate.frozen_deposits
              in
              let co_current =
                Frozen_tez.add_q_to_all_co_current
                  portion
                  frozen_deposits.co_current
              in
              let frozen_deposits = {frozen_deposits with co_current} in
              let staking_delegate_denominator =
                Z.add delegate.staking_delegate_denominator pseudo
              in
              {delegate with frozen_deposits; staking_delegate_denominator}
            in
            let account_map =
              update_account ~f:f_staker staker_name account_map
            in
            update_account ~f:f_delegate delegate_name account_map)

let apply_unstake cycle amount staker_name account_map =
  match String.Map.find staker_name account_map with
  | None -> fail_account_not_found "apply_unstake.staker" staker_name
  | Some staker -> (
      match staker.delegate with
      | None -> (* Invalid operation: no delegate *) account_map
      | Some delegate_name -> (
          match String.Map.find delegate_name account_map with
          | None ->
              fail_account_not_found "apply_unstake.delegate" delegate_name
          | Some delegate ->
              if delegate_name = staker_name then
                (* Case self stake *)
                (* No pseudotokens : no problem *)
                let frozen_deposits, amount_unstaked =
                  Frozen_tez.sub_current
                    amount
                    staker_name
                    delegate.frozen_deposits
                in
                let unstaked_frozen =
                  Unstaked_frozen.add_unstake
                    cycle
                    amount_unstaked
                    staker_name
                    delegate.unstaked_frozen
                in
                let delegate =
                  {delegate with frozen_deposits; unstaked_frozen}
                in
                update_account ~f:(fun _ -> delegate) delegate_name account_map
              else
                (* Case external stake *)
                let staked_amount =
                  Frozen_tez.get staker_name delegate.frozen_deposits
                in
                let pseudotokens, amount_q =
                  if Partial_tez.(staked_amount <= of_tez amount) then
                    (* Unstake all case *)
                    (staker.staking_delegator_numerator, staked_amount)
                  else
                    (* The staker requests "amount".
                       It translates to some "pseudotokens", valued at "amount_q".
                       If those pseudotokens would give strictly more than the requested amount,
                       then give one less pseudotoken. The actual amount unstaked is always lower than
                       the requested amount (except in the unstake all case) *)
                    unstake_values_real amount delegate
                in
                (* Actual unstaked amount (that will be finalized) *)
                let amount = Partial_tez.to_tez ~round:`Down amount_q in
                (* Delta from pseudotokens' value, to be redistributed amongst all remaining stakers
                   (including current if still staking) *)
                let portion = Partial_tez.(amount_q - of_tez amount) in
                let f_staker staker =
                  (* The staker's account representation doesn't change much,
                     the unstake request is stored on the delegate's side *)
                  let staking_delegator_numerator =
                    Z.sub staker.staking_delegator_numerator pseudotokens
                  in
                  {staker with staking_delegator_numerator}
                in
                let account_map =
                  update_account ~f:f_staker staker_name account_map
                in
                let f_delegate delegate =
                  let staking_delegate_denominator =
                    Z.sub delegate.staking_delegate_denominator pseudotokens
                  in
                  (* Just like in stake *)
                  (* Do the effective unstake *)
                  let frozen_deposits =
                    Frozen_tez.sub_current_q
                      amount_q
                      staker_name
                      delegate.frozen_deposits
                  in
                  (* Apply the delta *)
                  let co_current =
                    Frozen_tez.add_q_to_all_co_current
                      portion
                      frozen_deposits.co_current
                  in
                  let frozen_deposits = {frozen_deposits with co_current} in
                  (* Add unstake request
                     Note that "amount" might not be the initial requested amount *)
                  let unstaked_frozen =
                    Unstaked_frozen.add_unstake
                      cycle
                      amount
                      staker_name
                      delegate.unstaked_frozen
                  in
                  {
                    delegate with
                    staking_delegate_denominator;
                    frozen_deposits;
                    unstaked_frozen;
                  }
                in
                update_account ~f:f_delegate delegate_name account_map))

let apply_unslashable_f cycle delegate =
  let amount_unslashable, unstaked_frozen =
    Unstaked_frozen.pop_cycle cycle delegate.unstaked_frozen
  in
  let unstaked_finalizable =
    Unstaked_finalizable.add_from_poped_ufd
      amount_unslashable
      delegate.unstaked_finalizable
  in
  {delegate with unstaked_frozen; unstaked_finalizable}

(* Updates unstaked unslashable values for given account *)
let apply_unslashable cycle account_name account_map =
  update_account ~f:(apply_unslashable_f cycle) account_name account_map

(* Updates unstaked unslashable values in all accounts *)
let apply_unslashable_for_all cycle account_map =
  String.Map.map (apply_unslashable_f cycle) account_map

let apply_finalize staker_name account_map =
  match String.Map.find staker_name account_map with
  | None -> fail_account_not_found "apply_finalize" staker_name
  | Some _staker ->
      (* Because an account can still have finalizable funds from a delegate
         that is not its own, we iterate over all of them *)
      String.Map.fold
        (fun delegate_name delegate account_map_acc ->
          match
            String.Map.find staker_name delegate.unstaked_finalizable.map
          with
          | None -> account_map_acc
          | Some amount ->
              let f_staker staker =
                let liquid = Tez.(staker.liquid +! amount) in
                {staker with liquid}
              in
              let f_delegate delegate =
                let map =
                  String.Map.remove
                    staker_name
                    delegate.unstaked_finalizable.map
                in
                {
                  delegate with
                  unstaked_finalizable =
                    {delegate.unstaked_finalizable with map};
                }
              in
              let account_map_acc =
                update_account ~f:f_staker staker_name account_map_acc
              in
              update_account ~f:f_delegate delegate_name account_map_acc)
        account_map
        account_map

let balance_and_total_balance_of_account account_name account_map =
  let ({
         liquid_b;
         bonds_b;
         staked_b;
         unstaked_frozen_b;
         unstaked_finalizable_b;
         staking_delegator_numerator_b = _;
         staking_delegate_denominator_b = _;
       } as balance) =
    balance_of_account account_name account_map
  in
  ( balance,
    Tez.(
      liquid_b +! bonds_b
      +! Partial_tez.to_tez ~round:`Down staked_b
      +! unstaked_frozen_b +! unstaked_finalizable_b) )

let apply_slashing
    ( culprit,
      Protocol.Denunciations_repr.{rewarded; misbehaviour; operation_hash = _}
    ) constants account_map =
  let find_account_name_from_pkh_exn pkh account_map =
    match
      Option.map
        fst
        String.Map.(
          choose
          @@ filter
               (fun _ account ->
                 Signature.Public_key_hash.equal pkh account.pkh)
               account_map)
    with
    | None -> assert false
    | Some x -> x
  in
  let slashed_cycle =
    Block.current_cycle_of_level
      ~blocks_per_cycle:
        constants.Protocol.Alpha_context.Constants.Parametric.blocks_per_cycle
      ~current_level:(Protocol.Raw_level_repr.to_int32 misbehaviour.level)
  in
  let culprit_name = find_account_name_from_pkh_exn culprit account_map in
  let rewarded_name = find_account_name_from_pkh_exn rewarded account_map in
  let slashed_pct =
    match misbehaviour.kind with
    | Double_baking ->
        constants
          .Protocol.Alpha_context.Constants.Parametric
           .percentage_of_frozen_deposits_slashed_per_double_baking
    | Double_attesting | Double_preattesting ->
        constants.percentage_of_frozen_deposits_slashed_per_double_attestation
  in
  let get_total_supply acc_map =
    String.Map.fold
      (fun _name
           {
             pkh = _;
             contract = _;
             delegate = _;
             parameters = _;
             liquid;
             bonds;
             frozen_deposits;
             unstaked_frozen;
             unstaked_finalizable;
             staking_delegator_numerator = _;
             staking_delegate_denominator = _;
             frozen_rights = _;
             slashed_cycles = _;
           }
           tot ->
        Tez.(
          liquid +! bonds
          +! Frozen_tez.total_current frozen_deposits
          +! Unstaked_frozen.sum_current unstaked_frozen
          +! Unstaked_finalizable.total unstaked_finalizable
          +! tot))
      acc_map
      Tez.zero
  in
  let total_before_slash = get_total_supply account_map in
  let slash_culprit
      ({frozen_deposits; unstaked_frozen; frozen_rights; _} as acc) =
    let base_rights =
      CycleMap.find slashed_cycle frozen_rights
      |> Option.value ~default:Tez.zero
    in
    let frozen_deposits, slashed_frozen =
      Frozen_tez.slash base_rights slashed_pct frozen_deposits
    in
    let slashed_pct_q = Protocol.Percentage.to_q slashed_pct in
    let slashed_pct = Q.(100 // 1 * slashed_pct_q |> to_int) in
    let unstaked_frozen, slashed_unstaked =
      Unstaked_frozen.slash
        ~slashable_deposits_period:constants.consensus_rights_delay
        slashed_cycle
        slashed_pct
        unstaked_frozen
    in
    ( {acc with frozen_deposits; unstaked_frozen},
      slashed_frozen :: slashed_unstaked )
  in
  let culprit_account =
    String.Map.find culprit_name account_map
    |> Option.value_f ~default:(fun () ->
           fail_account_not_found "apply_slashing" culprit_name)
  in
  let slashed_culprit_account, total_slashed = slash_culprit culprit_account in
  let account_map =
    update_account
      ~f:(fun _ -> slashed_culprit_account)
      culprit_name
      account_map
  in
  let total_after_slash = get_total_supply account_map in
  let portion_reward =
    constants.adaptive_issuance.global_limit_of_staking_over_baking + 2
  in
  (* For each container slashed, the snitch gets a reward transferred. It gets rounded
     down each time *)
  let reward_to_snitch =
    List.map
      (fun x -> Tez.mul_q x Q.(1 // portion_reward) |> Tez.of_q ~round:`Down)
      total_slashed
    |> List.fold_left Tez.( +! ) Tez.zero
  in
  let account_map =
    add_liquid_rewards reward_to_snitch rewarded_name account_map
  in
  let actual_total_burnt_amount =
    Tez.(total_before_slash -! total_after_slash -! reward_to_snitch)
  in
  (account_map, actual_total_burnt_amount)

(* Given cycle is the cycle for which the rights are computed, usually current +
   consensus rights delay *)
let update_frozen_rights_cycle cycle account_map =
  String.Map.map
    (fun ({frozen_deposits; frozen_rights; _} as acc) ->
      let total_frozen = Frozen_tez.total_current frozen_deposits in
      let frozen_rights = CycleMap.add cycle total_frozen frozen_rights in
      {acc with frozen_rights})
    account_map

let get_balance_from_context ctxt contract =
  let open Lwt_result_syntax in
  let* liquid_b = Context.Contract.balance ctxt contract in
  let* bonds_b = Context.Contract.frozen_bonds ctxt contract in
  let* staked_b = Context.Contract.staked_balance ctxt contract in
  let staked_b =
    Option.value ~default:Tez.zero staked_b |> Partial_tez.of_tez
  in
  let* unstaked_frozen_b =
    Context.Contract.unstaked_frozen_balance ctxt contract
  in
  let unstaked_frozen_b = Option.value ~default:Tez.zero unstaked_frozen_b in
  let* unstaked_finalizable_b =
    Context.Contract.unstaked_finalizable_balance ctxt contract
  in
  let unstaked_finalizable_b =
    Option.value ~default:Tez.zero unstaked_finalizable_b
  in
  let* total_balance = Context.Contract.full_balance ctxt contract in
  let* staking_delegator_numerator_b =
    Context.Contract.staking_numerator ctxt contract
  in
  let*! staking_delegate_denominator_b =
    match (contract : Protocol.Alpha_context.Contract.t) with
    | Implicit pkh ->
        let*! result = Context.Delegate.staking_denominator ctxt pkh in
        Lwt.return
          (match result with
          | Ok v -> v
          | Error _ -> (* Not a delegate *) Z.zero)
    | Originated _ -> Lwt.return Z.zero
  in
  let bd =
    {
      liquid_b;
      bonds_b;
      staked_b;
      unstaked_frozen_b;
      unstaked_finalizable_b;
      staking_delegator_numerator_b;
      staking_delegate_denominator_b;
    }
  in
  return (bd, total_balance)

let assert_pseudotokens_consistency ~loc balance account account_name
    account_map =
  let open Lwt_result_syntax in
  let {delegate; staking_delegator_numerator = num_pt; _} = account in
  let exact_staking_balance = balance.staked_b in
  match delegate with
  | None -> return_unit
  | Some delegate_name -> (
      if account_name = delegate_name then return_unit
      else
        match String.Map.find delegate_name account_map with
        | None ->
            fail_account_not_found
              "assert_pseudotokens_consistency"
              delegate_name
        | Some delegate_account ->
            let total_co =
              Frozen_tez.total_co_current_q
                delegate_account.frozen_deposits.co_current
            in
            let den_pt = delegate_account.staking_delegate_denominator in
            if Z.(equal den_pt zero) then
              Assert.equal
                ~loc
                Q.equal
                (Format.asprintf
                   "%s : Delegate should not have external stake with a 0 \
                    staking denominator"
                   account_name)
                Q.pp_print
                total_co
                Q.zero
            else
              let expected = Q.(num_pt /// den_pt * total_co) in
              Assert.equal
                ~loc
                Q.equal
                (Format.asprintf
                   "%s : Pseudotokens do not match exact staking balance"
                   account_name)
                Q.pp_print
                exact_staking_balance
                expected)

let assert_balance_check ~loc ctxt account_name account_map =
  let open Lwt_result_syntax in
  match String.Map.find account_name account_map with
  | None -> fail_account_not_found "assert_balance_check" account_name
  | Some account ->
      let* balance_ctxt, total_balance_ctxt =
        get_balance_from_context ctxt account.contract
      in
      let balance, total_balance =
        balance_and_total_balance_of_account account_name account_map
      in
      let*! r0 =
        assert_pseudotokens_consistency
          ~loc
          balance
          account
          account_name
          account_map
      in
      let*! r1 = assert_balance_equal ~loc account_name balance_ctxt balance in
      let*! r1 = join_errors r0 r1 in
      let*! r2 =
        Assert.equal
          ~loc
          Tez.equal
          (Format.asprintf "%s : Total balances do not match" account_name)
          Tez.pp
          total_balance_ctxt
          total_balance
      in
      join_errors r1 r2

let log_debug_balance account_name account_map : unit =
  let balance, total_balance =
    balance_and_total_balance_of_account account_name account_map
  in
  Log.debug
    "Model balance of %s:\n%aTotal balance: %a\n"
    account_name
    balance_pp
    balance
    Tez.pp
    total_balance

let log_debug_rpc_balance name contract block : unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* balance, total_balance = get_balance_from_context (B block) contract in
  Log.debug
    "RPC balance of %s:\n%aTotal balance: %a\n"
    name
    balance_pp
    balance
    Tez.pp
    total_balance ;
  return_unit

let log_debug_balance_update account_name old_account_map new_account_map : unit
    =
  let old_balance, old_total_balance =
    balance_and_total_balance_of_account account_name old_account_map
  in
  let new_balance, new_total_balance =
    balance_and_total_balance_of_account account_name new_account_map
  in
  Log.debug
    "Balance update of %s:\n%aTotal balance: %a -> %a\n"
    account_name
    balance_update_pp
    (old_balance, new_balance)
    Tez.pp
    old_total_balance
    Tez.pp
    new_total_balance
