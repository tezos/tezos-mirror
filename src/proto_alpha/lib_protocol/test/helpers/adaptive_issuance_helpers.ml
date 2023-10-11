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

(** Tez manipulation module *)
module Tez = struct
  include Protocol.Alpha_context.Tez

  let ( + ) a b =
    let open Lwt_result_wrap_syntax in
    let*?@ s = a +? b in
    return s

  let ( - ) a b =
    let open Lwt_result_wrap_syntax in
    let*?@ s = a -? b in
    return s

  let ( +! ) a b =
    let a = to_mutez a in
    let b = to_mutez b in
    Int64.add a b |> of_mutez_exn

  let ( -! ) a b =
    let a = to_mutez a in
    let b = to_mutez b in
    Int64.sub a b |> of_mutez_exn

  let of_mutez = of_mutez_exn

  let of_z a = Z.to_int64 a |> of_mutez

  let of_q a = Q.to_bigint a |> of_z

  let ratio num den =
    Q.make (Z.of_int64 (to_mutez num)) (Z.of_int64 (to_mutez den))

  let mul_q tez portion =
    let tez_z = to_mutez tez |> Z.of_int64 in
    Q.(mul portion ~$$tez_z)
end

(** Representation of Tez with non integer values *)
module Partial_tez = struct
  include Q

  let of_tez a = Tez.to_mutez a |> of_int64

  let to_tez_rem {num; den} =
    let tez, rem = Z.div_rem num den in
    (Tez.of_z tez, rem /// den)

  let to_tez ?(round_up = false) a =
    let tez, rem = to_tez_rem a in
    if round_up && Q.(gt rem zero) then Tez.(tez +! one_mutez) else tez

  let get_rem a = snd (to_tez_rem a)

  let pp fmt a =
    let tez, rem = to_tez_rem a in
    (* If rem = 0, we keep the (+ 0), to indicate that it's a partial tez *)
    Format.fprintf fmt "%a ( +%aµꜩ )" Tez.pp tez Q.pp_print rem
end

module Cycle = Protocol.Alpha_context.Cycle

(** [Frozen_tez] represents frozen stake and frozen unstaked funds.
    Properties:
    - sum of all current partial tez is an integer
    - slashing is always a portion of initial
    - Can only add integer amounts
    - Can always subtract integer amount (if lower than frozen amount)
    - If subtracting partial amount, must be the whole frozen amount (for given contract).
      The remainder is then distributed equally amongst remaining accounts, to keep property 1.
    - All entries of current are positive, non zero.
*)
module Frozen_tez = struct
  (* The map in current maps the stakers' name with their staked value.
     It contains only delegators of the delegate which owns the frozen tez *)
  type t = {initial : Tez.t; current : Partial_tez.t String.Map.t}

  let zero = {initial = Tez.zero; current = String.Map.empty}

  let init amount account =
    {
      initial = amount;
      current = String.Map.singleton account (Partial_tez.of_tez amount);
    }

  let get account frozen_tez =
    match String.Map.find account frozen_tez.current with
    | None -> Partial_tez.zero
    | Some p -> p

  let union a b =
    {
      initial = Tez.(a.initial +! b.initial);
      current =
        String.Map.union
          (fun _ x y -> Some Partial_tez.(x + y))
          a.current
          b.current;
    }

  let total_current_q current =
    String.Map.fold
      (fun _ x acc -> Partial_tez.(x + acc))
      current
      Partial_tez.zero

  let total_current a =
    let r = total_current_q a.current in
    let tez, rem = Partial_tez.to_tez_rem r in
    assert (Q.(equal rem zero)) ;
    tez

  let add_q_to_all_current quantity current =
    let s = total_current_q current in
    let f p_amount =
      let q = Q.div p_amount s in
      Partial_tez.add p_amount (Q.mul quantity q)
    in
    String.Map.map f current

  (* For rewards, distribute equally *)
  let add_tez_to_all_current tez a =
    let quantity = Partial_tez.of_tez tez in
    let current = add_q_to_all_current quantity a.current in
    {a with current}

  (* For slashing, slash equally *)
  let sub_tez_from_all_current tez a =
    let s = total_current_q a.current in
    if Partial_tez.(geq (of_tez tez) s) then {a with current = String.Map.empty}
    else
      let f p_amount =
        let q = Q.div p_amount s in
        Partial_tez.sub p_amount (Tez.mul_q tez q)
        (* > 0 *)
      in
      {a with current = String.Map.map f a.current}

  (* Adds frozen to account. Happens each stake in frozen deposits *)
  let add_current amount account a =
    {
      a with
      current =
        String.Map.update
          account
          (function
            | None -> Some (Partial_tez.of_tez amount)
            | Some q -> Some Partial_tez.(add q (of_tez amount)))
          a.current;
    }

  (* Adds frozen to account. Happens each unstake to unstaked frozen deposits *)
  let add_init amount account a = union a (init amount account)

  (* Allows amount greater than current frozen amount.
     Happens each unstake in frozen deposits *)
  let sub_current amount account a =
    match String.Map.find account a.current with
    | None -> (a, Tez.zero)
    | Some frozen ->
        let amount_q = Partial_tez.of_tez amount in
        if Q.(geq amount_q frozen) then
          let removed, remainder = Partial_tez.to_tez_rem frozen in
          let current = String.Map.remove account a.current in
          let current = add_q_to_all_current remainder current in
          ({a with current}, removed)
        else
          let current =
            String.Map.add account Q.(frozen - amount_q) a.current
          in
          ({a with current}, amount)

  (* Refresh initial amount at beginning of cycle *)
  let refresh_at_new_cycle a = {a with initial = total_current a}
end

(** Representation of Unstaked frozen deposits *)
module Unstaked_frozen = struct
  type t = (Cycle.t * Frozen_tez.t) list

  let zero = []

  let fold unstaked =
    List.fold_left
      (fun acc (_, frozen) -> Frozen_tez.union acc frozen)
      Frozen_tez.zero
      unstaked

  let get account unstaked =
    List.map (fun (c, frozen) -> (c, Frozen_tez.get account frozen)) unstaked

  let get_total account unstaked = Frozen_tez.get account (fold unstaked)

  let sum_current unstaked = Frozen_tez.total_current (fold unstaked)

  (* Happens each unstake operation *)
  let rec add_unstake cycle amount account = function
    | [] -> [(cycle, Frozen_tez.init amount account)]
    | (c, a) :: t ->
        if Cycle.equal c cycle then
          (c, Frozen_tez.add_init amount account a) :: t
        else (c, a) :: add_unstake cycle amount account t

  (* Makes given cycle finalizable (and unslashable) *)
  let rec pop_cycle cycle = function
    | [] -> (Frozen_tez.zero, [])
    | (c, a) :: t ->
        if Cycle.(c = cycle) then (a, t)
        else if Cycle.(c < cycle) then
          Stdlib.failwith
            "Unstaked_frozen: found unfinalized cycle before given [cycle]. \
             Make sure to call [apply_unslashable] every cycle"
        else
          let amount, rest = pop_cycle cycle t in
          (amount, (c, a) :: rest)

  (* Refresh initial amount at beginning of cycle (unused) *)
  let refresh_at_new_cycle l =
    List.map (fun (c, t) -> (c, Frozen_tez.refresh_at_new_cycle t)) l
end

(** Representation of unstaked finalizable tez *)
module Unstaked_finalizable = struct
  (* Slashing might put inaccessible tez in this container: they are represented in the remainder.
     They still count towards the total supply, but are currently owned by noone.
     At most one mutez per unstaking account per slashed cycle *)
  type t = {map : Tez.t String.Map.t; remainder : Tez.t}

  let zero = {map = String.Map.empty; remainder = Tez.zero}

  (* Called when unstaked frozen for some cycle becomes finalizable *)
  let add_from_frozen (frozen : Frozen_tez.t) {map; remainder} =
    let map_rounded_down =
      String.Map.map (fun qt -> Partial_tez.to_tez qt) frozen.current
    in
    let map =
      String.Map.union (fun _ a b -> Some Tez.(a +! b)) map map_rounded_down
    in
    let full_frozen = Frozen_tez.total_current frozen in
    let actual_frozen =
      String.Map.fold (fun _ x acc -> Tez.(x +! acc)) map_rounded_down Tez.zero
    in
    let undistributed = Tez.(full_frozen -! actual_frozen) in
    let remainder = Tez.(remainder +! undistributed) in
    {map; remainder}

  let total {map; remainder} =
    String.Map.fold (fun _ x acc -> Tez.(x +! acc)) map remainder

  let get account {map; _} =
    match String.Map.find account map with None -> Tez.zero | Some x -> x
end

(** Abstraction of the staking parameters for tests *)
type staking_parameters = {
  limit_of_staking_over_baking : Q.t;
  edge_of_baking_over_staking : Q.t;
}

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
     the maps with the cycle at which the unstake operation occured. *)
  frozen_deposits : Frozen_tez.t;
  unstaked_frozen : Unstaked_frozen.t;
  unstaked_finalizable : Unstaked_finalizable.t;
}

let init_account ?delegate ~pkh ~contract ~parameters ?(liquid = Tez.zero)
    ?(bonds = Tez.zero) ?(frozen_deposits = Frozen_tez.zero)
    ?(unstaked_frozen = Unstaked_frozen.zero)
    ?(unstaked_finalizable = Unstaked_finalizable.zero) () =
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
  }

type account_map = account_state String.Map.t

(** Balance returned by RPCs. Partial tez are rounded down *)
type balance = {
  liquid_b : Tez.t;
  bonds_b : Tez.t;
  staked_b : Partial_tez.t;
  unstaked_frozen_b : Partial_tez.t;
  unstaked_finalizable_b : Tez.t;
}

let balance_zero =
  {
    liquid_b = Tez.zero;
    bonds_b = Tez.zero;
    staked_b = Partial_tez.zero;
    unstaked_frozen_b = Partial_tez.zero;
    unstaked_finalizable_b = Tez.zero;
  }

let balance_of_account account_name (account_map : account_map) =
  match String.Map.find account_name account_map with
  | None -> raise Not_found
  | Some account ->
      let balance =
        {balance_zero with liquid_b = account.liquid; bonds_b = account.bonds}
      in
      let balance =
        match account.delegate with
        | None -> balance
        | Some d -> (
            match String.Map.find d account_map with
            | None -> raise Not_found
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
              Q.(
                frozen
                + Unstaked_frozen.get_total
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
          (Q.zero, Tez.zero)
      in
      {balance with unstaked_frozen_b; unstaked_finalizable_b}

let balance_pp fmt
    {liquid_b; bonds_b; staked_b; unstaked_frozen_b; unstaked_finalizable_b} =
  Format.fprintf
    fmt
    "{@;\
     @[<v 2>  liquid : %a@;\
     bonds : %a@;\
     staked : %a@;\
     unstaked_frozen : %a@;\
     unstaked_finalizable : %a@;\
     }@."
    Tez.pp
    liquid_b
    Tez.pp
    bonds_b
    Partial_tez.pp
    staked_b
    Partial_tez.pp
    unstaked_frozen_b
    Tez.pp
    unstaked_finalizable_b

let balance_update_pp fmt
    ( {
        liquid_b = a_liquid_b;
        bonds_b = a_bonds_b;
        staked_b = a_staked_b;
        unstaked_frozen_b = a_unstaked_frozen_b;
        unstaked_finalizable_b = a_unstaked_finalizable_b;
      },
      {
        liquid_b = b_liquid_b;
        bonds_b = b_bonds_b;
        staked_b = b_staked_b;
        unstaked_frozen_b = b_unstaked_frozen_b;
        unstaked_finalizable_b = b_unstaked_finalizable_b;
      } ) =
  Format.fprintf
    fmt
    "{@;\
     @[<v 2>  liquid : %a -> %a@;\
     bonds : %a -> %a@;\
     staked : %a -> %a@;\
     unstaked_frozen : %a -> %a@;\
     unstaked_finalizable : %a -> %a@;\
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
    Partial_tez.pp
    a_unstaked_frozen_b
    Partial_tez.pp
    b_unstaked_frozen_b
    Tez.pp
    a_unstaked_finalizable_b
    Tez.pp
    b_unstaked_finalizable_b

let assert_balance_equal ~loc
    {
      liquid_b = a_liquid_b;
      bonds_b = a_bonds_b;
      staked_b = a_staked_b;
      unstaked_frozen_b = a_unstaked_frozen_b;
      unstaked_finalizable_b = a_unstaked_finalizable_b;
    }
    {
      liquid_b = b_liquid_b;
      bonds_b = b_bonds_b;
      staked_b = b_staked_b;
      unstaked_frozen_b = b_unstaked_frozen_b;
      unstaked_finalizable_b = b_unstaked_finalizable_b;
    } =
  let open Lwt_result_syntax in
  let* () = Assert.equal_tez ~loc a_liquid_b b_liquid_b in
  let* () = Assert.equal_tez ~loc a_bonds_b b_bonds_b in
  let* () =
    Assert.equal_tez
      ~loc
      (Partial_tez.to_tez a_staked_b)
      (Partial_tez.to_tez b_staked_b)
  in
  let* () =
    Assert.equal_tez
      ~loc
      (Partial_tez.to_tez a_unstaked_frozen_b)
      (Partial_tez.to_tez b_unstaked_frozen_b)
  in
  let* () =
    Assert.equal_tez ~loc a_unstaked_finalizable_b b_unstaked_finalizable_b
  in
  return_unit

let update_account ~f account_name account_map =
  String.Map.update
    account_name
    (function None -> raise Not_found | Some x -> Some (f x))
    account_map

let add_liquid_rewards amount account_name account_map =
  let f account =
    let liquid = Tez.(account.liquid +! amount) in
    {account with liquid}
  in
  update_account ~f account_name account_map

let add_frozen_rewards amount account_name account_map =
  let f account =
    let frozen_deposits =
      Frozen_tez.add_tez_to_all_current amount account.frozen_deposits
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
  | _ -> raise Not_found

let apply_stake amount staker_name account_map =
  match String.Map.find staker_name account_map with
  | None -> raise Not_found
  | Some staker -> (
      match staker.delegate with
      | None ->
          (* Invalid operation: no delegate *)
          account_map
      | Some delegate_name ->
          if Tez.(staker.liquid < amount) then
            (* Invalid amount: operation will fail *)
            account_map
          else
            let f_staker staker =
              let liquid = Tez.(staker.liquid -! amount) in
              {staker with liquid}
            in
            let f_delegate delegate =
              let frozen_deposits =
                Frozen_tez.add_current
                  amount
                  staker_name
                  delegate.frozen_deposits
              in
              {delegate with frozen_deposits}
            in
            let account_map =
              update_account ~f:f_staker staker_name account_map
            in
            update_account ~f:f_delegate delegate_name account_map)

let apply_unstake cycle amount staker_name account_map =
  match String.Map.find staker_name account_map with
  | None -> raise Not_found
  | Some staker -> (
      match staker.delegate with
      | None -> (* Invalid operation: no delegate *) account_map
      | Some delegate_name -> (
          match String.Map.find delegate_name account_map with
          | None -> raise Not_found
          | Some delegate ->
              let frozen_deposits, amount_unstaked =
                Frozen_tez.sub_current
                  amount
                  staker_name
                  delegate.frozen_deposits
              in
              let delegate = {delegate with frozen_deposits} in
              let account_map =
                String.Map.add delegate_name delegate account_map
              in
              let f delegate =
                let unstaked_frozen =
                  Unstaked_frozen.add_unstake
                    cycle
                    amount_unstaked
                    staker_name
                    delegate.unstaked_frozen
                in
                {delegate with unstaked_frozen}
              in
              update_account ~f delegate_name account_map))

(* Updates unstaked unslashable values in all accounts *)
let apply_unslashable cycle account_map =
  let f delegate =
    let amount_unslashable, unstaked_frozen =
      Unstaked_frozen.pop_cycle cycle delegate.unstaked_frozen
    in
    let unstaked_finalizable =
      Unstaked_finalizable.add_from_frozen
        amount_unslashable
        delegate.unstaked_finalizable
    in
    {delegate with unstaked_frozen; unstaked_finalizable}
  in
  String.Map.map f account_map

let apply_finalize staker_name account_map =
  match String.Map.find staker_name account_map with
  | None -> raise Not_found
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
  let ({liquid_b; bonds_b; staked_b; unstaked_frozen_b; unstaked_finalizable_b}
      as balance) =
    balance_of_account account_name account_map
  in
  ( balance,
    Tez.(
      liquid_b +! bonds_b
      +! Partial_tez.to_tez staked_b
      +! Partial_tez.to_tez unstaked_frozen_b
      +! unstaked_finalizable_b) )

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
  let unstaked_frozen_b =
    Option.value ~default:Tez.zero unstaked_frozen_b |> Partial_tez.of_tez
  in
  let* unstaked_finalizable_b =
    Context.Contract.unstaked_finalizable_balance ctxt contract
  in
  let unstaked_finalizable_b =
    Option.value ~default:Tez.zero unstaked_finalizable_b
  in
  let* total_balance = Context.Contract.full_balance ctxt contract in
  let bd =
    {liquid_b; bonds_b; staked_b; unstaked_frozen_b; unstaked_finalizable_b}
  in
  return (bd, total_balance)

let assert_balance_check ~loc ctxt account_name account_map =
  let open Lwt_result_syntax in
  match String.Map.find account_name account_map with
  | None -> raise Not_found
  | Some account ->
      let* balance_ctxt, total_balance_ctxt =
        get_balance_from_context ctxt account.contract
      in
      let balance, total_balance =
        balance_and_total_balance_of_account account_name account_map
      in
      let* () = assert_balance_equal ~loc balance_ctxt balance in
      let* () = Assert.equal_tez ~loc total_balance_ctxt total_balance in
      return_unit

let get_launch_cycle ~loc blk =
  let open Lwt_result_syntax in
  let* launch_cycle_opt = Context.get_adaptive_issuance_launch_cycle (B blk) in
  Assert.get_some ~loc launch_cycle_opt

(** AI operations *)

let stake ctxt contract amount =
  Op.transaction
    ctxt
    ~entrypoint:Protocol.Alpha_context.Entrypoint.stake
    ~fee:Tez.zero
    contract
    contract
    amount

let set_delegate_parameters ctxt delegate
    ~parameters:{limit_of_staking_over_baking; edge_of_baking_over_staking} =
  let entrypoint = Protocol.Alpha_context.Entrypoint.set_delegate_parameters in
  let limit_of_staking_over_baking_millionth =
    Q.mul limit_of_staking_over_baking (Q.of_int 1_000_000) |> Q.to_int
  in
  let edge_of_baking_over_staking_billionth =
    Q.mul edge_of_baking_over_staking (Q.of_int 1_000_000_000) |> Q.to_int
  in
  let parameters =
    Protocol.Alpha_context.Script.lazy_expr
      (Expr.from_string
         (Printf.sprintf
            "Pair %d (Pair %d Unit)"
            limit_of_staking_over_baking_millionth
            edge_of_baking_over_staking_billionth))
  in
  Op.transaction
    ctxt
    ~entrypoint
    ~parameters
    ~fee:Tez.zero
    delegate
    delegate
    Tez.zero

let unstake ctxt contract amount =
  Op.transaction
    ctxt
    ~entrypoint:Protocol.Alpha_context.Entrypoint.unstake
    ~fee:Tez.zero
    contract
    contract
    amount

let finalize_unstake ctxt ?(amount = Tez.zero) contract =
  Op.transaction
    ctxt
    ~entrypoint:Protocol.Alpha_context.Entrypoint.finalize_unstake
    ~fee:Tez.zero
    contract
    contract
    amount
