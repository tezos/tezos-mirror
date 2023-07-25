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

  let of_mutez = of_mutez_exn

  let ratio num den =
    Q.make (Z.of_int64 (to_mutez num)) (Z.of_int64 (to_mutez den))

  let mul_q tez portion =
    let tez_z = to_mutez tez |> Z.of_int64 in
    Q.(mul portion ~$$tez_z |> to_int64) |> of_mutez
end

type balance_breakdown = {
  liquid : Tez.t;
  bonds : Tez.t;
  staked : Q.t;
  unstaked_frozen : Tez.t;
  unstaked_finalizable : Tez.t;
  pool_tez : Tez.t;
  pool_pseudo : Q.t;
}

let balance_pp fmt
    {
      liquid;
      bonds;
      staked;
      unstaked_frozen;
      unstaked_finalizable;
      pool_tez;
      pool_pseudo;
    } =
  Format.fprintf
    fmt
    "{@;\
     @[<v 2>  liquid : %a@;\
     bonds : %a@;\
     staked : %a@;\
     unstaked_frozen : %a@;\
     unstaked_finalizable : %a@;\
     pool_tez : %a@;\
     pool_pseudo : %a@]@;\
     }@."
    Tez.pp
    liquid
    Tez.pp
    bonds
    Q.pp_print
    staked
    Tez.pp
    unstaked_frozen
    Tez.pp
    unstaked_finalizable
    Tez.pp
    pool_tez
    Q.pp_print
    pool_pseudo

let balance_update_pp fmt (a, b) =
  Format.fprintf
    fmt
    "{@;\
     @[<v 2>  liquid : %a -> %a@;\
     bonds : %a -> %a@;\
     staked : %a -> %a@;\
     unstaked_frozen : %a -> %a@;\
     unstaked_finalizable : %a -> %a@;\
     pool_tez : %a -> %a@;\
     pool_pseudo : %a -> %a@]@;\
     }@."
    Tez.pp
    a.liquid
    Tez.pp
    b.liquid
    Tez.pp
    a.bonds
    Tez.pp
    b.bonds
    Q.pp_print
    a.staked
    Q.pp_print
    b.staked
    Tez.pp
    a.unstaked_frozen
    Tez.pp
    b.unstaked_frozen
    Tez.pp
    a.unstaked_finalizable
    Tez.pp
    b.unstaked_finalizable
    Tez.pp
    a.pool_tez
    Tez.pp
    b.pool_tez
    Q.pp_print
    a.pool_pseudo
    Q.pp_print
    b.pool_pseudo

let assert_balance_equal ~loc a b =
  let open Lwt_result_syntax in
  let* () = Assert.equal_tez ~loc a.liquid a.liquid in
  let* () = Assert.equal_tez ~loc a.bonds b.bonds in
  let* () =
    Assert.equal ~loc Q.equal "Assert equal staked" Q.pp_print a.staked b.staked
  in
  let* () = Assert.equal_tez ~loc a.unstaked_frozen b.unstaked_frozen in
  let* () =
    Assert.equal_tez ~loc a.unstaked_finalizable b.unstaked_finalizable
  in
  let* () = Assert.equal_tez ~loc a.pool_tez b.pool_tez in
  let* () =
    Assert.equal
      ~loc
      Q.equal
      "Assert equal pool pseudotokens"
      Q.pp_print
      a.pool_pseudo
      b.pool_pseudo
  in
  return_unit

let balance_add bbd1 bbd2 =
  let open Lwt_result_syntax in
  let* liquid = Tez.(bbd1.liquid + bbd2.liquid) in
  let* bonds = Tez.(bbd1.bonds + bbd2.bonds) in
  let staked = Q.add bbd1.staked bbd2.staked in
  let pool_pseudo = Q.add bbd1.pool_pseudo bbd2.pool_pseudo in
  let* pool_tez = Tez.(bbd1.pool_tez + bbd2.pool_tez) in
  let* unstaked_frozen = Tez.(bbd1.unstaked_frozen + bbd2.unstaked_frozen) in
  let* unstaked_finalizable =
    Tez.(bbd1.unstaked_finalizable + bbd2.unstaked_finalizable)
  in
  return
    {
      liquid;
      bonds;
      staked;
      unstaked_frozen;
      unstaked_finalizable;
      pool_tez;
      pool_pseudo;
    }

(* Will raise an error if one of the tez field in bbd1 is less than the same field in bbd2 *)
let balance_sub bbd1 bbd2 =
  let open Lwt_result_syntax in
  let* liquid = Tez.(bbd1.liquid - bbd2.liquid) in
  let* bonds = Tez.(bbd1.bonds - bbd2.bonds) in
  let staked = Q.sub bbd1.staked bbd2.staked in
  let pool_pseudo = Q.sub bbd1.pool_pseudo bbd2.pool_pseudo in
  let* pool_tez = Tez.(bbd1.pool_tez - bbd2.pool_tez) in
  let* unstaked_frozen = Tez.(bbd1.unstaked_frozen - bbd2.unstaked_frozen) in
  let* unstaked_finalizable =
    Tez.(bbd1.unstaked_finalizable - bbd2.unstaked_finalizable)
  in
  return
    {
      liquid;
      bonds;
      staked;
      unstaked_frozen;
      unstaked_finalizable;
      pool_tez;
      pool_pseudo;
    }

let add_liquid_rewards amount bbd =
  let open Lwt_result_syntax in
  let* liquid = Tez.(bbd.liquid + amount) in
  return {bbd with liquid}

let add_frozen_rewards amount bbd =
  let open Lwt_result_syntax in
  let* pool_tez = Tez.(bbd.pool_tez + amount) in
  return {bbd with pool_tez}

let tez_of_staked ~pool_tez ~pool_pseudo staked =
  if Q.(staked = zero) then Tez.zero
  else if Q.(pool_pseudo = zero) then (
    assert (Tez.(pool_tez = zero)) ;
    Tez.zero)
  else
    let portion = Q.div staked pool_pseudo in
    Tez.mul_q pool_tez portion

let staked_of_tez ~pool_tez ~pool_pseudo amount =
  if Tez.(amount = zero) then Q.zero
  else if Tez.(pool_tez = zero) then
    if Q.(pool_pseudo = zero) then Q.one
    else assert false
      (* Happens when completely slashed: stake is forbidden in this case *)
  else
    let portion = Tez.ratio amount pool_tez in
    Q.mul portion pool_pseudo

let apply_transfer amount (bbd_src, bbd_dst) =
  let open Lwt_result_syntax in
  if Tez.(amount = zero) then return (bbd_src, bbd_dst)
  else
    let amount = Tez.min bbd_src.liquid amount in
    let* liquid_src = Tez.(bbd_src.liquid - amount) in
    let* liquid_dst = Tez.(bbd_dst.liquid + amount) in
    return
      ({bbd_src with liquid = liquid_src}, {bbd_dst with liquid = liquid_dst})

let apply_stake amount (bbd_staker, bbd_delegate) =
  let open Lwt_result_syntax in
  if Tez.(amount = zero) then return (bbd_staker, bbd_delegate)
  else
    let amount = Tez.min bbd_staker.liquid amount in
    let new_stake =
      staked_of_tez
        ~pool_tez:bbd_delegate.pool_tez
        ~pool_pseudo:bbd_delegate.pool_pseudo
        amount
    in
    let* liquid = Tez.(bbd_staker.liquid - amount) in
    let staked = Q.(bbd_staker.staked + new_stake) in
    let* pool_tez = Tez.(bbd_delegate.pool_tez + amount) in
    let pool_pseudo = Q.(bbd_delegate.pool_pseudo + new_stake) in
    return
      ( {bbd_staker with liquid; staked},
        {bbd_delegate with pool_tez; pool_pseudo} )

let apply_self_stake amount bbd =
  let open Lwt_result_syntax in
  if Tez.(amount = zero) then return bbd
  else
    let* a, b = apply_stake amount (bbd, bbd) in
    let* added = balance_add a b in
    balance_sub added bbd

let apply_unstake amount (bbd_staker, bbd_delegate) =
  let open Lwt_result_syntax in
  if Tez.(amount = zero) then return (bbd_staker, bbd_delegate)
  else
    let amount = Tez.min bbd_delegate.pool_tez amount in
    let to_unstake =
      staked_of_tez
        ~pool_tez:bbd_delegate.pool_tez
        ~pool_pseudo:bbd_delegate.pool_pseudo
        amount
    in
    let amount, to_unstake =
      if Q.(to_unstake >= bbd_staker.staked) then
        ( tez_of_staked
            ~pool_tez:bbd_delegate.pool_tez
            ~pool_pseudo:bbd_delegate.pool_pseudo
            bbd_staker.staked,
          bbd_staker.staked )
      else (amount, to_unstake)
    in
    let staked = Q.(bbd_staker.staked - to_unstake) in
    let* unstaked_frozen = Tez.(bbd_staker.unstaked_frozen + amount) in
    let pool_pseudo = Q.(bbd_delegate.pool_pseudo - to_unstake) in
    let* pool_tez = Tez.(bbd_delegate.pool_tez - amount) in
    return
      ( {bbd_staker with staked; unstaked_frozen},
        {bbd_delegate with pool_pseudo; pool_tez} )

let apply_self_unstake amount bbd =
  let open Lwt_result_syntax in
  if Tez.(amount = zero) then return bbd
  else
    let* a, b = apply_unstake amount (bbd, bbd) in
    let* added = balance_add a b in
    balance_sub added bbd

let apply_unslashable amount bbd =
  let open Lwt_result_syntax in
  let* unstaked_frozen = Tez.(bbd.unstaked_frozen - amount) in
  let* unstaked_finalizable = Tez.(bbd.unstaked_finalizable + amount) in
  return {bbd with unstaked_frozen; unstaked_finalizable}

let apply_finalize bbd =
  let open Lwt_result_syntax in
  let unstaked_finalizable = Tez.zero in
  let* liquid = Tez.(bbd.unstaked_finalizable + bbd.liquid) in
  return {bbd with liquid; unstaked_finalizable}

let total_balance_of_breakdown
    {liquid; bonds; staked; unstaked_frozen; unstaked_finalizable; _} ~pool_tez
    ~pool_pseudo =
  let staked_tez = tez_of_staked staked ~pool_tez ~pool_pseudo in
  Tez.(
    liquid + bonds >>=? ( + ) staked_tez >>=? ( + ) unstaked_frozen
    >>=? ( + ) unstaked_finalizable)

let get_balance_breakdown ctxt contract =
  let open Lwt_result_syntax in
  let* liquid = Context.Contract.balance ctxt contract in
  let* bonds = Context.Contract.frozen_bonds ctxt contract in
  let* staked_balance = Context.Contract.staked_balance ctxt contract in
  let staked_balance = Option.value ~default:Tez.zero staked_balance in
  let* unstaked_frozen =
    Context.Contract.unstaked_frozen_balance ctxt contract
  in
  let unstaked_frozen = Option.value ~default:Tez.zero unstaked_frozen in
  let* unstaked_finalizable =
    Context.Contract.unstaked_finalizable_balance ctxt contract
  in
  let unstaked_finalizable =
    Option.value ~default:Tez.zero unstaked_finalizable
  in
  let* total_balance = Context.Contract.full_balance ctxt contract in
  let bd =
    {
      liquid;
      bonds;
      staked = Q.zero;
      (* unused *)
      pool_tez = Tez.zero;
      (* unused *)
      pool_pseudo = Q.zero;
      (* unused *)
      unstaked_frozen;
      unstaked_finalizable;
    }
  in
  return (bd, staked_balance, total_balance)

let assert_balance_breakdown ~loc ctxt contract
    ({
       liquid;
       bonds;
       staked;
       unstaked_frozen;
       unstaked_finalizable;
       pool_tez = _;
       pool_pseudo = _;
     } as asserted_balance) ~pool_tez ~pool_pseudo =
  let open Lwt_result_syntax in
  let* bd, staked_balance, total_balance =
    get_balance_breakdown ctxt contract
  in
  let asserted_staked_balance = tez_of_staked staked ~pool_tez ~pool_pseudo in
  let* asserted_total_balance =
    total_balance_of_breakdown asserted_balance ~pool_tez ~pool_pseudo
  in
  let* () = Assert.equal_tez ~loc bd.liquid liquid in
  let* () = Assert.equal_tez ~loc bd.bonds bonds in
  let* () = Assert.equal_tez ~loc staked_balance asserted_staked_balance in
  let* () = Assert.equal_tez ~loc total_balance asserted_total_balance in
  let* () = Assert.equal_tez ~loc bd.unstaked_frozen unstaked_frozen in
  let* () =
    Assert.equal_tez ~loc bd.unstaked_finalizable unstaked_finalizable
  in
  return_unit

let get_launch_cycle ~loc blk =
  let open Lwt_result_syntax in
  let* launch_cycle_opt = Context.get_adaptive_issuance_launch_cycle (B blk) in
  Assert.get_some ~loc launch_cycle_opt

let stake ctxt contract amount =
  Op.transaction
    ctxt
    ~entrypoint:Protocol.Alpha_context.Entrypoint.stake
    ~fee:Tez.zero
    contract
    contract
    amount

let set_delegate_parameters ctxt delegate ~limit_of_staking_over_baking
    ~edge_of_baking_over_staking_billionth =
  let entrypoint = Protocol.Alpha_context.Entrypoint.set_delegate_parameters in
  let parameters =
    Protocol.Alpha_context.Script.lazy_expr
      (Expr.from_string
         (Printf.sprintf
            "Pair %d (Pair %d Unit)"
            limit_of_staking_over_baking
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
  let parameters =
    Protocol.Alpha_context.Script.lazy_expr
      (Expr.from_string (Printf.sprintf "%Ld" (Tez.to_mutez amount)))
  in
  Op.transaction
    ctxt
    ~entrypoint:Protocol.Alpha_context.Entrypoint.unstake
    ~parameters
    ~fee:Tez.zero
    contract
    contract
    Tez.zero

let finalize_unstake ctxt ?(amount = Tez.zero) contract =
  Op.transaction
    ctxt
    ~entrypoint:Protocol.Alpha_context.Entrypoint.finalize_unstake
    ~fee:Tez.zero
    contract
    contract
    amount
