(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context
open Manager_operation_helpers

let lwt_run f =
  match Lwt_main.run f with
  | Error err ->
      QCheck.Test.fail_reportf "@.Lwt_main.run error: %a@." pp_print_trace err
  | Ok v -> v

(** {2 Datatypes} *)

(** Constraints on generated values.

    {ul
      {li [Free] states that nothing has to be generated}

      {li [Pure n] generate n}

      {li [Less {n;origin}] (resp Greater) states the expected
      constraints for the generated values that must be lesser (resp
      greater) than [n] and shrink toward [origin] in case of error}

      {li [Range {min;max;origin}] states the expected constraints for
      the generated values that must be between [min] and [max] and
      shrink toward [origin] in case of error.}} *)
type cstrs =
  | Free
  | Pure of int
  | Less of {n : int; origin : int}
  | Greater of {n : int; origin : int}
  | Range of {min : int; max : int; origin : int}

(** Gas frequency. *)
type gas_freq = {
  low : int;
  max : int;
  high : int;
  zero : int;
  custom : int * cstrs;
}

(** Operation constraints. *)
type operation_cstrs = {
  counter : cstrs;
  fee : cstrs;
  gas_limit : gas_freq;
  storage_limit : cstrs;
  force_reveal : bool option;
  amount : cstrs;
}

(** Context constraints. *)
type ctxt_cstrs = {
  hard_gas_limit_per_block : cstrs;
  src_cstrs : cstrs;
  dest_cstrs : cstrs;
  del_cstrs : cstrs;
  sc_cstrs : cstrs;
  zk_cstrs : cstrs;
}
(** {2 Default values} *)

(** Default constraint. *)
let default_cstrs = Free

(** Default gas frequency. *)
let default_gas_freq =
  {low = 0; max = 0; high = 1; zero = 0; custom = (0, Free)}

(** Default constraints for operation. *)
let default_operation_cstrs =
  {
    counter = default_cstrs;
    fee = default_cstrs;
    gas_limit = default_gas_freq;
    storage_limit = default_cstrs;
    force_reveal = None;
    amount = default_cstrs;
  }

(** Default constraints for context. *)
let default_ctxt_cstrs =
  {
    hard_gas_limit_per_block = default_cstrs;
    src_cstrs = default_cstrs;
    dest_cstrs = default_cstrs;
    del_cstrs = default_cstrs;
    sc_cstrs = default_cstrs;
    zk_cstrs = default_cstrs;
  }

(** {2 Generators} *)

(** Generator of positive integers. *)
let gen_pos : cstrs -> int option QCheck2.Gen.t =
 fun c ->
  let open QCheck2.Gen in
  match c with
  | Free -> pure None
  | Pure n -> pure (Some n)
  | Less {n; origin} ->
      let+ v = int_range ~origin 0 n in
      Some v
  | Greater {n; origin} ->
      let+ v = int_range ~origin n max_int in
      Some v
  | Range {min; max; origin} ->
      let+ v = int_range ~origin min max in
      Some v

(** Generator for Z.t that is used for gas limit. *)
let gen_z : cstrs -> Z.t option QCheck2.Gen.t =
 fun cstrs ->
  let open QCheck2.Gen in
  let+ v = gen_pos cstrs in
  Option.map Z.of_int v

(** Generator for Manager_counter.t. *)
let gen_counter : cstrs -> Manager_counter.t option QCheck2.Gen.t =
 fun cstrs ->
  let open QCheck2.Gen in
  let+ v = gen_pos cstrs in
  Option.map Manager_counter.Internal_for_tests.of_int v

(** Generator for Tez.t. *)
let gen_tez : cstrs -> Tez.t option QCheck2.Gen.t =
 fun cstrs ->
  let open QCheck2.Gen in
  let+ amount = gen_pos cstrs in
  match amount with
  | Some amount ->
      let amount = Int64.of_int amount in
      Tez.of_mutez amount
  | None -> None

(** Generator for gas integral. *)
let gen_gas_integral : cstrs -> Gas.Arith.integral option QCheck2.Gen.t =
 fun cstrs ->
  let open QCheck2.Gen in
  let+ v = gen_pos cstrs in
  Option.map Gas.Arith.integral_of_int_exn v

(** Generator for Op.gas_limit. *)
let gen_gas_limit : gas_freq -> Op.gas_limit option QCheck2.Gen.t =
 fun gas_freq ->
  let open QCheck2.Gen in
  frequency
    [
      (gas_freq.low, return (Some Op.Low));
      (gas_freq.max, return (Some Op.Max));
      (gas_freq.high, return (Some Op.High));
      (gas_freq.zero, return (Some Op.Zero));
      (let freq, cstrs = gas_freq.custom in
       ( freq,
         let+ gas = gen_gas_integral cstrs in
         match gas with None -> None | Some g -> Some (Op.Custom_gas g) ));
    ]

(** Generator for manager_operation_kind. *)
let gen_kind :
    manager_operation_kind list -> manager_operation_kind QCheck2.Gen.t =
 fun subjects -> QCheck2.Gen.oneofl subjects

(** Generator for mode. *)
let gen_mode : mode QCheck2.Gen.t =
  QCheck2.Gen.oneofl [Construction; Mempool; Application]

(** Generator for operation requirements. *)
let gen_operation_req :
    operation_cstrs ->
    manager_operation_kind list ->
    operation_req QCheck2.Gen.t =
 fun {counter; fee; gas_limit; storage_limit; force_reveal; amount} subjects ->
  let open QCheck2.Gen in
  let* kind = gen_kind subjects in
  let* counter = gen_counter counter in
  let* fee = gen_tez fee in
  let* gas_limit = gen_gas_limit gas_limit in
  let* storage_limit = gen_z storage_limit in
  let+ amount = gen_tez amount in
  {kind; counter; fee; gas_limit; storage_limit; force_reveal; amount}

(** Generator for a pair of operations with the same source and
   sequential counters.*)
let gen_2_operation_req :
    operation_cstrs ->
    manager_operation_kind list ->
    (operation_req * operation_req) QCheck2.Gen.t =
 fun op_cstrs subjects ->
  let open QCheck2.Gen in
  let* op1 =
    gen_operation_req {op_cstrs with force_reveal = Some true} subjects
  in
  let counter =
    match op1.counter with
    | Some x -> Manager_counter.Internal_for_tests.to_int x
    | None -> 1
  in
  let op_cstr =
    {
      {op_cstrs with counter = Pure (counter + 2)} with
      force_reveal = Some false;
    }
  in
  let+ op2 = gen_operation_req op_cstr subjects in
  (op1, op2)

(** Generator for context requirement. *)
let gen_ctxt_req : ctxt_cstrs -> ctxt_req QCheck2.Gen.t =
 fun {
       hard_gas_limit_per_block;
       src_cstrs;
       dest_cstrs;
       del_cstrs;
       sc_cstrs;
       zk_cstrs;
     } ->
  let open QCheck2.Gen in
  let* hard_gas_limit_per_block = gen_gas_integral hard_gas_limit_per_block in
  let* fund_src = gen_tez src_cstrs in
  let* fund_dest = gen_tez dest_cstrs in
  let* fund_del = gen_tez del_cstrs in
  let* fund_sc = gen_tez sc_cstrs in
  let+ fund_zk = gen_tez zk_cstrs in
  {
    hard_gas_limit_per_block;
    fund_src;
    fund_dest;
    fund_del;
    reveal_accounts = true;
    fund_sc;
    fund_zk;
    flags = all_enabled;
  }

(** {2 Wrappers} *)

let wrap ~name ?print ?(count = 1) ?check ~(gen : 'a QCheck2.Gen.t)
    (f : 'a -> bool tzresult Lwt.t) =
  Qcheck2_helpers.qcheck_make_result_lwt
    ~name
    ?print
    ~count
    ?check
    ~extract:Lwt_main.run
    ~pp_error:pp_print_trace
    ~gen
    f

let wrap_mode infos op mode = validate_diagnostic ~mode infos op
