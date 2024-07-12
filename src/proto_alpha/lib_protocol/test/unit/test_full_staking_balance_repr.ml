(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    protocol
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                  -- --file test_full_staking_balance_repr.ml
    Subject:      test the Full_staking_balance_repr module
*)

open Protocol
open Alpha_context
open Data_encoding

(* We duplicate the definition of the Full_staking_balance_repr module
   from Oxford to test a None case in the current encoding *)
module Full_staking_balance_repr_oxford = struct
  type t = {
    own_frozen : Tez_repr.t;
    staked_frozen : Tez_repr.t;
    delegated : Tez_repr.t;
  }

  let make ~own_frozen ~staked_frozen ~delegated =
    {own_frozen; staked_frozen; delegated}

  let encoding =
    let open Data_encoding in
    conv
      (fun {own_frozen; staked_frozen; delegated} ->
        (own_frozen, staked_frozen, delegated))
      (fun (own_frozen, staked_frozen, delegated) ->
        {own_frozen; staked_frozen; delegated})
      (obj3
         (req "own_frozen" Tez_repr.encoding)
         (req "staked_frozen" Tez_repr.encoding)
         (req "delegated" Tez_repr.encoding))
end

(* for [level_of_min_delegated], only [level] is checked *)
let equal_full_staking_balance (a : Full_staking_balance_repr.t)
    (b : Full_staking_balance_repr.t) =
  let open Lwt_result_syntax in
  let open Full_staking_balance_repr in
  let open Full_staking_balance_repr.Internal_for_tests_and_RPCs in
  let* () = Assert.equal_tez_repr ~loc:__LOC__ (own_frozen a) (own_frozen b) in
  let* () =
    Assert.equal_tez_repr ~loc:__LOC__ (staked_frozen a) (staked_frozen b)
  in
  let* () =
    Assert.equal_tez_repr
      ~loc:__LOC__
      (current_delegated a)
      (current_delegated b)
  in
  let* () =
    Assert.equal_tez_repr
      ~loc:__LOC__
      (min_delegated_in_cycle a)
      (min_delegated_in_cycle b)
  in
  let* () =
    (* compare for [Level_repr.t] only checks [level] *)
    Assert.equal
      ~loc:__LOC__
      (Option.equal Level_repr.( = ))
      "Level_repr aren't equal"
      (fun ppf x ->
        match x with
        | Some x -> Level_repr.pp ppf x
        | None -> Format.fprintf ppf "None")
      (level_of_min_delegated a)
      (level_of_min_delegated b)
  in
  return_unit

let equal_full_staking_balance_with_cycle staking_balance
    expected_staking_balance ~cycle_of_min_delegated =
  let open Lwt_result_wrap_syntax in
  let module F = Full_staking_balance_repr in
  let* () =
    equal_full_staking_balance expected_staking_balance staking_balance
  in

  (* Check a [cycle] field of [Level_repr.t] as well *)
  let level_of_min_delegated_new =
    WithExceptions.Option.get ~loc:__LOC__
    @@ F.Internal_for_tests_and_RPCs.level_of_min_delegated staking_balance
  in
  let cycle_of_min_delegated = Cycle_repr.of_int32_exn cycle_of_min_delegated in
  let* () =
    Assert.equal_cycle_repr
      ~loc:__LOC__
      level_of_min_delegated_new.cycle
      cycle_of_min_delegated
  in
  return_unit

let equal_full_staking_balance_bytes sb_bytes
    (sb_expected : Full_staking_balance_repr.t) =
  let open Lwt_result_syntax in
  let encoding = Full_staking_balance_repr.encoding in
  let* sb =
    match Binary.of_bytes encoding sb_bytes with
    | Ok x -> return x
    | Error e ->
        failwith
          "Data_encoding.Binary.read shouldn't have failed with \
           Full_staking_balance_repr.encoding: %a"
          Binary.pp_read_error
          e
  in
  equal_full_staking_balance sb sb_expected

let test_encodings () =
  let open Lwt_result_syntax in
  let own_frozen = Tez_repr.(mul_exn one 1) in
  let staked_frozen = Tez_repr.(mul_exn one 2) in
  let delegated = Tez_repr.(mul_exn one 5) in

  (* Test a [Some] case for [added_in_p] *)
  let staking_balance =
    Full_staking_balance_repr.init
      ~own_frozen
      ~staked_frozen
      ~delegated
      ~current_level:Level_repr.Internal_for_tests.root
  in
  let encoding = Full_staking_balance_repr.encoding in
  let sb_bytes = Binary.to_bytes_exn encoding staking_balance in
  let* () = equal_full_staking_balance_bytes sb_bytes staking_balance in

  (* Test a [None] case for [added_in_p] *)
  let staking_balance =
    Full_staking_balance_repr.Internal_for_tests_and_RPCs.init_raw
      ~own_frozen
      ~staked_frozen
      ~delegated
      ~min_delegated_in_cycle:delegated
      ~level_of_min_delegated:None
  in
  let staking_balance_o =
    Full_staking_balance_repr_oxford.make ~own_frozen ~staked_frozen ~delegated
  in
  let encoding_o = Full_staking_balance_repr_oxford.encoding in
  let sb_o_bytes = Binary.to_bytes_exn encoding_o staking_balance_o in
  let* () = equal_full_staking_balance_bytes sb_o_bytes staking_balance in
  return_unit

(** Tests for add_delegated and remove_delegated *)

let constants =
  {
    Default_parameters.constants_test with
    issuance_weights =
      {
        Default_parameters.constants_test.issuance_weights with
        base_total_issued_per_minute = Tez.zero;
      };
    consensus_threshold = 0;
    origination_size = 0;
  }

let originate_implicit_unrevealed_account b ?(amount = Tez_helpers.of_int 10)
    source =
  let open Lwt_result_syntax in
  let a = Account.new_account () in
  let c = Contract.Implicit a.pkh in
  let* operation = Op.transaction (B b) ~fee:Tez.zero source c amount in
  let* b = Block.bake b ~operation in
  return (b, c)

let get_min_delegated_in_current_cycle b ~(delegate : Contract.t) =
  let open Lwt_result_wrap_syntax in
  let* ctxt = Block.get_alpha_ctxt b in
  let raw_ctxt = Alpha_context.Internal_for_tests.to_raw ctxt in
  let* delegate_account = Context.Contract.manager (B b) delegate in
  let*@ min_delegated_in_current_cycle =
    Delegate_storage.For_RPC.min_delegated_in_current_cycle
      raw_ctxt
      delegate_account.pkh
  in
  return min_delegated_in_current_cycle

let get_delegated_balance b ~(delegate : Contract.t) =
  let open Lwt_result_syntax in
  let* delegate_account = Context.Contract.manager (B b) delegate in
  let* info = Context.Delegate.info (B b) delegate_account.pkh in
  return info.delegated_balance

let check_delegated_balance_and_delegated b ~loc ~(delegator : Contract.t)
    ~(delegate : Contract.t) ~delegated_balance ~current_delegated =
  let open Lwt_result_syntax in
  let* delegated_balance_computed = get_delegated_balance b ~delegate in
  let* () =
    Assert.equal_tez ~loc delegated_balance_computed delegated_balance
  in
  let* delegator_balance = Context.Contract.balance (B b) delegator in
  let* () = Assert.equal_tez ~loc delegator_balance delegated_balance in

  let* liquid_balance = Context.Contract.balance (B b) delegate in
  let current_delegated_computed =
    Tez_helpers.(liquid_balance +! delegated_balance)
  in
  let current_delegated = Tez.(mul_exn one current_delegated) in
  let* () =
    Assert.equal_tez ~loc current_delegated_computed current_delegated
  in
  return_unit

let pp_staking_balance (op : Full_staking_balance_repr.t) =
  let to_json =
    Data_encoding.Json.construct Full_staking_balance_repr.encoding op
  in
  let s = Format.asprintf "\n%a\n" Data_encoding.Json.pp to_json in
  Log.info "full_staking_balance = %s" s

let get_staking_balance b ~(delegate : Contract.t) =
  let open Lwt_result_wrap_syntax in
  let* ctxt = Block.get_alpha_ctxt b in
  let raw_ctxt = Alpha_context.Internal_for_tests.to_raw ctxt in
  let* delegate_account = Context.Contract.manager (B b) delegate in
  let*@ staking_balance =
    Storage.Stake.Staking_balance.get raw_ctxt delegate_account.pkh
  in
  Log.info ~color:Log.Color.BG.green "Staking balance:" ;
  pp_staking_balance staking_balance ;
  return staking_balance

let create_staking_balance ~own_frozen ~staked_frozen ~delegated
    ?min_delegated_in_cycle ?level_of_min_delegated () =
  let open Lwt_result_wrap_syntax in
  let own_frozen = Tez_repr.(mul_exn one own_frozen) in
  let staked_frozen = Tez_repr.(mul_exn one staked_frozen) in
  let delegated = Tez_repr.(mul_exn one delegated) in
  let min_delegated_in_cycle =
    match min_delegated_in_cycle with
    | Some x -> Tez_repr.(mul_exn one x)
    | None -> delegated
  in
  return
  @@ Full_staking_balance_repr.Internal_for_tests_and_RPCs.init_raw
       ~own_frozen
       ~staked_frozen
       ~delegated
       ~min_delegated_in_cycle
       ~level_of_min_delegated

let cycle_eras_init ~blocks_per_cycle =
  let cycle_era =
    {
      Level_repr.first_level = Raw_level_repr.root;
      first_cycle = Cycle_repr.root;
      blocks_per_cycle;
      blocks_per_commitment =
        Default_parameters.constants_test.blocks_per_commitment;
    }
  in
  Level_repr.create_cycle_eras [cycle_era]

let level_from_int ~cycle_eras l =
  let open Lwt_result_wrap_syntax in
  let*?@ raw_level = Raw_level_repr.of_int32 l in
  return @@ Level_repr.level_from_raw ~cycle_eras raw_level

let create_staking_balance_level ~own_frozen ~staked_frozen ~delegated
    ?min_delegated_in_cycle ~level_of_min_delegated ~blocks_per_cycle () =
  let open Lwt_result_wrap_syntax in
  let*?@ cycle_eras = cycle_eras_init ~blocks_per_cycle in
  let* level = level_from_int ~cycle_eras level_of_min_delegated in
  create_staking_balance
    ~own_frozen
    ~staked_frozen
    ~delegated
    ?min_delegated_in_cycle
    ~level_of_min_delegated:level
    ()

let check_min_delegated_in_cycle_rpc b ~(delegate : Contract.t) =
  let open Lwt_result_wrap_syntax in
  let module F = Full_staking_balance_repr in
  let* ctxt = Block.get_alpha_ctxt b in
  let*@ balance = Alpha_context.Contract.get_balance ctxt delegate in
  let* liquid_balance = Context.Contract.balance (B b) delegate in
  let* () = Assert.equal_tez ~loc:__LOC__ balance liquid_balance in

  let* min_delegated_in_current_cycle =
    get_min_delegated_in_current_cycle b ~delegate
  in
  let min_delegated_in_cycle_rpc, level_of_min_delegated_rpc =
    min_delegated_in_current_cycle
  in

  let* staking_balance = get_staking_balance b ~delegate in
  let min_delegated_in_cycle =
    F.Internal_for_tests_and_RPCs.min_delegated_in_cycle staking_balance
  in
  let level_of_min_delegated =
    F.Internal_for_tests_and_RPCs.level_of_min_delegated staking_balance
  in
  let level_of_min_delegated =
    Option.value
      ~default:Level_repr.Internal_for_tests.root
      level_of_min_delegated
  in

  let current_cycle = Block.current_cycle b in
  match level_of_min_delegated_rpc with
  | None ->
      let* () =
        Assert.equal_tez_repr
          ~loc:__LOC__
          (F.current_delegated staking_balance)
          min_delegated_in_cycle_rpc
      in
      let* () =
        Assert.not_equal_int32
          ~loc:__LOC__
          (Cycle.to_int32 current_cycle)
          (Cycle_repr.to_int32 level_of_min_delegated.cycle)
      in
      return_unit
  | Some level_of_min_delegated_rpc ->
      let* () =
        Assert.equal_tez_repr
          ~loc:__LOC__
          min_delegated_in_cycle
          min_delegated_in_cycle_rpc
      in
      let* () =
        Assert.equal_level_repr
          ~loc:__LOC__
          level_of_min_delegated_rpc
          level_of_min_delegated
      in
      let* () =
        Assert.equal_int32
          ~loc:__LOC__
          (Cycle.to_int32 current_cycle)
          (Cycle_repr.to_int32 level_of_min_delegated.cycle)
      in
      return_unit

let print_min_delegated_in_cycle_rpc b ~(delegate : Contract.t) =
  let open Lwt_result_wrap_syntax in
  let* liquid_balance = Context.Contract.balance (B b) delegate in
  Log.info "liquid_balance = %a" Tez.pp liquid_balance ;

  let* min_delegated_in_current_cycle =
    get_min_delegated_in_current_cycle b ~delegate
  in
  let min_delegated_in_cycle, level_of_min_delegated =
    min_delegated_in_current_cycle
  in
  Log.info "min_delegated_in_cycle = %a" Tez_repr.pp min_delegated_in_cycle ;
  Log.info
    "level_of_min_delegated_RPC = %s"
    (match level_of_min_delegated with
    | None -> "NONE"
    | Some lvl -> Format.asprintf "%a" Level_repr.pp lvl) ;

  let* delegated_balance = get_delegated_balance b ~delegate in
  Log.info "delegated_balance = %a" Tez.pp delegated_balance ;
  let* ctxt = Block.get_alpha_ctxt b in
  Log.info "level of last baked block = %a" Level.pp_full (Level.current ctxt) ;
  return_unit

let test_min_delegated_in_cycle () =
  let open Lwt_result_wrap_syntax in
  let constants =
    constants |> Constants_helpers.Set.Adaptive_issuance.force_activation true
  in
  let expected_staking_balance ~delegated ~min_delegated_in_cycle
      ~level_of_min_delegated =
    let blocks_per_cycle = constants.blocks_per_cycle in
    (* [own_frozen] and [staked_frozen] are not changed *)
    let own_frozen = 200_000 in
    let staked_frozen = 0 in
    create_staking_balance_level
      ~own_frozen
      ~staked_frozen
      ~delegated
      ~min_delegated_in_cycle
      ~level_of_min_delegated
      ~blocks_per_cycle
      ()
  in
  let get_staking_balance b ~delegate =
    let* () = print_min_delegated_in_cycle_rpc b ~delegate in
    let* () = check_min_delegated_in_cycle_rpc b ~delegate in
    let* staking_balance = get_staking_balance b ~delegate in
    return staking_balance
  in

  let* b, (delegate, contractor) = Context.init_with_constants2 constants in
  let amount_1000 = Tez_helpers.of_int 1_000 in
  let* b, delegator =
    originate_implicit_unrevealed_account ~amount:amount_1000 b contractor
  in
  let* delegate_account = Context.Contract.manager (B b) delegate in
  let* set_delegate =
    Op.delegation ~force_reveal:true (B b) delegator (Some delegate_account.pkh)
  in
  let* b = Block.bake ~operation:set_delegate b in
  Log.info ~color:Log.Color.BG.blue "After setting delegate" ;
  let* staking_balance = get_staking_balance b ~delegate in

  let delegated_init = 3_801_000 in
  let* init_staking_balance =
    expected_staking_balance
      ~delegated:delegated_init
      ~min_delegated_in_cycle:3_800_000
      ~level_of_min_delegated:0l
  in
  let* () =
    equal_full_staking_balance_with_cycle
      staking_balance
      init_staking_balance
      ~cycle_of_min_delegated:0l
  in

  let* b = Block.bake_until_cycle_end b in
  let delegated_amount = amount_1000 in
  let* () =
    check_delegated_balance_and_delegated
      b
      ~loc:__LOC__
      ~delegator
      ~delegate
      ~delegated_balance:delegated_amount
      ~current_delegated:delegated_init
  in
  Log.info ~color:Log.Color.BG.blue "Before remove" ;
  let* staking_balance = get_staking_balance b ~delegate in
  let* () =
    equal_full_staking_balance_with_cycle
      staking_balance
      init_staking_balance
      ~cycle_of_min_delegated:0l
  in

  (* Remove 10 tez from delegated_balance *)
  let amount_10 = Tez_helpers.of_int 10 in
  let* op_minus_10 = Op.transaction (B b) delegator contractor amount_10 in
  let* b = Block.bake ~operation:op_minus_10 b in
  let delegated_amount = Tez_helpers.(delegated_amount -! amount_10) in
  let delegated_minus_10 = delegated_init - 10 in
  let* () =
    check_delegated_balance_and_delegated
      b
      ~loc:__LOC__
      ~delegator
      ~delegate
      ~delegated_balance:delegated_amount
      ~current_delegated:delegated_minus_10
  in
  Log.info ~color:Log.Color.BG.blue "After remove" ;
  let* staking_balance = get_staking_balance b ~delegate in
  let cycle_of_min_delegated_minus_10 =
    Cycle.to_int32 @@ Block.current_cycle b
  in
  let* staking_balance_minus_10 =
    expected_staking_balance
      ~delegated:delegated_minus_10
      ~min_delegated_in_cycle:delegated_minus_10
      ~level_of_min_delegated:(Block.current_level b)
  in
  let* () =
    equal_full_staking_balance_with_cycle
      staking_balance
      staking_balance_minus_10
      ~cycle_of_min_delegated:cycle_of_min_delegated_minus_10
  in

  let* b = Block.bake_until_cycle_end b in
  Log.info ~color:Log.Color.BG.blue "Start of the new cycle" ;
  let* staking_balance = get_staking_balance b ~delegate in
  let* () =
    equal_full_staking_balance_with_cycle
      staking_balance
      staking_balance_minus_10
      ~cycle_of_min_delegated:cycle_of_min_delegated_minus_10
  in

  let* b = Block.bake_n 3 b in
  Log.info ~color:Log.Color.BG.blue "Before add" ;
  let* staking_balance = get_staking_balance b ~delegate in
  let* () =
    equal_full_staking_balance_with_cycle
      staking_balance
      staking_balance_minus_10
      ~cycle_of_min_delegated:cycle_of_min_delegated_minus_10
  in

  (* Add 20 tez to delegated_balance *)
  let amount_20 = Tez_helpers.of_int 20 in
  let* op_plus_20 = Op.transaction (B b) contractor delegator amount_20 in
  let* b = Block.bake ~operation:op_plus_20 b in
  let delegated_amount = Tez_helpers.(delegated_amount +! amount_20) in
  let delegated_plus_20 = delegated_minus_10 + 20 in
  let* () =
    check_delegated_balance_and_delegated
      b
      ~loc:__LOC__
      ~delegator
      ~delegate
      ~delegated_balance:delegated_amount
      ~current_delegated:delegated_plus_20
  in
  Log.info ~color:Log.Color.BG.blue "After add" ;
  let* staking_balance = get_staking_balance b ~delegate in
  let cycle_of_min_delegated_add_20 = Cycle.to_int32 @@ Block.current_cycle b in
  let* staking_balance_add_20 =
    expected_staking_balance
      ~delegated:delegated_plus_20
      ~min_delegated_in_cycle:delegated_minus_10
      ~level_of_min_delegated:(Block.current_level b)
  in
  let* () =
    equal_full_staking_balance_with_cycle
      staking_balance
      staking_balance_add_20
      ~cycle_of_min_delegated:cycle_of_min_delegated_add_20
  in
  return_unit

type kind = Add | Remove

let title_color = Log.Color.FG.yellow

let info_color = Log.Color.BG.green

let check_delegated_variation ~kind ~amount ~current_level
    ~min_delegated_in_cycle ~level_of_min_delegated ~cycle_of_min_delegated
    ~blocks_per_cycle staking_balance =
  let open Lwt_result_wrap_syntax in
  let module F = Full_staking_balance_repr in
  let*?@ cycle_eras = cycle_eras_init ~blocks_per_cycle in
  let* current_level = level_from_int ~cycle_eras current_level in
  let* level_of_min_delegated =
    level_from_int ~cycle_eras level_of_min_delegated
  in
  let min_delegated_in_cycle = Tez_repr.(mul_exn one min_delegated_in_cycle) in

  let amount = Tez_repr.(mul_exn one amount) in
  let*?@ new_staking_balance =
    match kind with
    | Add -> F.add_delegated ~current_level ~amount staking_balance
    | Remove -> F.remove_delegated ~current_level ~amount staking_balance
  in
  Log.info ~color:Log.Color.BG.blue "Before change" ;
  let () = pp_staking_balance staking_balance in
  Log.info ~color:Log.Color.BG.blue "After change" ;
  let () = pp_staking_balance new_staking_balance in

  (* [own_frozen] and [staked_frozen] are not changed *)
  (* [delegated] is either increased or decreased by [amount] tez *)
  let*?@ delegated_expected =
    match kind with
    | Add -> Tez_repr.(F.current_delegated staking_balance +? amount)
    | Remove -> Tez_repr.(F.current_delegated staking_balance -? amount)
  in
  let expected_staking_balance =
    Full_staking_balance_repr.Internal_for_tests_and_RPCs.init_raw
      ~own_frozen:(F.own_frozen staking_balance)
      ~staked_frozen:(F.staked_frozen staking_balance)
      ~delegated:delegated_expected
      ~min_delegated_in_cycle
      ~level_of_min_delegated:(Some level_of_min_delegated)
  in
  let* () =
    equal_full_staking_balance_with_cycle
      expected_staking_balance
      new_staking_balance
      ~cycle_of_min_delegated
  in
  return new_staking_balance

(* L_{M}: init_staking_balance; L_{N}: [+10 tez]; L_{N+1}: [-10 tez]
   L_{M} and L_{N+1} are in different cycles
   L_{N} and L_{N+1} are in the same cycle *)
let test_add_delegated_different_cycle () =
  let open Lwt_result_wrap_syntax in
  let blocks_per_cycle = 20l in
  let check_delegated_variation = check_delegated_variation ~blocks_per_cycle in
  let create_staking_balance_level =
    create_staking_balance_level ~blocks_per_cycle
  in
  let delegated = 1000 in
  let* init_staking_balance =
    create_staking_balance_level
      ~own_frozen:50
      ~staked_frozen:100
      ~delegated
      ~level_of_min_delegated:10l
      ()
  in
  Log.info ~color:title_color "Test add_delegated_different_cycle" ;
  Log.info ~color:info_color "Different cycle" ;
  (* Add 10 tez to delegated_balance *)
  let* staking_balance =
    check_delegated_variation
      ~kind:Add
      ~amount:10
      ~current_level:50l
      ~min_delegated_in_cycle:delegated
      ~level_of_min_delegated:50l
      ~cycle_of_min_delegated:2l
      init_staking_balance
  in
  Log.info ~color:info_color "Next level" ;
  (* Remove 10 tez from delegated_balance *)
  let* _staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:10
      ~current_level:51l
      ~min_delegated_in_cycle:delegated
      ~level_of_min_delegated:50l
      ~cycle_of_min_delegated:2l
      staking_balance
  in
  return_unit

(* L_{M}: init_staking_balance; L_{N}: [+10 tez]; L_{N+1}: [-10 tez]
   L_{M} and L_{N+1} are in the same cycle
   L_{N} and L_{N+1} are in the same cycle *)
let test_add_delegated_same_cycle () =
  let open Lwt_result_wrap_syntax in
  let blocks_per_cycle = 20l in
  let check_delegated_variation = check_delegated_variation ~blocks_per_cycle in
  let create_staking_balance_level =
    create_staking_balance_level ~blocks_per_cycle
  in
  let delegated = 1000 in
  let* init_staking_balance =
    create_staking_balance_level
      ~own_frozen:50
      ~staked_frozen:100
      ~delegated
      ~level_of_min_delegated:10l
      ()
  in
  Log.info ~color:Log.Color.BG.yellow "Test add_delegated_same_cycle" ;
  Log.info ~color:info_color "Same cycle" ;
  (* Add 10 tez to delegated_balance *)
  let* staking_balance =
    check_delegated_variation
      ~kind:Add
      ~amount:10
      ~current_level:11l
      ~min_delegated_in_cycle:delegated
      ~level_of_min_delegated:10l
      ~cycle_of_min_delegated:0l
      init_staking_balance
  in
  Log.info ~color:info_color "Next level" ;
  (* Remove 10 tez from delegated_balance *)
  let* _staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:10
      ~current_level:12l
      ~min_delegated_in_cycle:delegated
      ~level_of_min_delegated:10l
      ~cycle_of_min_delegated:0l
      staking_balance
  in
  return_unit

(* L_{M}: init_staking_balance; L_{N}: [-10 tez]; L_{N+1}: [-10 tez]
   L_{M} and L_{N+1} are in different cycles
   L_{N} and L_{N+1} are in the same cycle *)
let test_remove_delegated_different_cycle () =
  let open Lwt_result_wrap_syntax in
  let blocks_per_cycle = 20l in
  let check_delegated_variation = check_delegated_variation ~blocks_per_cycle in
  let create_staking_balance_level =
    create_staking_balance_level ~blocks_per_cycle
  in
  let delegated = 1000 in
  let* init_staking_balance =
    create_staking_balance_level
      ~own_frozen:50
      ~staked_frozen:100
      ~delegated
      ~level_of_min_delegated:10l
      ()
  in
  Log.info ~color:Log.Color.BG.yellow "Test remove_delegated_different_cycle" ;
  Log.info ~color:info_color "Different cycle" ;
  (* Remove 10 tez from delegated_balance *)
  let* staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:10
      ~current_level:50l
      ~min_delegated_in_cycle:990
      ~level_of_min_delegated:50l
      ~cycle_of_min_delegated:2l
      init_staking_balance
  in
  Log.info ~color:info_color "Next level" ;
  (* Remove 10 tez from delegated_balance *)
  let* _staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:10
      ~current_level:51l
      ~min_delegated_in_cycle:980
      ~level_of_min_delegated:51l
      ~cycle_of_min_delegated:2l
      staking_balance
  in
  return_unit

(* L_{M}: init_staking_balance; L_{N}: [-10 tez]; L_{N+1}: [-10 tez]
   L_{M} and L_{N+1} are either in the same cycle
   L_{N} and L_{N+1} are in the same cycle *)
let test_remove_delegated_same_cycle () =
  let open Lwt_result_wrap_syntax in
  let blocks_per_cycle = 20l in
  let check_delegated_variation = check_delegated_variation ~blocks_per_cycle in
  let create_staking_balance_level =
    create_staking_balance_level ~blocks_per_cycle
  in
  let delegated = 1000 in
  let* init_staking_balance =
    create_staking_balance_level
      ~own_frozen:50
      ~staked_frozen:100
      ~delegated
      ~level_of_min_delegated:10l
      ()
  in
  Log.info ~color:Log.Color.BG.yellow "Test remove_delegated_same_cycle" ;
  Log.info ~color:info_color "Same cycle" ;
  (* Remove 10 tez from delegated_balance *)
  let* staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:10
      ~current_level:11l
      ~min_delegated_in_cycle:990
      ~level_of_min_delegated:11l
      ~cycle_of_min_delegated:0l
      init_staking_balance
  in
  Log.info ~color:info_color "Next level" ;
  (* Remove 10 tez from delegated_balance *)
  let* _staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:10
      ~current_level:12l
      ~min_delegated_in_cycle:980
      ~level_of_min_delegated:12l
      ~cycle_of_min_delegated:0l
      staking_balance
  in
  return_unit

(* L_{M}: init_staking_balance; L_{N}: [-10 tez; +100 tez; -90 tez]; L_{N+1}: [-10 tez]
   L_{M} and L_{N+1} are either in different cycles
   L_{N} and L_{N+1} are in the same cycle *)
let test_add_remove_delegated_no_global_change_different_cycle () =
  let open Lwt_result_wrap_syntax in
  let blocks_per_cycle = 20l in
  let check_delegated_variation = check_delegated_variation ~blocks_per_cycle in
  let create_staking_balance_level =
    create_staking_balance_level ~blocks_per_cycle
  in
  let delegated = 1000 in
  let* init_staking_balance =
    create_staking_balance_level
      ~own_frozen:50
      ~staked_frozen:100
      ~delegated
      ~level_of_min_delegated:10l
      ()
  in
  Log.info
    ~color:Log.Color.BG.yellow
    "Test add_remove_delegated_no_global_change_different_cycle" ;
  Log.info ~color:info_color "Different cycle" ;
  (* Remove 10 tez from delegated_balance (= 990) *)
  let* staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:10
      ~current_level:50l
      ~min_delegated_in_cycle:990
      ~level_of_min_delegated:50l
      ~cycle_of_min_delegated:2l
      init_staking_balance
  in
  Log.info ~color:info_color "Same level" ;
  (* Add 100 tez to delegated_balance (= 1090) *)
  let* staking_balance =
    check_delegated_variation
      ~kind:Add
      ~amount:100
      ~current_level:50l
      ~min_delegated_in_cycle:990
      ~level_of_min_delegated:50l
      ~cycle_of_min_delegated:2l
      staking_balance
  in
  Log.info ~color:info_color "Same level" ;
  (* Remove 90 tez from delegated_balance (= 1000) *)
  let* staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:90
      ~current_level:50l
      ~min_delegated_in_cycle:990
      ~level_of_min_delegated:50l
      ~cycle_of_min_delegated:2l
      staking_balance
  in
  Log.info ~color:info_color "Next level" ;
  (* Remove 10 tez from delegated_balance (= 990) *)
  let* _staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:10
      ~current_level:51l
      ~min_delegated_in_cycle:990
      ~level_of_min_delegated:50l
      ~cycle_of_min_delegated:2l
      staking_balance
  in
  return_unit

(* L_{M}: init_staking_balance; L_{N}: [-10 tez; +100 tez; -90 tez]; L_{N+1}: [-10 tez]
   L_{M} and L_{N+1} are either in the same cycle
   L_{N} and L_{N+1} are in the same cycle *)
let test_add_remove_delegated_no_global_change_same_cycle () =
  let open Lwt_result_wrap_syntax in
  let blocks_per_cycle = 20l in
  let check_delegated_variation = check_delegated_variation ~blocks_per_cycle in
  let create_staking_balance_level =
    create_staking_balance_level ~blocks_per_cycle
  in
  let delegated = 1000 in
  let* init_staking_balance =
    create_staking_balance_level
      ~own_frozen:50
      ~staked_frozen:100
      ~delegated
      ~level_of_min_delegated:10l
      ()
  in
  Log.info
    ~color:Log.Color.BG.yellow
    "Test add_remove_delegated_no_global_change_same_cycle" ;
  Log.info ~color:info_color "Same cycle" ;
  (* Remove 10 tez from delegated_balance (= 990) *)
  let* staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:10
      ~current_level:11l
      ~min_delegated_in_cycle:990
      ~level_of_min_delegated:11l
      ~cycle_of_min_delegated:0l
      init_staking_balance
  in
  Log.info ~color:info_color "Same level" ;
  (* Add 100 tez to delegated_balance (= 1090) *)
  let* staking_balance =
    check_delegated_variation
      ~kind:Add
      ~amount:100
      ~current_level:11l
      ~min_delegated_in_cycle:990
      ~level_of_min_delegated:11l
      ~cycle_of_min_delegated:0l
      staking_balance
  in
  Log.info ~color:info_color "Same level" ;
  (* Remove 90 tez from delegated_balance (= 1000) *)
  let* staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:90
      ~current_level:11l
      ~min_delegated_in_cycle:990
      ~level_of_min_delegated:11l
      ~cycle_of_min_delegated:0l
      staking_balance
  in
  Log.info ~color:info_color "Next level" ;
  (* Remove 10 tez from delegated_balance (= 990) *)
  let* _staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:10
      ~current_level:12l
      ~min_delegated_in_cycle:990
      ~level_of_min_delegated:11l
      ~cycle_of_min_delegated:0l
      staking_balance
  in
  return_unit

(* L_{M}: init_staking_balance; L_{N}: [-100 tez; +10 tez]; L_{N+1}: [-10 tez]
   L_{M} and L_{N+1} are in different cycles
   L_{N} and L_{N+1} are in the same cycle *)
let test_add_remove_delegated_new_min_different_cycle () =
  let open Lwt_result_wrap_syntax in
  let blocks_per_cycle = 20l in
  let check_delegated_variation = check_delegated_variation ~blocks_per_cycle in
  let create_staking_balance_level =
    create_staking_balance_level ~blocks_per_cycle
  in
  let delegated = 1000 in
  let* init_staking_balance =
    create_staking_balance_level
      ~own_frozen:50
      ~staked_frozen:100
      ~delegated
      ~level_of_min_delegated:10l
      ()
  in
  Log.info
    ~color:Log.Color.BG.yellow
    "Test add_remove_delegated_new_min_different_cycle" ;
  Log.info ~color:info_color "Different cycle" ;
  (* Remove 100 tez from delegated_balance (= 900) *)
  let* staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:100
      ~current_level:50l
      ~min_delegated_in_cycle:900
      ~level_of_min_delegated:50l
      ~cycle_of_min_delegated:2l
      init_staking_balance
  in
  Log.info ~color:info_color "Same level" ;
  (* Add 10 tez to delegated_balance (= 910) *)
  let* staking_balance =
    check_delegated_variation
      ~kind:Add
      ~amount:10
      ~current_level:50l
      ~min_delegated_in_cycle:900
      ~level_of_min_delegated:50l
      ~cycle_of_min_delegated:2l
      staking_balance
  in
  Log.info ~color:info_color "Next level" ;
  (* Remove 10 tez from delegated_balance (= 900) *)
  let* _staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:10
      ~current_level:51l
      ~min_delegated_in_cycle:900
      ~level_of_min_delegated:50l
      ~cycle_of_min_delegated:2l
      staking_balance
  in
  return_unit

(* L_{M}: init_staking_balance; L_{N}: [-100 tez; +10 tez]; L_{N+1}: [-10 tez]
   L_{M} and L_{N+1} are either in the same cycle
   L_{N} and L_{N+1} are in the same cycle *)
let test_add_remove_delegated_new_min_same_cycle () =
  let open Lwt_result_wrap_syntax in
  let blocks_per_cycle = 20l in
  let check_delegated_variation = check_delegated_variation ~blocks_per_cycle in
  let create_staking_balance_level =
    create_staking_balance_level ~blocks_per_cycle
  in
  let delegated = 1000 in
  let* init_staking_balance =
    create_staking_balance_level
      ~own_frozen:50
      ~staked_frozen:100
      ~delegated
      ~level_of_min_delegated:10l
      ()
  in
  Log.info
    ~color:Log.Color.BG.yellow
    "Test add_remove_delegated_new_min_same_cycle" ;
  Log.info ~color:info_color "Same cycle" ;
  (* Remove 100 tez from delegated_balance (= 900) *)
  let* staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:100
      ~current_level:11l
      ~min_delegated_in_cycle:900
      ~level_of_min_delegated:11l
      ~cycle_of_min_delegated:0l
      init_staking_balance
  in
  Log.info ~color:info_color "Same level" ;
  (* Add 10 tez to delegated_balance (= 910) *)
  let* staking_balance =
    check_delegated_variation
      ~kind:Add
      ~amount:10
      ~current_level:11l
      ~min_delegated_in_cycle:900
      ~level_of_min_delegated:11l
      ~cycle_of_min_delegated:0l
      staking_balance
  in
  Log.info ~color:info_color "Next level" ;
  (* Remove 10 tez from delegated_balance (= 900) *)
  let* _staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:10
      ~current_level:12l
      ~min_delegated_in_cycle:900
      ~level_of_min_delegated:11l
      ~cycle_of_min_delegated:0l
      staking_balance
  in
  return_unit

let tests =
  Tztest.
    [
      tztest "full staking balance - encoding" `Quick test_encodings;
      tztest "min delegated in cycle" `Quick test_min_delegated_in_cycle;
      tztest
        "add delegated (different cycle)"
        `Quick
        test_add_delegated_different_cycle;
      tztest "add delegated (same cycle)" `Quick test_add_delegated_same_cycle;
      tztest
        "remove delegated (different cycle)"
        `Quick
        test_remove_delegated_different_cycle;
      tztest
        "remove delegated (same cycle)"
        `Quick
        test_remove_delegated_same_cycle;
      tztest
        "add/remove delegated with no global change (different cycle)"
        `Quick
        test_add_remove_delegated_no_global_change_different_cycle;
      tztest
        "add/remove delegated with no global change (same cycle)"
        `Quick
        test_add_remove_delegated_no_global_change_same_cycle;
      tztest
        "add/remove delegated with new min (different cycle)"
        `Quick
        test_add_remove_delegated_new_min_different_cycle;
      tztest
        "add/remove delegated with new min (same cycle)"
        `Quick
        test_add_remove_delegated_new_min_same_cycle;
    ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("full_staking_balance", tests)]
  |> Lwt_main.run
