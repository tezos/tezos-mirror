(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    protocol
    Invocation:   dune exec src/proto_023_PtSeouLo/lib_protocol/test/unit/main.exe \
                  -- --file test_full_staking_balance_repr.ml
    Subject:      test the Full_staking_balance_repr module
*)

open Protocol
open Alpha_context
open Data_encoding

let cycle_eras_init ~blocks_per_cycle =
  Environment.wrap_tzresult
  @@ Level_repr.Internal_for_tests.make_cycle_eras
       ~blocks_per_cycle
       ~blocks_per_commitment:
         Default_parameters.constants_test.blocks_per_commitment

let level_from_int ~cycle_eras l =
  let open Lwt_result_wrap_syntax in
  let*?@ level = Level_repr.Internal_for_tests.level_from_int32 ~cycle_eras l in
  return level

(* We duplicate the definition of the Full_staking_balance_repr module
   from Oxford to test encoding compatibility. *)
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

(* We duplicate the definition of the Full_staking_balance_repr module
   from Paris to test encoding compatibility. *)
module Full_staking_balance_repr_paris = struct
  type t = {
    own_frozen : Tez_repr.t;
    staked_frozen : Tez_repr.t;
    delegated : Tez_repr.t;
    min_delegated_in_cycle : Tez_repr.t;
    level_of_min_delegated : Level_repr.t option;
  }

  let encoding =
    let open Data_encoding in
    (* This encoding is backward-compatible with the encoding used in Oxford, so
       as to avoid a stitching in P. It will act as a lazy migration.
       The case in which [added_in_p] is [None] happen only for pre-existing
       values in the storage.
       For them, using [(delegated, None)] and using Cycle_repr.root when no level
       is set will behave correctly. *)
    let added_in_p =
      obj2
        (req "min_delegated_in_cycle" Tez_repr.encoding)
        (req "level_of_min_delegated" (option Level_repr.encoding))
    in
    conv
      (fun {
             own_frozen;
             staked_frozen;
             delegated;
             min_delegated_in_cycle;
             level_of_min_delegated;
           }
         ->
        ( own_frozen,
          staked_frozen,
          delegated,
          Some (min_delegated_in_cycle, level_of_min_delegated) ))
      (fun (own_frozen, staked_frozen, delegated, added_in_p_opt) ->
        let min_delegated_in_cycle, level_of_min_delegated =
          added_in_p_opt |> Option.value ~default:(delegated, None)
        in
        {
          own_frozen;
          staked_frozen;
          delegated;
          min_delegated_in_cycle;
          level_of_min_delegated;
        })
      (obj4
         (req "own_frozen" Tez_repr.encoding)
         (req "staked_frozen" Tez_repr.encoding)
         (req "delegated" Tez_repr.encoding)
         (varopt "min_delegated_in_cycle_and_level" added_in_p))
end

(* for [Level_repr.t], only [level] is checked *)
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
    Assert.equal_level_repr
      ~loc:__LOC__
      (last_modified_level a)
      (last_modified_level b)
  in
  let* () =
    let is_equal =
      Option.equal
        (fun (a : Tez_repr.t * Level_repr.t) (b : Tez_repr.t * Level_repr.t) ->
          Tez_repr.(fst a = fst b) && Level_repr.(snd a = snd b))
    in
    Assert.equal
      ~loc:__LOC__
      is_equal
      "previous_min aren't equal"
      (fun ppf x ->
        match x with
        | Some (previous_min_value, previous_min_level) ->
            Format.fprintf
              ppf
              "(%a, %a)"
              Tez_repr.pp
              previous_min_value
              Level_repr.pp
              previous_min_level
        | None -> Format.fprintf ppf "None")
      (previous_min a)
      (previous_min b)
  in
  return_unit

(* check post-condition after each add/remove delegated *)
let check_staking_balance_invariant ~(current_level : Level_repr.t)
    (staking_balance : Full_staking_balance_repr.t) =
  let open Lwt_result_wrap_syntax in
  let module F = Full_staking_balance_repr in
  (* last_modified_level = current_level *)
  let last_modified_level =
    F.Internal_for_tests_and_RPCs.last_modified_level staking_balance
  in
  let* () =
    Assert.equal_level_repr ~loc:__LOC__ last_modified_level current_level
  in

  (* if previous_min is Some, then
     - previous_min_level.cycle = current_level.cycle
     - previous_min_level.level < last_modified_level.level *)
  match F.Internal_for_tests_and_RPCs.previous_min staking_balance with
  | None -> return_unit
  | Some (_previous_min_value, previous_min_level) ->
      let* () =
        Assert.equal_cycle_repr
          ~loc:__LOC__
          previous_min_level.cycle
          current_level.cycle
      in
      let* () =
        let lt a b =
          Assert.lt
            ~loc:__LOC__
            Raw_level_repr.compare
            "Raw_level_repr is not less"
            Raw_level_repr.pp
            a
            b
        in
        lt previous_min_level.level last_modified_level.level
      in
      return_unit

(** Encodes [staking_balance_to_serialize] using
    [serialization_encoding], then decodes it using the current
    {!Full_staking_balance_repr.encoding}, and check that we obtain
    [expected]. *)
let check_encoding_compatibility ~__LOC__ ~serialization_encoding
    ~staking_balance_to_serialize ~expected =
  let open Lwt_result_syntax in
  let bytes =
    Binary.to_bytes_exn serialization_encoding staking_balance_to_serialize
  in
  let* sb =
    match Binary.of_bytes Full_staking_balance_repr.encoding bytes with
    | Ok x -> return x
    | Error e ->
        Test.fail
          ~__LOC__
          "Data_encoding.Binary.read shouldn't have failed with \
           Full_staking_balance_repr.encoding: %a"
          Binary.pp_read_error
          e
  in
  equal_full_staking_balance sb expected

let test_encodings () =
  let open Lwt_result_syntax in
  let own_frozen = Tez_repr.(mul_exn one 1) in
  let staked_frozen = Tez_repr.(mul_exn one 2) in
  let delegated = Tez_repr.(mul_exn one 5) in
  let*? cycle_eras = cycle_eras_init ~blocks_per_cycle:10l in
  let* level_32 = level_from_int ~cycle_eras 32l in
  let* level_52 = level_from_int ~cycle_eras 52l in
  let* level_57 = level_from_int ~cycle_eras 57l in

  let expected_when_reading_old_encodings =
    Full_staking_balance_repr.Internal_for_tests_and_RPCs.init_raw
      ~own_frozen
      ~staked_frozen
      ~delegated
      ~last_modified_level:Level_repr.Internal_for_tests.root
      ~previous_min:None
  in

  (* Oxford encoding *)
  let staking_balance_to_serialize =
    Full_staking_balance_repr_oxford.make ~own_frozen ~staked_frozen ~delegated
  in
  let* () =
    check_encoding_compatibility
      ~__LOC__
      ~serialization_encoding:Full_staking_balance_repr_oxford.encoding
      ~staking_balance_to_serialize
      ~expected:expected_when_reading_old_encodings
  in

  (* Paris encoding when [level_of_min_delegated = Some _] *)
  let staking_balance_to_serialize =
    Full_staking_balance_repr_paris.
      {
        own_frozen;
        staked_frozen;
        delegated;
        min_delegated_in_cycle = delegated;
        level_of_min_delegated = Some level_52;
      }
  in
  let* () =
    check_encoding_compatibility
      ~__LOC__
      ~serialization_encoding:Full_staking_balance_repr_paris.encoding
      ~staking_balance_to_serialize
      ~expected:expected_when_reading_old_encodings
  in

  (* Paris encoding when [level_of_min_delegated = None]

     Note that in practice, Paris never encodes a staking balance
     where [min_delegated_in_cycle] is [None], but we might as well
     test it anyway. *)
  let staking_balance_to_serialize =
    Full_staking_balance_repr_paris.
      {
        own_frozen;
        staked_frozen;
        delegated;
        min_delegated_in_cycle = delegated;
        level_of_min_delegated = None;
      }
  in
  let* () =
    check_encoding_compatibility
      ~__LOC__
      ~serialization_encoding:Full_staking_balance_repr_paris.encoding
      ~staking_balance_to_serialize
      ~expected:expected_when_reading_old_encodings
  in

  (* Protocol Q encoding when [previous_min = None] *)
  let staking_balance_to_serialize =
    Full_staking_balance_repr.Internal_for_tests_and_RPCs.init_raw
      ~own_frozen
      ~staked_frozen
      ~delegated
      ~last_modified_level:level_52
      ~previous_min:None
  in
  let* () =
    check_encoding_compatibility
      ~__LOC__
      ~serialization_encoding:Full_staking_balance_repr.encoding
      ~staking_balance_to_serialize
      ~expected:staking_balance_to_serialize
  in

  (* Protocol Q encoding when [previous_min = Some _], when the levels
     are in the same cycle. *)
  let staking_balance_to_serialize =
    Full_staking_balance_repr.Internal_for_tests_and_RPCs.init_raw
      ~own_frozen
      ~staked_frozen
      ~delegated
      ~last_modified_level:level_57
      ~previous_min:(Some (Tez_repr.(mul_exn one 100), level_52))
  in
  let* () =
    check_encoding_compatibility
      ~__LOC__
      ~serialization_encoding:Full_staking_balance_repr.encoding
      ~staking_balance_to_serialize
      ~expected:staking_balance_to_serialize
  in

  (* Protocol Q encoding when [previous_min = Some _], when the levels
     are in different cycles. *)
  let staking_balance_to_serialize =
    Full_staking_balance_repr.Internal_for_tests_and_RPCs.init_raw
      ~own_frozen
      ~staked_frozen
      ~delegated
      ~last_modified_level:level_57
      ~previous_min:(Some (Tez_repr.(mul_exn one 100), level_32))
  in
  let* () =
    check_encoding_compatibility
      ~__LOC__
      ~serialization_encoding:Full_staking_balance_repr.encoding
      ~staking_balance_to_serialize
      ~expected:staking_balance_to_serialize
  in
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
    consensus_threshold_size = 0;
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
    ~last_modified_level ~previous_min =
  let open Lwt_result_wrap_syntax in
  let own_frozen = Tez_repr.(mul_exn one own_frozen) in
  let staked_frozen = Tez_repr.(mul_exn one staked_frozen) in
  let delegated = Tez_repr.(mul_exn one delegated) in
  return
  @@ Full_staking_balance_repr.Internal_for_tests_and_RPCs.init_raw
       ~own_frozen
       ~staked_frozen
       ~delegated
       ~last_modified_level
       ~previous_min

let make_previous_min ?previous_min_value ?previous_min_level ~blocks_per_cycle
    () =
  let open Lwt_result_syntax in
  let*? cycle_eras = cycle_eras_init ~blocks_per_cycle in
  match (previous_min_value, previous_min_level) with
  | Some previous_min_value, Some previous_min_level ->
      let* previous_min_level = level_from_int ~cycle_eras previous_min_level in
      let previous_min_value = Tez_repr.(mul_exn one previous_min_value) in
      return @@ Some (previous_min_value, previous_min_level)
  | _ -> return None

let create_staking_balance_level ~own_frozen ~staked_frozen ~delegated
    ~last_modified_level ?previous_min_value ?previous_min_level
    ~blocks_per_cycle () =
  let open Lwt_result_syntax in
  let*? cycle_eras = cycle_eras_init ~blocks_per_cycle in
  let* last_modified_level = level_from_int ~cycle_eras last_modified_level in
  let* previous_min =
    make_previous_min
      ?previous_min_value
      ?previous_min_level
      ~blocks_per_cycle
      ()
  in
  create_staking_balance
    ~own_frozen
    ~staked_frozen
    ~delegated
    ~last_modified_level
    ~previous_min

let check_min_delegated_and_its_level ~blocks_per_cycle ~current_level
    ~min_delegated ~min_delegated_level staking_balance =
  let open Lwt_result_syntax in
  let module F = Full_staking_balance_repr in
  let min_delegated = Tez_repr.(mul_exn one min_delegated) in
  let*? cycle_eras = cycle_eras_init ~blocks_per_cycle in
  let* min_delegated_level = level_from_int ~cycle_eras min_delegated_level in
  let min_delegated_computed, min_delegated_level_computed =
    F.Internal_for_tests_and_RPCs.min_delegated_and_level
      ~cycle_eras
      ~current_level
      staking_balance
  in
  let* () =
    Assert.equal_tez_repr ~loc:__LOC__ min_delegated_computed min_delegated
  in
  let* () =
    Assert.equal_level_repr
      ~loc:__LOC__
      min_delegated_level_computed
      min_delegated_level
  in
  return_unit

let check_min_delegated_and_its_level_rpc b ~(delegate : Contract.t)
    ~min_delegated ~min_delegated_level =
  let open Lwt_result_wrap_syntax in
  let* min_delegated_in_current_cycle_rpc =
    get_min_delegated_in_current_cycle b ~delegate
  in
  let min_delegated_in_cycle_rpc, level_of_min_delegated_rpc =
    min_delegated_in_current_cycle_rpc
  in
  let* () =
    Assert.equal_tez_repr ~loc:__LOC__ min_delegated_in_cycle_rpc min_delegated
  in
  let* () =
    Assert.equal
      ~loc:__LOC__
      (Option.equal Level_repr.( = ))
      "level_min_delegated are not equal"
      (fun ppf x ->
        match x with
        | Some x -> Format.fprintf ppf "%a" Level_repr.pp x
        | None -> Format.fprintf ppf "None")
      min_delegated_level
      level_of_min_delegated_rpc
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

let check_staking_balance ~check_invariant ~current_level ~blocks_per_cycle
    ~min_delegated ~min_delegated_level ~new_staking_balance
    ~expected_staking_balance =
  let open Lwt_result_wrap_syntax in
  let* () =
    equal_full_staking_balance expected_staking_balance new_staking_balance
  in
  let* () =
    if check_invariant then
      check_staking_balance_invariant ~current_level new_staking_balance
    else return_unit
  in
  let* () =
    check_min_delegated_and_its_level
      ~blocks_per_cycle
      ~current_level
      ~min_delegated
      ~min_delegated_level
      new_staking_balance
  in
  return_unit

let check_staking_balance_level ~check_invariant ~current_level
    ~blocks_per_cycle ~min_delegated ~min_delegated_level ~new_staking_balance
    ~expected_staking_balance =
  let open Lwt_result_wrap_syntax in
  let*? cycle_eras = cycle_eras_init ~blocks_per_cycle in
  let* current_level = level_from_int ~cycle_eras current_level in
  let* () =
    check_staking_balance
      ~check_invariant
      ~current_level
      ~blocks_per_cycle
      ~min_delegated
      ~min_delegated_level
      ~new_staking_balance
      ~expected_staking_balance
  in
  return_unit

let test_min_delegated_in_cycle () =
  let open Lwt_result_wrap_syntax in
  let blocks_per_cycle = constants.blocks_per_cycle in
  let expected_staking_balance ~delegated ~last_modified_level
      ~previous_min_value ~previous_min_level =
    (* [own_frozen] and [staked_frozen] are not changed *)
    let own_frozen = 200_000 in
    let staked_frozen = 0 in
    create_staking_balance_level
      ~own_frozen
      ~staked_frozen
      ~delegated
      ~last_modified_level
      ~previous_min_value
      ~previous_min_level
      ~blocks_per_cycle
      ()
  in
  let check_staking_balance_level =
    check_staking_balance_level ~blocks_per_cycle
  in
  let get_staking_balance b ~delegate =
    let* () = print_min_delegated_in_cycle_rpc b ~delegate in
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

  let current_level_init = Block.current_level b in
  let* init_staking_balance =
    expected_staking_balance
      ~delegated:delegated_init (* 3_801_000 *)
      ~last_modified_level:current_level_init
      ~previous_min_value:3_800_000
      ~previous_min_level:0l (* first level of the current cycle *)
  in
  let* () =
    check_staking_balance_level
      ~check_invariant:true
      ~current_level:current_level_init
      ~min_delegated:3_800_000
      ~min_delegated_level:0l (* first level of the current cycle *)
      ~new_staking_balance:staking_balance
      ~expected_staking_balance:init_staking_balance
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
    check_staking_balance_level
      ~check_invariant:false
      ~current_level:(Block.current_level b)
      ~min_delegated:delegated_init
      ~min_delegated_level:12l (* first level of the current cycle *)
      ~new_staking_balance:staking_balance
      ~expected_staking_balance:init_staking_balance
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

  let current_level_minus_10 = Block.current_level b in
  let* staking_balance_minus_10 =
    expected_staking_balance
      ~delegated:delegated_minus_10 (* 3_800_990 *)
      ~last_modified_level:current_level_minus_10
      ~previous_min_value:delegated_init
      ~previous_min_level:12l (* first level of the current cycle *)
  in
  let* () =
    check_staking_balance_level
      ~check_invariant:true
      ~current_level:current_level_minus_10
      ~min_delegated:delegated_minus_10
      ~min_delegated_level:current_level_minus_10
      ~new_staking_balance:staking_balance
      ~expected_staking_balance:staking_balance_minus_10
  in
  let* b = Block.bake_until_cycle_end b in
  Log.info ~color:Log.Color.BG.blue "Start of the new cycle" ;

  let* staking_balance = get_staking_balance b ~delegate in
  let* () =
    check_staking_balance_level
      ~check_invariant:false
      ~current_level:(Block.current_level b)
      ~min_delegated:delegated_minus_10
      ~min_delegated_level:24l (* first level of the current cycle *)
      ~new_staking_balance:staking_balance
      ~expected_staking_balance:staking_balance_minus_10
  in
  let* b = Block.bake_n 3 b in
  Log.info ~color:Log.Color.BG.blue "Before add" ;
  let* staking_balance = get_staking_balance b ~delegate in
  let* () =
    check_staking_balance_level
      ~check_invariant:false
      ~current_level:(Block.current_level b)
      ~min_delegated:delegated_minus_10
      ~min_delegated_level:24l (* first level of the current cycle *)
      ~new_staking_balance:staking_balance
      ~expected_staking_balance:staking_balance_minus_10
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
  let current_level_add_20 = Block.current_level b in
  let* staking_balance_add_20 =
    expected_staking_balance
      ~delegated:delegated_plus_20 (* 3_801_010 *)
      ~last_modified_level:current_level_add_20
      ~previous_min_value:delegated_minus_10
      ~previous_min_level:24l (* first level of the current cycle *)
  in
  let* () =
    check_staking_balance_level
      ~check_invariant:true
      ~current_level:current_level_add_20
      ~min_delegated:3_800_990
      ~min_delegated_level:24l (* first level of the current cycle *)
      ~new_staking_balance:staking_balance
      ~expected_staking_balance:staking_balance_add_20
  in
  return_unit

(* Remove X tez in the last block and add Y tez in the first block of
   the next cycle *)
let test_min_delegated_rpc_for_last_and_first_blocks () =
  let open Lwt_result_wrap_syntax in
  let get_staking_balance b ~delegate =
    let* () = print_min_delegated_in_cycle_rpc b ~delegate in
    let* staking_balance = get_staking_balance b ~delegate in
    return staking_balance
  in
  let level_from_int32 level =
    let blocks_per_cycle = constants.blocks_per_cycle in
    let*? cycle_eras = cycle_eras_init ~blocks_per_cycle in
    let* level = level_from_int ~cycle_eras level in
    return level
  in

  let* b, (delegate, contractor) = Context.init_with_constants2 constants in
  Log.info ~color:Log.Color.BG.blue "Initial state" ;
  let* _staking_balance = get_staking_balance b ~delegate in
  let delegated_init = Tez_repr.(mul_exn one 3_800_000) in
  let* zero_level = level_from_int32 0l in
  let* () =
    check_min_delegated_and_its_level_rpc
      b
      ~delegate
      ~min_delegated:delegated_init
      ~min_delegated_level:(Some zero_level)
  in

  Log.info ~color:Log.Color.BG.blue "Baking until the last level of 1st cycle" ;
  let*?@ last_level =
    Raw_level.of_int32 (Int32.sub constants.blocks_per_cycle 2l)
  in
  let* b = Block.bake_until_level last_level b in
  let* _staking_balance = get_staking_balance b ~delegate in
  let* () =
    check_min_delegated_and_its_level_rpc
      b
      ~delegate
      ~min_delegated:delegated_init
      ~min_delegated_level:(Some zero_level)
  in

  (* Remove 1000 tez from delegated_balance in the last block *)
  Log.info ~color:Log.Color.BG.blue "Removing 1_000 tez in the last block" ;
  let amount_1000 = Tez_helpers.of_int 1000 in
  let* op_minus_1000 = Op.transaction (B b) delegate contractor amount_1000 in
  let* b = Block.bake ~operation:op_minus_1000 b in
  let* _staking_balance = get_staking_balance b ~delegate in
  let*?@ delegated_minus_1000 =
    Tez_repr.(delegated_init -? Tez_helpers.to_repr amount_1000)
  in
  let* last_level_of_fst_block = level_from_int32 (Block.current_level b) in
  let* () =
    check_min_delegated_and_its_level_rpc
      b
      ~delegate
      ~min_delegated:delegated_minus_1000
      ~min_delegated_level:(Some last_level_of_fst_block)
  in

  (* Add 5000 tez to delegated_balance in 1st block of the next cycle *)
  Log.info
    ~color:Log.Color.BG.blue
    "Adding 5_000 tez in 1st block of the next cycle" ;
  let amount_5000 = Tez_helpers.of_int 5000 in
  let* op_plus_5000 = Op.transaction (B b) contractor delegate amount_5000 in
  let* b = Block.bake ~operation:op_plus_5000 b in
  let* _staking_balance = get_staking_balance b ~delegate in
  let*?@ delegated_plus_5000 =
    Tez_repr.(delegated_minus_1000 +? Tez_helpers.to_repr amount_5000)
  in
  let* first_level_of_snd_block = level_from_int32 (Block.current_level b) in
  let* () =
    check_min_delegated_and_its_level_rpc
      b
      ~delegate
      ~min_delegated:delegated_plus_5000
      ~min_delegated_level:(Some first_level_of_snd_block)
  in

  (* Baking until the last level of 2nd cycle *)
  Log.info ~color:Log.Color.BG.blue "Baking until the last level of 2nd cycle" ;
  let*?@ last_level_2nd_cycle =
    Raw_level.of_int32 (Int32.sub (Int32.mul constants.blocks_per_cycle 2l) 1l)
  in
  let* b = Block.bake_until_level last_level_2nd_cycle b in
  let* _staking_balance = get_staking_balance b ~delegate in
  let* () =
    check_min_delegated_and_its_level_rpc
      b
      ~delegate
      ~min_delegated:delegated_plus_5000
      ~min_delegated_level:(Some first_level_of_snd_block)
  in
  return_unit

type kind = Add | Remove

let title_color = Log.Color.FG.yellow

let info_color = Log.Color.BG.green

let check_delegated_variation ~kind ~amount ~current_level ~last_modified_level
    ?previous_min_value ?previous_min_level ~blocks_per_cycle ~min_delegated
    ~min_delegated_level staking_balance () =
  let open Lwt_result_wrap_syntax in
  let module F = Full_staking_balance_repr in
  let*? cycle_eras = cycle_eras_init ~blocks_per_cycle in
  let* current_level = level_from_int ~cycle_eras current_level in
  let* last_modified_level = level_from_int ~cycle_eras last_modified_level in
  let* previous_min =
    make_previous_min
      ?previous_min_value
      ?previous_min_level
      ~blocks_per_cycle
      ()
  in

  let amount = Tez_repr.(mul_exn one amount) in
  let*?@ new_staking_balance =
    match kind with
    | Add -> F.add_delegated ~cycle_eras ~current_level ~amount staking_balance
    | Remove ->
        F.remove_delegated ~cycle_eras ~current_level ~amount staking_balance
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
      ~last_modified_level
      ~previous_min
  in
  let* () =
    check_staking_balance
      ~check_invariant:true
      ~current_level
      ~blocks_per_cycle
      ~min_delegated
      ~min_delegated_level
      ~new_staking_balance
      ~expected_staking_balance
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
      ~last_modified_level:10l
      ~previous_min_value:delegated
      ~previous_min_level:8l
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
      ~last_modified_level:50l
      ~previous_min_value:delegated
      ~previous_min_level:40l
      ~min_delegated:1000
      ~min_delegated_level:40l
      init_staking_balance
      ()
  in
  Log.info ~color:info_color "Next level" ;
  (* Remove 10 tez from delegated_balance *)
  let* _staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:10
      ~current_level:51l
      ~last_modified_level:51l
      ~previous_min_value:delegated
      ~previous_min_level:40l
      ~min_delegated:1000
      ~min_delegated_level:40l
      staking_balance
      ()
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
      ~last_modified_level:10l
      ~previous_min_value:delegated
      ~previous_min_level:8l
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
      ~last_modified_level:11l
      ~previous_min_value:delegated
      ~previous_min_level:8l
      ~min_delegated:1000
      ~min_delegated_level:8l
      init_staking_balance
      ()
  in
  Log.info ~color:info_color "Next level" ;
  (* Remove 10 tez from delegated_balance *)
  let* _staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:10
      ~current_level:12l
      ~last_modified_level:12l
      ~previous_min_value:delegated
      ~previous_min_level:8l
      ~min_delegated:1000
      ~min_delegated_level:8l
      staking_balance
      ()
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
      ~last_modified_level:10l
      ~previous_min_value:delegated
      ~previous_min_level:8l
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
      ~last_modified_level:50l
      ~previous_min_value:delegated
      ~previous_min_level:40l
      ~min_delegated:990
      ~min_delegated_level:50l
      init_staking_balance
      ()
  in
  Log.info ~color:info_color "Next level" ;
  (* Remove 10 tez from delegated_balance *)
  let* _staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:10
      ~current_level:51l
      ~last_modified_level:51l
      ~previous_min_value:990
      ~previous_min_level:50l
      ~min_delegated:980
      ~min_delegated_level:51l
      staking_balance
      ()
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
      ~last_modified_level:10l
      ~previous_min_value:delegated
      ~previous_min_level:8l
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
      ~last_modified_level:11l
      ~previous_min_value:delegated
      ~previous_min_level:8l
      ~min_delegated:990
      ~min_delegated_level:11l
      init_staking_balance
      ()
  in
  Log.info ~color:info_color "Next level" ;
  (* Remove 10 tez from delegated_balance *)
  let* _staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:10
      ~current_level:12l
      ~last_modified_level:12l
      ~previous_min_value:990
      ~previous_min_level:11l
      ~min_delegated:980
      ~min_delegated_level:12l
      staking_balance
      ()
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
      ~last_modified_level:10l
      ~previous_min_value:delegated
      ~previous_min_level:8l
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
      ~last_modified_level:50l
      ~previous_min_value:delegated
      ~previous_min_level:40l
      ~min_delegated:990
      ~min_delegated_level:50l
      init_staking_balance
      ()
  in
  Log.info ~color:info_color "Same level" ;
  (* Add 100 tez to delegated_balance (= 1090) *)
  let* staking_balance =
    check_delegated_variation
      ~kind:Add
      ~amount:100
      ~current_level:50l
      ~last_modified_level:50l
      ~previous_min_value:delegated
      ~previous_min_level:40l
      ~min_delegated:1000
      ~min_delegated_level:40l
      staking_balance
      ()
  in
  Log.info ~color:info_color "Same level" ;
  (* Remove 90 tez from delegated_balance (= 1000) *)
  let* staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:90
      ~current_level:50l
      ~last_modified_level:50l
      ~previous_min_value:delegated
      ~previous_min_level:40l
      ~min_delegated:1000
      ~min_delegated_level:40l
      staking_balance
      ()
  in
  Log.info ~color:info_color "Next level" ;
  (* Remove 10 tez from delegated_balance (= 990) *)
  let* _staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:10
      ~current_level:51l
      ~last_modified_level:51l
      ~previous_min_value:delegated
      ~previous_min_level:40l
      ~min_delegated:990
      ~min_delegated_level:51l
      staking_balance
      ()
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
      ~last_modified_level:10l
      ~previous_min_value:delegated
      ~previous_min_level:8l
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
      ~last_modified_level:11l
      ~previous_min_value:delegated
      ~previous_min_level:8l
      ~min_delegated:990
      ~min_delegated_level:11l
      init_staking_balance
      ()
  in
  Log.info ~color:info_color "Same level" ;
  (* Add 100 tez to delegated_balance (= 1090) *)
  let* staking_balance =
    check_delegated_variation
      ~kind:Add
      ~amount:100
      ~current_level:11l
      ~last_modified_level:11l
      ~previous_min_value:delegated
      ~previous_min_level:8l
      ~min_delegated:1000
      ~min_delegated_level:8l
      staking_balance
      ()
  in
  Log.info ~color:info_color "Same level" ;
  (* Remove 90 tez from delegated_balance (= 1000) *)
  let* staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:90
      ~current_level:11l
      ~last_modified_level:11l
      ~previous_min_value:delegated
      ~previous_min_level:8l
      ~min_delegated:1000
      ~min_delegated_level:8l
      staking_balance
      ()
  in
  Log.info ~color:info_color "Next level" ;
  (* Remove 10 tez from delegated_balance (= 990) *)
  let* _staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:10
      ~current_level:12l
      ~last_modified_level:12l
      ~previous_min_value:delegated
      ~previous_min_level:8l
      ~min_delegated:990
      ~min_delegated_level:12l
      staking_balance
      ()
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
      ~last_modified_level:10l
      ~previous_min_value:delegated
      ~previous_min_level:8l
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
      ~last_modified_level:50l
      ~previous_min_value:delegated
      ~previous_min_level:40l
      ~min_delegated:900
      ~min_delegated_level:50l
      init_staking_balance
      ()
  in
  Log.info ~color:info_color "Same level" ;
  (* Add 10 tez to delegated_balance (= 910) *)
  let* staking_balance =
    check_delegated_variation
      ~kind:Add
      ~amount:10
      ~current_level:50l
      ~last_modified_level:50l
      ~previous_min_value:delegated
      ~previous_min_level:40l
      ~min_delegated:910
      ~min_delegated_level:50l
      staking_balance
      ()
  in
  Log.info ~color:info_color "Next level" ;
  (* Remove 10 tez from delegated_balance (= 900) *)
  let* _staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:10
      ~current_level:51l
      ~last_modified_level:51l
      ~previous_min_value:910
      ~previous_min_level:50l
      ~min_delegated:900
      ~min_delegated_level:51l
      staking_balance
      ()
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
      ~last_modified_level:10l
      ~previous_min_value:delegated
      ~previous_min_level:8l
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
      ~last_modified_level:11l
      ~previous_min_value:delegated
      ~previous_min_level:8l
      ~min_delegated:900
      ~min_delegated_level:11l
      init_staking_balance
      ()
  in
  Log.info ~color:info_color "Same level" ;
  (* Add 10 tez to delegated_balance (= 910) *)
  let* staking_balance =
    check_delegated_variation
      ~kind:Add
      ~amount:10
      ~current_level:11l
      ~last_modified_level:11l
      ~previous_min_value:delegated
      ~previous_min_level:8l
      ~min_delegated:910
      ~min_delegated_level:11l
      staking_balance
      ()
  in
  Log.info ~color:info_color "Next level" ;
  (* Remove 10 tez from delegated_balance (= 900) *)
  let* _staking_balance =
    check_delegated_variation
      ~kind:Remove
      ~amount:10
      ~current_level:12l
      ~last_modified_level:12l
      ~previous_min_value:910
      ~previous_min_level:11l
      ~min_delegated:900
      ~min_delegated_level:12l
      staking_balance
      ()
  in
  return_unit

let tests =
  Tztest.
    [
      tztest "full staking balance - encoding" `Quick test_encodings;
      tztest "min delegated in cycle" `Quick test_min_delegated_in_cycle;
      tztest
        "min delegated RPC for last and first blocks"
        `Quick
        test_min_delegated_rpc_for_last_and_first_blocks;
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
