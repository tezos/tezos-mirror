(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:    Plugin.Mempool
    Invocation:   dune exec src/proto_alpha/lib_plugin/test/main.exe \
                  -- --file test_conflict_handler.ml
    Subject:      Unit tests the Mempool.conflict_handler function of the plugin
*)

let pp_answer fmt = function
  | `Keep -> Format.fprintf fmt "Keep"
  | `Replace -> Format.fprintf fmt "Replace"

let check_answer ?__LOC__ expected actual =
  assert
    (Qcheck2_helpers.qcheck_eq ~pp:pp_answer ?__LOC__ expected actual : bool)

let is_manager_op ((_ : Operation_hash.t), op) =
  (* This is implemented differently from
     [Plugin.Mempool.is_manager_operation] (which relies on
     [Alpha_context.Operation.acceptable_pass]), used in
     [Plugin.Mempool.conflict_handler], to avoid the test being just a
     copy of the code. *)
  let {Alpha_context.protocol_data = Operation_data proto_data; _} = op in
  match proto_data.contents with
  | Single (Manager_operation _) | Cons (Manager_operation _, _) -> true
  | _ -> false

(** Test that when the operations are not both manager operations, the
    conflict handler picks the higher operation according to
    [Operation.compare]. *)
let test_random_ops () =
  let ops =
    let open Operation_generator in
    QCheck2.Gen.(generate ~n:100 (pair generate_operation generate_operation))
  in
  List.iter
    (fun ((_, op1), (_, op2)) ->
      let answer =
        Plugin.Mempool.conflict_handler
          Plugin.Mempool.default_config
          ~existing_operation:op1
          ~new_operation:op2
      in
      if is_manager_op op1 && is_manager_op op2 then
        (* When both operations are manager operations, the result is
           complicated and depends on the [config]. Testing it here
           would mean basically reimplementing
           [conflict_handler]. Instead, we test this case in
           [test_manager_ops] below. *)
        ()
      else if
        (* When there is at least one non-manager operation, the
           conflict handler defers to [Operation.compare]: the higher
           operation is selected. *)
        Alpha_context.Operation.compare op1 op2 >= 0
      then check_answer ~__LOC__ `Keep answer
      else check_answer ~__LOC__ `Replace answer)
    ops ;
  return_unit

(** Generator for a manager batch with the specified total fee and gas. *)
let generate_manager_op_with_fee_and_gas ~fee_in_mutez ~gas =
  let open Alpha_context in
  let open QCheck2.Gen in
  let rec set_fee_and_gas :
      type kind. _ -> _ -> kind contents_list -> kind contents_list t =
   fun desired_total_fee desired_total_gas -> function
    | Single (Manager_operation data) ->
        let fee = Tez.of_mutez_exn (Int64.of_int desired_total_fee) in
        let gas_limit = Gas.Arith.integral_of_int_exn desired_total_gas in
        return (Single (Manager_operation {data with fee; gas_limit}))
    | Cons (Manager_operation data, tail) ->
        let* local_fee =
          (* We generate some corner cases where some individual
             operations in the batch have zero fees. *)
          let* r = frequencyl [(7, `Random); (2, `Zero); (1, `All)] in
          match r with
          | `Random -> int_range 0 desired_total_fee
          | `Zero -> return 0
          | `All -> return desired_total_fee
        in
        let* local_gas = int_range 0 desired_total_gas in
        let fee = Tez.of_mutez_exn (Int64.of_int local_fee) in
        let gas_limit = Gas.Arith.integral_of_int_exn local_gas in
        let* tail =
          set_fee_and_gas
            (desired_total_fee - local_fee)
            (desired_total_gas - local_gas)
            tail
        in
        return (Cons (Manager_operation {data with fee; gas_limit}, tail))
    | Single _ ->
        (* This function is only called on a manager operation. *) assert false
  in
  (* Generate a random manager operation. *)
  let* batch_size = int_range 1 Operation_generator.max_batch_size in
  let* op = Operation_generator.generate_manager_operation batch_size in
  (* Modify its fee and gas to match the [fee_in_mutez] and [gas] inputs. *)
  let {shell = _; protocol_data = Operation_data protocol_data} = op in
  let* contents = set_fee_and_gas fee_in_mutez gas protocol_data.contents in
  let protocol_data = {protocol_data with contents} in
  let op = {op with protocol_data = Operation_data protocol_data} in
  return (Operation.hash_packed op, op)

let check_conflict_handler ~__LOC__ config ~old ~nw expected =
  let answer =
    Plugin.Mempool.conflict_handler
      config
      ~existing_operation:old
      ~new_operation:nw
  in
  check_answer ~__LOC__ expected answer

(** Test the semantics of the conflict handler on manager operations,
    with either hand-picked or carefully generated fee and gas. *)
let test_manager_ops () =
  let make_op ~fee_in_mutez ~gas =
    QCheck2.Gen.generate1
      (generate_manager_op_with_fee_and_gas ~fee_in_mutez ~gas)
  in

  (* Test operations with specific fee and gas, using the default
     configuration. This configuration replaces the old operation when
     the new one is at least 5% better, in terms of both fees and
     fee/gas ratios. *)
  let default = Plugin.Mempool.default_config in
  let ref_fee = 10_000_000 in
  let ref_gas = 2100 in
  (* Reference operation arbitrarily has 10 tez of fees and 2100
     gas. The gas is chosen to still give an integer when multiplied
     by 100/105. *)
  let old = make_op ~fee_in_mutez:ref_fee ~gas:ref_gas in
  (* Operation with same fee and ratio. *)
  let op_same = make_op ~fee_in_mutez:ref_fee ~gas:ref_gas in
  check_conflict_handler ~__LOC__ default ~old ~nw:op_same `Keep ;
  (* 5% better fee but same ratio (because gas is also 5% more). *)
  let more5 = Q.make (Z.of_int 105) (Z.of_int 100) in
  let fee_more5 = Q.(to_int (mul more5 (of_int ref_fee))) in
  let gas_more5 = Q.(to_int (mul more5 (of_int ref_gas))) in
  let op_fee5 = make_op ~fee_in_mutez:fee_more5 ~gas:gas_more5 in
  check_conflict_handler ~__LOC__ default ~old ~nw:op_fee5 `Keep ;
  (* 5% better ratio but same fee (because gas is multiplied by 100/105). *)
  let less5 = Q.make (Z.of_int 100) (Z.of_int 105) in
  let gas_less5 = Q.(to_int (mul less5 (of_int ref_gas))) in
  let op_ratio5 = make_op ~fee_in_mutez:ref_fee ~gas:gas_less5 in
  check_conflict_handler ~__LOC__ default ~old ~nw:op_ratio5 `Keep ;
  (* Both 5% better fee and 5% better ratio. *)
  let op_both5 = make_op ~fee_in_mutez:fee_more5 ~gas:ref_gas in
  check_conflict_handler ~__LOC__ default ~old ~nw:op_both5 `Replace ;

  (* Config that requires 10% better fee and ratio to replace. *)
  let config10 =
    {
      Plugin.Mempool.default_config with
      replace_by_fee_factor = Q.make (Z.of_int 11) (Z.of_int 10);
    }
  in
  check_conflict_handler ~__LOC__ config10 ~old ~nw:op_same `Keep ;
  check_conflict_handler ~__LOC__ config10 ~old ~nw:op_fee5 `Keep ;
  check_conflict_handler ~__LOC__ config10 ~old ~nw:op_ratio5 `Keep ;
  check_conflict_handler ~__LOC__ config10 ~old ~nw:op_both5 `Keep ;
  (* Config that replaces when the new op has at least as much fee and ratio. *)
  let config0 =
    {Plugin.Mempool.default_config with replace_by_fee_factor = Q.one}
  in
  check_conflict_handler ~__LOC__ config0 ~old ~nw:op_same `Replace ;
  check_conflict_handler ~__LOC__ config0 ~old ~nw:op_fee5 `Replace ;
  check_conflict_handler ~__LOC__ config0 ~old ~nw:op_ratio5 `Replace ;
  check_conflict_handler ~__LOC__ config0 ~old ~nw:op_both5 `Replace ;
  (* This config does not replace when the new operation has worse
     fees (even when the ratio is higher). *)
  let op_less_fee = make_op ~fee_in_mutez:(ref_fee - 1) ~gas:(ref_gas - 1) in
  check_conflict_handler ~__LOC__ default ~old ~nw:op_less_fee `Keep ;
  (* This config does not replace either when the ratio is smaller. *)
  let op_worse_ratio = make_op ~fee_in_mutez:ref_fee ~gas:(ref_gas + 1) in
  check_conflict_handler ~__LOC__ default ~old ~nw:op_worse_ratio `Keep ;

  (* Generate random operations which do not have 5% better fees than
     the reference [op]: they should not replace [op] when using the
     default config. *)
  let open QCheck2.Gen in
  let repeat = 30 in
  let max_gas = 5 * ref_gas in
  let generator_not_5more_fee =
    let* fee_in_mutez = int_range 0 (fee_more5 - 1) in
    let* gas = int_range 0 max_gas in
    Format.eprintf "op_not_fee5: fee = %d; gas = %d@." fee_in_mutez gas ;
    generate_manager_op_with_fee_and_gas ~fee_in_mutez ~gas
  in
  let ops_not_5more_fee = generate ~n:repeat generator_not_5more_fee in
  List.iter
    (fun nw -> check_conflict_handler ~__LOC__ default ~old ~nw `Keep)
    ops_not_5more_fee ;
  (* Generate random operations which do not have 5% better ratio than
     the reference [op]: they should not replace [op] when using the
     default config. *)
  let ratio_5more =
    Q.(mul more5 (make (Z.of_int ref_fee) (Z.of_int ref_gas)))
  in
  let generator_not_5more_ratio =
    let* gas = int_range 0 max_gas in
    let fee_for_5more_ratio = Q.(mul (of_int gas) ratio_5more) in
    let fee_upper_bound = Q.to_int fee_for_5more_ratio - 1 in
    let* fee_in_mutez = int_range 0 (max 0 fee_upper_bound) in
    Format.eprintf "op_not_ratio5: fee = %d; gas = %d@." fee_in_mutez gas ;
    generate_manager_op_with_fee_and_gas ~fee_in_mutez ~gas
  in
  let ops_not_5more_ratio = generate ~n:repeat generator_not_5more_ratio in
  List.iter
    (fun nw -> check_conflict_handler ~__LOC__ default ~old ~nw `Keep)
    ops_not_5more_ratio ;
  (* Generate random operations which have both 5% higher fees and 5%
     better ratio than the reference [op]: they should replace [op]
     when using the default config. *)
  let max_fee =
    (* We use a significantly higher factor to define [max_fee] from
       [ref_fee] than [max_gas] from [ref_gas]. Therefore, even if we
       generate a gas equal to [max_gas], we can still generate a fee
       that makes the ratio at least 5% better than the reference
       operation's. *)
    10 * ref_fee
  in
  let generator_both_5more =
    let* gas = int_range 0 max_gas in
    let fee_for_5more_ratio = Q.(mul (of_int gas) ratio_5more) in
    let fee_lower_bound = max fee_more5 (Q.to_int fee_for_5more_ratio + 1) in
    let* fee_in_mutez = int_range fee_lower_bound max_fee in
    Format.eprintf "op_both_better: fee = %d; gas = %d@." fee_in_mutez gas ;
    generate_manager_op_with_fee_and_gas ~fee_in_mutez ~gas
  in
  let ops_both_5more = generate ~n:repeat generator_both_5more in
  List.iter
    (fun nw -> check_conflict_handler ~__LOC__ default ~old ~nw `Replace)
    ops_both_5more ;
  return_unit

let () =
  Alcotest_lwt.run
    ~__FILE__
    Protocol.name
    [
      ( "conflict_handler",
        [
          Tztest.tztest
            "Random operations (not both manager)"
            `Quick
            test_random_ops;
          Tztest.tztest "Manager operations" `Quick test_manager_ops;
        ] );
    ]
  |> Lwt_main.run
