(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Invocation:   dune exec src/proto_016_PtMumbai/lib_plugin/test/main.exe \
                  -- --file test_fee_needed_to_replace_by_fee.ml
    Subject:      Unit tests the fee_needed_to_replace_by_fee function
                  of the mempool plugin
*)

let register_test =
  Helpers.register_test
    ~__FILE__
    ~file_title:"fee_needed_to_replace_by_fee"
    ~file_tags:["mempool"; "fee_needed_to_replace_by_fee"]

(** Test that [fee_needed_to_replace_by_fee] returns [None] when at least
    one argument is a non-manager operation. *)
let () =
  register_test
    ~title:"non-manager operations"
    ~additional_tags:["nonmanager"; "random"]
  @@ fun () ->
  let n = (* Number of non-manager operations to generate *) 30 in
  let non_manager_ops =
    QCheck2.Gen.generate ~n Helpers.non_manager_operation_gen
  in
  (* Test with two non-manager operations. *)
  let test op_to_replace candidate_op =
    assert (
      Option.is_none
        (Plugin.Mempool.Internal_for_tests.fee_needed_to_replace_by_fee
           Plugin.Mempool.default_config
           ~op_to_replace
           ~candidate_op))
  in
  Helpers.iter_neighbors test non_manager_ops ;
  (* Test with one non-manager and one manager operation. *)
  let manager_ops = QCheck2.Gen.generate ~n Helpers.manager_operation_gen in
  let test_both op1 op2 =
    test op1 op2 ;
    test op2 op1
  in
  Helpers.iter2_exn test_both non_manager_ops manager_ops ;
  unit

(** Check that {!Plugin.Mempool.fee_needed_to_replace_by_fee}
    correctly returns the minimal fee that [candidate_op] would need to
    replace [op_to_replace] through {!Plugin.Mempool.conflict_handler}.

    Precondition: both operations are manager operations with respective
    total fee and gas limit [fee_r], [gas_r] and [fee_c], [gas_c]. *)
let test_manager_ops config (op_to_replace, fee_r, gas_r)
    (candidate_op, fee_c, gas_c) =
  Log.debug
    "Test op_to_replace: {fee=%dmutez; gas=%d} and candidate_op: {fee=%dmutez; \
     gas=%d}"
    fee_r
    gas_r
    fee_c
    gas_c ;
  let fee_needed =
    WithExceptions.Option.get ~loc:__LOC__
    @@ Plugin.Mempool.Internal_for_tests.fee_needed_to_replace_by_fee
         config
         ~op_to_replace:(snd op_to_replace)
         ~candidate_op:(snd candidate_op)
  in
  Log.debug "  --> fee_needed: %Ld" fee_needed ;
  let with_fee fee =
    (fst candidate_op, Helpers.set_fee fee (snd candidate_op))
  in
  (if fee_needed > 0L then
   let fee_smaller = Int64.pred fee_needed in
   match
     Plugin.Mempool.conflict_handler
       config
       ~existing_operation:op_to_replace
       ~new_operation:(with_fee fee_smaller)
   with
   | `Keep -> ()
   | `Replace ->
       Test.fail
         ~__LOC__
         "Adjusted candidate_op: {fee=%Ldmutez; gas=%d} with fee smaller than \
          fee_needed should not be allowed to replace op_to_replace: \
          {fee=%dmutez; gas=%d}"
         fee_smaller
         gas_c
         fee_r
         gas_r) ;
  match
    Plugin.Mempool.conflict_handler
      config
      ~existing_operation:op_to_replace
      ~new_operation:(with_fee fee_needed)
  with
  | `Keep ->
      Test.fail
        ~__LOC__
        "Adjusted candidate_op: {fee=%Ldmutez; gas=%d} with fee_needed should \
         replace op_to_replace: {fee=%dmutez; gas=%d}"
        fee_needed
        gas_c
        fee_r
        gas_r
  | `Replace -> ()

(** Test manager operations with hand-picked fee and gas. *)
let () =
  register_test
    ~title:"hand-picked fee and gas"
    ~additional_tags:["manager"; "handpicked"]
  @@ fun () ->
  let fee_in_mutez_and_gas_list =
    [
      (* Various relative gas limits and fees: equal, off by one,
         multiple/divisor, high ppcm, coprime, zero, one, much
         higher/lower etc. *)
      (1000, 1000);
      (500, 1000);
      (1000, 1001);
      (1000, 999);
      (1000, 500);
      (1000, 4000);
      (1000, 1200);
      (333, 777);
      (11, 7);
      (1000, 31);
      (1000, 1);
      (1, 100_000);
      (1_000_000, 100_001);
      (0, 10);
      (* Values such that fee or fee/gas, relative to (1000, 1000) that
         appears above in the list, is close to the default
         [replace_by_fee_factor] of 105/100 or its inverse. *)
      (1050, 1000);
      (1051, 1000);
      (1049, 1000);
      (1050, 1001);
      (1050, 999);
      (1000, 1050);
      (1000, 1051);
      (1000, 1049);
    ]
  in
  let ops =
    List.map
      (fun (fee_in_mutez, gas) ->
        let op =
          Helpers.generate_manager_op_with_fee_and_gas ~fee_in_mutez ~gas
        in
        (op, fee_in_mutez, gas))
      fee_in_mutez_and_gas_list
  in
  List.iter
    (fun op ->
      List.iter (test_manager_ops Plugin.Mempool.default_config op) ops)
    ops ;
  unit

(** Test manager operations with random fee and gas, and random config. *)
let () =
  register_test
    ~title:"random fee, gas, and config"
    ~additional_tags:["manager"; "random"]
  @@ fun () ->
  let open QCheck2.Gen in
  let gen =
    let* fee_in_mutez = int_range 0 100_000_000 in
    let* gas = int_range 1 50_000_000 in
    let* op = Helpers.manager_op_with_fee_and_gas_gen ~fee_in_mutez ~gas in
    return (op, fee_in_mutez, gas)
  in
  let gen_config =
    let* num = int_range 0 1000 in
    let* den = int_range 1 1000 in
    return
      (Plugin.Mempool.Internal_for_tests.default_config_with_replace_factor
         (Q.of_ints num den))
  in
  let test_manager_ops op_fee_gas1 op_fee_gas2 =
    test_manager_ops (generate1 gen_config) op_fee_gas1 op_fee_gas2
  in
  Helpers.iter_neighbors test_manager_ops (generate ~n:100 gen) ;
  Lwt.return_unit
