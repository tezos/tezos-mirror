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

(* Testing
   -------
   Component:    Precheck
   Invocation:   dune exec tezt/tests/main.exe -- --file replace_by_fees.ml
   Subject:      Tests for the "replace by fees" feature in the prevelidator
*)

(** This modules implement tests related to the manager operations replacement
    in the mempool when precheck is activated *)

open Tezt_tezos

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2208
   Factorize the Memchecks and Helpers modules and the node_client and
   the two_nodes types in a separate  file *)

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2208
   Refactor the tests as suggested in
   https://gitlab.com/tezos/tezos/-/merge_requests/3968#note_753442465 once
   helpers and checkers of manager_operations are refactored
*)

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2275
   Write tests for the Client part of replace-by-fees feature *)
module Memchecks = Manager_operations.Memchecks
module Helpers = Manager_operations.Helpers

type node_client = Manager_operations.node_client = {
  node : Node.t;
  client : Client.t;
}

type two_nodes = Manager_operations.two_nodes = {
  main : node_client;
  observer : node_client;
}

(* Some local helper and wapper functions *)
let check_validated ~__LOC__ nodes inject =
  Memchecks.with_validated_checks
    ~__LOC__
    nodes
    ~expected_statuses:[]
    ~bake:false
    inject

let check_branch_delayed ~__LOC__ nodes inject =
  Memchecks.with_branch_delayed_checks ~__LOC__ ~bake:false nodes inject

let check_refused ~__LOC__ nodes inject =
  Memchecks.with_refused_checks ~__LOC__ ~bake:false nodes inject

let op_is_validated ~__LOC__ nodes opH =
  Lwt_list.iter_s
    (fun client ->
      let* mempool = Mempool.get_mempool client in
      Memchecks.check_operation_is_in_mempool ~__LOC__ `Validated mempool opH ;
      unit)
    [nodes.main.client; nodes.observer.client]

let op_is_branch_delayed ~__LOC__ nodes opH =
  Lwt_list.iter_s
    (fun client ->
      let* mempool = Mempool.get_mempool client in
      Memchecks.check_operation_is_in_mempool
        ~__LOC__
        `Branch_delayed
        mempool
        opH ;
      unit)
    [nodes.main.client; nodes.observer.client]

(* minimal extra fee replacement is set to 5% *)
let minimal_replacement_fee =
  let factor = Q.make (Z.of_int 105) (Z.of_int 100) in
  fun fee ->
    let r = Q.mul (Q.of_int fee) factor in
    (* rounding to upper value *)
    Z.cdiv (Q.num r) (Q.den r) |> Z.to_int

(* Default fee used in the tests of this module *)
let default_fee = 1000

(* Default fee bumped by replacement fees factor *)
let replacement_fee = minimal_replacement_fee default_fee

(* Default gas limit used in the tests of this module *)
let default_gas = 2000

(* Default transferred amount used in the tests of this module *)
let default_amount = 1

(* Default sources of the tests in this module *)
let default_source = Constant.bootstrap1

(* A record type that contains all needed/variable informations in the tests
   of this module *)
type op_info = {
  fee : int;
  gas : int;
  amount : int;
  (* if counter is none, the next counter will be used *)
  get_counter : Client.t -> int option Lwt.t;
}

(* Default op, with default gas and fees and no imposed counter *)
let default_op =
  {
    fee = default_fee;
    gas = default_gas;
    amount = default_amount;
    get_counter = (fun _ -> Lwt.return_none);
  }

(* An op with replacement fees instead of default ones *)
let replacement_op = {default_op with fee = replacement_fee}

(* Auxiliary function to get the current counter *)
let get_counter ?(contract = default_source) client =
  let* counter_json =
    RPC.Client.call client
    @@ RPC.get_chain_block_context_contract_counter
         ~id:contract.Account.public_key_hash
         ()
  in
  Lwt.return @@ JSON.as_int counter_json

(* Auxiliary function that constructs a batch transfer of the given size *)
let mk_batch client op_data size =
  let open Operation.Manager in
  let* counter_opt = op_data.get_counter client in
  let* counter =
    match counter_opt with
    | None -> get_next_counter client
    | Some c -> Lwt.return c
  in
  let fee, gas_limit, amount = (op_data.fee, op_data.gas, op_data.amount) in
  let transfers =
    List.map (fun _ -> transfer ~dest:default_source ~amount ()) (range 1 size)
  in
  make_batch ~counter ~fee ~gas_limit transfers |> return

(* This is a generic function used to write most of the tests below. In
   addition to the test's title the function takes:
   - a first operation to inject [op1], and the checks to perform [incheck1]
   while injecting,
   - a second operation to inject [op2], the checks to perform [incheck2]
   while injecting, and the checks [postcheck2] to perform on op1 and op2's
   hashes after their injections,
   - if provided, a third operation to inject [op3], the checks to perform
   [incheck3] while injecting, and the checks [postcheck3] to perform on op1,
   op2 and op3's hashes after their injections.

   The function does the following:
   - construct a first (eventually batched) operation, forge and inject it, and
     monitor its propagation through a main and an observer nodes using
     function [incheck1],
   - construct a second (eventually batched) operation with the same manager,
     forge and inject it, and monitor its propagation through a main and an
     observer nodes using function [incheck2],
   - use function [postcheck2] to make some checks about the operations
     once the second one is injected (eg. is the first operation validated or
     removed ? ...)
   - if a third operation and its checkers are provided:
   + construct a third (eventually batched) operation with the same manager,
     forge and inject it, and monitor its propagation through a main and an
     observer nodes using function [incheck3],
   + use function [postcheck3] to make some checks about the operations (eg.
   to check which one is validated, which one is removed, ...)
*)
let replacement_test_helper ~title ~__LOC__ ~op1 ?(size1 = 1) ~op2 ?(size2 = 1)
    ~incheck1 ~incheck2 ~postcheck2 ?op3 ?(size3 = 1) ?incheck3 ?postcheck3 () =
  Protocol.register_test
    ~__FILE__
    ~title
    ~tags:["replace"; "fee"; "manager"]
    ~supports:(Protocol.From_protocol 015)
  @@ fun protocol ->
  let* nodes = Helpers.init ~protocol () in
  let client = nodes.main.client in
  let inject batch = Operation.Manager.inject ~force:true batch client in
  let* oph1 =
    let* batch = mk_batch client op1 size1 in
    incheck1 ~__LOC__ nodes @@ fun () -> inject batch
  in
  let* oph2 =
    let* batch = mk_batch client op2 size2 in
    incheck2 ~__LOC__ nodes @@ fun () -> inject batch
  in
  let* () = postcheck2 nodes oph1 oph2 in
  match (op3, incheck3, postcheck3) with
  | None, None, None -> unit
  | Some op3, Some incheck3, Some postcheck3 ->
      let* oph3 =
        let* batch = mk_batch client op3 size3 in
        incheck3 ~__LOC__ nodes @@ fun () -> inject batch
      in
      postcheck3 nodes oph1 oph2 oph3
  | _ -> Test.fail ~__LOC__ "Test is illformed"

(* 2nd operation is identical (same hash) to the first one. *)
let identical_operations =
  replacement_test_helper
    ~__LOC__
    ~title:"Injecting the same operation twice"
    ~op1:default_op
    ~op2:default_op
    ~incheck1:check_validated
    ~incheck2:check_validated
    ~postcheck2:(fun _nodes h1 h2 ->
      assert (h1 = h2) ;
      unit)
    ()

(* 2nd operation's gas and fees are equal and not the hash. *)
let same_gas_and_fees_but_different_ops =
  replacement_test_helper
    ~__LOC__
    ~title:"Inject two different operations with same gas and fee"
    ~op1:default_op
    ~op2:{default_op with amount = default_amount + 1}
    ~incheck1:check_validated
    ~incheck2:check_branch_delayed
    ~postcheck2:(fun nodes h1 _h2 -> op_is_validated ~__LOC__ nodes h1)
    ()

(* Fees of 2nd operation just below replacement threshold. *)
let replacement_fees_below_threshold =
  replacement_test_helper
    ~__LOC__
    ~title:"Second op's fees are below threshold by 1 mutez"
    ~op1:default_op
    ~op2:{replacement_op with fee = replacement_op.fee - 1}
    ~incheck1:check_validated
    ~incheck2:check_branch_delayed
    ~postcheck2:(fun nodes h1 _h2 -> op_is_validated ~__LOC__ nodes h1)
    ()

(* Fees of 2nd operation equal to replacement threshold. *)
let replacement_fees_equal_to_threshold =
  replacement_test_helper
    ~__LOC__
    ~title:"Second op's fees are equal to replacement fees threshold"
    ~op1:default_op
    ~op2:replacement_op
    ~incheck1:check_validated
    ~incheck2:check_validated
    ~postcheck2:(fun nodes h1 _h2 -> op_is_branch_delayed ~__LOC__ nodes h1)
    ()

(* Fees of 2nd operation just above replacement threshold. *)
let replacement_fees_above_threshold =
  replacement_test_helper
    ~__LOC__
    ~title:"Second op's fees are above replacement fees threshold by 1 mutez"
    ~op1:default_op
    ~op2:{replacement_op with fee = replacement_op.fee + 1}
    ~incheck1:check_validated
    ~incheck2:check_validated
    ~postcheck2:(fun nodes h1 _h2 -> op_is_branch_delayed ~__LOC__ nodes h1)
    ()

(* Fees of 2nd operation just above replacement threshold of the
   first operation, but fees of the 3rd operation are below replacement
   threshold of the second operation. *)
let third_operation_fees_below_replacement_threshold =
  let op2 = {replacement_op with fee = replacement_op.fee + 1} in
  replacement_test_helper
    ~__LOC__
    ~title:"Replace a replacement requires bumping fees again"
    ~op1:default_op
    ~op2
    ~incheck1:check_validated
    ~incheck2:check_validated
    ~postcheck2:(fun nodes h1 _h2 -> op_is_branch_delayed ~__LOC__ nodes h1)
    ~op3:{default_op with fee = minimal_replacement_fee op2.fee - 1}
    ~incheck3:check_branch_delayed
    ~postcheck3:(fun nodes _h1 h2 _h3 -> op_is_validated ~__LOC__ nodes h2)
    ()

(* Fees of 2nd operation just above replacement threshold of the
   first operation, and fees of the 3rd operation are equal to replacement
   threshold of the second operation. *)
let third_operation_fees_equal_to_replacement_threshold =
  let op2 = {replacement_op with fee = replacement_op.fee + 1} in
  replacement_test_helper
    ~__LOC__
    ~title:"Replace a replacement op with enough fees"
    ~op1:default_op
    ~op2
    ~incheck1:check_validated
    ~incheck2:check_validated
    ~postcheck2:(fun nodes h1 _h2 -> op_is_branch_delayed ~__LOC__ nodes h1)
    ~op3:{op2 with fee = minimal_replacement_fee op2.fee}
    ~incheck3:check_validated
    ~postcheck3:(fun nodes _h1 h2 _h3 -> op_is_branch_delayed ~__LOC__ nodes h2)
    ()

(* Fees of 2nd operation equal to replacement threshold, but the gas is also
   increased by 1 unit. *)
let replacement_fees_equal_to_threshold_but_gas_increased =
  replacement_test_helper
    ~__LOC__
    ~title:"Second op's fees are equal to replacement fees. But gas increased"
    ~op1:default_op
    ~op2:{replacement_op with gas = replacement_op.gas + 1}
    ~incheck1:check_validated
    ~incheck2:check_branch_delayed
    ~postcheck2:(fun nodes h1 _h2 -> op_is_validated ~__LOC__ nodes h1)
    ()

(* Fees of 2nd operation just below replacement threshold. Even if the gas
   limit of the second operation is decreased by 1, replacement will fail,
   because fees are not sufficient. *)
let replacement_fees_below_threshold_even_if_gas_is_decreased =
  let op2 = replacement_op in
  replacement_test_helper
    ~__LOC__
    ~title:
      "Second op's gas descreased by 1, but still no enough replacement fees"
    ~op1:default_op
    ~op2:{op2 with fee = op2.fee - 1; gas = op2.gas - 1}
    ~incheck1:check_validated
    ~incheck2:check_branch_delayed
    ~postcheck2:(fun nodes h1 _h2 -> op_is_validated ~__LOC__ nodes h1)
    ()

(* The ratio fee/gas is far better than the first one, but the current
   implemented policy doesn't allow to decrease amount of fees when replacing
   and operation. *)
let fees_of_second_op_below_fees_of_first_one =
  let op1 = {default_op with fee = 500_000; gas = 1_000_000} in
  (* The ratio fee/gas is more important, but fee is lower to replace *)
  let op2 = {op1 with fee = op1.fee; gas = op1.gas / 10} in
  replacement_test_helper
    ~__LOC__
    ~title:"Op2's gas/fee is more important, but fees are not higher than max"
    ~op1
    ~op2
    ~incheck1:check_validated
    ~incheck2:check_branch_delayed
    ~postcheck2:(fun nodes h1 _h2 -> op_is_validated ~__LOC__ nodes h1)
    ()

(* The second operation has much more fees than the first one, and a
   better fee/gas ratio. But its counter is in the future. It cannot be
   validated, and thus replace the first operation *)
let cannot_replace_with_an_op_having_diffrent_counter =
  let get_counter client =
    let* counter = get_counter client in
    (* counter in the future*)
    Lwt.return_some @@ (counter + 100)
  in
  let op2 = {default_op with fee = 100 * default_op.fee; get_counter} in
  replacement_test_helper
    ~__LOC__
    ~title:"Much more fees in second op, but counter in the future"
    ~op1:default_op
    ~op2
    ~incheck1:check_validated
    ~incheck2:check_branch_delayed
    ~postcheck2:(fun nodes h1 _h2 -> op_is_validated ~__LOC__ nodes h1)
    ()

(* The first operation is not validated. So the second operation doesn't need to
   have fees at least equal to "replacement fees threshold" to be validated. *)
let cannot_replace_a_non_validated_operation =
  let get_counter client =
    let* counter = get_counter client in
    (* counter in the future*)
    Lwt.return_some @@ (counter + 100)
  in
  let op1 = {default_op with get_counter} in
  replacement_test_helper
    ~__LOC__
    ~title:
      "First operation not validated, second one validated with default fees"
    ~op1
    ~op2:default_op
    ~incheck1:check_branch_delayed
    ~incheck2:check_validated
    ~postcheck2:(fun _nodes _h1 _h2 -> unit)
    ()

(* Sum of fees of the second operation is ok, but gas doubled. So ratio is not
   good to make the replacement *)
let replace_simple_op_with_a_batched_low_fees =
  replacement_test_helper
    ~__LOC__
    ~title:"Gas limit doubled in batch. More fees needed"
    ~op1:default_op
    ~op2:{default_op with fee = (replacement_fee / 2) + 1}
    ~size2:2
    ~incheck1:check_validated
    ~incheck2:check_branch_delayed
    ~postcheck2:(fun nodes h1 _h2 -> op_is_validated ~__LOC__ nodes h1)
    ()

(* Sum of fees of the second operation is ok, and gas is ok in the
   whole batch. So, replacement is ok. *)
let replace_simple_op_with_a_batched =
  replacement_test_helper
    ~__LOC__
    ~title:"2nd operation's gas limit is constant. Replacement possible."
    ~op1:{default_op with gas = default_gas * 2}
    ~op2:{default_op with gas = default_gas; fee = (replacement_fee / 2) + 1}
    ~size2:2
    ~incheck1:check_validated
    ~incheck2:check_validated
    ~postcheck2:(fun nodes h1 _h2 -> op_is_branch_delayed ~__LOC__ nodes h1)
    ()

(* Replacing a batched operation not possible due to low fees *)
let replace_batched_op_with_simple_one_low_fees =
  replacement_test_helper
    ~__LOC__
    ~title:"Replacing a batched operation not possible due to low fees"
    ~op1:default_op
    ~size1:2
    ~op2:replacement_op
    ~incheck1:check_validated
    ~incheck2:check_branch_delayed
    ~postcheck2:(fun nodes h1 _h2 -> op_is_validated ~__LOC__ nodes h1)
    ()

(* Replacing a batched op is possible if enough fees are provided *)
let replace_batched_op_with_simple_one =
  replacement_test_helper
    ~__LOC__
    ~title:"Replacing a batched op is possible if enough fees are provided"
    ~op1:{default_op with fee = default_op.fee / 2}
    ~size1:2
    ~op2:replacement_op
    ~incheck1:check_validated
    ~incheck2:check_validated
    ~postcheck2:(fun nodes h1 _h2 -> op_is_branch_delayed ~__LOC__ nodes h1)
    ()

(* Fees of 2nd operation are bigger than the account's supply. *)
let low_balance_to_pay_fees =
  replacement_test_helper
    ~__LOC__
    ~title:"Low balance - second op's fees are equal to max_int64-1"
    ~op1:default_op
    ~op2:{default_op with fee = max_int}
    ~size2:2
    ~incheck1:check_validated
    ~incheck2:check_branch_delayed
    ~postcheck2:(fun nodes h1 _h2 -> op_is_validated ~__LOC__ nodes h1)
    ()

(* Sum of fees of the 2nd operation overflow on int64. *)
let sum_fees_overflow =
  replacement_test_helper
    ~__LOC__
    ~title:"Sum of fees of the 2nd operation overflow on int64"
    ~op1:default_op
    ~op2:{default_op with fee = max_int}
    ~size2:10
    ~incheck1:check_validated
      (* We notice that the source cannot afford the fees before finding
         out that the fees overflow, hence the branch_delayed
         classification instead of refused. *)
    ~incheck2:check_branch_delayed
    ~postcheck2:(fun nodes h1 _h2 -> op_is_validated ~__LOC__ nodes h1)
    ()

let register ~protocols =
  identical_operations protocols ;
  same_gas_and_fees_but_different_ops protocols ;
  replacement_fees_below_threshold protocols ;
  replacement_fees_equal_to_threshold protocols ;
  replacement_fees_above_threshold protocols ;
  third_operation_fees_below_replacement_threshold protocols ;
  third_operation_fees_equal_to_replacement_threshold protocols ;
  replacement_fees_equal_to_threshold_but_gas_increased protocols ;
  replacement_fees_below_threshold_even_if_gas_is_decreased protocols ;
  fees_of_second_op_below_fees_of_first_one protocols ;
  cannot_replace_with_an_op_having_diffrent_counter protocols ;
  cannot_replace_a_non_validated_operation protocols ;
  replace_simple_op_with_a_batched_low_fees protocols ;
  replace_simple_op_with_a_batched protocols ;
  replace_batched_op_with_simple_one_low_fees protocols ;
  replace_batched_op_with_simple_one protocols ;
  low_balance_to_pay_fees protocols ;
  sum_fees_overflow protocols
