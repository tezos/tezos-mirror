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
    Invocation:   dune exec src/proto_alpha/lib_plugin/test/main.exe \
                  -- --file test_fee_needed_to_overtake.ml
    Subject:      Unit tests the Mempool.fee_needed_to_overtake
                  function of the plugin
*)

let register_test =
  Helpers.register_test
    ~__FILE__
    ~file_title:"fee_needed_to_overtake"
    ~file_tags:["mempool"; "fee_needed_to_overtake"]

let rec iter_neighbors f = function
  | [] | [_] -> ()
  | x :: (y :: _ as l) ->
      f x y ;
      iter_neighbors f l

let iter2_exn f l1 l2 =
  match List.iter2 ~when_different_lengths:() f l1 l2 with
  | Ok () -> ()
  | Error () ->
      Test.fail
        ~__LOC__
        "Lists have respective lengths %d and %d."
        (List.length l1)
        (List.length l2)

(** Test that [fee_needed_to_overtake] returns [None] when at least
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
  let test op_to_overtake candidate_op =
    assert (
      Option.is_none
        (Plugin.Mempool.fee_needed_to_overtake ~op_to_overtake ~candidate_op))
  in
  iter_neighbors test non_manager_ops ;
  (* Test with one non-manager and one manager operation. *)
  let manager_ops = QCheck2.Gen.generate ~n Helpers.manager_operation_gen in
  let test_both op1 op2 =
    test op1 op2 ;
    test op2 op1
  in
  iter2_exn test_both non_manager_ops manager_ops ;
  unit

(** Change the total fee of the packed operation [op] to [fee] (in mutez)
    and its source to {!Signature.Public_key_hash.zero}.

    Precondition: [op] must be a manager operation. *)
let set_fee_and_source fee op =
  let open Alpha_context in
  let open QCheck2.Gen in
  let source = Signature.Public_key_hash.zero in
  let rec set_fee_contents_list_gen :
      type kind. int64 -> kind contents_list -> kind contents_list t =
   fun desired_total_fee (* in mutez *) -> function
    | Single (Manager_operation data) ->
        let fee = Tez.of_mutez_exn desired_total_fee in
        return (Single (Manager_operation {data with fee; source}))
    | Cons (Manager_operation data, tail) ->
        let* local_fee =
          (* We generate some corner cases where some individual
             operations in the batch have zero fees. *)
          let* r = frequencyl [(7, `Random); (2, `Zero); (1, `All)] in
          match r with
          | `Random ->
              let* n = int_range 0 (Int64.to_int desired_total_fee) in
              return (Int64.of_int n)
          | `Zero -> return 0L
          | `All -> return desired_total_fee
        in
        let fee = Tez.of_mutez_exn local_fee in
        let* tail =
          set_fee_contents_list_gen (Int64.sub desired_total_fee local_fee) tail
        in
        return (Cons (Manager_operation {data with fee; source}, tail))
    | Single _ -> (* see precondition: manager operation *) assert false
  in
  let {shell = _; protocol_data = Operation_data data} = op in
  let contents = generate1 (set_fee_contents_list_gen fee data.contents) in
  {op with protocol_data = Operation_data {data with contents}}

(** Return an [Operation_hash.t] that is distinct from [different_from]. *)
let different_oph ~different_from =
  if Operation_hash.(different_from = zero) then (
    let new_hash = Operation_hash.hash_string ["1"] in
    assert (Operation_hash.(new_hash <> zero)) ;
    new_hash)
  else Operation_hash.zero

(** Check that {!Plugin.Mempool.fee_needed_to_overtake} correctly
    returns the minimal fee with which [candidate_op] would be
    guaranteed to be greater than [op_to_overtake].

    Precondition: both operations are manager operations with respective
    total fee and gas limit [fee_o], [gas_o] and [fee_c], [gas_c]. *)
let test_manager_ops (op_to_overtake, fee_o, gas_o) (candidate_op, fee_c, gas_c)
    =
  Log.debug
    "Test op_to_overtake: {fee=%dmutez; gas=%d} and candidate_op: \
     {fee=%dmutez; gas=%d}"
    fee_o
    gas_o
    fee_c
    gas_c ;
  let fee_needed =
    WithExceptions.Option.get ~loc:__LOC__
    @@ Plugin.Mempool.fee_needed_to_overtake
         ~op_to_overtake:(snd op_to_overtake)
         ~candidate_op:(snd candidate_op)
  in
  Log.debug "  --> fee_needed: %Ld" fee_needed ;
  (* We need to ensure that in the operation comparisons below, the
     hashes provided as first elements of the pairs are distinct.
     Indeed, {!Alpha_context.Operation.compare} always returns 0 when
     these hashes are equal, regardless of the operations themselves. *)
  let fake_oph = different_oph ~different_from:(fst op_to_overtake) in
  (* We also set the source to {!Signature.Public_key_hash.zero} in
     the operation that will be compared to [op_to_overtake], so that
     if their weights (fee/gas ratio) are equal, then the former is
     smaller (see [Operation_repr.compare_manager_weight]). *)
  let with_fee fee = (fake_oph, set_fee_and_source fee (snd candidate_op)) in
  let fee_smaller = Int64.sub fee_needed 1L in
  if Alpha_context.Operation.compare (with_fee fee_smaller) op_to_overtake > 0
  then
    Test.fail
      ~__LOC__
      "Adjusted candidate_op: {fee=%Ldmutez; gas=%d} with fee smaller than \
       fee_needed should be smaller than or equal to op_to_overtake: \
       {fee=%dmutez; gas=%d}"
      fee_smaller
      gas_c
      fee_o
      gas_o ;
  if Alpha_context.Operation.compare (with_fee fee_needed) op_to_overtake <= 0
  then
    Test.fail
      ~__LOC__
      "Adjusted candidate_op: {fee=%Ldmutez; gas=%d} with fee_needed should be \
       greater than op_to_overtake: {fee=%dmutez; gas=%d}"
      fee_needed
      gas_c
      fee_o
      gas_o

(** Test manager operations with hand-picked fee and gas. *)
let () =
  register_test
    ~title:"hand-picked fee and gas"
    ~additional_tags:["manager"; "handpicked"]
  @@ fun () ->
  (* Various relative gas limits and fees: equal, off by one,
     multiple/divisor, high ppcm, coprime, zero, one, much higher/lower, etc. *)
  let fee_in_mutez_and_gas_list =
    [
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
  List.iter (fun op -> List.iter (test_manager_ops op) ops) ops ;
  unit

(** Test manager operations with random fee and gas. *)
let () =
  register_test
    ~title:"random fee and gas"
    ~additional_tags:["manager"; "random"]
  @@ fun () ->
  let gen =
    let open QCheck2.Gen in
    let* fee_in_mutez = int_range 0 100_000_000 in
    let* gas = int_range 1 50_000_000 in
    let* op = Helpers.manager_op_with_fee_and_gas_gen ~fee_in_mutez ~gas in
    return (op, fee_in_mutez, gas)
  in
  iter_neighbors test_manager_ops (QCheck2.Gen.generate ~n:100 gen) ;
  unit
