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
    Component:    Prevalidator_bounding
    Invocation:   dune exec src/lib_shell/test/main.exe -- -f test_prevalidator_bounding.ml
    Subject:      Unit tests for {!Prevalidator_bounding.T}
*)

let register_test ~title ~additional_tags =
  Test.register
    ~__FILE__
    ~title:("Shell: Mempool bounding: " ^ title)
    ~tags:(["mempool"; "bounding"] @ additional_tags)

(** {2 Module instantiations} *)

(** Same as [Mock_all_unit] except that [operation_data] is [int], so
    that it can be used by [compare_operations]. We will refer to this
    integer as the "weight" of an operation. *)
module Mock_protocol :
  Tezos_protocol_environment.PROTOCOL
    with type operation_data = int
     and type operation_receipt = unit
     and type validation_state = unit
     and type application_state = unit = struct
  include
    Tezos_protocol_environment.Internal_for_tests.Environment_protocol_T_test
    .Mock_all_unit

  type operation_data = int

  type operation = {
    shell : Operation.shell_header;
    protocol_data : operation_data;
  }

  let operation_data_encoding = Data_encoding.int16

  let operation_data_encoding_with_legacy_attestation_name =
    operation_data_encoding

  let operation_data_and_receipt_encoding =
    Data_encoding.conv fst (fun n -> (n, ())) Data_encoding.int16

  let operation_data_and_receipt_encoding_with_legacy_attestation_name =
    operation_data_and_receipt_encoding

  let acceptable_pass _ = assert false

  let compare_operations (oph1, {shell = _; protocol_data = n1})
      (oph2, {shell = _; protocol_data = n2}) =
    Compare.or_else (Int.compare n1 n2) (fun () ->
        Operation_hash.compare oph1 oph2)

  let validate_operation ?check_signature:_ = assert false

  let apply_operation _ = assert false

  module Mempool = struct
    include Mempool

    type conflict_handler =
      existing_operation:Tezos_crypto.Hashed.Operation_hash.t * operation ->
      new_operation:Tezos_crypto.Hashed.Operation_hash.t * operation ->
      [`Keep | `Replace]

    let add_operation ?check_signature:_ ?conflict_handler:_ _ _ _ =
      assert false

    let merge ?conflict_handler:_ () () = assert false

    let operations () = assert false
  end
end

module Bounding = Prevalidator_bounding.Internal_for_tests.Make (Mock_protocol)
open Shell_operation
open Prevalidator_bounding
open Bounding

type operation = protocol_operation Shell_operation.operation

(** {2 Misc Helpers} *)

let weight op = op.protocol.Mock_protocol.protocol_data

let pp_op fmt op = Format.fprintf fmt "{s=%d;w=%d}" op.size (weight op)

let pp_sep fmt () = Format.fprintf fmt "; "

let pp_state fmt state =
  Format.fprintf
    fmt
    "{ cardinal = %d; total_bytes = %d; ops are [ %a ]}"
    state.cardinal
    state.total_bytes
    (Format.pp_print_seq ~pp_sep pp_op)
    (Opset.to_seq state.opset)

let pp_option pp fmt = function
  | None -> Format.fprintf fmt "None"
  | Some x -> Format.fprintf fmt "(Some %a)" pp x

let pp_op_verbose fmt op =
  Format.fprintf
    fmt
    "{h=%a;s=%d;w=%d}"
    Operation_hash.pp_short
    op.hash
    op.size
    (weight op)

let pp_state_verbose fmt state =
  let pp_op_seq = Format.pp_print_seq ~pp_sep pp_op_verbose in
  Format.fprintf
    fmt
    "{ cardinal = %d; total_bytes = %d; min_op = %a;\n\
    \  opset = [ %a ]\n\
    \  ophmap = [ %a ]\n\
     }"
    state.cardinal
    state.total_bytes
    (pp_option pp_op_verbose)
    state.minop
    pp_op_seq
    (Opset.to_seq state.opset)
    pp_op_seq
    (Seq.map snd (Operation_hash.Map.to_seq state.ophmap))

(** Configuration used in the tests. *)
let config_for_tests = {max_operations = 10; max_total_bytes = 100}

(** Well-typed shell header: it doesn't matter in the tests. *)
let shell_header = {Operation.branch = Block_hash.zero}

(** Well-typed raw operation: it doesn't matter in the tests. *)
let default_raw_op = {Operation.shell = shell_header; proto = Bytes.empty}

let compare_ops op1 op2 =
  Mock_protocol.compare_operations
    (op1.hash, op1.protocol)
    (op2.hash, op2.protocol)

(** Check {!Bounding.state} invariants (both structural invariants and
    bound constraints specified in the given [config]). *)
let check_invariants ~__LOC__ config state =
  let broken_invariant format =
    Format.kasprintf
      (fun description ->
        Test.fail
          "%s:\nBroken state invariant: %s\nstate = %a@."
          __LOC__
          description
          pp_state_verbose
          state)
      format
  in
  (* Structural invariants *)
  (* Check cardinal. *)
  if not (Opset.cardinal state.opset = state.cardinal) then
    broken_invariant
      "cardinal = %d should be equal to cardinal(opset) = %d"
      state.cardinal
      (Opset.cardinal state.opset) ;
  if not (Operation_hash.Map.cardinal state.ophmap = state.cardinal) then
    broken_invariant
      "cardinal = %d should be equal to cardinal(ophmap) = %d"
      state.cardinal
      (Operation_hash.Map.cardinal state.ophmap) ;
  (* Check minop. *)
  if
    not
      (Option.equal
         (fun op1 op2 -> Operation_hash.equal op1.hash op2.hash)
         (Opset.min_elt state.opset)
         state.minop)
  then
    broken_invariant
      "minop = %a should be minimum(opset) = %a"
      (pp_option pp_op)
      state.minop
      (pp_option pp_op)
      (Opset.min_elt state.opset) ;
  (* Check that ophmap and opset contain the same operations (we
     already know that they have the same cardinal, so we only need to
     test that one is included in the other), and check total_bytes. *)
  let computed_total_bytes =
    Opset.fold
      (fun op sum_sizes ->
        if not (Operation_hash.Map.mem op.hash state.ophmap) then
          broken_invariant
            "opset and ophmap should contain the same operations, but %a is \
             present is opset and not in ophmap."
            Operation_hash.pp_short
            op.hash ;
        sum_sizes + op.size)
      state.opset
      0
  in
  if not (computed_total_bytes = state.total_bytes) then
    broken_invariant
      "total_bytes = %d should be the sum of the sizes of the elements of \
       opset which is %d."
      state.total_bytes
      computed_total_bytes ;
  (* Bound invariants *)
  if state.cardinal > config.max_operations then
    broken_invariant
      "cardinal = %d should be <= config.max_operations = %d."
      state.cardinal
      config.max_operations ;
  if state.total_bytes > config.max_total_bytes then
    broken_invariant
      "total_bytes = %d should be <= config.max_total_bytes = %d."
      state.total_bytes
      config.max_total_bytes

(** {2 Generators} *)

let oph_gen = Generators.operation_hash_gen

(** Default operation size generator: high chance of a "small"
    operation, medium chance of a "medium" operation, and low chance of
    a "large" operation. If [may_exceed_max_total_bytes] is true, then
    there is also a low chance that the generated size is higher than
    the maximal total size allowed by [config_for_tests]. *)
let size_gen ~may_exceed_max_total_bytes =
  let max_size = config_for_tests.max_total_bytes in
  let open QCheck2.Gen in
  let frequences =
    [(5, int_range 1 5); (3, int_range 6 30); (1, int_range 31 max_size)]
  in
  let frequences =
    if may_exceed_max_total_bytes then
      (1, int_range (max_size + 1) (10 * max_size)) :: frequences
    else frequences
  in
  frequency frequences

(** Default generator for weights. The bounds are chosen so that we
    can manually create operations with weight around 1000 that are
    smaller than all other generated operations. Always using weights
    in the thousands range lowers the risk of confusing them with sizes
    (which are in the 1-100 range). *)
let weight_gen = QCheck2.Gen.int_range 2000 3000

let make_op oph ~size ~weight =
  let signature_checked = QCheck2.Gen.(generate1 bool) in
  let data = {Mock_protocol.shell = shell_header; protocol_data = weight} in
  Shell_operation.Internal_for_tests.make_operation
    ~signature_checked
    ~size
    oph
    default_raw_op
    data

let copy_op_with_size op size = make_op op.hash ~size ~weight:(weight op)

let copy_op_with_weight op weight = make_op op.hash ~size:op.size ~weight

let op_gen ~may_exceed_max_total_bytes =
  let open QCheck2.Gen in
  let* oph = oph_gen in
  let* size = size_gen ~may_exceed_max_total_bytes in
  let* weight = weight_gen in
  return (make_op oph ~size ~weight)

(* Precondition: maximal_size >= 1 *)
let size_gen_for_initial_state maximal_size =
  let open QCheck2.Gen in
  (* All bounds must be >= 1 and <= maximal_size, and the min must
     be <= the max. *)
  let low_min = 1 in
  let low_max = max (maximal_size / 3) 1 in
  let medium_min = min (low_max + 1) maximal_size in
  let medium_max = max (maximal_size / 3) medium_min in
  let high_min = min (medium_max + 1) maximal_size in
  let high_max = maximal_size in
  frequency
    [
      (3, int_range low_min low_max);
      (2, int_range medium_min medium_max);
      (1, int_range high_min high_max);
    ]

(* Preconditions: n >= 1 and total_bytes >= n *)
let size_list_gen ~n ~total_bytes =
  let open QCheck2.Gen in
  let rec loop n remaining_size acc =
    if n <= 1 then return (remaining_size :: acc)
    else
      let* size = size_gen_for_initial_state (remaining_size - (n - 1)) in
      loop (n - 1) (remaining_size - size) (size :: acc)
  in
  loop n total_bytes []

let generate_fresh_oph ophmap =
  QCheck2.Gen.generate1 (Generators.fresh_oph_gen ophmap)

let rec fill_ophmap sizes weights ophmap =
  match (sizes, weights) with
  | [], [] -> ophmap
  | size :: sizes, weight :: weights ->
      let oph = generate_fresh_oph ophmap in
      let ophmap =
        Operation_hash.Map.add oph (make_op oph ~size ~weight) ophmap
      in
      fill_ophmap sizes weights ophmap
  | _ ->
      invalid_arg
        "fill_ophmap: inconsistent number of hashes, sizes, and weights"

let make_state sizes weights =
  let ophmap = fill_ophmap sizes weights Operation_hash.Map.empty in
  let opset, cardinal, total_bytes =
    Operation_hash.Map.fold
      (fun _oph op (opset, cardinal, size) ->
        (Opset.add op opset, cardinal + 1, size + op.size))
      ophmap
      (Opset.empty, 0, 0)
  in
  {opset; ophmap; minop = Opset.min_elt opset; cardinal; total_bytes}

(** Generate a non-empty {!type-state} that satisfies {!val-config}.
    (NB: Tests on an empty state are done separately.) *)
let state_gen ?cardinal ?total_bytes () =
  let open QCheck2.Gen in
  let* cardinal =
    match cardinal with
    | Some v -> pure v
    | None -> int_range 1 config_for_tests.max_operations
  in
  let* total_bytes =
    match total_bytes with
    | Some v -> pure v
    | None -> int_range cardinal config_for_tests.max_total_bytes
  in
  let* sizes = size_list_gen ~n:cardinal ~total_bytes in
  let* weights = list_size (pure cardinal) weight_gen in
  let state = make_state sizes weights in
  assert (state.cardinal = cardinal && state.total_bytes = total_bytes) ;
  return state

type state_and_ops = {
  state : state;
  present_op : operation;
  fresh_op : operation;
}

let state_and_ops_gen_aux ?cardinal ?total_bytes () =
  let open QCheck2.Gen in
  let* state = state_gen ?cardinal ?total_bytes () in
  let* present_op = oneofl (Opset.elements state.opset) in
  let* fresh_oph = Generators.fresh_oph_gen state.ophmap in
  let* size = size_gen ~may_exceed_max_total_bytes:true in
  let* weight = weight_gen in
  let fresh_op = make_op fresh_oph ~size ~weight in
  return {state; present_op; fresh_op}

let state_and_ops_gen = state_and_ops_gen_aux ()

(** Generate a full state, as well as present and fresh operations.
    The state is full in terms of both operation cardinal and total size
    with probability 1/2, or only either one with probability 1/4 each. *)
let full_state_and_ops_gen =
  let open QCheck2.Gen in
  let* both_full_cardinal_and_full_size = bool in
  if both_full_cardinal_and_full_size then
    state_and_ops_gen_aux
      ~cardinal:config_for_tests.max_operations
      ~total_bytes:config_for_tests.max_total_bytes
      ()
  else
    let* full_cardinal = bool in
    if full_cardinal then
      state_and_ops_gen_aux ~cardinal:config_for_tests.max_operations ()
    else state_and_ops_gen_aux ~total_bytes:config_for_tests.max_total_bytes ()

(** {2 Tests} *)

(* Test addition to and removal from an empty state, and check that
   removing the added operation goes back to an empty state. *)
let () =
  register_test ~title:"add/remove (empty state)" ~additional_tags:["empty"]
  @@ fun () ->
  let config = config_for_tests in
  let op = QCheck2.Gen.generate1 (op_gen ~may_exceed_max_total_bytes:false) in
  (* Removal from an empty state does nothing:
     the same state (physically) is returned. *)
  let state = remove_operation empty op.hash in
  assert (state == empty) ;
  (* Add an operation to the empty state. *)
  let state, replacements =
    WithExceptions.Result.get_ok ~loc:__LOC__ (add_operation empty config op)
  in
  (* Check that the state contains a single operation which is [op].
     Then the other fields are correct thanks to [check_invariants]. *)
  assert (state.cardinal = 1) ;
  assert (Opset.mem op state.opset) ;
  check_invariants ~__LOC__ config state ;
  assert (List.is_empty replacements) ;
  (* Removing the added operation goes back to an empty state. *)
  let state = remove_operation state op.hash in
  assert (state = empty) ;
  (* Adding an operation larger than the max_total_bytes fails.
     Moreover, there is no returned op_to_overtake, since the
     operation can never be added to the state no matter its weight. *)
  let over_max_op = copy_op_with_size op (config.max_total_bytes + 1) in
  assert (add_operation empty config over_max_op = Error None) ;
  (* Adding an operation with exactly the max_total_bytes succeeds. *)
  let max_op = copy_op_with_size op config.max_total_bytes in
  assert (Result.is_ok (add_operation empty config max_op)) ;
  unit

(** Return the cardinal and total size of all operations in opset that
    are smaller than op. *)
let nb_and_size_smaller op opset =
  let rec loop opset acc_nb acc_size =
    match Opset.min_elt opset with
    | Some smaller_op when compare_ops smaller_op op < 0 ->
        let opset = Opset.remove smaller_op opset in
        loop opset (acc_nb + 1) (acc_size + smaller_op.size)
    | Some _ | None -> (acc_nb, acc_size)
  in
  loop opset 0 0

let check_successful_add ?expected_nb_replacements ~state_before ~state
    ~replacements op =
  check_invariants ~__LOC__ config_for_tests state ;
  assert (Opset.mem op state.opset) ;
  assert (Operation_hash.Map.mem op.hash state.ophmap) ;
  let minop = WithExceptions.Option.get ~loc:__LOC__ state.minop in
  let nb_replaced, size_replaced =
    List.fold_left
      (fun (nb_repl, size_repl) replaced_oph ->
        (* Each replaced operation should be in state_before but not in
           state, and should be smaller than the new minimal operation. *)
        let replaced_op =
          WithExceptions.Option.get
            ~loc:__LOC__
            (Operation_hash.Map.find replaced_oph state_before.ophmap)
        in
        assert (not (Opset.mem replaced_op state.opset)) ;
        assert (not (Operation_hash.Map.mem replaced_oph state.ophmap)) ;
        assert (compare_ops replaced_op minop < 0) ;
        (nb_repl + 1, size_repl + replaced_op.size))
      (0, 0)
      replacements
  in
  assert (state.cardinal = state_before.cardinal - nb_replaced + 1) ;
  assert (state.total_bytes = state_before.total_bytes - size_replaced + op.size) ;
  Option.iter
    (fun expected_nb_replacements ->
      Check.((expected_nb_replacements = nb_replaced) int)
        ~error_msg:"Expected %L replacements but got %R.")
    expected_nb_replacements

let check_failed_add state op ~op_to_overtake =
  (* Failure to add an operation should only happen when removing all
     operations that are smaller is not enough to make room for it. *)
  let nb_smaller, size_smaller = nb_and_size_smaller op state.opset in
  assert (
    state.cardinal - nb_smaller + 1 > config_for_tests.max_operations
    || state.total_bytes - size_smaller + op.size
       > config_for_tests.max_total_bytes) ;
  (* Moreover, op with a revised weight should be accepted by
     the state if and only if it exceeds op_to_overtake. *)
  match op_to_overtake with
  | Some op_to_overtake ->
      let weight_to_overtake = weight op_to_overtake in
      let op_lighter = copy_op_with_weight op (weight_to_overtake - 1) in
      assert (Result.is_error (add_operation state config_for_tests op_lighter)) ;
      let op_heavier = copy_op_with_weight op (weight_to_overtake + 1) in
      assert (Result.is_ok (add_operation state config_for_tests op_heavier))
      (* (We don't test with exactly the same weight as op_to_overtake,
          because then the hashes would be used as a tie-breaker.) *)
  | None ->
      (* If op_to_overtake is None then the operation cannot be added, no
         matter its weight. We test with a weight much higher than all
         weights in the state (see [weight_gen]). *)
      let op_heaviest = copy_op_with_weight op 10_000 in
      assert (Result.is_error (add_operation state config_for_tests op_heaviest))

(* Preconditions:
   - [state_before] verifies the state invariants
   - [op] is not in [state_before] *)
let check_add_fresh state_before op =
  assert (not (Operation_hash.Map.mem op.hash state_before.ophmap)) ;
  match add_operation state_before config_for_tests op with
  | Ok (state, replacements) ->
      check_successful_add ~state_before ~state ~replacements op
  | Error op_to_overtake -> check_failed_add state_before op ~op_to_overtake

(* Preconditions:
   - [state_before] verifies the state invariants
   - [op] is present in [state_before] *)
let check_add_present state_before op =
  assert (Opset.mem op state_before.opset) ;
  let res = add_operation state_before config_for_tests op in
  (* add_operation should return the same state (physical equality
     intended) and an empty list of replacements. *)
  let state, replacements = WithExceptions.Result.get_ok ~loc:__LOC__ res in
  assert (state == state_before) ;
  assert (List.is_empty replacements)

(* Preconditions:
   - [state_before] verifies the state invariants
   - [op] is present in [state_before] *)
let check_remove_present state_before op =
  assert (Opset.mem op state_before.opset) ;
  let state = remove_operation state_before op.hash in
  check_invariants ~__LOC__ config_for_tests state ;
  assert (not (Opset.mem op state.opset)) ;
  assert (not (Operation_hash.Map.mem op.hash state.ophmap)) ;
  assert (state.cardinal = state_before.cardinal - 1) ;
  assert (state.total_bytes = state_before.total_bytes - op.size)

(* Preconditions:
   - [state_before] verifies the state invariants
   - [op] is not in [state_before] *)
let check_remove_fresh state_before op =
  assert (not (Operation_hash.Map.mem op.hash state_before.ophmap)) ;
  (* Should return the same state (physical equality intended). *)
  let state = remove_operation state_before op.hash in
  assert (state == state_before)

let test_add_remove {state; present_op; fresh_op} =
  Log.debug
    "Testing with\n  state: %a\n  present_op: %a\n  fresh_op: %a"
    pp_state
    state
    pp_op
    present_op
    pp_op
    fresh_op ;
  check_add_fresh state fresh_op ;
  check_add_present state present_op ;
  check_remove_present state present_op ;
  check_remove_fresh state fresh_op

(* Test the [add_operation] and [remove_operation] functions on random
   non-empty states. Various scenarios involving either function are
   grouped in this single test in order to mutualize the generation of
   random states. *)
let () =
  register_test ~title:"add/remove (random state)" ~additional_tags:["random"]
  @@ fun () ->
  let state_and_ops_list = QCheck2.Gen.generate ~n:50 state_and_ops_gen in
  List.iter test_add_remove state_and_ops_list ;
  unit

(* Test the [add_operation] and [remove_operation] functions on random
   full states (full cardinal or full total byte size or
   both). Various scenarios involving either function are grouped in
   this single test in order to mutualize the generation of random
   full states. *)
let () =
  register_test
    ~title:"add/remove (random full state)"
    ~additional_tags:["random"; "full"]
  @@ fun () ->
  let state_and_ops_list = QCheck2.Gen.generate ~n:20 full_state_and_ops_gen in
  List.iter test_add_remove state_and_ops_list ;
  unit

let check_add_fails ~__LOC__ state op =
  Log.debug "Check failure: add op %a to %a" pp_op op pp_state state ;
  match add_operation state config_for_tests op with
  | Ok _ -> Test.fail "%s: add_operation unexpectedly succeeded." __LOC__
  | Error op_to_overtake -> (
      try check_failed_add state op ~op_to_overtake
      with exn ->
        Test.fail
          "%s: Problem while checking failure of add_operation:\n%s"
          __LOC__
          (Printexc.to_string exn))

let add_successfully ~__LOC__ ~expected_nb_replacements state_before op =
  Log.debug "Check success: add op %a to %a" pp_op op pp_state state_before ;
  let ((state, replacements) as res) =
    WithExceptions.Result.get_ok ~loc:__LOC__
    @@ add_operation state_before config_for_tests op
  in
  (try
     check_successful_add
       ~expected_nb_replacements
       ~state_before
       ~state
       ~replacements
       op
   with exn ->
     Test.fail
       "%s: Unexpected output while adding operation:\n%s"
       __LOC__
       (Printexc.to_string exn)) ;
  res

let check_add_succeeds ~__LOC__ ~expected_nb_replacements state op =
  ignore (add_successfully ~__LOC__ ~expected_nb_replacements state op)

(* Test addition to a state that has the maximal number of operations. *)
let () =
  register_test
    ~title:"add to full state (full cardinal)"
    ~additional_tags:["full"]
  @@ fun () ->
  (* State with 9 operations (out of config_for_tests.max_operations = 10)
     and total size 50 (out of max_total_bytes = 100) and all operation
     weights between 2000 and 3000 (see [weight_gen]). *)
  let state =
    QCheck2.Gen.generate1 (state_gen ~cardinal:9 ~total_bytes:50 ())
  in
  (* Add an operation with weight 1000 and size 1. The state is now full. *)
  let oph1000 = generate_fresh_oph state.ophmap in
  let op1000 = make_op oph1000 ~size:1 ~weight:1000 in
  let state, _replacements =
    add_successfully ~__LOC__ ~expected_nb_replacements:0 state op1000
  in
  assert (state.minop = Some op1000) ;
  assert (state.cardinal = config_for_tests.max_operations) ;
  (* Adding an operation with weight 999 fails. *)
  let fresh_oph = generate_fresh_oph state.ophmap in
  check_add_fails ~__LOC__ state (make_op fresh_oph ~size:1 ~weight:999) ;
  (* Adding an operation with weight 1001 succeeds and replaces the
     operation with weight 1000. *)
  let op1001 = make_op fresh_oph ~size:1 ~weight:1001 in
  let state, replacements =
    add_successfully ~__LOC__ ~expected_nb_replacements:1 state op1001
  in
  assert (state.minop = Some op1001) ;
  assert (replacements = [oph1000]) ;
  unit

(* Test addition to a state that has the maximal total size, or close to it. *)
let () =
  register_test
    ~title:"add to (almost) full state (full bytes)"
    ~additional_tags:["full"]
  @@ fun () ->
  (* Craft a state with full total size. *)
  let sizes = [10; 40; 20; 1; 29] in
  let weights = [1000; 1100; 1200; 1300; 1400] in
  let state = make_state sizes weights in
  assert (state.total_bytes = config_for_tests.max_total_bytes) ;
  let fresh_oph = generate_fresh_oph state.ophmap in
  (* Adding operations with insufficient weight for their size. *)
  check_add_fails ~__LOC__ state (make_op fresh_oph ~size:1 ~weight:999) ;
  check_add_fails ~__LOC__ state (make_op fresh_oph ~size:11 ~weight:1050) ;
  check_add_fails ~__LOC__ state (make_op fresh_oph ~size:51 ~weight:1150) ;
  check_add_fails ~__LOC__ state (make_op fresh_oph ~size:90 ~weight:1350) ;
  (* Adding operations with enough weight for their size. *)
  check_add_succeeds ~__LOC__ ~expected_nb_replacements:1 state
  @@ make_op fresh_oph ~size:10 ~weight:1001 ;
  check_add_succeeds ~__LOC__ ~expected_nb_replacements:2 state
  @@ make_op fresh_oph ~size:50 ~weight:1101 ;
  check_add_succeeds ~__LOC__ ~expected_nb_replacements:4 state
  @@ make_op fresh_oph ~size:71 ~weight:1301 ;
  (* Replace the operation with weight 1000 and size 10 with an operation
     with weight 1001 and size 5 to have an almost full state. *)
  let state, _replacements =
    add_successfully ~__LOC__ ~expected_nb_replacements:1 state
    @@ make_op fresh_oph ~size:5 ~weight:1001
  in
  assert (state.total_bytes = 95) ;
  let fresh_oph = generate_fresh_oph state.ophmap in
  (* Adding operations with insufficient weight for their size. *)
  check_add_fails ~__LOC__ state (make_op fresh_oph ~size:6 ~weight:1000) ;
  check_add_fails ~__LOC__ state (make_op fresh_oph ~size:30 ~weight:1099) ;
  check_add_fails ~__LOC__ state (make_op fresh_oph ~size:60 ~weight:1199) ;
  (* Adding operations with enough weight for their size. *)
  check_add_succeeds ~__LOC__ ~expected_nb_replacements:0 state
  @@ make_op fresh_oph ~size:5 ~weight:0 ;
  check_add_succeeds ~__LOC__ ~expected_nb_replacements:1 state
  @@ make_op fresh_oph ~size:10 ~weight:1002 ;
  check_add_succeeds ~__LOC__ ~expected_nb_replacements:3 state
  @@ make_op fresh_oph ~size:60 ~weight:1201 ;
  check_add_succeeds ~__LOC__ ~expected_nb_replacements:5 state
  @@ make_op fresh_oph ~size:100 ~weight:2000 ;
  unit

let init_list n f =
  WithExceptions.Result.get_ok
    ~loc:__LOC__
    (List.init ~when_negative_length:() n f)

(* Test addition to a state that has both the maximal number of
   operations and the maximal total size. *)
let () =
  register_test
    ~title:"add to full state (full cardinal and full bytes)"
    ~additional_tags:["full"]
  @@ fun () ->
  (* Craft a state with full cardinal and full total size. Operations all
     have size 10 and their weights are [1000; 1100; 1200; ... ; 1900]. *)
  let sizes = init_list 10 (fun _n -> 10) in
  let weights = init_list 10 (fun n -> 1000 + (100 * n)) in
  let state = make_state sizes weights in
  assert (state.cardinal = config_for_tests.max_operations) ;
  assert (state.total_bytes = config_for_tests.max_total_bytes) ;
  let fresh_oph = generate_fresh_oph state.ophmap in
  (* Adding operations with insufficient weight for their size. *)
  check_add_fails ~__LOC__ state (make_op fresh_oph ~size:1 ~weight:999) ;
  check_add_fails ~__LOC__ state (make_op fresh_oph ~size:11 ~weight:1099) ;
  check_add_fails ~__LOC__ state (make_op fresh_oph ~size:31 ~weight:1299) ;
  check_add_fails ~__LOC__ state (make_op fresh_oph ~size:91 ~weight:1899) ;
  (* Insufficient weight; the cardinal is full, regardless of the size. *)
  check_add_fails ~__LOC__ state (make_op fresh_oph ~size:0 ~weight:999) ;
  (* Adding operations with enough weight for their size. *)
  check_add_succeeds ~__LOC__ ~expected_nb_replacements:1 state
  @@ make_op fresh_oph ~size:1 ~weight:1001 ;
  check_add_succeeds ~__LOC__ ~expected_nb_replacements:2 state
  @@ make_op fresh_oph ~size:11 ~weight:1101 ;
  check_add_succeeds ~__LOC__ ~expected_nb_replacements:4 state
  @@ make_op fresh_oph ~size:31 ~weight:1301 ;
  check_add_succeeds ~__LOC__ ~expected_nb_replacements:10 state
  @@ make_op fresh_oph ~size:91 ~weight:1901 ;
  (* Don't replace more operations than necessary, even with a high weight. *)
  check_add_succeeds ~__LOC__ ~expected_nb_replacements:1 state
  @@ make_op fresh_oph ~size:1 ~weight:3000 ;
  check_add_succeeds ~__LOC__ ~expected_nb_replacements:4 state
  @@ make_op fresh_oph ~size:40 ~weight:3000 ;
  unit
