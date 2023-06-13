(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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
    Component:    Prevalidation
    Invocation:   dune exec src/lib_shell/test/main.exe \
                  -- --file test_prevalidation.ml
    Subject:      Unit tests for {!Prevalidation.T}
*)

(* Note: the logic of Prevalidation.t.manager_map is not tested here
   because the mock protocol operations don't have a notion of
   manager. However, it will serve to indicate the fees needed to
   replace the previous operation in a manager conflict error, which
   will be tested in tezt. *)
(* TODO: https://gitlab.com/tezos/tezos/-/issues/5197
   Update the comment above once the fees needed to replace are
   implemented and tested. *)

let register_test ~title ~additional_tags =
  Test.register
    ~__FILE__
    ~title:("Shell: Mempool prevalidation: " ^ title)
    ~tags:(["mempool"; "prevalidation"] @ additional_tags)

module Init = struct
  let chain_id = Shell_test_helpers.chain_id

  (** [wrap_tzresult_lwt_disk f ()] provides an instance of {!Context.t} to
      a test [f]. For this, it creates a temporary directory on disk,
      populates it with the data required for a {!Context.t} and then calls
      [f] by passing it an empty [Context.t]. After [f] finishes, the state
      is cleaned up. *)
  let wrap_tzresult_lwt_disk
      (f : Tezos_protocol_environment.Context.t -> unit tzresult Lwt.t) () :
      unit tzresult Lwt.t =
    Lwt_utils_unix.with_tempdir "tezos_test_" (fun base_dir ->
        let open Lwt_result_syntax in
        let root = Filename.concat base_dir "context" in
        let*! idx = Context.init root in
        let* genesis =
          Context.commit_genesis
            idx
            ~chain_id
            ~time:Shell_test_helpers.genesis_time
            ~protocol:Shell_test_helpers.genesis_protocol_hash
        in
        let*! v = Context.checkout_exn idx genesis in
        let v = Tezos_shell_context.Shell_context.wrap_disk_context v in
        f v)

  let genesis_block ~timestamp ctxt =
    let context_hash = Context_ops.hash ~time:timestamp ctxt in
    let repr =
      Block_repr.create_genesis_block
        ~genesis:Shell_test_helpers.genesis
        context_hash
    in
    Store.Unsafe.block_of_repr repr

  (** Register a Tezt test from a function that takes a context as
      argument and returns a tzresult promise. *)
  let register_test_that_needs_ctxt f =
    register_test (fun () ->
        let open Lwt_syntax in
        let* res = wrap_tzresult_lwt_disk f () in
        match res with
        | Ok () -> unit
        | Error err ->
            Test.fail "Test failed with tztrace:@.%a" pp_print_trace err)
end

let make_chain_store ctxt =
  let module Chain_store = struct
    type chain_store = unit

    let context () _block : Tezos_protocol_environment.Context.t tzresult Lwt.t
        =
      Lwt_result_syntax.return ctxt

    let chain_id () = Init.chain_id
  end in
  (module Chain_store : Prevalidation.Internal_for_tests.CHAIN_STORE
    with type chain_store = unit)

(** Module [Environment_protocol_T_test.Mock_all_unit] (where all
    functions are [assert false]), with just enough functions actually
    implemented so that [Prevalidation.create] can be run successfully. *)
module Mock_protocol :
  Tezos_protocol_environment.PROTOCOL
    with type operation_data = unit
     and type operation_receipt = unit
     and type validation_state = unit
     and type application_state = unit = struct
  open Tezos_protocol_environment.Internal_for_tests
  include Environment_protocol_T_test.Mock_all_unit

  let begin_validation _ctxt _chain_id _mode ~predecessor:_ ~cache:_ =
    Lwt_result_syntax.return_unit

  module Mempool = struct
    include Mempool

    let init _ _ ~head_hash:_ ~head:_ ~cache:_ = Lwt_result.return ((), ())
  end
end

module MakeFilter (Proto : Tezos_protocol_environment.PROTOCOL) :
  Shell_plugin.FILTER
    with type Proto.operation_data = Proto.operation_data
     and type Proto.operation = Proto.operation
     and type Proto.Mempool.t = Proto.Mempool.t = Shell_plugin.No_filter (struct
  let hash = Protocol_hash.zero

  include Proto

  let complete_b58prefix _ = assert false
end)

module Mock_bounding :
  Prevalidator_bounding.T with type protocol_operation = Mock_protocol.operation =
struct
  type state = unit

  let empty = ()

  type protocol_operation = Mock_protocol.operation

  let add_operation _ _ _ = assert false

  let remove_operation _ _ = assert false
end

module MakePrevalidation = Prevalidation.Internal_for_tests.Make

let now () = Time.System.to_protocol (Tezos_base.Time.System.now ())

(** The value of [chain_store] used in all tests below. *)
let chain_store = ()

(** Test that [create] returns [Ok] in a pristine context. *)
let () =
  Init.register_test_that_needs_ctxt ~title:"create" ~additional_tags:["create"]
  @@ fun ctxt ->
  let open Lwt_result_syntax in
  let (module Chain_store) = make_chain_store ctxt in
  let module Filter = MakeFilter (Mock_protocol) in
  let module P = MakePrevalidation (Chain_store) (Filter) (Mock_bounding) in
  let timestamp : Time.Protocol.t = now () in
  let head = Init.genesis_block ~timestamp ctxt in
  let* _ = P.create chain_store ~head ~timestamp in
  return_unit

module Parser = Shell_operation.MakeParser (Mock_protocol)

(** A generator of [operation] values that makes sure
    to return distinct operations (hashes are not fake and they are
    all different). Returned maps are exactly of size [n]. *)
let operations_gen ~(n : int) =
  let mk_operation (hash, (raw : Operation.t)) =
    match Parser.parse hash raw with
    | Ok x -> x
    | Error err ->
        Format.printf "%a" Error_monad.pp_print_trace err ;
        assert false
  in
  let open QCheck2.Gen in
  (* We need to specify the protocol bytes generator to always generate the
     empty string, otherwise the call to [Parser.parse] will fail with the
     bytes being too long (hereby looking like an attack). *)
  let proto_gen : string QCheck2.Gen.t = QCheck2.Gen.pure "" in
  let+ (ops : Operation.t Operation_hash.Map.t) =
    Generators.raw_op_map_gen_n ~proto_gen ?block_hash_t:None n
  in
  List.map mk_operation (Operation_hash.Map.bindings ops)

let mk_ops n =
  let ops = QCheck2.Gen.generate1 (operations_gen ~n) in
  assert (Compare.List_length_with.(ops = n)) ;
  ops

let pp_classification fmt classification =
  let print_error_classification name trace =
    Format.fprintf fmt "%s: %a" name pp_print_trace trace
  in
  match classification with
  | `Validated -> Format.fprintf fmt "Validated"
  | `Branch_delayed trace -> print_error_classification "Branch_delayed" trace
  | `Branch_refused trace -> print_error_classification "Branch_refused" trace
  | `Refused trace -> print_error_classification "Refused" trace
  | `Outdated trace -> print_error_classification "Outdated" trace

let unexpected_classification ~__LOC__ expected classification =
  Test.fail
    "%s:@.Expected classification %s, but got %a"
    __LOC__
    expected
    pp_classification
    classification

let assert_success ~__LOC__ = function
  | `Validated -> ()
  | classification ->
      unexpected_classification ~__LOC__ "Validated" classification

let assert_operation_conflict ~__LOC__ = function
  | `Branch_delayed [Validation_errors.Operation_conflict _] -> ()
  | cl ->
      unexpected_classification
        ~__LOC__
        "Branch_delayed: [Operation_conflict]"
        cl

let assert_rejected_by_full_mempool ~__LOC__ = function
  | `Branch_delayed [Validation_errors.Rejected_by_full_mempool _] -> ()
  | cl ->
      unexpected_classification
        ~__LOC__
        "Branch_delayed: [Rejected_by_full_mempool]"
        cl

let assert_replacement ~__LOC__ = function
  (* Replaced by the protocol *)
  | `Outdated [Validation_errors.Operation_replacement _] -> ()
  (* Replaced by the filter *)
  | `Branch_delayed [Validation_errors.Removed_from_full_mempool _] -> ()
  | classification ->
      unexpected_classification
        ~__LOC__
        "Outdated: [Operation_replacement] or Branch_delayed: \
         [Removed_from_full_mempool]"
        classification

let assert_exn ~__LOC__ = function
  | `Branch_delayed [Exn _] -> ()
  | cl -> unexpected_classification ~__LOC__ "Branch_delayed: [Exn]" cl

type error += Branch_delayed_error | Branch_refused_error | Refused_error

let () =
  let register_aux category id from_error to_error =
    register_error_kind
      category
      ~id
      ~title:id
      ~description:id
      Data_encoding.empty
      from_error
      to_error
  in
  register_aux
    `Temporary
    "test.branch_delayed_error"
    (function Branch_delayed_error -> Some () | _ -> None)
    (fun () -> Branch_delayed_error) ;
  register_aux
    `Branch
    "test.branch_refused_error"
    (function Branch_refused_error -> Some () | _ -> None)
    (fun () -> Branch_refused_error) ;
  register_aux
    `Permanent
    "test.refused_error"
    (function Refused_error -> Some () | _ -> None)
    (fun () -> Refused_error)

let assert_branch_delayed_error ~__LOC__ = function
  | `Branch_delayed [Branch_delayed_error] -> ()
  | cl ->
      unexpected_classification
        ~__LOC__
        "Branch_delayed: [Branch_delayed_error]"
        cl

let assert_branch_refused_error ~__LOC__ = function
  | `Branch_refused [Branch_refused_error] -> ()
  | cl ->
      unexpected_classification
        ~__LOC__
        "Branch_refused: [Branch_refused_error]"
        cl

let assert_refused_error ~__LOC__ = function
  | `Refused [Refused_error] -> ()
  | cl -> unexpected_classification ~__LOC__ "Refused: [Refused_error]" cl

type proto_replacement = Replacement | No_replacement

(** Possible outcomes of protocol's [Mempool.add_operation] that we
    want to test. *)
type proto_outcome =
  | Proto_success of proto_replacement
      (** Return either [Proto.Mempool.Added] (when [proto_replacement] is
          [No_replacement]) or [Proto.Mempool.Replaced] (when it is
          [Replacement]. *)
  | Proto_unchanged  (** Return [Proto.Mempool.Unchanged]. *)
  | Proto_branch_delayed  (** Fail with a [`Temporary] error. *)
  | Proto_branch_refused  (** Fail with a [`Branch] error. *)
  | Proto_refused  (** Fail with a [`Permanent] error. *)
  | Proto_crash  (** Raise an exception. *)

let proto_outcome_gen =
  (* We try to give higher weights to more usual outcomes, and in
     particular to [Proto_success No_replacement] so that the number
     of operations in the mempool can grow. *)
  QCheck2.Gen.frequencyl
    [
      (8, Proto_success No_replacement);
      (2, Proto_success Replacement);
      (2, Proto_unchanged);
      (1, Proto_branch_delayed);
      (1, Proto_branch_refused);
      (1, Proto_refused);
      (1, Proto_crash);
    ]

(** Get a random operation hash from an [Operation_hash.Map.t].
    Fail with [Invalid_argument] when the map is empty. *)
let random_oph_from_map ophmap =
  fst QCheck2.Gen.(generate1 (oneofl (Operation_hash.Map.bindings ophmap)))

(** Mock protocol with a toy mempool that has an adjustable
    [add_operation] function.

    At the center of the toy mempool is the [Mempool.t] type, which is
    no longer [unit] as in [Mock_protocol]. Instead, this type has two
    components:

    - A map that keeps track of validated operations and can be
      retrieved with [Mempool.operations]. This allows the tests to check
      that operations were correctly added or removed.

    - A [proto_outcome] that allows the caller of
      [Mempool.add_operation] to specify the desired outcome of this
      function. It is then returned unchanged by the function. It is
      first initialized to [Proto_success No_replacement], so that the
      mempool can easily be filled with valid operations by simply
      leaving it unmodified. *)
module Toy_proto :
  Tezos_protocol_environment.PROTOCOL
    with type operation_data = unit
     and type operation = Mock_protocol.operation
     and type Mempool.t =
      Mock_protocol.operation Operation_hash.Map.t * proto_outcome = struct
  include Mock_protocol

  module Mempool = struct
    include Mempool

    type t = operation Operation_hash.Map.t * proto_outcome

    type validation_info = unit

    let init _ctxt _chain_id ~head_hash:_ ~head:_ ~cache:_ =
      (* The default outcome is [Proto_success No_replacement]: this
         is useful when we just want to set up a mempool that contains
         operations, as in [test_remove_operation]. *)
      Lwt_result.return
        ((), (Operation_hash.Map.empty, Proto_success No_replacement))

    (* Fake encoding that will not be used anyway. *)
    let encoding =
      Data_encoding.conv
        (fun _ -> ())
        (fun () -> (Operation_hash.Map.empty, Proto_success No_replacement))
        Data_encoding.unit

    let add_operation ?check_signature:_ ?conflict_handler _info
        (state, desired_outcome) (oph, op) =
      if Option.is_none conflict_handler then
        Test.fail
          "Prevalidation should always call [Proto.Mempool.add_operation] with \
           an explicit [conflict_handler]." ;
      match desired_outcome with
      (* To be able to replace an operation, we need the mempool to be
         non-empty. If it is empty, then [Proto_success Replacement] falls back
         to the behavior of [Proto_success No_replacement]. *)
      | Proto_success Replacement when not (Operation_hash.Map.is_empty state)
        ->
          let removed = random_oph_from_map state in
          let state = Operation_hash.Map.remove removed state in
          let state = Operation_hash.Map.add oph op state in
          Lwt_result.return ((state, desired_outcome), Replaced {removed})
      | Proto_success (No_replacement | Replacement) ->
          let state = Operation_hash.Map.add oph op state in
          Lwt_result.return ((state, desired_outcome), Added)
      | Proto_unchanged ->
          Lwt_result.return ((state, desired_outcome), Unchanged)
      | Proto_branch_delayed ->
          Lwt_result.fail (Validation_error [Branch_delayed_error])
      | Proto_branch_refused ->
          Lwt_result.fail (Validation_error [Branch_refused_error])
      | Proto_refused -> Lwt_result.fail (Validation_error [Refused_error])
      | Proto_crash -> assert false

    let remove_operation (state, desired_outcome) oph =
      (Operation_hash.Map.remove oph state, desired_outcome)

    let merge ?conflict_handler:_ _ _ = assert false

    let operations = fst
  end
end

module Toy_filter = MakeFilter (Toy_proto)

type bounding_outcome =
  | B_success of int
      (** Add the operation successfully, and replace [n] operations in the
          process (or replace all operations in the state if there are less
          than [n]).
          In practice we generate [0 <= n <= 3], and very often [n = 0]
          meaning that there is no replacement. *)
  | B_error
      (** Return [Error op_to_overtake_opt] where [op_to_overtake_opt] is
          randomly [None] or an operation present in the state. *)
  | B_crash  (** Raise an exception. *)

(** This generator returns:
    - with odds 3/5: [B_success 0]
    - with odds 1/15 each: [B_success n], for [1 <= n <= 3]
    - with odds 2/15: [B_error]
    - with odds 1/15: [B_crash]

    We strongly favor [B_success 0] (aka add the operation without any
    replacements) so that the mempool may grow on average. *)
let bounding_outcome_gen =
  let open QCheck2.Gen in
  frequency
    [
      ( 12,
        let* n = frequencyl [(9, 0); (1, 1); (1, 2); (1, 3)] in
        return (B_success n) );
      (2, pure B_error);
      (1, pure B_crash);
    ]

(** Toy bounding with an adjustable [add] function.

    As in [Toy_proto], the [state] has a dual role of tracking the
    valid operations and providing [add] with the desired
    [bounding_outcome].

    Note that despite its name, the [Toy_bounding] does not bound the
    mempool, but instead may return any output at any time, because
    the purpose of the current file is to test the [Prevalidation]
    component. The actual Bounding built by [Prevalidator_bounding.Make]
    is tested by itself in [test_prevalidator_bounding.ml]. *)
module Toy_bounding :
  Prevalidator_bounding.T
    with type protocol_operation = Toy_proto.operation
     and type state =
      Toy_proto.operation Shell_operation.operation Operation_hash.Map.t
      * bounding_outcome = struct
  type protocol_operation = Toy_proto.operation

  type state =
    Toy_proto.operation Shell_operation.operation Operation_hash.Map.t
    * bounding_outcome

  (* Similarly to [Toy_proto.Mempool.t], we initialize the [state]
     with the outcome [B_success 0] so that it is easy to add
     operations to the mempool and make it grow. *)
  let empty = (Operation_hash.Map.empty, B_success 0)

  (** Remove and return [n] random hashes from [ophmap].
      If [n > cardinal ophmap], return all the keys of [ophmap] instead.
      Not tail-rec because in practice it is used with n <= 3. *)
  let rec pop_n ophmap n =
    if n <= 0 || Operation_hash.Map.is_empty ophmap then (ophmap, [])
    else
      let oph = random_oph_from_map ophmap in
      let ophmap = Operation_hash.Map.remove oph ophmap in
      let ophmap, popped = pop_n ophmap (n - 1) in
      (ophmap, oph :: popped)

  let add_operation (ophmap, desired_outcome) _config op =
    match desired_outcome with
    | B_success n ->
        let ophmap, replaced = pop_n ophmap n in
        let ophmap = Operation_hash.Map.add op.Shell_operation.hash op ophmap in
        Ok ((ophmap, desired_outcome), replaced)
    | B_error ->
        let op_to_overtake =
          let open QCheck2.Gen in
          if Operation_hash.Map.is_empty ophmap || generate1 bool then None
          else
            Some (snd (generate1 (oneofl (Operation_hash.Map.bindings ophmap))))
        in
        Error op_to_overtake
    | B_crash -> assert false

  let remove_operation ((ophmap, outcome) as state) oph =
    if Operation_hash.Map.mem oph ophmap then
      (Operation_hash.Map.remove oph ophmap, outcome)
    else state
end

(** Test [Prevalidation.add_operation].

    For various outcomes of the protocol's [Mempool.add_operation] and
    the filter's [Mempool.add_operation_and_enforce_mempool_bound],
    check the returned classification and the updates of the protocol
    and filter internal states. *)
let () =
  Init.register_test_that_needs_ctxt
    ~title:"add_operation"
    ~additional_tags:["add"]
  @@ fun ctxt ->
  (* Number of operations that will be added. *)
  let nb_ops = 100 in
  let open Lwt_result_syntax in
  let open Shell_operation in
  let (module Chain_store) = make_chain_store ctxt in
  let module P = MakePrevalidation (Chain_store) (Toy_filter) (Toy_bounding) in
  let open P.Internal_for_tests in
  let add_op state (op, (proto_outcome, bounding_outcome)) =
    let proto_ophmap_before = get_mempool_operations state in
    let bounding_ophmap_before = fst (get_bounding_state state) in
    assert (not (Operation_hash.Map.mem op.hash proto_ophmap_before)) ;
    assert (not (Operation_hash.Map.mem op.hash bounding_ophmap_before)) ;
    let state = set_mempool state (proto_ophmap_before, proto_outcome) in
    let state =
      set_bounding_state state (bounding_ophmap_before, bounding_outcome)
    in
    let*! state, returned_op, classification, replacements =
      P.add_operation state P.default_config op
    in
    (* Check the classification. *)
    (match (proto_outcome, bounding_outcome) with
    | Proto_success _, B_success _ -> assert_success ~__LOC__ classification
    | Proto_success _, B_error ->
        assert_rejected_by_full_mempool ~__LOC__ classification
    | Proto_unchanged, _ -> assert_operation_conflict ~__LOC__ classification
    | Proto_branch_delayed, _ ->
        assert_branch_delayed_error ~__LOC__ classification
    | Proto_branch_refused, _ ->
        assert_branch_refused_error ~__LOC__ classification
    | Proto_refused, _ -> assert_refused_error ~__LOC__ classification
    | Proto_crash, _ | Proto_success _, B_crash ->
        assert_exn ~__LOC__ classification) ;
    (* Check that the states have been correctly updated. *)
    let proto_ophmap = get_mempool_operations state in
    let bounding_ophmap = fst (get_bounding_state state) in
    let count_before = Operation_hash.Map.cardinal proto_ophmap_before in
    let count = Operation_hash.Map.cardinal proto_ophmap in
    (match (proto_outcome, bounding_outcome) with
    | Proto_success proto_replacement, B_success nb_replaced_bounding ->
        assert (Operation_hash.Map.mem op.hash proto_ophmap) ;
        assert (Operation_hash.Map.mem op.hash bounding_ophmap) ;
        List.iter
          (fun (replaced, replacement_classification) ->
            assert (Operation_hash.Map.mem replaced proto_ophmap_before) ;
            assert (Operation_hash.Map.mem replaced bounding_ophmap_before) ;
            assert (not (Operation_hash.Map.mem replaced proto_ophmap)) ;
            assert (not (Operation_hash.Map.mem replaced bounding_ophmap)) ;
            assert_replacement ~__LOC__ replacement_classification)
          replacements ;
        let nb_replaced_proto =
          match proto_replacement with Replacement -> 1 | No_replacement -> 0
        in
        let nb_replaced = nb_replaced_proto + nb_replaced_bounding in
        let nb_replaced = min nb_replaced count_before in
        assert (List.compare_length_with replacements nb_replaced = 0) ;
        assert (count = count_before + 1 - nb_replaced)
    | _ ->
        assert (not (Operation_hash.Map.mem op.hash proto_ophmap)) ;
        assert (not (Operation_hash.Map.mem op.hash bounding_ophmap)) ;
        assert (List.is_empty replacements) ;
        assert (count = count_before)) ;
    (* Check that the operation has been correctly updated (or left alone). *)
    (match proto_outcome with
    | Proto_success _ | Proto_unchanged -> assert returned_op.signature_checked
    | _ -> assert (returned_op == op)) ;
    Lwt.return state
  in
  let timestamp : Time.Protocol.t = now () in
  let head = Init.genesis_block ~timestamp ctxt in
  let* prevalidation_state = P.create chain_store ~head ~timestamp in
  let ops = mk_ops nb_ops in
  let outcomes =
    QCheck2.Gen.(
      generate ~n:nb_ops (pair proto_outcome_gen bounding_outcome_gen))
  in
  let ops_and_outcomes, leftovers = List.combine_with_leftovers ops outcomes in
  assert (Option.is_none leftovers) ;
  let*! final_prevalidation_state =
    List.fold_left_s add_op prevalidation_state ops_and_outcomes
  in
  let final_proto_ophmap = get_mempool_operations final_prevalidation_state in
  let final_bounding_ophmap =
    fst (get_bounding_state final_prevalidation_state)
  in
  assert (
    Operation_hash.Map.cardinal final_proto_ophmap
    = Operation_hash.Map.cardinal final_bounding_ophmap) ;
  Operation_hash.Map.iter
    (fun oph _ -> assert (Operation_hash.Map.mem oph final_bounding_ophmap))
    final_proto_ophmap ;
  return_unit

(** Test [Prevalidation.remove_operation]. *)
let () =
  Init.register_test_that_needs_ctxt
    ~title:"remove_operation"
    ~additional_tags:["remove"]
  @@ fun ctxt ->
  (* Number of operations initially added to the validation state. *)
  let nb_initial_ops = 50 in
  (* Number of operations on which we will call [remove_operation]
     after we have added [nb_initial_ops] operations to the state. We
     must have [nb_ops_to_remove <= nb_initial_ops]; removal from an
     empty state will be tested separately. *)
  let nb_ops_to_remove = 30 in
  let open Lwt_result_syntax in
  let (module Chain_store) = make_chain_store ctxt in
  let module P = MakePrevalidation (Chain_store) (Toy_filter) (Toy_bounding) in
  let open P.Internal_for_tests in
  let timestamp : Time.Protocol.t = now () in
  let head = Init.genesis_block ~timestamp ctxt in
  (* Test removal from empty state. *)
  let* state = P.create chain_store ~head ~timestamp in
  let oph = QCheck2.Gen.generate1 Generators.operation_hash_gen in
  let state = P.remove_operation state oph in
  assert (Operation_hash.Map.is_empty (get_mempool_operations state)) ;
  assert (Operation_hash.Map.is_empty (fst (get_bounding_state state))) ;
  (* Prepare the initial state. *)
  let*! initial_state =
    List.fold_left_s
      (fun state op ->
        let*! state, _op, _classification, _replacement =
          P.add_operation state P.default_config op
        in
        Lwt.return state)
      state
      (mk_ops nb_initial_ops)
  in
  let initial_proto_ophmap = get_mempool_operations initial_state in
  let initial_cardinal = Operation_hash.Map.cardinal initial_proto_ophmap in
  assert (initial_cardinal = nb_initial_ops) ;
  (* Test the removal of present or fresh operations. *)
  let test_remove (state, proto_ophmap_before, cardinal_before) =
    if QCheck2.Gen.(generate1 bool) then (
      (* Remove a present operation. *)
      let oph = random_oph_from_map proto_ophmap_before in
      let state = P.remove_operation state oph in
      let proto_ophmap = get_mempool_operations state in
      let bounding_ophmap = fst (get_bounding_state state) in
      assert (not (Operation_hash.Map.mem oph proto_ophmap)) ;
      assert (not (Operation_hash.Map.mem oph bounding_ophmap)) ;
      let cardinal = Operation_hash.Map.cardinal proto_ophmap in
      assert (cardinal = cardinal_before - 1) ;
      assert (Operation_hash.Map.cardinal bounding_ophmap = cardinal) ;
      (state, proto_ophmap, cardinal))
    else
      (* Remove a fresh operation. *)
      let bounding_ophmap_before = get_bounding_state state in
      let oph =
        QCheck2.Gen.generate1 (Generators.fresh_oph_gen proto_ophmap_before)
      in
      let state = P.remove_operation state oph in
      let proto_ophmap = get_mempool_operations state in
      let bounding_ophmap = get_bounding_state state in
      (* Internal states are physically unchanged. *)
      assert (proto_ophmap == proto_ophmap_before) ;
      assert (bounding_ophmap == bounding_ophmap_before) ;
      (state, proto_ophmap, cardinal_before)
  in
  let rec fun_power f x n = if n <= 0 then x else fun_power f (f x) (n - 1) in
  let final_state, final_proto_ophmap, final_cardinal =
    fun_power
      test_remove
      (initial_state, initial_proto_ophmap, initial_cardinal)
      nb_ops_to_remove
  in
  let final_bounding_ophmap = fst (get_bounding_state final_state) in
  assert (Operation_hash.Map.cardinal final_bounding_ophmap = final_cardinal) ;
  assert (
    Operation_hash.Map.for_all
      (fun oph _op -> Operation_hash.Map.mem oph final_bounding_ophmap)
      final_proto_ophmap) ;
  return_unit
