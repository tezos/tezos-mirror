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
    Invocation:   dune exec src/lib_shell/test/main.exe -- -f test_prevalidation.ml
    Subject:      Unit tests for {!Prevalidation.T}
*)

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
     and type Mempool.state = unit
     and type Proto.Mempool.t = Proto.Mempool.t = Shell_plugin.No_filter (struct
  let hash = Protocol_hash.zero

  include Proto

  let complete_b58prefix _ = assert false
end)

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
  let module P = MakePrevalidation (Chain_store) (Filter) in
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
  | `Applied -> Format.fprintf fmt "Applied"
  | `Prechecked -> Format.fprintf fmt "Prechecked"
  | `Branch_delayed trace -> print_error_classification "Branch_delayed" trace
  | `Branch_refused trace -> print_error_classification "Branch_refused" trace
  | `Refused trace -> print_error_classification "Refused" trace
  | `Outdated trace -> print_error_classification "Outdated" trace

let check_classification_is_exn loc
    (classification : Prevalidator_classification.classification) =
  match classification with
  | `Branch_delayed [Exn _] -> ()
  | _ ->
      QCheck2.Test.fail_reportf
        "%s:@.Expected classification (Branch_delayed: [Exn]), but got %a"
        loc
        pp_classification
        classification

let pp_expected fmt = function
  | `Prechecked -> Format.fprintf fmt "Prechecked"
  | `Branch_delayed -> Format.fprintf fmt "Branch_delayed"
  | `Branch_refused -> Format.fprintf fmt "Branch_refused"
  | `Refused -> Format.fprintf fmt "Refused"

let check_classification loc ~expected
    (classification : Prevalidator_classification.classification) =
  match (expected, classification) with
  | `Prechecked, `Prechecked
  | `Branch_delayed, `Branch_delayed _
  | `Branch_refused, `Branch_refused _
  | `Refused, `Refused _ ->
      ()
  | _ ->
      QCheck2.Test.fail_reportf
        "%s:@.Expected classification %a, but got %a"
        loc
        pp_expected
        expected
        pp_classification
        classification

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
        QCheck2.Test.fail_reportf
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

(** Possible outcomes of filter's
    [Mempool.add_operation_and_enforce_mempool_bound] that we want to test. *)
type filter_add_outcome =
  | F_no_replace  (** Return [`No_replace]. *)
  | F_replace  (** Return [`Replace _]. *)
  | F_branch_delayed  (** Fail with a [`Temporary] error. *)
  | F_branch_refused  (** Fail with a [`Branch] error. *)
  | F_refused  (** Fail with a [`Permanent] error. *)
  | F_crash  (** Raise an exception. *)

let filter_add_outcome_encoding =
  Data_encoding.string_enum
    [
      ("No_replace", F_no_replace);
      ("Replace", F_replace);
      ("Branch_delayed", F_branch_delayed);
      ("Branch_refused", F_branch_refused);
      ("Refused", F_refused);
      ("Crash", F_crash);
    ]

let filter_add_outcome_gen =
  (* We try to give higher weights to more usual outcomes, and in
     particular to [F_no_replace] so that the number of operations in
     the mempool can grow. *)
  QCheck2.Gen.frequencyl
    [
      (8, F_no_replace);
      (4, F_replace);
      (1, F_branch_delayed);
      (1, F_branch_refused);
      (1, F_refused);
      (1, F_crash);
    ]

(** Toy mempool filter with an adjustable
    [add_operation_and_enforce_mempool_bound] and an actual [state] that
    keeps track of added operations. *)
module Toy_filter = struct
  include MakeFilter (Toy_proto)

  module Mempool = struct
    (* Once again, we hack this type to specify the desired outcome. *)
    type config = filter_add_outcome

    let config_encoding = filter_add_outcome_encoding

    let default_config = F_no_replace

    type state = Operation_hash.Set.t

    let init _ ~head:_ = Lwt_result.return Operation_hash.Set.empty

    let flush _ ~head:_ = assert false

    let remove ~filter_state oph = Operation_hash.Set.remove oph filter_state

    let pre_filter _ ~filter_state:_ _ = assert false

    let add_operation_and_enforce_mempool_bound ?replace config filter_state
        (oph, _op) =
      let filter_state =
        match replace with
        | None -> filter_state
        | Some replace_oph -> Operation_hash.Set.remove replace_oph filter_state
      in
      match config with
      (* To be able to replace an operation, we need the state to be
         non-empty and [replace] to be [None]. Indeed, if we have
         already removed an operation because of [replace], then the
         state is not full and the filter shouldn't also remove an
         operation. If these conditions are not fulfilled, then
         [F_replace] falls back to the behavior of [F_no_replace]. *)
      | F_replace
        when (not (Operation_hash.Set.is_empty filter_state))
             && Option.is_none replace ->
          let replace_oph =
            QCheck2.Gen.(
              generate1 (oneofl (Operation_hash.Set.elements filter_state)))
          in
          let filter_state =
            Operation_hash.Set.remove replace_oph filter_state
          in
          let filter_state = Operation_hash.Set.add oph filter_state in
          let replacement =
            (replace_oph, `Branch_delayed [Branch_delayed_error])
          in
          Lwt_result.return (filter_state, `Replace replacement)
      | F_no_replace | F_replace ->
          let filter_state = Operation_hash.Set.add oph filter_state in
          Lwt_result.return (filter_state, `No_replace)
      | F_branch_delayed ->
          Lwt_result.fail (`Branch_delayed [Branch_delayed_error])
      | F_branch_refused ->
          Lwt_result.fail (`Branch_refused [Branch_refused_error])
      | F_refused -> Lwt_result.fail (`Refused [Refused_error])
      | F_crash -> assert false

    let conflict_handler _ ~existing_operation:_ ~new_operation:_ = assert false
  end
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
  let (module Chain_store) = make_chain_store ctxt in
  let module P = MakePrevalidation (Chain_store) (Toy_filter) in
  let open P.Internal_for_tests in
  let add_op state (op, (proto_outcome, filter_outcome)) =
    let proto_ophmap_before = get_mempool_operations state in
    let filter_state_before = get_filter_state state in
    assert (
      not (Operation_hash.Map.mem op.Shell_operation.hash proto_ophmap_before)) ;
    assert (not (Operation_hash.Set.mem op.hash filter_state_before)) ;
    let state = set_mempool state (proto_ophmap_before, proto_outcome) in
    let*! ( state,
            (_op : Mock_protocol.operation Shell_operation.operation),
            classification,
            replacements ) =
      P.add_operation state filter_outcome op
    in
    (* Check the classification. *)
    (match (proto_outcome, filter_outcome) with
    | Proto_success _, (F_no_replace | F_replace) ->
        check_classification __LOC__ ~expected:`Prechecked classification
    | (Proto_unchanged | Proto_branch_delayed), _
    | Proto_success _, F_branch_delayed ->
        check_classification __LOC__ ~expected:`Branch_delayed classification
    | Proto_branch_refused, _ | Proto_success _, F_branch_refused ->
        check_classification __LOC__ ~expected:`Branch_refused classification
    | Proto_refused, _ | Proto_success _, F_refused ->
        check_classification __LOC__ ~expected:`Refused classification
    | Proto_crash, _ | Proto_success _, F_crash ->
        check_classification_is_exn __LOC__ classification) ;
    (* Check whether the new operation has been added, whether there
       is a replacement, and when there is one, whether it has been removed. *)
    let proto_ophmap = get_mempool_operations state in
    let filter_state = get_filter_state state in
    (match (proto_outcome, filter_outcome) with
    | Proto_success proto_replacement, (F_no_replace | F_replace) -> (
        assert (Operation_hash.Map.mem op.hash proto_ophmap) ;
        assert (Operation_hash.Set.mem op.hash filter_state) ;
        match (proto_replacement, filter_outcome) with
        | (Replacement, _ | _, F_replace)
          when not (Operation_hash.Map.is_empty proto_ophmap_before) -> (
            match replacements with
            | [] | _ :: _ :: _ -> assert false
            | [(removed, _)] ->
                assert (Operation_hash.Map.mem removed proto_ophmap_before) ;
                assert (Operation_hash.Set.mem removed filter_state_before) ;
                assert (not (Operation_hash.Map.mem removed proto_ophmap)) ;
                assert (not (Operation_hash.Set.mem removed filter_state)))
        | _ -> assert (List.is_empty replacements))
    | _ ->
        assert (not (Operation_hash.Map.mem op.hash proto_ophmap)) ;
        assert (not (Operation_hash.Set.mem op.hash filter_state)) ;
        assert (List.is_empty replacements)) ;
    Lwt.return state
  in
  let timestamp : Time.Protocol.t = now () in
  let head = Init.genesis_block ~timestamp ctxt in
  let* prevalidation_state = P.create chain_store ~head ~timestamp in
  let ops = mk_ops nb_ops in
  let outcomes =
    QCheck2.Gen.(
      generate ~n:nb_ops (pair proto_outcome_gen filter_add_outcome_gen))
  in
  let ops_and_outcomes, leftovers = List.combine_with_leftovers ops outcomes in
  assert (Option.is_none leftovers) ;
  let*! final_prevalidation_state =
    List.fold_left_s add_op prevalidation_state ops_and_outcomes
  in
  let final_proto_ophmap = get_mempool_operations final_prevalidation_state in
  let final_filter_state = get_filter_state final_prevalidation_state in
  assert (
    Operation_hash.Map.cardinal final_proto_ophmap
    = Operation_hash.Set.cardinal final_filter_state) ;
  Operation_hash.Map.iter
    (fun oph _ -> assert (Operation_hash.Set.mem oph final_filter_state))
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
  let module P = MakePrevalidation (Chain_store) (Toy_filter) in
  let open P.Internal_for_tests in
  let timestamp : Time.Protocol.t = now () in
  let head = Init.genesis_block ~timestamp ctxt in
  (* Test removal from empty state. *)
  let* state = P.create chain_store ~head ~timestamp in
  let oph = QCheck2.Gen.generate1 Generators.operation_hash_gen in
  let state = P.remove_operation state oph in
  assert (Operation_hash.Map.is_empty (get_mempool_operations state)) ;
  assert (Operation_hash.Set.is_empty (get_filter_state state)) ;
  (* Prepare the initial state. *)
  let*! initial_state =
    List.fold_left_s
      (fun state op ->
        let*! state, _op, _classification, _replacement =
          P.add_operation state F_no_replace op
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
      let filter_state = get_filter_state state in
      assert (not (Operation_hash.Map.mem oph proto_ophmap)) ;
      assert (not (Operation_hash.Set.mem oph filter_state)) ;
      let cardinal = Operation_hash.Map.cardinal proto_ophmap in
      assert (cardinal = cardinal_before - 1) ;
      assert (Operation_hash.Set.cardinal filter_state = cardinal) ;
      (state, proto_ophmap, cardinal))
    else
      (* Remove a fresh operation. *)
      let filter_state_before = get_filter_state state in
      let oph =
        QCheck2.Gen.generate1 (Generators.fresh_oph_gen proto_ophmap_before)
      in
      let state = P.remove_operation state oph in
      let proto_ophmap = get_mempool_operations state in
      let filter_state = get_filter_state state in
      (* Internal states are physically unchanged. *)
      assert (proto_ophmap == proto_ophmap_before) ;
      assert (filter_state == filter_state_before) ;
      (state, proto_ophmap, cardinal_before)
  in
  let rec fun_power f x n = if n <= 0 then x else fun_power f (f x) (n - 1) in
  let final_state, final_proto_ophmap, final_cardinal =
    fun_power
      test_remove
      (initial_state, initial_proto_ophmap, initial_cardinal)
      nb_ops_to_remove
  in
  let final_filter_state = get_filter_state final_state in
  assert (Operation_hash.Set.cardinal final_filter_state = final_cardinal) ;
  assert (
    Operation_hash.Map.for_all
      (fun oph _op -> Operation_hash.Set.mem oph final_filter_state)
      final_proto_ophmap) ;
  return_unit
