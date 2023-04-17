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
     and type Proto.Mempool.validation_info = Proto.Mempool.validation_info =
Shell_plugin.No_filter (struct
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

(** Possible outcomes of protocol's [Mempool.add_operation] that we
    want to test. *)
type proto_add_outcome =
  | Proto_added  (** Return [Proto.Mempool.Added]. *)
  | Proto_replaced  (** Return [Proto.Mempool.Replaced]. *)
  | Proto_unchanged  (** Return [Proto.Mempool.Unchanged]. *)
  | Proto_branch_delayed  (** Fail with a [`Temporary] error. *)
  | Proto_branch_refused  (** Fail with a [`Branch] error. *)
  | Proto_refused  (** Fail with a [`Permanent] error. *)
  | Proto_crash  (** Raise an exception. *)

let proto_add_outcome_gen =
  (* We try to give higher weights to more usual outcomes, and in
     particular to [Proto_added] so that the number of operations in
     the mempool can grow. *)
  QCheck2.Gen.frequencyl
    [
      (4, Proto_added);
      (2, Proto_replaced);
      (2, Proto_unchanged);
      (1, Proto_branch_delayed);
      (1, Proto_branch_refused);
      (1, Proto_refused);
      (1, Proto_crash);
    ]

(** Mock protocol with a toy mempool that has an adjustable
    [add_operation] function: it behaves as instructed by the provided
    [proto_add_outcome].

    Unlike in [Mock_protocol], here [Mempool.t] is an actual state
    that keeps track of validated operations and can be retrieved with
    [Mempool.operations]. This allows the test below to check that
    operations were correctly added or removed. *)
module Toy_proto :
  Tezos_protocol_environment.PROTOCOL
    with type operation_data = unit
     and type operation = Mock_protocol.operation
     and type Mempool.validation_info = proto_add_outcome = struct
  include Mock_protocol

  module Mempool = struct
    include Mempool

    type t = operation Operation_hash.Map.t

    (* We use this type as a hack to tell [add_operation] which
       outcome we want. *)
    type validation_info = proto_add_outcome

    let init _ctxt _chain_id ~head_hash:_ ~head:_ ~cache:_ =
      Lwt_result.return (Proto_crash, Operation_hash.Map.empty)

    let operation_encoding =
      Data_encoding.conv
        (fun {shell; protocol_data = ()} -> shell)
        (fun shell -> {shell; protocol_data = ()})
        Operation.shell_header_encoding

    let encoding = Operation_hash.Map.encoding operation_encoding

    let add_operation ?check_signature:_ ?conflict_handler info state (oph, op)
        =
      if Option.is_none conflict_handler then
        QCheck2.Test.fail_reportf
          "Prevalidation should always call [Proto.Mempool.add_operation] with \
           an explicit [conflict_handler]." ;
      match (info : proto_add_outcome) with
      | Proto_added ->
          let state = Operation_hash.Map.add oph op state in
          Lwt_result.return (state, Added)
      | Proto_replaced ->
          let removed =
            match Operation_hash.Map.choose state with
            | Some (hash, _) -> hash
            | None ->
                (* This outcome should not be used when the mempool is
                   empty. See [consistent_outcomes]. *)
                assert false
          in
          let state = Operation_hash.Map.remove removed state in
          let state = Operation_hash.Map.add oph op state in
          Lwt_result.return (state, Replaced {removed})
      | Proto_unchanged -> Lwt_result.return (state, Unchanged)
      | Proto_branch_delayed ->
          Lwt_result.fail (Validation_error [Branch_delayed_error])
      | Proto_branch_refused ->
          Lwt_result.fail (Validation_error [Branch_refused_error])
      | Proto_refused -> Lwt_result.fail (Validation_error [Refused_error])
      | Proto_crash -> assert false

    let remove_operation state oph = Operation_hash.Map.remove oph state

    let merge ?conflict_handler:_ _ _ = assert false

    let operations = Fun.id
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
     particular to [Proto_added] so that the number of operations in
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
      let filter_state = Operation_hash.Set.add oph filter_state in
      match config with
      | F_no_replace -> Lwt_result.return (filter_state, `No_replace)
      | F_replace ->
          let replace_oph =
            match Operation_hash.Set.choose filter_state with
            | Some hash -> hash
            | None ->
                (* This outcome should not be used when the mempool is
                   empty. See [consistent_outcomes]. *)
                assert false
          in
          let filter_state =
            Operation_hash.Set.remove replace_oph filter_state
          in
          let replacement =
            (replace_oph, `Branch_delayed [Branch_delayed_error])
          in
          Lwt_result.return (filter_state, `Replace replacement)
      | F_branch_delayed ->
          Lwt_result.fail (`Branch_delayed [Branch_delayed_error])
      | F_branch_refused ->
          Lwt_result.fail (`Branch_refused [Branch_refused_error])
      | F_refused -> Lwt_result.fail (`Refused [Refused_error])
      | F_crash -> assert false

    let conflict_handler _ ~existing_operation:_ ~new_operation:_ = assert false
  end
end

(** Adjust the outcomes of [Proto.Mempool.add_operation] and
    [Filter.Mempool.add_operation_and_enforce_mempool_bound] we wish to
    test, to avoid asking these functions to return a result that
    wouldn't make sense. *)
let consistent_outcomes ~mempool_is_empty proto_outcome filter_outcome =
  if mempool_is_empty then
    (* If the mempool contains no valid operations, then there is no
       operation to replace, so outcomes can be neither
       [Proto_replaced] nor [F_replace]. *)
    let proto_outcome =
      match proto_outcome with
      | Proto_replaced -> Proto_added
      | _ -> proto_outcome
    in
    let filter_outcome =
      match filter_outcome with
      | F_replace -> F_no_replace
      | _ -> filter_outcome
    in
    (proto_outcome, filter_outcome)
  else
    (* If the protocol already causes the removal of an old operation,
       then the mempool is not full and the filter won't also remove
       an operation. In other words, the outcomes [Proto_replaced] and
       [F_replace] are incompatible. *)
    match (proto_outcome, filter_outcome) with
    | Proto_replaced, F_replace -> (Proto_replaced, F_no_replace)
    | _ -> (proto_outcome, filter_outcome)

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
  let add_op state (op, (proto_outcome, filter_outcome)) =
    let valid_ops_before = P.Internal_for_tests.get_valid_operations state in
    let filter_state_before = P.Internal_for_tests.get_filter_state state in
    assert (
      not (Operation_hash.Map.mem op.Shell_operation.hash valid_ops_before)) ;
    assert (not (Operation_hash.Set.mem op.hash filter_state_before)) ;
    let proto_outcome, filter_outcome =
      let mempool_is_empty = Operation_hash.Map.is_empty valid_ops_before in
      consistent_outcomes ~mempool_is_empty proto_outcome filter_outcome
    in
    let state = P.Internal_for_tests.set_validation_info state proto_outcome in
    let*! ( state,
            (_op : Mock_protocol.operation Shell_operation.operation),
            classification,
            replacements ) =
      P.add_operation state filter_outcome op
    in
    (* Check the classification. *)
    (match (proto_outcome, filter_outcome) with
    | (Proto_added | Proto_replaced), (F_no_replace | F_replace) ->
        check_classification __LOC__ ~expected:`Prechecked classification
    | (Proto_unchanged | Proto_branch_delayed), _
    | (Proto_added | Proto_replaced), F_branch_delayed ->
        check_classification __LOC__ ~expected:`Branch_delayed classification
    | Proto_branch_refused, _ | (Proto_added | Proto_replaced), F_branch_refused
      ->
        check_classification __LOC__ ~expected:`Branch_refused classification
    | Proto_refused, _ | (Proto_added | Proto_replaced), F_refused ->
        check_classification __LOC__ ~expected:`Refused classification
    | Proto_crash, _ | (Proto_added | Proto_replaced), F_crash ->
        check_classification_is_exn __LOC__ classification) ;
    (* Check whether the new operation has been added, whether there
       is a replacement, and when there is one, whether it has been removed. *)
    let valid_ops = P.Internal_for_tests.get_valid_operations state in
    let filter_state = P.Internal_for_tests.get_filter_state state in
    (match (proto_outcome, filter_outcome) with
    | Proto_added, F_no_replace ->
        assert (Operation_hash.Map.mem op.hash valid_ops) ;
        assert (Operation_hash.Set.mem op.hash filter_state) ;
        assert (List.is_empty replacements)
    | Proto_added, F_replace | Proto_replaced, F_no_replace -> (
        assert (Operation_hash.Map.mem op.hash valid_ops) ;
        assert (Operation_hash.Set.mem op.hash filter_state) ;
        match replacements with
        | [] | _ :: _ :: _ -> assert false
        | [(removed, _)] ->
            assert (Operation_hash.Map.mem removed valid_ops_before) ;
            assert (Operation_hash.Set.mem removed filter_state_before) ;
            assert (not (Operation_hash.Map.mem removed valid_ops)) ;
            assert (not (Operation_hash.Set.mem removed filter_state)))
    | Proto_replaced, F_replace ->
        (* [consistent_outcomes] makes this case impossible. *) assert false
    | _ ->
        assert (not (Operation_hash.Map.mem op.hash valid_ops)) ;
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
      generate ~n:nb_ops (pair proto_add_outcome_gen filter_add_outcome_gen))
  in
  let ops_and_outcomes, leftovers = List.combine_with_leftovers ops outcomes in
  assert (Option.is_none leftovers) ;
  let*! final_prevalidation_state =
    List.fold_left_s add_op prevalidation_state ops_and_outcomes
  in
  let final_valid_ops =
    P.Internal_for_tests.get_valid_operations final_prevalidation_state
  in
  let final_filter_state =
    P.Internal_for_tests.get_filter_state final_prevalidation_state
  in
  assert (
    Operation_hash.Map.cardinal final_valid_ops
    = Operation_hash.Set.cardinal final_filter_state) ;
  Operation_hash.Map.iter
    (fun oph _ -> assert (Operation_hash.Set.mem oph final_filter_state))
    final_valid_ops ;
  return_unit

(** Test [Prevalidation.remove_operation]. *)
let () =
  Init.register_test_that_needs_ctxt
    ~title:"remove_operation"
    ~additional_tags:["remove"]
  @@ fun ctxt ->
  (* Number of operations initially added to the validation state. *)
  let nb_initial_ops = 20 in
  (* Number of operations on which we will call [remove_operation]
     after we have added [nb_initial_ops] operations to the state. We
     must have [nb_ops_to_remove <= nb_initial_ops]; removal from an
     empty state will be tested separately. *)
  let nb_ops_to_remove = 10 in
  let open Lwt_result_syntax in
  let (module Chain_store) = make_chain_store ctxt in
  let module P = MakePrevalidation (Chain_store) (Toy_filter) in
  let timestamp : Time.Protocol.t = now () in
  let head = Init.genesis_block ~timestamp ctxt in
  let* state = P.create chain_store ~head ~timestamp in
  (* Test removal from empty state. *)
  let state = P.remove_operation state Operation_hash.zero in
  assert (
    Operation_hash.Map.is_empty
      (P.Internal_for_tests.get_valid_operations state)) ;
  assert (
    Operation_hash.Set.is_empty (P.Internal_for_tests.get_filter_state state)) ;
  (* For each operation that will be removed, we generate a boolean
     that indicates whether the operation should be present in the
     prevalidation state. *)
  let removed_op_was_present =
    QCheck2.Gen.(generate ~n:nb_ops_to_remove bool)
  in
  (* We will need as many original operations to remove as there are
     occurrences of [false] in [removed_op_was_present]. *)
  let nb_original_ops_to_remove =
    List.fold_left (fun n b -> if b then n else n + 1) 0 removed_op_was_present
  in
  (* We generate all needed operations at the same time to have
     [operations_gen]'s guarantee that their hashes are distinct. *)
  let ops = mk_ops (nb_initial_ops + nb_original_ops_to_remove) in
  (* Add [nb_initial_ops] operations to the prevalidation state. *)
  let state = P.Internal_for_tests.set_validation_info state Proto_added in
  let rec add_ops n state ops =
    if n <= 0 then return (state, ops)
    else
      match ops with
      | [] ->
          (* We generated more than [nb_initial_ops] operations. *) assert false
      | op :: remaining_ops ->
          let*! state, _op, _classification, _replacement =
            P.add_operation state F_no_replace op
          in
          add_ops (n - 1) state remaining_ops
  in
  let* state, ops = add_ops nb_initial_ops state ops in
  assert (
    Operation_hash.Map.cardinal
      (P.Internal_for_tests.get_valid_operations state)
    = nb_initial_ops) ;
  (* Call [Prevalidation.remove_operation] on [nb_ops_to_remove]
     operations, which are already present in the state or not as
     specified by [removed_op_was_present]. *)
  let rec remove_ops state old_cardinal removed_op_was_present ops =
    match removed_op_was_present with
    | [] -> ()
    | was_present :: rest_was_present ->
        let oph, remaining_ops =
          if was_present then
            match
              Operation_hash.Map.choose
                (P.Internal_for_tests.get_valid_operations state)
            with
            | Some (oph, _) -> (oph, ops)
            | None ->
                (* More operations have been added to the state than removed. *)
                assert false
          else
            match ops with
            | op :: remaining_ops -> (op.Shell_operation.hash, remaining_ops)
            | [] ->
                (* We generated enough operations for each occurrence of
                   [false] in [removed_op_was_present]. *)
                assert false
        in
        let state = P.remove_operation state oph in
        let valid_ops = P.Internal_for_tests.get_valid_operations state in
        let filter_state = P.Internal_for_tests.get_filter_state state in
        assert (not (Operation_hash.Map.mem oph valid_ops)) ;
        assert (not (Operation_hash.Set.mem oph filter_state)) ;
        let new_cardinal = Operation_hash.Map.cardinal valid_ops in
        assert (
          if was_present then new_cardinal = old_cardinal - 1
          else new_cardinal = old_cardinal) ;
        remove_ops state new_cardinal rest_was_present remaining_ops
  in
  remove_ops state nb_initial_ops removed_op_was_present ops ;
  return_unit
