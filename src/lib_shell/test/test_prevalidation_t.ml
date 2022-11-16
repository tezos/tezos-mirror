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

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/4113

   This file is part of the test suite for the new mempool, which
   uses features of the protocol that only exist since Lima.

   When you modify this file, consider whether you should also change
   the ones that test the legacy mempool for Kathmandu. They all
   start with the "legacy" prefix and will be removed when Lima is
   activated on Mainnet. *)

(** Testing
    -------
    Component:    Prevalidation
    Invocation:   dune exec src/lib_shell/test/test_prevalidation_t.exe
    Subject:      Unit tests for {!Prevalidation.T}
*)

module Mock_protocol :
  Tezos_protocol_environment.PROTOCOL
    with type operation_data = unit
     and type operation_receipt = unit
     and type validation_state = unit
     and type application_state = unit = struct
  open Tezos_protocol_environment.Internal_for_tests
  include Environment_protocol_T_test.Mock_all_unit

  (* We need to override these functions so that they're not [assert
     false], because the tests below use [Prevalidation.create] which
     calls them. *)

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
     and type Mempool.state = unit = Shell_plugin.No_filter (struct
  let hash = Tezos_crypto.Protocol_hash.zero

  include Proto

  let complete_b58prefix _ = assert false
end)

module Mock_filter = MakeFilter (Mock_protocol)

let filter_state : Mock_filter.Mempool.state = ()

let filter_config = Mock_filter.Mempool.default_config

module Init = struct
  let genesis_protocol =
    Tezos_crypto.Protocol_hash.of_b58check_exn
      "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp"

  let chain_id = Tezos_crypto.Chain_id.zero

  let genesis_time = Time.Protocol.of_seconds 0L

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
            ~time:genesis_time
            ~protocol:genesis_protocol
        in
        let*! v = Context.checkout_exn idx genesis in
        let v = Tezos_shell_context.Shell_context.wrap_disk_context v in
        f v)

  let genesis_block (context_hash : Tezos_crypto.Context_hash.t) : Store.Block.t
      =
    let block_hash : Tezos_crypto.Block_hash.t =
      Tezos_crypto.Block_hash.hash_string ["genesis"]
    in
    let genesis : Genesis.t =
      {time = genesis_time; block = block_hash; protocol = genesis_protocol}
    in
    let repr : Block_repr.t =
      Block_repr.create_genesis_block ~genesis context_hash
    in
    Store.Unsafe.block_of_repr repr
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

module MakePrevalidation = Prevalidation.Internal_for_tests.Make

let make_prevalidation_mock_protocol ctxt =
  let (module Chain_store) = make_chain_store ctxt in
  let module Prevalidation_t = MakePrevalidation (Chain_store) (Mock_filter) in
  (module Prevalidation_t : Prevalidation.T
    with type protocol_operation = Mock_protocol.operation
     and type filter_state = Mock_filter.Mempool.state
     and type filter_config = Mock_filter.Mempool.config
     and type chain_store = unit)

let now () = Time.System.to_protocol (Tezos_base.Time.System.now ())

(** The value of [chain_store] used in all tests below. *)
let chain_store = ()

(** Test that [create] returns [Ok] in a pristine context. *)
let test_create ctxt =
  let open Lwt_result_syntax in
  let timestamp : Time.Protocol.t = now () in
  let (module P) = make_prevalidation_mock_protocol ctxt in
  let head = Init.genesis_block @@ Context_ops.hash ~time:timestamp ctxt in
  let* _ = P.create chain_store ~head ~timestamp () in
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
  let proto_gen : string QCheck2.Gen.t = QCheck2.Gen.return "" in
  let+ (ops : Operation.t Tezos_crypto.Operation_hash.Map.t) =
    Generators.raw_op_map_gen_n ~proto_gen ?block_hash_t:None n
  in
  List.map mk_operation (Tezos_crypto.Operation_hash.Map.bindings ops)

(** The number of operations used by tests that follow *)
let nb_ops = 100

let mk_ops () =
  let ops = QCheck2.Gen.generate1 (operations_gen ~n:nb_ops) in
  assert (Compare.List_length_with.(ops = nb_ops)) ;
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

(** Test that [Prevalidation.add_operation] always returns
    [Branch_delayed [Exn _]] when the protocol's
    [Mempool.add_operation] crashes.

    Indeed, recall that [Mock_protocol] is built from
    [Environment_protocol_T_test.Mock_all_unit] which implements all
    functions as [assert false]. *)
let test_add_operation_crash ctxt =
  let open Lwt_result_syntax in
  let timestamp : Time.Protocol.t = now () in
  let ops = mk_ops () in
  let head = Init.genesis_block @@ Context_ops.hash ~time:timestamp ctxt in
  let (module P) = make_prevalidation_mock_protocol ctxt in
  let* pv = P.create chain_store ~head ~timestamp () in
  let add_op pv op =
    let*! ( pv,
            (_filter_state : P.filter_state),
            (_op : Mock_protocol.operation Shell_operation.operation),
            classification,
            (_replacement : P.replacement) ) =
      P.add_operation pv filter_state filter_config op
    in
    check_classification_is_exn __LOC__ classification ;
    Lwt.return pv
  in
  let*! _ = List.fold_left_s add_op pv ops in
  return_unit

module Proto_random_add_operation :
  Tezos_protocol_environment.PROTOCOL
    with type operation_data = unit
     and type operation = Mock_protocol.operation = struct
  include Mock_protocol

  (** Toy mempool with a random [add_operation] function.

      Unlike [Mock_protocol.Mempool], this mempool's type [t] is an
      actual state that keeps track of validated operations and can be
      retrieved with [operations]. This allows the test below to check
      that operations were correctly added or removed. *)
  module Mempool = struct
    include Mempool
    open Tezos_crypto

    type t = operation Operation_hash.Map.t

    type validation_info = unit

    let init _ctxt _chain_id ~head_hash:_ ~head:_ ~cache:_ =
      Lwt_result.return ((), Operation_hash.Map.empty)

    let operation_encoding =
      Data_encoding.conv
        (fun {shell; protocol_data = ()} -> shell)
        (fun shell -> {shell; protocol_data = ()})
        Operation.shell_header_encoding

    let encoding = Operation_hash.Map.encoding operation_encoding

    let add_operation ?check_signature:_ ?conflict_handler _info state (oph, op)
        =
      if Option.is_none conflict_handler then
        QCheck2.Test.fail_reportf
          "Prevalidation should always call [Proto.Mempool.add_operation] with \
           an explicit [conflict_handler]." ;
      match
        QCheck2.Gen.(
          generate1 (oneofl [`Added; `Replaced; `Unchanged; `Error; `Crash]))
      with
      | `Added ->
          let state = Operation_hash.Map.add oph op state in
          Lwt_result.return (state, Added)
      | `Replaced ->
          let removed =
            match Operation_hash.Map.choose state with
            | Some (hash, _) -> hash
            | None -> Tezos_crypto.Operation_hash.zero
          in
          let state = Operation_hash.Map.remove removed state in
          let state = Operation_hash.Map.add oph op state in
          Lwt_result.return (state, Replaced {removed})
      | `Unchanged -> Lwt_result.return (state, Unchanged)
      | `Error ->
          let err = error_of_fmt "Error during protocol validation." in
          Lwt_result.fail (Validation_error [err])
      | `Crash -> assert false

    let remove_operation state oph = Operation_hash.Map.remove oph state

    let merge ?conflict_handler:_ _ _ = assert false

    let operations = Fun.id
  end
end

module Filter_random_add_operation = MakeFilter (Proto_random_add_operation)

let filter_config = Filter_random_add_operation.Mempool.default_config

(** Test that [Prevalidation.apply_operations] makes field [applied]
    grow and that it grows only for operations on which the protocol
    [apply_operation] returns [Ok]. *)
let test_apply_operation_applied ctxt =
  let open Lwt_result_syntax in
  let open Tezos_crypto in
  let timestamp : Time.Protocol.t = now () in
  let ops = mk_ops () in
  let head = Init.genesis_block @@ Context_ops.hash ~time:timestamp ctxt in
  let (module Chain_store) = make_chain_store ctxt in
  let module P = MakePrevalidation (Chain_store) (Filter_random_add_operation)
  in
  let* pv = P.create chain_store ~head ~timestamp () in
  let apply_op pv op =
    let*! ( pv,
            (_filter_state : P.filter_state),
            (_op : Mock_protocol.operation Shell_operation.operation),
            classification,
            replacement ) =
      P.add_operation pv filter_state filter_config op
    in
    let valid_ops = P.Internal_for_tests.get_valid_operations pv in
    (match classification with
    | `Prechecked -> (
        assert (Operation_hash.Map.mem op.hash valid_ops) ;
        match replacement with
        | None -> ()
        | Some (removed, _) ->
            assert (not (Operation_hash.Map.mem removed valid_ops)))
    | `Branch_delayed _ ->
        assert (not (Operation_hash.Map.mem op.hash valid_ops)) ;
        assert (Option.is_none replacement)
    | `Branch_refused _ | `Refused _ | `Outdated _ | `Applied ->
        (* These cases cannot happen because the only possible error in
           [Proto_random_add_operation.Mempool.add_operation] has a
           [Branch_delayed] classification, protocol crashes are wrapped
           into a [Branch_delayed] error by [protect], and operation
           conflicts are also [Branch_delayed]. *)
        QCheck2.Test.fail_reportf "%s:@.Unexpected classification." __LOC__) ;
    Lwt.return pv
  in
  let*! _ = List.fold_left_s apply_op pv ops in
  return_unit

let () =
  Alcotest_lwt.run
    "mempool-prevalidation"
    [
      (* Run only those tests with:
         dune exec src/lib_shell/test/test_prevalidation_t.exe -- test create '0' *)
      ( "create",
        [
          Tztest.tztest
            "[create] returns Ok"
            `Quick
            (Init.wrap_tzresult_lwt_disk test_create);
        ] );
      (* Run only those tests with:
         dune exec src/lib_shell/test/test_prevalidation_t.exe -- test add_operation '0..1' *)
      ( "add_operation",
        [
          Tztest.tztest
            "Proto [add_operation] crash"
            `Quick
            (Init.wrap_tzresult_lwt_disk test_add_operation_crash);
          Tztest.tztest
            "[apply_operation] makes the [applied] field grow for [Applied] \
             operations (and only for them)"
            `Quick
            (Init.wrap_tzresult_lwt_disk test_apply_operation_applied);
        ] );
    ]
  |> Lwt_main.run
