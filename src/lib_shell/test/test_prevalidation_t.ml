(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
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
    Invocation:   dune exec src/lib_shell/test/test_prevalidation_t.exe
    Subject:      Unit tests for {!Prevalidation.T}
*)

module Mock_protocol :
  Tezos_protocol_environment.PROTOCOL
    with type operation_data = unit
     and type operation_receipt = unit
     and type validation_state = unit = struct
  include Environment_protocol_T_test.Internal_for_tests.Mock_all_unit

  let begin_construction ~chain_id:_ ~predecessor_context:_
      ~predecessor_timestamp:_ ~predecessor_level:_ ~predecessor_fitness:_
      ~predecessor:_ ~timestamp:_ ?protocol_data:_ ~cache:_ _ =
    (* We need to override this function (so that it's not [assert false]),
       because Prevalidation.create calls this function, so we need it
       to work in all tests below. *)
    return ()
end

module Internal_for_tests = Tezos_shell.Prevalidation.Internal_for_tests

module Init = struct
  let genesis_protocol =
    Protocol_hash.of_b58check_exn
      "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp"

  let chain_id = Chain_id.zero

  let genesis_time = Time.Protocol.of_seconds 0L

  (** [wrap_tzresult_lwt f ()] provides an instance of {!Context.t} to
      a test [f]. For this, it creates a temporary directory on disk,
      populates it with the data required for a {!Context.t} and then calls
      [f] by passing it an empty [Context.t]. After [f] finishes, the state
      is cleaned up. *)
  let wrap_tzresult_lwt (f : Context.t -> unit tzresult Lwt.t) () :
      unit tzresult Lwt.t =
    Lwt_utils_unix.with_tempdir "tezos_test_" (fun base_dir ->
        let root = Filename.concat base_dir "context" in
        Context.init root >>= fun idx ->
        Context.commit_genesis
          idx
          ~chain_id
          ~time:genesis_time
          ~protocol:genesis_protocol
        >>=? fun genesis -> Context.checkout_exn idx genesis >>= f)

  let genesis_block (context_hash : Context_hash.t) : Store.Block.t =
    let block_hash : Block_hash.t = Block_hash.hash_string ["genesis"] in
    let genesis : Genesis.t =
      {time = genesis_time; block = block_hash; protocol = genesis_protocol}
    in
    let repr : Block_repr.t =
      Block_repr.create_genesis_block ~genesis context_hash
    in
    Store.Unsafe.block_of_repr repr
end

let create_prevalidation
    (module Mock_protocol : Tezos_protocol_environment.PROTOCOL
      with type operation_data = unit
       and type operation_receipt = unit
       and type validation_state = unit) ctxt =
  let module Chain_store :
    Internal_for_tests.CHAIN_STORE with type chain_store = unit = struct
    type chain_store = unit

    let context () _block : Tezos_context.Context.t tzresult Lwt.t = return ctxt

    let chain_id () = Init.chain_id
  end in
  let module Prevalidation =
    Internal_for_tests.Make (Chain_store) (Mock_protocol)
  in
  (module Prevalidation : Tezos_shell.Prevalidation.T
    with type operation_receipt = unit
     and type validation_state = unit
     and type chain_store = Chain_store.chain_store)

let now () = Time.System.to_protocol (Tezos_stdlib_unix.Systime_os.now ())

(** The value of [chain_store] used in all tests below. *)
let chain_store = ()

(** Test that [create] returns [Ok] in a pristine context. *)
let test_create ctxt =
  let live_operations = Operation_hash.Set.empty in
  let timestamp : Time.Protocol.t = now () in
  let (module Prevalidation) =
    create_prevalidation (module Mock_protocol) ctxt
  in
  let predecessor : Store.Block.t =
    Init.genesis_block @@ Context.hash ~time:timestamp ctxt
  in
  Prevalidation.create chain_store ~predecessor ~live_operations ~timestamp ()
  >|=? ignore

(** A generator of [Prevalidation.operation] values that make sure
    to return distinct operations (hashes are not fake and they are
    all different). Returned maps are exactly of size [n]. *)
let prevalidation_operations_gen (type a)
    (module P : Prevalidation.T with type protocol_operation = a) ~(n : int) :
    a Prevalidation.operation list QCheck2.Gen.t =
  let mk_operation (hash, (raw : Operation.t)) :
      P.protocol_operation Prevalidation.operation =
    match P.parse hash raw with
    | Ok x -> x
    | Error err ->
        Format.printf "%a" Error_monad.pp_print_trace err ;
        assert false
  in
  let open QCheck2.Gen in
  (* We need to specify the protocol bytes generator to always generate the
     empty string, otherwise the call to [P.parse] will fail with the
     bytes being too long (hereby looking like an attack). *)
  let proto_gen : string QCheck2.Gen.t = QCheck2.Gen.return "" in
  let+ (ops : Operation.t Operation_hash.Map.t) =
    Generators.raw_op_map_gen_n ~proto_gen ?block_hash_t:None n
  in
  List.map mk_operation (Operation_hash.Map.bindings ops)

(** The number of operations used by tests that follow *)
let nb_ops = 100

let mk_ops (type a)
    (module P : Prevalidation.T with type protocol_operation = a) :
    a Prevalidation.operation list =
  let ops =
    QCheck2.Gen.generate1 (prevalidation_operations_gen (module P) ~n:nb_ops)
  in
  assert (Compare.List_length_with.(ops = nb_ops)) ;
  ops

(** Test that [Prevalidation.apply_operations] only returns [Branch_delayed _]
    when the protocol's [apply_operation] crashes. *)
let test_apply_operation_crash ctxt =
  let live_operations = Operation_hash.Set.empty in
  let timestamp : Time.Protocol.t = now () in
  let (module P) = create_prevalidation (module Mock_protocol) ctxt in
  let ops : P.protocol_operation Prevalidation.operation list =
    mk_ops (module P)
  in
  let predecessor : Store.Block.t =
    Init.genesis_block @@ Context.hash ~time:timestamp ctxt
  in
  P.create chain_store ~predecessor ~live_operations ~timestamp ()
  >>=? fun pv ->
  let apply_op pv op =
    P.apply_operation pv op >|= fun application_result ->
    match application_result with
    | Applied _ | Branch_refused _ | Refused _ | Outdated _ ->
        (* These cases should not happen because
           [Mock_protocol.apply_operation] is [assert false]. *)
        assert false
    | Branch_delayed _ ->
        (* This is the only allowed case. *)
        pv
  in
  Lwt_list.fold_left_s apply_op pv ops >>= fun _ -> return_unit

(** Logical implication *)
let ( ==> ) a b = (not a) || b

(** Returns a random generator initialized with a seed from [QCheck2] *)
let mk_rand () =
  (* We use QCheck2 as the source of randomness, as we hope one day
     this will become a traditional QCheck2 test. *)
  QCheck2.Gen.generate ~n:8 QCheck2.Gen.int
  |> Array.of_list |> Random.State.make

(** [mk_live_operations rand ops] returns a subset of [ops], which is
    appropriate for being passed as the [live_operations] argument
    of [Prevalidation.create] *)
let mk_live_operations (type a) rand (ops : a Prevalidation.operation list) =
  List.fold_left
    (fun acc (op : _ Prevalidation.operation) ->
      if Random.State.bool rand then
        Operation_hash.Set.add
          (Internal_for_tests.to_raw op |> Operation.hash)
          acc
      else acc)
    Operation_hash.Set.empty
    ops

(** Test that [Prevalidation.apply_operations] returns [Outdated]
    for operations in [live_operations] *)
let test_apply_operation_live_operations ctxt =
  let timestamp : Time.Protocol.t = now () in
  let rand : Random.State.t = mk_rand () in
  let (module Protocol : Tezos_protocol_environment.PROTOCOL
        with type operation_data = unit
         and type operation_receipt = unit
         and type validation_state = unit) =
    (module struct
      include Mock_protocol

      let apply_operation _ _ =
        Lwt.return
          (if Random.State.bool rand then Ok ((), ())
          else error_with "Operation doesn't apply")
    end)
  in
  let (module P) = create_prevalidation (module Protocol) ctxt in
  let ops : P.protocol_operation Prevalidation.operation list =
    mk_ops (module P)
  in
  let live_operations : Operation_hash.Set.t = mk_live_operations rand ops in
  let predecessor : Store.Block.t =
    Init.genesis_block @@ Context.hash ~time:timestamp ctxt
  in
  P.create chain_store ~predecessor ~live_operations ~timestamp ()
  >>=? fun pv ->
  let op_in_live_operations op =
    Operation_hash.Set.mem
      (Internal_for_tests.to_raw op |> Operation.hash)
      live_operations
  in
  let apply_op pv (op : _ Prevalidation.operation) =
    P.apply_operation pv op >|= fun application_result ->
    let (next_pv, result_is_outdated) =
      match application_result with
      | Applied (next_pv, _receipt) -> (next_pv, false)
      | Outdated _ -> (pv, true)
      | Branch_delayed _ | Branch_refused _ | Refused _ -> (pv, false)
    in
    (* Here is the main check of this test: *)
    assert (op_in_live_operations op ==> result_is_outdated) ;
    next_pv
  in
  Lwt_list.fold_left_s apply_op pv ops >>= fun _ -> return_unit

(** Test that [Prevalidation.apply_operations] makes field [applied]
    grow and that it grows only for operations on which the protocol
    [apply_operation] returns [Ok]. *)
let test_apply_operation_applied ctxt =
  let timestamp : Time.Protocol.t = now () in
  let rand : Random.State.t = mk_rand () in
  let (module Protocol : Tezos_protocol_environment.PROTOCOL
        with type operation_data = unit
         and type operation_receipt = unit
         and type validation_state = unit) =
    (module struct
      include Mock_protocol

      let apply_operation _ _ =
        Lwt.return
          (if Random.State.bool rand then Ok ((), ())
          else error_with "Operation doesn't apply")
    end)
  in
  let (module P) = create_prevalidation (module Protocol) ctxt in
  let ops : P.protocol_operation Prevalidation.operation list =
    mk_ops (module P)
  in
  let live_operations : Operation_hash.Set.t = mk_live_operations rand ops in
  let predecessor : Store.Block.t =
    Init.genesis_block @@ Context.hash ~time:timestamp ctxt
  in
  P.create chain_store ~predecessor ~live_operations ~timestamp ()
  >>=? fun pv ->
  let to_applied = P.Internal_for_tests.to_applied in
  let apply_op pv (op : _ Prevalidation.operation) =
    let applied_before = to_applied pv in
    P.apply_operation pv op >|= fun application_result ->
    let (next_pv, result_is_applied) =
      match application_result with
      | Applied (next_pv, _receipt) -> (next_pv, true)
      | Branch_delayed _ ->
          (* As in [test_apply_operation_crash] *)
          (pv, false)
      | Outdated _ ->
          (* This case can happen, because we specified a non-empty [live_operations] set *)
          (pv, false)
      | Branch_refused _ | Refused _ ->
          (* As in [test_apply_operation_crash], these cases cannot happen. *)
          assert false
    in
    let applied_after = to_applied next_pv in
    (* Here is the main check of this test: *)
    if result_is_applied then
      assert (Stdlib.List.tl applied_after = applied_before)
    else
      (* Physical equality: intended, the [applied] field should
         not be changed in this case. *)
      assert (applied_after == applied_before) ;
    next_pv
  in
  Lwt_list.fold_left_s apply_op pv ops >>= fun _ -> return_unit

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
            (Init.wrap_tzresult_lwt test_create);
        ] );
      (* Run only those tests with:
         dune exec src/lib_shell/test/test_prevalidation_t.exe -- test apply_operation '0..2' *)
      ( "apply_operation",
        [
          Tztest.tztest
            "[apply_operation] returns [Branch_delayed] when [apply_operation] \
             from the protocol crashes"
            `Quick
            (Init.wrap_tzresult_lwt test_apply_operation_crash);
          Tztest.tztest
            "[apply_operation] returns [Outdated] on operations in \
             [live_operations]"
            `Quick
            (Init.wrap_tzresult_lwt test_apply_operation_live_operations);
          Tztest.tztest
            "[apply_operation] makes the [applied] field grow for [Applied] \
             operations (and only for them)"
            `Quick
            (Init.wrap_tzresult_lwt test_apply_operation_applied);
        ] );
    ]
  |> Lwt_main.run
