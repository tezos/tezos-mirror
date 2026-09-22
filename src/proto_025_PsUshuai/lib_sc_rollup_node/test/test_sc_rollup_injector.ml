(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:  Smart rollup node library, injection of refutation moves
    Invocation: dune exec src/proto_025_PsUshuai/lib_sc_rollup_node/test/main.exe \
                  -- -f test_sc_rollup_injector.ml
    Subject:    A refutation move carrying an inbox proof is injected together
                with the operation publishing the index proof of its level.
*)

open Octez_smart_rollup

let refute ?(index_proof = []) refutation =
  L1_operation.Refute
    {
      rollup = Address.zero;
      opponent = Signature.Public_key_hash.zero;
      refutation;
      index_proof;
    }

let proof_move proof = Game.Move {choice = Z.zero; step = Game.Proof proof}

let dissection_move = Game.Move {choice = Z.zero; step = Game.Dissection []}

(* A proof as the protocol serializes them, so that the conversion to a manager
   operation can read it back. *)
let a_proof =
  let hex =
    WithExceptions.Option.get ~loc:__LOC__ (List.hd Serialized_proofs.proofs)
  in
  WithExceptions.Option.get ~loc:__LOC__ (Hex.to_string (`Hex hex))

let index_proof_messages = ["first chunk"; "second chunk"]

let kinds operation =
  List.map
    (fun (Protocol.Alpha_context.Manager operation) ->
      match operation with
      | Protocol.Alpha_context.Sc_rollup_add_messages _ -> "add_messages"
      | Sc_rollup_refute _ -> "refute"
      | _ -> "other")
    (Sc_rollup_injector.manager_operations_of_injector_operation operation)

let check_kinds ~__LOC__ expected operation =
  Check.(
    (kinds operation = expected)
      (list string)
      ~__LOC__
      ~error_msg:"expected the manager operations %R, got %L")

let () =
  Test.register
    ~__FILE__
    ~title:"Sc_rollup_injector: an index proof is batched with its move"
    ~tags:["sc_rollup"; "injector"; "refutation"; "index_proof"]
  @@ fun () ->
  (* The operation proper comes last, which is the index the injector reports
     the whole thing at. *)
  check_kinds
    ~__LOC__
    ["add_messages"; "refute"]
    (refute ~index_proof:index_proof_messages (proof_move a_proof)) ;
  check_kinds ~__LOC__ ["refute"] (refute (proof_move a_proof)) ;
  check_kinds ~__LOC__ ["refute"] (refute dissection_move) ;
  check_kinds
    ~__LOC__
    ["add_messages"]
    (L1_operation.Add_messages {messages = ["a message"]}) ;
  let size (Protocol.Alpha_context.Manager operation) =
    Data_encoding.Binary.length
      Protocol.Alpha_context.Operation.contents_encoding
      (Contents
         (Manager_operation
            {
              source = Signature.Public_key_hash.zero;
              operation;
              fee = Protocol.Alpha_context.Tez.zero;
              counter =
                Protocol.Alpha_context.Manager_counter.Internal_for_tests.of_int
                  0;
              gas_limit = Protocol.Alpha_context.Gas.Arith.zero;
              storage_limit = Z.zero;
            }))
  in
  (* The move Layer 1 sees carries the bare proof, and the messages are the
     ones the index proof was cut into. *)
  (match
     ( Sc_rollup_injector.manager_operations_of_injector_operation
         (refute ~index_proof:index_proof_messages (proof_move a_proof)),
       Sc_rollup_injector.manager_operations_of_injector_operation
         (refute (proof_move a_proof)) )
   with
  | ( [
        (Protocol.Alpha_context.Manager (Sc_rollup_add_messages {messages}) as
         add_messages);
        (Manager (Sc_rollup_refute _) as batched);
      ],
      [bare] ) ->
      Check.(
        (messages = index_proof_messages)
          (list string)
          ~__LOC__
          ~error_msg:"expected the messages %R, got %L") ;
      Check.(
        (size batched = size bare)
          int
          ~__LOC__
          ~error_msg:"the batched move should carry the bare proof (%L vs %R)") ;
      Check.(
        (size add_messages > 0)
          int
          ~__LOC__
          ~error_msg:"the messages should weigh something (%L)")
  | _ -> Test.fail "expected an add_messages followed by a refute") ;
  unit

let status index_in_batch status = {Injector_sigs.index_in_batch; status}

let failed = Injector_sigs.Unsuccessful (Failed [])

let () =
  Test.register
    ~__FILE__
    ~title:"Sc_rollup_injector: an expanded move reports one status"
    ~tags:["sc_rollup"; "injector"; "refutation"; "index_proof"]
  @@ fun () ->
  let check ~__LOC__ ~arities statuses expected =
    let found = Sc_rollup_injector.collapse_statuses ~arities statuses in
    let show =
      List.map (fun {Injector_sigs.index_in_batch; status} ->
          Format.asprintf
            "%d:%s"
            index_in_batch
            (match status with
            | Injector_sigs.Successful -> "successful"
            | Unsuccessful (Failed _) -> "failed"
            | Unsuccessful Backtracked -> "backtracked"
            | Unsuccessful Skipped -> "skipped"
            | Unsuccessful Other_branch -> "other_branch"
            | Unsuccessful Never_included -> "never_included"))
    in
    Check.(
      (show found = show expected)
        (list string)
        ~__LOC__
        ~error_msg:"expected the statuses %R, got %L")
  in
  let successful i = status i Injector_sigs.Successful in
  (* Operations that took one manager operation each are reported unchanged. *)
  check
    ~__LOC__
    ~arities:[1; 1]
    [successful 0; successful 1]
    [successful 0; successful 1] ;
  (* A reveal operation shifts every index, and the expanded operation is
     reported at the index of the move, not of its messages. *)
  check ~__LOC__ ~arities:[2] [successful 1; successful 2] [successful 2] ;
  (* One failing manager operation fails the whole move, whichever it is. *)
  check ~__LOC__ ~arities:[2] [status 0 failed; successful 1] [status 1 failed] ;
  check ~__LOC__ ~arities:[2] [successful 0; status 1 failed] [status 1 failed] ;
  check
    ~__LOC__
    ~arities:[2]
    [status 0 failed; status 1 (Unsuccessful Skipped)]
    [status 1 failed] ;
  (* A move that succeeded but was backtracked keeps that status. *)
  check
    ~__LOC__
    ~arities:[2]
    [successful 0; status 1 (Unsuccessful Backtracked)]
    [status 1 (Unsuccessful Backtracked)] ;
  unit
