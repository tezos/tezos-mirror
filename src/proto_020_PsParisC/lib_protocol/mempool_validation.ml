(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Alpha_context
open Validate

type t = {
  predecessor_hash : Block_hash.t;
  operation_state : operation_conflict_state;
  operations : packed_operation Operation_hash.Map.t;
}

type validation_info = info

type add_result = Added | Replaced of {removed : Operation_hash.t} | Unchanged

type operation_conflict = Validate_errors.operation_conflict =
  | Operation_conflict of {
      existing : Operation_hash.t;
      new_operation : Operation_hash.t;
    }

type add_error =
  | Validation_error of error trace
  | Add_conflict of operation_conflict

type merge_error = Incompatible_mempool | Merge_conflict of operation_conflict

let encoding : t Data_encoding.t =
  let open Data_encoding in
  def "mempool"
  @@ conv
       (fun {predecessor_hash; operation_state; operations} ->
         (predecessor_hash, operation_state, operations))
       (fun (predecessor_hash, operation_state, operations) ->
         {predecessor_hash; operation_state; operations})
  @@ obj3
       (req "predecessor_hash" Block_hash.encoding)
       (req "operation_state" operation_conflict_state_encoding)
       (req
          "operations"
          (Operation_hash.Map.encoding
             (dynamic_size ~kind:`Uint30 Operation.encoding)))

let init ctxt chain_id ~predecessor_level ~predecessor_round ~predecessor_hash :
    validation_info * t =
  let {info; operation_state; _} =
    begin_partial_construction
      ctxt
      chain_id
      ~predecessor_level
      ~predecessor_round
  in
  ( info,
    {predecessor_hash; operation_state; operations = Operation_hash.Map.empty}
  )

type conflict_handler =
  existing_operation:Operation_hash.t * packed_operation ->
  new_operation:Operation_hash.t * packed_operation ->
  [`Keep | `Replace]

let remove_operation mempool oph =
  match Operation_hash.Map.find_opt oph mempool.operations with
  | None -> mempool
  | Some {shell; protocol_data = Operation_data protocol_data} ->
      let operations = Operation_hash.Map.remove oph mempool.operations in
      let operation_state =
        remove_operation mempool.operation_state {shell; protocol_data}
      in
      {mempool with operations; operation_state}

let add_operation ?(check_signature = true)
    ?(conflict_handler : conflict_handler option) info mempool
    (oph, (packed_op : packed_operation)) :
    (t * add_result, add_error) result Lwt.t =
  let open Lwt_syntax in
  let {shell; protocol_data = Operation_data protocol_data} = packed_op in
  let operation : _ Alpha_context.operation = {shell; protocol_data} in
  let* validate_result = check_operation ~check_signature info operation in
  match validate_result with
  | Error err -> Lwt.return_error (Validation_error err)
  | Ok () -> (
      match check_operation_conflict mempool.operation_state oph operation with
      | Ok () ->
          let operation_state =
            add_valid_operation mempool.operation_state oph operation
          in
          let operations =
            Operation_hash.Map.add oph packed_op mempool.operations
          in
          let result = Added in
          Lwt.return_ok ({mempool with operation_state; operations}, result)
      | Error
          (Validate_errors.Operation_conflict
             {existing; new_operation = new_oph} as x) -> (
          match conflict_handler with
          | Some handler -> (
              let new_operation = (new_oph, packed_op) in
              let existing_operation =
                match
                  Operation_hash.Map.find_opt existing mempool.operations
                with
                | None -> assert false
                | Some op -> (existing, op)
              in
              match handler ~existing_operation ~new_operation with
              | `Keep -> Lwt.return_ok (mempool, Unchanged)
              | `Replace ->
                  let mempool = remove_operation mempool existing in
                  let operation_state =
                    add_valid_operation
                      mempool.operation_state
                      new_oph
                      operation
                  in
                  let operations =
                    Operation_hash.Map.add oph packed_op mempool.operations
                  in
                  Lwt.return_ok
                    ( {mempool with operations; operation_state},
                      Replaced {removed = existing} ))
          | None -> Lwt.return_error (Add_conflict x)))

let merge ?conflict_handler existing_mempool new_mempool =
  let open Result_syntax in
  if
    Block_hash.(
      existing_mempool.predecessor_hash <> new_mempool.predecessor_hash)
  then Error Incompatible_mempool
  else
    let unique_new_operations =
      (* only retain unique operations that are in new_mempool *)
      Operation_hash.Map.(
        merge
          (fun _ l r ->
            match (l, r) with
            | None, Some r -> Some r
            | Some _, None -> None
            | Some _, Some _ -> None
            | None, None -> None)
          existing_mempool.operations
          new_mempool.operations)
    in
    let unopt_assert = function None -> assert false | Some o -> o in
    let handle_conflict new_operation_content conflict =
      match (conflict, conflict_handler) with
      | Ok (), _ -> Ok `Add_new
      | Error conflict, None -> Error (Merge_conflict conflict)
      | ( Error (Operation_conflict {existing; new_operation}),
          Some (f : conflict_handler) ) -> (
          (* New operations can only conflict with operations
             already present in the existing mempool. *)
          let existing_operation_content =
            Operation_hash.Map.find_opt existing existing_mempool.operations
            |> unopt_assert
          in
          match
            f
              ~existing_operation:(existing, existing_operation_content)
              ~new_operation:(new_operation, new_operation_content)
          with
          | `Keep -> Ok `Do_nothing
          | `Replace -> Ok (`Replace existing))
    in
    Operation_hash.Map.fold_e
      (fun roph packed_right_op mempool_acc ->
        let {shell; protocol_data = Operation_data protocol_data} =
          packed_right_op
        in
        let right_op = ({shell; protocol_data} : _ operation) in
        let* conflict =
          check_operation_conflict mempool_acc.operation_state roph right_op
          |> handle_conflict packed_right_op
        in
        match conflict with
        | `Do_nothing -> return mempool_acc
        | `Add_new ->
            let operation_state =
              add_valid_operation mempool_acc.operation_state roph right_op
            in
            let operations =
              Operation_hash.Map.add roph packed_right_op mempool_acc.operations
            in
            return {mempool_acc with operation_state; operations}
        | `Replace loph ->
            let mempool_acc = remove_operation mempool_acc loph in
            let operation_state =
              add_valid_operation mempool_acc.operation_state roph right_op
            in
            let operations =
              Operation_hash.Map.add roph packed_right_op mempool_acc.operations
            in
            return {mempool_acc with operation_state; operations})
      unique_new_operations
      existing_mempool

let operations mempool = mempool.operations
