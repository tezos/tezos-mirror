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

type error +=
  | Zk_rollup_does_not_exist of Zk_rollup_repr.t
  | Zk_rollup_invalid_op_code of int
  | Zk_rollup_pending_list_too_short
  | Zk_rollup_negative_length

let () =
  register_error_kind
    `Temporary
    ~id:"zk_rollup_does_not_exist"
    ~title:"ZK Rollup does not exist"
    ~description:"Attempted to use a ZK rollup that has not been originated."
    ~pp:(fun ppf x ->
      Format.fprintf ppf "Rollup %a does not exist" Zk_rollup_repr.Address.pp x)
    Data_encoding.(obj1 (req "rollup" Zk_rollup_repr.Address.encoding))
    (function Zk_rollup_does_not_exist x -> Some x | _ -> None)
    (fun x -> Zk_rollup_does_not_exist x) ;
  register_error_kind
    `Permanent
    ~id:"zk_rollup_invalid_op code"
    ~title:"Invalid op code in append"
    ~description:"Invalid op code in append"
    ~pp:(fun ppf oc ->
      Format.fprintf ppf "Op code %d is not valid for this ZK Rollup" oc)
    Data_encoding.(obj1 (req "op_code" int31))
    (function Zk_rollup_invalid_op_code oc -> Some oc | _ -> None)
    (fun oc -> Zk_rollup_invalid_op_code oc) ;
  register_error_kind
    `Temporary
    ~id:"zk_rollup_pending_list_too_short"
    ~title:"Pending list is too short"
    ~description:"Pending list is too short"
    Data_encoding.unit
    (function Zk_rollup_pending_list_too_short -> Some () | _ -> None)
    (fun () -> Zk_rollup_pending_list_too_short) ;
  register_error_kind
    `Permanent
    ~id:"zk_rollup_negative_length"
    ~title:"Negative length for pending list prefix"
    ~description:"Negative length for pending list prefix"
    Data_encoding.unit
    (function Zk_rollup_negative_length -> Some () | _ -> None)
    (fun () -> Zk_rollup_negative_length)

let account = Storage.Zk_rollup.Account.get

let pending_list = Storage.Zk_rollup.Pending_list.get

let pending_op ctxt id = Storage.Zk_rollup.Pending_operation.get (ctxt, id)

let originate ctxt static ~init_state =
  let open Lwt_result_syntax in
  let*? ctxt, nonce = Raw_context.increment_origination_nonce ctxt in
  let*? address = Zk_rollup_repr.Address.from_nonce nonce in
  let origination_size = Constants_storage.zk_rollup_origination_size ctxt in
  let initial_account =
    Zk_rollup_account_repr.
      {
        static;
        dynamic =
          {
            state = init_state;
            paid_l2_operations_storage_space = Z.of_int origination_size;
            used_l2_operations_storage_space = Z.zero;
          };
      }
  in
  let* ctxt, account_size =
    Storage.Zk_rollup.Account.init ctxt address initial_account
  in
  let init_pl = Zk_rollup_repr.(Empty {next_index = 0L}) in
  let* ctxt, pl_size =
    Storage.Zk_rollup.Pending_list.init ctxt address init_pl
  in
  let address_size = Zk_rollup_repr.Address.size in
  let size =
    Z.of_int (origination_size + address_size + account_size + pl_size)
  in
  return (ctxt, address, size)

let add_to_pending ctxt rollup ops =
  let open Lwt_result_syntax in
  let open Zk_rollup_repr in
  let open Zk_rollup_operation_repr in
  let* ctxt, acc = account ctxt rollup in
  let*? () =
    List.iter_e
      (fun (op, _ticket_hash_opt) ->
        if Compare.Int.(op.op_code >= acc.static.nb_ops || op.op_code < 0) then
          error @@ Zk_rollup_invalid_op_code op.op_code
        else ok ())
      ops
  in
  let* ctxt, pl = Storage.Zk_rollup.Pending_list.get ctxt rollup in
  let next_index, length =
    match pl with
    | Empty {next_index} -> (next_index, 0)
    | Pending {next_index; length} -> (next_index, length)
  in
  let* ctxt, next_index, length, storage_diff =
    List.fold_left_es
      (fun (ctxt, next_index, length, storage_diff) op ->
        let* ctxt, new_storage_diff, _was_bound =
          Storage.Zk_rollup.Pending_operation.add (ctxt, rollup) next_index op
        in
        return
          ( ctxt,
            Int64.succ next_index,
            length + 1,
            new_storage_diff + storage_diff ))
      (ctxt, next_index, length, 0)
      ops
  in
  let used_l2_operations_storage_space =
    Z.(add acc.dynamic.used_l2_operations_storage_space (Z.of_int storage_diff))
  in
  let l2_operations_storage_space_to_pay =
    Z.(
      max
        zero
        (sub
           used_l2_operations_storage_space
           acc.dynamic.paid_l2_operations_storage_space))
  in
  let paid_l2_operations_storage_space =
    Z.(
      add
        acc.dynamic.paid_l2_operations_storage_space
        l2_operations_storage_space_to_pay)
  in
  let acc =
    {
      acc with
      dynamic =
        {
          acc.dynamic with
          paid_l2_operations_storage_space;
          used_l2_operations_storage_space;
        };
    }
  in

  let pl =
    if Compare.Int.(length = 0) then Empty {next_index}
    else Pending {next_index; length}
  in
  (* Users aren't charged for storage diff in the account or pending list
     description of a ZKRU.
     When updating a ZKRU account, the storage diff can only come from the
     dynamically sized [Z.t] used for the watermark. These changes
     in storage size will not be accounted for.
     As for the pending list description, the storage size is fixed for
     each of the two cases (empty / non-empty). Then, there will be a storage
     diff when switching between these two, which won't be accounted for
     either.
  *)
  let* ctxt, _diff_acc = Storage.Zk_rollup.Account.update ctxt rollup acc in
  let* ctxt, _diff_pl = Storage.Zk_rollup.Pending_list.update ctxt rollup pl in
  return (ctxt, l2_operations_storage_space_to_pay)

let pending_length =
  let open Zk_rollup_repr in
  function Empty _ -> 0 | Pending {length; _} -> length

let head =
  let open Zk_rollup_repr in
  function
  | Empty _ -> error Zk_rollup_pending_list_too_short
  | Pending {next_index; length} ->
      Result_syntax.return Int64.(sub next_index (of_int length))

let next_index =
  let open Zk_rollup_repr in
  function
  | Empty {next_index} -> next_index | Pending {next_index; _} -> next_index

let get_pending_length ctxt rollup =
  let open Lwt_result_syntax in
  let* ctxt, pl = pending_list ctxt rollup in
  return (ctxt, pending_length pl)

(** Same as [Tezos_stdlib.Utils.fold_n_times] but with Lwt and Error monad *)
let fold_n_times_es ~when_negative n f e =
  let open Lwt_result_syntax in
  if Compare.Int.(n < 0) then tzfail when_negative
  else
    let rec go acc = function
      | 0 -> return acc
      | n ->
          let* acc = f acc in
          (go [@ocaml.tailcall]) acc (n - 1)
    in
    go e n

let get_prefix ctxt rollup n =
  let open Lwt_result_syntax in
  if Compare.Int.(n = 0) then return (ctxt, [])
  else
    let* ctxt, pl = pending_list ctxt rollup in
    let pl_length = pending_length pl in
    let*? () =
      error_when Compare.Int.(n > pl_length) Zk_rollup_pending_list_too_short
    in
    let*? hd = head pl in
    let* ctxt, ops, _i =
      (* Get the l2 ops corresponding to indeces [hd + n - 1 .. hd],
         so that the accumulated list is in the right order *)
      fold_n_times_es
        ~when_negative:Zk_rollup_negative_length
        n
        (fun (ctxt, ops, i) ->
          let* ctxt, op = pending_op ctxt rollup i in
          return (ctxt, op :: ops, Int64.pred i))
        (ctxt, [], Int64.(sub (add hd (of_int n)) 1L))
    in
    return (ctxt, ops)

let update ctxt rollup ~pending_to_drop ~new_account =
  let open Lwt_result_syntax in
  let open Zk_rollup_repr in
  let open Zk_rollup_account_repr in
  let* ctxt, pl = pending_list ctxt rollup in
  let* ctxt, acc = account ctxt rollup in
  let pl_length = pending_length pl in
  let*? () =
    error_when
      Compare.Int.(pending_to_drop > pl_length)
      Zk_rollup_pending_list_too_short
  in
  let next_index = next_index pl in
  (* Drop the indeces from [head] to [head + pending_to_drop - 1]
     from the storage of L2 operations. *)
  let* ctxt, freed =
    match head pl with
    | Error _e ->
        (* If the pending list is empty, then [pending_to_drop] must be 0. *)
        return (ctxt, 0)
    | Ok head ->
        let* ctxt, freed, _i =
          fold_n_times_es
            ~when_negative:Zk_rollup_negative_length
            pending_to_drop
            (fun (ctxt, freed, i) ->
              let* ctxt, new_freed, _bound =
                Storage.Zk_rollup.Pending_operation.remove (ctxt, rollup) i
              in
              return (ctxt, freed + new_freed, Int64.succ i))
            (ctxt, 0, head)
        in
        return (ctxt, freed)
  in
  (* Subtract the bytes freed by removing pending operations from
     acc.dynamic.used_l2_operations_storage_space, and update
     [new_account].
  *)
  let used_l2_operations_storage_space =
    Z.(sub acc.dynamic.used_l2_operations_storage_space (Z.of_int freed))
  in
  let new_account =
    {
      new_account with
      dynamic =
        {
          state = new_account.dynamic.state;
          paid_l2_operations_storage_space =
            new_account.dynamic.paid_l2_operations_storage_space;
          used_l2_operations_storage_space;
        };
    }
  in
  let* ctxt, _diff_acc =
    Storage.Zk_rollup.Account.update ctxt rollup new_account
  in
  (* Update the pending list descriptor *)
  let pl_length = pl_length - pending_to_drop in
  let pl =
    if Compare.Int.(pl_length = 0) then Empty {next_index}
    else Pending {next_index; length = pl_length}
  in
  let* ctxt, _diff_pl = Storage.Zk_rollup.Pending_list.update ctxt rollup pl in
  return ctxt

let assert_exist ctxt rollup =
  let open Lwt_result_syntax in
  let* ctxt, exists = Storage.Zk_rollup.Account.mem ctxt rollup in
  let*? () = error_unless exists (Zk_rollup_does_not_exist rollup) in
  return ctxt

let exists ctxt rollup = Storage.Zk_rollup.Account.mem ctxt rollup
