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

let () =
  register_error_kind
    `Temporary
    ~id:"Zk_rollup_does_not_exist"
    ~title:"ZK Rollup does not exist"
    ~description:"Attempted to use a ZK rollup that has not been originated."
    ~pp:(fun ppf x ->
      Format.fprintf ppf "Rollup %a does not exist" Zk_rollup_repr.Address.pp x)
    Data_encoding.(obj1 (req "rollup" Zk_rollup_repr.Address.encoding))
    (function Zk_rollup_does_not_exist x -> Some x | _ -> None)
    (fun x -> Zk_rollup_does_not_exist x) ;
  register_error_kind
    `Permanent
    ~id:"Zk_rollup_invalid_op code"
    ~title:"Invalid op code in append"
    ~description:"Invalid op code in append"
    ~pp:(fun ppf oc ->
      Format.fprintf ppf "Op code %d is not valid for this ZK Rollup" oc)
    Data_encoding.(obj1 (req "op_code" int31))
    (function Zk_rollup_invalid_op_code oc -> Some oc | _ -> None)
    (fun oc -> Zk_rollup_invalid_op_code oc)

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
  let* ctxt, acc = Storage.Zk_rollup.Account.get ctxt rollup in
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

let assert_exist ctxt rollup =
  let open Lwt_result_syntax in
  let* ctxt, exists = Storage.Zk_rollup.Account.mem ctxt rollup in
  let*? () = error_unless exists (Zk_rollup_does_not_exist rollup) in
  return ctxt

let exists ctxt rollup = Storage.Zk_rollup.Account.mem ctxt rollup
