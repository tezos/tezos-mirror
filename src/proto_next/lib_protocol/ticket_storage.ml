(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Trili Tech, <contact@trili.tech>                       *)
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
  | Negative_ticket_balance of {key : Ticket_hash_repr.t; balance : Z.t}
  | Used_storage_space_underflow

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"Negative_ticket_balance"
    ~title:"Negative ticket balance"
    ~description:"Attempted to set a negative ticket balance value"
    ~pp:(fun ppf (key, balance) ->
      Format.fprintf
        ppf
        "Attempted to set negative ticket balance value '%a' for key %a."
        Z.pp_print
        balance
        Ticket_hash_repr.pp
        key)
    (obj2 (req "key" Ticket_hash_repr.encoding) (req "balance" Data_encoding.z))
    (function
      | Negative_ticket_balance {key; balance} -> Some (key, balance)
      | _ -> None)
    (fun (key, balance) -> Negative_ticket_balance {key; balance}) ;
  register_error_kind
    `Permanent
    ~id:"Used_storage_underflow"
    ~title:"Ticket balance used storage underflow"
    ~description:
      "Attempt to free more bytes than allocated for the tickets balance"
    empty
    (function Used_storage_space_underflow -> Some () | _ -> None)
    (fun () -> Used_storage_space_underflow)

let get_balance ctxt key =
  let open Lwt_result_syntax in
  let+ ctxt, res = Storage.Ticket_balance.Table.find ctxt key in
  (res, ctxt)

let set_balance ctxt key balance =
  let cost_of_key = Z.of_int 65 in
  let open Lwt_result_syntax in
  let* () =
    fail_when
      Compare.Z.(balance < Z.zero)
      (Negative_ticket_balance {key; balance})
  in
  if Compare.Z.(balance = Z.zero) then
    let+ ctxt, freed, existed = Storage.Ticket_balance.Table.remove ctxt key in
    (* If we remove an existing entry, then we return the freed size for
       both the key and the value. *)
    let freed =
      if existed then Z.neg @@ Z.add cost_of_key (Z.of_int freed) else Z.zero
    in
    (freed, ctxt)
  else
    let+ ctxt, size_diff, existed =
      Storage.Ticket_balance.Table.add ctxt key balance
    in
    let size_diff =
      let z_diff = Z.of_int size_diff in
      (* For a new entry we also charge the space for storing the key *)
      if existed then z_diff else Z.add cost_of_key z_diff
    in
    (size_diff, ctxt)

let adjust_balance ctxt key ~delta =
  let open Lwt_result_syntax in
  let* res, ctxt = get_balance ctxt key in
  let old_balance = Option.value ~default:Z.zero res in
  set_balance ctxt key (Z.add old_balance delta)

let adjust_storage_space ctxt ~storage_diff =
  let open Lwt_result_syntax in
  if Compare.Z.(storage_diff = Z.zero) then return (Z.zero, ctxt)
  else
    let* used_storage = Storage.Ticket_balance.Used_storage_space.find ctxt in
    let used_storage = Option.value ~default:Z.zero used_storage in
    let* paid_storage = Storage.Ticket_balance.Paid_storage_space.find ctxt in
    let paid_storage = Option.value ~default:Z.zero paid_storage in
    let new_used_storage = Z.add used_storage storage_diff in
    let*? () =
      error_when
        Compare.Z.(new_used_storage < Z.zero)
        Used_storage_space_underflow
    in
    let*! ctxt =
      Storage.Ticket_balance.Used_storage_space.add ctxt new_used_storage
    in
    let diff = Z.sub new_used_storage paid_storage in
    if Compare.Z.(Z.zero < diff) then
      let*! ctxt =
        Storage.Ticket_balance.Paid_storage_space.add ctxt new_used_storage
      in
      return (diff, ctxt)
    else return (Z.zero, ctxt)

module Internal_for_tests = struct
  let used_storage_space c =
    let open Lwt_result_syntax in
    let+ value = Storage.Ticket_balance.Used_storage_space.find c in
    Option.value ~default:Z.zero value

  let paid_storage_space c =
    let open Lwt_result_syntax in
    let+ value = Storage.Ticket_balance.Paid_storage_space.find c in
    Option.value ~default:Z.zero value
end
