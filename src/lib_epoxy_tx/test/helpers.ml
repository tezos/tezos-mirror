(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
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

open Epoxy_tx.Tx_rollup
open Plompiler
open Types.P
open Constants.Bound

module V (L : LIB) = struct
  open V (L)

  let make_pos i = Bounded.make ~bound:max_nb_leaves (Z.of_int i)

  let get_rollup_id (r : P.generate_op_result) =
    match r.tx with
    | Transfer t -> t.header.rollup_id
    | Create t -> t.header.rollup_id
    | Credit t -> t.header.rollup_id
    | Debit t -> t.header.rollup_id

  let circuit_op (r : P.generate_op_result) init_state state () =
    predicate_op
      ~old_root:(MerklePV.P.root init_state.accounts_tree)
      ~old_next_pos:(make_pos init_state.next_position)
      ~new_root:(MerklePV.P.root state.accounts_tree)
      ~new_next_pos:(make_pos state.next_position)
      ~fee:r.fee
      ~exit_validity:r.exit_validity
      ~rollup_id:(get_rollup_id r)
      r.tx
      r.tx_s

  let pos_of_index ?(offset = 0) index =
    Z.of_int @@ ((Epoxy_tx.Constants.max_nb_tickets * index) + offset)

  let inner_batch (rs : P.generate_op_result list) init_state state =
    let get_op : tx -> transfer = function
      | Transfer op -> op
      | _ -> assert false
    in
    let get_op_s : tx_storage -> transfer_storage = function
      | Transfer op -> op
      | _ -> assert false
    in
    predicate_private_batch
      ~old_root:(MerklePV.P.root init_state.accounts_tree)
      ~old_next_pos:(make_pos init_state.next_position)
      ~new_root:(MerklePV.P.root state.accounts_tree)
      ~new_next_pos:(make_pos state.next_position)
      ~fees:
        (List.fold_left
           (fun acc (r : P.generate_op_result) ->
             Bounded.add_left ~unsafe:true acc r.fee)
           (Bounded.make ~bound:max_amount Z.zero)
           rs)
      ~rollup_id:(get_rollup_id (List.hd rs))
      (List.map (fun (r : P.generate_op_result) -> get_op r.tx) rs)
      (List.map (fun (r : P.generate_op_result) -> get_op_s r.tx_s) rs)
end
