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

module Constants = Constants
module Types = Types
module Utils = Utils
open Utils
open Plompiler
module HashPV = Anemoi128
module MerklePV = Gadget.Merkle (HashPV)
module SchnorrPV = Plompiler.Schnorr (HashPV)
module Hash = HashPV.P
module Merkle = MerklePV.P
module Schnorr = SchnorrPV.P
module Curve = Mec.Curve.Jubjub.AffineEdwards

module P = struct
  open Types.P
  open Constants
  (* These functions aimed to format integers more efficiently by compressing
     them in a scalar. *)

  (* This function computes the maximal bound of a list of bounded variables
     written as [(value, bound)_1, ..., (value, bound)_n] *)
  let compression_bound (values : unit Bounded.t list) =
    let values = (values :> (Z.t * Z.t) list) in
    List.fold_left Z.mul Z.one (List.map snd values)

  (* This function attempts to compress a list of integers into one scalar. *)
  let compress (values : unit Bounded.t list) =
    assert (compression_bound values < S.order) ;
    let values = (values :> (Z.t * Z.t) list) in
    List.fold_left
      (fun acc (v, v_bound) -> Z.(v + (acc * v_bound)))
      (fst @@ List.hd values)
      (List.tl values)

  let scalar_of_account (acc : account) =
    (* We can use just the u coordinate of pk as Edwards curves are symmetric and as such there are only two possible v coordinates and the same sk is used to generate both.
       We could set a convention to only use pk with v coordination of a given parity, for instance v odd. *)
    (* TODO move this in schnorr and make the pk directly a single coordinate *)
    let u = Curve.get_u_coordinate acc.pk |> of_bls_scalar in
    let compressed = compress Bounded.[f acc.tez_balance; f acc.cnt] in
    let h = Hash.direct ~input_length:2 [|u; S.of_z compressed|] in
    (* we leverage the Correlation-Intractablity (with respecto to +) of the
       hash function and add [h] to the ticket's root (which is also the output
       of a hash); alternatively (and to avoid relying on CI) we could define
       [scalar_of_account] as the hash of all 3 inputs [u, compressed,
       acc.tickets_root ], but that would require one extra iteration of the
       hash function *)
    S.add h acc.tickets_root

  let scalar_of_leaf (l : leaf) =
    let compressed = compress Bounded.[f l.pos; f l.ticket.amount] in
    Hash.direct ~input_length:2 [|l.ticket.id; S.of_z compressed|]

  let default_leaf pos =
    {
      pos = Bounded.make ~bound:Bound.max_nb_leaves (Z.of_int pos);
      ticket = Dummy.ticket_balance;
    }

  let empty_ticket_tree start_pos =
    let size = max_nb_tickets in
    let leaves = Array.init size (fun i -> default_leaf (i + start_pos)) in
    ( leaves,
      Merkle.generate_tree
        ~leaves:(Array.map scalar_of_leaf leaves)
        tickets_depth )

  let default_account acc_index =
    let start_pos = max_nb_tickets * acc_index in
    let leaves, ticket_tree = empty_ticket_tree start_pos in
    let tickets_root = Merkle.root ticket_tree in
    ( {
        pk = Curve.one;
        tez_balance = Bounded.make ~bound:Bound.max_balance Z.zero;
        cnt = Bounded.make ~bound:Bound.max_counter Z.zero;
        tickets_root;
      },
      leaves,
      ticket_tree )

  let get_account :
      int ->
      (account * leaf array * Merkle.tree) IMap.t ->
      account * leaf array * Merkle.tree =
   fun i accs ->
    IMap.find_opt i accs |> Option.value ~default:(default_account i)

  let random_leaf pos =
    let id = S.random () in
    let amount = Bounded.random Bound.max_balance in
    {pos; ticket = {id; amount}}

  let random_ticket_tree start_pos =
    let size = max_nb_tickets in
    let leaves =
      Array.init size (fun i ->
          random_leaf
            (Bounded.make ~bound:Bound.max_nb_leaves
            @@ Z.of_int @@ (i + start_pos)))
    in
    ( Merkle.generate_tree
        ~leaves:(Array.map scalar_of_leaf leaves)
        tickets_depth,
      leaves )

  let random_account sks i =
    let open Bound in
    let tez_balance = Bounded.make Z.(v max_balance / two) ~bound:max_balance in
    let pk = Schnorr.neuterize sks.(i) in
    let cnt = Bounded.random max_counter in
    let start_pos = max_nb_tickets * i in
    let ticket_tree, leaves = random_ticket_tree start_pos in
    let tickets_root = Merkle.root ticket_tree in
    ({tez_balance; pk; cnt; tickets_root}, leaves, ticket_tree)

  let random_state sks () =
    (* We don't generate empty states *)
    let size = 1 + random_int (max_nb_accounts - 1) in
    let next_index = random_int max_nb_accounts in
    let next_position = max_nb_tickets * next_index in
    let indices = List.init size (fun _ -> random_int max_nb_accounts) in
    let indices = List.filter (fun i -> next_index <> i) indices in
    let accs_list = List.map (random_account sks) indices in
    let indexed_accounts = List.combine indices accs_list in
    let accounts = IMap.of_seq (List.to_seq indexed_accounts) in
    let account_scalars =
      Array.init max_nb_accounts (fun i ->
          scalar_of_account
            (let x, _, _ = default_account i in
             x))
    in
    let () =
      List.iter
        (fun (i, (acc, _, _)) -> account_scalars.(i) <- scalar_of_account acc)
        indexed_accounts
    in
    let accounts_tree =
      Merkle.generate_tree ~leaves:account_scalars accounts_depth
    in
    {accounts; accounts_tree; next_position}

  let empty_state () =
    let accounts = Array.init max_nb_accounts (fun i -> default_account i) in
    let accounts_tree =
      Merkle.generate_tree
        ~leaves:(Array.map (fun (a, _, _) -> scalar_of_account a) accounts)
        accounts_depth
    in
    let accounts = Array.mapi (fun i x -> (i, x)) accounts in
    let accounts = IMap.of_seq @@ Array.to_seq accounts in
    {accounts; accounts_tree; next_position = 0}

  let make_state (bals : (Schnorr.pk * Z.t * balance ticket array) list) =
    let open Bound in
    let s = empty_state () in
    let _, accounts, accounts_tree =
      List.fold_left
        (fun (i, accounts, accounts_tree) (pk, tez_bal, tickets) ->
          let acc, leaves, _tree = get_account i accounts in
          let leaves =
            Array.mapi
              (fun i {pos; ticket} ->
                let ticket =
                  try tickets.(i) with Invalid_argument _ -> ticket
                in
                {pos; ticket})
              leaves
          in
          let tree =
            Merkle.generate_tree
              ~leaves:(Array.map scalar_of_leaf leaves)
              tickets_depth
          in
          let acc =
            {
              acc with
              tez_balance = Bounded.make ~bound:max_balance tez_bal;
              pk;
              tickets_root = Merkle.root tree;
            }
          in
          let accounts = IMap.add i (acc, leaves, tree) accounts in
          let accounts_tree =
            Merkle.update_tree
              ~input_length:2
              accounts_tree
              i
              (scalar_of_account acc)
          in
          (i + 1, accounts, accounts_tree))
        (0, s.accounts, s.accounts_tree)
        bals
    in
    {accounts; accounts_tree; next_position = List.length bals * max_nb_tickets}

  let coerce (type a) (x : a Bounded.t) = fst (x : a Bounded.t :> Z.t * Z.t)

  let hash_op (t : unsigned_tx) =
    let module Curve = Mec.Curve.Jubjub.AffineEdwards in
    let module S = Bls12_381.Fr in
    match t with
    (* Do not use wildcard patterns, make sure we never forget to sign a field *)
    | Transfer {header; payload = {src; dst; fee; amount; cnt}} ->
        let compressed_msg =
          compress
            Bounded.
              [
                f header.op_code;
                f header.price.amount;
                f src;
                f dst;
                f fee;
                f amount.amount;
                f cnt;
              ]
        in
        Hash.direct
          ~input_length:4
          [|
            scalar_of_bytes header.l1_dst;
            S.of_z compressed_msg;
            header.price.id;
            amount.id;
          |]
    | Create {header; payload = {pk; fee}} ->
        let compressed_msg =
          compress Bounded.[f header.op_code; f header.price.amount; f fee]
        in
        let pk_x, pk_y = affine_to_point pk in
        Hash.direct
          ~input_length:5
          [|
            scalar_of_bytes header.l1_dst;
            S.of_z compressed_msg;
            pk_x;
            pk_y;
            header.price.id;
          |]
    | Credit {header; payload = {dst; amount; cnt}} ->
        let compressed_msg =
          compress
            Bounded.
              [
                f header.op_code;
                f header.price.amount;
                f dst;
                f amount.amount;
                f cnt;
              ]
        in
        Hash.direct
          ~input_length:4
          [|
            scalar_of_bytes header.l1_dst;
            S.of_z compressed_msg;
            header.price.id;
            amount.id;
          |]
    | Debit {header; payload = {src; amount; cnt}} ->
        let compressed_msg =
          compress
            Bounded.
              [
                f header.op_code;
                f header.price.amount;
                f src;
                f amount.amount;
                f cnt;
              ]
        in
        Hash.direct
          ~input_length:4
          [|
            scalar_of_bytes header.l1_dst;
            S.of_z compressed_msg;
            header.price.id;
            amount.id;
          |]

  let sign_op sk (t : unsigned_tx) : tx =
    let module Curve = Mec.Curve.Jubjub.AffineEdwards in
    let module S = Bls12_381.Fr in
    let msg = hash_op t in
    match t with
    (* Do not use wildcard patterns, make sure we never forget to sign a field *)
    | Transfer {header; payload = {src; dst; fee; amount; cnt}} ->
        let signature =
          let random = Curve.Scalar.random () in
          Schnorr.sign ~compressed:true sk msg random
        in
        Transfer
          {header; payload = {msg = {src; dst; fee; amount; cnt}; signature}}
    | Create {header; payload = {pk; fee}} ->
        let signature =
          let random = Curve.Scalar.random () in
          Schnorr.sign ~compressed:true sk msg random
        in
        Create {header; payload = {msg = {pk; fee}; signature}}
    | Debit {header; payload = {src; amount; cnt}} ->
        let signature =
          let random = Curve.Scalar.random () in
          Schnorr.sign ~compressed:true sk msg random
        in
        Debit {header; payload = {msg = {src; amount; cnt}; signature}}
    | Credit t -> Credit t

  (* Check if an operation is valid in a certain state and, if possible,
     return the storage needed to make the proof.
     The only case where the storage isn't computed is for ill-formed ops,
     as the ill-formed circuits do not need any storage.
  *)
  let preprocess_operation :
      state -> tx -> tezos_zkru -> state * tx * tx_storage option =
   fun s tx rollup_id ->
    match tx with
    | Transfer
        ({header; payload = {msg = {cnt; src; dst; amount; fee}; signature}} as
        op) ->
        let msg =
          hash_op @@ Transfer {header; payload = {cnt; src; dst; amount; fee}}
        in
        let well_formed =
          Bounded.(
            check cnt && check src && check dst && check amount.amount
            && check fee)
        in
        if not well_formed then (s, Transfer op, None)
        else
          let src_index = Z.to_int (coerce src) / Constants.max_nb_tickets in
          let src_offset = Z.to_int (coerce src) mod Constants.max_nb_tickets in
          let dst_index = Z.to_int (coerce dst) / Constants.max_nb_tickets in
          let dst_offset = Z.to_int (coerce dst) mod Constants.max_nb_tickets in
          let src_account, src_leaves, src_tree =
            get_account src_index s.accounts
          in
          let src_leaf = src_leaves.(src_offset) in

          let is_tez = amount.id = Constants.tez_id in

          let ticket_amount =
            if is_tez then Bounded.make ~bound:Constants.Bound.max_amount Z.zero
            else amount.amount
          in
          let tez_transfer_amount =
            if is_tez then amount.amount
            else Bounded.make ~bound:Constants.Bound.max_amount Z.zero
          in
          let tez_amount =
            Bounded.add_left ~unsafe:true tez_transfer_amount fee
          in

          let new_tez_amount_src =
            Bounded.(sub_left ~unsafe:true src_account.tez_balance tez_amount)
          in
          let new_ticket_amount_src =
            Bounded.(sub_left ~unsafe:true src_leaf.ticket.amount ticket_amount)
          in

          let dst_account, dst_leaves, _dst_tree =
            (* When src and dst are the same, we need to apply the effects of
               the transfer from src to the state before checking the validity
               of the dst *)
            if dst_index = src_index then (
              let new_ticket_src =
                {src_leaf.ticket with amount = new_ticket_amount_src}
              in
              let new_leaf_src = {src_leaf with ticket = new_ticket_src} in
              src_leaves.(src_offset) <- new_leaf_src ;
              let src_tree =
                Merkle.update_tree
                  ~input_length:2
                  src_tree
                  src_offset
                  (scalar_of_leaf new_leaf_src)
              in
              let new_cnt_src = Bounded.succ ~unsafe:true src_account.cnt in
              let new_acc_src =
                {
                  src_account with
                  tez_balance = new_tez_amount_src;
                  cnt = new_cnt_src;
                  tickets_root = Merkle.root src_tree;
                }
              in
              (new_acc_src, src_leaves, src_tree))
            else get_account dst_index s.accounts
          in
          let dst_leaf = dst_leaves.(dst_offset) in
          let new_tez_amount_dst =
            Bounded.(
              add_left ~unsafe:true dst_account.tez_balance tez_transfer_amount)
          in
          let new_ticket_amount_dst =
            Bounded.(add_left ~unsafe:true dst_leaf.ticket.amount ticket_amount)
          in

          let check_counter = src_account.cnt < cnt in
          let check_signature =
            Schnorr.verify
              ~compressed:true
              ~msg
              ~pk:src_account.pk
              ~signature
              ()
          in
          let check_balances =
            Z.(coerce new_tez_amount_src >= zero)
            && Z.(coerce new_ticket_amount_src >= zero)
            && Bounded.check new_tez_amount_dst
            && Bounded.check new_ticket_amount_dst
          in
          (* TODO: add check for rollup id *)
          let check_rollup_id = rollup_id = header.rollup_id in
          let check_ticket_ids =
            let check_src = is_tez || amount.id = src_leaf.ticket.id in
            let check_dst =
              is_tez
              || Z.(Bounded.v @@ dst_leaf.ticket.amount = zero)
              || amount.id = dst_leaf.ticket.id
            in
            check_src && check_dst
          in
          let check_dst_pk = dst_account.pk <> Curve.one in
          let check_price = Z.(zero = Bounded.v header.price.amount) in

          let valid =
            check_counter && check_signature && check_balances && check_dst_pk
            && check_ticket_ids && check_price && check_rollup_id
          in

          let src_proof, s =
            let _, path = Merkle.proof_path src_index s.accounts_tree in
            let s =
              if valid then (
                let new_ticket_src =
                  {src_leaf.ticket with amount = new_ticket_amount_src}
                in
                let new_leaf_src = {src_leaf with ticket = new_ticket_src} in
                src_leaves.(src_offset) <- new_leaf_src ;
                let src_tree =
                  Merkle.update_tree
                    ~input_length:2
                    src_tree
                    src_offset
                    (scalar_of_leaf new_leaf_src)
                in
                let new_cnt_src = Bounded.succ ~unsafe:true src_account.cnt in
                let new_acc_src =
                  {
                    src_account with
                    tez_balance = new_tez_amount_src;
                    cnt = new_cnt_src;
                    tickets_root = Merkle.root src_tree;
                  }
                in
                let accounts =
                  IMap.add
                    src_index
                    (new_acc_src, src_leaves, src_tree)
                    s.accounts
                in
                let accounts_tree =
                  Merkle.update_tree
                    ~input_length:2
                    s.accounts_tree
                    src_index
                    (scalar_of_account new_acc_src)
                in
                {s with accounts_tree; accounts})
              else s
            in
            let root = Merkle.root s.accounts_tree in
            ({path; root}, s)
          in
          let acc_after_src, leaves_after_src, tree_after_src =
            get_account src_index s.accounts
          in
          let leaf_after_src = leaves_after_src.(src_offset) in
          let _, src_leaf_path = Merkle.proof_path src_offset tree_after_src in
          let src =
            {
              account =
                {before = src_account; after = acc_after_src; proof = src_proof};
              leaf =
                {
                  before = src_leaf;
                  after = leaf_after_src;
                  path = src_leaf_path;
                };
            }
          in
          (* We retrieve bl_dst after updating the src_leaf as dst may be equal to src *)
          let dst_account, dst_leaves, dst_tree =
            get_account dst_index s.accounts
          in
          let dst_proof, s =
            let _, path = Merkle.proof_path dst_index s.accounts_tree in
            let s =
              if valid then (
                let new_ticket_dst =
                  {
                    id = (if is_tez then dst_leaf.ticket.id else amount.id);
                    amount = new_ticket_amount_dst;
                  }
                in
                let new_leaf_dst = {dst_leaf with ticket = new_ticket_dst} in
                dst_leaves.(dst_offset) <- new_leaf_dst ;
                let dst_tree =
                  Merkle.update_tree
                    ~input_length:2
                    dst_tree
                    dst_offset
                    (scalar_of_leaf new_leaf_dst)
                in
                let new_acc_dst =
                  {
                    dst_account with
                    tez_balance = new_tez_amount_dst;
                    tickets_root = Merkle.root dst_tree;
                  }
                in
                let accounts =
                  IMap.add
                    dst_index
                    (new_acc_dst, dst_leaves, dst_tree)
                    s.accounts
                in
                let accounts_tree =
                  Merkle.update_tree
                    ~input_length:2
                    s.accounts_tree
                    dst_index
                    (scalar_of_account new_acc_dst)
                in
                {s with accounts_tree; accounts})
              else s
            in
            let root = Merkle.root s.accounts_tree in
            ({path; root}, s)
          in

          let acc_after_dst, leaves_after_dst, tree_after_dst =
            get_account dst_index s.accounts
          in
          let leaf_after_dst = leaves_after_dst.(dst_offset) in
          let _, dst_leaf_path = Merkle.proof_path dst_offset tree_after_dst in
          let dst =
            {
              account =
                {before = dst_account; after = acc_after_dst; proof = dst_proof};
              leaf =
                {
                  before = dst_leaf;
                  after = leaf_after_dst;
                  path = dst_leaf_path;
                };
            }
          in
          (s, Transfer op, Some (Transfer {src; dst; valid}))
    | Create ({header; payload = {msg = {pk; fee}; signature}} as op) ->
        let msg = hash_op @@ Create {header; payload = {pk; fee}} in
        let well_formed = Bounded.(check fee) in
        if not well_formed then (s, Create op, None)
        else
          let dst_pos = s.next_position in
          let dst_index = dst_pos / Constants.max_nb_tickets in
          let dst_offset = dst_pos mod Constants.max_nb_tickets in
          assert (dst_offset = 0) ;
          let next_empty_pos = dst_pos + Constants.max_nb_tickets in
          let next_empty_index = dst_index + 1 in
          let next_empty_offset = 0 in
          let s = {s with next_position = next_empty_pos} in
          let dst_account, dst_leaves, dst_tree =
            get_account dst_index s.accounts
          in
          let next_empty_account, next_empty_leaves, next_empty_tree =
            get_account (dst_index + 1) s.accounts
          in
          let check_next_is_empty = next_empty_account.pk = Curve.one in
          let check_rollup_id = rollup_id = header.rollup_id in
          let check_signature =
            Schnorr.verify ~compressed:true ~msg ~pk ~signature ()
          in
          let check_price =
            Bounded.v header.price.amount = Bounded.v fee
            && header.price.id = Constants.tez_id
          in
          let valid =
            check_signature && check_next_is_empty && check_price
            && check_rollup_id
          in

          let next_empty_proof, s =
            let _, path = Merkle.proof_path next_empty_index s.accounts_tree in
            let root = Merkle.root s.accounts_tree in
            ({path; root}, s)
          in
          let dst_proof, s =
            let _, path = Merkle.proof_path dst_index s.accounts_tree in
            let s =
              if valid then
                let new_acc_dst = {dst_account with pk} in
                let accounts =
                  IMap.add
                    dst_index
                    (new_acc_dst, dst_leaves, dst_tree)
                    s.accounts
                in
                let accounts_tree =
                  Merkle.update_tree
                    ~input_length:2
                    s.accounts_tree
                    dst_index
                    (scalar_of_account new_acc_dst)
                in
                {s with accounts_tree; accounts}
              else s
            in
            let root = Merkle.root s.accounts_tree in
            ({path; root}, s)
          in
          let acc_after_dst, _, _ = get_account dst_index s.accounts in
          let dst_leaf = dst_leaves.(dst_offset) in
          let _, dst_leaf_path = Merkle.proof_path dst_offset dst_tree in
          let dst =
            {
              account =
                {before = dst_account; after = acc_after_dst; proof = dst_proof};
              leaf = {before = dst_leaf; after = dst_leaf; path = dst_leaf_path};
            }
          in
          let next_empty_leaf = next_empty_leaves.(next_empty_offset) in
          let _, ne_leaf_path =
            Merkle.proof_path next_empty_offset next_empty_tree
          in
          let next_empty =
            {
              account =
                {
                  before = next_empty_account;
                  after = next_empty_account;
                  proof = next_empty_proof;
                };
              leaf =
                {
                  before = next_empty_leaf;
                  after = next_empty_leaf;
                  path = ne_leaf_path;
                };
            }
          in
          (s, Create op, Some (Create {dst; next_empty; valid}))
    | Credit ({header; payload = {cnt; dst; amount}} as op) ->
        let well_formed =
          Bounded.(check cnt && check dst && check amount.amount)
        in
        if not well_formed then (s, Credit op, None)
        else
          let dst_index = Z.to_int (coerce dst) / Constants.max_nb_tickets in
          let dst_offset = Z.to_int (coerce dst) mod Constants.max_nb_tickets in

          let is_tez = amount.id = Constants.tez_id in
          let ticket_amount =
            if is_tez then Bounded.make ~bound:Constants.Bound.max_amount Z.zero
            else amount.amount
          in
          let tez_amount =
            if is_tez then amount.amount
            else Bounded.make ~bound:Constants.Bound.max_amount Z.zero
          in

          let dst_account, dst_leaves, dst_tree =
            get_account dst_index s.accounts
          in
          let dst_leaf = dst_leaves.(dst_offset) in

          let new_tez_amount_dst =
            Bounded.(add_left ~unsafe:true dst_account.tez_balance tez_amount)
          in
          let new_ticket_amount_dst =
            Bounded.(add_left ~unsafe:true dst_leaf.ticket.amount ticket_amount)
          in

          let check_counter = dst_account.cnt < cnt in
          let check_balances =
            Bounded.check new_tez_amount_dst
            && Bounded.check new_ticket_amount_dst
          in
          let check_rollup_id = rollup_id = header.rollup_id in
          let check_ticket_ids =
            is_tez
            || Z.(Bounded.v @@ dst_leaf.ticket.amount = zero)
            || amount.id = dst_leaf.ticket.id
          in
          let check_price =
            Bounded.v header.price.amount = Bounded.v amount.amount
            && header.price.id = amount.id
          in
          let check_dst_pk = dst_account.pk <> Curve.one in

          let valid =
            check_counter && check_dst_pk && check_balances && check_ticket_ids
            && check_price && check_rollup_id
          in

          let dst_proof, s =
            let _, path = Merkle.proof_path dst_index s.accounts_tree in
            let s =
              if valid then (
                let new_ticket_dst =
                  {id = amount.id; amount = new_ticket_amount_dst}
                in
                let new_leaf_dst = {dst_leaf with ticket = new_ticket_dst} in
                dst_leaves.(dst_offset) <- new_leaf_dst ;
                let dst_tree =
                  Merkle.update_tree
                    ~input_length:2
                    dst_tree
                    dst_offset
                    (scalar_of_leaf new_leaf_dst)
                in
                let new_acc_dst =
                  {
                    dst_account with
                    tez_balance = new_tez_amount_dst;
                    cnt;
                    tickets_root = Merkle.root dst_tree;
                  }
                in
                let accounts =
                  IMap.add
                    dst_index
                    (new_acc_dst, dst_leaves, dst_tree)
                    s.accounts
                in
                let accounts_tree =
                  Merkle.update_tree
                    ~input_length:2
                    s.accounts_tree
                    dst_index
                    (scalar_of_account new_acc_dst)
                in
                {s with accounts_tree; accounts})
              else s
            in
            let root = Merkle.root s.accounts_tree in
            ({path; root}, s)
          in

          let acc_after_dst, leaves_after_dst, tree_after_dst =
            get_account dst_index s.accounts
          in
          let leaf_after_dst = leaves_after_dst.(dst_offset) in
          let _, dst_leaf_path = Merkle.proof_path dst_offset tree_after_dst in
          let dst =
            {
              account =
                {before = dst_account; after = acc_after_dst; proof = dst_proof};
              leaf =
                {
                  before = dst_leaf;
                  after = leaf_after_dst;
                  path = dst_leaf_path;
                };
            }
          in
          (s, Credit op, Some (Credit {dst; valid}))
    | Debit ({header; payload = {msg = {cnt; src; amount}; signature}} as op) ->
        let msg = hash_op @@ Debit {header; payload = {cnt; src; amount}} in
        let well_formed =
          Bounded.(check cnt && check src && check amount.amount)
        in
        if not well_formed then (s, Debit op, None)
        else
          let src_index = Z.to_int (coerce src) / Constants.max_nb_tickets in
          let src_offset = Z.to_int (coerce src) mod Constants.max_nb_tickets in

          let src_account, src_leaves, src_tree =
            get_account src_index s.accounts
          in
          let src_leaf = src_leaves.(src_offset) in

          let is_tez = amount.id = Constants.tez_id in

          let ticket_amount =
            if is_tez then Bounded.make ~bound:Constants.Bound.max_amount Z.zero
            else amount.amount
          in
          let tez_amount =
            if is_tez then amount.amount
            else Bounded.make ~bound:Constants.Bound.max_amount Z.zero
          in

          let new_tez_amount_src =
            Bounded.(sub_left ~unsafe:true src_account.tez_balance tez_amount)
          in
          let new_ticket_amount_src =
            Bounded.(sub_left ~unsafe:true src_leaf.ticket.amount ticket_amount)
          in

          let check_counter = src_account.cnt < cnt in
          let check_signature =
            Schnorr.verify
              ~compressed:true
              ~msg
              ~pk:src_account.pk
              ~signature
              ()
          in
          let check_balances =
            Z.(coerce new_tez_amount_src >= zero)
            && Z.(coerce new_ticket_amount_src >= zero)
          in
          let check_rollup_id = rollup_id = header.rollup_id in
          let check_ticket_ids = is_tez || amount.id = src_leaf.ticket.id in
          let check_price =
            Bounded.v header.price.amount = Bounded.v amount.amount
            && header.price.id = amount.id
          in
          let valid =
            check_counter && check_signature && check_balances
            && check_ticket_ids && check_price && check_rollup_id
          in

          let src_proof, s =
            let _, path = Merkle.proof_path src_index s.accounts_tree in
            let s =
              if valid then (
                let new_ticket_src =
                  {src_leaf.ticket with amount = new_ticket_amount_src}
                in
                let new_leaf_src = {src_leaf with ticket = new_ticket_src} in
                src_leaves.(src_offset) <- new_leaf_src ;
                let src_tree =
                  Merkle.update_tree
                    ~input_length:2
                    src_tree
                    src_offset
                    (scalar_of_leaf new_leaf_src)
                in
                let new_cnt_src = Bounded.succ ~unsafe:true src_account.cnt in
                let new_acc_src =
                  {
                    src_account with
                    tez_balance = new_tez_amount_src;
                    cnt = new_cnt_src;
                    tickets_root = Merkle.root src_tree;
                  }
                in
                let accounts =
                  IMap.add
                    src_index
                    (new_acc_src, src_leaves, src_tree)
                    s.accounts
                in
                let accounts_tree =
                  Merkle.update_tree
                    ~input_length:2
                    s.accounts_tree
                    src_index
                    (scalar_of_account new_acc_src)
                in
                {s with accounts_tree; accounts})
              else s
            in
            let root = Merkle.root s.accounts_tree in
            ({path; root}, s)
          in
          let acc_after_src, leaves_after_src, tree_after_src =
            get_account src_index s.accounts
          in
          let leaf_after_src = leaves_after_src.(src_offset) in
          let _, src_leaf_path = Merkle.proof_path src_offset tree_after_src in
          let src =
            {
              account =
                {before = src_account; after = acc_after_src; proof = src_proof};
              leaf =
                {
                  before = src_leaf;
                  after = leaf_after_src;
                  path = src_leaf_path;
                };
            }
          in
          (s, Debit op, Some (Debit {src; valid}))

  (* Get validity from an optional tx storage, as computed by preprocess_op *)
  let get_validity tx_s : bool =
    match tx_s with
    | Some (Transfer t_s) -> t_s.valid
    | Some (Create t_s) -> t_s.valid
    | Some (Credit t_s) -> t_s.valid
    | Some (Debit t_s) -> t_s.valid
    | None -> false

  (* Get the actual fee of an op *)
  let tx_fee (op : tx) op_s =
    let z = Bounded.make ~bound:Constants.Bound.max_fee Z.zero in
    match (op, get_validity op_s) with
    | Transfer tx, true -> tx.payload.msg.fee
    | Create tx, true -> tx.payload.msg.fee
    | _ -> z

  let preprocess_private_batch (s : state) ops rollup_id =
    let s, ops, ops_s, fees =
      List.fold_left
        (fun (s, ops, ops_s, acc_fee) op ->
          let s, tx, tx_s = preprocess_operation s op rollup_id in
          let fee = tx_fee tx tx_s in
          let op = match tx with Transfer op -> op | _ -> assert false in
          let op_s =
            match tx_s with Some (Transfer op_s) -> op_s | _ -> assert false
          in
          ( s,
            op :: ops,
            op_s :: ops_s,
            Bounded.add_left ~unsafe:true acc_fee fee ))
        (s, [], [], Bounded.make ~bound:Constants.Bound.max_amount Z.zero)
        ops
    in
    let ops, ops_s = (List.rev ops, List.rev ops_s) in
    (s, ops, ops_s, fees)

  type generate_op_result = {
    tx : tx;
    tx_s : tx_storage;
    fee : fee Bounded.t;
    hash : S.t;
    exit_validity : bool;
  }

  let generate_transaction :
      ?src_pos:Z.t ->
      ?dst_pos:Z.t ->
      ?amount:amount ticket ->
      ?fee:Z.t ->
      ?cnt:Z.t ->
      ?valid:bool ->
      ?unsafe:bool ->
      sks:Schnorr.sk array ->
      state ->
      generate_op_result * state =
   fun ?src_pos
       ?dst_pos
       ?amount
       ?fee
       ?cnt
       ?(valid = true)
       ?(unsafe = false)
       ~sks
       s ->
    let open Bound in
    let unpack_optional ~bound ?opts ?(maxv = Bound.v bound) arg =
      match (arg, opts) with
      | Some v, _ -> Bounded.make ~unsafe ~bound v
      | None, Some opts ->
          let len_opts = List.length opts in
          assert (len_opts > 0) ;
          let i = random_int len_opts in
          Bounded.make ~unsafe ~bound @@ List.nth opts i
      | _ -> Bounded.random ~maxv bound
    in
    let open_positions =
      List.of_seq
      (* TODO: use other offsets *)
      @@ Seq.map (fun (i, _) -> Z.of_int @@ (max_nb_tickets * i))
      @@ IMap.to_seq s.accounts
    in
    let src_pos =
      unpack_optional ~bound:max_nb_leaves ~opts:open_positions src_pos
    in
    let src_pos_i = Z.to_int @@ Bounded.v src_pos in
    let src_index = src_pos_i / max_nb_tickets in
    let src_offset = src_pos_i mod max_nb_tickets in
    let dst_pos =
      unpack_optional ~bound:max_nb_leaves ~opts:open_positions dst_pos
    in
    let dst_pos_i = Z.to_int @@ Bounded.v dst_pos in

    let dst_index = dst_pos_i / max_nb_tickets in
    let dst_offset = dst_pos_i mod max_nb_tickets in
    let sk_src = sks.(src_index) in
    let src_acc, src_leaves, _src_tree = get_account src_index s.accounts in
    let dst_acc, dst_leaves, _dst_tree = get_account dst_index s.accounts in

    let src_leaf = src_leaves.(src_offset) in
    let dst_leaf = dst_leaves.(dst_offset) in
    let cnt =
      Option.(
        value
          ~default:src_acc.cnt
          (map (Bounded.make ~unsafe ~bound:max_counter) cnt))
    in
    let cnt = Bounded.succ cnt in
    let fee =
      unpack_optional
        ~bound:max_fee
        ~maxv:Z.(min (Bounded.v src_acc.tez_balance) (Bound.v max_fee))
        fee
    in
    let amount_id =
      Option.(value ~default:tez_id (map (fun {id; _} -> id) amount))
    in
    let is_tez = amount_id = tez_id in
    let max_src_amount =
      if is_tez then
        Z.(min Bounded.(v src_acc.tez_balance - v fee) (Bound.v max_amount))
      else Z.(min Bounded.(v src_leaf.ticket.amount) (Bound.v max_amount))
    in
    let max_dst_amount =
      let bal =
        if is_tez then dst_acc.tez_balance else dst_leaf.ticket.amount
      in
      Z.(Bound.v max_balance - Bounded.v bal)
    in
    let amount_amount =
      unpack_optional
        ~bound:max_amount
        ~maxv:Z.(min max_src_amount max_dst_amount)
        (Option.map (fun {amount; id = _id} -> Bounded.v amount) amount)
    in
    let amount = {id = amount_id; amount = amount_amount} in
    let header = Dummy.header in
    let unsigned_payload = {cnt; src = src_pos; dst = dst_pos; amount; fee} in
    let op = sign_op sk_src (Transfer {header; payload = unsigned_payload}) in
    let msg = hash_op (Transfer {header; payload = unsigned_payload}) in
    let s, tx, tx_s = preprocess_operation s op header.rollup_id in
    let tx_s = Option.get tx_s in
    ( {
        tx;
        tx_s;
        fee =
          (if valid then fee else Bounded.make ~unsafe ~bound:max_fee Z.zero);
        hash = msg;
        exit_validity = false;
      },
      s )

  let generate_transactions :
      ?src_pos:Z.t ->
      ?dst_pos:Z.t ->
      ?amount:amount ticket ->
      ?fee:Z.t ->
      ?cnt:Z.t ->
      ?valid:bool ->
      ?unsafe:bool ->
      nb_batches:int ->
      batch_size:int ->
      sks:Schnorr.sk array ->
      state ->
      (generate_op_result list * state) list =
   fun ?src_pos
       ?dst_pos
       ?amount
       ?fee
       ?cnt
       ?(valid = true)
       ?(unsafe = false)
       ~nb_batches
       ~batch_size
       ~sks
       state ->
    let make_batch state =
      let batch, state =
        List.fold_left
          (fun (txs, state) _ ->
            let tx, state =
              generate_transaction
                ?src_pos
                ?dst_pos
                ?amount
                ?fee
                ?cnt
                ~valid
                ~unsafe
                ~sks
                state
            in
            (tx :: txs, state))
          ([], state)
          (List.init batch_size Fun.id)
      in
      (List.rev batch, state)
    in
    let batches, _ =
      List.fold_left
        (fun (batches, state) _ ->
          let batch, state = make_batch state in
          ((batch, state) :: batches, state))
        ([], state)
        (List.init nb_batches Fun.id)
    in
    List.rev batches
end

module V (L : LIB) = struct
  module Hash = HashPV.V (L)
  module Plompiler_Curve = JubjubEdwards (L)
  module Schnorr = SchnorrPV.V (L)
  module Merkle = MerklePV.V (L)
  open L
  module T = Types.V (L)
  open T
  module Encodings = Types.Encodings (L)

  let compression_bound (values : unit Bounded_u.t list) =
    let values = (values :> (scalar repr * Z.t) list) in
    List.fold_left Z.mul Z.one (List.map snd values)

  let monadic_compress (values : unit Bounded_u.t list) =
    assert (compression_bound values < S.order) ;
    let values = (values :> (scalar repr * Z.t) list) in
    foldM
      (fun acc (v, v_bound) -> Num.add ~ql:(S.of_z v_bound) acc v)
      (fst @@ List.hd values)
      (List.tl values)

  let assert_merkle_proof x path root =
    let* b = Merkle.merkle_proof path x root in
    Bool.assert_true b

  let hash_leaf (l : leaf_u) =
    let* compressed = monadic_compress Bounded_u.[f l.pos; f l.ticket.amount] in
    Hash.digest ~input_length:2 (to_list [l.ticket.id; compressed])

  let hash_account (acc : account_u) =
    let pk_x, _pk_y = of_pair acc.pk in
    let* compressed =
      monadic_compress Bounded_u.[f acc.tez_balance; f acc.cnt]
    in
    let* h = Hash.digest ~input_length:2 (to_list [pk_x; compressed]) in
    (* we leverage the Correlation-Intractablity (with respecto to +) of the
       hash function and add [h] to the ticket's root (which is also the output
       of a hash); alternatively (and to avoid relying on CI) we could define
       [scalar_of_account] as the hash of all 3 inputs [pk_x, compressed,
       acc.tickets_root ], but that would require one extra iteration of the
       hash function *)
    Num.add h acc.tickets_root

  let assert_tree_proofs (acc : account_u) (leaf : leaf_u) path_acc path_leaf
      root =
    let* scalar_acc = hash_account acc in
    assert_merkle_proof scalar_acc path_acc root
    >* let* scalar_leaf = hash_leaf leaf in
       assert_merkle_proof scalar_leaf path_leaf acc.tickets_root

  let coerce (type a) (x : a Bounded_u.t) =
    fst (x : a Bounded_u.t :> scalar repr * Z.t)

  let check_eq_account (a : account_u) (b : account_u) =
    with_bool_check (equal a.pk b.pk)
    >* with_bool_check (equal (coerce a.tez_balance) (coerce b.tez_balance))
    >* with_bool_check (equal (coerce a.cnt) (coerce b.cnt))
    >* with_bool_check (equal a.tickets_root b.tickets_root)

  let check_eq_leaf (a : leaf_u) (b : leaf_u) =
    with_bool_check (equal (coerce a.pos) (coerce b.pos))
    >* with_bool_check (equal a.ticket.id b.ticket.id)
    >* with_bool_check (equal (coerce a.ticket.amount) (coerce b.ticket.amount))

  let predicate_fees ~old_root ~old_next_pos ~new_root ~new_next_pos ~fees
      operator =
    let safety = Encodings.Bounded_e.Unsafe in

    let* old_root = input ~kind:`Public @@ Input.scalar old_root in
    let* old_next_pos =
      input ~kind:`Public @@ Encodings.(pos_encoding ~safety).input old_next_pos
    in
    let* new_root = input ~kind:`Public @@ Input.scalar new_root in
    let* new_next_pos =
      input ~kind:`Public @@ Encodings.(pos_encoding ~safety).input new_next_pos
    in
    let* fees =
      input ~kind:`Public @@ Encodings.((amount_encoding ~safety).input) fees
    in
    let* operator =
      input @@ Encodings.account_tree_el_encoding.input operator
    in
    let* generator =
      Plompiler_Curve.(input_point @@ affine_to_point Curve.one)
    in
    let fees = Encodings.((amount_encoding ~safety).decode) fees in
    let operator = Encodings.account_tree_el_encoding.decode operator in
    assert_equal old_next_pos new_next_pos
    >* let* before_s = hash_account operator.before in
       let* after_s = hash_account operator.after in
       assert_merkle_proof before_s operator.proof.path old_root
       >* assert_merkle_proof after_s operator.proof.path new_root
       (* When checking MPs, we need to check that the whole leaves are
          consistent, not just the new balance *)
       >* let* new_bl_operator =
            Bounded_u.add_left operator.before.tez_balance fees
          in
          let new_acc_operator =
            {operator.before with tez_balance = new_bl_operator}
          in
          check_eq_account new_acc_operator operator.after
          >*
          let x_pk = of_pair operator.before.pk |> fst in
          let x_g = of_pair generator |> fst in
          let* diff = Num.add x_pk ~qr:S.mone x_g in
          with_bool_check (Num.is_not_zero diff)

  let hash_op = function
    | `Transfer (tx : transfer_u) ->
        let* compressed =
          monadic_compress
            Bounded_u.
              [
                f tx.header.op_code;
                f tx.header.price.amount;
                f tx.payload.msg.src;
                f tx.payload.msg.dst;
                f tx.payload.msg.fee;
                f tx.payload.msg.amount.amount;
                f tx.payload.msg.cnt;
              ]
        in
        Hash.digest
          ~input_length:4
          (to_list
             [
               tx.header.l1_dst;
               compressed;
               tx.header.price.id;
               tx.payload.msg.amount.id;
             ])
    | `Create (tx : create_u) ->
        let* compressed =
          monadic_compress
            Bounded_u.
              [
                f tx.header.op_code;
                f tx.header.price.amount;
                f tx.payload.msg.fee;
              ]
        in
        let x_pk, y_pk = of_pair tx.payload.msg.pk in
        Hash.digest
          ~input_length:5
          (to_list
             [tx.header.l1_dst; compressed; x_pk; y_pk; tx.header.price.id])
    | `Credit (tx : credit_u) ->
        let* compressed =
          monadic_compress
            Bounded_u.
              [
                f tx.header.op_code;
                f tx.header.price.amount;
                f tx.payload.dst;
                f tx.payload.amount.amount;
                f tx.payload.cnt;
              ]
        in
        Hash.digest
          ~input_length:4
          (to_list
             [
               tx.header.l1_dst;
               compressed;
               tx.header.price.id;
               tx.payload.amount.id;
             ])
    | `Debit (tx : debit_u) ->
        let* compressed =
          monadic_compress
            Bounded_u.
              [
                f tx.header.op_code;
                f tx.header.price.amount;
                f tx.payload.msg.src;
                f tx.payload.msg.amount.amount;
                f tx.payload.msg.cnt;
              ]
        in
        Hash.digest
          ~input_length:4
          (to_list
             [
               tx.header.l1_dst;
               compressed;
               tx.header.price.id;
               tx.payload.msg.amount.id;
             ])

  let expected_op_code : Types.P.tx -> S.t = function
    | Types.P.Transfer _ -> S.zero
    | Types.P.Create _ -> S.one
    | Types.P.Credit _ -> S.of_int 2
    | Types.P.Debit _ -> S.of_int 3

  let get_op_code : Types.P.tx -> Z.t =
    let open Types.P.Bounded in
    function
    | Types.P.Transfer tx -> v tx.header.op_code
    | Types.P.Create tx -> v tx.header.op_code
    | Types.P.Credit tx -> v tx.header.op_code
    | Types.P.Debit tx -> v tx.header.op_code

  let predicate_ill_formed ~old_root ~old_next_pos ~new_root ~new_next_pos ~fee
      ~exit_validity ~rollup_id (t : Types.P.tx) =
    let safety = Encodings.Bounded_e.Safe in
    let* old_root = input ~kind:`Public @@ Input.scalar old_root in
    let* old_next_pos =
      input ~kind:`Public @@ Encodings.(pos_encoding ~safety).input old_next_pos
    in
    let* new_root = input ~kind:`Public @@ Input.scalar new_root in
    let* new_next_pos =
      input ~kind:`Public @@ Encodings.(pos_encoding ~safety).input new_next_pos
    in
    let* fee =
      input ~kind:`Public @@ Encodings.((fee_encoding ~safety).input) fee
    in
    let fee = Encodings.((fee_encoding ~safety).decode) fee in
    let* exit_validity = input ~kind:`Public @@ Input.bool exit_validity in
    let* _rollup_id =
      input ~kind:`Public @@ Encodings.(tezos_zkru_encoding.input) rollup_id
    in
    (* Assert that fee = 0 *)
    Bool.assert_false (unsafe_bool_of_scalar @@ coerce fee)
    >* assert_equal old_root new_root
    >* assert_equal old_next_pos new_next_pos
    >*
    match t with
    | Types.P.Transfer tx ->
        let* tx =
          input ~kind:`Public
          @@ Encodings.((transfer_encoding ~safety).input) tx
        in
        let tx = Encodings.((transfer_encoding ~safety).decode) tx in
        Num.assert_eq_const (coerce tx.header.op_code) (expected_op_code t)
        >* let* b_tx = get_checks_wire in
           Bool.assert_false b_tx
    | Types.P.Create tx ->
        let* tx =
          input ~kind:`Public @@ Encodings.((create_encoding ~safety).input) tx
        in
        let tx = Encodings.((create_encoding ~safety).decode) tx in
        Num.assert_eq_const (coerce tx.header.op_code) (expected_op_code t)
        >* Bool.assert_true exit_validity
        >* let* b_tx = get_checks_wire in
           Bool.assert_false b_tx
    | Types.P.Credit tx ->
        let* tx =
          input ~kind:`Public @@ Encodings.(credit_encoding ~safety).input tx
        in
        let tx = Encodings.((credit_encoding ~safety).decode) tx in
        Num.assert_eq_const (coerce tx.header.op_code) (expected_op_code t)
        >* Bool.assert_true exit_validity
        >* let* b_tx = get_checks_wire in
           Bool.assert_false b_tx
    | Types.P.Debit tx ->
        let* tx =
          input ~kind:`Public @@ Encodings.(debit_encoding ~safety).input tx
        in
        let tx = Encodings.((debit_encoding ~safety).decode) tx in
        Num.assert_eq_const (coerce tx.header.op_code) (expected_op_code t)
        >* Bool.assert_false exit_validity
        >* let* b_tx = get_checks_wire in
           Bool.assert_false b_tx

  let transfer_circuit ~op_code ~old_root ~old_next_pos ~rollup_id ~generator
      (tx : transfer_u) (tx_s : transfer_storage_u) =
    (* The validation of a transaction is done through a series of
             boolean checks and assertions.
             The assertions are important to stop malicious validators
             from censoring transactions. Given that the circuit can
             only determine the validity of a Tx from the src/dst accounts
             provided by the prover, it is important to make sure that
             these values are correct (through the Merkle proofs) and
             in the right positions.
             Thus, the Merkle proofs must always be valid. This has two
             consecuences:
               1. The positions present in a Tx must always be in range.
               2. We add more constraints, as we need to check that the
               leaves used for the proofs of the new state correspond to
               their expected values.
    *)
    Num.assert_eq_const (coerce tx.header.op_code) op_code
    >* (* ---------- Assert the init src leaf is in the init tree ---------- *)
    assert_tree_proofs
      tx_s.src.account.before
      tx_s.src.leaf.before
      tx_s.src.account.proof.path
      tx_s.src.leaf.path
      old_root
    >* (* ---------- Assert the init dst leaf is in the tmp tree ---------- *)
    assert_tree_proofs
      tx_s.dst.account.before
      tx_s.dst.leaf.before
      tx_s.dst.account.proof.path
      tx_s.dst.leaf.path
      tx_s.src.account.proof.root
    >* (* ---------- Assert the new src leaf is in the tmp tree ---------- *)
    assert_tree_proofs
      tx_s.src.account.after
      tx_s.src.leaf.after
      tx_s.src.account.proof.path
      tx_s.src.leaf.path
      tx_s.src.account.proof.root
    >* (* ---------- Assert the new dst leaf is in the new tree ---------- *)
    assert_tree_proofs
      tx_s.dst.account.after
      tx_s.dst.leaf.after
      tx_s.dst.account.proof.path
      tx_s.dst.leaf.path
      tx_s.dst.account.proof.root
    >* (* ------------------- Assert positions ---------------------
          Leaves contain their position to check that the proof's
          path actually corresponds to the correct leaf. *)
    assert_equal (coerce tx.payload.msg.src) (coerce tx_s.src.leaf.before.pos)
    >* assert_equal
         (coerce tx.payload.msg.dst)
         (coerce tx_s.dst.leaf.before.pos)
    (* ----------------- Check rollup id ----------------------- *)
    >* with_bool_check (equal rollup_id tx.header.rollup_id)
    >* (* ----------------- Check ticket ids ----------------------
          If the amount is in tez (or the dst balance is 0), then we don't enforce this *)
    let* is_tez = Num.is_eq_const tx.payload.msg.amount.id Constants.tez_id in
    let* dst_bal_is_0 =
      Num.is_zero @@ coerce tx_s.dst.leaf.before.ticket.amount
    in
    let* equal_src =
      equal tx.payload.msg.amount.id tx_s.src.leaf.before.ticket.id
    in
    with_bool_check (Bool.bor is_tez equal_src)
    >* let* is_tez_or_bal_0 = Bool.bor is_tez dst_bal_is_0 in
       let* equal_dst =
         equal tx.payload.msg.amount.id tx_s.dst.leaf.before.ticket.id
       in
       with_bool_check (Bool.bor is_tez_or_bal_0 equal_dst)
       >* (* ----------------- Check new leaves -----------------------*)
       let* z = constant_scalar S.zero in
       let* ticket_amount =
         Bool.ifthenelse is_tez z (coerce tx.payload.msg.amount.amount)
       in
       let ticket_amount =
         Bounded_u.make_unsafe ~bound:Constants.Bound.max_amount ticket_amount
       in
       let* new_ticket_amnt_src =
         Bounded_u.sub_left tx_s.src.leaf.before.ticket.amount ticket_amount
       in
       let new_ticket_src =
         {id = tx_s.src.leaf.before.ticket.id; amount = new_ticket_amnt_src}
       in
       let new_leaf_src = {tx_s.src.leaf.before with ticket = new_ticket_src} in
       let* new_ticket_amnt_dst =
         Bounded_u.add_left tx_s.dst.leaf.before.ticket.amount ticket_amount
       in
       let* new_ticket_id_dst =
         Bool.ifthenelse
           is_tez
           tx_s.dst.leaf.before.ticket.id
           tx.payload.msg.amount.id
       in
       let new_ticket_dst =
         {id = new_ticket_id_dst; amount = new_ticket_amnt_dst}
       in
       let new_leaf_dst = {tx_s.dst.leaf.before with ticket = new_ticket_dst} in
       check_eq_leaf new_leaf_src tx_s.src.leaf.after
       >* check_eq_leaf new_leaf_dst tx_s.dst.leaf.after
       >* (* ----------------- Check new accounts -----------------------*)
       let* tez_transfer_amount =
         Bool.ifthenelse is_tez (coerce tx.payload.msg.amount.amount) z
       in
       let tez_transfer_amount =
         Bounded_u.make_unsafe
           ~bound:Constants.Bound.max_amount
           tez_transfer_amount
       in
       let* tez_amount =
         Bounded_u.add_left tez_transfer_amount tx.payload.msg.fee
       in

       let* new_tez_bal_src =
         Bounded_u.sub_left tx_s.src.account.before.tez_balance tez_amount
       in
       let new_acc_src =
         {
           tx_s.src.account.before with
           tez_balance = new_tez_bal_src;
           cnt = tx.payload.msg.cnt;
           (* This value has already been checked in the Merkle proof, see README *)
           tickets_root = tx_s.src.account.after.tickets_root;
         }
       in
       let* new_tez_bal_dst =
         Bounded_u.add_left
           tx_s.dst.account.before.tez_balance
           tez_transfer_amount
       in
       let new_acc_dst =
         {
           tx_s.dst.account.before with
           tickets_root = tx_s.dst.account.after.tickets_root;
           tez_balance = new_tez_bal_dst;
         }
       in
       check_eq_account new_acc_src tx_s.src.account.after
       >* check_eq_account new_acc_dst tx_s.dst.account.after
       >* (* ---------------------- Check counter ----------------------- *)
       let* expected_cnt = Bounded_u.succ tx_s.src.account.before.cnt in
       with_bool_check (equal (coerce expected_cnt) (coerce tx.payload.msg.cnt))
       >* (* ---------------------- Check price = 0 ----------------------- *)
       with_bool_check (Num.is_zero @@ coerce tx.header.price.amount)
       >*
       (* Check pk_dst <> gen (used as dummy pk to note closed accounts)
          Checking that the x coordinates of pk_dst and generator are
          different is enough as we do not want both points with the
          generator x coordinate to be used as public key. *)
       let x_pk = of_pair tx_s.dst.account.before.pk |> fst in
       let x_g = of_pair generator |> fst in
       let* diff = Num.add x_pk ~qr:S.mone x_g in
       with_bool_check (Num.is_not_zero diff)
       >* (* Building signature message
             ---------- Verify signature ---------- *)
       let* msg = hash_op (`Transfer tx) in
       (* Building signature proof *)
       with_bool_check
         (Schnorr.verify
            ~compressed:true
            ~g:generator
            ~msg
            ~pk:tx_s.src.account.before.pk
            ~signature:tx.payload.signature
            ())
       >* let* b_tx = get_checks_wire in
          let* expected_fee =
            Bool.ifthenelse b_tx (coerce tx.payload.msg.fee) z
          in
          let* root_next =
            Bool.ifthenelse b_tx tx_s.dst.account.proof.root old_root
          in
          assert_equal b_tx tx_s.valid
          >* ret
               ( root_next,
                 old_next_pos,
                 Bounded_u.make_unsafe
                   ~bound:Constants.Bound.max_fee
                   expected_fee )

  let predicate_op ?(public = true) ~old_root ~old_next_pos ~new_root
      ~new_next_pos ~fee ~exit_validity ~rollup_id (t : Types.P.tx)
      (t_storage : Types.P.tx_storage) =
    (* bounded encoding safety *)
    let safety = Encodings.Bounded_e.Unsafe in
    let* old_root = input ~kind:`Public @@ Input.scalar old_root in
    let* old_next_pos =
      input ~kind:`Public @@ Encodings.(pos_encoding ~safety).input old_next_pos
    in
    let* new_root = input ~kind:`Public @@ Input.scalar new_root in
    let* new_next_pos =
      input ~kind:`Public @@ Encodings.(pos_encoding ~safety).input new_next_pos
    in
    let* fee =
      input ~kind:`Public @@ Encodings.((fee_encoding ~safety).input) fee
    in
    let fee = Encodings.((fee_encoding ~safety).decode) fee in
    let* exit_validity = input ~kind:`Public @@ Input.bool exit_validity in
    let* rollup_id =
      input ~kind:`Public @@ Encodings.(tezos_zkru_encoding.input) rollup_id
    in
    match (t, t_storage) with
    | Transfer tx, Transfer tx_s ->
        let kind = if public then `Public else `Private in
        let* tx =
          input ~kind @@ Encodings.((transfer_encoding ~safety).input) tx
        in
        let tx = Encodings.((transfer_encoding ~safety).decode) tx in
        let* tx_s = input @@ Encodings.(transfer_storage_encoding.input) tx_s in
        let tx_s = Encodings.(transfer_storage_encoding.decode) tx_s in
        let* generator =
          Plompiler_Curve.(input_point @@ affine_to_point Curve.one)
        in
        let* root_next, next_pos_next, computed_fee =
          transfer_circuit
            ~op_code:(expected_op_code t)
            ~old_root
            ~old_next_pos
            ~rollup_id
            ~generator
            tx
            tx_s
        in
        assert_equal root_next new_root
        >* assert_equal next_pos_next new_next_pos
        >* assert_equal (coerce computed_fee) (coerce fee)
    | Create tx, Create tx_s ->
        assert public ;
        let* tx =
          input ~kind:`Public @@ Encodings.((create_encoding ~safety).input) tx
        in
        let tx = Encodings.((create_encoding ~safety).decode) tx in
        let* tx_s = input @@ Encodings.(create_storage_encoding.input) tx_s in
        let tx_s = Encodings.(create_storage_encoding.decode) tx_s in
        let* generator =
          Plompiler_Curve.(input_point @@ affine_to_point Curve.one)
        in
        Num.assert_eq_const (coerce tx.header.op_code) (expected_op_code t)
        >* let* dst_account_before_s = hash_account tx_s.dst.account.before in
           let* dst_account_after_s = hash_account tx_s.dst.account.after in
           let* next_empty_account_s =
             hash_account tx_s.next_empty.account.before
           in
           assert_merkle_proof
             dst_account_before_s
             tx_s.dst.account.proof.path
             old_root
           >* assert_merkle_proof
                dst_account_after_s
                tx_s.dst.account.proof.path
                tx_s.dst.account.proof.root
           >* assert_merkle_proof
                next_empty_account_s
                tx_s.next_empty.account.proof.path
                old_root
           (* [tx_s.next_empty.after] is ignored, as that account doesn't change *)
           >* (* Assert that the position used is the old_next_pos
                 This value has already been checked in the Merkle proof, see README *)
           assert_equal old_next_pos (coerce tx_s.dst.leaf.after.pos)
           >* assert_equal new_next_pos (coerce tx_s.next_empty.leaf.before.pos)
           >*
           (* Assert new_next_pos is "default" *)
           let x_pk = of_pair tx_s.next_empty.account.before.pk |> fst in
           let x_g = of_pair generator |> fst in
           let* diff = Num.add x_pk ~qr:S.mone x_g in
           with_bool_check (Num.is_zero diff)
           >*
           (* Check initial account is "default" *)
           let x_pk = of_pair tx_s.dst.account.before.pk |> fst in
           let x_g = of_pair generator |> fst in
           let* diff = Num.add x_pk ~qr:S.mone x_g in
           with_bool_check (Num.is_zero diff)
           (* Compare with expected account *)
           >*
           let new_acc_dst =
             {tx_s.dst.account.before with pk = tx.payload.msg.pk}
           in
           check_eq_account new_acc_dst tx_s.dst.account.after
           (* ----------------- Check rollup id -----------------------*)
           >* with_bool_check (equal rollup_id tx.header.rollup_id)
           >* (* -- Check price = fee and that fee is the expected value -- *)
           with_bool_check (Num.is_eq_const tx.header.price.id Constants.tez_id)
           >* with_bool_check
                (equal
                   (coerce tx.header.price.amount)
                   (coerce tx.payload.msg.fee))
           >* with_bool_check
                (Num.is_eq_const
                   (coerce tx.payload.msg.fee)
                   (S.of_z Constants.create_fee))
           >* (* ---------- Verify signature ---------- *)
              (* Building signature message *)
              (* TODO: We could hash it as
                 Hash(pk_x, pk_y, fee) := Anemoi(pk_x, pk_y + 2 * fee)
              *)
           let* msg = hash_op (`Create tx) in
           (* Building signature proof *)
           with_bool_check
             (Schnorr.verify
                ~compressed:true
                ~g:generator
                ~msg
                ~pk:tx.payload.msg.pk
                ~signature:tx.payload.signature
                ())
           >* let* b_tx = get_checks_wire in
              let* z = constant_scalar S.zero in
              let* expected_fee =
                Bool.ifthenelse b_tx (coerce tx.payload.msg.fee) z
              in
              assert_equal (coerce fee) expected_fee
              >* let* root_next =
                   Bool.ifthenelse b_tx tx_s.dst.account.proof.root old_root
                 in
                 assert_equal b_tx tx_s.valid
                 >* let* not_valid = Bool.bnot b_tx in
                    assert_equal not_valid exit_validity
                    >* assert_equal root_next new_root
    | Credit tx, Credit tx_s ->
        assert public ;
        let* tx =
          input ~kind:`Public @@ Encodings.(credit_encoding ~safety).input tx
        in
        let tx = Encodings.(credit_encoding ~safety).decode tx in
        let* tx_s = input @@ Encodings.(credit_storage_encoding.input) tx_s in
        let tx_s = Encodings.(credit_storage_encoding.decode) tx_s in
        let* generator =
          Plompiler_Curve.(input_point @@ affine_to_point Curve.one)
        in

        assert_equal old_next_pos new_next_pos
        >* Num.assert_eq_const (coerce tx.header.op_code) (expected_op_code t)
        >* (* ---------- Assert the init dst leaf is in the init tree ---------- *)
        assert_tree_proofs
          tx_s.dst.account.before
          tx_s.dst.leaf.before
          tx_s.dst.account.proof.path
          tx_s.dst.leaf.path
          old_root
        >* (* ---------- Assert the new dst leaf is in the new tree ---------- *)
        assert_tree_proofs
          tx_s.dst.account.after
          tx_s.dst.leaf.after
          tx_s.dst.account.proof.path
          tx_s.dst.leaf.path
          tx_s.dst.account.proof.root
        >* 
        (* Leaves contain their position to check that the proof's
           path actually corresponds to the correct leaf. *)(* ------------------- Assert positions --------------------- *)
        assert_equal (coerce tx.payload.dst) (coerce tx_s.dst.leaf.before.pos)
        (* Assert fee is equal to 0 *)
        >* Bool.assert_false (unsafe_bool_of_scalar @@ coerce fee)
        (* ----------------- Check rollup id -----------------------*)
        >* with_bool_check (equal rollup_id tx.header.rollup_id)
        (* ----------------- Check ticket ids ---------------------- *)
        (* You can credit to any position with a balance of 0 *)
        >* let* is_tez =
             Num.is_eq_const tx.payload.amount.id Constants.tez_id
           in
           let* eq_id =
             equal tx.payload.amount.id tx_s.dst.leaf.before.ticket.id
           in
           let* is_tez_or_eq_id = Bool.bor is_tez eq_id in
           let* bal_0 =
             Num.is_zero (coerce tx_s.dst.leaf.before.ticket.amount)
           in
           with_bool_check (Bool.bor is_tez_or_eq_id bal_0)
           >* (* ----------------- Check new leaf and acc ---------------------- *)
           let* z = constant_scalar S.zero in
           let* ticket_amount =
             Bool.ifthenelse is_tez z (coerce tx.payload.amount.amount)
           in
           let ticket_amount =
             Bounded_u.make_unsafe
               ~bound:Constants.Bound.max_amount
               ticket_amount
           in
           let* new_ticket_amnt_dst =
             Bounded_u.add_left tx_s.dst.leaf.before.ticket.amount ticket_amount
           in
           let* new_ticket_id_dst =
             Bool.ifthenelse
               is_tez
               tx_s.dst.leaf.before.ticket.id
               tx.payload.amount.id
           in
           let new_ticket_dst =
             {id = new_ticket_id_dst; amount = new_ticket_amnt_dst}
           in

           let new_leaf_dst =
             {tx_s.dst.leaf.before with ticket = new_ticket_dst}
           in
           let* tez_credit_amount =
             Bool.ifthenelse is_tez (coerce tx.payload.amount.amount) z
           in
           let tez_credit_amount =
             Bounded_u.make_unsafe
               ~bound:Constants.Bound.max_amount
               tez_credit_amount
           in
           let* new_tez_bal_dst =
             Bounded_u.add_left
               tx_s.dst.account.before.tez_balance
               tez_credit_amount
           in
           let new_acc_dst =
             {
               tx_s.dst.account.before with
               tez_balance = new_tez_bal_dst;
               cnt = tx.payload.cnt;
               tickets_root = tx_s.dst.account.after.tickets_root;
             }
           in
           check_eq_leaf new_leaf_dst tx_s.dst.leaf.after
           >* check_eq_account new_acc_dst tx_s.dst.account.after
           >* (* ---------------------- Check counter ----------------------- *)
           let* expected_cnt = Bounded_u.succ tx_s.dst.account.before.cnt in
           with_bool_check (equal (coerce expected_cnt) (coerce tx.payload.cnt))
           >*
           (* Check pk_dst <> gen (used as dummy pk to note closed accounts) *)
           (* Checking that the x coordinates of pk_dst and generator are
              different is enough as we do not want both points with the
              generator x coordinate to be used as public key. *)
           let x_pk = of_pair tx_s.dst.account.before.pk |> fst in
           let x_g = of_pair generator |> fst in
           let* diff = Num.add x_pk ~qr:S.mone x_g in
           with_bool_check (Num.is_not_zero diff)
           >* (* ---------- Check price = amount ---------- *)
           with_bool_check (equal tx.header.price.id tx.payload.amount.id)
           >* with_bool_check
                (equal
                   (coerce tx.header.price.amount)
                   (coerce tx.payload.amount.amount))
           >* let* b_tx = get_checks_wire in
              let* root_next =
                Bool.ifthenelse b_tx tx_s.dst.account.proof.root old_root
              in
              assert_equal b_tx tx_s.valid
              >* let* not_valid = Bool.bnot b_tx in
                 assert_equal not_valid exit_validity
                 >* assert_equal root_next new_root
    | Debit tx, Debit tx_s ->
        assert public ;
        let* tx =
          input ~kind:`Public @@ Encodings.(debit_encoding ~safety).input tx
        in
        let tx = Encodings.(debit_encoding ~safety).decode tx in
        let* tx_s = input @@ Encodings.(debit_storage_encoding.input) tx_s in
        let tx_s = Encodings.(debit_storage_encoding.decode) tx_s in
        let* generator =
          Plompiler_Curve.(input_point @@ affine_to_point Curve.one)
        in

        assert_equal old_next_pos new_next_pos
        >* Num.assert_eq_const (coerce tx.header.op_code) (expected_op_code t)
        >* (* ---------- Assert the init src leaf is in the init tree ---------- *)
        assert_tree_proofs
          tx_s.src.account.before
          tx_s.src.leaf.before
          tx_s.src.account.proof.path
          tx_s.src.leaf.path
          old_root
        >* (* ---------- Assert the new src leaf is in the new tree ---------- *)
        assert_tree_proofs
          tx_s.src.account.after
          tx_s.src.leaf.after
          tx_s.src.account.proof.path
          tx_s.src.leaf.path
          tx_s.src.account.proof.root
        >* 
        (* Leaves contain their position to check that the proof's
           path actually corresponds to the correct leaf. *)(* ------------------- Assert positions --------------------- *)
        assert_equal
          (coerce tx.payload.msg.src)
          (coerce tx_s.src.leaf.before.pos)
        (* Assert fee is equal to 0 *)
        >* Bool.assert_false (unsafe_bool_of_scalar @@ coerce fee)
        (* ----------------- Check rollup id -----------------------*)
        >* with_bool_check (equal rollup_id tx.header.rollup_id)
        (* ----------------- Check ticket ids ---------------------- *)
        >* let* is_tez =
             Num.is_eq_const tx.payload.msg.amount.id Constants.tez_id
           in
           let* eq_id =
             equal tx.payload.msg.amount.id tx_s.src.leaf.before.ticket.id
           in
           with_bool_check (Bool.bor is_tez eq_id)
           >* (* ----------------- Check new leaves -----------------------*)
           let* z = constant_scalar S.zero in
           let* ticket_amount =
             Bool.ifthenelse is_tez z (coerce tx.payload.msg.amount.amount)
           in
           let ticket_amount =
             Bounded_u.make_unsafe
               ~bound:Constants.Bound.max_amount
               ticket_amount
           in
           let* new_ticket_amnt_src =
             Bounded_u.sub_left tx_s.src.leaf.before.ticket.amount ticket_amount
           in
           let new_ticket_src =
             {id = tx.payload.msg.amount.id; amount = new_ticket_amnt_src}
           in
           let new_leaf_src =
             {tx_s.src.leaf.before with ticket = new_ticket_src}
           in

           let* tez_debit_amount =
             Bool.ifthenelse is_tez (coerce tx.payload.msg.amount.amount) z
           in
           let tez_debit_amount =
             Bounded_u.make_unsafe
               ~bound:Constants.Bound.max_amount
               tez_debit_amount
           in
           let* new_tez_bal_src =
             Bounded_u.sub_left
               tx_s.src.account.before.tez_balance
               tez_debit_amount
           in
           let new_acc_src =
             {
               tx_s.src.account.before with
               tez_balance = new_tez_bal_src;
               cnt = tx.payload.msg.cnt;
               tickets_root = tx_s.src.account.after.tickets_root;
             }
           in

           check_eq_leaf new_leaf_src tx_s.src.leaf.after
           >* check_eq_account new_acc_src tx_s.src.account.after
           >* (* ---------------------- Check counter ----------------------- *)
           let* expected_cnt = Bounded_u.succ tx_s.src.account.before.cnt in
           with_bool_check
             (equal (coerce expected_cnt) (coerce tx.payload.msg.cnt))
           >* (* ---------- Check price = amount ---------- *)
           with_bool_check (equal tx.header.price.id tx.payload.msg.amount.id)
           >* with_bool_check
                (equal
                   (coerce tx.header.price.amount)
                   (coerce tx.payload.msg.amount.amount))
           >* (* ---------- Verify signature ---------- *)

              (* Building signature message *)
           let* msg = hash_op (`Debit tx) in
           (* Building signature proof *)
           with_bool_check
             (Schnorr.verify
                ~compressed:true
                ~g:generator
                ~msg
                ~pk:tx_s.src.account.before.pk
                ~signature:tx.payload.signature
                ())
           >* let* b_tx = get_checks_wire in
              let* root_next =
                Bool.ifthenelse b_tx tx_s.src.account.proof.root old_root
              in
              assert_equal b_tx tx_s.valid
              >* assert_equal b_tx exit_validity
              >* assert_equal root_next new_root
    | _ -> assert false

  let predicate_private_batch ~old_root ~old_next_pos ~new_root ~new_next_pos
      ~fees ~rollup_id (ops : Types.P.transfer list)
      (ops_s : Types.P.transfer_storage list) =
    assert (List.compare_lengths ops ops_s = 0) ;
    let safety = Encodings.Bounded_e.Unsafe in
    let* old_root = input ~kind:`Public @@ Input.scalar old_root in
    let* old_next_pos =
      input ~kind:`Public @@ Encodings.(pos_encoding ~safety).input old_next_pos
    in
    let* new_root = input ~kind:`Public @@ Input.scalar new_root in
    let* new_next_pos =
      input ~kind:`Public @@ Encodings.(pos_encoding ~safety).input new_next_pos
    in
    let* fees =
      input ~kind:`Public @@ Encodings.((amount_encoding ~safety).input) fees
    in
    let fees = Encodings.((amount_encoding ~safety).decode) fees in
    let* rollup_id =
      input ~kind:`Public @@ Encodings.(tezos_zkru_encoding.input) rollup_id
    in
    let* ops =
      mapM
        (fun tx -> input @@ Encodings.((transfer_encoding ~safety).input tx))
        ops
    in
    let ops = List.map Encodings.((transfer_encoding ~safety).decode) ops in
    let* ops_s =
      mapM
        (fun tx_s -> input @@ Encodings.(transfer_storage_encoding.input tx_s))
        ops_s
    in
    let ops_s = List.map Encodings.(transfer_storage_encoding.decode) ops_s in
    let* generator =
      Plompiler_Curve.(input_point @@ affine_to_point Curve.one)
    in
    let op_code = S.zero in
    let* z = constant_scalar S.zero in
    let z = Bounded_u.make_unsafe ~bound:Constants.Bound.max_amount z in
    let* computed_root, computed_fees =
      fold2M
        (fun (computed_root, computed_fees) op op_s ->
          let* computed_root, _, fee =
            transfer_circuit
              ~op_code
              ~old_root:computed_root
              ~old_next_pos
              ~rollup_id
              ~generator
              op
              op_s
          in
          (* TODO: should we bound check every time? Or just at the end *)
          let* computed_fees =
            Bounded_u.add_left ~unsafe:true computed_fees fee
          in
          ret (computed_root, computed_fees))
        (old_root, z)
        ops
        ops_s
    in
    assert_equal computed_root new_root
    >* assert_equal old_next_pos new_next_pos
    >* assert_equal (coerce computed_fees) (coerce fees)
end

(* for each proof, pi = [old_root, old_next_pos, new_root, new_next_pos, fees, rollup_id]
   public pi are the first old_root, the last next_root, the last fees, the last rollup_id
   - for all proofs, next old_root must be equal to current next_root
   - for all proofs, rollup_id must be the same
   - for each proof, fees is the sum of its transactions fees
   - for all proofs, old_next_pos & new_next_pos must be the same
*)
module PI_parameters_predicate_private_batch = struct
  module L = LibCircuit

  (* accumulator type for the fold_left in check_pi *)
  type acc = {
    root : L.scalar L.repr;
    pos : L.scalar L.repr;
    total_fees : L.scalar L.repr;
  }

  let inner_elt pi_list =
    match pi_list with
    | [old_root; old_next_pos; new_root; new_next_pos; fees; rollup_id] ->
        (old_root, old_next_pos, new_root, new_next_pos, fees, rollup_id)
    | _ -> failwith "invalid inner_pi format."

  let outer_elt pi_list =
    match pi_list with
    | [old_root; new_root; total_fees; rollup_id] ->
        (old_root, new_root, total_fees, rollup_id)
    | _ -> failwith "invalid outer_pi format."

  let nb_inner = 6

  let nb_outer = 4

  (* /!\ Note that this function assumes that the first proof is not turned off by the switches (ie the first switch is true) ;
     If the first proof is turned off, this function will NOT return the expected result
  *)
  let check ~switches ~outer ~inner =
    let open L in
    let init, first_root, init_rollup_id =
      let first_root, _old_next_pos, new_root, new_next_pos, fees, rollup_id =
        inner_elt (List.hd inner)
      in
      ( ({root = new_root; pos = new_next_pos; total_fees = fees}, []),
        first_root,
        rollup_id )
    in
    let old_root, new_root, total_fees, outer_rollup_id = outer_elt outer in
    let* acc, inner_checks =
      fold2M
        (fun (acc, checks) pi_list switch ->
          let* n_switch = Bool.bnot switch in
          let old_root, old_next_pos, new_root, new_next_pos, fees, rollup_id =
            inner_elt pi_list
          in
          let* check_old_pos =
            let* res = equal acc.pos old_next_pos in
            Bool.bor n_switch res
          in
          let* check_roots =
            let* res = equal acc.root old_root in
            Bool.bor n_switch res
          in
          let* check_id =
            let* res = equal outer_rollup_id rollup_id in
            Bool.bor n_switch res
          in
          let* total_fees =
            let* fees = Num.mul (scalar_of_bool switch) fees in
            Num.add acc.total_fees fees
          in
          let checks = [check_old_pos; check_roots; check_id] @ checks in
          let* root = Bool.ifthenelse switch new_root acc.root in
          let* pos = Bool.ifthenelse switch new_next_pos acc.pos in
          ret ({root; pos; total_fees}, checks))
        init
        (List.tl inner)
        (List.tl switches)
    in
    let* check_fees = equal total_fees acc.total_fees in
    let* check_first_root = equal old_root first_root in
    let* check_last_root = equal new_root acc.root in
    let* check_fst_rollup_id = equal outer_rollup_id init_rollup_id in
    Bool.band_list
      ([check_fees; check_first_root; check_last_root; check_fst_rollup_id]
      @ inner_checks)

  let outer_of_inner inner =
    let old_root, _, _, _, first_fees, rollup_id = inner_elt (List.hd inner) in
    let new_root, total_fees =
      List.fold_left
        (fun (_, acc_fees) pi ->
          let _, _, new_root, _, fees, _ = inner_elt pi in
          let acc_fees = Plonk.Bls.Scalar.(acc_fees + fees) in
          (new_root, acc_fees))
        (old_root, first_fees)
        (List.tl inner)
    in
    [old_root; new_root; total_fees; rollup_id]
end
