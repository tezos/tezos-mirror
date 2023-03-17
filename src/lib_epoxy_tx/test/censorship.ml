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
open Plonk_test.Helpers
open Helpers
open P
module TP = Types.P
module HashPV = Plompiler.Anemoi128
module SchnorrPV = Plompiler.Schnorr (HashPV)
module Schnorr = SchnorrPV.P

let sks : Schnorr.sk array =
  Array.init Constants.max_nb_accounts (fun _ ->
      Mec.Curve.Jubjub.AffineEdwards.Scalar.random ())

let pks = Array.map Schnorr.neuterize sks

module Censorship (L : LIB) = struct
  open Utils (L)

  open Helpers.V (L)

  open V (L)

  let tez i =
    TP.
      {
        id = Epoxy_tx.Constants.tez_id;
        amount = Bounded.make ~bound:Constants.Bound.max_amount (Z.of_int i);
      }

  (* Invalid Merkle proofs:
     A malicious validator tries to use an incorrect Merkle proof
     to invalidate a valid transaction.
  *)
  let tests_incorrect_proof =
    let state =
      P.make_state Z.[(pks.(0), of_int 100, [||]); (pks.(1), of_int 10, [||])]
    in
    let r, state' =
      P.generate_transaction
        ~src_pos:Z.zero
        ~dst_pos:Z.(of_int @@ Constants.max_nb_tickets)
        ~amount:(tez 50)
        ~fee:Z.zero
        ~valid:false
        ~unsafe:true
        ~sks
        state
    in
    let tx_s = match r.tx_s with Transfer tx -> tx | _ -> assert false in
    let bad_src_proof =
      {tx_s.src.account.proof with path = List.rev tx_s.src.account.proof.path}
    in
    let bad_dst_path =
      {tx_s.dst.leaf with path = List.rev tx_s.dst.leaf.path}
    in
    let tx_s1 =
      {
        tx_s with
        src =
          {
            tx_s.src with
            account = {tx_s.src.account with proof = bad_src_proof};
          };
      }
    in
    let tx_s2 = {tx_s with dst = {tx_s.dst with leaf = bad_dst_path}} in
    let tx_s3 =
      {
        tx_s with
        src =
          {
            tx_s.src with
            account =
              {
                tx_s.src.account with
                proof = tx_s.dst.account.proof;
                before = tx_s.dst.account.before;
              };
          };
      }
    in
    [
      test
        ~valid:false
        ~name:"incorrect src proof"
        (circuit_op {r with tx_s = Transfer tx_s1} state state');
      test
        ~valid:false
        ~name:"incorrect dst proof"
        (circuit_op {r with tx_s = Transfer tx_s2} state state');
      test
        ~valid:false
        ~name:"incorrect src proof"
        (circuit_op {r with tx_s = Transfer tx_s3} state state');
    ]

  (* Prover tries to censor a valid Tx by using another leaf from the same pk
   *)
  (* let tests_bad_pos =
     (* State in which pk0 has two leaves: 0 and 2, with balaces 100 and 10
        and pk1 has a balance of 100 at pos 1.
     *)
     let open Constants.Bound in
     let open Types.P in
     let s =
       let max_size = Z.to_int (v max_nb_accounts) in
       let acc0 =
         {
           balance = Bounded.make ~bound:max_balance (Z.of_int 100);
           pk = Schnorr.neuterize P.sks.(0);
           cnt = Bounded.make ~bound:max_counter Z.zero;
           pos = Bounded.make ~bound:max_nb_accounts Z.zero;
         }
       in
       let acc1 =
         {
           balance = Bounded.make ~bound:max_balance (Z.of_int 100);
           pk = Schnorr.neuterize P.sks.(1);
           cnt = Bounded.make ~bound:max_counter Z.zero;
           pos = Bounded.make ~bound:max_nb_accounts Z.one;
         }
       in
       let acc2 =
         {
           acc0 with
           balance = Bounded.make ~bound:max_balance (Z.of_int 10);
           pos = Bounded.make ~bound:max_nb_accounts Z.(of_int 2);
         }
       in
       let accs_list = [(0, acc0); (1, acc1); (2, acc2)] in
       let accounts = IMap.of_seq (List.to_seq accs_list) in
       let leaves =
         Array.append
           (Array.of_list (List.map (fun (_, acc) -> P.make_leaf acc) accs_list))
           (Array.init (max_size - 3) (fun i ->
                P.make_leaf (P.default_account (3 + i))))
       in
       let tree = Merkle.generate_tree ~leaves Constants.depth_tree in
       {accounts; tree; next_position = List.length accs_list}
     in
     (* There is a Tx from pk0 (pos 0) to pk1 of 50 tokens, which should be
        valid. However, we craft it using pos 2 for pk0, and change it
        afterwards. The circuit should be able to catch this.
     *)
     let (r, s') =
       let unsafe = true in
       let actual_src_pos = Bounded.make ~bound:max_nb_accounts Z.zero in
       let src_pos_i = 2 in
       let dst_pos_i = 1 in
       let dst_pos = Bounded.make ~bound:max_nb_accounts (Z.of_int dst_pos_i) in
       let sk_src = P.sks.(0) in
       let acc_src = P.get_account src_pos_i s.accounts in
       let acc_dst = P.get_account dst_pos_i s.accounts in
       let cnt = Bounded.make ~bound:max_counter Z.one in
       let fee = Bounded.make ~bound:max_fee Z.zero in
       let amount = Bounded.make ~bound:max_amount (Z.of_int 50) in
       let msg =
         let compressed_msg =
           P.compress
             Bounded.[f actual_src_pos; f dst_pos; f fee; f amount; f cnt]
         in
         HashPV.P.direct ~input_length:1 [|S.of_z compressed_msg|]
       in
       let signature =
         let random = Curve.Scalar.random () in
         Schnorr.sign ~compressed:true sk_src msg random
       in
       let (src_proof, s) =
         let (_, path) = Merkle.proof_path src_pos_i s.tree in
         let new_bl_src =
           Bounded.(sub_left ~unsafe acc_src.balance (add ~unsafe amount fee))
         in
         let new_cnt_src = Bounded.succ ~unsafe acc_src.cnt in
         let new_acc_src =
           {acc_src with balance = new_bl_src; cnt = new_cnt_src}
         in
         let new_leaf_src = P.make_leaf new_acc_src in
         let accounts = IMap.add src_pos_i new_acc_src s.accounts in
         let tree =
           Merkle.update_tree ~input_length:2 s.tree src_pos_i new_leaf_src
         in
         let root = Merkle.root tree in
         ({path; root}, {s with tree; accounts})
       in
       let (dst_proof, s) =
         let (_, path) = Merkle.proof_path dst_pos_i s.tree in
         let new_bl_dst = Bounded.(add_left ~unsafe acc_dst.balance amount) in
         let new_acc_dst = {acc_dst with balance = new_bl_dst} in
         let new_leaf = P.make_leaf new_acc_dst in
         let accounts = IMap.add dst_pos_i new_acc_dst s.accounts in
         let tree =
           Merkle.update_tree ~input_length:2 s.tree dst_pos_i new_leaf
         in
         let root = Merkle.root tree in
         ({path; root}, {s with tree; accounts})
       in
       let acc_after_src = P.get_account src_pos_i s.accounts in
       let acc_after_dst = P.get_account dst_pos_i s.accounts in
       (* TODO: fix this! *)
       let payload : transfer_payload =
         {
           msg = {cnt; src = actual_src_pos; dst = dst_pos; amount; fee};
           signature;
         }
       in
       let header = Dummy.header in
       let op : transfer = {header; payload} in
       let src =
         {
           account_before = acc_src;
           account_after = acc_after_src;
           proof = src_proof;
         }
       in
       let dst =
         {
           account_before = acc_dst;
           account_after = acc_after_dst;
           proof = dst_proof;
         }
       in
       let tx_s : tx_storage = Transfer {src; dst; valid = true} in
       ({tx = Transfer op; tx_s; fee; hash = msg; exit_validity = false}, s)
     in
     [test ~valid:false ~name:"incorrect src proof" (circuit_op r s s')] *)

  let tests = tests_incorrect_proof
  (* @ tests_bad_pos *)
end

let tests =
  [
    Alcotest.test_case "Censorship" `Quick (to_test (module Censorship : Test));
    Alcotest.test_case
      "Censorship plonk"
      `Slow
      (to_test ~plonk:(module Plonk.Main_protocol) (module Censorship : Test));
  ]
