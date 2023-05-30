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

open Plompiler
open Plonk_test
module HashPV = Poseidon128
module MerklePV = Gadget.Merkle (HashPV)
module SchnorrPV = Plompiler.Schnorr (HashPV)
module Curve = SchnorrPV.Curve
module Hash = HashPV.P
module Merkle = MerklePV.P
module Schnorr = SchnorrPV.P

let two = Z.of_int 2

let depth_tree = 10

let max_nb_accounts = Z.(shift_left one depth_tree)

let max_balance = Z.(shift_left one 20)

let max_amount = Z.(shift_left one 20)

let max_fee = Z.(shift_left one 10)

let max_counter = Z.(shift_left one 20)

(* Checks to ensure that we do not have overflows when updating the balance. *)
let () = assert (Z.(two * max_balance < Bls12_381.Fr.order))

let () = assert (Z.(two * (max_amount + max_fee) < Bls12_381.Fr.order))

(* If size of an account balance and counter < BLS12-381 Fr order, we can
   compress the Merkle leaf to 2 scalars (pk_x, compress(balance, counter)). *)
let () =
  assert (
    Z.numbits max_balance + Z.numbits max_counter
    < Z.(numbits Bls12_381.Fr.order))

(* If the accumulated size of the two accounts, tx amount, tx fee and
   cnt_src < BLS12-381 Fr order, we can compress the Schnorr message to 1 scalar. *)
let () =
  assert (
    (2 * depth_tree) + Z.numbits max_amount + Z.numbits max_fee
    + Z.numbits max_counter
    < Z.(numbits Bls12_381.Fr.order))

let nb_bits_base = Z.numbits S.order

module TxHelpers = struct
  let of_int x = Z.of_int x |> S.of_z

  let to_bls_scalar s = Curve.Base.of_z (S.to_z s)

  let of_bls_scalar s = S.of_z (Curve.Base.to_z s)

  let point_to_affine (u, v) =
    Curve.unsafe_from_coordinates ~u:(to_bls_scalar u) ~v:(to_bls_scalar v)

  let affine_to_point p =
    let u = Curve.get_u_coordinate p in
    let v = Curve.get_v_coordinate p in
    (of_bls_scalar u, of_bls_scalar v)
end

include TxHelpers

let random_int = Stdlib.Random.int

let random_z bound = Random.int64 (Z.to_int64 bound) |> Z.of_int64

(* These functions aimed to format integers more efficiently by compressing
   them in a scalar. *)

(* This function computes the maximal bound of a list of bounded variables
   written as [(value, bound)_1, ..., (value, bound)_n] *)
let compression_bound values = List.fold_left Z.mul Z.one (List.map snd values)

(* This function attempts to compress a list of integers into one scalar. *)
let compress values =
  assert (compression_bound values < S.order) ;
  List.fold_left
    (fun acc (v, v_bound) -> Z.(v + (acc * v_bound)))
    (fst @@ List.hd values)
    (List.tl values)

type state = {
  sks : Schnorr.sk array;
  balances : Z.t array;
  counters : Z.t array;
  tree : Merkle.tree;
}

type account = {pk : Schnorr.pk; balance : Z.t; cnt : Z.t}

type proof = {init_path : Merkle.path; root : S.t; new_path : Merkle.path}

type tx = {
  src_pos : int;
  dst_pos : int;
  amount : Z.t;
  fee : Z.t;
  msg : S.t;
  src : account;
  dst : account;
  src_proof : proof;
  dst_proof : proof;
  signature : Schnorr.signature;
}

let make_leaf pk bal counter =
  (* We can use just the u coordinate of pk as Edwards curves are symmetric and as such there are only two possible v coordinates and the same sk is used to generate both.
     We could set a convention to only use pk with v coordination of a given parity, for instance v odd. *)
  (* TODO move this in schnorr and make the pk directly a single coordinate *)
  let u = Curve.get_u_coordinate pk |> of_bls_scalar in
  let compressed = compress [(bal, max_balance); (counter, max_counter)] in
  Hash.direct ~input_length:2 [|u; S.of_z compressed|]

let init_state () =
  let size = Z.to_int max_nb_accounts in
  let sks = Array.init size (fun _ -> Curve.Scalar.random ()) in
  let balances = Array.init size (fun _ -> random_z Z.(max_balance / two)) in
  let counters = Array.init size (fun _ -> random_z max_counter) in
  let leaves =
    Array.init size (fun i ->
        make_leaf (Schnorr.neuterize sks.(i)) balances.(i) counters.(i))
  in
  let tree = Merkle.generate_tree ~leaves depth_tree in
  {sks; balances; counters; tree}

let generate_transaction : state -> tx * state =
 fun ({sks; balances; counters; tree} as state) ->
  let size = Z.to_int max_nb_accounts in
  let src_pos = random_int size in
  let dst_pos = random_int size in
  let sk_src = sks.(src_pos) in
  let bl_src = balances.(src_pos) in
  let bl_dst = balances.(dst_pos) in
  let cnt_src = counters.(src_pos) in
  let pk_src = Schnorr.neuterize sk_src in
  let fee = random_z Z.(min bl_src max_fee) in
  let max_src_amount = Z.(min (bl_src - fee) max_amount) in
  let max_dst_amount = Z.(max_balance - bl_dst) in
  let amount = random_z Z.(min max_src_amount max_dst_amount) in
  let msg =
    let compressed_msg =
      compress
        [
          (Z.of_int src_pos, max_nb_accounts);
          (Z.of_int dst_pos, max_nb_accounts);
          (fee, max_fee);
          (amount, max_amount);
          (cnt_src, max_counter);
        ]
    in
    Hash.direct ~input_length:1 [|S.of_z compressed_msg|]
  in
  let signature =
    let random = Curve.Scalar.random () in
    Schnorr.sign ~compressed:true sk_src msg random
  in
  let src_proof, tree =
    let _, init_path = Merkle.proof_path src_pos tree in
    let new_bl_src = Z.(bl_src - amount - fee) in
    let new_cnt_src = Z.succ cnt_src in
    let new_leaf_src = make_leaf pk_src new_bl_src new_cnt_src in
    balances.(src_pos) <- Z.(bl_src - amount - fee) ;
    counters.(src_pos) <- new_cnt_src ;
    let tree = Merkle.update_tree ~input_length:2 tree src_pos new_leaf_src in
    let root = Merkle.root tree in

    let _, new_path = Merkle.proof_path src_pos tree in
    ({init_path; root; new_path}, tree)
  in
  (* We retrieve bl_dst after updating the src_leaf as dst may be equal to src *)
  let sk_dst = sks.(dst_pos) in
  let bl_dst = balances.(dst_pos) in
  let cnt_dst = counters.(dst_pos) in
  let pk_dst = Schnorr.neuterize sk_dst in
  let dst_proof, tree =
    let _, init_path = Merkle.proof_path dst_pos tree in
    let new_leaf = make_leaf pk_dst Z.(bl_dst + amount) cnt_dst in
    balances.(dst_pos) <- Z.(bl_dst + amount) ;
    let tree = Merkle.update_tree ~input_length:2 tree dst_pos new_leaf in
    let root = Merkle.root tree in
    let _, new_path = Merkle.proof_path dst_pos tree in
    ({init_path; root; new_path}, tree)
  in
  ( {
      src_pos;
      dst_pos;
      amount;
      fee;
      msg;
      src = {pk = pk_src; balance = bl_src; cnt = Z.(succ cnt_src)};
      dst = {pk = pk_dst; balance = bl_dst; cnt = cnt_dst};
      src_proof;
      dst_proof;
      signature;
    },
    {state with tree; balances; counters} )

(* TODO this should make a block *)
let generate_transactions n state =
  let rec aux n state acc_tx =
    if n <= 0 then (List.rev acc_tx, state)
    else
      let tx, state = generate_transaction state in
      aux (n - 1) state (tx :: acc_tx)
  in
  aux n state []

module Benchmark (L : LIB) = struct
  module Hash = HashPV.V (L)
  module Plompiler_Curve = JubjubEdwards (L)
  module Schnorr = SchnorrPV.V (L)
  module Merkle = MerklePV.V (L)

  open Helpers.Utils (L)

  open L

  let monadic_compress values =
    assert (compression_bound values < S.order) ;
    foldM
      (fun acc (v, v_bound) -> Num.add ~ql:(S.of_z v_bound) acc v)
      (fst @@ List.hd values)
      (List.tl values)

  open Encodings

  type curve_t_u = (scalar * scalar) repr

  let curve_base_to_s c = S.of_z @@ Curve.Base.to_z c

  let curve_base_of_s c = Curve.Base.of_z @@ S.to_z c

  let curve_scalar_to_s c = S.of_z @@ Curve.Scalar.to_z c

  let curve_scalar_of_s c = Curve.Scalar.of_z @@ S.to_z c

  let s_of_int x = S.of_z (Z.of_int x)

  let s_to_int s = Z.to_int (S.to_z s)

  type curve_base_t_u = scalar repr

  type curve_scalar_t_u = scalar repr

  let curve_scalar_t_encoding : (Curve.Scalar.t, curve_scalar_t_u, _) encoding =
    conv
      (fun r -> r)
      (fun r -> r)
      curve_scalar_to_s
      curve_scalar_of_s
      scalar_encoding

  let curve_t_encoding : (Curve.t, curve_t_u, _) encoding =
    with_implicit_bool_check Plompiler_Curve.is_on_curve
    @@ conv
         (fun r -> of_pair r)
         (fun (u, v) -> pair u v)
         (fun c ->
           ( curve_base_to_s @@ Curve.get_u_coordinate c,
             curve_base_to_s @@ Curve.get_v_coordinate c ))
         (fun (u, v) ->
           Curve.from_coordinates_exn
             ~u:(curve_base_of_s u)
             ~v:(curve_base_of_s v))
         (obj2_encoding scalar_encoding scalar_encoding)

  type account_u = {
    pk : Schnorr.pk repr;
    balance : scalar repr;
    cnt : scalar repr;
  }

  let account_encoding : (account, account_u, _) encoding =
    conv
      (fun {pk; balance; cnt} -> (pk, (balance, cnt)))
      (fun (pk, (balance, cnt)) -> {pk; balance; cnt})
      (fun (acc : account) -> (acc.pk, (S.of_z acc.balance, S.of_z acc.cnt)))
      (fun (pk, (balance, cnt)) ->
        {pk; balance = S.to_z balance; cnt = S.to_z cnt})
      (obj3_encoding Schnorr.pk_encoding scalar_encoding scalar_encoding)

  type proof_u = {
    init_path : Merkle.path;
    root : scalar repr;
    new_path : Merkle.path;
  }

  let proof_encoding : (proof, proof_u, _) encoding =
    conv
      (fun {init_path; root; new_path} -> (init_path, (root, new_path)))
      (fun (init_path, (root, new_path)) -> {init_path; root; new_path})
      (fun ({init_path; root; new_path} : proof) ->
        (init_path, (root, new_path)))
      (fun (init_path, (root, new_path)) -> {init_path; root; new_path})
      (obj3_encoding Merkle.path_encoding scalar_encoding Merkle.path_encoding)

  type tx_u = {
    src_pos : scalar repr;
    dst_pos : scalar repr;
    amount : scalar repr;
    fee : scalar repr;
    msg : curve_base_t_u;
    src : account_u;
    dst : account_u;
    src_proof : proof_u;
    dst_proof : proof_u;
    signature : Schnorr.signature;
  }

  let tx_encoding : (tx, tx_u, _) encoding =
    conv
      (fun tx ->
        ( tx.src_pos,
          ( tx.dst_pos,
            ( tx.amount,
              ( tx.fee,
                ( tx.msg,
                  ( tx.src,
                    (tx.dst, (tx.src_proof, (tx.dst_proof, tx.signature))) ) )
              ) ) ) ))
      (fun ( src_pos,
             ( dst_pos,
               ( amount,
                 (fee, (msg, (src, (dst, (src_proof, (dst_proof, signature))))))
               ) ) ) ->
        {
          src_pos;
          dst_pos;
          amount;
          fee;
          msg;
          src;
          dst;
          src_proof;
          dst_proof;
          signature;
        })
      (fun (tx : tx) ->
        ( s_of_int tx.src_pos,
          ( s_of_int tx.dst_pos,
            ( S.of_z tx.amount,
              ( S.of_z tx.fee,
                ( tx.msg,
                  ( tx.src,
                    (tx.dst, (tx.src_proof, (tx.dst_proof, tx.signature))) ) )
              ) ) ) ))
      (fun ( src_pos,
             ( dst_pos,
               ( amount,
                 (fee, (msg, (src, (dst, (src_proof, (dst_proof, signature))))))
               ) ) ) ->
        {
          src_pos = s_to_int src_pos;
          dst_pos = s_to_int dst_pos;
          amount = S.to_z amount;
          fee = S.to_z fee;
          msg;
          src;
          dst;
          src_proof;
          dst_proof;
          signature;
        })
      (obj10_encoding
         scalar_encoding
         scalar_encoding
         scalar_encoding
         scalar_encoding
         scalar_encoding
         account_encoding
         account_encoding
         proof_encoding
         proof_encoding
         Schnorr.signature_encoding)

  let check_merkle_proof pk bl cnt path root =
    let pk_x, _pk_y = of_pair pk in
    let* compressed =
      monadic_compress [(bl, max_balance); (cnt, max_counter)]
    in
    (* Building leaf *)
    let* leaf = Hash.digest ~input_length:2 (to_list [pk_x; compressed]) in
    (* Building merkle proof *)
    Merkle.merkle_proof path leaf root

  let make_rollup init_tree tx_list () =
    let* generator =
      Plompiler_Curve.(input_point @@ affine_to_point Curve.one)
    in
    let* init_root = Input.scalar init_tree |> input in
    let* inputs_rollup =
      mapM
        (fun (tx : tx) ->
          let i = tx_encoding.input tx in
          input i)
        tx_list
    in
    let* _root =
      foldM
        (fun fold_root ptx ->
          let tx = tx_encoding.decode ptx in
          (* ---------- Check the init src leaf is in the init tree ---------- *)
          let* init_cnt_src = Num.add_constant S.mone tx.src.cnt in
          let* b_init_src_leaf =
            check_merkle_proof
              tx.src.pk
              tx.src.balance
              init_cnt_src
              tx.src_proof.init_path
              fold_root
          in
          (* ---------- Check the new src leaf is in the tmp tree ---------- *)
          (* Building new src leaf *)
          let* new_bl_src =
            Num.add_list
              ~coeffs:S.[one; mone; mone]
              (to_list [tx.src.balance; tx.amount; tx.fee])
          in
          let* b_new_src_leaf =
            check_merkle_proof
              tx.src.pk
              new_bl_src
              tx.src.cnt
              tx.src_proof.new_path
              tx.src_proof.root
          in
          (* ---------- Check the init dst leaf is in the init tree ---------- *)
          let* b_init_dst_leaf =
            check_merkle_proof
              tx.dst.pk
              tx.dst.balance
              tx.dst.cnt
              tx.dst_proof.init_path
              tx.src_proof.root
          in
          (* ---------- Check the new dst leaf is in the new tree ---------- *)
          (* Building new dst leaf *)
          let* new_bl_dst = Num.add tx.dst.balance tx.amount in
          let* b_new_dst_leaf =
            check_merkle_proof
              tx.dst.pk
              new_bl_dst
              tx.dst.cnt
              tx.dst_proof.new_path
              tx.dst_proof.root
          in

          Bool.assert_true b_init_src_leaf
          >* (* ---------- Batching merkle proof checks ---------- *)
          Num.assert_custom
            ~qm:S.one
            ~qc:S.mone
            (scalar_of_bool b_init_src_leaf)
            (scalar_of_bool b_new_src_leaf)
            (scalar_of_bool b_new_src_leaf)
          >* Num.assert_custom
               ~qm:S.one
               ~qc:S.mone
               (scalar_of_bool b_init_dst_leaf)
               (scalar_of_bool b_new_dst_leaf)
               (scalar_of_bool b_new_dst_leaf)
          (* ---------------------- Check balances ---------------------- *)
          (* These checks require that (max_amount + max_fee) and max_balance
             be strictly smaller than BLS.order/2 in order to avoid overflows.
             That way, if fee is in [0, max_fee), amount is in [0, max_amount)
             and we have the precondition that bl_src is in [0, max_balance),
             then new_bl_src = bl_src - (fee + amount) cannot fall back
             into [0, max_balance) unless we really have bl_src >= (fee + amount).
             The last check on new_bl_dst is only necessary to preserve the
             invariant that balances are in [0, max_balance), but it is not
             necessary in terms of "conservation of money". *)
          >* with_bool_check (Num.is_upper_bounded_unsafe ~bound:max_fee tx.fee)
          >* with_bool_check
               (Num.is_upper_bounded_unsafe ~bound:max_amount tx.amount)
          >* with_bool_check
               (Num.is_upper_bounded_unsafe ~bound:max_counter tx.src.cnt)
          >* with_bool_check
               (Num.is_upper_bounded_unsafe ~bound:max_balance new_bl_src)
          >* with_bool_check
               (Num.is_upper_bounded_unsafe ~bound:max_balance new_bl_dst)
          >*
          (* Check pk_dst <> gen (used as dummy pk to note closed accounts) *)
          (* Checking that the x coordinates of pk_dst and generator are
             different is enough as we do not want both points with the
             generator x coordinate to be used as public key. *)
          let x_pk = of_pair tx.dst.pk |> fst in
          let x_g = of_pair generator |> fst in
          let* diff = Num.add x_pk ~qr:S.mone x_g in
          Num.assert_nonzero diff
          >* (* Building signature message *)
          let* compressed =
            monadic_compress
              [
                (tx.src_pos, max_nb_accounts);
                (tx.dst_pos, max_nb_accounts);
                (tx.fee, max_fee);
                (tx.amount, max_amount);
                (init_cnt_src, max_counter);
              ]
          in
          let* msg = Hash.digest ~input_length:1 (to_list [compressed]) in
          (* Building signature proof *)
          let* b_schnorr =
            Schnorr.verify
              ~compressed:true
              ~g:generator
              ~msg
              ~pk:tx.src.pk
              ~signature:tx.signature
              ()
          in
          Bool.assert_true b_schnorr >* ret tx.dst_proof.root)
        init_root
        inputs_rollup
    in
    ret unit
end

let benchmark () =
  let module Main = Plonk.Main_protocol in
  let open Helpers in
  let circuit_info = ref None in
  let init_state = init_state () in
  (* nb_txs is the number of transactions in one circuit *)
  let nb_txs = 1 in
  (* nb_proofs is the number of proofs to aggregate *)
  let nb_proofs = 1 in
  let tx_state_lists =
    let rec_state = ref init_state in
    print_endline "" ;
    Time.time
      (Printf.sprintf
         "Generate %d proofs comprising %d transactions each."
         nb_proofs
         nb_txs)
    @@ fun () ->
    List.init nb_proofs (fun _ ->
        let start_state = !rec_state in
        let txs, end_state = generate_transactions nb_txs start_state in
        rec_state := end_state ;
        (txs, start_state))
  in
  let witness_list, cs_opt =
    Time.time "Build witness" @@ fun () ->
    List.fold_left
      (fun (acc_inputs, _cs) (txs, state) ->
        let cs, circuit =
          match !circuit_info with
          | None ->
              let module E1 = Benchmark (LibCircuit) in
              let circuit = E1.make_rollup (Merkle.root state.tree) txs in
              let cs = LibCircuit.(get_cs ~optimize:true (circuit ())) in
              circuit_info := Some (cs, circuit) ;
              (cs, circuit)
          | Some info -> info
        in
        let witness =
          let initial, _ = LibCircuit.(get_inputs (circuit ())) in
          let private_inputs = Solver.solve cs.solver initial in
          private_inputs
        in
        (* We check satisfiability of CS with the witness in case of a wrong erronerous transaction *)
        assert (CS.sat cs witness) ;
        let inputs = Main.{witness; input_commitments = []} in
        (inputs :: acc_inputs, Some cs))
      ([], None)
      tx_state_lists
  in
  let cs = Option.get cs_opt in
  let msg =
    Printf.sprintf
      "Transfer circuit for %d transactions on a tree of depth %d.\n\
       Constraints: %i\n"
      nb_txs
      depth_tree
      Array.(concat cs.cs |> length)
  in
  Printf.fprintf !Helpers.output_buffer "%s" msg ;
  print_endline msg ;
  let circuit =
    Time.time "PlonK convertion" @@ fun () -> Plonk.Circuit.to_plonk cs
  in
  let circuit_map = Plonk.SMap.singleton "" (circuit, nb_proofs) in
  let inputs = Plonk.SMap.singleton "" witness_list in
  let pp_prv, pp_vrf =
    Time.time "Setup" @@ fun () ->
    Main.setup ~zero_knowledge:false circuit_map ~srs:Helpers.srs
  in
  let proof = Time.time "Prove" @@ fun () -> Main.prove pp_prv ~inputs in
  let v =
    let verifier_inputs = Main.to_verifier_inputs pp_prv inputs in
    Time.time "Verify" @@ fun () ->
    Main.verify pp_vrf ~inputs:verifier_inputs proof
  in
  assert v

let bench = [Alcotest.test_case "Tx benchmark" `Slow benchmark]
