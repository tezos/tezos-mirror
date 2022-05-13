(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(* Check that each nullifier is not already present in the state and add it.
   Important to avoid spending the same input twice in a transaction. *)
let rec check_and_update_nullifiers ctxt state inputs =
  match inputs with
  | [] -> return (ctxt, Some state)
  | input :: inputs -> (
      Sapling_storage.nullifiers_mem ctxt state Sapling.UTXO.(input.nf)
      >>=? function
      | ctxt, true -> return (ctxt, None)
      | ctxt, false ->
          let state =
            Sapling_storage.nullifiers_add state Sapling.UTXO.(input.nf)
          in
          check_and_update_nullifiers ctxt state inputs)

let verify_update :
    Raw_context.t ->
    Sapling_storage.state ->
    Sapling_repr.transaction ->
    string ->
    (Raw_context.t * (Int64.t * Sapling_storage.state) option) tzresult Lwt.t =
 fun ctxt state transaction key ->
  (* Check the transaction *)
  (* To avoid overflowing the balance, the number of inputs and outputs must be
     bounded.
     Ciphertexts' memo_size must match the state's memo_size.
     These constraints are already enforced at the encoding level. *)
  assert (Compare.Int.(List.compare_length_with transaction.inputs 5208 <= 0)) ;
  assert (Compare.Int.(List.compare_length_with transaction.outputs 2019 <= 0)) ;
  let pass =
    List.for_all
      (fun output ->
        Compare.Int.(
          Sapling.Ciphertext.get_memo_size Sapling.UTXO.(output.ciphertext)
          = state.memo_size))
      transaction.outputs
  in
  if not pass then return (ctxt, None)
  else
    (* Check the root is a recent state *)
    Sapling_storage.root_mem ctxt state transaction.root >>=? fun pass ->
    if not pass then return (ctxt, None)
    else
      check_and_update_nullifiers ctxt state transaction.inputs >|=? function
      | ctxt, None -> (ctxt, None)
      | ctxt, Some state ->
          Sapling.Verification.with_verification_ctx (fun vctx ->
              let pass =
                (* Check all the output ZK proofs *)
                List.for_all
                  (fun output -> Sapling.Verification.check_output vctx output)
                  transaction.outputs
              in
              if not pass then (ctxt, None)
              else
                let pass =
                  (* Check all the input Zk proofs and signatures *)
                  List.for_all
                    (fun input ->
                      Sapling.Verification.check_spend
                        vctx
                        input
                        transaction.root
                        key)
                    transaction.inputs
                in
                if not pass then (ctxt, None)
                else
                  let pass =
                    (* Check the signature and balance of the whole transaction *)
                    Sapling.Verification.final_check vctx transaction key
                  in
                  if not pass then (ctxt, None)
                  else
                    (* update tree *)
                    let list_to_add =
                      List.map
                        (fun output ->
                          Sapling.UTXO.(output.cm, output.ciphertext))
                        transaction.outputs
                    in
                    let state = Sapling_storage.add state list_to_add in
                    (ctxt, Some (transaction.balance, state)))
