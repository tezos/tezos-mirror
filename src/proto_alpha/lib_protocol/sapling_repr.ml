(* The MIT License (MIT)
 *
 * Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE. *)

type transaction = Sapling.UTXO.transaction

let transaction_encoding = Sapling.UTXO.transaction_encoding

(* The three datastructures in the state are all ordered by position, a diff
   contains the elements starting from an offset position up to the most recent
   position. A diff can be applied to a state stored in a context to obtain a
   new state.
   Diffs are used by the Michelson interpreter during the evaluation of smart
   contracts to keep a temporary state that may be discarded.
   Diffs are also returned by an RPC to allow a client to synchonize its own
   state with the chain.
 *)
type diff = {
  commitments : Sapling.Commitment.t list;
  ciphertexts : Sapling.Ciphertext.t list;
  nullifiers : Sapling.Nullifier.t list;
}

let diff_encoding =
  let open Data_encoding in
  conv
    (fun d -> (d.commitments, d.ciphertexts, d.nullifiers))
    (fun (commitments, ciphertexts, nullifiers) ->
      assert (Compare.Int.(List.compare_lengths commitments ciphertexts = 0)) ;
      ( match ciphertexts with
      | [] ->
          ()
      | hd :: rest ->
          let memo_size = Sapling.Ciphertext.get_memo_size hd in
          List.iter
            (fun ct ->
              assert (
                Compare.Int.(Sapling.Ciphertext.get_memo_size ct = memo_size)
              ))
            rest ) ;
      {commitments; ciphertexts; nullifiers})
    (obj3
       (req "commitments" (list Sapling.Commitment.encoding))
       (req "ciphertexts" (list Sapling.Ciphertext.encoding))
       (req "nullifiers" (list Sapling.Nullifier.encoding)))

type memo_size = {memo_size : int}

let memo_size_encoding =
  let open Data_encoding in
  conv
    (fun {memo_size} -> memo_size)
    (fun memo_size -> {memo_size})
    (obj1 (req "memo_size" uint16))
