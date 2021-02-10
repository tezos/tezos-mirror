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

(* The two data structures in the state are all ordered by position, a diff
   contains the elements starting from an offset position up to the most recent
   position. A diff can be applied to a state stored in a context to obtain a
   new state.
   Diffs are used by the Michelson interpreter during the evaluation of smart
   contracts to keep a temporary state that may be discarded.
   Diffs are also returned by an RPC to allow a client to synchronize its own
   state with the chain.
 *)
type diff = {
  commitments_and_ciphertexts :
    (Sapling.Commitment.t * Sapling.Ciphertext.t) list;
  nullifiers : Sapling.Nullifier.t list;
}

let diff_encoding =
  let open Data_encoding in
  conv
    (fun d -> (d.commitments_and_ciphertexts, d.nullifiers))
    (fun (commitments_and_ciphertexts, nullifiers) ->
      ( match commitments_and_ciphertexts with
      | [] ->
          ()
      | (_cm_hd, ct_hd) :: rest ->
          let memo_size = Sapling.Ciphertext.get_memo_size ct_hd in
          List.iter
            (fun (_cm, ct) ->
              assert (
                Compare.Int.(Sapling.Ciphertext.get_memo_size ct = memo_size)
              ))
            rest ) ;
      {commitments_and_ciphertexts; nullifiers})
    (obj2
       (req
          "commitments_and_ciphertexts"
          (list (tup2 Sapling.Commitment.encoding Sapling.Ciphertext.encoding)))
       (req "nullifiers" (list Sapling.Nullifier.encoding)))

module Memo_size = struct
  type t = int

  let encoding = Data_encoding.uint16

  let equal = Compare.Int.( = )

  let max_uint16 = 0xffff

  let max_uint16_z = Z.of_int max_uint16

  let err =
    Error
      ( "a positive 16-bit integer (between 0 and " ^ string_of_int max_uint16
      ^ ")" )

  let parse_z z =
    if Compare.Z.(Z.zero <= z) && Compare.Z.(z <= max_uint16_z) then
      Ok (Z.to_int z)
    else err

  let unparse_to_z = Z.of_int
end

let transaction_get_memo_size (transaction : Sapling.UTXO.transaction) =
  match transaction.outputs with
  | [] ->
      None
  | {ciphertext; _} :: _ ->
      (* Encoding ensures all ciphertexts have the same memo size. *)
      Some (Sapling.Ciphertext.get_memo_size ciphertext)
