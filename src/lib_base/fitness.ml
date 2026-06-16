(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** A block fitness is an abstract list of byte sequences used by nodes to
    decide whether a newly received block is better than the current head. The
    meaning of these bytes is protocol-dependent and remains opaque outside the
    protocol.

    Fitness comparison is defined as:
    - shorter fitness lists are considered smaller;
    - when lists have identical length, comparison proceeds lexicographically
      over each byte sequence.

    Details about the fitness decoding can be found in
    [bin_signer/handler.ml] *)

type t = Bytes.t list

include Compare.Make (struct
  type nonrec t = t

  let compare_bytes b1 b2 =
    let len1 = Bytes.length b1 in
    let len2 = Bytes.length b2 in
    let c = compare len1 len2 in
    if c <> 0 then c
    else
      let rec compare_byte b1 b2 pos len =
        if pos = len then 0
        else
          let c = compare (Bytes.get b1 pos) (Bytes.get b2 pos) in
          if c <> 0 then c else compare_byte b1 b2 (pos + 1) len
      in
      compare_byte b1 b2 0 len1

  (** Fitness comparison:
      - shortest lists are smaller ;
      - lexicographical order for lists of the same length. *)
  let compare f1 f2 =
    let rec compare_rec f1 f2 =
      match (f1, f2) with
      | [], [] -> 0
      | i1 :: f1, i2 :: f2 ->
          let i = compare_bytes i1 i2 in
          if i = 0 then compare_rec f1 f2 else i
      | _, _ -> assert false
    in
    let len = List.compare_lengths f1 f2 in
    if len = 0 then compare_rec f1 f2 else len
end)

let rec pp fmt = function
  | [] -> ()
  | [f] -> Format.fprintf fmt "%a" Hex.pp (Hex.of_bytes f)
  | f1 :: f -> Format.fprintf fmt "%a::%a" Hex.pp (Hex.of_bytes f1) pp f

let encoding =
  let open Data_encoding in
  def
    "fitness"
    ~title:"Block fitness"
    ~description:
      "The fitness, or score, of a block, that allow the Tezos to decide which \
       chain is the best. A fitness value is a list of byte sequences. They \
       are compared as follows: shortest lists are smaller; lists of the same \
       length are compared according to the lexicographical order."
  @@ splitted ~json:(list bytes) ~binary:(list (def "fitness.elem" bytes))

let to_bytes v = Data_encoding.Binary.to_bytes_exn encoding v

let of_bytes b = Data_encoding.Binary.of_bytes_opt encoding b
