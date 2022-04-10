(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxheadalpha.com>                    *)
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

type t = Contract of Contract_repr.t | Tx_rollup of Tx_rollup_repr.t
(* If you add more cases to this type, please update the
   [test_compare_destination] test in
   [test/unit/test_destinatino_repr.ml] to ensure that the compare
   function keeps its expected behavior to distinguish between
   implicit accounts and smart contracts. *)

include Compare.Make (struct
  type nonrec t = t

  let compare l1 l2 =
    match (l1, l2) with
    | (Contract k1, Contract k2) -> Contract_repr.compare k1 k2
    | (Tx_rollup k1, Tx_rollup k2) -> Tx_rollup_repr.compare k1 k2
    (* This function is used by the Michelson interpreter to compare
       addresses. It is of significant importance to remember that in
       Michelson, address comparison is used to distinguish between
       KT1 and tz1. As a consequence, we want to preserve that [tz1 <
       KT1 < others], which the two following lines ensure. The
       wildcards are therefore here for a reason, and should not be
       modified when new constructors are added to [t]. *)
    | (Contract _, _) -> -1
    | (_, Contract _) -> 1
end)

let to_b58check = function
  | Contract k -> Contract_repr.to_b58check k
  | Tx_rollup k -> Tx_rollup_repr.to_b58check k

type error += Invalid_destination_b58check of string

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"destination_repr.invalid_b58check"
    ~title:"Destination decoding failed"
    ~description:
      "Failed to read a valid destination from a b58check_encoding data"
    (obj1 (req "input" string))
    (function Invalid_destination_b58check x -> Some x | _ -> None)
    (fun x -> Invalid_destination_b58check x)

let of_b58check s =
  match Contract_repr.of_b58check s with
  | Ok s -> Ok (Contract s)
  | Error _ -> (
      match Tx_rollup_repr.of_b58check s with
      | Ok s -> Ok (Tx_rollup s)
      | Error _ -> error (Invalid_destination_b58check s))

let encoding =
  let open Data_encoding in
  def
    "transaction_destination"
    ~title:"A destination of a transaction"
    ~description:
      "A destination notation compatible with the contract notation as given \
       to an RPC or inside scripts. Can be a base58 implicit contract hash, a \
       base58 originated contract hash, or a base58 originated transaction \
       rollup."
  @@ splitted
       ~binary:
         (union
            ~tag_size:`Uint8
            (Contract_repr.cases
               (function Contract x -> Some x | _ -> None)
               (fun x -> Contract x)
            @ [
                case
                  (Tag 2)
                  (Fixed.add_padding Tx_rollup_repr.encoding 1)
                  ~title:"Tx_rollup"
                  (function Tx_rollup k -> Some k | _ -> None)
                  (fun k -> Tx_rollup k);
              ]))
       ~json:
         (conv
            to_b58check
            (fun s ->
              match of_b58check s with
              | Ok s -> s
              | Error _ ->
                  Data_encoding.Json.cannot_destruct
                    "Invalid destination notation.")
            string)

let pp : Format.formatter -> t -> unit =
 fun fmt -> function
  | Contract k -> Contract_repr.pp fmt k
  | Tx_rollup k -> Tx_rollup_repr.pp fmt k

let in_memory_size =
  let open Cache_memory_helpers in
  function
  | Contract k -> h1w +! Contract_repr.in_memory_size k
  | Tx_rollup k -> h1w +! Tx_rollup_repr.in_memory_size k
