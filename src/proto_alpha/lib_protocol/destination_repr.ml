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

type t = Contract of Contract_repr.t

include Compare.Make (struct
  type nonrec t = t

  let compare l1 l2 =
    match (l1, l2) with
    | (Contract k1, Contract k2) -> Contract_repr.compare k1 k2
end)

let to_b58check = function Contract k -> Contract_repr.to_b58check k

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
  | Error _ -> error (Invalid_destination_b58check s)

let encoding =
  let open Data_encoding in
  def
    "transaction_destination"
    ~title:"A destination of a transaction"
    ~description:
      "A destination notation compatible with the contract notation as given \
       to an RPC or inside scripts. Can be a base58 implicit contract hash or \
       a base58 originated contract hash."
  @@ splitted
       ~binary:
         (union
            ~tag_size:`Uint8
            (Contract_repr.cases
               (function Contract x -> Some x)
               (fun x -> Contract x)))
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
 fun fmt -> function Contract k -> Contract_repr.pp fmt k

let in_memory_size =
  let open Cache_memory_helpers in
  function Contract k -> h1w +! Contract_repr.in_memory_size k
