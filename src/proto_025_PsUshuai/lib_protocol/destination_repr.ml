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

type t =
  | Contract of Contract_repr.t
  | Sc_rollup of Sc_rollup_repr.t
  | Zk_rollup of Zk_rollup_repr.t

(* If you add more cases to this type, please update the
   [test_compare_destination] test in
   [test/unit/test_destination_repr.ml] to ensure that the compare
   function keeps its expected behavior to distinguish between
   implicit accounts and smart contracts. *)

include Compare.Make (struct
  type nonrec t = t

  let compare l1 l2 =
    match (l1, l2) with
    | Contract k1, Contract k2 -> Contract_repr.compare k1 k2
    | Sc_rollup k1, Sc_rollup k2 -> Sc_rollup_repr.Address.compare k1 k2
    | Zk_rollup k1, Zk_rollup k2 -> Zk_rollup_repr.Address.compare k1 k2
    (* This function is used by the Michelson interpreter to compare
       addresses. It is of significant importance to remember that in
       Michelson, address comparison is used to distinguish between
       KT1 and tz1. As a consequence, we want to preserve that [tz1 <
       KT1 < others], which the two following lines ensure. The
       wildcards are therefore here for a reason, and should not be
       modified when new constructors are added to [t]. *)
    | Contract _, _ -> -1
    | _, Contract _ -> 1
    | Sc_rollup _, _ -> -1
    | _, Sc_rollup _ -> 1
end)

let to_b58check = function
  | Contract k -> Contract_repr.to_b58check k
  | Sc_rollup k -> Sc_rollup_repr.Address.to_b58check k
  | Zk_rollup k -> Zk_rollup_repr.Address.to_b58check k

type error += Invalid_destination_b58check of string

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"destination_repr.invalid_b58check"
    ~title:"Destination decoding failed"
    ~description:
      "Failed to read a valid destination from a b58check_encoding data"
    (obj1 (req "input" (string Plain)))
    (function Invalid_destination_b58check x -> Some x | _ -> None)
    (fun x -> Invalid_destination_b58check x)

let of_b58data data =
  let decode_on_none decode wrap = function
    | Some x -> Some x
    | None -> Option.map wrap @@ decode data
  in
  None
  |> decode_on_none Contract_repr.of_b58data (fun c -> Contract c)
  |> decode_on_none Sc_rollup_repr.Address.of_b58data (fun s -> Sc_rollup s)
  |> decode_on_none Zk_rollup_repr.Address.of_b58data (fun z -> Zk_rollup z)

let of_b58check_opt s = Option.bind (Base58.decode s) of_b58data

let of_b58check s =
  let open Result_syntax in
  match of_b58check_opt s with
  | None -> tzfail (Invalid_destination_b58check s)
  | Some dest -> return dest

let encoding =
  let open Data_encoding in
  let case = function
    | Tag tag ->
        (* The tag was used by old variant. It have been removed in
           protocol proposal O, it can be unblocked in the future. *)
        let tx_rollup_address_reserved_tag = 2 in
        assert (Compare.Int.(tag <> tx_rollup_address_reserved_tag)) ;
        case (Tag tag)
    | _ as c -> case c
  in
  def
    "transaction_destination"
    ~title:"A destination of a transaction"
    ~description:
      "A destination notation compatible with the contract notation as given \
       to an RPC or inside scripts. Can be a base58 implicit contract hash, a \
       base58 originated contract hash, a base58 originated transaction \
       rollup, or a base58 originated smart rollup."
  @@ splitted
       ~binary:
         (union
            ~tag_size:`Uint8
            (Contract_repr.cases
               (function Contract x -> Some x | _ -> None)
               (fun x -> Contract x)
            @ [
                case
                  (Tag 3)
                  (Fixed.add_padding Sc_rollup_repr.Address.encoding 1)
                  ~title:"Smart_rollup"
                  (function Sc_rollup k -> Some k | _ -> None)
                  (fun k -> Sc_rollup k);
                case
                  (Tag 4)
                  (Fixed.add_padding Zk_rollup_repr.Address.encoding 1)
                  ~title:"Zk_rollup"
                  (function Zk_rollup k -> Some k | _ -> None)
                  (fun k -> Zk_rollup k);
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
            (string Plain))

let pp : Format.formatter -> t -> unit =
 fun fmt -> function
  | Contract k -> Contract_repr.pp fmt k
  | Sc_rollup k -> Sc_rollup_repr.pp fmt k
  | Zk_rollup k -> Zk_rollup_repr.Address.pp fmt k

let in_memory_size =
  let open Cache_memory_helpers in
  function
  | Contract k -> h1w +! Contract_repr.in_memory_size k
  | Sc_rollup k -> h1w +! Sc_rollup_repr.in_memory_size k
  | Zk_rollup k -> h1w +! Zk_rollup_repr.in_memory_size k

let rpc_arg =
  let construct = to_b58check in
  let destruct hash =
    Result.map_error (fun _ -> "Cannot parse destination id") (of_b58check hash)
  in
  RPC_arg.make
    ~descr:"A destination identifier encoded in b58check."
    ~name:"destination_id"
    ~construct
    ~destruct
    ()

module Index = struct
  type nonrec t = t

  let path_length = 1

  let to_path c l =
    let raw_key = Data_encoding.Binary.to_bytes_exn encoding c in
    let (`Hex key) = Hex.of_bytes raw_key in
    key :: l

  let of_path = function
    | [key] ->
        Option.bind
          (Hex.to_bytes (`Hex key))
          (Data_encoding.Binary.of_bytes_opt encoding)
    | _ -> None

  let rpc_arg = rpc_arg

  let encoding = encoding

  let compare = compare
end
