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

type t =
  | Implicit of Signature.Public_key_hash.t
  | Originated of Contract_hash.t

include Compare.Make (struct
  type nonrec t = t

  let compare l1 l2 =
    match (l1, l2) with
    | Implicit pkh1, Implicit pkh2 ->
        Signature.Public_key_hash.compare pkh1 pkh2
    | Originated h1, Originated h2 -> Contract_hash.compare h1 h2
    | Implicit _, Originated _ -> -1
    | Originated _, Implicit _ -> 1
end)

let blake2b_hash_size =
  let open Cache_memory_helpers in
  h1w +! string_size_gen 20

let public_key_hash_in_memory_size =
  let open Cache_memory_helpers in
  header_size +! word_size +! blake2b_hash_size

let in_memory_size =
  let open Cache_memory_helpers in
  function
  | Implicit _ -> h1w +! public_key_hash_in_memory_size
  | Originated _ -> h1w +! blake2b_hash_size

type error += Invalid_contract_notation of string (* `Permanent *)

let to_b58check = function
  | Implicit pbk -> Signature.Public_key_hash.to_b58check pbk
  | Originated h -> Contract_hash.to_b58check h

let implicit_of_b58data : Base58.data -> Signature.public_key_hash option =
  function
  | Ed25519.Public_key_hash.Data h -> Some (Signature.Ed25519 h)
  | Secp256k1.Public_key_hash.Data h -> Some (Signature.Secp256k1 h)
  | P256.Public_key_hash.Data h -> Some (Signature.P256 h)
  | _ -> None

let originated_of_b58data = function
  | Contract_hash.Data h -> Some h
  | _ -> None

let contract_of_b58data data =
  match implicit_of_b58data data with
  | Some pkh -> Some (Implicit pkh)
  | None -> (
      match originated_of_b58data data with
      | Some contract_hash -> Some (Originated contract_hash)
      | None -> None)

let of_b58check_gen ~of_b58data s =
  match Base58.decode s with
  | Some data -> (
      match of_b58data data with
      | Some c -> ok c
      | None -> error (Invalid_contract_notation s))
  | None -> error (Invalid_contract_notation s)

let of_b58check = of_b58check_gen ~of_b58data:contract_of_b58data

let pp ppf = function
  | Implicit pbk -> Signature.Public_key_hash.pp ppf pbk
  | Originated h -> Contract_hash.pp ppf h

let pp_short ppf = function
  | Implicit pbk -> Signature.Public_key_hash.pp_short ppf pbk
  | Originated h -> Contract_hash.pp_short ppf h

let cases is_contract to_contract =
  Data_encoding.
    [
      case
        (Tag 0)
        ~title:"Implicit"
        Signature.Public_key_hash.encoding
        (fun k ->
          match is_contract k with Some (Implicit k) -> Some k | _ -> None)
        (fun k -> to_contract (Implicit k));
      case
        (Tag 1)
        (Fixed.add_padding Contract_hash.encoding 1)
        ~title:"Originated"
        (fun k ->
          match is_contract k with Some (Originated k) -> Some k | _ -> None)
        (fun k -> to_contract (Originated k));
    ]

let encoding =
  let open Data_encoding in
  def
    "contract_id"
    ~title:"A contract handle"
    ~description:
      "A contract notation as given to an RPC or inside scripts. Can be a \
       base58 implicit contract hash or a base58 originated contract hash."
  @@ splitted
       ~binary:(union ~tag_size:`Uint8 @@ cases (fun x -> Some x) (fun x -> x))
       ~json:
         (conv
            to_b58check
            (fun s ->
              match of_b58check s with
              | Ok s -> s
              | Error _ -> Json.cannot_destruct "Invalid contract notation.")
            string)

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"contract.invalid_contract_notation"
    ~title:"Invalid contract notation"
    ~pp:(fun ppf x -> Format.fprintf ppf "Invalid contract notation %S" x)
    ~description:
      "A malformed contract notation was given to an RPC or in a script."
    (obj1 (req "notation" string))
    (function Invalid_contract_notation loc -> Some loc | _ -> None)
    (fun loc -> Invalid_contract_notation loc)

let originated_contract nonce =
  let data =
    Data_encoding.Binary.to_bytes_exn Origination_nonce.encoding nonce
  in
  Originated (Contract_hash.hash_bytes [data])

let originated_contracts
    ~since:
      Origination_nonce.{origination_index = first; operation_hash = first_hash}
    ~until:
      (Origination_nonce.{origination_index = last; operation_hash = last_hash}
      as origination_nonce) =
  assert (Operation_hash.equal first_hash last_hash) ;
  let[@coq_struct "origination_index"] rec contracts acc origination_index =
    if Compare.Int32.(origination_index < first) then acc
    else
      let origination_nonce = {origination_nonce with origination_index} in
      let acc = originated_contract origination_nonce :: acc in
      contracts acc (Int32.pred origination_index)
  in
  contracts [] (Int32.pred last)

let rpc_arg =
  let construct = to_b58check in
  let destruct hash =
    Result.map_error (fun _ -> "Cannot parse contract id") (of_b58check hash)
  in
  RPC_arg.make
    ~descr:"A contract identifier encoded in b58check."
    ~name:"contract_id"
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
