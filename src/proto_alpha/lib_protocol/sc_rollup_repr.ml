(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module PVM = struct
  type boot_sector = string

  let boot_sector_encoding = Data_encoding.string

  let boot_sector_of_string s = s
end

module Address = struct
  let prefix = "scr1"

  let encoded_size = 37

  let decoded_prefix = "\001\118\132\217" (* "scr1(37)" decoded from base 58. *)

  module H =
    Blake2B.Make
      (Base58)
      (struct
        let name = "Sc_rollup_hash"

        let title = "A smart contract rollup address"

        let b58check_prefix = decoded_prefix

        let size = Some 20
      end)

  include H

  let () = Base58.check_encoded_prefix b58check_encoding prefix encoded_size

  include Path_encoding.Make_hex (H)

  type error += (* `Permanent *) Error_sc_rollup_address_generation

  let () =
    let open Data_encoding in
    let msg = "Error while generating rollup address" in
    register_error_kind
      `Permanent
      ~id:"rollup.error_smart_contract_rollup_address_generation"
      ~title:msg
      ~pp:(fun ppf () -> Format.fprintf ppf "%s" msg)
      ~description:msg
      unit
      (function Error_sc_rollup_address_generation -> Some () | _ -> None)
      (fun () -> Error_sc_rollup_address_generation)

  let from_nonce nonce =
    Data_encoding.Binary.to_bytes_opt Origination_nonce.encoding nonce
    |> function
    | None -> error Error_sc_rollup_address_generation
    | Some nonce -> ok @@ hash_bytes [nonce]
end

type t = Address.t

let description =
  "A smart contract rollup is identified by a base58 address starting with "
  ^ Address.prefix

type error += (* `Permanent *) Invalid_sc_rollup_address of string

let error_description =
  Format.sprintf
    "A smart contract rollup address must be a valid hash starting with '%s'."
    Address.prefix

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"rollup.invalid_smart_contract_rollup_address"
    ~title:"Invalid smart contract rollup address"
    ~pp:(fun ppf x ->
      Format.fprintf ppf "Invalid smart contract rollup address %S" x)
    ~description:error_description
    (obj1 (req "address" string))
    (function Invalid_sc_rollup_address loc -> Some loc | _ -> None)
    (fun loc -> Invalid_sc_rollup_address loc)

let of_b58check s =
  match Base58.decode s with
  | Some (Address.Data hash) -> ok hash
  | _ -> Error (Format.sprintf "Invalid_sc_rollup_address %s" s)

let encoding =
  let open Data_encoding in
  def
    "rollup_address"
    ~title:"A smart contract rollup address"
    ~description
    (conv_with_guard Address.to_b58check of_b58check string)

let rpc_arg =
  let construct = Address.to_b58check in
  let destruct hash =
    Result.map_error (fun _ -> error_description) (of_b58check hash)
  in
  RPC_arg.make
    ~descr:"A smart contract rollup address."
    ~name:"sc_rollup_address"
    ~construct
    ~destruct
    ()

module Index = struct
  type t = Address.t

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

  let compare = Address.compare
end

module Kind = struct
  (*

      Each time we add a data constructor to [t], we also need:
      - to extend [Sc_rollups.all] with this new constructor ;
      - to update [Sc_rollups.kind_of_string] and [encoding].

  *)
  type t = Example_arith

  let example_arith_case =
    Data_encoding.(
      case
        ~title:"Example_arith smart contract rollup kind"
        (Tag 0)
        unit
        (function Example_arith -> Some ())
        (fun () -> Example_arith))

  let encoding = Data_encoding.union ~tag_size:`Uint16 [example_arith_case]
end
