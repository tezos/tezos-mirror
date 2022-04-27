(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

module Internal_for_tests = struct
  let originated_sc_rollup nonce =
    let data =
      Data_encoding.Binary.to_bytes_exn Origination_nonce.encoding nonce
    in
    Address.hash_bytes [data]
end

(* 32 *)
let commitment_hash_prefix = "\017\144\021\100" (* scc1(54) *)

module Commitment_hash = struct
  let prefix = "scc1"

  let encoded_size = 54

  module H =
    Blake2B.Make
      (Base58)
      (struct
        let name = "commitment_hash"

        let title = "The hash of a commitment of a smart contract rollup"

        let b58check_prefix = commitment_hash_prefix

        (* defaults to 32 *)
        let size = None
      end)

  include H

  let () = Base58.check_encoded_prefix b58check_encoding prefix encoded_size

  include Path_encoding.Make_hex (H)
end

(* 32 *)
let state_hash_prefix = "\017\144\122\202" (* scs1(54) *)

module State_hash = struct
  let prefix = "scs1"

  let encoded_size = 54

  module H =
    Blake2B.Make
      (Base58)
      (struct
        let name = "state_hash"

        let title = "The hash of the VM state of a smart contract rollup"

        let b58check_prefix = state_hash_prefix

        (* defaults to 32 *)
        let size = None
      end)

  include H

  let () = Base58.check_encoded_prefix b58check_encoding prefix encoded_size

  include Path_encoding.Make_hex (H)
end

type t = Address.t

module Staker = Signature.Public_key_hash

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

let pp = Address.pp

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

module Commitment_hash_index = struct
  include Commitment_hash
end

module Number_of_messages = Bounded.Int32.Make (struct
  let min_int = 0l

  let max_int = 4096l
  (* TODO: check this is reasonable.
     See https://gitlab.com/tezos/tezos/-/issues/2373
  *)
end)

module Number_of_ticks = Bounded.Int32.Make (struct
  let min_int = 0l

  let max_int = Int32.max_int
end)

module Commitment = struct
  type t = {
    compressed_state : State_hash.t;
    inbox_level : Raw_level_repr.t;
    predecessor : Commitment_hash.t;
    number_of_messages : Number_of_messages.t;
    number_of_ticks : Number_of_ticks.t;
  }

  let pp fmt
      {
        compressed_state;
        inbox_level;
        predecessor;
        number_of_messages;
        number_of_ticks;
      } =
    Format.fprintf
      fmt
      "@[<v 2>SCORU Commitment:@ compressed_state: %a@ inbox_level: %a@ \
       predecessor: %a@ number_of_messages: %d@ number_of_ticks: %d@]"
      State_hash.pp
      compressed_state
      Raw_level_repr.pp
      inbox_level
      Commitment_hash.pp
      predecessor
      (Int32.to_int (Number_of_messages.to_int32 number_of_messages))
      (Int32.to_int (Number_of_ticks.to_int32 number_of_ticks))

  let encoding =
    let open Data_encoding in
    conv
      (fun {
             compressed_state;
             inbox_level;
             predecessor;
             number_of_messages;
             number_of_ticks;
           } ->
        ( compressed_state,
          inbox_level,
          predecessor,
          number_of_messages,
          number_of_ticks ))
      (fun ( compressed_state,
             inbox_level,
             predecessor,
             number_of_messages,
             number_of_ticks ) ->
        {
          compressed_state;
          inbox_level;
          predecessor;
          number_of_messages;
          number_of_ticks;
        })
      (obj5
         (req "compressed_state" State_hash.encoding)
         (req "inbox_level" Raw_level_repr.encoding)
         (req "predecessor" Commitment_hash.encoding)
         (req "number_of_messages" Number_of_messages.encoding)
         (req "number_of_ticks" Number_of_ticks.encoding))

  let hash commitment =
    let commitment_bytes =
      Data_encoding.Binary.to_bytes_exn encoding commitment
    in
    Commitment_hash.hash_bytes [commitment_bytes]
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

  let equal x y = match (x, y) with (Example_arith, Example_arith) -> true

  let pp fmt kind =
    match kind with Example_arith -> Format.fprintf fmt "Example_arith"
end
