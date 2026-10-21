(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type runtime = Tezos

let runtime_of_string_opt, string_of_runtime, known_runtimes =
  let known_runtimes = [("tezos", Tezos)] in
  let known_runtime_names = List.map (fun (x, y) -> (y, x)) known_runtimes in
  ( (fun n -> List.assoc_opt ~equal:String.equal n known_runtimes),
    (fun n -> Stdlib.List.assoc n known_runtime_names),
    List.map snd known_runtimes )

let pp_runtime fmt runtime =
  Format.pp_print_string fmt (string_of_runtime runtime)

let feature_flag = function
  | Tezos -> Durable_storage_path.Feature_flags.tezos_runtime

let target_sunrise_level_path ~storage_version = function
  | Tezos ->
      Durable_storage_path.michelson_runtime_target_sunrise_level
        ~storage_version

let encode_target_sunrise_level n =
  Bytes.unsafe_to_string
    (Ethereum_types.encode_u256_le (Ethereum_types.Qty (Z.of_int n)))

let runtime_encoding : runtime Data_encoding.t =
  let open Data_encoding in
  (* FIXME: use string_enum instead once we have more than one runtime *)
  conv (fun _ -> ()) (fun () -> Tezos) (constant "tezos")

module Ethereum_runtime = struct
  type address = Ethereum_types.address

  let address_from_string address =
    let open Result_syntax in
    if String.has_prefix ~prefix:"0x" address && String.length address = 42 then
      let+ (`Hex hex) = Misc.normalize_hex address in
      Ethereum_types.Address (Hex hex)
    else tzfail (error_of_fmt "%s is not a valid Ethereum address" address)

  let generate_alias address_str =
    let (`Hex alias) =
      let alias =
        Tezos_crypto.Hacl.Hash.Keccak_256.digest (Bytes.of_string address_str)
      in
      Hex.of_bytes (Bytes.sub alias 0 20)
    in
    Ethereum_types.Address (Hex alias)
end

module Tezos_runtime = struct
  type address = Signature.V3.public_key_hash

  let address_of_string address =
    let open Result_syntax in
    match Tezos_crypto.Signature.V3.Public_key_hash.of_b58check address with
    | Ok tezos_kh -> return tezos_kh
    | Error _ -> tzfail (error_of_fmt "%s is not a valid Tezos address" address)

  type account_info = {
    balance : Tezos_types.Tez.t;
    nonce : int64;
    public_key : Signature.V3.public_key option;
  }

  let decode_account_info bytes =
    let open Result_syntax in
    let info_hex = Hex.of_bytes bytes in

    let fail stage () =
      tzfail
        (error_of_fmt
           "Cannot decode a Tezos account info (stage %s) 0x%s"
           stage
           (Hex.show info_hex))
    in
    try
      match Rlp.decode bytes with
      | Ok (Rlp.List [Value balance; Value nonce; public_key]) ->
          let (Qty balance) = Ethereum_types.decode_number_le balance in
          let balance = Tezos_types.Tez.of_mutez_exn (Z.to_int64 balance) in
          let (Qty nonce) = Ethereum_types.decode_number_le nonce in
          let nonce = Z.to_int64 nonce in
          let* public_key =
            match public_key with
            | Value b when b = Bytes.empty -> return_none
            | Value value ->
                return_some
                  (Signature.V3.Public_key.of_b58check_exn
                     (Bytes.unsafe_to_string value))
            | _ -> fail "public key" ()
          in
          return {balance; nonce; public_key}
      | _ -> fail "list" ()
    with _ -> fail "decoding values" ()

  (** [extract_field f bytes] decodes [bytes] as an [account_info] and
      returns [f info]. Helper for [Durable_storage] resolvers that
      only need to project a single field out of the RLP-encoded info. *)
  let extract_field f bytes =
    let open Result_syntax in
    let* info = decode_account_info bytes in
    return (f info)

  let encode_account_info {balance; nonce; public_key} =
    let padded_32_le_int_bytes z =
      String.of_bytes @@ Ethereum_types.encode_u256_le (Qty z)
    in
    let le_int64_bytes i =
      let b = Bytes.make 8 '\000' in
      Bytes.set_int64_le b 0 i ;
      String.of_bytes b
    in
    let balance =
      balance |> Tezos_types.Tez.to_mutez_z |> padded_32_le_int_bytes
      |> Bytes.of_string
    in
    let nonce = le_int64_bytes nonce |> Bytes.of_string in
    let public_key =
      match public_key with
      | Some public_key ->
          Rlp.Value
            (Bytes.of_string (Signature.V3.Public_key.to_b58check public_key))
      | None -> Value Bytes.empty
    in
    Rlp.encode Rlp.(List [Value balance; Value nonce; public_key])

  let generate_alias address_str =
    let (Tezos_crypto.Hacl.Blake2b.Hash hash) =
      Tezos_crypto.Hacl.Blake2b.direct (Bytes.of_string address_str) 20
    in
    Tezos_types.Contract.of_originated
      (Tezlink_imports.Imported_protocol.Contract_hash.of_bytes_exn hash)
end

(* Kernel IPC for `tezosx_compute_alias`: [input_encoding] mirrors the
   kernel's [ComputeAliasInput] and [result_encoding] its
   [ComputeAliasResult] union ([evm_node_entrypoint.rs]), whose unit tests
   pin the wire bytes. *)
module Compute_alias = struct
  type target = [`Tezos | `Ethereum]

  let input_encoding =
    Data_encoding.(
      obj2 (req "target_runtime" uint8) (req "native_address" string))

  let encode_input ~(target : target) ~native_address =
    let target_runtime = match target with `Tezos -> 0 | `Ethereum -> 1 in
    Data_encoding.Binary.to_bytes_exn
      input_encoding
      (target_runtime, native_address)

  let result_encoding =
    Data_encoding.(
      union
        [
          case
            (Tag 0)
            ~title:"success"
            string
            (function Ok alias -> Some alias | Error _ -> None)
            (fun alias -> Ok alias);
          case
            (Tag 1)
            ~title:"error"
            string
            (function Error msg -> Some msg | Ok _ -> None)
            (fun msg -> Error msg);
        ])

  let decode_result bytes =
    let open Result_syntax in
    match Data_encoding.Binary.of_bytes_opt result_encoding bytes with
    | Some (Ok alias) -> return alias
    | Some (Error msg) ->
        tzfail (error_of_fmt "The kernel could not compute the alias: %s" msg)
    | None -> tzfail (error_of_fmt "Invalid tezosx_compute_alias result")
end

module Durable_storage_path = struct
  module Accounts = struct
    module Tezos = struct
      let info pkh =
        Durable_storage_path.michelson_ledger_root ^ "/"
        ^ Signature.V3.Public_key_hash.to_b58check pkh
        ^ "/info"
    end
  end
end

type address =
  | Ethereum_address of Ethereum_runtime.address
  | Tezos_address of Tezos_runtime.address

let address_of_string address =
  let open Result_syntax in
  match Ethereum_runtime.address_from_string address with
  | Ok address -> return (Ethereum_address address)
  | Error _ -> (
      match Tezos_runtime.address_of_string address with
      | Ok tezos_kh -> return (Tezos_address tezos_kh)
      | Error _ -> tzfail (error_of_fmt "%s is not a valid address" address))
