(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type level = {level : int32; cycle : int32; cycle_position : int32}

type error += Serialization_for_conversion of string * string

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.tezlink.conversion_failure"
    ~title:"Failed to convert a value through serialization"
    ~description:
      "Failed to convert a value to a protocol private datatype through \
       serialization."
    ~pp:(fun ppf (name, error) ->
      Format.fprintf
        ppf
        "Failed to convert a value of type %s to a protocol private datatype \
         through serialization: %s"
        name
        error)
    Data_encoding.(obj2 (req "type_name" string) (req "error_msg" string))
    (function
      | Serialization_for_conversion (name, error) -> Some (name, error)
      | _ -> None)
    (fun (name, error) -> Serialization_for_conversion (name, error))

let convert_using_serialization ~name ~dst ~src value =
  let open Result_syntax in
  let* bytes =
    Data_encoding.Binary.to_bytes src value
    |> Result.map_error_e (fun e ->
           tzfail
           @@ Serialization_for_conversion
                ( name,
                  Format.asprintf "%a" Data_encoding.Binary.pp_write_error e ))
  in
  Data_encoding.Binary.of_bytes dst bytes
  |> Result.map_error_e (fun e ->
         tzfail
         @@ Serialization_for_conversion
              (name, Format.asprintf "%a" Data_encoding.Binary.pp_read_error e))

module Contract = struct
  type t = Tezlink_imports.Imported_context.Contract.t

  let encoding = Tezlink_imports.Imported_context.Contract.encoding

  let implicit_encoding =
    Tezlink_imports.Imported_context.Contract.implicit_encoding

  let of_implicit c = Tezlink_imports.Imported_context.Contract.Implicit c

  let of_originated c = Tezlink_imports.Imported_context.Contract.Originated c

  let of_b58check s =
    Tezlink_imports.Imported_env.wrap_tzresult
    @@ Tezlink_imports.Imported_context.Contract.of_b58check s

  let of_hex contract =
    let bytes = Hex.to_bytes_exn (`Hex contract) in
    Data_encoding.Binary.of_bytes_opt implicit_encoding bytes
end

module Tez = struct
  include Tezlink_imports.Imported_context.Tez

  let of_string_exn str =
    match of_string str with
    | None ->
        raise (Invalid_argument (Printf.sprintf "Invalid tez value: %s" str))
    | Some s -> s

  let to_mutez_z t = t |> to_mutez |> Z.of_int64
end

module Operation = struct
  module ImportedOperation = Tezlink_imports.Imported_context.Operation

  type t = {
    source : Signature.V2.public_key_hash;
    first_counter : Z.t;
    length : int;
    op : Tezlink_imports.Imported_context.packed_operation;
    raw : bytes;
    fee : Tez.t;
    gas_limit : Z.t;
  }

  let counter_to_z counter =
    convert_using_serialization
      ~name:"counter"
      ~dst:Data_encoding.n
      ~src:Tezlink_imports.Imported_context.Manager_counter.encoding_for_RPCs
      counter

  let gas_limit_to_z = Tezlink_imports.Imported_context.Gas.Arith.integral_to_z

  let encoding : t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun {source; first_counter; length; op; raw; fee; gas_limit} ->
        (source, first_counter, length, op, raw, fee, gas_limit))
      (fun (source, first_counter, length, op, raw, fee, gas_limit) ->
        {source; first_counter; length; op; raw; fee; gas_limit})
      (tup7
         Signature.V2.Public_key_hash.encoding
         z
         int31
         (dynamic_size Tezlink_imports.Imported_context.Operation.encoding)
         bytes
         Tez.encoding
         z)

  let hash_operation
      {
        source = _;
        first_counter = _;
        length = _;
        op;
        raw = _;
        fee = _;
        gas_limit = _;
      } =
    let hash = ImportedOperation.hash_packed op in
    let (`Hex hex) = Operation_hash.to_hex hash in
    Ethereum_types.Hash (Ethereum_types.Hex hex)

  let minimum_operation =
    let open Tezlink_imports.Imported_context in
    let open Tezlink_imports.Imported_context.Operation in
    let open Tezlink_imports.Imported_env in
    let shell : Operation.shell_header =
      {branch = Tezos_crypto.Hashed.Block_hash.zero}
    in
    let signature = Some Signature.zero in
    let source = Signature.Public_key_hash.zero in
    let protocol_data : packed_protocol_data =
      Operation_data
        {
          contents =
            Single
              (Manager_operation
                 {
                   source;
                   fee = Tez.zero;
                   counter = Manager_counter.Internal_for_tests.of_int 0;
                   gas_limit = Gas.Arith.zero;
                   storage_limit = Z.zero;
                   operation =
                     Reveal
                       {
                         public_key =
                           Signature.Public_key.of_b58check_exn
                             "edpkuSLWfVU1Vq7Jg9FucPyKmma6otcMHac9zG4oU1KMHSTBpJuGQ2";
                         proof = None;
                       };
                 });
          signature;
        }
    in
    let op : packed_operation = {shell; protocol_data} in
    op

  let minimum_operation_size =
    let raw =
      Data_encoding.Binary.to_bytes_exn
        Tezlink_imports.Imported_context.Operation.encoding
        minimum_operation
    in
    Bytes.length raw
end

module Manager = Tezlink_imports.Imported_protocol.Manager_repr
