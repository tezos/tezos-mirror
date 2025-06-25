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
  type t = Tezlink_imports.Alpha_context.Contract.t

  let encoding = Tezlink_imports.Alpha_context.Contract.encoding

  let of_implicit c = Tezlink_imports.Alpha_context.Contract.Implicit c

  let of_b58check s =
    Tezlink_imports.Imported_env.wrap_tzresult
    @@ Tezlink_imports.Alpha_context.Contract.of_b58check s
end

module Operation = struct
  module ImportedOperation = Tezlink_imports.Alpha_context.Operation

  type t = {
    source : Signature.V1.public_key_hash;
    counter : Z.t;
    op : Tezlink_imports.Alpha_context.packed_operation;
    raw : bytes;
  }

  let decode raw =
    let open Result_syntax in
    let* op =
      match
        Data_encoding.Binary.of_bytes_opt
          Tezlink_imports.Alpha_context.Operation.encoding
          raw
      with
      | None -> error_with "Can't parse the operation"
      | Some op -> return op
    in
    let from_contents (type kind)
        (contents : kind Tezlink_imports.Alpha_context.contents) : t tzresult =
      match contents with
      | Tezlink_imports.Alpha_context.Manager_operation {source; counter; _} ->
          let* counter =
            convert_using_serialization
              ~name:"counter"
              ~dst:Data_encoding.n
              ~src:
                Tezlink_imports.Alpha_context.Manager_counter.encoding_for_RPCs
              counter
          in
          return ({source; counter; op; raw} : t)
      | _ -> error_with "Not a manager operation"
    in
    let (Operation_data op) = op.protocol_data in
    match op.contents with
    | Single contents -> from_contents contents
    | Cons _ ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/8008
           support operation batches
        *)
        error_with "Unsupported feature: operation batch"

  let encoding : t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun {source; counter; op; raw} -> (source, counter, op, raw))
      (fun (source, counter, op, raw) -> {source; counter; op; raw})
      (tup4
         Signature.V1.Public_key_hash.encoding
         z
         (dynamic_size Tezlink_imports.Alpha_context.Operation.encoding)
         bytes)

  let hash_operation {source = _; counter = _; op; raw = _} =
    let hash = ImportedOperation.hash_packed op in
    let (`Hex hex) = Operation_hash.to_hex hash in
    Ethereum_types.Hash (Ethereum_types.Hex hex)
end

module Tez = struct
  include Tezlink_imports.Alpha_context.Tez

  let of_string_exn str =
    match of_string str with
    | None ->
        raise (Invalid_argument (Printf.sprintf "Invalid tez value: %s" str))
    | Some s -> s
end

module Manager = Tezlink_imports.Imported_protocol.Manager_repr
