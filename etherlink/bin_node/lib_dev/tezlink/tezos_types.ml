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

  let implicit_encoding =
    Tezlink_imports.Alpha_context.Contract.implicit_encoding

  let of_implicit c = Tezlink_imports.Alpha_context.Contract.Implicit c

  let of_b58check s =
    Tezlink_imports.Imported_env.wrap_tzresult
    @@ Tezlink_imports.Alpha_context.Contract.of_b58check s

  let of_hex contract =
    let bytes = Hex.to_bytes_exn (`Hex contract) in
    Data_encoding.Binary.of_bytes_opt implicit_encoding bytes
end

module Operation = struct
  module ImportedOperation = Tezlink_imports.Alpha_context.Operation

  type t = {
    source : Signature.public_key_hash;
    first_counter : Z.t;
    length : int;
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
    let source_and_counter_from_contents (type kind)
        (contents : kind Tezlink_imports.Alpha_context.contents) :
        (Signature.public_key_hash * Z.t) tzresult =
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
          return (source, counter)
      | _ -> error_with "Not a manager operation"
    in
    let source_and_first_counter_from_contents_list (type kind)
        (contents_list : kind Tezlink_imports.Alpha_context.contents_list) :
        (Signature.public_key_hash * Z.t) tzresult =
      match contents_list with
      | Single contents -> source_and_counter_from_contents contents
      | Cons (first_contents, _) ->
          source_and_counter_from_contents first_contents
    in
    let rec contents_list_length :
        type kind.
        kind Tezlink_imports.Alpha_context.contents_list -> int -> int =
     fun contents_list acc ->
      match contents_list with
      | Single _ -> 1 + acc
      | Cons (_, remaining) -> contents_list_length remaining (1 + acc)
    in
    let (Operation_data op_data) = op.protocol_data in
    let l = op_data.contents in
    let* source, first_counter =
      source_and_first_counter_from_contents_list l
    in
    let length = contents_list_length l 0 in
    return {source; first_counter; length; op; raw}

  let encoding : t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun {source; first_counter; length; op; raw} ->
        (source, first_counter, length, op, raw))
      (fun (source, first_counter, length, op, raw) ->
        {source; first_counter; length; op; raw})
      (tup5
         Signature.Public_key_hash.encoding
         z
         int31
         (dynamic_size Tezlink_imports.Alpha_context.Operation.encoding)
         bytes)

  let hash_operation {source = _; first_counter = _; length = _; op; raw = _} =
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
