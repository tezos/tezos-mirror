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
end
