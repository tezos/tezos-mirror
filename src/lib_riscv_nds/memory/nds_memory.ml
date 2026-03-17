(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2026 Trilitech <contact@trili.tech>               *)
(*                                                                           *)
(*****************************************************************************)

module Api =
  Octez_riscv_nds_memory_api.Octez_riscv_durable_storage_in_memory_api

type error += Invalid_argument of Api.invalid_argument_error

let invalid_argument_error_encoding =
  let open Data_encoding in
  string_enum
    [
      ("key_not_found", Api.Key_not_found);
      ("key_too_long", Api.Key_too_long);
      ("offset_too_large", Api.Offset_too_large);
      ("database_index_out_of_bounds", Api.Database_index_out_of_bounds);
      ("registry_resize_too_large", Api.Registry_resize_too_large);
    ]

let pp_invalid_argument_error fmt = function
  | Api.Key_not_found -> Format.pp_print_string fmt "key not found"
  | Api.Key_too_long -> Format.pp_print_string fmt "key too long"
  | Api.Offset_too_large -> Format.pp_print_string fmt "offset too large"
  | Api.Database_index_out_of_bounds ->
      Format.pp_print_string fmt "database index out of bounds"
  | Api.Registry_resize_too_large ->
      Format.pp_print_string fmt "registry resize too large"

let () =
  register_error_kind
    `Permanent
    ~id:"riscv_nds_memory.invalid_argument"
    ~title:"Invalid argument for in-memory durable storage"
    ~description:
      "An invalid argument was provided to the in-memory durable storage."
    ~pp:(fun ppf err ->
      Format.fprintf
        ppf
        "Invalid argument for in-memory durable storage: %a"
        pp_invalid_argument_error
        err)
    Data_encoding.(obj1 (req "error" invalid_argument_error_encoding))
    (function Invalid_argument e -> Some e | _ -> None)
    (fun e -> Invalid_argument e)

let wrap_error = function
  | Ok x -> Ok x
  | Error e -> Result_syntax.tzfail (Invalid_argument e)

module Registry = struct
  type t = Api.registry

  let size registry = Api.octez_riscv_durable_in_memory_registry_size registry

  let resize registry n =
    Api.octez_riscv_durable_in_memory_registry_resize registry n |> wrap_error

  let copy_database registry ~src ~dst =
    Api.octez_riscv_durable_in_memory_registry_copy registry src dst
    |> wrap_error

  let move_database registry ~src ~dst =
    Api.octez_riscv_durable_in_memory_registry_move registry src dst
    |> wrap_error
end
