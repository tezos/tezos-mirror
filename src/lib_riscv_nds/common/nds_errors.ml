(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type invalid_argument_error =
  | Key_not_found
  | Key_too_long
  | Offset_too_large
  | Database_index_out_of_bounds
  | Registry_resize_too_large

type error += Invalid_argument of invalid_argument_error

let invalid_argument_error_encoding =
  let open Data_encoding in
  string_enum
    [
      ("key_not_found", Key_not_found);
      ("key_too_long", Key_too_long);
      ("offset_too_large", Offset_too_large);
      ("database_index_out_of_bounds", Database_index_out_of_bounds);
      ("registry_resize_too_large", Registry_resize_too_large);
    ]

let string_of_invalid_argument_error = function
  | Key_not_found -> "key not found"
  | Key_too_long -> "key too long"
  | Offset_too_large -> "offset too large"
  | Database_index_out_of_bounds -> "database index out of bounds"
  | Registry_resize_too_large -> "registry resize too large"

let () =
  register_error_kind
    `Permanent
    ~id:"riscv_nds.invalid_argument"
    ~title:"Invalid argument for durable storage"
    ~description:"An invalid argument was provided to the durable storage."
    ~pp:(fun ppf err ->
      Format.fprintf
        ppf
        "Invalid argument for durable storage: %s"
        (string_of_invalid_argument_error err))
    Data_encoding.(obj1 (req "error" invalid_argument_error_encoding))
    (function Invalid_argument e -> Some e | _ -> None)
    (fun e -> Invalid_argument e)

type error += Proof_deserialisation_error of string

let () =
  register_error_kind
    `Permanent
    ~id:"riscv_nds.proof_deserialisation_error"
    ~title:"Proof deserialisation error for durable storage"
    ~description:"Failed to deserialise a proof for the durable storage."
    ~pp:(fun ppf msg ->
      Format.fprintf ppf "Proof deserialisation error: %s" msg)
    Data_encoding.(obj1 (req "message" string))
    (function Proof_deserialisation_error msg -> Some msg | _ -> None)
    (fun msg -> Proof_deserialisation_error msg)

type verification_error = Not_found

type error += Verification_error of verification_error

let verification_error_encoding =
  let open Data_encoding in
  conv (function Not_found -> ()) (fun () -> Not_found) (constant "not_found")

let string_of_verification_error = function Not_found -> "not found"

let () =
  register_error_kind
    `Permanent
    ~id:"riscv_nds.verification_error"
    ~title:"Verification error for durable storage"
    ~description:"A verification error occurred in the durable storage."
    ~pp:(fun ppf err ->
      Format.fprintf
        ppf
        "Verification error for durable storage: %s"
        (string_of_verification_error err))
    Data_encoding.(obj1 (req "error" verification_error_encoding))
    (function Verification_error e -> Some e | _ -> None)
    (fun e -> Verification_error e)

type verification_argument_error =
  | Verification_invalid_argument of invalid_argument_error
  | Verification of verification_error

type error += Verification_argument_error of verification_argument_error

let verification_argument_error_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"invalid_argument"
        invalid_argument_error_encoding
        (function
          | Verification_invalid_argument e -> Some e | Verification _ -> None)
        (fun e -> Verification_invalid_argument e);
      case
        (Tag 1)
        ~title:"verification"
        verification_error_encoding
        (function
          | Verification e -> Some e | Verification_invalid_argument _ -> None)
        (fun e -> Verification e);
    ]

let string_of_verification_argument_error = function
  | Verification_invalid_argument e -> string_of_invalid_argument_error e
  | Verification e -> string_of_verification_error e

let () =
  register_error_kind
    `Permanent
    ~id:"riscv_nds.verification_argument_error"
    ~title:"Verification argument error for durable storage"
    ~description:
      "A verification argument error occurred in the durable storage."
    ~pp:(fun ppf err ->
      Format.fprintf
        ppf
        "Verification argument error for durable storage: %s"
        (string_of_verification_argument_error err))
    Data_encoding.(obj1 (req "error" verification_argument_error_encoding))
    (function Verification_argument_error e -> Some e | _ -> None)
    (fun e -> Verification_argument_error e)
