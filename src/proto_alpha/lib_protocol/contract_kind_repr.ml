(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type originated_kind =
  | Script of Script_repr.t
  | Native of Script_native_repr.with_storage

let michelson_with_storage_encoding = Script_repr.encoding

let encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"michelson"
        (Tag 0)
        michelson_with_storage_encoding
        (function Script m -> Some m | Native _ -> None)
        (fun m -> Script m);
      case
        ~title:"native"
        (Tag 1)
        Script_native_repr.with_storage_encoding
        (function Native n -> Some n | Script _ -> None)
        (fun n -> Native n);
    ]
