(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori     <contact@functori.com>          *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

include
  Blake2B.Make
    (Base58)
    (struct
      let name = "injector_operation_hash"

      let title = "An identifier (hash) for an operation in the injector"

      let b58check_prefix = "\064\007\206" (* iop(53) *)

      let size = None
    end)

let () = Base58.check_encoded_prefix b58check_encoding "iop" 53
