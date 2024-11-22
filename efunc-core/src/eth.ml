(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(*                                                                           *)
(*****************************************************************************)

(* This file is sourced from the efunc library, licensed under the MIT License:
   https://gitlab.com/functori/dev/efunc. *)

module ETypes = Types

module Let = struct
  let ( let> ) = Lwt.bind

  let ( let|> ) p f = Lwt.map f p

  let ( let>? ) p f =
    Lwt.bind p (function Error e -> Lwt.return_error e | Ok x -> f x)

  let ( let|>? ) p f = Lwt.map (Result.map f) p

  let rok = Lwt.return_ok

  let rerr = Lwt.return_error
end

include ETypes
include Let
module Forge = Forge
module Crypto = Crypto
module Evm = Evm
