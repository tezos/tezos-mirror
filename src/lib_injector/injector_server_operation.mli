(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Parameters for smart contract calls *)
type parameters = {entrypoint : string; value : string}

(** Operations which can be processed by the injector *)
type t =
  | Transaction of {
      amount : int64;
      destination : string;
      parameters : parameters option;
    }

(** Encoding for operations (used by injector for on-disk persistence) *)
val encoding : t Data_encoding.t

(** Pretty printer (human readable) for operations *)
val pp : Format.formatter -> t -> unit

(** [false] if the injector will accept duplicate such operations. *)
val unique : t -> bool
