(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Os : sig
  type t = Cos | Debian

  (** [default] is [Cos]. *)
  val default : t

  val of_string_exn : string -> t

  val of_string_opt : string -> t option

  val to_string : t -> string

  val typ : t Clap.typ

  val encoding : t Data_encoding.encoding
end
