(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_rpc.Context

module type REGISTERED = sig
  type t = {
    registered_backend : Profiler.view list;
    backends : (string * Profiler.view) list;
  }

  val encoding : t Data_encoding.t

  module S : sig
    val registered :
      ([`GET], unit, unit, unit, unit, t) Tezos_rpc.Service.service
  end

  val registered : #simple -> t tzresult Lwt.t
end

val registered_module : unit -> (module REGISTERED)
