(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module Network : sig
  val user_activated_upgrades :
    ([`GET], unit, unit, unit, unit, User_activated.upgrades) RPC_service.t

  val user_activated_protocol_overrides :
    ( [`GET],
      unit,
      unit,
      unit,
      unit,
      User_activated.protocol_overrides )
    RPC_service.t
end

val history_mode :
  ([`GET], unit, unit, unit, unit, History_mode.t, error trace) RPC_service.raw

module Logging : sig
  val configure :
    ( [`PUT],
      unit,
      unit,
      unit,
      Tezos_stdlib_unix.Internal_event_unix.Configuration.t,
      unit )
    RPC_service.service
end

val user_activated_upgrades :
  #RPC_context.simple -> User_activated.upgrades tzresult Lwt.t
