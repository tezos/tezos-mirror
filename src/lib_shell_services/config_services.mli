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
    ( [`GET],
      unit,
      unit,
      unit,
      unit,
      User_activated.upgrades )
    Tezos_rpc.Service.t

  val user_activated_protocol_overrides :
    ( [`GET],
      unit,
      unit,
      unit,
      unit,
      User_activated.protocol_overrides )
    Tezos_rpc.Service.t

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5926

     Consider exposing the whole network record via `config/network`
     instead of exposing each field separately. Note that the
     [Config_file.blockchain_network] lives in [lib_node_config], which isn't js_combatible,
     so we can't reference it in [lib_shell_services], which is js_compatible.
     Hence to do this we would need to extract [Config_file.blockchain_network] into a
     separate directory, as done for [lib_dal_config]. *)

  val dal : ([`GET], unit, unit, unit, unit, Dal_config.t) Tezos_rpc.Service.t
end

val history_mode :
  ( [`GET],
    unit,
    unit,
    unit,
    unit,
    History_mode.t,
    error trace )
  Tezos_rpc.Service.raw

module Logging : sig
  val configure :
    ( [`PUT],
      unit,
      unit,
      unit,
      Tezos_base.Internal_event_config.t,
      unit )
    Tezos_rpc.Service.service
end

val user_activated_upgrades :
  #Tezos_rpc.Context.simple -> User_activated.upgrades tzresult Lwt.t

val dal : #Tezos_rpc.Context.simple -> Dal_config.t tzresult Lwt.t
