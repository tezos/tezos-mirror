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

let path = Tezos_rpc.Path.(open_root / "config")

module Network = struct
  let user_activated_upgrades =
    Tezos_rpc.Service.get_service
      ~description:"List of protocols to switch to at given levels"
      ~query:Tezos_rpc.Query.empty
      ~output:User_activated.upgrades_encoding
      Tezos_rpc.Path.(path / "network" / "user_activated_upgrades")

  let user_activated_protocol_overrides =
    Tezos_rpc.Service.get_service
      ~description:"List of protocols which replace other protocols"
      ~query:Tezos_rpc.Query.empty
      ~output:User_activated.protocol_overrides_encoding
      Tezos_rpc.Path.(path / "network" / "user_activated_protocol_overrides")

  let dal_config =
    Tezos_rpc.Service.get_service
      ~description:"Configuration for the DAL"
      ~query:Tezos_rpc.Query.empty
      ~output:Dal_config.encoding
      Tezos_rpc.Path.(path / "network" / "dal")
end

let history_mode_encoding =
  Data_encoding.(obj1 (req "history_mode" History_mode.encoding))

let history_mode =
  Tezos_rpc.Service.get_service
    ~description:"Returns the history mode of the node's underlying storage."
    ~query:Tezos_rpc.Query.empty
    ~output:history_mode_encoding
    Tezos_rpc.Path.(path / "history_mode")

module Logging = struct
  let configure =
    Tezos_rpc.Service.put_service
      ~description:"Replace the logging configuration of the node."
      ~query:Tezos_rpc.Query.empty
      ~input:Tezos_base.Internal_event_config.encoding
      ~output:Data_encoding.empty
      Tezos_rpc.Path.(root / "config" / "logging")
end

let user_activated_upgrades cctxt =
  Tezos_rpc.Context.make_call Network.user_activated_upgrades cctxt () () ()

let dal_config cctxt =
  Tezos_rpc.Context.make_call Network.dal_config cctxt () () ()
