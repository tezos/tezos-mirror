(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
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

open Protocol.Alpha_context
module Sc_services = Sc_rollup_services

module Local = struct
  open Tezos_rpc.Path

  include Sc_services.Make_services (struct
    type prefix = unit

    let prefix = open_root / "local"
  end)

  let durable_state_value (pvm_kind : Protocol.Alpha_context.Sc_rollup.Kind.t) =
    Tezos_rpc.Service.get_service
      ~description:
        "Retrieve value by key from durable storage. PVM state corresponds to \
         the last L2 operation in the queue. Value returned in hex format."
      ~query:Sc_services.Query.key_query
      ~output:Data_encoding.(option bytes)
      (path / "durable" / Sc_rollup.Kind.to_string pvm_kind / "value")

  let durable_state_subkeys (pvm_kind : Protocol.Alpha_context.Sc_rollup.Kind.t)
      =
    Tezos_rpc.Service.get_service
      ~description:
        "Retrieve value by key from durable storage. PVM state corresponds to \
         the last L2 operation in the queue."
      ~query:Sc_services.Query.key_query
      ~output:Data_encoding.(list string)
      (path / "durable" / Sc_rollup.Kind.to_string pvm_kind / "subkeys")

  let injection = Sc_services.Local.injection
end
