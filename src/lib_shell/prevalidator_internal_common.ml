(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

let advertisement_delay = 0.1

module Name = struct
  type t = Tezos_crypto.Chain_id.t * Tezos_crypto.Protocol_hash.t

  let encoding =
    Data_encoding.tup2
      Tezos_crypto.Chain_id.encoding
      Tezos_crypto.Protocol_hash.encoding

  let base = ["prevalidator"]

  let pp fmt (chain_id, proto_hash) =
    Format.fprintf
      fmt
      "%a:%a"
      Tezos_crypto.Chain_id.pp_short
      chain_id
      Tezos_crypto.Protocol_hash.pp_short
      proto_hash

  let equal (c1, p1) (c2, p2) =
    Tezos_crypto.Chain_id.equal c1 c2 && Tezos_crypto.Protocol_hash.equal p1 p2
end

open Prevalidator_worker_state

module type T = sig
  type types_state

  val get_rpc_directory :
    types_state -> types_state Tezos_rpc.Directory.t lazy_t

  val name : Name.t

  module Types : Worker_intf.TYPES with type state = types_state

  module Worker :
    Worker.T
      with type ('a, 'b) Request.t = ('a, 'b) Request.t
       and type Request.view = Request.view
       and type Types.state = types_state

  type worker = Worker.infinite Worker.queue Worker.t

  val worker : worker Lazy.t
end

type t = (module T)
