(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

include Tezos_stdlib
module Error_monad = Tezos_error_monad.Error_monad
include Tezos_micheline
module Data_encoding = Data_encoding
include Tezos_error_monad.TzLwtreslib

module List = struct
  include Tezos_stdlib.TzList
  include Tezos_error_monad.TzLwtreslib.List
end

module String = struct
  include String
  include Tezos_stdlib.TzString

  module Hashtbl = Tezos_error_monad.TzLwtreslib.Hashtbl.MakeSeeded (struct
    type t = string

    let equal = String.equal

    (* in OCaml 5, the interface of the [Hashtbl.MakeSeeded] functor has changed:
       it expects the seeded hash function to be called [seeded_hash]. This will
       reduce the amount of boilerplate in the long run, but in the transitionary
       period it increases it instead: we need to expose both [hash] and
       [seeded_hash] to be compatible with both versions of the compiler.

       Depending on which OCaml compiler (4 or 5) is used, either of the hashing
       function ([hash] or [seeded_hash]) is used. The other function is unused.
       Thus we also need to suppress the associated warning. *)
    [@@@ocaml.warning "-32"]

    let hash = Hashtbl.seeded_hash

    let seeded_hash = Hashtbl.seeded_hash

    [@@@ocaml.warning "+32"]
  end)

  module Map = Tezos_error_monad.TzLwtreslib.Map.Make (String)
  module Set = Tezos_error_monad.TzLwtreslib.Set.Make (String)
end

module Bytes = struct
  include Bytes
  include Tezos_stdlib.TzBytes
end

module Time = Time
module Fitness = Fitness
module User_activated = User_activated
module Block_header = Block_header
module Genesis = Genesis
module Operation = Operation
module Protocol = Protocol
module Test_chain_status = Test_chain_status
module Block_locator = Block_locator
module Mempool = Mempool
module P2p_addr = P2p_addr
module P2p_identity = P2p_identity
module P2p_peer = P2p_peer
module P2p_point = P2p_point
module P2p_connection = P2p_connection
module P2p_stat = P2p_stat
module P2p_version = P2p_version
module P2p_rejection = P2p_rejection
module P2p_params = P2p_params
module Distributed_db_version = Distributed_db_version
module Network_version = Network_version
include Tezos_crypto.Hashed
module Signature = Tezos_crypto.Signature
include Utils.Infix
include Error_monad

module Option_syntax =
  Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Option_syntax

module Lwt_option_syntax =
  Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Lwt_option_syntax

module Internal_event = Internal_event

module Filename = struct
  include Stdlib.Filename
  include Tezos_stdlib.TzFilename
end

module Bounded = Bounded

module Empty = struct
  type t = |

  let get_ok : ('a, t) result -> 'a = function Ok a -> a | Error _ -> .

  let absurd : t -> 'a = function _ -> .
end

module Skip_list = Skip_list
module Profiler = Profiler

module type PRINTABLE = sig
  type t

  val pp : Format.formatter -> t -> unit
end

module type COMPARABLE = Compare.S

module type ENCODABLE = sig
  type t

  val encoding : t Data_encoding.t
end
