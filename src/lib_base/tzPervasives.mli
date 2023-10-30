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

include module type of struct
  include Tezos_stdlib
end

module Error_monad = Tezos_error_monad.Error_monad
module Data_encoding = Data_encoding

(** The following modules are part of [TzLwtreslib]. We just remove
    - the [Monad] module (already available, with some name simplifications,
      from [Error_monad]), and
    - the [List] module (exported just afterwards with a small addition). *)
module Hashtbl = Tezos_error_monad.TzLwtreslib.Hashtbl

module Map = Tezos_error_monad.TzLwtreslib.Map
module Option = Tezos_error_monad.TzLwtreslib.Option
module Result = Tezos_error_monad.TzLwtreslib.Result
module Seq = Tezos_error_monad.TzLwtreslib.Seq
module Seq_e = Tezos_error_monad.TzLwtreslib.Seq_e
module Seq_s = Tezos_error_monad.TzLwtreslib.Seq_s
module Seq_es = Tezos_error_monad.TzLwtreslib.Seq_es
module Set = Tezos_error_monad.TzLwtreslib.Set
module Unit = Tezos_error_monad.TzLwtreslib.Unit
module WithExceptions = Tezos_error_monad.TzLwtreslib.WithExceptions

module List : sig
  include module type of Tezos_stdlib.TzList

  include module type of Tezos_error_monad.TzLwtreslib.List
end

module String : sig
  include module type of String

  include module type of Tezos_stdlib.TzString

  module Hashtbl :
    Tezos_error_monad.TzLwtreslib.Hashtbl.SeededS with type key = t

  module Map : Tezos_error_monad.TzLwtreslib.Map.S with type key = t

  module Set : Tezos_error_monad.TzLwtreslib.Set.S with type elt = t
end

module Bytes : sig
  include module type of Bytes

  include module type of Tezos_stdlib.TzBytes
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
module Block_hash = Tezos_crypto.Hashed.Block_hash
module Block_metadata_hash = Tezos_crypto.Hashed.Block_metadata_hash
module Chain_id = Tezos_crypto.Hashed.Chain_id
module Context_hash = Tezos_crypto.Hashed.Context_hash
module Operation_hash = Tezos_crypto.Hashed.Operation_hash
module Operation_list_hash = Tezos_crypto.Hashed.Operation_list_hash
module Operation_list_list_hash = Tezos_crypto.Hashed.Operation_list_list_hash
module Operation_metadata_hash = Tezos_crypto.Hashed.Operation_metadata_hash
module Operation_metadata_list_hash =
  Tezos_crypto.Hashed.Operation_metadata_list_hash
module Operation_metadata_list_list_hash =
  Tezos_crypto.Hashed.Operation_metadata_list_list_hash
module Protocol_hash = Tezos_crypto.Hashed.Protocol_hash
module Signature = Tezos_crypto.Signature
module Skip_list = Skip_list

include module type of Utils.Infix

include module type of Tezos_error_monad.Error_monad

module Option_syntax =
  Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Option_syntax

module Lwt_option_syntax =
  Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Lwt_option_syntax

module Internal_event = Internal_event

module Filename : sig
  include module type of Filename

  module Infix : sig
    val ( // ) : string -> string -> string
  end
end

module Bounded = Bounded

(** The main purpose of this module is to be used with parametric
   types such as [('a, Empty.t) result]. Such type is actually
   isomorphic to ['a] (see [get_ok] function). This is useful if a
   module signature expects a generic [('a,'b) result] type, but for
   some instantiation, ['b] is actually empty. Here is a small example
   how such module can be used:

   {[ module type S = sig

       type error

       type 'a t

       val return : 'a -> 'a t

       val fail : error -> 'a t

       val bind : 'a t -> ('a -> 'a t) -> (error -> 'a t) -> 'a t end

     module M : S with type error = Empty.t and type 'a t = ('a,
   Empty.t) result = struct

       type error = Empty.t

       type 'a t = ('a, Empty.t) result

       let return = fun x -> Ok x

       let fail = Empty.absurd

       let bind = fun data left _right -> match data with | Ok x ->
   left x | Error err -> Empty.absurd err end

     let _ = let data = M.bind (M.return 5) (fun x -> M.return (x +
   2)) (Empty.absurd) in assert (data |> Empty.get_ok = 7) ]} *)
module Empty : sig
  (** This type has no canonical inhabitant (there is no terminating
     OCaml value which inhabits this type). *)
  type t = |

  (** [get_ok r] eliminates the impossible case. This function is
       total and does not raise any exception. *)
  val get_ok : ('a, t) result -> 'a

  (** [absurd empty] allows constructing any type from the absurd
      case. This is useful to typecheck impossible branches, e.g.
      {[
        Result.fold ~ok:(fun i -> i + 1) ~error:Empty.absurd (Ok 41)
      ]}
      Note that this particular example using `result` can be rewritten as:
      {[
        let i = Empty.get_ok (Ok 41) in i + 1
      ]} *)
  val absurd : t -> 'a
end

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
