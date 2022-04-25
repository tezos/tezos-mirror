(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Protocol_client_context
open Protocol
open Alpha_context
open Common

type tag =
  [ `Commitment
  | `Submit_batch
  | `Finalize_commitment
  | `Remove_commitment
  | `Rejection
  | `Dispatch_withdrawals ]

module Tags : Set.S with type elt = tag

type tags = Tags.t

val tags_encoding : tags Data_encoding.t

val pp_tags : Format.formatter -> tags -> unit

module Request : sig
  type 'a t =
    | Add_pending : L1_operation.t -> unit t
    | New_tezos_head :
        Alpha_block_services.block_info * Alpha_block_services.block_info reorg
        -> unit t
    | Inject : unit t

  type view = View : _ t -> view

  include Worker_intf.REQUEST with type 'a t := 'a t and type view := view
end

module Name : Worker_intf.NAME with type t = public_key_hash

module Dummy_event : Worker_intf.EVENT with type t = unit

module Logger :
  Worker_intf.LOGGER
    with module Event = Dummy_event
     and type Request.view = Request.view
