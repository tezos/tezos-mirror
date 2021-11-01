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

module Request : sig
  type view = New_head of Block_hash.t | New_branch of Block_hash.t * int

  val encoding : view Data_encoding.encoding

  val pp : Format.formatter -> view -> unit
end

module Event : sig
  type block_received = {peer : P2p_peer.Id.t; hash : Block_hash.t}

  type t =
    | Request of
        (Request.view * Worker_types.request_status * error list option)
    | Validating_new_branch of {peer : P2p_peer.Id.t; nb_blocks : int}
    | New_branch_validated of block_received
    | Fetching_operations_for_head of block_received
    | Requesting_new_head_validation of block_received
    | New_head_validation_end of block_received
    | Ignoring_head of block_received
    | Ignoring_previously_validated_block of block_received
    | Ignoring_prechecked_block of block_received
    | Ignoring_invalid_block of block_received
    | Missing_new_head_predecessor of block_received
    | Ignoring_branch_with_invalid_locator of block_received
    | Ignoring_branch_without_common_ancestor of block_received
    | No_new_head_from_peer of {peer : P2p_peer.Id.t; timespan : float}
    | Processing_new_head of block_received
    | Processing_new_branch of block_received
    | Terminating_worker of {peer : P2p_peer.Id.t; reason : string}
    | Ignoring_prechecked_invalid_block of block_received

  type view = t

  val view : t -> view

  val level : t -> Internal_event.level

  val encoding : t Data_encoding.encoding

  val pp : Format.formatter -> t -> unit
end

type pipeline_length = {fetched_header_length : int; fetched_block_length : int}

val pipeline_length_encoding : pipeline_length Data_encoding.encoding
