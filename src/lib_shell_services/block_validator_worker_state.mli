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
  type validation_view = {
    chain_id : Chain_id.t;
    block : Block_hash.t;
    peer : P2p_peer.Id.t option;
  }

  type preapplication_view = {chain_id : Chain_id.t; level : int32}

  type view =
    | Validation of validation_view
    | Preapplication of preapplication_view

  val encoding : view Data_encoding.encoding

  val pp : Format.formatter -> view -> unit
end

module Event : sig
  type t =
    | Validation_success of
        Request.validation_view * Worker_types.request_status
    | Validation_failure of
        Request.validation_view * Worker_types.request_status * error trace
    | Preapplication_success of
        Request.preapplication_view * Worker_types.request_status
    | Preapplication_failure of
        Request.preapplication_view * Worker_types.request_status * error trace
    | Validation_failure_after_precheck of
        Request.validation_view * Worker_types.request_status * error trace
    | Precheck_failure of
        Request.validation_view * Worker_types.request_status * error trace
    | Could_not_find_context of Block_hash.t
    | Previously_validated of Block_hash.t
    | Validating_block of Block_hash.t
    | Prechecking_block of Block_hash.t
    | Prechecked_block of Block_hash.t

  type view = t

  val view : view -> t

  val level : t -> Internal_event.level

  val encoding : t Data_encoding.encoding

  val pp : Format.formatter -> t -> unit
end
