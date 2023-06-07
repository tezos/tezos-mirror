(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
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

open Tezos_rpc

(** This module defines subcontext type of the subdirectory and
    the way to project it from Node_context and a path prefix. *)
module type PARAM = sig
  include Sc_rollup_services.PREFIX

  type context

  val context_of_prefix : Node_context.rw -> prefix -> context tzresult Lwt.t
end

(** This module is a helper to register your endpoints and
    build a resulting subdirectory eventually. *)
module Make_directory (S : PARAM) : sig
  (** Register an endpoint with no parameters in the path. *)
  val register0 :
    ([< Resto.meth], 'prefix, 'prefix, 'query, 'input, 'output) Service.t ->
    (S.context -> 'query -> 'input -> 'output tzresult Lwt.t) ->
    unit

  (** Register an endpoint with a single parameter in the path. *)
  val register1 :
    ( [< Resto.meth],
      'prefix,
      'prefix * 'param1,
      'query,
      'input,
      'output )
    Service.t ->
    (S.context -> 'param1 -> 'query -> 'input -> 'output tzresult Lwt.t) ->
    unit

  (** Build directory with registered endpoints with respect to Node_context. *)
  val build_directory : Node_context.rw -> unit Tezos_rpc.Directory.t
end

(** This module is a helper to extract a block hash
    from block reference and Node_context *)
module Block_directory_helpers : sig
  val block_of_prefix :
    Node_context.rw ->
    [< `Cemented | `Finalized | `Hash of Block_hash.t | `Head | `Level of int32] ->
    Block_hash.t tzresult Lwt.t
end
