(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2018-2020 Tarides <contact@tarides.com>                     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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
module Tezos_context_encoding = Tezos_context_brassaia_encoding

module type TEZOS_CONTEXT_UNIX = sig
  include
    Tezos_context_sigs.Context.TEZOS_CONTEXT
      with type memory_context_tree := Tezos_context_memory.Context.tree

  (** Sync the context with disk. Only useful for read-only instances.
      Does not fail when the context is not in read-only mode. *)
  val sync : index -> unit Lwt.t

  (** A Brassaia context corresponds to an in-memory overlay (corresponding
      to the type {!tree}) over some on-disk data. Writes are buffered in
      the overlay temporarily. Calling [flush] performs these writes on
      disk and returns a context with an empty overlay. *)
  val flush : t -> t Lwt.t

  (** Offline integrity checking and statistics for contexts. *)
  module Checks : sig
    module Pack : Brassaia_pack_unix.Checks.S

    module Index : Brassaia_index.Index.Checks.S
  end
end

(** Tezos - Versioned, block indexed (key x value) store *)
module Make (Encoding : module type of Tezos_context_encoding.Context) :
  TEZOS_CONTEXT_UNIX
