(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

(** [get_inbox context] returns the current state of the inbox,
    if it exists. *)
val get_inbox :
  Raw_context.t -> (Sc_rollup_inbox_repr.t * Raw_context.t) tzresult Lwt.t

(** [add_external_messages context messages] adds [messages] to the smart
    rollups internal inbox level witness. *)
val add_external_messages :
  Raw_context.t -> string list -> Raw_context.t tzresult Lwt.t

(** [add_deposit ~payload ~sender ~source ~destination ctxt] adds the
    internal deposit message of [payload], [sender], and [source] to
    the smart-contract rollups' inbox.

    See [add_external_messages] for returned values and failures.
*)
val add_deposit :
  Raw_context.t ->
  payload:Script_repr.expr ->
  sender:Contract_hash.t ->
  source:Signature.public_key_hash ->
  destination:Sc_rollup_repr.Address.t ->
  Raw_context.t tzresult Lwt.t

(** Initialize the inbox in the storage at protocol initialization. *)
val init_inbox :
  predecessor:Block_hash.t -> Raw_context.t -> Raw_context.t tzresult Lwt.t

(** Adds the [Info_per_level] in the in-memory inbox level witness. If
    the current level is the first level of the current protocol then
    also add [Migration] message.  *)
val add_level_info :
  predecessor:Block_hash.t -> Raw_context.t -> Raw_context.t tzresult Lwt.t

(** [finalize_inbox_level ctxt] ends the internal representation for the block.
*)
val finalize_inbox_level : Raw_context.t -> Raw_context.t tzresult Lwt.t
