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

(** [add_external_messages context msg] adds [msg] to the smart rollups' inbox.

    This function returns the updated context as well as the size diff.

    May fail with:
    {ul
      {li [Sc_rollup_max_number_of_available_messages] if [inbox] is full}
      {li [Sc_rollup_max_number_of_messages_reached_for_commitment_period] if
      the number of messages pushed during commitment period is too high}
    }
*)
val add_external_messages :
  Raw_context.t ->
  string list ->
  (Sc_rollup_inbox_repr.t * Z.t * Raw_context.t) tzresult Lwt.t

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
  (Sc_rollup_inbox_repr.t * Z.t * Raw_context.t) tzresult Lwt.t

(** Initialize the inbox in the storage at protocol initialization. *)
val init : Raw_context.t -> Raw_context.t tzresult Lwt.t

(** Push a [Start_of_level] internal inbox message in the inbox using
    {!add_internal_message}.
*)
val add_start_of_level :
  Raw_context.t -> (Sc_rollup_inbox_repr.t * Z.t * Raw_context.t) tzresult Lwt.t

(** Push a [End_of_level] internal inbox message in the inbox using
    {!add_internal_message}.
*)
val add_end_of_level :
  Raw_context.t -> (Sc_rollup_inbox_repr.t * Z.t * Raw_context.t) tzresult Lwt.t

(**/**)

module Internal_for_tests : sig
  (** [update_num_and_size_of_messages ~num_messages ~total_messages_size
      message] returns the length and total messages size
      [messages]. *)
  val update_num_and_size_of_messages :
    num_messages:int ->
    total_messages_size:int ->
    Sc_rollup_inbox_message_repr.serialized ->
    int * int
end
