(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** These errors are only to be matched in tests. *)
type error +=
  | Zk_rollup_does_not_exist of Zk_rollup_repr.t
        (** Emitted when trying to perform an operation over a ZK rollup
            that hasn't been initialised. *)
  | Zk_rollup_invalid_op_code of int
        (** Emitted when trying to add to the pending list and operation
            with an invalid op code. *)
  | Zk_rollup_pending_list_too_short
        (** Emitted when trying to process more public operations than
            those available in the pending list. *)

(** [account context rollup] fetches the ZK [rollup]'s account from the
    storage.
*)
val account :
  Raw_context.t ->
  Zk_rollup_repr.t ->
  (Raw_context.t * Zk_rollup_account_repr.t) tzresult Lwt.t

(* [pending_list context rollup] fetches the ZK [rollup]'s
   pending list description from the storage.
   See {! Zk_rollup_repr.pending_list}. *)
val pending_list :
  Raw_context.t ->
  Zk_rollup_repr.t ->
  (Raw_context.t * Zk_rollup_repr.pending_list) tzresult Lwt.t

(* [pending_op context rollup i] fetches the [i]th L2 operation from
   ZK [rollup]'s pending list, alongside an optional ticket hash
   to perform an exit (see {!Zk_rollup_apply} for more details).
*)
val pending_op :
  Raw_context.t ->
  Zk_rollup_repr.t ->
  int64 ->
  (Raw_context.t * (Zk_rollup_operation_repr.t * Ticket_hash_repr.t option))
  tzresult
  Lwt.t

(** [originate context static ~init_state] produces an address [a] for
    a ZK rollup storage using the [origination_nonce] from
    the [context]. This function also initializes the storage,
    indexing the initial ZKRU account by [a].

     Returns the new context and ZKRU address, alongside the size
     of the new account.
*)
val originate :
  Raw_context.t ->
  Zk_rollup_account_repr.static ->
  init_state:Zk_rollup_state_repr.t ->
  (Raw_context.t * Zk_rollup_repr.t * Z.t) tzresult Lwt.t

(** [add_to_pending context rollup operations] appends to the
    ZK [rollup]'s pending list a list of L2 [operations].
    Returns the new context alongside the size of the new operations.

    May fail with:
    {ul
      {li [Zk_rollup_invalid_op_code op_code] if the [op_code]
        of one of the [operations] is greater or equal to the
        number of declared operations for this [rollup].
      }
    }
*)
val add_to_pending :
  Raw_context.t ->
  Zk_rollup_repr.t ->
  (Zk_rollup_operation_repr.t * Ticket_hash_repr.t option) list ->
  (Raw_context.t * Z.t) tzresult Lwt.t

(** [get_pending_length context rollup] returns the length of a
    ZK [rollup]'s pending list.
*)
val get_pending_length :
  Raw_context.t -> Zk_rollup_repr.t -> (Raw_context.t * int) tzresult Lwt.t

(** [get_prefix context rollup n] returns the prefix of length [n]
    of the [rollup]'s pending list.

    May fail with:
    {ul
      {li [Zk_rollup_pending_list_too_short] if [n] is greater than
        the length of the pending list.}
      {li [Zk_rollup_negative_length] if [n] is negative.}
    }
*)
val get_prefix :
  Raw_context.t ->
  Zk_rollup_repr.t ->
  int ->
  (Raw_context.t
  * (Zk_rollup_operation_repr.t * Ticket_hash_repr.t option) list)
  tzresult
  Lwt.t

(** [update context rollup ~pending_to_drop ~new_account] sets the
    [rollup]'s account to [new_account]. Additionally, it removes
    the first [pending_to_drop] entries from the [rollup]'s pending
    list.
    Returns the new context.

    May fail with:
    {ul
      {li [Zk_rollup_pending_list_too_short] if [pending_to_drop] is
        greater than the length of the pending list.}
      {li [Zk_rollup_negative_length] if [pending_to_drop] is negative.}
    }
*)
val update :
  Raw_context.t ->
  Zk_rollup_repr.t ->
  pending_to_drop:int ->
  new_account:Zk_rollup_account_repr.t ->
  Raw_context.t tzresult Lwt.t

(** [assert_exist context rollup] asserts that [rollup] has been initialized.
    Returns the new context.

    May fail with:
    {ul
      {li [Zk_rollup_does_not_exist] if [rollup] is not found.}
    }
*)
val assert_exist :
  Raw_context.t -> Zk_rollup_repr.t -> Raw_context.t tzresult Lwt.t

(** [exists context rollup] returns a boolean representing whether
    [rollup] has been initialized.
*)
val exists :
  Raw_context.t -> Zk_rollup_repr.t -> (Raw_context.t * bool) tzresult Lwt.t
