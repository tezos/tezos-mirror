(** This module implements a bounded FIFO queue to model the outputs.
    Each element of the queue is a list of outbox messages produced on this level.
*)

type output_info = {
  outbox_level : int32;  (** The outbox level at which the message exists.*)
  message_index : Z.t;  (** The index of the message in the outbox. *)
}

exception Non_initialized_outbox

exception Already_initialized_outbox

exception Invalid_outbox_limit

exception Full_outbox

exception Outdated_level

exception Invalid_level

exception Invalid_id

exception Invalid_diff

module Messages = Lazy_vector.Mutable.ZVector
module Outboxes = Lazy_map.Mutable.LwtInt32Map

type t = {
  outboxes : bytes Messages.t Outboxes.t;
  mutable last_level : int32 option;
  validity_period : int32;
  message_limit : Z.t;  (** Limit of messages per outbox *)
}

(** [alloc ~validity_period ~last_level] allocates a new output_buffer. If
    [last_level] is [Some level], the corresponding outbox is allocated. *)
val alloc :
  validity_period:int32 -> message_limit:Z.t -> last_level:int32 option -> t

(** [is_initialized buffer] returns [true] if the output buffer has been
    initialized. *)
val is_initialized : t -> bool

(** [initialize_outbox buffer level] initialize the output_buffer with a fresh
    inbox at the given level.

   @raise Already_initialized_outbox if the output_buffer has already been initialized
*)
val initialize_outbox : t -> int32 -> unit

(** [move_outbox_forward outboxes] increments the last level of the outbox,
    allocates a new outbox and removes the outbox at the previous first level,
    according to the validity period.

    @raise Non_initialized_outbox if the output buffer has never been initialized.
*)
val move_outbox_forward : t -> unit

(** [push_message outboxes msg] push a new message in the last outbox, and
    returns the its level and index in the outbox.

    @raise Full_outbox if the last outbox is full.
*)
val push_message : t -> bytes -> output_info Lwt.t

(** [get_outbox outboxes level] returns the outbox for the given [level].

    @raise Invalid_level if no outbox exists for the given level.
    @raise Outdated_level if the outbox is outdated according to the validity
      period.
*)
val get_outbox : t -> int32 -> bytes Messages.Vector.t Lwt.t

(** [get_message outboxes message_info] finds a message in the output buffer.

    @raise Invalid_level if no outbox exists for the given level.
    @raise Outdated_level if the outbox is outdated according to the validity
      period.
    @raise Invalid_id if no message with the given id exists in the outbox at
      this level.
*)
val get_message : t -> output_info -> bytes Lwt.t

(** [sandbox outboxes] create a snapshot of the outbox.
    You can modify original one, snapshotted one will stay untouched.
*)
val snapshot : t -> t Lwt.t

module Internal_for_tests : sig
  val get_outbox : t -> int32 -> bytes Messages.t Lwt.t

  val make : bytes Messages.t Outboxes.t -> t

  val is_outbox_available : t -> int32 -> bool

  val get_outbox_last_message_index : 'a Messages.t -> Z.t option

  val first_level : t -> int32 option

  val level_range : t -> (int32 * int32) option
end
