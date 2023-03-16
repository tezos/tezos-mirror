(** This module implements a FIFO queue to model the input. The messages are
    queued in an input_buffer in their order of appearance in the inbox. *)

type message = {raw_level : int32; message_counter : Z.t; payload : bytes}
[@@deriving show]

(** An element of type t will have a content which is a lazy_vector of messages
    and a pointer to the number of elements to be able to dequeue. At this point
    there is no cleanup operation so an input_buffer content will likely have
    more than [num_elements] messages (see #3340). *)

type t = message Lazy_vector.Mutable.ZVector.t

exception Bounds

exception SizeOverflow

exception Cannot_store_an_earlier_message

exception Dequeue_from_empty_queue

(** [alloc ()] returns an empty input_buffer. *)
val alloc : unit -> t

(** [num_elements buffer] is the number of elements of [buffer].
    It is used by [dequeue] to pick the current message. Note that it is not
    necessarily equal to the length of the content of the inbox (see #3340). *)
val num_elements : t -> Z.t

(** [reset buffer] removes the current contents of the buffer. *)
val reset : t -> unit

(** [dequeue buffer] pops the current message from buffer and returns it.
    Note that the input buffer models a FIFO queue so the current message is
    the oldest in the queue. If the queue is empty it raises
    [Dequeue_from_empty_queue]. *)
val dequeue : t -> message Lwt.t

(** [enqueue buffer message] pushes the given [message] into the [buffer].
    Note that the message will have to have a higher raw_level/message_counter
    than than the newest message in the queue.
    If that fails it will raise the error [Cannot_store_an_earlier_message].
*)
val enqueue : t -> message -> unit Lwt.t

(** [snapshot buffer] returns snapshotted buffer.
    You can modify original one, snapshotted one will stay untouched
*)
val snapshot : t -> t
