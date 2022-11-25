type output_info = {
  outbox_level : int32;  (** The outbox level at which the message exists.*)
  message_index : Z.t;  (** The index of the message in the outbox. *)
}

exception Full_outbox

exception Invalid_level

exception Invalid_id

module Messages = Lazy_vector.Mutable.ZVector
module Outboxes = Lazy_vector.Mutable.Int32Vector

type t = bytes Messages.t Outboxes.t

let get_last_level outboxes =
  let last_level = Int32.pred (Outboxes.num_elements outboxes) in
  if last_level < 0l then None else Some last_level

let get_outbox_last_message_index messages =
  let last_message_index = Z.pred (Messages.num_elements messages) in
  if Z.Compare.(last_message_index < Z.zero) then None
  else Some last_message_index

(** Predicates on the outboxes *)

let is_outbox_full outbox =
  match get_outbox_last_message_index outbox with
  | Some last_message_index ->
      Z.Compare.(last_message_index = Z.of_int32 Int32.max_int)
  | None -> false

let is_outbox_available outboxes level =
  match get_last_level outboxes with
  | None -> false
  | Some last_level -> level <= last_level && level >= 0l

let is_message_available messages index =
  match get_outbox_last_message_index messages with
  | None -> false
  | Some last_message -> Z.Compare.(index <= last_message && index >= Z.zero)

(** Outboxes creation and manipulation *)

(** [alloc ()] allocates a new output_buffer. *)
let alloc () = Outboxes.create 0l

(** [ensure_outbox_at_level outboxes level] checks the outbox exists for the
    desired level, and allocates it if it doesn't (and also allocates the
    previous outboxes if they don't exists).

    @raise Invalid_level if the level is negative.
*)
let ensure_outbox_at_level outboxes level =
  if level < 0l then raise Invalid_level
  else
    let diff =
      match get_last_level outboxes with
      | None ->
          (* No outbox exists yet, we need to create `level + 1` inboxes
             (level `0` and the rest). *)
          Int32.succ level
      | Some curr_level ->
          (* If the desired outbox is below the current maximum, we ensure won't
             grow the vector with a negative diff. *)
          Int32.(max (sub level curr_level) 0l)
    in
    Outboxes.grow ~default:(fun () -> Messages.create Z.zero) diff outboxes

(** [singleton_outbox msg] creates a new outbox with a unique message. *)
let singleton_outbox msg =
  let outbox = Messages.create Z.zero in
  Messages.cons msg outbox ;
  outbox

(** [push_message outboxes msg] push a new message in the last outbox, and
    returns the its level and index in the outbox.

    @raise Full_outbox if the last outbox is full.
*)
let push_message outboxes msg =
  let open Lwt.Syntax in
  let last_level = get_last_level outboxes in
  match last_level with
  | None ->
      (* There's no outbox at all, we push a new singleton one. *)
      Outboxes.cons (singleton_outbox msg) outboxes ;
      Lwt.return {outbox_level = 0l; message_index = Z.zero}
  | Some outbox_level ->
      (* The outbox exists, we check it can contain more messages and append the
         new message. *)
      let+ last_outbox = Outboxes.get outbox_level outboxes in
      if is_outbox_full last_outbox then raise Full_outbox
      else
        let new_message_index = Messages.append msg last_outbox in
        {outbox_level; message_index = new_message_index}

(** [get_message outboxes message_info] finds a message in the output buffer.

    @raise Invalid_level if no outbox exists for the given level.
    @raise Invalid_id if no message with the given id exists in the outbox at
      this level.*)
let get_message outboxes {outbox_level; message_index} =
  let open Lwt.Syntax in
  if not (is_outbox_available outboxes outbox_level) then raise Invalid_level
  else
    let* outbox = Outboxes.get outbox_level outboxes in
    if not (is_message_available outbox message_index) then raise Invalid_id
    else Messages.get message_index outbox
