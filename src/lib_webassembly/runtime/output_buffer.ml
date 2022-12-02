type output_info = {
  outbox_level : int32;  (** The outbox level at which the message exists.*)
  message_index : Z.t;  (** The index of the message in the outbox. *)
}

exception Non_initialized_outbox

exception Already_initialized_outbox

exception Full_outbox

exception Outdated_level

exception Invalid_level

exception Invalid_id

exception Invalid_diff

module Messages = Lazy_vector.Mutable.ZVector
module Outboxes = Lazy_map.Mutable.LwtInt32Map

(** Set of outboxes of levels [last_key - validity_period] to [last_key]. *)
type t = {
  outboxes : bytes Messages.t Outboxes.t;
  mutable last_level : int32 option;
  validity_period : int32;
}

let level_range buffer =
  Option.map
    (fun last_level ->
      (Int32.(succ (sub last_level buffer.validity_period)), last_level))
    buffer.last_level

let first_level buffer = Option.map fst (level_range buffer)

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

let is_outbox_available buffer level =
  match level_range buffer with
  | None -> false
  | Some (first_level, last_level) ->
      level <= last_level && level >= first_level

let is_message_available messages index =
  match get_outbox_last_message_index messages with
  | None -> false
  | Some last_message -> Z.Compare.(index <= last_message && index >= Z.zero)

(** Outboxes creation and manipulation *)

let allocate_outbox buffer level =
  Outboxes.set level (Messages.create Z.zero) buffer.outboxes

(** [alloc ~validity_period ~last_level] allocates a new output_buffer. If
    [last_level] is [Some level], the corresponding outbox is allocated. *)
let alloc ~validity_period ~last_level =
  let buffer =
    {
      outboxes = Outboxes.create ();
      validity_period;
      last_level
      (* Note that the last_level is not always known. At the origination of the
         of a kernel the level is not known yet. *);
    }
  in
  match last_level with
  | None -> buffer
  | Some level ->
      allocate_outbox buffer level ;
      buffer

(** [is_initialized buffer] returns [true] if the output buffer has been
    initialized. *)
let is_initialized {last_level; _} = last_level <> None

(** [initialize_outbox buffer level] initialize the output_buffer with a fresh
    inbox at the given level.

   @raise Already_initialized_outbox if the output_buffer has already been initialized
*)
let initialize_outbox buffer level =
  if level < 0l then raise Invalid_level
  else
    match buffer.last_level with
    | None ->
        (* No outbox exists yet. *)
        buffer.last_level <- Some level ;
        allocate_outbox buffer level
    | Some _ -> raise Already_initialized_outbox

(** [move_outbox_forward outboxes] increments the last level of the outbox,
    allocates a new outbox and removes the outbox at the previous first level,
    according to the validity period.

    @raise Non_initialized_outbox if the output buffer has never been initialized.
*)
let move_outbox_forward buffer =
  match level_range buffer with
  | None -> raise Non_initialized_outbox
  | Some (first_level, last_level) ->
      let level = Int32.succ last_level in
      buffer.last_level <- Some level ;
      allocate_outbox buffer level ;
      Outboxes.remove first_level buffer.outboxes

(** [push_message outboxes msg] push a new message in the last outbox, and
    returns the its level and index in the outbox.

    @raise Full_outbox if the last outbox is full.
*)
let push_message buffer msg =
  let open Lwt.Syntax in
  match buffer.last_level with
  | None ->
      (* Note that this should be an impossible case since the outbox should be
         allocated at the beginning of the collection phase of the PVM, as such
         there should always be an outbox at the moment the kernel push
         messages. *)
      raise Non_initialized_outbox
  | Some last_level ->
      (* The outbox exists, we check it can contain more messages and append the
         new message. *)
      let+ last_outbox = Outboxes.get last_level buffer.outboxes in
      if is_outbox_full last_outbox then raise Full_outbox
      else
        let new_message_index = Messages.append msg last_outbox in
        {outbox_level = last_level; message_index = new_message_index}

let get_outbox buffer level =
  if not (is_outbox_available buffer level) then
    if level < 0l then raise Invalid_level else raise Outdated_level
  else Outboxes.get level buffer.outboxes

(** [get_message outboxes message_info] finds a message in the output buffer.

    @raise Invalid_level if no outbox exists for the given level.
    @raise Outdated_level if the outbox is outdated according to the validity
      period.
    @raise Invalid_id if no message with the given id exists in the outbox at
      this level.
*)
let get_message buffer {outbox_level; message_index} =
  let open Lwt.Syntax in
  let* outbox = get_outbox buffer outbox_level in
  if not (is_message_available outbox message_index) then raise Invalid_id
  else Messages.get message_index outbox
