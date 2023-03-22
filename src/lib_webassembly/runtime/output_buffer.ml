open Proto_compat
open Error_monad

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

(** Set of outboxes of levels [last_key - validity_period] to [last_key]. *)
type t = {
  outboxes : bytes Messages.t Outboxes.t;
  mutable last_level : int32 option;
  validity_period : int32;
  message_limit : Z.t;  (** Limit of messages per outbox *)
}

let level_range buffer =
  Option.map
    (fun last_level ->
      (Int32.(succ (sub last_level buffer.validity_period)), last_level))
    buffer.last_level

let get_outbox_last_message_index messages =
  let last_message_index = Z.pred (Messages.num_elements messages) in
  if Compare.Z.(last_message_index < Z.zero) then None
  else Some last_message_index

(** Predicates on the outboxes *)

let is_outbox_full message_limit outbox =
  match get_outbox_last_message_index outbox with
  | Some last_message_index ->
      Compare.Z.(last_message_index >= Z.pred message_limit)
  | None -> false

let is_outbox_available buffer level =
  match level_range buffer with
  | None -> false
  | Some (first_level, last_level) ->
      Compare.Int32.(level <= last_level && level >= first_level)

let is_message_available messages index =
  match get_outbox_last_message_index messages with
  | None -> false
  | Some last_message -> Compare.Z.(index <= last_message && index >= Z.zero)

(** Outboxes creation and manipulation *)

let allocate_outbox buffer level =
  Outboxes.set level (Messages.create Z.zero) buffer.outboxes

let alloc ~validity_period ~message_limit ~last_level =
  let buffer =
    {
      outboxes = Outboxes.create ();
      validity_period;
      last_level
      (* Note that the last_level is not always known. At the origination of the
         of a kernel the level is not known yet. *);
      message_limit;
    }
  in
  match last_level with
  | None -> buffer
  | Some level ->
      allocate_outbox buffer level ;
      buffer

let is_initialized {last_level; _} = Option.is_some last_level

let initialize_outbox buffer level =
  if Compare.Int32.(level < 0l) then raise Invalid_level
  else
    match buffer.last_level with
    | None ->
        (* No outbox exists yet. *)
        buffer.last_level <- Some level ;
        allocate_outbox buffer level
    | Some _ -> raise Already_initialized_outbox

let move_outbox_forward buffer =
  match level_range buffer with
  | None -> raise Non_initialized_outbox
  | Some (first_level, last_level) ->
      let level = Int32.succ last_level in
      buffer.last_level <- Some level ;
      allocate_outbox buffer level ;
      Outboxes.remove first_level buffer.outboxes

let push_message buffer msg =
  let open Lwt_syntax in
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
      if is_outbox_full buffer.message_limit last_outbox then raise Full_outbox
      else
        let new_message_index = Messages.append msg last_outbox in
        {outbox_level = last_level; message_index = new_message_index}

let get_outbox_messages buffer level =
  if not (is_outbox_available buffer level) then
    if Compare.Int32.(level < 0l) then raise Invalid_level
    else raise Outdated_level
  else Outboxes.get level buffer.outboxes

(* This one is exported because it returns immutable vector *)
let get_outbox buffer level =
  Lwt.map Messages.snapshot @@ get_outbox_messages buffer level

let get_message buffer {outbox_level; message_index} =
  let open Lwt_syntax in
  let* outbox = get_outbox_messages buffer outbox_level in
  if not (is_message_available outbox message_index) then raise Invalid_id
  else Messages.get message_index outbox

(*
This implementation strongly relies on the fact,
that the only possible modifications of Outbox are:
1. push_message which pushes message in the last outbox
2. move_outbox_forward which slides the [outboxes] buffer,
   removing the first outbox and adding a new one to the end of the buffer
Also, another important detail is
that we don't return internal mutable structures (like Messages.t) from getters functions.

Given that, the following is sufficient for snapshotting:
1. Snapshot the Outboxes.t buffer
2. Shapshot a buffer's last element

For instance, there is no modification of arbitrary element of the buffer,
which makes the implementation below possible.
*)
let snapshot (buffer : t) =
  let open Lwt_syntax in
  let snapshotted_outboxes = Outboxes.snapshot buffer.outboxes in
  let+ snapshotted_outboxes =
    match buffer.last_level with
    | Some last_level ->
        let+ last_outbox = Outboxes.Map.get last_level snapshotted_outboxes in
        let last_outbox_snapshotted = Messages.snapshot last_outbox in
        let last_outbox_snapshotted =
          Messages.of_immutable last_outbox_snapshotted
        in
        Outboxes.Map.set last_level last_outbox_snapshotted snapshotted_outboxes
    | None ->
        (* No need to snapshot last element then *)
        Lwt.return snapshotted_outboxes
  in
  {buffer with outboxes = Outboxes.of_immutable snapshotted_outboxes}

module Internal_for_tests = struct
  let make level_map =
    {
      outboxes = level_map;
      validity_period = Int32.min_int;
      last_level = Some Int32.max_int;
      message_limit = Z.of_int max_int;
    }

  let get_outbox = get_outbox_messages

  let is_outbox_available = is_outbox_available

  let first_level buffer = Option.map fst (level_range buffer)

  let level_range = level_range

  let get_outbox_last_message_index = get_outbox_last_message_index
end
