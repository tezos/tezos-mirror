type output_info = {
  outbox_level : int32;  (** The outbox level at which the message exists.*)
  message_index : Z.t;  (** The index of the message in the outbox. *)
}

exception Id_too_large

exception Empty_output

exception Invalid_level

exception Invalid_id

module Messages_vector = Lazy_vector.Mutable.ZVector
module Outboxes_vector = Lazy_vector.Mutable.Int32Vector

type t = bytes Messages_vector.t Outboxes_vector.t

let get_level outboxes = Int32.(pred (Outboxes_vector.num_elements outboxes))

let get_id outboxes =
  let open Lwt.Syntax in
  let level = get_level outboxes in
  if level = -1l then raise Empty_output
  else
    let* last = Outboxes_vector.(get level outboxes) in
    Lwt.return @@ (level, Z.pred (Messages_vector.num_elements last))

let set_level outboxes level =
  Outboxes_vector.grow
    ~default:(fun () -> Messages_vector.create Z.zero)
    (Int32.sub level (get_level outboxes))
    outboxes

let index_create a =
  let init = Messages_vector.create Z.zero in
  Messages_vector.cons a init ;
  init

let set_value outboxes a =
  let open Lwt.Syntax in
  let* level, index = get_id outboxes in
  if level = -1l then
    Lwt.return @@ Outboxes_vector.cons (index_create a) outboxes
  else if Z.to_int32 index >= Int32.max_int then raise Id_too_large
  else
    let* last = Outboxes_vector.get level outboxes in
    Lwt.return @@ Messages_vector.grow ~default:(fun () -> a) Z.one last

let get outboxes level index =
  let max_level = get_level outboxes in
  if level > max_level || level < 0l then raise Invalid_level
  else
    let open Lwt.Syntax in
    let* index_vector = Outboxes_vector.get level outboxes in
    let max_index = Z.pred @@ Messages_vector.num_elements index_vector in
    if index > max_index || index < Z.zero then raise Invalid_id
    else Messages_vector.get index index_vector

let alloc () = Outboxes_vector.create 0l
