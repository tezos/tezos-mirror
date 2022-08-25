type output_info = {
  outbox_level : int32;  (** The outbox level at which the message exists.*)
  message_index : Z.t;  (** The index of the message in the outbox. *)
}

exception Id_too_large

exception Empty_output

exception Invalid_level

exception Invalid_id

module Index_Vector = Lazy_vector.Mutable.ZVector
module Level_Vector = Lazy_vector.Mutable.Int32Vector

type t = bytes Index_Vector.t Level_Vector.t

let get_level output = Int32.pred (Level_Vector.num_elements output)

let get_id output =
  let open Lwt.Syntax in
  let level = get_level output in
  if level = -1l then raise Empty_output
  else
    let* last = Level_Vector.(get level output) in
    Lwt.return @@ (level, Z.pred (Index_Vector.num_elements last))

let set_level output level =
  Level_Vector.grow
    ~default:(fun () -> Index_Vector.create Z.zero)
    (Int32.sub level (get_level output))
    output

let index_create a =
  let init = Index_Vector.create Z.zero in
  Index_Vector.cons a init ;
  init

let set_value output a =
  let open Lwt.Syntax in
  let* level, index = get_id output in
  if level = -1l then Lwt.return @@ Level_Vector.cons (index_create a) output
  else if Z.to_int32 index >= Int32.max_int then raise Id_too_large
  else
    let* last = Level_Vector.get level output in
    Lwt.return @@ Index_Vector.grow ~default:(fun () -> a) Z.one last

let get output level index =
  let max_level = get_level output in
  if level > max_level || level < 0l then raise Invalid_level
  else
    let open Lwt.Syntax in
    let* index_vector = Level_Vector.get level output in
    let max_index = Z.pred @@ Index_Vector.num_elements index_vector in
    if index > max_index || index < Z.zero then raise Invalid_id
    else Index_Vector.get index index_vector

let alloc () = Level_Vector.create 0l
