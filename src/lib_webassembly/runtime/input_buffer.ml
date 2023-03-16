type message = {
  raw_level : int32;
  message_counter : Z.t; [@printer Z.pp_print]
  payload : bytes;
}
[@@deriving show]

exception Bounds

exception SizeOverflow

exception Cannot_store_an_earlier_message

exception Dequeue_from_empty_queue

let reraise = function
  | Lazy_vector.Bounds -> raise Bounds
  | Lazy_vector.SizeOverflow -> raise SizeOverflow
  | exn -> raise exn

module Vector = Lazy_vector.Mutable.ZVector

type t = message Vector.t

let num_elements input = Vector.num_elements input

let alloc () = Vector.create Z.zero

let get_input index buffer =
  Lwt.catch (fun () -> Vector.get index buffer) reraise

let reset = Vector.reset

let dequeue input =
  let open Lwt.Syntax in
  let num_elems = num_elements input in
  if num_elems = Z.zero then raise Dequeue_from_empty_queue ;
  let* result = Vector.pop input in
  Lwt.return result

let is_successor {raw_level = msg_lv; message_counter = msg_counter; _}
    {raw_level = msg_lv'; message_counter = msg_counter'; _} =
  let first_compare = Int32.compare msg_lv msg_lv' in
  first_compare > 0
  || (first_compare = 0 && Z.compare msg_counter msg_counter' > 0)

let enqueue (input : t) r =
  let open Lwt.Syntax in
  let enqueue () =
    try
      let _ = Vector.append r input in
      Lwt.return_unit
    with exn -> reraise exn
  in
  let num_elements = num_elements input in
  if num_elements = Z.zero then enqueue ()
  else
    let* first = get_input (Z.pred num_elements) input in
    if is_successor r first then enqueue ()
    else raise Cannot_store_an_earlier_message

let snapshot v = Vector.of_immutable @@ Vector.snapshot v
