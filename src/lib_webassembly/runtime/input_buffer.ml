type message = {
  rtype : int32;
  raw_level : int32;
  message_counter : Z.t;
  payload : bytes;
}

include Memory_exn

exception Cannot_store_an_earlier_message

exception Dequeue_from_empty_queue

module Vector = Lazy_vector.Mutable.LwtZVector

type t = {content : message Vector.t; mutable num_elements : Z.t}

let num_elements input = input.num_elements

let alloc () = {content = Vector.create Z.zero; num_elements = Z.zero}

let dequeue input =
  let open Lwt.Syntax in
  let num_elems = num_elements input in
  if num_elems = Z.zero then raise Dequeue_from_empty_queue ;
  let* result = Vector.get (Z.pred num_elems) input.content in
  input.num_elements <- Z.pred num_elems ;
  Lwt.return result

let is_successor {raw_level = msg_lv; message_counter = msg_counter; _}
    {raw_level = msg_lv'; message_counter = msg_counter'; _} =
  let first_compare = Int32.compare msg_lv msg_lv' in
  first_compare > 0
  || (first_compare = 0 && Z.compare msg_counter msg_counter' > 0)

let enqueue (input : t) r =
  let open Lwt.Syntax in
  let num_elements = num_elements input in
  if input.num_elements = Z.zero then (
    Vector.cons r input.content ;
    input.num_elements <- Z.succ num_elements ;
    Lwt.return ())
  else
    let+ first = Vector.get Z.zero input.content in
    if is_successor r first then (
      Vector.cons r input.content ;
      input.num_elements <- Z.succ num_elements)
    else raise Cannot_store_an_earlier_message
