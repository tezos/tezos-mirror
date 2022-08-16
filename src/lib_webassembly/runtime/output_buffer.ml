type output_info = {
  outbox_level : int32;  (** The outbox level at which the message exists.*)
  message_index : Z.t;  (** The index of the message in the outbox. *)
}

exception Id_too_large

module OutputKey : Lazy_map.KeyS with type t = output_info = struct
  type t = output_info

  let compare {outbox_level = l1; message_index = i1}
      {outbox_level = l2; message_index = i2} =
    let c1 = compare l1 l2 in
    if c1 = 0 then Z.compare i1 i2 else c1

  let to_string {outbox_level; message_index} =
    Format.sprintf
      "Outboxlevel= %li; message_index=%s"
      outbox_level
      (Z.to_string message_index)
end

module Map = Lazy_map.Mutable.Make (OutputKey)

type t = {content : bytes Map.t; mutable level : int32; mutable id : Z.t}

let get_level output = output.level

let get_id output = output.id

let set_level output level =
  if level > output.level then (
    output.level <- level ;
    output.id <- Z.zero)

let increase_id output =
  let id = Z.succ output.id in
  if id > Z.of_int32 Int32.max_int then raise Id_too_large else output.id <- id

let alloc () = {content = Map.create (); level = 0l; id = Z.zero}
