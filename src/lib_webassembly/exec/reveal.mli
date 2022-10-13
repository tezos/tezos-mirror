exception Invalid_input_hash

type input_hash

(** @raise Invalid_input_hash *)
val input_hash_from_string_exn : string -> input_hash

val input_hash_to_string : input_hash -> string

type reveal = Reveal_raw_data of input_hash | Reveal_metadata
