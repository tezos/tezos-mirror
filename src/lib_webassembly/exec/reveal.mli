exception Invalid_reveal_hash

type reveal_hash

(** @raise Invalid_reveal_hash *)
val reveal_hash_from_string_exn : string -> reveal_hash

val reveal_hash_to_string : reveal_hash -> string

type reveal = Reveal_raw_data of reveal_hash | Reveal_metadata
