exception Invalid_reveal_hash

type reveal_hash = string

let reveal_hash_from_string_exn str =
  if String.length str = 32 then str else raise Invalid_reveal_hash

let reveal_hash_to_string hash = hash

type reveal = Reveal_raw_data of reveal_hash | Reveal_metadata
