exception Invalid_input_hash

type input_hash = string

let input_hash_from_string_exn str =
  if String.length str = 32 then str else raise Invalid_input_hash

let input_hash_to_string hash = hash

type reveal = Reveal_raw_data of input_hash
