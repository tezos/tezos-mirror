type bit = int

module Bit : sig
  type t

  val of_bytes_le : Bytes.t -> t

  val is_processed : t -> bool

  val of_bool_list : bool list -> t

  val next : t -> bit option

  val get_chunk : t -> ?default:int -> int -> bit list
end
