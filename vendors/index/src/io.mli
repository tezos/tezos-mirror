module type S = sig
  type t

  val v : readonly:bool -> fresh:bool -> generation:int64 -> string -> t

  val name : t -> string

  val offset : t -> int64

  val force_offset : t -> int64

  val readonly : t -> bool

  val read : t -> off:int64 -> bytes -> int

  val clear : t -> unit

  val sync : t -> unit

  val version : t -> string

  val set_generation : t -> int64 -> unit

  val get_generation : t -> int64

  val rename : src:t -> dst:t -> unit

  val append : t -> string -> unit
end
