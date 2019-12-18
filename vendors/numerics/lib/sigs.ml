module type Vector_sig =
sig
  type elt
  type t

  val create : int -> t
  val length : t -> int
  val copy : t -> t
  val init : int -> (int -> elt) -> t
  val get : t -> int -> elt
  val unsafe_get : t -> int -> elt
  val set : t -> int -> elt -> unit
  val unsafe_set : t -> int -> elt -> unit
  val concat : t -> t -> t
  val of_array : elt array -> t
  val to_array : t -> elt array
  val add : t -> t -> t
  val mul : t -> t -> t
  val linspace : elt -> elt -> int -> t
end

type ('e, 'a) vector_impl = (module Vector_sig with type elt = 'e
                                                and type t = 'a)

module type Matrix_sig =
sig
  type elt
  type t

  val create : lines:int -> cols:int -> t
  val dim1 : t -> int
  val dim2 : t -> int
  val shape : t -> int * int
  val copy : t -> t
  val init : lines:int -> cols:int -> f: (int -> int -> elt) -> t
  val get : t -> int -> int -> elt
  val unsafe_get : t -> int -> int -> elt
  val set : t -> int -> int -> elt -> unit
  val unsafe_set : t -> int -> int -> elt -> unit
  val concat_horiz : t -> t -> t
  val add : t -> t -> t
  val mul : t -> t -> t
end

type ('e, 'a) matrix_impl = (module Matrix_sig with type elt = 'e
                                                and type t = 'a)
