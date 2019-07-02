module type ELT = sig
  type t

  val encoded_size : int

  val decode : Bytes.t -> int -> t
end

module type S = sig
  include Search.ARRAY

  type io

  val v : io -> t
end

(** Takes an IO instance and wraps it in an Array interface with support for
    prefetching sections of the array. *)
module Make (IO : Io.S) (Elt : ELT) :
  S with type io = IO.t and type elt = Elt.t
