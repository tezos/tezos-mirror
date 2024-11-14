(** [Raw] wraps a file-descriptor with an file-format used internally by Index.
    The format contains the following header fields:

    - {b offset}: a 64-bit integer, denoting the length of the file containing
      valid data;
    - {b version}: an 8-byte version string;
    - {b generation}: a 64-bit integer denoting the generation number;
    - {b fan}: a 64-bit length field, followed by a string containing that many
      bytes. *)

open! Import

type t
(** The type of [raw] file handles. *)

val v : Unix.file_descr -> t
(** Construct a [raw] value from a file descriptor. *)

val unsafe_write : t -> off:int63 -> string -> int -> int -> unit
val unsafe_read : t -> off:int63 -> len:int -> bytes -> int
val fsync : t -> unit
val close : t -> unit
val fstat : t -> Unix.stats

exception Not_written

module Version : sig
  val get : t -> string
  val set : t -> string -> unit
end

module Offset : sig
  val get : t -> int63
  val set : t -> int63 -> unit
end

module Generation : sig
  val get : t -> int63
  val set : t -> int63 -> unit
end

module Fan : sig
  val get : t -> string
  val set : t -> string -> unit
  val get_size : t -> int63
  val set_size : t -> int63 -> unit
end

module Header : sig
  type raw

  type t = {
    offset : int63;  (** The length of the file containing valid data *)
    version : string;  (** Format version *)
    generation : int63;  (** Generation number *)
  }

  val get : raw -> t
  val set : raw -> t -> unit
end
with type raw := t

(** Functions for interacting with the header format {i without} the generation
    number, provided for use in [irmin-pack]. *)
module Header_prefix : sig
  type raw

  type t = {
    offset : int63;  (** The length of the file containing valid data *)
    version : string;  (** Format version *)
  }

  val get : raw -> t
  val set : raw -> t -> unit
end
with type raw := t
