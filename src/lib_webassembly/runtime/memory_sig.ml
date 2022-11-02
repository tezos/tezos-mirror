module type S = sig
  open Types
  open Values

  type memory

  type t = memory

  type size = int32 (* number of pages *)

  type address = int32

  type offset = int32

  type count = int32

  exception Type

  exception Bounds

  exception SizeOverflow

  exception SizeLimit

  exception OutOfMemory

  val page_size : int64

  val alloc : memory_type -> memory (* raises Type, SizeOverflow, OutOfMemory *)

  val type_of : memory -> memory_type

  val size : memory -> size

  val bound : memory -> int64

  val grow : memory -> size -> unit
  (* raises SizeLimit, SizeOverflow, OutOfMemory *)

  val load_byte : memory -> int64 -> int Lwt.t (* raises Bounds *)

  val store_byte : memory -> int64 -> int -> unit Lwt.t (* raises Bounds *)

  val load_bytes : memory -> address -> int -> string Lwt.t (* raises Bounds *)

  val store_bytes :
    memory -> address -> string -> unit Lwt.t (* raises Bounds *)

  val load_num :
    memory -> address -> offset -> num_type -> num Lwt.t (* raises Bounds *)

  val store_num :
    memory -> address -> offset -> num -> unit Lwt.t (* raises Bounds *)

  val load_num_packed :
    pack_size ->
    extension ->
    memory ->
    address ->
    offset ->
    num_type ->
    num Lwt.t

  (* raises Type, Bounds *)
  val store_num_packed :
    pack_size -> memory -> address -> offset -> num -> unit Lwt.t
  (* raises Type, Bounds *)

  val load_vec :
    memory -> address -> offset -> vec_type -> vec Lwt.t (* raises Bounds *)

  val store_vec : memory -> address -> offset -> vec -> unit Lwt.t

  (* raises Type, Bounds *)
  val load_vec_packed :
    pack_size ->
    vec_extension ->
    memory ->
    address ->
    offset ->
    vec_type ->
    vec Lwt.t
  (* raises Type, Bounds *)

  (** [store_bytes_from_bytes memory offset bytes] copies the contents of
      [bytes] into [memory] at the provided [offset]. Further modifications to
      [bytes] are not reflected in [memory]. *)
  val store_bytes_from_bytes : t -> address -> bytes -> unit Lwt.t

  (** [of_chunks mem_ty chunks] creates a memory instance using [chunks].
      Modifications are reflected bidirectionally, i.e. modifying the memory
      changes the chunks and vice versa. *)
  val of_chunks : Types.memory_type -> Chunked_byte_vector.t -> t
end
