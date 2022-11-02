module type S = sig
  open Types
  open Values

  type table

  type t = table

  type size = int32

  type index = int32

  type count = int32

  exception Type

  exception Bounds

  exception SizeOverflow

  exception SizeLimit

  exception OutOfMemory

  val alloc : table_type -> ref_ -> table (* raises Type, OutOfMemory *)

  val type_of : table -> table_type

  val size : table -> size

  val grow : table -> size -> ref_ -> unit
  (* raises SizeOverflow, SizeLimit, OutOfMemory *)

  val load : table -> index -> ref_ Lwt.t (* raises Bounds *)

  val store : table -> index -> ref_ -> unit (* raises Type, Bounds *)

  val blit : table -> index -> ref_ list -> unit (* raises Bounds *)

  (** [alloc_shallow table_ty] is like [alloc] but without a default value for
      the references in the table. That means accessing any unset element will
      raise [Lazy_map.UnexpectedAccess]. *)
  val alloc_shallow : table_type -> t

  module Vector = Lazy_vector.Mutable.Int32Vector

  val of_lazy_vector : table_type -> ref_ Vector.Vector.t -> t
end
