module Make (K : Index.Key) (V : Index.Value) :
  Index.S with type key = K.t and type value = V.t

(** These modules should not be used. They are exposed purely for testing
    purposes. *)
module Private : sig
  module IO : Index.IO
end

type stats = {
  mutable bytes_read : int;
  mutable nb_reads : int;
  mutable bytes_written : int;
  mutable nb_writes : int;
}

val reset_stats : unit -> unit

val get_stats : unit -> stats
