module Make (K : Index.Key) (V : Index.Value) :
  Index.S with type key = K.t and type value = V.t

(** These modules should not be used. They are exposed purely for testing purposes. *)
module Private : sig
  module IO : Index.IO
end
