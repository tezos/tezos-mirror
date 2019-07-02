module Make (K : Index.Key) (V : Index.Value) :
  Index.S with type key = K.t and type value = V.t
