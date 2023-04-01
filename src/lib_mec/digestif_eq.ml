module Make (D : sig
  val digest_size : int
end) =
struct
  let _ = D.digest_size

  let equal a b = Eqaf.equal a b

  let unsafe_compare a b = String.compare a b
end
