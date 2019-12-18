module type Ordered = sig
  type t

  val compare : t -> t -> int
end

module type Metric = sig
  type t

  val dist : t -> t -> float
end

module type Linear = sig
  type t

  val zero : t

  val ( + ) : t -> t -> t

  val ( * ) : float -> t -> t
end

module Float : sig
  type t = float

  include Ordered with type t := float

  include Linear with type t := float
end = struct
  type t = float

  let compare = Float.compare

  let zero = 0.0

  let ( + ) = ( +. )

  let ( * ) = ( *. )
end
