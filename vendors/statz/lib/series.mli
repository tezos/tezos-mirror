open Result

(* -------------------------------------------------------------------------- *)

type error += private Grid_dimensions_mismatch : 'a Grid.t -> Result.error

(** Type of series discretized over a grid of type 'grid and with elements
    of type 'elt. 'data is the type of the underlying representation. *)
type ('grid, 'elt, 'data) t

(** Type of admissible elements for the series. *)
type 'a elt

(** Admissible elements. *)
val float_elt : float elt

val complex_elt : Complex.t elt

(** Allowed underlying representation for the time series data. *)
type ('elt, 'data) representation

val dense_float64 : (float, Numerics.Float64.Vec.t) representation

val dense_complex64 : (Complex.t, Numerics.Complex64.Vec.t) representation

val fun_index : ('e, int -> 'e) representation

val fun_time : ('e, float -> 'e) representation

(** Get grid *)
val grid : ('g, 'e, 'a) t -> 'g Grid.t

(** Get data *)
val data : ('g, 'e, 'a) t -> 'a

(** Get length of data *)
val length : ('g, 'e, 'a) t -> int

(** Make signal from data and grid *)
val make : ('e, 'a) representation -> 'g Grid.t -> 'a -> ('g, 'e, 'a) t

(** Discretise a function over a regular grid. *)
val discretise :
  ('e, 'a) representation -> (float -> 'e) -> 'g Grid.t -> ('g, 'e, 'a) t

(** Evaluate time series at given time. This is logarithmic in the
    length of the data in case of a dense representation. *)
val eval_t : ('g, 'e, 'a) t -> float -> 'e

(** Evaluate time series at given index. This is a constant-time operation. *)
val eval_i : ('g, 'e, 'a) t -> int -> 'e

(** Map a function over a series. *)
val map :
  ('e2, 'a2) representation ->
  ('e1 -> 'e2) ->
  ('g, 'e1, 'a1) t ->
  ('g, 'e2, 'a2) t

(** Lifts a binary operator on data representations to series.
    Fails if the grids are different. *)
val lift2 :
  ('a -> 'a -> 'a) -> ('g, 'e, 'a) t -> ('g, 'e, 'a) t -> ('g, 'e, 'a) t result
