open Result

(** A 1d sampling grid is either regular (the most common case)
    or explicitly specified by an array of sampling times. *)
type regular = private Regular

type explicit = private Explicit

type 'a t = private
  | Regular :
      { step : float; start : int; (* measured in [step] *)
                                   count : int }
      -> regular t
  | Explicit : { reals : float array } -> explicit t

(** The intervals are understood to be inclusive. *)
type interval =
  | Real_interval of { start : float; stop : float }
  | Index_interval of { start : int; stop : int }

type error +=
  private
  | Invalid_real : float -> error
  | Invalid_index : int -> error
  | Invalid_interval : interval -> error

val equal : 'a t -> 'a t -> bool

val regular : start:int -> step:float -> count:int -> regular t

val sampling : freq:float -> start:float -> stop:float -> regular t result

val explicit : reals:float array -> explicit t

val to_explicit : regular t -> explicit t

val index_from_real : 'a t -> float -> int result

val real_from_index : 'a t -> int -> float result

val start : regular t -> int

val step : regular t -> float

val count : 'a t -> int

val t0 : 'a t -> float

val extent : 'a t -> float

val fold : ('acc -> float -> 'acc) -> 'acc -> 'a t -> 'acc

val fold_increments : ('acc -> float -> 'acc) -> 'acc -> 'a t -> 'acc

(** Intersects a grid with an interval. *)
val inter : 'a t -> interval -> 'a t result
