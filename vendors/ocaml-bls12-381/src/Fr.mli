include Ff_sig.T

(** Order of the additive group *)
val order : Z.t

(** Constructor from a Zarith element, modulo the order of the field. *)
val of_z : Z.t -> t

(** Convert into a Z element *)
val to_z : t -> Z.t

(** Returns the decimal representation as a string *)
val to_string : t -> String.t

(** Constructor from the decimal representation of the element, modulo the order
    of the field.
    Undefined behaviours if the given element is not in the field or any other
    representation than decimal is used. Use this function carefully.
 *)
val of_string : String.t -> t
