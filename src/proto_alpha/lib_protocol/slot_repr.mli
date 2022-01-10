(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Slot index representation *)

(** {1 Abstract type} *)

(** A slot index is in essence a bounded whole number. That is, it is not
   allowed to overflow [max_value], nor does it wrap when calling [succ
   max_value]. In this case it returns an [Invalid_slot] error.*)
type t

val encoding : t Data_encoding.t

(** {1 Constructors }*)

val zero : t

(** Upper bound on the value a slot index can take *)
val max_value : t

(** [of_int i] creates a slot index from integer [i]. 

    @return [Error (Invalid_slot i)] if [i < 0 || i > max_value], and 
            [Ok slot] otherwise
 *)
val of_int : int -> t tzresult

(** [of_int_do_not_use_except_for_parameters i] is an unchecked construction
   function. 

   It may be used in cases where one knows [0 <= i <= max_value], e.g., when
   creating protocol parameters. 
   
   When in doubt, use [of_int] or [of_int_exn].
 *)
val of_int_do_not_use_except_for_parameters : int -> t

(** {1 Operator and pretty-printer} *)

(** [succ n] either returns an [Invalid_slot] error if [n] is [max_value] or [ok
    value] otherwise. *)
val succ : t -> t tzresult

(** {1 Conversion/Printing} *)

(** [to_int slot] returns the integral representation of a slot index. This
    value is always a whole number. *)
val to_int : t -> int

val pp : Format.formatter -> t -> unit

(** {1 Submodules} *)

module Map : Map.S with type key = t

module Set : Set.S with type elt = t

include Compare.S with type t := t

module List : sig
  (** A list of slot is an ordered list of increasing slot values *)
  type nonrec t = private t list

  val encoding : t Data_encoding.t

  (** {3 Constructors} *)
  val slot_range : min:int -> count:int -> t tzresult
end
