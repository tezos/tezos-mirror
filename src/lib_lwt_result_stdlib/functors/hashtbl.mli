(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Make (Seq : Sigs.Seq.S) : sig
  (** Polymorphic hashing re-exported. These functions are meant to be passed to
      the [Make] and [MakeSeeded] functors (below). Check {!Stdlib.Hashtbl} for
      documentation. *)
  val hash : 'a -> int

  val seeded_hash : int -> 'a -> int

  val hash_param : meaningful:int -> total:int -> 'a -> int

  val seeded_hash_param : meaningful:int -> total:int -> int -> 'a -> int

  module type S =
    Sigs.Hashtbl.S with type 'error trace := 'error Seq.Monad.trace

  module Make (H : Stdlib.Hashtbl.HashedType) : S with type key = H.t

  module type SeededS =
    Sigs.Hashtbl.SeededS with type 'error trace := 'error Seq.Monad.trace

  module MakeSeeded (H : Stdlib.Hashtbl.SeededHashedType) :
    SeededS with type key = H.t

  module type S_LWT =
    Sigs.Hashtbl.S_LWT with type 'error trace := 'error Seq.Monad.trace

  module Make_Lwt (H : Stdlib.Hashtbl.HashedType) : S_LWT with type key = H.t
end
