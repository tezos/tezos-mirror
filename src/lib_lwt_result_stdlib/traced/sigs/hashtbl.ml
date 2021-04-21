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

(** In Lwtreslib, like in the Stdlib, the Hashtbl module exports mainly functors
    to instantiate hashtables with known-type keys. As a result, the bulk of the
    documentation for hashtables is located within the module types returned by
    the functors: in {!Traced_functor_outputs.Hashtbl}.

    Note the presence of [Make_es] which deviates from the Stdlib to provide
    specialised convenience for tables of elements the initialisation of which
    may take time and may fail. *)
module type S = sig
  type 'error trace

  val hash : 'a -> int

  val seeded_hash : int -> 'a -> int

  val hash_param : meaningful:int -> total:int -> 'a -> int

  val seeded_hash_param : meaningful:int -> total:int -> int -> 'a -> int

  module type S =
    Traced_functor_outputs.Hashtbl.S with type 'error trace := 'error trace

  module Make (H : Stdlib.Hashtbl.HashedType) : S with type key = H.t

  module type SeededS =
    Traced_functor_outputs.Hashtbl.SeededS
      with type 'error trace := 'error trace

  module MakeSeeded (H : Stdlib.Hashtbl.SeededHashedType) :
    SeededS with type key = H.t

  module type S_ES =
    Traced_functor_outputs.Hashtbl.S_ES with type 'error trace := 'error trace

  module Make_es (H : Stdlib.Hashtbl.HashedType) : S_ES with type key = H.t
end
