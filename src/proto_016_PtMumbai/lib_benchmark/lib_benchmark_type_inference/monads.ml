(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* Widely used module types. *)

module type S = sig
  type 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t

  val run : 'a t -> 'a
end

(* Signature of a state monad. *)
module type State_sig = sig
  type state

  type key

  type value

  include S with type 'a t = state -> 'a * state

  val empty : unit -> state

  val set : key -> value -> unit t

  val get : key -> value option t

  val iter_list : ('a -> unit t) -> 'a list -> unit t
end

module Make_state_monad (X : Stores.S) :
  State_sig
    with type state = X.state
     and type key = X.key
     and type value = X.value
     and type 'a t = X.state -> 'a * X.state = struct
  include X

  type 'a t = state -> 'a * state

  let ( >>= ) m f s =
    let x, s = m s in
    f x s

  let return x s = (x, s)

  let run m = fst (m (empty ()))

  let set k v s = ((), set k v s)

  let get k s = (get k s, s)

  let rec iter_list (f : 'a -> unit t) (l : 'a list) =
    match l with
    | [] -> return ()
    | elt :: tl -> f elt >>= fun () -> iter_list f tl
end
