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

(* Various implementations of Monads.Store_sig *)

(* Signature of a persistent store. *)
module type S = sig
  type state

  type key

  type value

  val empty : unit -> state

  val set : key -> value -> state -> state

  val get : key -> state -> value option

  val map : (value -> value) -> state -> state

  val to_string : state -> string
end

module type Map_store_param_sig = sig
  type key

  type value

  val key_to_string : key -> string

  val value_to_string : value -> string
end

(* An implemention of [S] using maps. *)
module Map (M : Map.S) (V : Map_store_param_sig with type key = M.key) :
  S with type state = V.value M.t and type key = M.key and type value = V.value =
struct
  type state = V.value M.t

  type key = M.key

  type value = V.value

  let empty () = M.empty

  let set = M.add

  let get = M.find_opt

  let map = M.map

  let to_string s =
    M.fold
      (fun key node acc ->
        Printf.sprintf
          "%s\n%s  |->  %s"
          acc
          (V.key_to_string key)
          (V.value_to_string node))
      s
      ""
end
