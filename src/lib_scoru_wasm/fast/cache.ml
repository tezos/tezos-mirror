(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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

module type CACHE_INFO = sig
  module Key : Stdlib.Hashtbl.HashedType

  type value

  val delete : value -> unit
end

module Make (Input : CACHE_INFO) = struct
  module Table = Hashtbl.Make (Input.Key)
  module Fifo = Queue

  type key = Input.Key.t

  type value = Input.value

  let is_in_fifo key =
    Fifo.fold (fun acc elt -> acc || Input.Key.equal key elt) false

  type cache = {md : value Table.t; fifo : key Fifo.t; size : int}

  let is_in {fifo; _} key = is_in_fifo key fifo

  let create size = {md = Table.create size; fifo = Fifo.create (); size}

  let find_opt {md; _} = Table.find_opt md

  (* should stay hidden*)
  let remove_module cache key =
    Option.iter
      (fun md ->
        Input.delete md ;
        Table.remove cache.md key)
      (find_opt cache key)

  (* should stay hidden*)
  let store_module {md; _} = Table.add md

  let push_key key cache =
    Fifo.push key cache.fifo ;
    if Fifo.length cache.fifo > cache.size then
      remove_module cache (Fifo.pop cache.fifo)

  let replace cache key value =
    if is_in cache key then remove_module cache key else push_key key cache ;
    store_module cache key value
end
