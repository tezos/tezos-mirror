(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Lwt.Infix

module Make (Table : Hashtbl.S) = struct
  type key = Table.key

  type 'a node = {
    mutable key : key;
    (* mutable to allow for re-use *)
    mutable value : 'a;
    (* mutable to allow for re-use *)
    mutable prev : 'a node option;
    mutable next : 'a node option;
  }

  type 'a cache = {
    table : 'a node Table.t;
    mutable size : int;
    capacity : int;
    mutable first : 'a node option;
    mutable last : 'a node option;
  }

  type 'a t = 'a cache

  let create ~capacity =
    if capacity <= 0 then
      raise (Invalid_argument "Lru_cache.create: negative or null capacity")
    else
      {
        table = Table.create capacity;
        size = 0;
        capacity;
        first = None;
        last = None;
      }

  let size cache = cache.size

  (* For testing purpose only *)
  let check_consistency cache =
    assert (cache.size <= cache.capacity) ;
    if cache.size = 0 then (
      assert (cache.first = None) ;
      assert (cache.last = None) ) ;
    if cache.size > 1 then (
      assert (cache.first <> None) ;
      assert (cache.last <> None) ) ;
    assert (cache.size = Table.length cache.table) ;
    let rec fwd_consistency acc = function
      | Some {next; _} ->
          fwd_consistency (succ acc) next
      | None ->
          assert (acc = cache.size)
    in
    fwd_consistency 0 cache.first ;
    let rec bwd_consistency acc = function
      | Some {key; prev; _} ->
          assert (Table.mem cache.table key) ;
          bwd_consistency (succ acc) prev
      | None ->
          assert (acc = cache.size)
    in
    bwd_consistency 0 cache.last

  let is_cached cache key = Table.mem cache.table key

  let push_new cache key value =
    if cache.size = 0 then (
      (* Corner-case: first value *)
      let node = {key; value; prev = None; next = None} in
      Table.replace cache.table key node ;
      cache.first <- Some node ;
      cache.last <- Some node ;
      cache.size <- 1 )
    else if cache.capacity = 1 then (
      (* Corner-case: singleton cache *)
      match cache.first with
      | None ->
          assert false (* because size = 1 *)
      | Some node ->
          Table.remove cache.table node.key ;
          node.key <- key ;
          node.value <- value ;
          Table.replace cache.table key node )
    else if cache.size = cache.capacity then (
      match cache.last with
      | None ->
          assert false (* because size > 1 *)
      | Some last ->
          Table.remove cache.table last.key ;
          ( match last.prev with
          | Some new_last ->
              cache.last <- last.prev ;
              new_last.next <- None
          | None ->
              (* because size > 1 *) assert false ) ;
          (* To avoid allocation, we re-use the [last] node *)
          last.key <- key ;
          last.value <- value ;
          last.prev <- None ;
          last.next <- cache.first ;
          ( match cache.first with
          | Some old_first ->
              old_first.prev <- Some last
          | None ->
              (* because size > 1 *) assert false ) ;
          cache.first <- Some last ;
          Table.replace cache.table key last )
    else
      let node = {key; value; prev = None; next = cache.first} in
      Table.replace cache.table key node ;
      ( match cache.first with
      | None ->
          assert false
      | Some first ->
          first.prev <- Some node ) ;
      cache.first <- Some node ;
      cache.size <- succ cache.size

  let promote cache node =
    match cache.first with
    | None ->
        assert false
    | Some node' when node == node' ->
        ()
    | Some prev_first ->
        (* promote neighbors *)
        ( match (node.prev, node.next) with
        | (None, None) ->
            ()
        | (Some prev, None) ->
            (* Node is last, promote the last as well *)
            prev.next <- None ;
            cache.last <- node.prev
        | (None, Some _next) ->
            (* first, treated above *)
            assert false
        | (Some prev, Some next) ->
            prev.next <- node.next ;
            next.prev <- node.prev ) ;
        (* promote to first *)
        prev_first.prev <- Some node ;
        node.prev <- None ;
        node.next <- cache.first ;
        cache.first <- Some node

  let get cache f key =
    match Table.find_opt cache.table key with
    | None ->
        let value = f key in
        push_new cache key value ; value
    | Some node ->
        promote cache node ; node.value

  let get_lwt cache f_lwt key =
    match Table.find_opt cache.table key with
    | None ->
        f_lwt key >>= fun value -> push_new cache key value ; Lwt.return value
    | Some node ->
        promote cache node ; Lwt.return node.value

  let get_opt_lwt cache f_opt_lwt key =
    match Table.find_opt cache.table key with
    | None -> (
        f_opt_lwt key
        >>= function
        | Some value ->
            push_new cache key value ; Lwt.return_some value
        | None ->
            Lwt.return_none )
    | Some node ->
        promote cache node ; Lwt.return_some node.value

  let push cache key value =
    match Table.find_opt cache.table key with
    | None ->
        push_new cache key value
    | Some node ->
        promote cache node

  let remove cache key =
    match Table.find_opt cache.table key with
    | None ->
        ()
    | Some node -> (
        Table.remove cache.table key ;
        cache.size <- pred cache.size ;
        ( match node.prev with
        | None ->
            cache.first <- node.next
        | Some prev ->
            prev.next <- node.next ) ;
        match node.next with
        | None ->
            cache.last <- node.prev
        | Some next ->
            next.prev <- node.prev )

  let bindings cache =
    let rec fwd acc = function
      | Some {next; key; value; _} ->
          fwd ((key, value) :: acc) next
      | None ->
          List.rev acc
    in
    fwd [] cache.first

  let clear cache =
    Table.clear cache.table ;
    cache.first <- None ;
    cache.last <- None ;
    cache.size <- 0
end
