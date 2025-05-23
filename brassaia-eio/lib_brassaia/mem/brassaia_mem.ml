(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module Brassaia = Brassaia_eio.Brassaia
open! Import

let src = Logs.Src.create "brassaia.mem" ~doc:"Brassaia in-memory store"

module Log = (val Logs.src_log src : Logs.LOG)

module Conf = struct
  include Brassaia.Backend.Conf

  let spec = Spec.init "mem"

  let root config = find_root config |> Option.value ~default:"."
end

module Read_only (K : Brassaia.Type.S) (V : Brassaia.Type.S) = struct
  module KMap = Map.Make (struct
    type t = K.t

    let compare = Brassaia.Type.(unstage (compare K.t))
  end)

  type key = K.t

  type value = V.t

  type 'a t = {mutable t : value KMap.t}

  let new_instance _root = {t = KMap.empty}

  let init =
    let cache : (string, 'a t) Hashtbl.t = Hashtbl.create 0 in
    fun config ->
      let root = Conf.root config in
      let t =
        match Hashtbl.find_opt cache root with
        | None ->
            let t = new_instance root in
            Hashtbl.add cache root t ;
            t
        | Some t -> t
      in
      t

  let clear t =
    [%log.debug "clear"] ;
    t.t <- KMap.empty

  let close _ =
    [%log.debug "close"] ;
    ()

  let cast t = (t :> read_write t)

  let batch t f = f (cast t)

  let pp_key = Brassaia.Type.pp K.t

  let find {t; _} key =
    [%log.debug "find %a" pp_key key] ;
    try Some (KMap.find key t) with Not_found -> None

  let mem {t; _} key =
    [%log.debug "mem %a" pp_key key] ;
    KMap.mem key t
end

module Append_only (K : Brassaia.Type.S) (V : Brassaia.Type.S) = struct
  include Read_only (K) (V)

  let add t key value =
    [%log.debug "add -> %a" pp_key key] ;
    t.t <- KMap.add key value t.t
end

module Atomic_write (K : Brassaia.Type.S) (V : Brassaia.Type.S) = struct
  module RO = Read_only (K) (V)
  module W = Brassaia.Backend.Watch.Make (K) (V)
  module L = Brassaia.Backend.Lock.Make (K)

  type t = {t : unit RO.t; w : W.t; lock : L.t}

  type key = RO.key

  type value = RO.value

  type watch = W.watch

  let watches = W.create ()

  let lock = L.create ()

  let init config =
    let t = RO.init config in
    {t; w = watches; lock}

  let close t =
    W.clear t.w ;
    RO.close t.t

  let find t = RO.find t.t

  let mem t = RO.mem t.t

  let watch_key t = W.watch_key t.w

  let watch t = W.watch t.w

  let unwatch t = W.unwatch t.w

  let list t =
    [%log.debug "list"] ;
    RO.KMap.fold (fun k _ acc -> k :: acc) t.t.RO.t []

  let set t key value =
    [%log.debug "update"] ;
    L.with_lock t.lock key (fun () ->
        t.t.RO.t <- RO.KMap.add key value t.t.RO.t) ;
    W.notify t.w key (Some value)

  let remove t key =
    [%log.debug "remove"] ;
    L.with_lock t.lock key (fun () -> t.t.RO.t <- RO.KMap.remove key t.t.RO.t) ;
    W.notify t.w key None

  let equal_v_opt = Brassaia.Type.(unstage (equal (option V.t)))

  let test_and_set t key ~test ~set =
    [%log.debug "test_and_set"] ;
    let updated =
      L.with_lock t.lock key (fun () ->
          let init = find t key in
          if equal_v_opt test init then
            let () =
              match set with
              | None -> t.t.RO.t <- RO.KMap.remove key t.t.RO.t
              | Some v -> t.t.RO.t <- RO.KMap.add key v t.t.RO.t
            in
            true
          else false)
    in
    if updated then W.notify t.w key set ;
    updated

  let clear t =
    W.clear t.w ;
    RO.clear t.t
end

let config () = Conf.empty Conf.spec

module Content_addressable = Brassaia.Content_addressable.Make (Append_only)
module S = Brassaia.Maker (Content_addressable) (Atomic_write)
module KV = Brassaia.KV_maker (Content_addressable) (Atomic_write)
include S

(* Enforce that {!S} is a sub-type of {!Brassaia.Maker}. *)
module Maker_is_a_maker : Brassaia.Maker = S

(* Enforce that {!KV} is a sub-type of {!Brassaia.KV_maker}. *)
module KV_is_a_KV : Brassaia.KV_maker = KV
