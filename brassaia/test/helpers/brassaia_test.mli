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

module type S = Common.S
module type Generic_key = Common.Generic_key

val reporter : ?prefix:string -> unit -> Logs.reporter

module Suite : sig
  type t

  val create :
    name:string ->
    ?init:(config:Brassaia.config -> unit Lwt.t) ->
    ?clean:(config:Brassaia.config -> unit Lwt.t) ->
    config:Brassaia.config ->
    store:(module S) ->
    ?stats:(unit -> int * int) ->
    ?import_supported:bool ->
    unit ->
    t

  val create_generic_key :
    name:string ->
    ?init:(config:Brassaia.config -> unit Lwt.t) ->
    ?clean:(config:Brassaia.config -> unit Lwt.t) ->
    config:Brassaia.config ->
    store:(module Generic_key) ->
    ?stats:(unit -> int * int) ->
    ?import_supported:bool ->
    unit ->
    t

  val name : t -> string
  val config : t -> Brassaia.config
  val store : t -> (module S) option
  val init : t -> config:Brassaia.config -> unit Lwt.t
  val clean : t -> config:Brassaia.config -> unit Lwt.t
end

val line : string -> unit

module Schema = Common.Schema

val store : (module Brassaia.Maker) -> (module S)
val testable : 'a Brassaia.Type.t -> 'a Alcotest.testable
val check : 'a Brassaia.Type.t -> string -> 'a -> 'a -> unit
val checks : 'a Brassaia.Type.t -> string -> 'a list -> 'a list -> unit

module Store : sig
  val run :
    __FILE__:string ->
    string ->
    ?slow:bool ->
    ?random_seed:int ->
    sleep:(float -> unit Lwt.t) ->
    misc:unit Alcotest_lwt.test list ->
    (Alcotest.speed_level * Suite.t) list ->
    unit Lwt.t
end

module Node = Node
