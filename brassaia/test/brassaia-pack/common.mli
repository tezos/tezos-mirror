(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

open Brassaia.Perms
module Int63 = Optint.Int63
module Dict : Brassaia_pack_unix.Dict.S
module I = Brassaia_pack_unix.Index
module Conf : Brassaia_pack.Conf.S
module File_manager : Brassaia_pack_unix.File_manager.S
module Io_errors = Brassaia_pack_unix.Io_errors
module Lower = Brassaia_pack_unix.Lower

module Schema :
  Brassaia.Schema.Extended
    with type Hash.t = Brassaia.Hash.SHA1.t
     and type Branch.t = string
     and type Contents.t = string

module Filename : sig
  include module type of Filename

  val quote_command :
    string ->
    ?stdin:string ->
    ?stdout:string ->
    ?stderr:string ->
    string list ->
    string
end

module Alcotest : sig
  include module type of Alcotest

  val int63 : Int63.t testable
  val kind : Brassaia_pack.Pack_value.Kind.t testable
  val hash : Schema.Hash.t testable

  val check_raises_pack_error :
    string ->
    (Brassaia_pack_unix.Errors.base_error -> bool) ->
    (unit -> _ Lwt.t) ->
    unit Lwt.t

  val check_raises_lwt : string -> exn -> (unit -> _ Lwt.t) -> unit Lwt.t
  val check_repr : 'a Brassaia.Type.t -> string -> 'a -> 'a -> unit
  val testable_repr : 'a Brassaia.Type.t -> 'a Alcotest.testable
end

module Alcotest_lwt : sig
  include module type of Alcotest_lwt

  val quick_tc : string -> (unit -> unit Lwt.t) -> unit test_case
  (** Convenience to create a `Quick test_case that doesn't need to use a
      switch. *)
end

module Index : module type of Brassaia_pack_unix.Index.Make (Schema.Hash)
module Key : Brassaia_pack_unix.Pack_key.S with type hash = Schema.Hash.t

module Pack :
  Brassaia_pack_unix.Pack_store.S
    with type hash = Schema.Hash.t
     and type key = Key.t
     and type value = string

(** Helper constructors for fresh pre-initialised dictionaries and packs *)
module Make_context (Config : sig
  val root : string
end) : sig
  val fresh_name : string -> string
  (** [fresh_name typ] is a clean directory for a resource of type [typ]. *)

  type d = { name : string; file_manager : File_manager.t; dict : Dict.t }

  val get_dict : ?name:string -> readonly:bool -> fresh:bool -> unit -> d
  val close_dict : d -> unit

  type t = {
    name : string;
    file_manager : File_manager.t;
    index : Index.t;
    pack : read Pack.t;
    dict : Dict.t;
  }

  val get_rw_pack : unit -> t Lwt.t
  val get_ro_pack : string -> t Lwt.t
  val reopen_rw : string -> t Lwt.t
  val close_pack : t -> unit Lwt.t
end

val get : 'a option -> 'a
val sha1 : string -> Schema.Hash.t
val sha1_contents : string -> Schema.Hash.t
val rm_dir : string -> unit
val index_log_size : int option
val random_string : int -> string
val random_letters : int -> string
val unlink_path : string -> unit
val create_lower_root : ?mkdir:bool -> unit -> string

val exec_cmd : string -> (unit, int) result
(** Exec a command, and return [Ok ()] or [Error n] if return code is n <> 0 *)

val setup_test_env : root_archive:string -> root_local_build:string -> unit
(** [setup_test_env ~root_archive ~root_local_build] copies an existing store to
    a temporary location, to be used by the test. *)
