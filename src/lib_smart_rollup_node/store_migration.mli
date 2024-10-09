(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

(** Migrate store located in rollup node {e data} directory [data_dir] if
    needed. If there is no possible migration path registered to go from the
    current version to the last {!Store.version}, this function resolves with an
    error. *)
val maybe_run_migration :
  Metadata.t -> Store_version.t -> data_dir:string -> unit tzresult Lwt.t

(** {2 Version specific standalone migration functions} *)

module V5_sqlite_migrations : sig
  module From_v4 : sig
    (** Migration between a store V4 and an SQL store (V5) in place. *)
    val migrate_between_stores :
      Metadata.t ->
      data_dir:string ->
      dest_data_dir:string ->
      Store_sigs.ro Store_v4.store ->
      Store_sigs.rw Store_v5.t ->
      unit tzresult Lwt.t
  end
end
