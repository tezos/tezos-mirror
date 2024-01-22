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

(** Type of parameter for migration functor {!Make}. *)
module type MIGRATION_ACTIONS = sig
  (** Type of store from which data is migrated. *)
  type from_store

  (** Type of store to which the data is migrated. *)
  type dest_store

  (** Action or actions to migrate data associated to a block. NOTE:
      [dest_store] is an empty R/W store initialized in a temporary location. *)
  val migrate_block_action :
    from_store -> dest_store -> Sc_rollup_block.t -> unit tzresult Lwt.t

  (** The final actions to be performed in the migration. In particular, this is
      where data from the temporary store in [dest_store] in [tmp_dir] should be
      reported in the actual [storage_dir]. *)
  val final_actions :
    storage_dir:string ->
    tmp_dir:string ->
    from_store ->
    dest_store ->
    unit tzresult Lwt.t
end

module type S = sig
  (** Migration function for the store located in [storage_dir]. *)
  val migrate :
    Metadata.t ->
    storage_dir:string ->
    index_buffer_size:int ->
    unit tzresult Lwt.t
end

(** Functor to create and {e register} a migration. *)
module Make
    (S_from : Store_sig.S)
    (S_dest : Store_sig.S)
    (Actions : MIGRATION_ACTIONS
                 with type from_store := Store_sigs.ro S_from.t
                  and type dest_store := Store_sigs.rw S_dest.t) : S

(** Migrate store located in rollup node {e store} directory [storage_dir] if
    needed. If there is no possible migration path registered to go from the
    current version to the last {!Store.version}, this function resolves with an
    error. *)
val maybe_run_migration :
  Metadata.t ->
  storage_dir:string ->
  index_buffer_size:int ->
  unit tzresult Lwt.t
