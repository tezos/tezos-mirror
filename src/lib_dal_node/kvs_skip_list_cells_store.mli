(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2024 Functori,     <contact@functori.com>                   *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(**
   This module instantiates the key value store to provide facilities for
   storing and retrieving the cells of the DAL skip list. The store maintains:

   - A mapping from hashes to cells, to allow retrieval of the cell associated
   with a hash. This is useful while constructing a DAL refutation proof.

   - A mapping from (attested) levels to hashes, to allow removing data related
   to a level once they cannot be used anymore for refutation games.

   The store only supports storing values with a fixed-size encoding (as it uses
   the {!Key_value_store}). Because the length of skip lists' cells slowly
   increases over time (because of new pointers), a sufficiently large maximum
   size is provided at initialization time; the binary representation of cells
   is padded accordingly before storing them.
*)

type t

(** [init ~node_store_dir ~skip_list_store_dir ~padded_encoded_cell_size
    ~encoded_hash_size] returns a new skip lists store under
    the path [node_store_dir/skip_list_store_dir]. The
    [padded_encoded_cell_size] parameter specifies the extended size of cells
    once encoded and stored as bytes in the key-value store. The
    [encoded_hash_size] parameter indicates the size of hashes once encoded as
    fixed-size bytes. *)
val init :
  node_store_dir:string ->
  skip_list_store_dir:string ->
  padded_encoded_cell_size:int ->
  encoded_hash_size:int ->
  t tzresult Lwt.t

(** [insert store ~attested_level values] inserts the given list of [values]
    associated to the given [attested_level] in the [store]. Any existing value
    is overridden. *)
val insert :
  t ->
  attested_level:int32 ->
  (Dal_proto_types.Skip_list_hash.t * Dal_proto_types.Skip_list_cell.t) list ->
  unit tzresult Lwt.t

(** [find store hash] returns the cell associated to [hash] in the [store], if
    any. *)
val find :
  t ->
  Dal_proto_types.Skip_list_hash.t ->
  Dal_proto_types.Skip_list_cell.t Error_monad.tzresult Lwt.t

(** [remove store ~attested_level] removes any data related to [attested_level]
    from the [store]. *)
val remove :
  t -> attested_level:int32 -> (unit, Error_monad.tztrace) result Lwt.t

(** [close store] waits until all pending reads and writes are
    completed and closes the store. *)
val close : t -> (unit, Error_monad.tztrace) result Lwt.t

(** Internal functions for testing purpose.  *)
module Internal_for_tests : sig
  val skip_list_hash_exists :
    t -> Dal_proto_types.Skip_list_hash.t -> bool tzresult Lwt.t
end

(** Internal functions for storage migrations. *)
module Internal_for_migrations : sig
  (** [get_attested_levels store] returns the attested levels
      registered in the [store]. Since the number of attested levels
      can be large, it returns a stream. *)
  val get_attested_levels : t -> int32 Lwt_stream.t

  (** [find_hash store ~attested_level ~slot_index] returns the cell hash
      associated to the [attested_level] and the [slot_index] in the
      [store], if any. *)
  val find_hash :
    t ->
    attested_level:int32 ->
    slot_index:int ->
    Dal_proto_types.Skip_list_hash.t tzresult Lwt.t
end
