(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
(* Copyright (c) 2023 DaiLambda, Inc. <contact@dailambda.jp>                 *)
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

(** Helpers for loading contexts, saving contexts, writing to contexts, etc.
    Also contains the [Key_map] module, heavily used for preparing benchmarks
    and computing statistics. *)

(* For Tezos_protocol_environment.Context *)
open Tezos_protocol_environment

val assert_ok : msg:string -> 'a tzresult -> 'a

val commit : Context.t -> Context_hash.t Lwt.t

val prepare_empty_context : string -> (Context_hash.t, tztrace) result Lwt.t

val load_context_from_disk :
  string -> Context_hash.t -> Context.t * Tezos_context.Context.index

val with_context :
  base_dir:string ->
  context_hash:Context_hash.t ->
  (Context.t -> 'a Lwt.t) ->
  'a

val prepare_base_dir : string -> unit

(** This function updates the context with random bytes at a given depth. *)
val initialize_key :
  Random.State.t -> Context.t -> Context.key -> int -> Context.t Lwt.t

val commit_and_reload :
  string ->
  Tezos_context.Context.index ->
  Context.t ->
  (Context.t * Tezos_context.Context.index) Lwt.t

(** Maps from string lists to bytes. No balancing. A key cannot be a prefix
    or a suffix to another key. *)
module Key_map : sig
  module String_map = TzPervasives.String.Map

  type 'a t = Leaf of 'a | Node of 'a t String_map.t

  val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t

  val empty : 'a t

  val is_empty : 'a t -> bool

  val insert : string list -> 'a -> 'a t -> 'a t

  val does_not_collide :
    String_map.key list ->
    'a t ->
    [> `Key_does_not_collide | `Key_exists | `Key_has_prefix | `Key_has_suffix]

  val mem : String_map.key list -> 'a t -> bool

  val find_opt : String_map.key list -> 'a t -> 'a option

  val to_seq : 'a t -> (String_map.key list * 'a) Seq.t

  val of_seq : (string list * 'a) Seq.t -> 'a t

  val fold_lwt :
    (String_map.key list -> 'a -> 'b -> 'b Lwt.t) -> 'a t -> 'b -> 'b Lwt.t

  val sample_uniform : 'a t -> (String_map.key list * 'a) option
end

val sample_without_replacement : int -> 'a list -> 'a list * 'a list

val file_copy : string -> string -> unit

val set_infos : string -> Unix.stats -> unit

val iter_dir : (string -> unit) -> string -> unit

val copy_rec : string -> string -> unit

(** Split a absolute path name.

   For example, [split_absolute_pat "/a/b/c" = Some ["a"; "b"; "c"]].
   It returns [None] for illigal paths such as ["a/b/c"], ["/a/../b"] and ["/a/b/."].
*)
val split_absolute_path : string -> string list option
