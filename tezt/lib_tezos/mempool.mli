(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** The mempool type description. *)
type t = {
  validated : string list;
  branch_delayed : string list;
  branch_refused : string list;
  refused : string list;
  outdated : string list;
  unprocessed : string list;
}

(** A comparable type for mempool where classification and ordering
   does not matter. *)
val typ : t Check.typ

(** A comparable type for mempool where ordering does not matter. *)
val classified_typ : t Check.typ

val empty : t

(** Symetric difference (union(a, b) - intersection(a, b)) *)
val symmetric_diff : t -> t -> t

(** Build a value of type {!t} from a json returned by
   {!RPC.get_mempool_pending_operations}. *)
val of_json : JSON.t -> t

(** Call [RPC.get_mempool_pending_operations] and wrap the result in a
    value of type [Mempool.t] *)
val get_mempool :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?validated:bool ->
  ?branch_delayed:bool ->
  ?branch_refused:bool ->
  ?refused:bool ->
  ?outdated:bool ->
  ?validation_passes:int list ->
  Client.t ->
  t Lwt.t

(** Check that each field of [t] contains the same elements as the
    argument of the same name. Ordening does not matter. Omitted
    arguments default to the empty list. This is useful when we expect a
    sparse mempool. *)
val check_mempool :
  ?validated:string list ->
  ?branch_delayed:string list ->
  ?branch_refused:string list ->
  ?refused:string list ->
  ?outdated:string list ->
  ?unprocessed:string list ->
  t ->
  unit

(** Mempool filter configuration. *)
module Config : sig
  (** Representation of the mempool configuration. [None] means that
      the field has been or will be omitted in the json encoding, in
      which case they are equal to the default values given below. *)
  type t = {
    minimal_fees : int option;
    minimal_nanotez_per_gas_unit : (int * int) option;
    minimal_nanotez_per_byte : (int * int) option;
    replace_by_fee_factor : (int * int) option;
    max_operations : int option;
    max_total_bytes : int option;
  }

  (** Build the Ezjsonm representation then encode it as a string. *)
  val to_string : t -> string

  (** Call [Check.( = )] on two configurations. *)
  val check_equal : t -> t -> unit

  (** Return the config corresponding to the given json. If any field
      is missing, it is set to [None]. *)
  val of_json : JSON.t -> t

  (** Default value for the [minimal_fees] field
      (see src/proto_alpha/lib_plugin/plugin.ml) *)
  val default_minimal_fees : int

  (** Default value for the [minimal_nanotez_per_gas_unit] field
      (see src/proto_alpha/lib_plugin/plugin.ml) *)
  val default_minimal_nanotez_per_gas_unit : int * int

  (** Default value for the [minimal_nanotez_per_byte] field
      (see src/proto_alpha/lib_plugin/plugin.ml) *)
  val default_minimal_nanotez_per_byte : int * int

  (** Default value for the [replace_by_fee_factor] field
      (see src/proto_alpha/lib_plugin/plugin.ml) *)
  val default_replace_by_fee_factor : int * int

  (** Default value for the [max_operations] field
      (see src/lib_shell/prevalidator_bounding.ml) *)
  val default_max_operations : int

  (** Default value for the [max_total_bytes] field
      (see src/lib_shell/prevalidator_bounding.ml) *)
  val default_max_total_bytes : int

  (** Configuration where each field is explicitely set to its default value. *)
  val default : t

  (** Returns a copy of the given filter config, where missing fields
      (i.e. containing [None]) have been set to their default value. *)
  val fill_with_default : t -> t

  (** Call the RPC [GET /chains/main/mempool/filter] with query
      parameter [include_default]. *)
  val call_get_filter :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?include_default:bool ->
    Client.t ->
    JSON.t Lwt.t

  (** Check that the RPC [GET /chains/main/mempool/filter] returns the json
      corresponding to the provided {!t}, testing all possibilities for
      the optional argument [include_default] (omitted/[true]/[false]). *)
  val check_get_filter_all_variations :
    ?log:bool ->
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    t ->
    Client.t ->
    unit Lwt.t

  (** Call the RPC [POST /chains/main/mempool/filter] with data set to
      the json representation of the given config {!t}.

      @param log When true, log the input config at the info
      level. Defaults to false. *)
  val post_filter :
    ?log:bool ->
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    t ->
    Client.t ->
    JSON.t Lwt.t

  (** Same as {!post_filter} but takes the data config as a string. *)
  val post_filter_str :
    ?log:bool ->
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    string ->
    Client.t ->
    JSON.t Lwt.t

  (** Set the mempool filter config to the provided parameters. Note
      that every omitted parameter will be set back to its default
      value, even if it previously held a different value.

      More precisely, call {!post_filter} on the config corresponding
      to the arguments named after config fields. Then, check that the
      config returned by the RPC is the same as the input one (where
      each omitted field has been filled with its default value).

      @param log is passed on to {!post_filter}. *)
  val set_filter :
    ?log:bool ->
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?minimal_fees:int ->
    ?minimal_nanotez_per_gas_unit:int * int ->
    ?minimal_nanotez_per_byte:int * int ->
    ?replace_by_fee_factor:int * int ->
    ?max_operations:int ->
    ?max_total_bytes:int ->
    Client.t ->
    unit Lwt.t
end
