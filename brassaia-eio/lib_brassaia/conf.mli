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

(** {1 Configuration converters}

    A configuration converter transforms a string value to an OCaml value and
    vice-versa. *)

(** {1:keys Keys} *)

(** The type for configuration keys whose lookup value is ['a]. *)
type 'a key

type k = K : 'a key -> k

module Spec : sig
  (** A configuration spec is used to group keys by backend *)
  type t

  (** [init name] is a new configuration specification named [name] *)
  val init : string -> t

  (** [clear_all t] clears the hash table containing the specs *)
  val clear_all : unit -> unit

  (** [name spec] is the name associated with a config spec *)
  val name : t -> string

  (** [list ()] is a sequence containing all available config specs *)
  val list : unit -> t Seq.t

  (** [find name] is the config spec associated with [name] if available *)
  val find : string -> t option

  (** [find_key spec k] is the key associated with the name [k] in [spec] *)
  val find_key : t -> string -> k option

  (** [keys spec] is a sequence of keys available in [spec] *)
  val keys : t -> k Seq.t

  (** [join a b] is a new [Spec.t] combining [a] and all specs present in [b]

      The name of the resulting spec will be the name of [a] and the names of
      the specs in [b] joined by hyphens. *)
  val join : t -> t list -> t
end

(** [key ~docs ~docv ~doc ~spec name conv default] is a configuration key named
    [name] that maps to value [default] by default. It will be associated with
    the config grouping [spec]. [conv] is used to convert key values provided by
    end users.

    [docs] is the title of a documentation section under which the key is
    documented. [doc] is a short documentation string for the key, this should
    be a single sentence or paragraph starting with a capital letter and ending
    with a dot. [docv] is a meta-variable for representing the values of the key
    (e.g. ["BOOL"] for a boolean).

    @raise Invalid_argument
      if the key name is not made of a sequence of ASCII lowercase letter,
      digit, dash or underscore.
    @raise Invalid_argument
      if [allow_duplicate] is [false] (the default) and [name] has already been
      used to create a key *)
val key :
  ?docs:string ->
  ?docv:string ->
  ?doc:string ->
  ?allow_duplicate:bool ->
  spec:Spec.t ->
  string ->
  'a Type.t ->
  'a ->
  'a key

(** The key name. *)
val name : 'a key -> string

(** [tc k] is [k]'s converter. *)
val ty : 'a key -> 'a Type.t

(** [default k] is [k]'s default value. *)
val default : 'a key -> 'a

(** [doc k] is [k]'s documentation string (if any). *)
val doc : 'a key -> string option

(** [docv k] is [k]'s value documentation meta-variable (if any). *)
val docv : 'a key -> string option

(** [docs k] is [k]'s documentation section (if any). *)
val docs : 'a key -> string option

(** Default [--root=ROOT] argument. *)
val root : Spec.t -> string key

(** {1:conf Configurations} *)

(** The type for configurations. *)
type t

(** [pp] is the pretty printer for configuration values. *)
val pp : t Fmt.t

(** [equal] is the equality for configuration values. Two values are equal if
    they have the same [pp] representation. *)
val equal : t -> t -> bool

(** [spec c] is the specification associated with [c] *)
val spec : t -> Spec.t

(** [empty spec] is an empty configuration. *)
val empty : Spec.t -> t

(** [singleton spec k v] is the configuration where [k] maps to [v]. *)
val singleton : Spec.t -> 'a key -> 'a -> t

(** [is_empty c] is [true] iff [c] is empty. *)
val is_empty : t -> bool

(** [mem c k] is [true] iff [k] has a mapping in [c]. *)
val mem : t -> 'a key -> bool

(** [add c k v] is [c] with [k] mapping to [v]. *)
val add : t -> 'a key -> 'a -> t

(** [rem c k] is [c] with [k] unbound. *)
val rem : t -> 'a key -> t

(** [union r s] is the union of the configurations [r] and [s]. *)
val union : t -> t -> t

(** [find c k] is [k]'s mapping in [c], if any. *)
val find : t -> 'a key -> 'a option

(** [get c k] is [k]'s mapping in [c].

    {b Raises.} [Not_found] if [k] is not bound in [d]. *)
val get : t -> 'a key -> 'a

(** [keys c] is a sequence of all keys present in [c] *)
val keys : t -> k Seq.t

(** [with_spec t s] is the config [t] with spec [s] *)
val with_spec : t -> Spec.t -> t

(** [verify t] is an identity function that ensures all keys match the spec

    {b Raises.} [Invalid_argument] if [t] contains invalid keys *)
val verify : t -> t

(** {1:builtin_converters Built-in value converters} *)

(** [uri] converts values with {!Uri.of_string}. *)
val uri : Uri.t Type.t

(** [find_root c] is [root]'s mapping in [c], if any. *)
val find_root : t -> string option

module Env : sig
  type _ Effect.t +=
    | Fs : Eio.Fs.dir_ty Eio.Path.t Effect.t
    | Net : _ Eio.Net.t Effect.t

  val fs : unit -> Eio.Fs.dir_ty Eio.Path.t

  val net : unit -> _ Eio.Net.t
end
