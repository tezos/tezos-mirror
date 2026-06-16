(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** An entrypoint is a non-empty string of at most 31 characters *)
type t

(** Total ordering of entrypoints *)
val compare : t -> t -> int

(** Equality of entrypoints *)
val ( = ) : t -> t -> bool

(** Default entrypoint "default" *)
val default : t

(** Checks whether an entrypoint is the default entrypoint *)
val is_default : t -> bool

(** Root entrypoint "root" *)
val root : t

(** Checks whether an entrypoint is the root entrypoint *)
val is_root : t -> bool

(** Entrypoint "do" *)
val do_ : t

(** Entrypoint "set_delegate" *)
val set_delegate : t

(** Entrypoint "remove_delegate" *)
val remove_delegate : t

(** Deposit entrypoint "deposit" *)
val deposit : t

(** Entrypoint "stake" *)
val stake : t

(** Entrypoint "unstake" *)
val unstake : t

(** Entrypoint "finalize_unstake" *)
val finalize_unstake : t

(** Entrypoint "set_delegate_parameters" *)
val set_delegate_parameters : t

(** Checks whether an entrypoint is the deposit entrypoint *)
val is_deposit : t -> bool

(** Converts an annot to an entrypoint.
    Returns an error if the string is too long or is "default". *)
val of_annot_strict :
  loc:Script_repr.location -> Non_empty_string.t -> t tzresult

(** Converts a string to an entrypoint.
    Returns an error if the string is too long or is "default".
    Converts "" to "default". *)
val of_string_strict : loc:Script_repr.location -> string -> t tzresult

(** Converts a string to an entrypoint.
    Fails with [Invalid_arg] if the string is too long or is "default".
    Converts "" to "default". *)
val of_string_strict_exn : string -> t

(** Converts an annot to an entrypoint.
    Returns an error if the string is too long.
    Accepts "default". *)
val of_annot_lax : Non_empty_string.t -> t tzresult

(** Converts an annot to an entrypoint.
    Returns [None] if the string is too long.
    Accepts "default". *)
val of_annot_lax_opt : Non_empty_string.t -> t option

(** Converts a string to an entrypoint.
    Returns an error if the string is too long.
    Accepts "default" and converts "" to "default". *)
val of_string_lax : string -> t tzresult

(** Converts an entrypoint to a non-empty string.
    "default" is kept as is. *)
val to_non_empty_string : t -> Non_empty_string.t

(** Converts an entrypoint to a string.
    "default" is kept as is. *)
val to_string : t -> string

(** Converts an entrypoint to a string used as an address suffix.
    For the default entrypoint, the result is the empty string.
    Otherwise it is "%" followed by the entrypoint. *)
val to_address_suffix : t -> string

(** Converts an entrypoint to a string used as a field annotation of a
    parameter union type. It is "%" followed by the entrypoint.
    The default entrypoint is converted to "%default". *)
val unparse_as_field_annot : t -> string

(** Pretty-print an entrypoint *)
val pp : Format.formatter -> t -> unit

(** An encoding of entrypoints reusing the lax semantics.
    Decoding fails if the string is too long. "" is decoded into "default".
    "default" is encoded into "default". *)
val simple_encoding : t Data_encoding.t

(** An encoding of entrypoints reusing the strict semantics.
    Decoding fails if the string is too long or is "default".
    "" is decoded into "default".
    "default" is encoded into "". *)
val value_encoding : t Data_encoding.t

(** An optimized encoding of entrypoints, used for operations. *)
val smart_encoding : t Data_encoding.t

(** Entrypoint RPC arg. *)
val rpc_arg : t RPC_arg.t

(** In-memory size of an entrypoint *)
val in_memory_size : t -> Saturation_repr.may_saturate Saturation_repr.t

(** Set of entrypoints *)
module Set : Set.S with type elt = t

(** Map of entrypoints *)
module Map : Map.S with type key = t
