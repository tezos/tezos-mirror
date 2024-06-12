(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Alpha_context

(** @return an error {!Unexpected_annotation} in the monad the list is not empty. *)
val error_unexpected_annot : Script.location -> 'a list -> unit tzresult

(** check_xxx_annot functions below are remains from the past (they were called
    parse_xxx_annot before).
    They check that annotations are well-formed and, depending on different
    contexts, that only the annotations that are expected to be found are
    present.
    Hopefully we will relax this property soon.
*)

(** Check a type annotation only. *)
val check_type_annot : Script.location -> string list -> unit tzresult

(** Check a field annotation only. *)
val is_field_annot : Script.location -> string -> bool tzresult

(** Checks whether a node has a field annotation. *)
val has_field_annot : Script.node -> bool tzresult

(** Removes a field annotation from a node. *)
val remove_field_annot : Script.node -> Script.node tzresult

(** Extract and remove a field annotation as an entrypoint from a node *)
val extract_entrypoint_annot :
  Script.node -> (Script.node * Entrypoint.t option) tzresult

(** Instruction annotations parsing *)

(** Check a variable annotation. *)
val check_var_annot : Script.location -> string list -> unit tzresult

val is_allowed_char : char -> bool

val check_constr_annot : Script.location -> string list -> unit tzresult

val check_two_var_annot : Script.location -> string list -> unit tzresult

val check_destr_annot : Script.location -> string list -> unit tzresult

val check_unpair_annot : Script.location -> string list -> unit tzresult

(** Parses a field annotation and converts it to an entrypoint.
    An error is returned if the annotation is too long or is "default".
    An empty annotation is converted to "default". *)
val parse_entrypoint_annot_strict :
  Script.location -> string list -> Entrypoint.t tzresult

(** Parse a field annotation and convert it to an entrypoint.
    An error is returned if the field annot is too long.
    An empty annotation is converted to "default". *)
val parse_entrypoint_annot_lax :
  Script.location -> string list -> Entrypoint.t tzresult

val check_var_type_annot : Script.location -> string list -> unit tzresult
