(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** This modules introduces the table of global variables which can is
    manipulated by jobs. *)

(** The contents of a global variable. *)
type var

(** The possible types of a global variable, when it exists. If it does not,
    the global variable is assumed to be [null]. *)
type ty = String | Int | Bool

(** The description of an update to apply to the table of global variables. *)
type update = {key : string; value : string option; var_type : ty}

type t

val empty : t

(** [merge old_vars new_vars] creates a new set of global variables where the
    the variables contained in [new_vars] are added to [old_vars], and replace
    them in case of conflicts. That is, if a variable exists both in [old_vars]
    and [new_vars], then the value in [new_vars] is kept. *)
val merge : t -> t -> t

(** {1 Accessors} *)

val get : t -> string -> var

(** [update vars u] applies the update encoded in [u] to the table of global
    variables [vars]. If [u.value = None], then the variable [u.key] is removed
    from the table. *)
val update : t -> update -> t

(** {1 Templates conversions} *)

(** The table of global variables is used in conjunction with {!Jingoo}’s
    template to easily customize jobs based on the dynamic state curated by
    these states. *)

val tvalue_of_var : var -> Jingoo.Jg_types.tvalue

val tvalue_of_vars : t -> Jingoo.Jg_types.tvalue

(** {1 Encodings} *)

val encoding : t Data_encoding.t

val var_encoding : var Data_encoding.t

val updates_encoding : update list Data_encoding.t
