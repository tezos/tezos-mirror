(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Kaitai.Types

(** [default_doc_spec] is without summary and references.  *)
val default_doc_spec : DocSpec.t

(** [Enum] module defines enum definitions needed for describing data-encoding
    ground types. *)
module Enum : sig
  (** [map] describes mapping of enum id (string) with the corresponding
      [EnumSpec.t]. *)
  type map = (string * EnumSpec.t) list

  (** [bool] is a mapping for boolean type. *)
  val bool : string * EnumSpec.t

  (** [add enums enum] returns a list of enum mappings. If [enums] don't contain
      [enum], then new list with it is returned, otherwise existing [enums] list
      is returned. *)
  val add : map -> string * EnumSpec.t -> map
end

(** [Attr] is module for getting [AttrSpec.t] of ground types. *)
module Attr : sig
  (** [bool] returns [AttrSpec.t] definition of bool ground type. *)
  val bool : AttrSpec.t

  (** [u1] returns [AttrSpec.t] definition of 8-bit unsigned integer. *)
  val u1 : AttrSpec.t
end
