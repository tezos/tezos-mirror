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

(** Enum definitions needed for describing data-encoding ground types. *)
module Enum : sig
  (** An [Enum.assoc] is an association list of enum id ([string]) with the
      corresponding [EnumSpec.t].

      See [Helpers.add_uniq_assoc] for handling helper. *)
  type assoc = (string * EnumSpec.t) list

  (** [bool_false_name] is the string used in the bool enum to represent the
      value [false]. *)
  val bool_false_name : string

  (** [bool_true_name] is the string used in the bool enum to represent the
      value [true]. *)
  val bool_true_name : string

  (** [bool] is an association for boolean type. *)
  val bool : string * EnumSpec.t
end

(** type definitions needed for describing data-encoding ground types. *)
module Type : sig
  (** A [Type.assoc] is an association list of type id ([string]) with the
      corresponding [ClassSpec.t] to be used in a [UserType].

      See [Helpers.add_uniq_assoc] for handling helper. *)
  type assoc = (string * ClassSpec.t) list

  (** [n_chunk] is an association for the chunks that compose [n]. *)
  val n_chunk : string * ClassSpec.t

  (** [n] is an association for n (arbitrarily large natural) type. Requires to
      also assoc [n_chunk]. *)
  val n : string * ClassSpec.t

  (** [z] is an association for z (arbitrarily large integer) type. Requires to
      also assoc [n] and [n_chunk]. *)
  val z : string * ClassSpec.t
end

(** [Attr] is module for getting [AttrSpec.t] of ground types.

    All the functions in this module take an [id] parameter. This is used for
    the [id] field of the generated attribute in the produced Kaitai Struct
    file. E.g., [float ~id:"foo"] generates the attribute

{[
    - id: foo
      type: f8
]}

 *)
module Attr : sig
  (** [bool ~id] is an [AttrSpec.t] definition of bool ground type. *)
  val bool : id:string -> AttrSpec.t

  (** [uint8 ~id] is an [AttrSpec.t] definition of 8-bit unsigned integer. *)
  val uint8 : id:string -> AttrSpec.t

  (** [int8 ~id] is an [AttrSpec.t] definition of 8-bit signed integer. *)
  val int8 : id:string -> AttrSpec.t

  (** [uint16 ~id] is an [AttrSpec.t] definition of 16-bit unsigned integer. *)
  val uint16 : id:string -> AttrSpec.t

  (** [int16 ~id] is an [AttrSpec.t] definition of 16-bit signed integer. *)
  val int16 : id:string -> AttrSpec.t

  (** [int32 ~id] is an [AttrSpec.t] definition of 32-bit signed integer. *)
  val int32 : id:string -> AttrSpec.t

  (** [int64 ~id] is an [AttrSpec.t] definition of 64-bit signed integer. *)
  val int64 : id:string -> AttrSpec.t

  (** [int31 ~id] is an [AttrSpec.t] definition of 31-bit signed integer.
      For more about this type see [Data_encoding.int31]. *)
  val int31 : id:string -> AttrSpec.t

  (** [uint30 ~id] is an [AttrSpec.t] definition of 30-bit unsigned integer.
      For more about this type see [Data_encoding.int31]. *)
  val uint30 : id:string -> AttrSpec.t

  (** [float ~id] is an [AttrSpec.t] definition of 64-bit float. *)
  val float : id:string -> AttrSpec.t

  (** [byte_size] is a description of the different kinds of sizing that bytes
      (and strings) can have in data-encoding. *)
  type byte_size =
    | Fixed of int  (** Fixed known size, makes [size: <int>] in kaitai *)
    | Dynamic of string
        (** Dynamic size header, the string is the [id] of the
                             header field, makes [size: <name>] in kaitai *)
    | Variable
        (** Unknown size (until end of stream), makes [size-eos: true]
                    in kaitai *)

  (** [bytes] is an [AttrSpec.t] definition of [Data_encoding.bytes]. See
      [byte_size] for details about this parameter. *)
  val bytes : id:string -> byte_size -> AttrSpec.t

  (** [string] is an [AttrSpec.t] definition of [Data_encoding.string]. See
      [bytes] for detail. *)
  val string : id:string -> byte_size -> AttrSpec.t

  (** [n] is an [AttrSpec.t] definition of [Data_encoding.n]. *)
  val n : id:string -> AttrSpec.t

  (** [z] is an [AttrSpec.t] definition of [Data_encoding.z]. *)
  val z : id:string -> AttrSpec.t

  (** [binary_length_kind ~id k] is an [AttrSpec.t] definition for the
    [Data_encoding__Binary_length.length] type. *)
  val binary_length_kind :
    id:string -> [`N | `Uint30 | `Uint16 | `Uint8] -> AttrSpec.t
end
