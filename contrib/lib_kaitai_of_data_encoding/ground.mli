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

  (** [bool] is an association for boolean type. *)
  val bool : string * EnumSpec.t
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

  (** [float ~id] is an [AttrSpec.t] definition of 64-bit float. *)
  val float : id:string -> AttrSpec.t

  (** [bytes ~id] is an [AttrSpec.t] definition of [Data_encoding.bytes]. *)
  val bytes : id:string -> AttrSpec.t

  (** [string ~id] is an [AttrSpec.t] definition of [Data_encoding.string]. *)
  val string : id:string -> AttrSpec.t
end

(** [Class] module consists of [ClassSpec.t] for ground types. *)
module Class : sig
  (** [bool] returns [ClassSpec.t] definition of bool ground type. *)
  val bool : encoding_name:string -> ?description:string -> unit -> ClassSpec.t

  (** [uint8] returns [ClassSpec.t] definition of 8-bit unsigned integer. *)
  val uint8 : encoding_name:string -> ?description:string -> unit -> ClassSpec.t

  (** [int8] returns [ClassSpec.t] definition of 8-bit signed integer. *)
  val int8 : encoding_name:string -> ?description:string -> unit -> ClassSpec.t

  (** [uin16] returns [ClassSpec.t] definition of 16-bit unsigned integer. *)
  val uint16 :
    encoding_name:string -> ?description:string -> unit -> ClassSpec.t

  (** [int16] returns [ClassSpec.t] definition of 16-bit signed integer. *)
  val int16 : encoding_name:string -> ?description:string -> unit -> ClassSpec.t

  (** [int32] returns [ClassSpec.t] definition of 32-bit signed integer. *)
  val int32 : encoding_name:string -> ?description:string -> unit -> ClassSpec.t

  (** [int64] returns [ClassSpec.t] definition of 64-bit signed integer. *)
  val int64 : encoding_name:string -> ?description:string -> unit -> ClassSpec.t

  (** [int31] returns [ClassSpec.t] definition of 31-bit signed integer.
      For more about this type see [Data_encoding.int31]. *)
  val int31 : encoding_name:string -> ?description:string -> unit -> ClassSpec.t

  (** [float] returns [ClassSpec.t] definition of 64-bit float. *)
  val float : encoding_name:string -> ?description:string -> unit -> ClassSpec.t

  (** [bytes] returns [ClassSpec.t] definition of [Data_encoding.bytes]. *)
  val bytes : encoding_name:string -> ?description:string -> unit -> ClassSpec.t

  (** [string] returns [ClassSpec.t] definition of [Data_encoding.string]. *)
  val string :
    encoding_name:string -> ?description:string -> unit -> ClassSpec.t

  (** [byte_group] represents a user defined type for a variable-length sequence
      of bytes encoding a Zarith natural number. It is used for describing
      encoding such as [Data_encoding.Z] and [Data_encoding.N].

      As from the [Data_encoding] documentation: "each byte has a running unary
      size bit: the most significant bit of each byte indicates whether this is
      the last byte in the sequence (0) or whether the sequence continues (1).
      Size bits ignored, the data is the binary representation of the number 
      in little-endian order." *)
  val byte_group : ClassSpec.t

  (** [n] returns [ClassSpec.t] for [Data_encoding.N]. *)
  val n : encoding_name:string -> ?description:string -> unit -> ClassSpec.t

  (** [z] returns [ClassSpec.t] for [Data_encoding.Z]. *)
  val z : encoding_name:string -> ?description:string -> unit -> ClassSpec.t
end
