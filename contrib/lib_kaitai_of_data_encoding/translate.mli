(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module AnyEncoding : sig
  (** [AnyEncoding] allows to pack multiple encodings with incompatible types in the same data structure. *)

  (* The [unboxed] annotation is used that physical equality is
     preserved (e.g. [AnyEncoding e == AnyEncoding e]) *)
  type t = AnyEncoding : _ Data_encoding.Encoding.t -> t [@@ocaml.unboxed]

  val pack : _ Data_encoding.Encoding.t -> t

  module Tbl : Hashtbl.S with type key = t
end

(** [escape_id id] replaces special characters in [id] to obtain a string which
    is valid to use in kaitai-struct files' [id] fields. *)
val escape_id : string -> string

(** [from_data_encoding ~id ?description encoding] generates a
    formal description of [encoding] as a kaitai [ClassSpec].

    @param [id] is escaped (no need to call [escape_id]) and added to the "meta"
    section of the class-spec.

    @param [extern] should contain all encodings that have their kaitai spec in a dedicated ksy file.
*)
val from_data_encoding :
  id:string ->
  ?extern:string AnyEncoding.Tbl.t ->
  'a Data_encoding.t ->
  Kaitai.Types.ClassSpec.t
