(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** [escape_id id] replaces special characters in [id] to obtain a string which
    is valid to use in kaitai-struct files' [id] fields. *)
val escape_id : string -> string

(** [from_data_encoding ~id ?description encoding] generates a
    formal description of [encoding] as a kaitai [ClassSpec].

    @param [id] is escaped (no need to call [escape_id]) and added to the "meta"
    section of the class-spec. *)
val from_data_encoding :
  id:string -> 'a Data_encoding.t -> Kaitai.Types.ClassSpec.t
