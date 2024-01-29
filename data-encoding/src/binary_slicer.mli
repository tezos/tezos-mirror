(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** Like most other [.mli] files in this directory, this is not intended for
    end-users. Instead, the interface from this file is used internally to
    assemble the end-user-intended module {!Data_encoding}. Refer to that module
    for doucmentation. *)

type slice = {name : string; value : string; pretty_printed : string}

type slicer_state

val make_slicer_state :
  string -> offset:int -> length:int -> slicer_state option

val slice :
  _ Encoding.t ->
  slicer_state ->
  (slice list, Binary_error_types.read_error) result

val slice_opt : _ Encoding.t -> slicer_state -> slice list option

val slice_exn : _ Encoding.t -> slicer_state -> slice list

val slice_string :
  _ Encoding.t -> string -> (slice list, Binary_error_types.read_error) result

val slice_string_opt : _ Encoding.t -> string -> slice list option

val slice_string_exn : _ Encoding.t -> string -> slice list

val slice_bytes :
  _ Encoding.t -> bytes -> (slice list, Binary_error_types.read_error) result

val slice_bytes_opt : _ Encoding.t -> bytes -> slice list option

val slice_bytes_exn : _ Encoding.t -> bytes -> slice list
