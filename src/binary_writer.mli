(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type writer_state

val make_writer_state :
  bytes -> offset:int -> allowed_bytes:int -> writer_state option

val write :
  'a Encoding.t ->
  'a ->
  writer_state ->
  (int, Binary_error_types.write_error) result

val write_opt : 'a Encoding.t -> 'a -> writer_state -> int option

val write_exn : 'a Encoding.t -> 'a -> writer_state -> int

val to_bytes :
  ?buffer_size:int ->
  'a Encoding.t ->
  'a ->
  (Bytes.t, Binary_error_types.write_error) result

val to_bytes_opt : ?buffer_size:int -> 'a Encoding.t -> 'a -> Bytes.t option

val to_bytes_exn : ?buffer_size:int -> 'a Encoding.t -> 'a -> Bytes.t

val to_string :
  ?buffer_size:int ->
  'a Encoding.t ->
  'a ->
  (string, Binary_error_types.write_error) result

val to_string_opt : ?buffer_size:int -> 'a Encoding.t -> 'a -> string option

val to_string_exn : ?buffer_size:int -> 'a Encoding.t -> 'a -> string
