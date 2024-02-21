(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2024 Functori,     <contact@functori.com>                   *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module type CONVERSION_INTF = sig
  (** The generic protocol agnostic representation of a protocol type. *)
  type t

  (** Import a value of the protocol value into the generic type. *)
  val of_proto : 'a Data_encoding.t -> 'a -> t

  (** Dual operation of {!of_proto}. *)
  val to_proto : 'a Data_encoding.t -> t -> 'a

  val encoding : t Data_encoding.t

  val equal : t -> t -> bool
end

(** Generic protocol-agnostic representation of DAL skip list cells. *)
module Skip_list_cell : CONVERSION_INTF

(** Generic protocol-agnostic representation of DAL skip list hashes. *)
module Skip_list_hash : CONVERSION_INTF
