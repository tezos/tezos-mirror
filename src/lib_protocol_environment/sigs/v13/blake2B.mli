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

(** Builds a new Hash type using Blake2B. *)

(** The parameters for creating a new Hash type using
    {!Make_Blake2B}. Both {!name} and {!title} are only informative,
    used in error messages and serializers. *)

module type Name = sig
  val name : string

  val title : string

  val size : int option
end

module type PrefixedName = sig
  include Name

  val b58check_prefix : string
end

(** Builds a new Hash type using Blake2B. *)
module Make_minimal (Name : Name) : S.MINIMAL_HASH

module type Register = sig
  val register_encoding :
    prefix:string ->
    length:int ->
    to_raw:('a -> string) ->
    of_raw:(string -> 'a option) ->
    wrap:('a -> Base58.data) ->
    'a Base58.encoding
end

module Make (Register : Register) (Name : PrefixedName) : S.HASH
