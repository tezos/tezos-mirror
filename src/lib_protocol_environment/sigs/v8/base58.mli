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

type 'a encoding

(** Decoder for a given kind of data. It returns [None] when
    the decoded data does not start with the expected prefix. *)
val simple_decode : 'a encoding -> string -> 'a option

(** Encoder for a given kind of data. *)
val simple_encode : 'a encoding -> 'a -> string

(** An extensible sum-type for decoded data: one case per known
    "prefix". See for instance [Hash.Block_hash.Hash] or
    [Environment.Ed25519.Public_key_hash]. *)
type data = ..

(** Register a new encoding. The function might raise [Invalid_arg] if
    the provided [prefix] overlaps with a previously registered
    prefix. The [to_raw] and [of_raw] are the ad-hoc
    serialisation/deserialisation for the data. The [wrap] should wrap
    the deserialised value into the extensible sum-type [data] (see
    the generic function [decode]). *)
val register_encoding :
  prefix:string ->
  length:int ->
  to_raw:('a -> string) ->
  of_raw:(string -> 'a option) ->
  wrap:('a -> data) ->
  'a encoding

(** Checks that an encoding has a certain prefix and length. *)
val check_encoded_prefix : 'a encoding -> string -> int -> unit

(** Generic decoder. It returns [None] when the decoded data does
    not start with a registered prefix. *)
val decode : string -> data option
