(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@tezos.com>                       *)
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

(* ------------------------------------------------------------------------- *)
(* Primitives for sampling basic data *)

type 'a sampler = Random.State.t -> 'a

(* range (inclusive) *)
type range = {min : int; max : int}

let range_encoding =
  let open Data_encoding in
  conv
    (fun {min; max} -> (min, max))
    (fun (min, max) -> {min; max})
    (obj2 (req "min" int31) (req "max" int31))

let sample_in_interval ~range:{min; max} state =
  if max - min >= 0 then min + Random.State.int state (max - min + 1)
  else invalid_arg "sample_in_interval"

let uniform_bool = Random.State.bool

let uniform_byte state = Char.chr (Random.State.int state 256)

let uniform_partial_byte ~nbits state =
  if nbits < 1 || nbits > 8 then
    Stdlib.failwith "uniform_partial_byte: invalid argument"
  else
    let i = Random.State.int state 256 in
    Char.chr (i lsr (8 - nbits))

let uniform_string ~nbytes state =
  String.init nbytes (fun _ -> uniform_byte state)

let uniform_bytes ~nbytes state =
  Bytes.unsafe_of_string (uniform_string ~nbytes state)

let uniform_nat ~nbytes state = Z.of_bits (uniform_string state ~nbytes)

let uniform_int ~nbytes state =
  let n = uniform_nat ~nbytes state in
  if Random.State.bool state then Z.neg n else n

let nat ~range state =
  let nbytes = sample_in_interval state ~range in
  uniform_nat state ~nbytes

let int ~range state =
  let nat = nat state ~range in
  let s = Random.State.bool state in
  if s then nat else Z.neg nat

let uniform_readable_ascii state =
  (* Consult the ascii table for the meaning of this. *)
  let i = Random.State.int state 93 in
  Char.chr (33 + i)

let readable_ascii_string ~range state =
  let nbytes = sample_in_interval state ~range in
  String.escaped (String.init nbytes (fun _ -> uniform_byte state))

let string ~range state =
  let nbytes = sample_in_interval state ~range in
  uniform_string state ~nbytes

let bytes ~range state =
  let nbytes = sample_in_interval state ~range in
  uniform_bytes state ~nbytes

(* ------------------------------------------------------------------------- *)
(* Sampling of "adversarial" values in the sense that they exhibit the
   worst-case performance of COMPARE. *)
module Adversarial = struct
  (* random string generator with a good probabiliy that sampling [n] times
     will yield distinct results. *)
  let salt state (n : int) : unit -> string =
    if n <= 0 then Stdlib.failwith "salt: n <= 0" ;
    let salt_length = 2 * Z.log2 (Z.of_int n) in
    fun () -> uniform_string state ~nbytes:salt_length

  (* Adversarial Z.t *)
  let integers ~range ~n state =
    if n <= 0 then invalid_arg "Base_samplers.Adversarial.integers" ;
    let common_prefix = string state ~range in
    let rand_suffix = salt state n in
    let elements =
      Stdlib.List.init n (fun _ -> Z.of_bits (rand_suffix () ^ common_prefix))
    in
    (Z.of_bits common_prefix, elements)

  (* Adversarial strings *)
  let strings ~range ~n state =
    if n <= 0 then invalid_arg "Base_samplers.Adversarial.strings" ;
    let common_prefix = string state ~range in
    let rand_suffix = salt state n in
    let elements =
      Stdlib.List.init n (fun _ -> common_prefix ^ rand_suffix ())
    in
    (common_prefix, elements)

  (* Adversarial bytes *)
  let bytes ~range ~n state =
    if n <= 0 then invalid_arg "Base_samplers.Adversarial.bytes" ;
    let (prefix, strs) = strings ~range ~n state in
    let p = Bytes.unsafe_of_string prefix in
    let ls = List.map Bytes.unsafe_of_string strs in
    (p, ls)
end
