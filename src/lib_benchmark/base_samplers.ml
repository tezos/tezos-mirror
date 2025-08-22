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
  else invalid_arg "Base_samplers.sample_in_interval"

let sample_float_in_interval ~min ~max state =
  let diff = max -. min in
  if diff > 0. then min +. Random.State.float state diff
  else invalid_arg "Base_samplers.sample_float_in_interval"

let uniform_bool = Random.State.bool

let uniform_byte state = Char.chr (Random.State.int state 256)

let uniform_partial_byte ~nbits state =
  if nbits < 1 || nbits > 8 then
    invalid_arg "Base_samplers.uniform_partial_byte" ;
  let i = Random.State.int state 256 in
  Char.chr (i lsr (8 - nbits))

let uniform_string ~nbytes state =
  String.init nbytes (fun _ -> uniform_byte state)

let uniform_bytes ~nbytes state =
  Bytes.init nbytes (fun _ -> uniform_byte state)

let uniform_nat ~nbytes state = Z.of_bits (uniform_string state ~nbytes)

let uniform_int ~nbytes state =
  let n = uniform_nat ~nbytes state in
  if Random.State.bool state then Z.neg n else n

let nat ~size state =
  let nbytes = sample_in_interval state ~range:size in
  uniform_nat state ~nbytes

let int ~size state =
  if size.min < 0 then invalid_arg "Base_samplers.int" ;
  let nat = nat state ~size in
  let s = Random.State.bool state in
  if s then nat else Z.neg nat

let uniform_readable_ascii state =
  (* Consult the ascii table for the meaning of this. *)
  let i = Random.State.int state 96 in
  if i = 95 then '\n' else Char.chr (32 + i)

let uniform_readable_ascii_string ~nbytes state =
  String.init nbytes (fun _ -> uniform_readable_ascii state)

let readable_ascii_string ~size state =
  if size.min < 0 then invalid_arg "Base_samplers.readable_ascii_string" ;
  let nbytes = sample_in_interval state ~range:size in
  uniform_readable_ascii_string ~nbytes state

let string ~size state =
  if size.min < 0 then invalid_arg "Base_samplers.string" ;
  let nbytes = sample_in_interval state ~range:size in
  uniform_string state ~nbytes

let bytes ~size state =
  if size.min < 0 then invalid_arg "Base_samplers.bytes" ;
  let nbytes = sample_in_interval state ~range:size in
  uniform_bytes state ~nbytes

(* ------------------------------------------------------------------------- *)
(* Sampling of "adversarial" values in the sense that they exhibit the
   worst-case performance of COMPARE. *)
module Adversarial = struct
  (* random string generator with a good probabiliy that sampling [n] times
     will yield distinct results. *)
  let salt_string state (n : int) : unit -> string =
    if n <= 0 then Stdlib.failwith "salt_string: n <= 0" ;
    let salt_length = 2 * Z.log2 (Z.of_int n) in
    fun () -> uniform_string state ~nbytes:salt_length

  (* random bytes generator with a good probabiliy that sampling [n] times
     will yield distinct results. *)
  let salt_bytes state (n : int) : unit -> bytes =
    if n <= 0 then Stdlib.failwith "salt_bytes: n <= 0" ;
    let salt_length = 2 * Z.log2 (Z.of_int n) in
    fun () -> uniform_bytes state ~nbytes:salt_length

  (* Adversarial Z.t *)
  let integers ~prefix_size ~card state =
    if card <= 0 then invalid_arg "Base_samplers.Adversarial.integers" ;
    if prefix_size.min < 0 then invalid_arg "Base_samplers.Adversarial.integers" ;
    let common_prefix = string state ~size:prefix_size in
    let rand_suffix = salt_string state card in
    let elements =
      Stdlib.List.init card (fun _ ->
          Z.of_bits (rand_suffix () ^ common_prefix))
    in
    (Z.of_bits common_prefix, elements)

  (* Adversarial strings *)
  let strings ~prefix_size ~card state =
    if card <= 0 then invalid_arg "Base_samplers.Adversarial.strings" ;
    if prefix_size.min < 0 then invalid_arg "Base_samplers.Adversarial.strings" ;
    let common_prefix = string state ~size:prefix_size in
    let rand_suffix = salt_string state card in
    let elements =
      List.init ~when_negative_length:() card (fun _ ->
          common_prefix ^ rand_suffix ())
      |>
      (* see [invalid_arg] above *)
      WithExceptions.Result.get_ok ~loc:__LOC__
    in
    (common_prefix, elements)

  (* Adversarial bytes *)
  let bytes ~prefix_size ~card state =
    if card <= 0 then invalid_arg "Base_samplers.Adversarial.bytes" ;
    if prefix_size.min < 0 then invalid_arg "Base_samplers.Adversarial.bytes" ;
    let common_prefix = bytes state ~size:prefix_size in
    let rand_suffix = salt_bytes state card in
    let elements =
      List.init ~when_negative_length:() card (fun _ ->
          Bytes.cat common_prefix (rand_suffix ()))
      |>
      (* see [invalid_arg] above *)
      WithExceptions.Result.get_ok ~loc:__LOC__
    in
    (common_prefix, elements)
end
