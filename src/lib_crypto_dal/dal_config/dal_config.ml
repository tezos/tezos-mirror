(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

type parameters = {
  redundancy_factor : int;
  page_size : int;
  slot_size : int;
  number_of_shards : int;
}

let parameters_encoding =
  let open Data_encoding in
  conv
    (fun {redundancy_factor; page_size; slot_size; number_of_shards} ->
      (redundancy_factor, page_size, slot_size, number_of_shards))
    (fun (redundancy_factor, page_size, slot_size, number_of_shards) ->
      {redundancy_factor; page_size; slot_size; number_of_shards})
    (obj4
       (req "redundancy_factor" uint8)
       (req "page_size" uint16)
       (req "slot_size" int31)
       (req "number_of_shards" uint16))
  [@@coverage off]

type t = {activated : bool; use_mock_srs_for_testing : parameters option}

let encoding : t Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {activated; use_mock_srs_for_testing} ->
      (activated, use_mock_srs_for_testing))
    (fun (activated, use_mock_srs_for_testing) ->
      {activated; use_mock_srs_for_testing})
    (obj2
       (req "activated" bool)
       (req "use_mock_srs_for_testing" (option parameters_encoding)))
  [@@coverage off]

let default = {activated = false; use_mock_srs_for_testing = None}
