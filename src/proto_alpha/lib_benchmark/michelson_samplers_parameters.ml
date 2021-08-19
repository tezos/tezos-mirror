(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type t = {
  int_size : Base_samplers.range;
  string_size : Base_samplers.range;
  bytes_size : Base_samplers.range;
  stack_size : Base_samplers.range;
  type_size : Base_samplers.range;
  list_size : Base_samplers.range;
  set_size : Base_samplers.range;
  map_size : Base_samplers.range;
}

let encoding =
  let open Data_encoding in
  let range = Base_samplers.range_encoding in
  conv
    (fun {
           int_size;
           string_size;
           bytes_size;
           stack_size;
           type_size;
           list_size;
           set_size;
           map_size;
         } ->
      ( int_size,
        string_size,
        bytes_size,
        stack_size,
        type_size,
        list_size,
        set_size,
        map_size ))
    (fun ( int_size,
           string_size,
           bytes_size,
           stack_size,
           type_size,
           list_size,
           set_size,
           map_size ) ->
      {
        int_size;
        string_size;
        bytes_size;
        stack_size;
        type_size;
        list_size;
        set_size;
        map_size;
      })
    (obj8
       (req "int_size" range)
       (req "string_size" range)
       (req "bytes_size" range)
       (req "stack_size" range)
       (req "michelson_type_size" range)
       (req "list_size" range)
       (req "set_size" range)
       (req "map_size" range))

module type S = sig
  val parameters : t

  val size : int

  (* By default, the algo is chosen randomly (uniformly) *)
  val algo : [`Algo of Signature.algo | `Default]
end
