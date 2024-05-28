(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Helpers to read JSON values.

    Contains functions to parse JSON values, show them for debugging, and
    extract information from them.

    Functions which extract information are rather strict: they don't try
    to be smart and automatically convert. For instance, [as_record] makes sure
    that all fields are taken into account. This is suited to write a tool that
    must be updated when the format of JSON values being manipulated changes.
    In our case, if the JSON schemas we read start to get more fields, we want
    to know; otherwise the resulting OpenAPI specification could be inaccurate. *)

include JSON

let as_variant json =
  match as_object json with
  | [(name, value)] -> (name, value)
  | _ -> error json "expected an object with a single field"

let as_variant_named json name =
  match as_variant json with
  | name', value when name' = name -> value
  | _ -> error json "expected a variant named %s" name

let ( |~> ) json name = as_variant_named json name

(* Convert an [`O] into a record.
   Ensure no field is left.
   [make] takes an argument to get field from their names,
   and if at the end it did not consume all fields,
   an exception is raised. *)
let as_record json make =
  let fields = ref (as_object json) in
  let get (field : string) =
    let rec find previous = function
      | [] -> None
      | ((head_name, head_value) as head) :: tail ->
          if head_name = field then (
            fields := List.rev_append previous tail ;
            Some head_value)
          else find (head :: previous) tail
    in
    find [] !fields
  in
  let result = make get in
  if !fields <> [] then
    error
      json
      "unexpected fields in object: %s"
      (String.concat ", " (List.map fst !fields)) ;
  result

let output json =
  Ezjsonm.value_to_channel ~minify:false stdout json ;
  print_newline ()
