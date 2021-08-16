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

type t = Ezjsonm.value

let from_file filename =
  let ch = open_in filename in
  match Ezjsonm.value_from_channel ch with
  | exception exn ->
      close_in ch ;
      raise exn
  | json ->
      close_in ch ;
      json

let rec show ?(indent = "") ?(depth = 1) (json : t) =
  match json with
  | `Null -> "null"
  | `Bool x -> string_of_bool x
  | `Float x -> string_of_float x
  | `String x -> "\"" ^ String.escaped x ^ "\""
  | `O [] -> "{}"
  | `O fields ->
      if depth <= 0 then "{ ... }"
      else
        let show_field (name, value) =
          "\"" ^ String.escaped name ^ "\": "
          ^ show ~indent:(indent ^ "  ") ~depth:(depth - 1) value
        in
        "{\n  " ^ indent
        ^ String.concat (",\n" ^ indent ^ "  ") (List.map show_field fields)
        ^ "\n" ^ indent ^ "}"
  | `A items ->
      if depth <= 0 then "[ ... ]"
      else
        let show = show ~indent:(indent ^ "  ") ~depth:(depth - 1) in
        "[\n  " ^ indent
        ^ String.concat (",\n" ^ indent ^ "  ") (List.map show items)
        ^ "\n" ^ indent ^ "]"

let output_debug ?depth json = prerr_endline (show ?depth json)

let output json =
  Ezjsonm.value_to_channel ~minify:false stdout json ;
  print_newline ()

let get field json =
  match json with
  | `O l -> (
      match List.filter (fun (name, _) -> name = field) l with
      | [] -> None
      | [(_, value)] -> Some value
      | _ :: _ :: _ ->
          output_debug json ;
          failwith ("found multiple occurrences of: " ^ field))
  | _ ->
      output_debug json ;
      failwith "expected an object"

let as_list = function
  | `A s -> s
  | json ->
      output_debug json ;
      failwith "expected a list"

let as_int = function
  | `Float f ->
      if Float.is_integer f then Float.to_int f
      else failwith "expected an integer"
  | json ->
      output_debug json ;
      failwith "expected an integer"

let as_string = function
  | `String s -> s
  | json ->
      output_debug json ;
      failwith "expected a string"

let as_variant = function
  | `O [(name, value)] -> (name, value)
  | json ->
      output_debug json ;
      failwith "expected an object with a single field"

(* Convert an [`O] into a record.
   Ensure no field is left.
   [make] takes an argument to get field from their names,
   and if at the end it did not consume all fields,
   an exception is raised. *)
let as_record json make =
  match json with
  | `O fields ->
      let fields = ref fields in
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
      if !fields <> [] then (
        output_debug ~depth:2 json ;
        failwith
          ("some fields were ignored: "
          ^ String.concat ", " (List.map fst !fields))) ;
      result
  | _ ->
      output_debug ~depth:2 json ;
      failwith "expected an object"

let rec remove_nodes name (json : t) : t =
  match json with
  | `Null | `Bool _ | `Float _ | `String _ -> json
  | `O fields ->
      let fields =
        fields
        |> List.filter (fun (n, _) -> n <> name)
        |> List.map (fun (n, x) -> (n, remove_nodes name x))
      in
      `O fields
  | `A items -> `A (List.map (remove_nodes name) items)
