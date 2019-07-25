(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type id = string

type t =
  | Record : {
      encoding : 'a Encoding.t ;
      description : string option ;
      pp : (Format.formatter -> 'a -> unit) option ;
    } -> t

module EncodingTable = Map.Make(String)

let table = ref EncodingTable.empty

let description (Record { description ; _ }) = description

let json_schema (Record { encoding ; _ }) =
  let json_schema = Json.schema encoding in
  json_schema

let binary_schema (Record { encoding ; _ }) =
  let binary_schema = Binary_description.describe encoding in
  binary_schema

let json_pretty_printer (Record { encoding ; pp ; _ }) =
  fun fmt json ->
    match pp with
    | Some pp ->
        let json = Json.destruct encoding json in
        Format.fprintf fmt "%a" pp json
    | None ->
        Format.fprintf fmt "%a" Json.pp json

let binary_pretty_printer (Record { encoding ; pp ; _ }) =
  fun fmt bytes ->
    let data = Binary_reader.of_bytes_exn encoding bytes in
    match pp with
    | Some pp ->
        Format.fprintf fmt "%a" pp data
    | None ->
        let json = Json.construct encoding data in
        Format.fprintf fmt "%a" Json.pp json

let lookup_inner_description ({ encoding ; _ } : 'a Encoding.t) =
  match encoding with
  | Describe { description ; _ } -> description
  | _ -> None

let register ~id ?description ?pp encoding =
  let description =
    match description with
    | Some description -> Some description
    | None -> lookup_inner_description encoding in
  let record = Record { encoding ; description ; pp } in
  table :=
    EncodingTable.update id (function
        | None -> Some record
        | Some _ ->
            Format.kasprintf
              Pervasives.invalid_arg
              "Encoding %s previously registered" id)
      !table

let find id =
  EncodingTable.find_opt id !table

let list () =
  EncodingTable.bindings !table

let bytes_of_json (Record { encoding ; _ }) json =
  let data = Json.destruct encoding json in
  Binary_writer.to_bytes encoding data

let json_of_bytes (Record { encoding ; _ }) bytes =
  Option.map
    ~f:(Json.construct encoding)
    (Binary_reader.of_bytes encoding bytes)
