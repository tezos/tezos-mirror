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
      json_schema : Json.schema ;
      binary_schema : Binary_schema.t ;
    } -> t

module EncodingTable = Map.Make(String)

let table = ref EncodingTable.empty

let description = function
  | Record { description ; _ } ->
      description

let json_schema = function
  | Record { json_schema ; _ } ->
      json_schema

let binary_schema = function
  | Record { binary_schema ; _ } ->
      binary_schema

let json_pretty_printer = function
  | Record { encoding ; pp ; _ } ->
      fun fmt json ->
        match pp with
        | Some pp ->
            let json = Json.destruct encoding json in
            Format.fprintf fmt "%a" pp json
        | None ->
            Format.fprintf fmt "%a" Json.pp json

let binary_pretty_printer = function
  | Record { encoding ; pp ; _ } ->
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
  let binary_schema = Binary_description.describe encoding in
  let json_schema = Json.schema encoding in
  let description =
    match description with
    | Some description -> Some description
    | None -> lookup_inner_description encoding in
  let record = Record { encoding ; description ; pp ;
                        binary_schema ; json_schema } in
  table :=
    EncodingTable.update id (function
        | None -> Some record
        | Some x ->
            Format.eprintf "Encoding %s previously registered@." id ;
            Some x)
      !table

let find id =
  EncodingTable.find_opt id !table

let list () =
  EncodingTable.bindings !table

let bytes_of_json record json =
  match record with
  | Record { encoding ; _ } ->
      let data = Json.destruct encoding json in
      Binary_writer.to_bytes encoding data

let json_of_bytes record bytes =
  match record with
  | Record { encoding ; _ } ->
      Option.map
        ~f:(fun data -> Json.construct encoding data)
        (Binary_reader.of_bytes encoding bytes)
