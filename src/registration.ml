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
      encoding : 'a Encoding.t;
      description : string option;
      pp : (Format.formatter -> 'a -> unit) option;
    }
      -> t

type introspectable = Any : _ Encoding.t -> introspectable

module EncodingTable = Map.Make (String)

let table = ref EncodingTable.empty

let description (Record {description; _}) = description

let slice (Record {encoding; _}) bytes =
  Binary_slicer.slice_string encoding bytes

let slice_all bytes =
  EncodingTable.fold
    (fun enc_id (Record {encoding; _}) sliced ->
      try
        match Binary_reader.of_string encoding bytes with
        | Ok _ ->
            let slice = Binary_slicer.slice_string_exn encoding bytes in
            (enc_id, slice) :: sliced
        | Error _ -> sliced
      with
      | (Out_of_memory | Stack_overflow) as e -> raise e
      | _ -> sliced)
    !table
    []

let json_schema (Record {encoding; _}) =
  let json_schema = Json.schema encoding in
  json_schema

let binary_schema (Record {encoding; _}) =
  let binary_schema = Binary_description.describe encoding in
  binary_schema

let json_pretty_printer (Record {encoding; pp; _}) fmt json =
  match pp with
  | Some pp ->
      let json = Json.destruct encoding json in
      Format.fprintf fmt "%a" pp json
  | None -> Format.fprintf fmt "%a" Json.pp json

let binary_pretty_printer (Record {encoding; pp; _}) fmt bytes =
  let data = Binary_reader.of_bytes_exn encoding bytes in
  match pp with
  | Some pp -> Format.fprintf fmt "%a" pp data
  | None ->
      let json = Json.construct encoding data in
      Format.fprintf fmt "%a" Json.pp json

let rec lookup_id_descr : 'a. 'a Encoding.t -> _ =
  fun (type a) ({encoding; _} : a Encoding.t) ->
   match encoding with
   | Splitted {encoding; _}
   | Dynamic_size {encoding; _}
   | Check_size {encoding; _} ->
       lookup_id_descr encoding
   | Describe {id; description; _} -> Some (id, description)
   | Null | Empty | Ignore | Constant _ | Bool | Int8 | Uint8 | Int16 _
   | Uint16 _ | Int31 _ | Int32 _ | Int64 _ | N | Z | RangedInt _
   | RangedFloat _ | Float | Bytes _ | String _ | Bigstring _
   | Padded (_, _)
   | String_enum (_, _)
   | Array _ | List _ | Obj _ | Objs _ | Tup _ | Tups _ | Union _ | Mu _
   | Conv _ | Delayed _ ->
       None

let register ?pp encoding =
  match lookup_id_descr encoding with
  | None ->
      invalid_arg "Data_encoding.Registration.register: non def(in)ed encoding"
  | Some (id, description) ->
      table :=
        EncodingTable.update
          id
          (function
            | None ->
                let record = Record {encoding; description; pp} in
                Some record
            | Some _ ->
                Format.kasprintf
                  invalid_arg
                  "Encoding %s previously registered"
                  id)
          !table

let find id = EncodingTable.find_opt id !table

let find_introspectable id =
  match EncodingTable.find_opt id !table with
  | Some (Record {encoding; _}) -> Some (Any encoding)
  | None -> None

let list () = EncodingTable.bindings !table

let iter : id:string -> (introspectable -> unit) -> unit =
 fun ~id f ->
  match find_introspectable id with
  | Some introspectable -> f introspectable
  | None -> ()

let bytes_of_json (Record {encoding; _}) json =
  let data = Json.destruct encoding json in
  Binary_writer.to_bytes_opt encoding data

let json_of_bytes (Record {encoding; _}) bytes =
  match Binary_reader.of_bytes_opt encoding bytes with
  | Some v -> Some (Json.construct encoding v)
  | None -> None
