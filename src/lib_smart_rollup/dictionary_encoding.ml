(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

let dictionary_encoding ~keys ~string_of_key ~key_of_string ~value_encoding =
  let open Data_encoding in
  let schema =
    let open Json_schema in
    let value_schema key = Data_encoding.Json.schema (value_encoding key) in
    let value_schema_r key = root (value_schema key) in
    let kind =
      Object
        {
          properties =
            List.map
              (fun key -> (string_of_key key, value_schema_r key, false, None))
              keys;
          pattern_properties = [];
          additional_properties = None;
          min_properties = 0;
          max_properties = None;
          schema_dependencies = [];
          property_dependencies = [];
        }
    in
    update
      (element kind)
      (value_schema
         (List.hd keys |> WithExceptions.Option.get ~loc:__LOC__)
         (* Dummy for definitions *))
  in
  conv
    ~schema
    (fun map ->
      let fields =
        map
        |> List.map (fun (k, v) ->
               ( string_of_key k,
                 Data_encoding.Json.construct (value_encoding k) v ))
      in
      `O fields)
    (function
      | `O fields ->
          List.map
            (fun (k, v) ->
              let k = key_of_string k in
              (k, Data_encoding.Json.destruct (value_encoding k) v))
            fields
      | _ -> assert false)
    Data_encoding.Json.encoding
