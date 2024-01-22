(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Jingoo.Jg_types

type var = tvalue

type ty = String | Int | Bool

type t = (string * var) list

type update = {key : string; value : string option; var_type : ty}

let empty = []

let cast str = function
  | String -> Tstr str
  | Int -> Tint (int_of_string str)
  | Bool -> Tbool (bool_of_string str)

let get vars key =
  match List.assoc_opt key vars with Some x -> x | None -> Tnull

let update vars {key; value; var_type} =
  let vars = List.remove_assoc key vars in
  match value with
  | Some value -> (key, cast value var_type) :: vars
  | None -> vars

let merge original_vars new_vars =
  List.fold_left
    (fun vars (key, var) ->
      let vars = List.remove_assoc key vars in
      (key, var) :: vars)
    original_vars
    new_vars

let json_of_var = function
  | Tnull -> `Null
  | Tstr s -> `String s
  | Tint i -> Ezjsonm.int i
  | Tbool b -> `Bool b
  | _ ->
      raise (Invalid_argument "json_of_var: unsupported [tvalue] constructor")

let var_of_json : Ezjsonm.value -> var = function
  | `Null -> Tnull
  | `String s -> Tstr s
  | `Float f -> Tint (int_of_float f)
  | `Bool b -> Tbool b
  | _ -> raise (Invalid_argument "var_of_json: unsupported [value] constructor")

let tvalue_of_var = Fun.id

let tvalue_of_vars vars = Tobj vars

let json_of_vars l =
  Ezjsonm.dict (List.map (fun (k, v) -> (k, json_of_var v)) l)

let vars_of_json = function
  | `O l -> List.map (fun (k, v) -> (k, var_of_json v)) l
  | _ -> raise (Invalid_argument "unsupported json for vars")

let var_encoding = Data_encoding.(conv json_of_var var_of_json Json.encoding)

let encoding = Data_encoding.(conv json_of_vars vars_of_json Json.encoding)

let var_update_of_json = function
  | `O l ->
      List.map
        (fun (key, v) ->
          match v with
          | `Null -> {key; value = None; var_type = String}
          | `String v -> {key; value = Some v; var_type = String}
          | `Bool b -> {key; value = Some (string_of_bool b); var_type = Bool}
          | `Float i ->
              {
                key;
                value = Some (int_of_float i |> string_of_int);
                var_type = Int;
              }
          | `O [("value", `String v); ("as", `String "string")] ->
              {key; value = Some v; var_type = String}
          | `O [("value", `String v); ("as", `String "bool")] ->
              {key; value = Some v; var_type = Bool}
          | `O [("value", `String v); ("as", `String "int")] ->
              {key; value = Some v; var_type = Int}
          | _ -> raise (Invalid_argument "var_update_of_json: invalid json"))
        l
  | _ -> raise (Invalid_argument "var_update_of_json: invalid json")

let json_of_var_update l =
  Ezjsonm.dict
    (List.map
       (function
         | {key; value; var_type} ->
             ( key,
               Ezjsonm.dict
                 [
                   ( "value",
                     match value with Some v -> `String v | None -> `Null );
                   ( "as",
                     `String
                       (match var_type with
                       | String -> "string"
                       | Int -> "int"
                       | Bool -> "bool") );
                 ] ))
       l)

let updates_encoding =
  Data_encoding.(conv json_of_var_update var_update_of_json Json.encoding)
