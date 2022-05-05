(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type typ = Void | Int | Buffer | Uint8 | Uint32 | Bool

type arg_name = string

type arg = {
  name : arg_name;
  kind : [`Input | `Output];
  typ : typ;
  index : int option;
  size : [`Absolute of int | `Relative of arg_name * int] option;
}

type t = {
  js_mod_name : string;
  js_fun_name : string;
  wasm_fun_name : string;
  return : typ;
  args : arg list;
}

let field name l = List.assoc name (Ezjsonm.get_dict l)

let field_opt name l = List.assoc_opt name (Ezjsonm.get_dict l)

let parse_size = function
  | `String name -> (
      match (String.split_on_char '+' name, String.split_on_char '-' name) with
      | ([name; plus], [_]) -> `Relative (name, int_of_string plus)
      | ([_], [name; minus]) -> `Relative (name, int_of_string minus)
      | ([_], [_]) -> `Relative (name, 0)
      | ([], _) | (_, []) | ([_], _) | (_, [_]) -> assert false
      | _ -> assert false)
  | `Float f -> `Absolute (int_of_float f)
  | _ -> assert false

let parse_typ json =
  match Ezjsonm.get_string json with
  | "void" -> Void
  | "int" -> Int
  | "buffer" -> Buffer
  | "bool" -> Bool
  | _ -> assert false

let parse_arg json =
  let name = Ezjsonm.get_string (field "name" json) in
  let kind =
    match Ezjsonm.get_string (field "kind" json) with
    | "input" -> `Input
    | "output" -> `Output
    | _ -> assert false
  in
  let typ = parse_typ (field "type" json) in
  let index = Option.map Ezjsonm.get_int (field_opt "interface_index" json) in
  let size = Option.map parse_size (field_opt "size" json) in
  {name; kind; typ; index; size}

let parse_entry ~js_mod_name ~js_fun_name json =
  let wasm_module_name = Ezjsonm.get_string (field "module" json) in
  let wasm_fun_name = Ezjsonm.get_string (field "name" json) in

  let custom_name =
    try Ezjsonm.get_bool (field "custom_module_name" json) with _ -> false
  in
  let wasm_fun_name =
    if custom_name then wasm_fun_name
    else wasm_module_name ^ "_" ^ wasm_fun_name
  in
  let args = Ezjsonm.get_list parse_arg (field "args" json) in
  let return = parse_typ (field "type" (field "return" json)) in
  {js_mod_name; js_fun_name; wasm_fun_name; args; return}

let parse_file file : t list =
  let ic = open_in file in
  let json = Ezjsonm.from_channel ic in
  close_in ic ;
  List.concat_map
    (fun (js_mod_name, json) ->
      List.map
        (fun (js_fun_name, json) -> parse_entry ~js_mod_name ~js_fun_name json)
        (Ezjsonm.get_dict json))
    (Ezjsonm.get_dict json)
