(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright 2016 OCamlPro                                                   *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Repr = struct
  (* Not for the faint of heart. *)

  type value = unit Js.t

  let repr = function
    | `String s -> Js.Unsafe.coerce (Js.string s)
    | `Float f -> Js.Unsafe.coerce (Obj.magic f)
    | `Bool true -> Js.Unsafe.coerce Js._true
    | `Bool false -> Js.Unsafe.coerce Js._false
    | `Null -> Obj.magic Js.null (* Oh, nom nom nom! *)
    | `O fields ->
        let obj = Js.Unsafe.new_obj (Js.Unsafe.pure_js_expr "Object") [||] in
        List.iter (fun (n, v) -> Js.Unsafe.set obj (Js.string n) v) fields ;
        obj
    | `A cells -> Js.Unsafe.coerce (Js.array (Array.of_list cells))

  let view v =
    match Js.to_string (Js.typeof v) with
    | "string" -> `String (Js.to_string (Js.Unsafe.coerce v))
    | "number" -> `Float (Obj.magic v)
    | "boolean" -> `Bool (Js.to_bool (Obj.magic v))
    | "undefined" -> `Null (* Oh yeah! *)
    | "object" ->
        if v == Js.Unsafe.pure_js_expr "null" then `Null
        else if Js.instanceof v (Js.Unsafe.pure_js_expr "Array") then
          let rec loop acc n =
            if n < 0 then `A acc else loop (Js.Unsafe.get v n :: acc) (n - 1)
          in
          loop [] (Js.Unsafe.get v (Js.string "length") - 1)
        else
          let fields : Js.js_string Js.t list =
            Array.to_list
            @@ Js.to_array
                 (Js.Unsafe.fun_call
                    (Js.Unsafe.js_expr
                       "(function(o){  var p=[];  for(var n in \
                        o){if(o.hasOwnProperty(n)){p.push(n);}}  return p;})")
                    [|Js.Unsafe.inject v|])
          in
          `O (List.map (fun f -> (Js.to_string f, Js.Unsafe.get v f)) fields)
    | _ -> invalid_arg "Json_repr_browser.Repr.view"

  let repr_uid = Json_repr.repr_uid ()
end

type value = Repr.value

let js_stringify ?indent obj =
  Js.Unsafe.meth_call
    (Js.Unsafe.pure_js_expr "JSON")
    "stringify"
    (match indent with
    | None -> [|Js.Unsafe.inject obj|]
    | Some indent ->
        [|
          Js.Unsafe.inject obj;
          Js.Unsafe.inject Js.null;
          Js.Unsafe.inject indent;
        |])

let parse_js_string jsstr =
  Js.Unsafe.meth_call
    (Js.Unsafe.pure_js_expr "JSON")
    "parse"
    [|Js.Unsafe.inject jsstr|]

let stringify ?indent obj = Js.to_string (js_stringify ?indent obj)

let parse str = parse_js_string (Js.string str)

module Json_encoding = Json_encoding.Make (Repr)
module Json_query = Json_query.Make (Repr)
