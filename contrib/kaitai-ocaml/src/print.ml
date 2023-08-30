(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
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

open Types
open Yaml

let scalar value =
  `Scalar
    {
      anchor = None;
      tag = None;
      value;
      plain_implicit = true;
      quoted_implicit = false;
      style = `Any;
    }

let sequence l =
  `A {s_anchor = None; s_tag = None; s_implicit = true; s_members = l}

let mapping l =
  `O
    {
      m_anchor = None;
      m_tag = None;
      m_implicit = true;
      m_members = List.map (fun (k, v) -> (scalar k, v)) l;
    }

let metaSpec (t : MetaSpec.t) =
  mapping
    (List.filter_map
       (fun x -> x)
       [(match t.id with None -> None | Some id -> Some ("id", scalar id))])

let classSpec _ = mapping [("test", scalar "test")]

let instanceSpec _ = mapping [("test", scalar "test")]

let enumSpec enumspec =
  mapping
    (List.map
       (fun (v, EnumValueSpec.{name; _}) -> (string_of_int v, scalar name))
       enumspec.EnumSpec.map)

let if_not_empty = function [] -> false | _ -> true

let to_yaml (t : ClassSpec.t) =
  mapping
    (List.filter_map
       (fun (b, n, v) -> if b then Some (n, v) else None)
       [
         (true, "meta", metaSpec t.meta);
         ( if_not_empty t.types,
           "types",
           mapping (t.types |> List.map (fun (k, v) -> (k, classSpec v))) );
         ( if_not_empty t.instances,
           "instances",
           mapping (t.instances |> List.map (fun (k, v) -> (k, instanceSpec v)))
         );
         ( if_not_empty t.enums,
           "enums",
           mapping (t.enums |> List.map (fun (k, v) -> (k, enumSpec v))) );
         ( if_not_empty t.seq,
           "seq",
           sequence
             (t.seq
             |> List.map (fun v ->
                    mapping
                      (("id", scalar v.AttrSpec.id)
                       ::
                       (* We only add "type" to Yaml if not [AnyType].
                          TODO: This is only correct if [AnyType] means no type? *)
                       (if v.AttrSpec.dataType = AnyType then []
                       else
                         [
                           ( "type",
                             scalar (DataType.to_string v.AttrSpec.dataType) );
                         ])
                      @
                      match v.AttrSpec.enum with
                      | None -> []
                      | Some enum -> [("enum", scalar enum)]))) );
       ])

let print t =
  let y = to_yaml t in
  match Yaml.yaml_to_string y with Ok x -> x | Error (`Msg m) -> failwith m
