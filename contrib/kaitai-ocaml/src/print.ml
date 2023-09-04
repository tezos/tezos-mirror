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

let ( @? ) x xs = match x with None -> xs | Some x -> x :: xs

let metaSpec (t : MetaSpec.t) =
  mapping
  @@ Option.map (fun id -> ("id", scalar id)) t.id
  @? Option.map
       (fun endian -> ("endian", scalar (Endianness.to_string endian)))
       t.endian
  @? []

let instanceSpec _ = mapping [("test", scalar "test")]

let instances_spec instances =
  mapping (instances |> List.map (fun (k, v) -> (k, instanceSpec v)))

let enumSpec enumspec =
  mapping
    (List.map
       (fun (v, EnumValueSpec.{name; _}) -> (string_of_int v, scalar name))
       enumspec.EnumSpec.map)

let enums_spec enums =
  mapping (enums |> List.map (fun (k, v) -> (k, enumSpec v)))

(** We only add "type" to Yaml if not [AnyType].
    TODO: This is only correct if [AnyType] means no type? *)
let attr_type_if_not_any attr =
  if attr.AttrSpec.dataType = AnyType then None
  else Some ("type", scalar (DataType.to_string attr.AttrSpec.dataType))

let attr_spec attr =
  match attr.AttrSpec.dataType with
  (* [BytesType] attr require size header. *)
  | BytesType (BytesLimitType {size; _}) ->
      [
        mapping
          (Some ("id", scalar attr.AttrSpec.id)
          @? Some ("size", scalar (Ast.to_string size))
          @? []);
      ]
  | _ ->
      [
        mapping
          (Some ("id", scalar attr.AttrSpec.id)
          @? attr_type_if_not_any attr
          @? Option.map (fun enum -> ("enum", scalar enum)) attr.AttrSpec.enum
          @? []);
      ]

let seq_spec seq = sequence (List.concat_map attr_spec seq)

let not_empty = function [] -> false | _ -> true

let spec_if_non_empty name args f =
  if not_empty args then Some (name, f args) else None

let rec to_yaml (t : ClassSpec.t) =
  mapping
  @@ (if t.isTopLevel then Some ("meta", metaSpec t.meta) else None)
  @? spec_if_non_empty "types" t.types types_spec
  @? spec_if_non_empty "instances" t.instances instances_spec
  @? spec_if_non_empty "enums" t.enums enums_spec
  @? spec_if_non_empty "seq" t.seq seq_spec
  @? []

and types_spec types = mapping (types |> List.map (fun (k, v) -> (k, to_yaml v)))

let print t =
  let y = to_yaml t in
  match Yaml.yaml_to_string y with Ok x -> x | Error (`Msg m) -> failwith m
