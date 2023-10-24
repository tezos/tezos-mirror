(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

exception Error

open Types
open Yaml

let empty_doc = DocSpec.{summary = None; refs = []}

let mapping = function `O m -> m | _ -> raise Error

let scalar = function `Scalar {value; _} -> value | _ -> raise Error

let sequence = function
  | `A (m : Yaml.sequence) -> m.s_members
  | _ -> raise Error

let find_key_opt (m : Yaml.mapping) x : Yaml.yaml option =
  List.find_map
    (fun (k, v) ->
      match k with
      | `Scalar {value; _} -> if String.equal x value then Some v else None
      | _ -> None)
    m.m_members

let find_key m x =
  match find_key_opt m x with None -> raise Error | Some x -> x

let keys m f =
  List.map
    (fun (k, v) ->
      match k with `Scalar {value; _} -> f value v | _ -> raise Error)
    m.m_members

let parse ?file ?(path = []) s =
  let rec classSpec ?(isTopLevel = false) yaml =
    let m = mapping yaml in
    let meta =
      let content = find_key m "meta" in
      let m = mapping content in
      let id =
        match find_key_opt m "id" with
        | None -> None
        | Some i -> Some (scalar i)
      in
      MetaSpec.
        {
          path = [];
          isOpaque = false;
          id;
          endian = None;
          bitEndian = None;
          encoding = None;
          forceDebug = false;
          opaqueTypes = None;
          zeroCopySubstream = None;
          imports = [];
        }
    in
    let types =
      match find_key_opt m "types" with
      | None -> []
      | Some content ->
          let m = mapping content in
          keys m (fun k v -> (k, classSpec v))
    in
    let instances =
      match find_key_opt m "instances" with
      | None -> []
      | Some content ->
          let m = mapping content in
          keys m (fun k v -> (k, instanceSpec v))
    in
    let enums =
      match find_key_opt m "enums" with
      | None -> []
      | Some content ->
          let m = mapping content in
          keys m (fun k v -> (k, enumSpec v))
    in
    let seq =
      match find_key_opt m "seq" with
      | None -> []
      | Some content ->
          sequence content
          |> List.map (fun x ->
                 let m = mapping x in
                 let id = find_key m "id" |> scalar in
                 let dataType =
                   match find_key_opt m "type" with
                   | None -> DataType.AnyType
                   | Some (`Scalar _) -> DataType.AnyType
                   | _ -> DataType.AnyType
                 in
                 let cond =
                   AttrSpec.ConditionalSpec.{ifExpr = None; repeat = NoRepeat}
                 in
                 let cond =
                   match find_key_opt m "if" with
                   | None -> cond
                   | Some v -> {cond with ifExpr = Some (expression v)}
                 in
                 let cond =
                   match find_key_opt m "repeat" with
                   | None -> cond
                   | Some v -> (
                       match scalar v with
                       | "expr" ->
                           let e = find_key m "repeat-expr" in
                           {cond with repeat = RepeatExpr (expression e)}
                       | "until" ->
                           let e = find_key m "repeat-until" in
                           {cond with repeat = RepeatUntil (expression e)}
                       | "eos" -> {cond with repeat = RepeatEos}
                       | _ -> raise Error)
                 in
                 let valid =
                   match find_key_opt m "valid" with
                   | None -> None
                   | Some e ->
                       Some (ValidationSpec.ValidationExpr (expression e))
                 in
                 let enum =
                   match find_key_opt m "enum" with
                   | None -> None
                   | Some e -> Some (scalar e)
                 in
                 let size =
                   match find_key_opt m "size" with
                   | None -> None
                   | Some e -> Some (expression e)
                 in
                 AttrSpec.
                   {
                     path = [];
                     id;
                     dataType;
                     cond;
                     valid;
                     doc = empty_doc;
                     enum;
                     size;
                   })
    in

    let doc = empty_doc in
    let doc =
      match find_key_opt m "doc" with
      | None -> doc
      | Some v ->
          let value = scalar v in
          {doc with summary = Some value}
    in
    let doc =
      match find_key_opt m "doc-ref" with
      | None -> doc
      | Some v ->
          let refs =
            List.map (fun x -> DocSpec.TextRef (scalar x)) (sequence v)
          in
          {doc with refs}
    in

    ClassSpec.
      {
        fileName = file;
        isTopLevel;
        path;
        meta;
        doc;
        toStringExpr = None;
        params = [];
        seq;
        types;
        instances;
        enums;
      }
  and instanceSpec _ = InstanceSpec.{doc = empty_doc; descr = ParseInstanceSpec}
  and enumSpec yaml =
    let m = mapping yaml in
    let map = keys m (fun k v -> (int_of_string k, enumValueSpec v)) in
    {path = []; map}
  and enumValueSpec yaml =
    match yaml with
    | `Scalar {value; _} -> EnumValueSpec.{name = value; doc = empty_doc}
    | `O m ->
        let id = find_key m "id" |> scalar in
        let doc =
          match find_key_opt m "doc" with
          | None -> empty_doc
          | Some _ -> empty_doc
        in
        EnumValueSpec.{name = id; doc}
    | _ -> raise Error
  and expression _ = Ast.Str "TODO" in
  let yaml =
    match Yaml.yaml_of_string s with
    | Ok x -> x
    | Error (`Msg msg) -> failwith msg
  in
  classSpec ~isTopLevel:true yaml
