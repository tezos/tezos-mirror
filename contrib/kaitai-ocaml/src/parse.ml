(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

exception Error of string

open Types
open Yaml

let empty_doc = DocSpec.{summary = None; refs = []}

let mapping = function `O m -> m | _ -> raise (Error "Expecting yaml mapping")

let scalar = function
  | `Scalar {value; _} -> value
  | _ -> raise (Error "scalar")

let bool = function
  | `Scalar {value = "true"; _} -> true
  | `Scalar {value = "false"; _} -> false
  | _ -> raise (Error "Expecting yaml bool")

let sequence = function
  | `A (m : Yaml.sequence) -> m.s_members
  | _ -> raise (Error "Expecting yaml sequence")

let find_key_opt (m : Yaml.mapping) x : Yaml.yaml option =
  List.find_map
    (fun (k, v) ->
      match k with
      | `Scalar {value; _} -> if String.equal x value then Some v else None
      | _ -> None)
    m.m_members

let find_key m x =
  match find_key_opt m x with
  | None -> raise (Error (Printf.sprintf "find_key(%s)" x))
  | Some x -> x

let keys m f =
  List.map
    (fun (k, v) ->
      match k with
      | `Scalar {value; _} -> f value v
      | _ -> raise (Error "key is not scalar"))
    m.m_members

let doc m =
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
          List.map
            (fun x ->
              let x = scalar x in
              if
                String.starts_with ~prefix:"http://" x
                || String.starts_with ~prefix:"https://" x
              then
                match String.split_on_char ' ' x with
                | [] -> assert false
                | [url] -> DocSpec.UrlRef {url; text = "Source"}
                | url :: rest ->
                    DocSpec.UrlRef {url; text = String.concat " " rest}
              else DocSpec.TextRef x)
            (sequence v)
        in
        {doc with refs}
  in
  doc

let expression : _ -> Ast.t = function
  | `Scalar {value = s; _} -> (
      try
        let lexbuf = Lexing.from_string s in
        Parser.expression Lexer.token lexbuf
      with e ->
        Printf.eprintf
          "Failed to parse, fallback to raw: %s\n%s\n"
          s
          (Printexc.to_string e) ;
        Raw s)
  | _ -> raise (Error "expression is not a scalar")

let dataType x _extra : DataType.t =
  let exception Not_builtin in
  match x with
  | `Scalar _ as x -> (
      let raw_s = scalar x in
      try
        let prefix, endian =
          if String.ends_with raw_s ~suffix:"be" then
            (String.sub raw_s 0 (String.length raw_s - 2), Some `BE)
          else if String.ends_with raw_s ~suffix:"le" then
            (String.sub raw_s 0 (String.length raw_s - 2), Some `LE)
          else (raw_s, None)
        in
        if String.equal prefix "" then raise Not_builtin ;
        let start = String.get prefix 0 in
        let size =
          match
            int_of_string_opt (String.sub prefix 1 (String.length prefix - 1))
          with
          | None -> raise Not_builtin
          | Some i -> i
        in
        let w_of_int : _ -> DataType.int_width = function
          | 1 -> W1
          | 2 -> W2
          | 4 -> W4
          | 8 -> W8
          | _ -> raise Not_builtin
        in
        match (start, size, endian) with
        | 'u', 1, None -> NumericType (Int_type (Int1Type {signed = false}))
        | 's', 1, None -> NumericType (Int_type (Int1Type {signed = true}))
        | 'u', i, _ ->
            NumericType
              (Int_type
                 (IntMultiType {signed = false; width = w_of_int i; endian}))
        | 's', i, _ ->
            NumericType
              (Int_type
                 (IntMultiType {signed = true; width = w_of_int i; endian}))
        | 'f', i, _ ->
            NumericType
              (Float_type (FloatMultiType {width = w_of_int i; endian}))
        | 'b', 1, None -> BooleanType (BitsType1 BigBitEndian)
        | 'b', i, _ ->
            NumericType
              (Int_type
                 (BitsType
                    {
                      width = i;
                      bit_endian =
                        (match endian with
                        | Some `BE | None -> BigBitEndian
                        | Some `LE -> LittleBitEndian);
                    }))
        | _, _, _ -> raise Not_builtin
      with Not_builtin ->
        if
          String.for_all
            (function
              | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false)
            raw_s
        then ComplexDataType (UserType raw_s)
        else Raw raw_s)
  | _ -> raise (Error "datatype not a scalar")

let seq x =
  let m = mapping x in
  let id = find_key m "id" |> scalar in
  let dataType =
    match find_key_opt m "type" with
    | None -> (
        match (find_key_opt m "size-eos", find_key_opt m "size") with
        | None, None -> raise (Error "not type, no size, no size-eos")
        | Some _, Some _ -> raise (Error "Cannot define both size and size-eos")
        | None, Some size ->
            DataType.BytesType
              (BytesLimitType
                 {
                   size = expression size;
                   terminator = None;
                   include_ = false;
                   padRight = None;
                   process = None;
                 })
        | Some b, None ->
            let b = bool b in
            if not b then raise (Error "size-eos: false") ;
            DataType.BytesType
              (BytesEosType
                 {
                   terminator = None;
                   include_ = false;
                   padRight = None;
                   process = None;
                 }))
    | Some x -> dataType x m
  in
  let cond = AttrSpec.ConditionalSpec.{ifExpr = None; repeat = NoRepeat} in
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
        | "expr" -> (
            match find_key_opt m "repeat-expr" with
            | Some e -> {cond with repeat = RepeatExpr (expression e)}
            | None -> raise (Error "repeat:expr no repeat-expr"))
        | "until" -> (
            match find_key_opt m "repeat-until" with
            | Some e -> {cond with repeat = RepeatUntil (expression e)}
            | None -> raise (Error "repeat:until no repeat-until"))
        | "eos" -> {cond with repeat = RepeatEos}
        | invalid ->
            raise
              (Error (Printf.sprintf "invalid value for repeat (%s)" invalid)))
  in
  let valid =
    match find_key_opt m "valid" with
    | None -> None
    | Some e -> (
        match e with
        | `O {m_members = [(`Scalar {value = "eq"; _}, e)]; _} ->
            Some (ValidationSpec.ValidationEq (expression e))
        | `Scalar _ as e -> Some (ValidationSpec.ValidationEq (expression e))
        | `O {m_members = [(`Scalar {value = "min"; _}, min)]; _} ->
            Some (ValidationSpec.ValidationMin (expression min))
        | `O {m_members = [(`Scalar {value = "max"; _}, max)]; _} ->
            Some (ValidationSpec.ValidationMax (expression max))
        | `O
            {
              m_members =
                [
                  (`Scalar {value = "min"; _}, min);
                  (`Scalar {value = "max"; _}, max);
                ];
              _;
            }
        | `O
            {
              m_members =
                [
                  (`Scalar {value = "max"; _}, max);
                  (`Scalar {value = "min"; _}, min);
                ];
              _;
            } ->
            Some
              (ValidationSpec.ValidationRange
                 {min = expression min; max = expression max})
        | `O {m_members = [(`Scalar {value = "expr"; _}, e)]; _} ->
            Some (ValidationSpec.ValidationExpr (expression e))
        | _ -> raise (Error "unknown valid layout"))
  in
  let size =
    match find_key_opt m "size" with
    | None -> None
    | Some s -> Some (expression s)
  in
  let enum =
    match find_key_opt m "enum" with None -> None | Some s -> Some (scalar s)
  in
  let doc = doc m in
  AttrSpec.{id; dataType; cond; valid; enum; size; doc}

let instanceSpec id v =
  let m = mapping v in
  let doc = doc m in
  let value = find_key m "value" in
  let ifExpr =
    match find_key_opt m "ifExpr" with
    | None -> None
    | Some e -> Some (expression e)
  in
  let descr =
    InstanceSpec.ValueInstanceSpec
      {value = expression value; id; ifExpr; dataTypeOpt = None}
  in
  InstanceSpec.{doc; descr}

let enumValueSpec yaml =
  match yaml with
  | `Scalar {value; _} -> EnumValueSpec.{name = value; doc = empty_doc}
  | `O m ->
      let id = find_key m "id" |> scalar in
      let doc = doc m in
      EnumValueSpec.{name = id; doc}
  | _ -> raise (Error "enumvaluespec")

let enumSpec yaml : EnumSpec.t =
  let m = mapping yaml in
  let map = keys m (fun k v -> (int_of_string k, enumValueSpec v)) in
  {map}

let endian x =
  match scalar x with "be" -> `BE | "le" -> `LE | _ -> raise (Error "endian")

let bitendian x : BitEndianness.t =
  match scalar x with
  | "be" -> BigBitEndian
  | "le" -> LittleBitEndian
  | _ -> raise (Error "bitendian")

let meta content =
  let m = mapping content in
  let id =
    match find_key_opt m "id" with None -> None | Some i -> Some (scalar i)
  in
  let endian =
    match find_key_opt m "endian" with
    | None -> None
    | Some x -> Some (endian x)
  in
  let bitEndian =
    match find_key_opt m "bit-endian" with
    | None -> None
    | Some x -> Some (bitendian x)
  in
  let forceDebug =
    match find_key_opt m "ks-debug" with None -> false | Some c -> bool c
  in
  let opaqueTypes =
    match find_key_opt m "ks-opaque-types" with
    | None -> None
    | Some c -> Some (bool c)
  in
  let zeroCopySubstream =
    match find_key_opt m "ks-zero-copy-substream" with
    | None -> None
    | Some c -> Some (bool c)
  in
  let imports =
    match find_key_opt m "imports" with
    | None -> []
    | Some l -> List.map scalar (sequence l)
  in
  MetaSpec.
    {
      id;
      endian;
      bitEndian;
      encoding = None;
      forceDebug;
      opaqueTypes;
      zeroCopySubstream;
      imports;
      isOpaque = false;
    }

let rec classSpec id yaml =
  let m = mapping yaml in
  let meta =
    match find_key_opt m "meta" with
    | Some content -> meta content
    | None ->
        MetaSpec.
          {
            isOpaque = false;
            id;
            endian = Some `BE;
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
        keys m (fun k v -> (k, classSpec None v))
  in
  let instances =
    match find_key_opt m "instances" with
    | None -> []
    | Some content ->
        let m = mapping content in
        keys m (fun k v -> (k, instanceSpec k v))
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
    | Some content -> sequence content |> List.map (fun x -> seq x)
  in
  let doc = doc m in

  ClassSpec.
    {
      meta;
      doc;
      types;
      toStringExpr = None;
      params = [];
      seq;
      instances;
      enums;
      fileName = None;
    }

let parse ?file s =
  let yaml =
    match Yaml.yaml_of_string s with
    | Ok x -> x
    | Error (`Msg msg) -> failwith msg
  in
  let id =
    match file with
    | None -> None
    | Some id -> Some (Filename.chop_suffix (Filename.basename id) ".ksy")
  in
  classSpec id yaml
