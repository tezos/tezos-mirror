(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Sexplib.Std

module Identifier = struct
  type t = string [@@deriving sexp]
end

module Ast = struct
  type boolop = Or | And [@@deriving sexp]

  type typeId = {absolute : bool; names : string list; isArray : bool}
  [@@deriving sexp]

  let empty_typeId = {absolute = false; names = []; isArray = false}

  let typeId_to_string {absolute; names; isArray} =
    let names = if absolute then "" :: names else names in
    let base = String.concat "::" names in
    if isArray then base ^ "[]" else base

  type operator =
    | Add
    | Sub
    | Mult
    | Div
    | Mod
    | LShift
    | RShift
    | BitOr
    | BitXor
    | BitAnd
  [@@deriving sexp]

  let operator_to_string = function
    | Add -> "+"
    | Sub -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Mod -> "%"
    | BitAnd -> "&"
    | BitOr -> "|"
    | BitXor -> "^"
    | LShift -> "<<"
    | RShift -> ">>"

  type unaryop = Invert | Not | Minus [@@deriving sexp]

  type cmpop = Eq | NotEq | Lt | LtE | Gt | GtE [@@deriving sexp]

  let cmpop_to_string = function
    | Lt -> "<"
    | LtE -> "<="
    | Gt -> ">"
    | GtE -> ">="
    | Eq -> "=="
    | NotEq -> "!="

  type t =
    | Raw of string
    | BoolOp of {op : boolop; values : t list}
    | BinOp of {left : t; op : operator; right : t}
    | UnaryOp of {op : unaryop; operand : t}
    | IfExp of {condition : t; ifTrue : t; ifFalse : t}
    | Compare of {left : t; ops : cmpop; right : t}
    | Call of {func : t; args : t list}
    | IntNum of int
    | FloatNum of float
    | Str of string
    | Bool of bool
    | EnumByLabel of {
        enumName : Identifier.t;
        label : Identifier.t;
        inType : typeId;
      }
    | EnumById of {enumName : Identifier.t; id : t; inType : typeId}
    | Attribute of {value : t; attr : Identifier.t}
    | CastToType of {value : t; typeName : typeId}
    | ByteSizeOfType of {typeName : typeId}
    | BitSizeOfType of {typeName : typeId}
    | Subscript of {value : t; idx : t}
    | Name of Identifier.t
    | List of t list
  [@@deriving sexp]

  type expr = t [@@deriving sexp]

  let string_of_unop = function Not -> "not" | Invert -> "~" | Minus -> "-"

  let rec to_string = function
    | IntNum n -> Int.to_string n
    | FloatNum f -> Float.to_string f
    | Name name -> name
    | UnaryOp {op; operand} -> string_of_unop op ^ " " ^ to_string operand
    | BinOp {left; op; right} ->
        Format.sprintf
          "(%s %s %s)"
          (to_string left)
          (operator_to_string op)
          (to_string right)
    | Compare {left; ops; right} ->
        Format.sprintf
          "(%s %s %s)"
          (to_string left)
          (cmpop_to_string ops)
          (to_string right)
    | Attribute {value; attr} -> Format.sprintf "(%s.%s)" (to_string value) attr
    | Subscript {value; idx} ->
        Format.sprintf "%s[%s]" (to_string value) (to_string idx)
    | CastToType {value; typeName} ->
        (* TODO: here and in other cases: https://gitlab.com/tezos/tezos/-/issues/6487 *)
        Format.sprintf "%s.as<%s>" (to_string value) (typeId_to_string typeName)
    | EnumByLabel {enumName; label; inType} -> (
        match typeId_to_string inType with
        | "" -> Printf.sprintf "%s::%s" enumName label
        | s -> Printf.sprintf "%s::%s::%s" s enumName label)
    | Raw s -> s
    | BoolOp {op = Or; values} ->
        String.concat " or " (List.map to_string values)
    | BoolOp {op = And; values} ->
        String.concat " and " (List.map to_string values)
    | IfExp {condition; ifTrue; ifFalse} ->
        Printf.sprintf
          "%s?%s:%s"
          (to_string condition)
          (to_string ifTrue)
          (to_string ifFalse)
    | Call {func; args} ->
        Printf.sprintf
          "%s(%s)"
          (to_string func)
          (String.concat ", " (List.map to_string args))
    | Str s -> s
    | Bool b -> Bool.to_string b
    | EnumById _ -> failwith "not implemented (EnumById)"
    | ByteSizeOfType {typeName} ->
        Printf.sprintf "sizeof<%s>" (typeId_to_string typeName)
    | BitSizeOfType {typeName} ->
        Printf.sprintf "bitsizeof<%s>" (typeId_to_string typeName)
    | List l ->
        Printf.sprintf "[%s]" (String.concat ", " (List.map to_string l))
end

type processExpr =
  | ProcessZlib
  | ProcessXor of {key : Ast.expr}
  | ProcessRotate of {left : int; key : Ast.expr}
  | ProcessCustom
[@@deriving sexp]

module BitEndianness = struct
  type t = LittleBitEndian | BigBitEndian [@@deriving sexp]

  let to_string = function LittleBitEndian -> "le" | BigBitEndian -> "be"
end

module Endianness = struct
  type fixed_endian = [`BE | `LE] [@@deriving sexp]

  type cases = (Ast.expr * fixed_endian) list [@@deriving sexp]

  type t = [fixed_endian | `Calc of Ast.expr * cases | `Inherited]
  [@@deriving sexp]

  let to_string = function
    | `BE -> "be"
    | `LE -> "le"
    | `Calc _ -> failwith "not supported (Calc)"
    | `Inherited -> failwith "not supported (Inherited)"
end

module DocSpec = struct
  type refspec = TextRef of string | UrlRef of {url : string; text : string}
  [@@deriving sexp]

  type t = {summary : string option; refs : refspec list} [@@deriving sexp]
end

module InstanceIdentifier = struct
  type t = string [@@deriving sexp]
end

module RepeatSpec = struct
  type t =
    | RepeatExpr of Ast.expr
    | RepeatUntil of Ast.expr
    | RepeatEos
    | NoRepeat
  [@@deriving sexp]
end

module ValidationSpec = struct
  type t =
    | ValidationEq of Ast.expr
    | ValidationMin of Ast.expr
    | ValidationMax of Ast.expr
    | ValidationRange of {min : Ast.expr; max : Ast.expr}
    | ValidationAnyOf of Ast.expr list
    | ValidationExpr of Ast.expr
  [@@deriving sexp]
end

module EnumValueSpec = struct
  type t = {name : string; doc : DocSpec.t} [@@deriving sexp]
end

module EnumSpec = struct
  type t = {map : (int * EnumValueSpec.t) list} [@@deriving sexp]
end

module MetaSpec = struct
  type t = {
    isOpaque : bool;
    id : string option;
    endian : Endianness.t option;
    bitEndian : BitEndianness.t option;
    mutable encoding : string option;
    forceDebug : bool;
    opaqueTypes : bool option;
    zeroCopySubstream : bool option;
    imports : string list;
  }
  [@@deriving sexp]
end

module DataType = struct
  type data_type =
    | NumericType of numeric_type
    | BooleanType of boolean_type
    | BytesType of bytes_type
    | StrType of str_type
    | ComplexDataType of complex_data_type
    | AnyType
    | Raw of string
  [@@deriving sexp]

  and int_width = W1 | W2 | W4 | W8 [@@deriving sexp]

  and numeric_type = Int_type of int_type | Float_type of float_type
  [@@deriving sexp]

  and int_type =
    | CalcIntType
    | Int1Type of {signed : bool}
    | IntMultiType of {
        signed : bool;
        width : int_width;
        endian : Endianness.fixed_endian option;
      }
    | BitsType of {width : int; bit_endian : BitEndianness.t}
  [@@deriving sexp]

  and float_type =
    | CalcFloatType
    | FloatMultiType of {
        width : int_width;
        endian : Endianness.fixed_endian option;
      }
  [@@deriving sexp]

  and boolean_type = BitsType1 of BitEndianness.t | CalcBooleanType
  [@@deriving sexp]

  and bytes_type =
    | CalcBytesType
    | BytesEosType of {
        terminator : int option;
        include_ : bool;
        padRight : int option;
        mutable process : processExpr option;
      }
    | BytesLimitType of {
        size : Ast.expr;
        terminator : int option;
        include_ : bool;
        padRight : int option;
        mutable process : processExpr option;
      }
    | BytesTerminatedType of {
        terminator : int;
        include_ : bool;
        consume : bool;
        eosError : bool;
        mutable process : processExpr option;
      }
  [@@deriving sexp]

  and str_type =
    | CalcStrType
    | StrFromBytesType of {bytes : bytes_type; encoding : string}
  [@@deriving sexp]

  and array_type = ArrayTypeInStream | CalcArrayType [@@deriving sexp]

  and complex_data_type =
    | StructType
    | UserType of string
    | ArrayType of array_type
  [@@deriving sexp]

  and switch_type = {
    on : Ast.expr;
    cases : (Ast.expr * data_type) list;
    isOwning : bool;
    mutable isOwningInExpr : bool;
  }
  [@@deriving sexp]

  type t = data_type [@@deriving sexp]

  let width_to_int = function W1 -> 1 | W2 -> 2 | W4 -> 4 | W8 -> 8

  let to_string = function
    | Raw s -> s
    | NumericType (Int_type int_type) -> (
        match int_type with
        | Int1Type {signed} -> if signed then "s1" else "u1"
        | IntMultiType {signed; width; endian} ->
            Printf.sprintf
              "%s%d%s"
              (if signed then "s" else "u")
              (width_to_int width)
              (endian
              |> Option.map Endianness.to_string
              |> Option.value ~default:"")
        | BitsType {width; bit_endian} ->
            Printf.sprintf "b%d%s" width (BitEndianness.to_string bit_endian)
        | CalcIntType -> failwith "not supported (CalcIntType)")
    | NumericType (Float_type (FloatMultiType {width = _; endian = _})) -> "f8"
    | NumericType (Float_type CalcFloatType) ->
        failwith "not supported (CalcFloatType)"
    | ComplexDataType (UserType id) -> id
    | ComplexDataType StructType -> failwith "not supported (StructType)"
    | ComplexDataType (ArrayType _) -> failwith "not supported (ArrayType)"
    | BooleanType (BitsType1 _) -> "b1"
    | BooleanType CalcBooleanType -> failwith "not supported (CalcBooleanType)"
    | BytesType _ ->
        failwith "Bytes types are ommitted in kaitai struct representation"
    | AnyType -> failwith "not supported (AnyType)"
    | StrType _ -> failwith "not supported (StrType)"
end

module AttrSpec = struct
  module ConditionalSpec = struct
    type t = {ifExpr : Ast.expr option; repeat : RepeatSpec.t} [@@deriving sexp]
  end

  type t = {
    id : Identifier.t;
    dataType : DataType.t;
    cond : ConditionalSpec.t;
    valid : ValidationSpec.t option;
    enum : string option;
    doc : DocSpec.t;
    size : Ast.expr option;
  }
  [@@deriving sexp]
end

module InstanceSpec = struct
  type t = {doc : DocSpec.t; descr : descr} [@@deriving sexp]

  and descr =
    | ValueInstanceSpec of {
        id : InstanceIdentifier.t;
        value : Ast.expr;
        ifExpr : Ast.expr option;
      }
    | ParseInstanceSpec (* TODO *)
  [@@deriving sexp]
end

module ParamDefSpec = struct
  type t = {id : Identifier.t; dataType : DataType.t; doc : DocSpec.t}
  [@@deriving sexp]
end

module ClassSpec = struct
  type t = {
    fileName : string option;
    meta : MetaSpec.t;
    doc : DocSpec.t;
    toStringExpr : Ast.expr option;
    params : ParamDefSpec.t list;
    seq : AttrSpec.t list;
    types : (string * t) list;
    instances : (InstanceIdentifier.t * InstanceSpec.t) list;
    enums : (string * EnumSpec.t) list;
  }
  [@@deriving sexp]
end
