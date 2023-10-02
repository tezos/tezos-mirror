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

module Identifier = struct
  type t = string
end

module Ast = struct
  type boolop = Or | And

  type typeId = {absolute : bool; names : string list; isArray : bool}

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

  let operator_to_string = function
    | BitAnd -> "&"
    | RShift -> ">>"
    | _ -> failwith "not implemented"

  type unaryop = Invert | Not | Minus

  type cmpop = Eq | NotEq | Lt | LtE | Gt | GtE

  let cmpop_to_string = function
    | NotEq -> "!="
    | Eq -> "=="
    | _ -> failwith "not implemented"

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

  type expr = t

  let rec to_string = function
    | IntNum n -> Int.to_string n
    | Name name -> name
    | UnaryOp {op; operand} -> (
        match op with
        | Not -> "not " ^ to_string operand
        | _ -> failwith "unary operator not supported")
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
    | _ -> failwith "not implemented"
end

type processExpr =
  | ProcessZlib
  | ProcessXor of {key : Ast.expr}
  | ProcessRotate of {left : int; key : Ast.expr}
  | ProcessCustom

module BitEndianness = struct
  type t = LittleBitEndian | BigBitEndidan
end

module Endianness = struct
  type fixed_endian = [`BE | `LE]

  type cases = (Ast.expr * fixed_endian) list

  type t = [fixed_endian | `Calc of Ast.expr * cases | `Inherited]

  let to_string = function
    | `BE -> "be"
    | `LE -> "le"
    | `Calc _ | `Inherited -> failwith "not supported"
end

module DocSpec = struct
  type refspec = TextRef of string | UrlRef of {url : string; text : string}

  type t = {summary : string option; refs : refspec list}
end

module InstanceIdentifier = struct
  type t = string
end

module RepeatSpec = struct
  type t =
    | RepeatExpr of Ast.expr
    | RepeatUntil of Ast.expr
    | RepeatEos
    | NoRepeat
end

module ValidationSpec = struct
  type t =
    | ValidationEq of Ast.expr
    | ValidationMin of Ast.expr
    | ValidationMax of Ast.expr
    | ValidationRange of {min : Ast.expr; max : Ast.expr}
    | ValidationAnyOf of Ast.expr list
    | ValidationExpr of Ast.expr
end

module EnumValueSpec = struct
  type t = {name : string; doc : DocSpec.t}
end

module EnumSpec = struct
  type t = {path : string list; map : (int * EnumValueSpec.t) list}
end

module MetaSpec = struct
  type t = {
    path : string list;
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
end

module rec DataType : sig
  type data_type =
    | NumericType of numeric_type
    | BooleanType
    | BytesType of bytes_type
    | StrType of str_type
    | ComplexDataType of complex_data_type
    | AnyType

  and int_width = W1 | W2 | W4 | W8

  and numeric_type = Int_type of int_type | Float_type of float_type

  and int_type =
    | CalcIntType
    | Int1Type of {signed : bool}
    | IntMultiType of {
        signed : bool;
        width : int_width;
        endian : Endianness.fixed_endian option;
      }
    | BitsType of {width : int; bit_endian : BitEndianness.t}

  and float_type =
    | CalcFloatType
    | FloatMultiType of {
        width : int_width;
        endian : Endianness.fixed_endian option;
      }

  and boolean_type = BitsType1 of BitEndianness.t | CalcBooleanType

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

  and str_type =
    | CalcStrType
    | StrFromBytesType of {bytes : bytes_type; encoding : string}

  and array_type = ArrayTypeInStream | CalcArrayType

  and complex_data_type =
    | StructType
    | UserType of ClassSpec.t
    | ArrayType of array_type

  and switch_type = {
    on : Ast.expr;
    cases : (Ast.expr * data_type) list;
    isOwning : bool;
    mutable isOwningInExpr : bool;
  }

  type t = data_type

  val to_string : t -> string
end = struct
  type data_type =
    | NumericType of numeric_type
    | BooleanType
    | BytesType of bytes_type
    | StrType of str_type
    | ComplexDataType of complex_data_type
    | AnyType

  and int_width = W1 | W2 | W4 | W8

  and numeric_type = Int_type of int_type | Float_type of float_type

  and int_type =
    | CalcIntType
    | Int1Type of {signed : bool}
    | IntMultiType of {
        signed : bool;
        width : int_width;
        endian : Endianness.fixed_endian option;
      }
    | BitsType of {width : int; bit_endian : BitEndianness.t}

  and float_type =
    | CalcFloatType
    | FloatMultiType of {
        width : int_width;
        endian : Endianness.fixed_endian option;
      }

  and boolean_type = BitsType1 of BitEndianness.t | CalcBooleanType

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

  and str_type =
    | CalcStrType
    | StrFromBytesType of {bytes : bytes_type; encoding : string}

  and array_type = ArrayTypeInStream | CalcArrayType

  and complex_data_type =
    | StructType
    | UserType of ClassSpec.t
    | ArrayType of array_type

  and switch_type = {
    on : Ast.expr;
    cases : (Ast.expr * data_type) list;
    isOwning : bool;
    mutable isOwningInExpr : bool;
  }

  type t = data_type

  let width_to_int = function W1 -> 1 | W2 -> 2 | W4 -> 4 | W8 -> 8

  let to_string = function
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
        | _ -> failwith "not supported")
    | NumericType (Float_type (FloatMultiType {width = _; endian = _})) -> "f8"
    | BytesType (BytesLimitType _) -> "fixed size bytes"
    | BytesType (BytesEosType _) -> "variable size bytes"
    | ComplexDataType (UserType {meta = {id = Some id; _}; _}) -> id
    | _ -> failwith "not supported"
end

and AttrSpec : sig
  module ConditionalSpec : sig
    type t = {ifExpr : Ast.expr option; repeat : RepeatSpec.t}
  end

  type t = {
    path : string list;
    id : Identifier.t;
    dataType : DataType.t;
    cond : ConditionalSpec.t;
    valid : ValidationSpec.t option;
    enum : string option;
    doc : DocSpec.t;
    size : Ast.expr option;
  }
end = struct
  module ConditionalSpec = struct
    type t = {ifExpr : Ast.expr option; repeat : RepeatSpec.t}
  end

  type t = {
    path : string list;
    id : Identifier.t;
    dataType : DataType.t;
    cond : ConditionalSpec.t;
    valid : ValidationSpec.t option;
    enum : string option;
    doc : DocSpec.t;
    size : Ast.expr option;
  }
end

and InstanceSpec : sig
  type t = {doc : DocSpec.t; descr : descr}

  and descr =
    | ValueInstanceSpec of {
        id : InstanceIdentifier.t;
        path : string list;
        value : Ast.expr;
        ifExpr : Ast.expr option;
        dataTypeOpt : DataType.t option;
      }
    | ParseInstanceSpec
end = struct
  type t = {doc : DocSpec.t; descr : descr}

  and descr =
    | ValueInstanceSpec of {
        id : InstanceIdentifier.t;
        path : string list;
        value : Ast.expr;
        ifExpr : Ast.expr option;
        dataTypeOpt : DataType.t option;
      }
    | ParseInstanceSpec (* TODO *)
end

and ParamDefSpec : sig
  type t = {
    path : string list;
    id : Identifier.t;
    dataType : DataType.t;
    doc : DocSpec.t;
  }
end = struct
  type t = {
    path : string list;
    id : Identifier.t;
    dataType : DataType.t;
    doc : DocSpec.t;
  }
end

and ClassSpec : sig
  type t = {
    fileName : string option;
    path : string list;
    isTopLevel : bool;
    meta : MetaSpec.t;
    doc : DocSpec.t;
    toStringExpr : Ast.expr option;
    params : ParamDefSpec.t list;
    seq : AttrSpec.t list;
    types : (string * t) list;
    instances : (InstanceIdentifier.t * InstanceSpec.t) list;
    enums : (string * EnumSpec.t) list;
  }
end = struct
  type t = {
    fileName : string option;
    path : string list;
    isTopLevel : bool;
    meta : MetaSpec.t;
    doc : DocSpec.t;
    toStringExpr : Ast.expr option;
    params : ParamDefSpec.t list;
    seq : AttrSpec.t list;
    types : (string * t) list;
    instances : (InstanceIdentifier.t * InstanceSpec.t) list;
    enums : (string * EnumSpec.t) list;
  }
end
