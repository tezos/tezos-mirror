(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(*                                                                           *)
(*****************************************************************************)

module Identifier : sig
  type t = string
end

(** https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/exprlang/Ast.scala *)
module Ast : sig
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

  type unaryop =
    (* Bitwise negation operator. Applicable only to `IntNum`s *)
    | Invert
    (* Boolean negation operator. Applicable only to `Boolean`s *)
    | Not
    (* Arithmetic negation operator. Applicable only to `IntNum`s / `FloatNum`s *)
    | Minus

  type cmpop = Eq | NotEq | Lt | LtE | Gt | GtE

  type t =
    | Raw of string
      (* Temporary: [Raw] So that we don't need to deal with parsing/printing of Ast.expr *)
    | BoolOp of {op : boolop; values : t list}
    | BinOp of {left : t; op : operator; right : t}
    | UnaryOp of {op : unaryop; operand : t}
    | IfExp of {condition : t; ifTrue : t; ifFalse : t}
    (* | Dict of {keys : t list; values : t list} *)
    (* Represents `X < Y`, `X > Y` and so on. *)
    | Compare of {left : t; ops : cmpop; right : t}
    | Call of {func : t; args : t list}
    | IntNum of int (* BigInt *)
    | FloatNum of float (* BigDecimal *)
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
    (* Represents `X[Y]`. *)
    | Subscript of {value : t; idx : t}
    | Name of Identifier.t
    | List of t list

  type expr = t

  val to_string : t -> string
end

type processExpr =
  | ProcessZlib
  | ProcessXor of {key : Ast.expr}
  | ProcessRotate of {left : int; key : Ast.expr}
  | ProcessCustom

module BitEndianness : sig
  type t = LittleBitEndian | BigBitEndidan
end

module Endianness : sig
  type fixed_endian = [`BE | `LE]

  type cases = (Ast.expr * fixed_endian) list

  type t = [fixed_endian | `Calc of Ast.expr * cases | `Inherited]

  val to_string : t -> string
end

module DocSpec : sig
  (* https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/format/DocSpec.scala *)
  type refspec = TextRef of string | UrlRef of {url : string; text : string}

  type t = {summary : string option; refs : refspec list}
end

module InstanceIdentifier : sig
  type t = string
end

module RepeatSpec : sig
  (* https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/format/RepeatSpec.scala *)
  type t =
    | RepeatExpr of Ast.expr
    | RepeatUntil of Ast.expr
    | RepeatEos
    | NoRepeat
end

module ValidationSpec : sig
  (* https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/format/ValidationSpec.scala *)
  type t =
    | ValidationEq of Ast.expr
    | ValidationMin of Ast.expr
    | ValidationMax of Ast.expr
    | ValidationRange of {min : Ast.expr; max : Ast.expr}
    | ValidationAnyOf of Ast.expr list
    | ValidationExpr of Ast.expr
end

module EnumValueSpec : sig
  (* https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/format/EnumValueSpec.scala *)
  type t = {name : string; doc : DocSpec.t}
end

module EnumSpec : sig
  (* https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/format/EnumSpec.scala *)
  type t = {path : string list; map : (int * EnumValueSpec.t) list}
end

module MetaSpec : sig
  (* https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/format/MetaSpec.scala *)
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
  (* https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/datatype/DataType.scala *)
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
    | Array_Type of array_type

  and switch_type = {
    on : Ast.expr;
    cases : (Ast.expr * data_type) list;
    isOwning : bool;
    mutable isOwningInExpr : bool;
  }

  type t = data_type

  val to_string : t -> string
end

and AttrSpec : sig
  (* https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/format/AttrSpec.scala *)
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
  }
end

and InstanceSpec : sig
  (* https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/format/InstanceSpec.scala *)
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
  (* https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/format/ParamDefSpec.scala *)
  type t = {
    path : string list;
    id : Identifier.t;
    dataType : DataType.t;
    doc : DocSpec.t;
  }
end

and ClassSpec : sig
  (* https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/format/ClassSpec.scala *)
  type t = {
    fileName : string option;
    path : string list;
    (* isTopLevel : bool; *)
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
