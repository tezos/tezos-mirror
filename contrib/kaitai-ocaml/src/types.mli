(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* TODO: https://gitlab.com/tezos/tezos/-/issues/6251
         Add/improve existing docstring. *)

module Identifier : sig
  (** [t] is a string identifier. *)
  type t = string
end

(** [Ast] module defines Kaitai Struct expression language.

    - For more about it see {{:https://doc.kaitai.io/user_guide.html#_expression_language} the Kaitai Struct User Guide}.
    - For a reference implementation {{:https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/exprlang/Ast.scala} see}.*)
module Ast : sig
  type boolop = Or | And

  type typeId = {absolute : bool; names : string list; isArray : bool}

  val empty_typeId : typeId

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
    | Invert  (** Bitwise negation operator. Applicable only to [IntNum]s. *)
    | Not  (** Boolean negation operator. Applicable only to [Boolean]s. *)
    | Minus
        (** Arithmetic negation operator. Applicable only to [IntNum]s or [FloatNum]s. *)

  type cmpop = Eq | NotEq | Lt | LtE | Gt | GtE

  type t =
    | Raw of string
        (** Temporary: [Raw] So that we don't need to deal with parsing/printing of [Ast.expr]. *)
    | BoolOp of {op : boolop; values : t list}
        (** [BoolOp] represent a boolean operation. *)
    | BinOp of {left : t; op : operator; right : t}
        (** [BinOp] represents a binary operation. *)
    | UnaryOp of {op : unaryop; operand : t}
        (** [UnaryOp] represents an unary operation. *)
    | IfExp of {condition : t; ifTrue : t; ifFalse : t}
        (** | Dict of {keys : t list; values : t list} *)
    | Compare of {left : t; ops : cmpop; right : t}
        (** Represents [X < Y], [X > Y] and so on. *)
    | Call of {func : t; args : t list}
        (** [Call] represents a function call. *)
    | IntNum of int
        (** [IntNum] is originally represented by {@scala[scala.math.BigInt]}. *)
    | FloatNum of float
        (** [FloatNum] is originally represented by {@scala[scala.math.BigDecimal]}. *)
    | Str of string  (** [Str] is a string expression. *)
    | Bool of bool  (** [Bool] is a bool expression. *)
    | EnumByLabel of {
        enumName : Identifier.t;
        label : Identifier.t;
        inType : typeId;
      }  (** [EnumByLabel] represents an enum istance. *)
    | EnumById of {enumName : Identifier.t; id : t; inType : typeId}
        (** [EnumById] represents an enum istance. *)
    | Attribute of {value : t; attr : Identifier.t}
        (** [Attribute] represents a [x.y] where [y] is [attr]. *)
    | CastToType of {value : t; typeName : typeId}
        (** [CastToType] is used for casting types. *)
    | ByteSizeOfType of {typeName : typeId}
        (** [BytesSizeOfType] expression for specifying type size in bytes. *)
    | BitSizeOfType of {typeName : typeId}
        (** [BitSizeOfType] expression for specifying type size in bits. *)
    | Subscript of {value : t; idx : t}  (** Represents [[X[Y]]]. *)
    | Name of Identifier.t
        (** [Name] is used for defining variable, e.g. [x]. *)
    | List of t list  (** [List] represents a list. *)

  type expr = t

  val to_string : t -> string
end

type processExpr =
  | ProcessZlib
  | ProcessXor of {key : Ast.expr}
  | ProcessRotate of {left : int; key : Ast.expr}
  | ProcessCustom

module BitEndianness : sig
  type t = LittleBitEndian | BigBitEndian

  val to_string : t -> string
end

module Endianness : sig
  type fixed_endian = [`BE | `LE]

  type cases = (Ast.expr * fixed_endian) list

  type t = [fixed_endian | `Calc of Ast.expr * cases | `Inherited]

  val to_string : t -> string
end

(** [DocSpec] module defines primitives for attaching documentation to kaitai
    specification files. 
   
    See the {{:https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/format/DocSpec.scala}
    reference implementation}.*)
module DocSpec : sig
  type refspec = TextRef of string | UrlRef of {url : string; text : string}

  type t = {summary : string option; refs : refspec list}
end

module InstanceIdentifier : sig
  type t = string
end

(** [RepeatSpec] defines possible repetitions of Kaitai primitives. 

    See the {{:https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/format/RepeatSpec.scala}
    reference implementation}.*)
module RepeatSpec : sig
  type t =
    | RepeatExpr of Ast.expr
    | RepeatUntil of Ast.expr
    | RepeatEos
    | NoRepeat
end

(** [ValidationSpec] defines validation logic for Kaitai primitives.

    See the {{:https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/format/ValidationSpec.scala}
    reference implementation}.*)
module ValidationSpec : sig
  type t =
    | ValidationEq of Ast.expr
    | ValidationMin of Ast.expr
    | ValidationMax of Ast.expr
    | ValidationRange of {min : Ast.expr; max : Ast.expr}
    | ValidationAnyOf of Ast.expr list
    | ValidationExpr of Ast.expr
end

(** For a reference implementation {{:https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/format/EnumValueSpec.scala} see}. *)
module EnumValueSpec : sig
  type t = {name : string; doc : DocSpec.t}
end

(** For a reference implementation {{:https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/format/EnumSpec.scala} see}.*)
module EnumSpec : sig
  type t = {map : (int * EnumValueSpec.t) list}
end

module MetaSpec : sig
  (** For a reference implementation {{:https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/format/MetaSpec.scala} see}.*)

  (** [t] defines the meta section of Kaitai specification file. *)
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
end

(** [DataType] module defines AST for describing underlying data types.

    For a reference implementation {{:https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/datatype/DataType.scala} see. *)
module DataType : sig
  type data_type =
    | NumericType of numeric_type
    | BooleanType of boolean_type
    | BytesType of bytes_type
    | StrType of str_type
    | ComplexDataType of complex_data_type
    | AnyType
    | Raw of string

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
    | UserType of string
      (* Note that in Kaitai's reference implementation, the [UserType]
         AST node carries a [ClassSpec]. However, for our use case, we
         only need the [UserType]'s [id]. So we use the simpler [string]
         payload which avoids the mutual recursion of [DataType] and
         [ClassSpec]. *)
    | ArrayType of array_type

  and switch_type = {
    on : Ast.expr;
    cases : (Ast.expr * data_type) list;
    isOwning : bool;
    mutable isOwningInExpr : bool;
  }

  type t = data_type

  val to_string : t -> string
end

(** For a reference implementation {{:https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/format/AttrSpec.scala} see}.*)
module AttrSpec : sig
  module ConditionalSpec : sig
    type t = {ifExpr : Ast.expr option; repeat : RepeatSpec.t}
  end

  (** [t] is a single element inside [ClassSpec.t.seq]. *)
  type t = {
    id : Identifier.t;
    dataType : DataType.t;
    cond : ConditionalSpec.t;
    valid : ValidationSpec.t option;
    enum : string option;
    doc : DocSpec.t;
    size : Ast.expr option;
  }
end

module InstanceSpec : sig
  (** For a reference implementation {{:https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/format/InstanceSpec.scala} see}. *)

  (** [t] defines a Kaitai instance.
  
      For more about it {{:https://doc.kaitai.io/user_guide.html#_instances_data_beyond_the_sequence} see}. *)
  type t = {doc : DocSpec.t; descr : descr}

  and descr =
    | ValueInstanceSpec of {
        id : InstanceIdentifier.t;
        value : Ast.expr;
        ifExpr : Ast.expr option;
      }
    | ParseInstanceSpec (* TODO *)
end

(** For a reference implementation {{:https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/format/ParamDefSpec.scala} see}. *)
module ParamDefSpec : sig
  type t = {id : Identifier.t; dataType : DataType.t; doc : DocSpec.t}
end

module ClassSpec : sig
  (** For a reference implementation {{:https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/format/ClassSpec.scala} see}. *)

  (** [t] is an outermost type that describes Kaitai Struct specification files. *)
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
