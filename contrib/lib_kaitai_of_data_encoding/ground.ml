(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Kaitai.Types
open Helpers

let default_doc_spec = DocSpec.{summary = None; refs = []}

let cond_no_cond =
  AttrSpec.ConditionalSpec.{ifExpr = None; repeat = RepeatSpec.NoRepeat}

let default_attr_spec =
  AttrSpec.
    {
      path = [];
      id = "";
      dataType = DataType.AnyType;
      cond = cond_no_cond;
      valid = None;
      doc = default_doc_spec;
      enum = None;
    }

module Enum = struct
  type map = (string * Kaitai.Types.EnumSpec.t) list

  let bool =
    ( "bool",
      EnumSpec.
        {
          path = [];
          map =
            [
              (0, EnumValueSpec.{name = "false"; doc = default_doc_spec});
              (255, EnumValueSpec.{name = "true"; doc = default_doc_spec});
            ];
        } )
end

module Attr = struct
  let bool =
    {
      default_attr_spec with
      id = "bool";
      dataType = DataType.(NumericType (Int_type (Int1Type {signed = false})));
      valid = Some (ValidationAnyOf [IntNum 0; IntNum 255]);
      enum = Some (fst Enum.bool);
    }

  let int1_type_attr_spec ~signed =
    {
      default_attr_spec with
      id = (if signed then "int8" else "uint8");
      dataType = DataType.(NumericType (Int_type (Int1Type {signed})));
    }

  let int_multi_type_atrr_spec ~id ~signed width =
    {
      default_attr_spec with
      id;
      dataType =
        DataType.(
          NumericType (Int_type (IntMultiType {signed; width; endian = None})));
    }

  let float_multi_type_attr_spec ~id =
    {
      default_attr_spec with
      id;
      dataType =
        DataType.(
          NumericType
            (Float_type
               (FloatMultiType
                  {
                    (* Data-encoding supports only 64-bit floats. *)
                    width = DataType.W8;
                    endian = None;
                  })));
    }

  let bytes_limit_type_attr_spec ~id =
    {
      default_attr_spec with
      id;
      dataType =
        DataType.(
          BytesType
            (BytesLimitType
               {
                 size = Name "size";
                 terminator = None;
                 include_ = false;
                 padRight = None;
                 process = None;
               }));
    }

  let u1 = int1_type_attr_spec ~signed:false

  let s1 = int1_type_attr_spec ~signed:true

  let u2 = int_multi_type_atrr_spec ~id:"uint16" ~signed:false DataType.W2

  let s2 = int_multi_type_atrr_spec ~id:"int16" ~signed:true DataType.W2

  let u4 ?(id = "uint32") () =
    int_multi_type_atrr_spec ~id ~signed:false DataType.W4

  let s4 = int_multi_type_atrr_spec ~id:"int32" ~signed:true DataType.W4

  let s8 = int_multi_type_atrr_spec ~id:"int64" ~signed:true DataType.W8

  let int31 =
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/6261
             There should be a validation that [Int31] is in the appropriate
             range. *)
    int_multi_type_atrr_spec ~id:"int31" ~signed:true DataType.W4

  let f8 = float_multi_type_attr_spec ~id:"float"

  let fixed_size_header_class_spec =
    {
      (default_class_spec ~encoding_name:"fixed_bytes") with
      seq = u4 ~id:"size" () :: [bytes_limit_type_attr_spec ~id:"value"];
      isTopLevel = false;
    }

  let bytes =
    (* TODO:  https://gitlab.com/tezos/tezos/-/issues/6260
              We fix size header to [`Uint30] for now. This corresponds to
              size header of ground bytes encoding. Later on we want to add
              support for [`Uint16], [`Uint8] and [`N]. *)
    {
      default_attr_spec with
      id = "fixed size (uint30) bytes";
      dataType =
        DataType.(ComplexDataType (UserType fixed_size_header_class_spec));
    }

  let string = bytes
end

module Class = struct
  let bool ~encoding_name =
    class_spec_of_attr ~encoding_name ~enums:[Enum.bool] Attr.bool

  let uint8 ~encoding_name = class_spec_of_attr ~encoding_name Attr.u1

  let int8 ~encoding_name = class_spec_of_attr ~encoding_name Attr.s1

  let uint16 ~encoding_name = class_spec_of_attr ~encoding_name Attr.u2

  let int16 ~encoding_name = class_spec_of_attr ~encoding_name Attr.s2

  let int32 ~encoding_name = class_spec_of_attr ~encoding_name Attr.s4

  let int64 ~encoding_name = class_spec_of_attr ~encoding_name Attr.s8

  let int31 ~encoding_name = class_spec_of_attr ~encoding_name Attr.int31

  let float ~encoding_name = class_spec_of_attr ~encoding_name Attr.f8

  let bytes ~encoding_name = class_spec_of_attr ~encoding_name Attr.bytes

  let string ~encoding_name = class_spec_of_attr ~encoding_name Attr.string

  let byte_group =
    {
      (default_class_spec ~encoding_name:"group") with
      seq = [{Attr.u1 with id = "b"}];
      instances =
        [
          ( "has_next",
            default_instance_spec
              ~id:"has_next"
              Ast.(
                Compare
                  {
                    left =
                      BinOp {left = Name "b"; op = BitAnd; right = IntNum 128};
                    ops = NotEq;
                    right = IntNum 0;
                  }) );
          ( "value",
            default_instance_spec
              ~id:"value"
              (BinOp {left = Name "b"; op = BitAnd; right = IntNum 127}) );
        ];
      isTopLevel = false;
    }

  let repeat_until_end_bytes_group_attr =
    {
      default_attr_spec with
      id = "groups";
      dataType = DataType.(ComplexDataType (UserType byte_group));
      cond =
        AttrSpec.ConditionalSpec.
          {
            ifExpr = None;
            repeat =
              RepeatSpec.RepeatUntil
                Ast.(
                  UnaryOp
                    {
                      op = Not;
                      operand = Attribute {value = Name "_"; attr = "has_next"};
                    });
          };
    }

  let n ~encoding_name =
    class_spec_of_attr ~encoding_name repeat_until_end_bytes_group_attr
end
