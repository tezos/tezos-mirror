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

module Enum = struct
  type assoc = (string * Kaitai.Types.EnumSpec.t) list

  let bool =
    ( "bool",
      EnumSpec.
        {
          path = [];
          map =
            [
              (0, EnumValueSpec.{name = "false"; doc = Helpers.default_doc_spec});
              ( 255,
                EnumValueSpec.{name = "true"; doc = Helpers.default_doc_spec} );
            ];
        } )
end

let n_group =
  {
    (* TODO/nice to have: Add a docstring, i.e. [?description]
                          to custom defined class spec. *)
    (Helpers.default_class_spec ~encoding_name:"n_group" ())
    with
    seq =
      [
        {
          Helpers.default_attr_spec with
          id = "b";
          dataType =
            DataType.(NumericType (Int_type (Int1Type {signed = false})));
        };
      ];
    instances =
      [
        ( "has_next",
          Helpers.default_instance_spec
            ~id:"has_next"
            Ast.(
              Compare
                {
                  left = BinOp {left = Name "b"; op = BitAnd; right = IntNum 128};
                  ops = NotEq;
                  right = IntNum 0;
                }) );
        ( "value",
          Helpers.default_instance_spec
            ~id:"value"
            (BinOp {left = Name "b"; op = BitAnd; right = IntNum 127}) );
      ];
    isTopLevel = false;
  }

let n_attr =
  (* defining this here to break circular dependencies *)
  {
    Helpers.default_attr_spec with
    id = "n";
    dataType = DataType.(ComplexDataType (UserType n_group));
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

module Type = struct
  type assoc = (string * Kaitai.Types.ClassSpec.t) list

  let n =
    let class_spec =
      Helpers.class_spec_of_attrs
        ~encoding_name:"n"
        ~enums:[]
        ~types:[("n_group", n_group)]
        ~instances:[]
        [n_attr]
    in
    ("n", class_spec)

  let z =
    let class_spec =
      let instances =
        [
          ( "is_negative",
            Helpers.default_instance_spec
              ~id:"is_negative"
              Ast.(
                Compare
                  {
                    left =
                      BinOp
                        {
                          left =
                            Attribute
                              {
                                value =
                                  Subscript
                                    {value = Name "groups"; idx = IntNum 0};
                                attr = "value";
                              };
                          op = RShift;
                          right = IntNum 6;
                        };
                    ops = Eq;
                    right = IntNum 1;
                  }) );
        ]
      in
      Helpers.class_spec_of_attrs
        ~encoding_name:"z"
        ~enums:[]
        ~types:[("n_group", n_group)]
        ~instances
        [n_attr]
    in
    ("z", class_spec)
end

module Attr = struct
  let bool ~id =
    {
      Helpers.default_attr_spec with
      id;
      dataType = DataType.(NumericType (Int_type (Int1Type {signed = false})));
      valid = Some (ValidationAnyOf [IntNum 0; IntNum 255]);
      enum = Some (fst Enum.bool);
    }

  let int1_type_attr_spec ~id ~signed =
    {
      Helpers.default_attr_spec with
      id;
      dataType = DataType.(NumericType (Int_type (Int1Type {signed})));
    }

  let int_multi_type_atrr_spec ~id ~signed width =
    {
      Helpers.default_attr_spec with
      id;
      dataType =
        DataType.(
          NumericType (Int_type (IntMultiType {signed; width; endian = None})));
    }

  let float_multi_type_attr_spec ~id =
    {
      Helpers.default_attr_spec with
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

  let uint8 ~id = int1_type_attr_spec ~id ~signed:false

  let int8 ~id = int1_type_attr_spec ~id ~signed:true

  let uint16 ~id = int_multi_type_atrr_spec ~id ~signed:false DataType.W2

  let int16 ~id = int_multi_type_atrr_spec ~id ~signed:true DataType.W2

  let uint32 ~id = int_multi_type_atrr_spec ~id ~signed:false DataType.W4

  let int32 ~id = int_multi_type_atrr_spec ~id ~signed:true DataType.W4

  let int64 ~id = int_multi_type_atrr_spec ~id ~signed:true DataType.W8

  let int31 ~id =
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/6261
             There should be a validation that [Int31] is in the appropriate
             range. *)
    int_multi_type_atrr_spec ~id ~signed:true DataType.W4

  let float ~id = float_multi_type_attr_spec ~id

  let bytes_limit_type_attr_spec ~id =
    {
      Helpers.default_attr_spec with
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

  let fixed_size_header_class_spec =
    {
      (* TODO / nice to have: Add a docstring, i.e. [?description]
                              to custom defined class spec. *)
      (Helpers.default_class_spec ~encoding_name:"fixed_bytes" ())
      with
      seq = [uint32 ~id:"size"; bytes_limit_type_attr_spec ~id:"value"];
      isTopLevel = false;
    }

  let bytes ~id =
    (* TODO:  https://gitlab.com/tezos/tezos/-/issues/6260
              We fix size header to [`Uint30] for now. This corresponds to
              size header of ground bytes encoding. Later on we want to add
              support for [`Uint16], [`Uint8] and [`N]. *)
    {
      Helpers.default_attr_spec with
      id;
      dataType =
        DataType.(ComplexDataType (UserType fixed_size_header_class_spec));
    }

  let bytes_eos ~id =
    {
      Helpers.default_attr_spec with
      id;
      dataType =
        DataType.(
          BytesType
            (BytesEosType
               {
                 terminator = None;
                 include_ = false;
                 padRight = None;
                 process = None;
               }));
    }

  let bytes_fixed ~id n =
    {Helpers.default_attr_spec with id; size = Some (Ast.IntNum n)}

  let string ~id = bytes ~id

  let string_fixed ~id n = bytes_fixed ~id n

  let string_eos ~id = bytes_eos ~id

  let n ~id =
    {
      Helpers.default_attr_spec with
      id;
      dataType = DataType.(ComplexDataType (UserType (snd Type.n)));
    }

  let z ~id =
    {
      Helpers.default_attr_spec with
      id;
      dataType = DataType.(ComplexDataType (UserType (snd Type.z)));
    }
end

module Class = struct
  let bool ~encoding_name ?description () =
    Helpers.class_spec_of_attrs
      ~encoding_name
      ?description
      ~enums:[Enum.bool]
      [Attr.bool ~id:encoding_name]

  let uint8 ~encoding_name ?description () =
    Helpers.class_spec_of_attrs
      ~encoding_name
      ?description
      [Attr.uint8 ~id:encoding_name]

  let int8 ~encoding_name ?description () =
    Helpers.class_spec_of_attrs
      ~encoding_name
      ?description
      [Attr.int8 ~id:encoding_name]

  let uint16 ~encoding_name ?description () =
    Helpers.class_spec_of_attrs
      ~encoding_name
      ?description
      [Attr.uint16 ~id:encoding_name]

  let int16 ~encoding_name ?description () =
    Helpers.class_spec_of_attrs
      ~encoding_name
      ?description
      [Attr.int16 ~id:encoding_name]

  let int32 ~encoding_name ?description () =
    Helpers.class_spec_of_attrs
      ~encoding_name
      ?description
      [Attr.int32 ~id:encoding_name]

  let int64 ~encoding_name ?description () =
    Helpers.class_spec_of_attrs
      ~encoding_name
      ?description
      [Attr.int64 ~id:encoding_name]

  let int31 ~encoding_name ?description () =
    Helpers.class_spec_of_attrs
      ~encoding_name
      ?description
      [Attr.int31 ~id:encoding_name]

  let float ~encoding_name ?description () =
    Helpers.class_spec_of_attrs
      ~encoding_name
      ?description
      [Attr.float ~id:encoding_name]

  let bytes ~encoding_name ?description () =
    Helpers.class_spec_of_attrs
      ~encoding_name
      ?description
      [Attr.bytes ~id:encoding_name]

  let string ~encoding_name ?description () =
    Helpers.class_spec_of_attrs
      ~encoding_name
      ?description
      [Attr.string ~id:encoding_name]

  let n ~encoding_name ?description () =
    Helpers.class_spec_of_attrs ~encoding_name ?description [Attr.n ~id:"n"]

  let z ~encoding_name ?description () =
    Helpers.class_spec_of_attrs ~encoding_name ?description [Attr.n ~id:"z"]
end
