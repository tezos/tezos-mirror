(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Kaitai.Types

module Enum = struct
  type assoc = (string * Kaitai.Types.EnumSpec.t) list

  let bool_false_name = "false"

  let bool_true_name = "true"

  let bool =
    ( "bool",
      EnumSpec.
        {
          map =
            [
              ( 0,
                EnumValueSpec.
                  {name = bool_false_name; doc = Helpers.default_doc_spec} );
              ( 255,
                EnumValueSpec.
                  {name = bool_true_name; doc = Helpers.default_doc_spec} );
            ];
        } )
end

(* Defining a few types now to break circular dependencies. *)

let n_chunk_type =
  ( "n_chunk",
    {
      (* TODO/nice to have: Add a docstring, i.e. [?description]
                            to custom defined class spec. *)
      (Helpers.default_class_spec ())
      with
      seq =
        [
          {
            (Helpers.default_attr_spec ~id:"has_more") with
            dataType =
              DataType.(
                NumericType
                  (Int_type (BitsType {width = 1; bit_endian = BigBitEndian})));
          };
          {
            (Helpers.default_attr_spec ~id:"payload") with
            dataType =
              DataType.(
                NumericType
                  (Int_type (BitsType {width = 7; bit_endian = BigBitEndian})));
          };
        ];
    } )

let n_seq_attr =
  {
    (Helpers.default_attr_spec ~id:"n") with
    dataType = ComplexDataType (UserType (fst n_chunk_type));
    cond =
      {
        Helpers.cond_no_cond with
        repeat =
          RepeatUntil
            (UnaryOp
               {
                 op = Not;
                 operand =
                   CastToType
                     {
                       value = Attribute {value = Name "_"; attr = "has_more"};
                       typeName =
                         {absolute = false; names = ["bool"]; isArray = false};
                     };
               });
      };
  }

let n_type =
  ( "n",
    {
      (* TODO/nice to have: Add a docstring, i.e. [?description]
                            to custom defined class spec. *)
      (Helpers.default_class_spec ())
      with
      seq = [n_seq_attr];
    } )

let z_type =
  ( "z",
    {
      (Helpers.default_class_spec ()) with
      seq =
        [
          {
            (Helpers.default_attr_spec ~id:"has_tail") with
            dataType =
              DataType.(
                NumericType
                  (Int_type (BitsType {width = 1; bit_endian = BigBitEndian})));
          };
          {
            (Helpers.default_attr_spec ~id:"sign") with
            dataType =
              DataType.(
                NumericType
                  (Int_type (BitsType {width = 1; bit_endian = BigBitEndian})));
          };
          {
            (Helpers.default_attr_spec ~id:"payload") with
            dataType =
              DataType.(
                NumericType
                  (Int_type (BitsType {width = 6; bit_endian = BigBitEndian})));
          };
          {
            (Helpers.default_attr_spec ~id:"tail") with
            dataType = ComplexDataType (UserType (fst n_chunk_type));
            cond =
              {
                ifExpr =
                  Some
                    (CastToType
                       {
                         value = Name "has_tail";
                         typeName =
                           {absolute = false; names = ["bool"]; isArray = false};
                       });
                repeat =
                  RepeatUntil
                    (UnaryOp
                       {
                         op = Not;
                         operand =
                           CastToType
                             {
                               value =
                                 Attribute {value = Name "_"; attr = "has_more"};
                               typeName =
                                 {
                                   absolute = false;
                                   names = ["bool"];
                                   isArray = false;
                                 };
                             };
                       });
              };
          };
        ];
    } )

let int1_type_attr_spec ~id ~signed =
  {
    (Helpers.default_attr_spec ~id) with
    dataType = DataType.(NumericType (Int_type (Int1Type {signed})));
  }

let int_multi_type_attr_spec ~id ~signed width =
  {
    (Helpers.default_attr_spec ~id) with
    dataType =
      DataType.(
        NumericType (Int_type (IntMultiType {signed; width; endian = None})));
  }

let uint30_attr ~id =
  (* the integer literal bounds are from data-encoding source, specifically
     the binary reader *)
  {
    (int_multi_type_attr_spec ~id ~signed:false DataType.W4) with
    valid = Some (ValidationSpec.ValidationMax (Ast.IntNum ((1 lsl 30) - 1)));
  }

module Type = struct
  type assoc = (string * Kaitai.Types.ClassSpec.t) list

  let n_chunk = n_chunk_type

  let n = n_type

  let z = z_type

  let uint30 =
    ( "uint30",
      (* the integer literal bounds are from data-encoding source, specifically
         the binary reader *)
      {(Helpers.default_class_spec ()) with seq = [uint30_attr ~id:"uint30"]} )

  let int31 =
    ( "int31",
      (* the integer literal bounds are from data-encoding source, specifically
         the binary reader *)
      {
        (Helpers.default_class_spec ()) with
        seq =
          [
            {
              (int_multi_type_attr_spec ~id:"int31" ~signed:true DataType.W4) with
              valid =
                Some
                  (ValidationSpec.ValidationRange
                     {
                       min = Ast.IntNum (-0x4000_0000);
                       max = Ast.IntNum 0x3fff_ffff;
                     });
            };
          ];
      } )

  let bytes_dyn_uint8 =
    let id = "bytes_dyn_uint8" in
    let size_id = "len_" ^ id in
    ( id,
      {
        (Helpers.default_class_spec ()) with
        seq =
          [
            int1_type_attr_spec ~id:size_id ~signed:false;
            {
              (Helpers.default_attr_spec ~id) with
              dataType =
                DataType.(
                  BytesType
                    (BytesLimitType
                       {
                         size = Ast.Name size_id;
                         terminator = None;
                         include_ = false;
                         padRight = None;
                         process = None;
                       }));
              size = Some (Ast.Name size_id);
            };
          ];
      } )

  let bytes_dyn_uint16 =
    let id = "bytes_dyn_uint16" in
    let size_id = "len_" ^ id in
    ( id,
      {
        (Helpers.default_class_spec ()) with
        seq =
          [
            int_multi_type_attr_spec ~id:size_id ~signed:false DataType.W2;
            {
              (Helpers.default_attr_spec ~id) with
              dataType =
                DataType.(
                  BytesType
                    (BytesLimitType
                       {
                         size = Ast.Name size_id;
                         terminator = None;
                         include_ = false;
                         padRight = None;
                         process = None;
                       }));
              size = Some (Ast.Name size_id);
            };
          ];
      } )

  let bytes_dyn_uint30 =
    let id = "bytes_dyn_uint30" in
    let size_id = "len_" ^ id in
    ( id,
      {
        (Helpers.default_class_spec ()) with
        seq =
          [
            uint30_attr ~id:size_id;
            {
              (Helpers.default_attr_spec ~id) with
              dataType =
                DataType.(
                  BytesType
                    (BytesLimitType
                       {
                         size = Ast.Name size_id;
                         terminator = None;
                         include_ = false;
                         padRight = None;
                         process = None;
                       }));
              size = Some (Ast.Name size_id);
            };
          ];
      } )
end

module Attr = struct
  let bool ~id =
    {
      (Helpers.default_attr_spec ~id) with
      dataType = DataType.(NumericType (Int_type (Int1Type {signed = false})));
      enum = Some (fst Enum.bool);
    }

  let int1_type_attr_spec ~id ~signed =
    {
      (Helpers.default_attr_spec ~id) with
      dataType = DataType.(NumericType (Int_type (Int1Type {signed})));
    }

  let int_multi_type_attr_spec ~id ~signed width =
    {
      (Helpers.default_attr_spec ~id) with
      dataType =
        DataType.(
          NumericType (Int_type (IntMultiType {signed; width; endian = None})));
    }

  let float_multi_type_attr_spec ~id =
    {
      (Helpers.default_attr_spec ~id) with
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

  let uint16 ~id = int_multi_type_attr_spec ~id ~signed:false DataType.W2

  let int16 ~id = int_multi_type_attr_spec ~id ~signed:true DataType.W2

  let int32 ~id = int_multi_type_attr_spec ~id ~signed:true DataType.W4

  let int64 ~id = int_multi_type_attr_spec ~id ~signed:true DataType.W8

  let int31 ~id =
    {
      (Helpers.default_attr_spec ~id) with
      dataType = ComplexDataType (UserType (fst Type.int31));
    }

  let uint30 ~id =
    (* the integer literal bounds are from data-encoding source, specifically
       the binary reader *)
    {
      (int_multi_type_attr_spec ~id ~signed:false DataType.W4) with
      valid = Some (ValidationSpec.ValidationMax (Ast.IntNum ((1 lsl 30) - 1)));
    }

  let float ~id = float_multi_type_attr_spec ~id

  type byte_size =
    | Fixed of int  (** [size: <int>] *)
    | Dynamic8  (** pointer to [Type.bytes_dyn_uint8] *)
    | Dynamic16  (** pointer to [Type.bytes_dyn_uint16] *)
    | Dynamic30  (** pointer to [Type.bytes_dyn_uint30] *)
    | Variable  (** [size-eos: true] *)

  let bytes ~id = function
    | Fixed n ->
        {
          (Helpers.default_attr_spec ~id) with
          dataType =
            DataType.(
              BytesType
                (BytesLimitType
                   {
                     size = Ast.IntNum n;
                     terminator = None;
                     include_ = false;
                     padRight = None;
                     process = None;
                   }));
          size = Some (Ast.IntNum n);
        }
    | Dynamic8 ->
        {
          (Helpers.default_attr_spec ~id) with
          dataType = ComplexDataType (UserType (fst Type.bytes_dyn_uint8));
        }
    | Dynamic16 ->
        {
          (Helpers.default_attr_spec ~id) with
          dataType = ComplexDataType (UserType (fst Type.bytes_dyn_uint16));
        }
    | Dynamic30 ->
        {
          (Helpers.default_attr_spec ~id) with
          dataType = ComplexDataType (UserType (fst Type.bytes_dyn_uint30));
        }
    | Variable ->
        {
          (Helpers.default_attr_spec ~id) with
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

  let string = bytes

  let n ~id =
    {
      (Helpers.default_attr_spec ~id) with
      dataType = ComplexDataType (UserType (fst Type.n));
    }

  let z ~id =
    {
      (Helpers.default_attr_spec ~id) with
      dataType = ComplexDataType (UserType (fst Type.z));
    }

  let binary_length_kind ~id kind =
    match kind with
    | `N -> failwith "Not implemented"
    | `Uint30 -> uint30 ~id
    | `Uint16 -> uint16 ~id
    | `Uint8 -> uint8 ~id
end
