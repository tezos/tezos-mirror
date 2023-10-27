(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* Identifiers have a strict pattern to follow, this function removes the
   irregularities.

   Without escaping, the kaitai-struct-compiler throws the following error
   message: [error: invalid meta ID, expected /^[a-z][a-z0-9_]*$/] *)
let escape_id id =
  if String.length id < 1 then raise (Invalid_argument "empty id") ;
  let b = Buffer.create (String.length id) in
  if match id.[0] with 'a' .. 'z' | 'A' .. 'Z' -> false | _ -> true then
    Buffer.add_string b "id_" ;
  String.iter
    (function
      | ('a' .. 'z' | '0' .. '9' | '_') as c -> Buffer.add_char b c
      | 'A' .. 'Z' as c -> Buffer.add_char b (Char.lowercase_ascii c)
      | '.' | '-' | ' ' -> Buffer.add_string b "__"
      | c ->
          (* we print [%S] to force escaping of the character *)
          raise
            (Failure
               (Format.asprintf
                  "Unsupported: special character (%C) in id (%S)"
                  c
                  id)))
    id ;
  Buffer.contents b

open Kaitai.Types

(* We need to access the definition of data-encoding's [descr] type. For this
   reason we alias the private/internal module [Data_encoding__Encoding] (rather
   than the public module [Data_encoding.Encoding]. *)
module DataEncoding = Data_encoding__Encoding

let summary ~title ~description =
  match (title, description) with
  | None, None -> None
  | None, (Some _ as s) | (Some _ as s), None -> s
  | Some t, Some d -> Some (t ^ ": " ^ d)

(* in kaitai-struct, some fields can be added to single attributes but not to a
   group of them. When we want to attach a field to a group of attributes, we
   need to create an indirection to a named type. [redirect] is a function for
   adding a field to an indirection. *)
let redirect types attrs fattr id =
  let ((_, user_type) as type_) = (id, Helpers.class_spec_of_attrs ~id attrs) in
  let types = Helpers.add_uniq_assoc types type_ in
  let attr =
    fattr
      {
        (Helpers.default_attr_spec ~id) with
        dataType = DataType.(ComplexDataType (UserType user_type));
      }
  in
  (types, attr)

(* [redirect_if_many] is like [redirect] but it only does the redirection when
   there are multiple attributes, otherwise it adds the field directly. *)
let redirect_if_many :
    Ground.Type.assoc ->
    AttrSpec.t list ->
    (AttrSpec.t -> AttrSpec.t) ->
    string ->
    Ground.Type.assoc * AttrSpec.t =
 fun types attrs fattr id ->
  match attrs with
  | [] -> failwith "Not supported"
  | [attr] -> (types, {(fattr attr) with id})
  | _ :: _ :: _ as attrs -> redirect types attrs fattr id

let rec seq_field_of_data_encoding :
    type a.
    Ground.Enum.assoc ->
    Ground.Type.assoc ->
    a DataEncoding.t ->
    string ->
    Ground.Enum.assoc * Ground.Type.assoc * AttrSpec.t list =
 fun enums types {encoding; _} id ->
  let id = escape_id id in
  match encoding with
  | Null -> (enums, types, [])
  | Empty -> (enums, types, [])
  | Ignore -> (enums, types, [])
  | Constant _ -> (enums, types, [])
  | Bool ->
      let enums = Helpers.add_uniq_assoc enums Ground.Enum.bool in
      (enums, types, [Ground.Attr.bool ~id])
  | Uint8 -> (enums, types, [Ground.Attr.uint8 ~id])
  | Int8 -> (enums, types, [Ground.Attr.int8 ~id])
  | Uint16 -> (enums, types, [Ground.Attr.uint16 ~id])
  | Int16 -> (enums, types, [Ground.Attr.int16 ~id])
  | Int32 -> (enums, types, [Ground.Attr.int32 ~id])
  | Int64 -> (enums, types, [Ground.Attr.int64 ~id])
  | Int31 -> (enums, types, [Ground.Attr.int31 ~id])
  | RangedInt {minimum; maximum} ->
      let size = Data_encoding__Binary_size.range_to_size ~minimum ~maximum in
      if minimum <= 0 then
        let valid =
          Some
            (ValidationSpec.ValidationRange
               {min = Ast.IntNum minimum; max = Ast.IntNum maximum})
        in
        let uvalid =
          if minimum = 0 then
            Some (ValidationSpec.ValidationMax (Ast.IntNum maximum))
          else valid
        in
        match size with
        | `Uint8 ->
            (enums, types, [{(Ground.Attr.uint8 ~id) with valid = uvalid}])
        | `Uint16 ->
            (enums, types, [{(Ground.Attr.uint16 ~id) with valid = uvalid}])
        | `Uint30 ->
            (enums, types, [{(Ground.Attr.uint30 ~id) with valid = uvalid}])
        | `Int8 -> (enums, types, [{(Ground.Attr.int8 ~id) with valid}])
        | `Int16 -> (enums, types, [{(Ground.Attr.int16 ~id) with valid}])
        | `Int31 -> (enums, types, [{(Ground.Attr.int31 ~id) with valid}])
      else
        (* when [minimum > 0] (as is the case in this branch), data-encoding
           shifts the value of the binary representation so that the minimum is at
           [0]. E.g., the interval [200]–[300] is represented on the wire as the
           interval [0]-[100] and the de/serialisation function is responsible for
           shifting to/from the actual range. *)
        let shift = minimum in
        let shifted_id = id ^ "_shifted_to_zero" in
        let shifted_encoding : a DataEncoding.t =
          {
            encoding =
              RangedInt {minimum = minimum - shift; maximum = maximum - shift};
            json_encoding = None;
          }
        in
        let enums, types, represented_interval_attrs =
          seq_field_of_data_encoding enums types shifted_encoding shifted_id
        in
        let instance_type : Kaitai.Types.DataType.int_type =
          match size with
          | `Uint8 -> Int1Type {signed = false}
          | `Uint16 -> IntMultiType {signed = false; width = W2; endian = None}
          | `Uint30 -> IntMultiType {signed = false; width = W4; endian = None}
          | `Int8 -> Int1Type {signed = true}
          | `Int16 -> IntMultiType {signed = true; width = W2; endian = None}
          | `Int31 -> IntMultiType {signed = true; width = W4; endian = None}
        in
        let represented_interval_class =
          Helpers.class_spec_of_attrs
            ~id:shifted_id
            ~instances:
              [
                ( "value",
                  {
                    doc =
                      {
                        summary =
                          Some
                            "The interval is represented shifted towards 0 for \
                             compactness, this instance corrects the shift.";
                        refs = [];
                      };
                    descr =
                      ValueInstanceSpec
                        {
                          id = "value";
                          path = [];
                          value =
                            BinOp
                              {
                                left = Name shifted_id;
                                op = Add;
                                right = IntNum shift;
                              };
                          ifExpr = None;
                          dataTypeOpt =
                            Some (NumericType (Int_type instance_type));
                        };
                  } );
              ]
            represented_interval_attrs
        in
        let types =
          Helpers.add_uniq_assoc types (shifted_id, represented_interval_class)
        in
        ( enums,
          types,
          [
            {
              (Helpers.default_attr_spec ~id) with
              dataType =
                DataType.(ComplexDataType (UserType represented_interval_class));
            };
          ] )
  | Float -> (enums, types, [Ground.Attr.float ~id])
  | RangedFloat {minimum; maximum} ->
      let valid =
        Some
          (ValidationSpec.ValidationRange
             {min = Ast.FloatNum minimum; max = Ast.FloatNum maximum})
      in
      (enums, types, [{(Ground.Attr.float ~id) with valid}])
  | Bytes (`Fixed n, _) -> (enums, types, [Ground.Attr.bytes ~id (Fixed n)])
  | Bytes (`Variable, _) -> (enums, types, [Ground.Attr.bytes ~id Variable])
  | Dynamic_size {kind; encoding = {encoding = Bytes (`Variable, _); _}} ->
      let size_id = "len_" ^ id in
      let size_attr = Ground.Attr.binary_length_kind ~id:size_id kind in
      (enums, types, [size_attr; Ground.Attr.bytes ~id (Dynamic size_id)])
  | String (`Fixed n, _) -> (enums, types, [Ground.Attr.string ~id (Fixed n)])
  | String (`Variable, _) -> (enums, types, [Ground.Attr.string ~id Variable])
  | Dynamic_size {kind; encoding = {encoding = String (`Variable, _); _}} ->
      let size_id = "len_" ^ id in
      let size_attr = Ground.Attr.binary_length_kind ~id:size_id kind in
      (enums, types, [size_attr; Ground.Attr.string ~id (Dynamic size_id)])
  | N ->
      let types = Helpers.add_uniq_assoc types Ground.Type.n_chunk in
      let types = Helpers.add_uniq_assoc types Ground.Type.n in
      (enums, types, [Ground.Attr.n ~id])
  | Z ->
      let types = Helpers.add_uniq_assoc types Ground.Type.n_chunk in
      let types = Helpers.add_uniq_assoc types Ground.Type.z in
      (enums, types, [Ground.Attr.z ~id])
  | Conv {encoding; _} -> seq_field_of_data_encoding enums types encoding id
  | Tup _ ->
      (* single-field tup *)
      let tid_gen =
        let already_called = ref false in
        fun () ->
          if !already_called then
            raise (Invalid_argument "multiple fields inside a single-field tup") ;
          already_called := true ;
          id
      in
      seq_field_of_tups enums types tid_gen encoding
  | Tups _ ->
      (* multi-field tup *)
      let tid_gen = Helpers.mk_tid_gen id in
      seq_field_of_tups enums types tid_gen encoding
  | Obj f -> seq_field_of_field enums types f
  | Objs {kind = _; left; right} ->
      let enums, types, left = seq_field_of_data_encoding enums types left id in
      let enums, types, right =
        seq_field_of_data_encoding enums types right id
      in
      let seq = left @ right in
      (enums, types, seq)
  | Dynamic_size {kind; encoding} ->
      let len_id = "len_" ^ id in
      let len_attr =
        match kind with
        | `N -> failwith "Not implemented"
        | `Uint30 -> Ground.Attr.uint30 ~id:len_id
        | `Uint16 -> Ground.Attr.uint16 ~id:len_id
        | `Uint8 -> Ground.Attr.uint8 ~id:len_id
      in
      let enums, types, attrs =
        seq_field_of_data_encoding enums types encoding id
      in
      let types, attr =
        redirect_if_many
          types
          attrs
          (fun attr -> {attr with size = Some (Ast.Name len_id)})
          id
      in
      (enums, types, [len_attr; attr])
  | Splitted {encoding; json_encoding = _; is_obj = _; is_tup = _} ->
      seq_field_of_data_encoding enums types encoding id
  | Describe {encoding; id; description; title} -> (
      let id = escape_id id in
      let description = summary ~title ~description in
      let enums, types, attrs =
        seq_field_of_data_encoding enums types encoding id
      in
      match attrs with
      | [] -> failwith "Not supported"
      | [attr] -> (enums, types, [Helpers.merge_summaries attr description])
      | _ :: _ :: _ as attrs ->
          let described_class =
            Helpers.class_spec_of_attrs
              ~id
              ?description
              ~enums:[]
              ~types:[]
              ~instances:[]
              attrs
          in
          let types = Helpers.add_uniq_assoc types (id, described_class) in
          let attr =
            {
              (Helpers.default_attr_spec ~id) with
              dataType = DataType.(ComplexDataType (UserType described_class));
            }
          in
          (enums, types, [attr]))
  | Check_size {limit = _; encoding} ->
      (* TODO: Add a guard for check size.*)
      seq_field_of_data_encoding enums types encoding id
  | _ -> failwith "Not implemented"

and seq_field_of_tups :
    type a.
    Ground.Enum.assoc ->
    Ground.Type.assoc ->
    Helpers.tid_gen ->
    a DataEncoding.desc ->
    Ground.Enum.assoc * Ground.Type.assoc * AttrSpec.t list =
 fun enums types tid_gen d ->
  match d with
  | Tup {encoding = Tup _ as e; json_encoding = _} ->
      seq_field_of_tups enums types tid_gen e
  | Tup {encoding = Tups _ as e; json_encoding = _} ->
      seq_field_of_tups enums types tid_gen e
  | Tup e ->
      let id = tid_gen () in
      let enums, types, attrs = seq_field_of_data_encoding enums types e id in
      let types, attrs =
        match attrs with
        | [] -> (types, [])
        | [attr] ->
            if attr.id = id then (types, attrs)
            else
              (* [id] was over-ridden by a [Describe] or other construct, we
                 restore it here to guarantee names are unique. *)
              (types, [Helpers.merge_summaries {attr with id} (Some attr.id)])
        | _ :: _ ->
            let types, attr = redirect types attrs Fun.id id in
            (types, [attr])
      in
      (enums, types, attrs)
  | Tups {kind = _; left; right} ->
      let enums, types, left =
        seq_field_of_tups enums types tid_gen left.encoding
      in
      let enums, types, right =
        seq_field_of_tups enums types tid_gen right.encoding
      in
      let seq = left @ right in
      (enums, types, seq)
  | _ -> failwith "Non-tup(s) inside a tups"

and seq_field_of_field :
    type a.
    Ground.Enum.assoc ->
    Ground.Type.assoc ->
    a DataEncoding.field ->
    Ground.Enum.assoc * Ground.Type.assoc * AttrSpec.t list =
 fun enums types f ->
  match f with
  | Req {name; encoding; title; description} ->
      let id = escape_id name in
      let enums, types, attrs =
        seq_field_of_data_encoding enums types encoding id
      in
      let summary = summary ~title ~description in
      let types, attr =
        redirect_if_many
          types
          attrs
          (fun attr -> Helpers.merge_summaries attr summary)
          id
      in
      (enums, types, [attr])
  | Opt {name; kind = _; encoding; title; description} ->
      let cond_id = escape_id (name ^ "_tag") in
      let enums = Helpers.add_uniq_assoc enums Ground.Enum.bool in
      let cond_attr = Ground.Attr.bool ~id:cond_id in
      let cond =
        {
          Helpers.cond_no_cond with
          ifExpr =
            Some
              Ast.(
                Compare
                  {
                    left = Name cond_id;
                    ops = Eq;
                    right =
                      EnumByLabel
                        {
                          enumName = fst Ground.Enum.bool;
                          label = Ground.Enum.bool_true_name;
                          inType =
                            {
                              absolute = true;
                              names = [fst Ground.Enum.bool];
                              isArray = false;
                            };
                        };
                  });
        }
      in
      let id = escape_id name in
      let enums, types, attrs =
        seq_field_of_data_encoding enums types encoding id
      in
      let summary = summary ~title ~description in
      let types, attr =
        redirect_if_many
          types
          attrs
          (fun attr -> {(Helpers.merge_summaries attr summary) with cond})
          id
      in
      (enums, types, [cond_attr; attr])
  | Dft {name; encoding; default = _; title; description} ->
      (* NOTE: in binary format Dft is the same as Req *)
      let id = escape_id name in
      let enums, types, attrs =
        seq_field_of_data_encoding enums types encoding id
      in
      let summary = summary ~title ~description in
      let types, attr =
        redirect_if_many
          types
          attrs
          (fun attr -> Helpers.merge_summaries attr summary)
          id
      in
      (enums, types, [attr])

let from_data_encoding :
    type a. id:string -> ?description:string -> a DataEncoding.t -> ClassSpec.t
    =
 fun ~id ?description encoding ->
  let encoding_name = escape_id id in
  match encoding.encoding with
  | Describe {encoding; description; id; _} ->
      let enums, types, attrs = seq_field_of_data_encoding [] [] encoding id in
      Helpers.class_spec_of_attrs
        ~top_level:true
        ~id:encoding_name
        ?description
        ~enums
        ~types
        ~instances:[]
        attrs
  | _ ->
      let enums, types, attrs =
        seq_field_of_data_encoding [] [] encoding encoding_name
      in
      Helpers.class_spec_of_attrs
        ~top_level:true
        ~id:encoding_name
        ?description
        ~enums
        ~types
        ~instances:[]
        attrs
