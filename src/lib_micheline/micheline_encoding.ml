(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Micheline

let canonical_location_encoding =
  let open Data_encoding in
  def
    "micheline.location"
    ~title:"Canonical location in a Micheline expression"
    ~description:
      "The location of a node in a Micheline expression tree in prefix order, \
       with zero being the root and adding one for every basic node, sequence \
       and primitive application."
  @@ int31

(* Along the life of the Micheline encoding, several bugs have been discovered
   and fixed. Each time, in order to preserve backwards compatibility within the
   protocol, the existing buggy implementation is kept and a new implementation
   is added. This leads to versioning, controlled via the [semantics] values. *)
type semantics = V0 | V1 | V2

let internal_canonical_encoding ~semantics ~variant prim_encoding =
  let open Data_encoding in
  let int_encoding = obj1 (req "int" z) in
  let string_encoding = obj1 (req "string" string) in
  let bytes_encoding = obj1 (req "bytes" bytes) in
  let int_encoding tag =
    case
      tag
      int_encoding
      ~title:"Int"
      (function Int (_, v) -> Some v | _ -> None)
      (fun v -> Int (0, v))
  in
  let string_encoding tag =
    case
      tag
      string_encoding
      ~title:"String"
      (function String (_, v) -> Some v | _ -> None)
      (fun v -> String (0, v))
  in
  let bytes_encoding tag =
    case
      tag
      bytes_encoding
      ~title:"Bytes"
      (function Bytes (_, v) -> Some v | _ -> None)
      (fun v -> Bytes (0, v))
  in
  let seq_encoding tag expr_encoding =
    case
      tag
      (list expr_encoding)
      ~title:"Sequence"
      (function Seq (_, v) -> Some v | _ -> None)
      (fun args -> Seq (0, args))
  in
  let annots_encoding =
    match semantics with
    | V0 | V1 ->
        let split s =
          if s = "" then
            if semantics = V0 then
              (* in V0 there was a bug whereby an empty list of annotation ([[]])
                 would be decoded as a list containing one empty string ([[""]]).
                 Thus, the special case for [semantics <> V0]) *)
              [""]
            else []
          else
            let annots = String.split_on_char ' ' s in
            List.iter
              (fun a ->
                if String.length a > 255 then failwith "Oversized annotation")
              annots ;
            if String.concat " " annots <> s then
              failwith
                "Invalid annotation string, must be a sequence of valid \
                 annotations with spaces" ;
            annots
        in
        splitted
          ~json:(list (Bounded.string 255))
          ~binary:(conv (String.concat " ") split string)
    | V2 ->
        (* in V1 there was a bug whereby a syntactically invalid annotation
           (i.e., one containing invalid characters) would be decoded as a valid
           annotation. Thus this branch where an additional well-formed-ness
           check is performed. *)
        let well_formed_annotation s =
          if Micheline_parser.check_annot s then Ok ()
          else Error "Malformed annotation"
        in
        let split_with_guard s =
          let annots = if s = "" then [] else String.split_on_char ' ' s in
          if List.for_all Micheline_parser.check_annot annots then Ok annots
          else Error "Malformed annotation"
        in
        splitted
          ~json:
            (list
               (with_decoding_guard well_formed_annotation (Bounded.string 255)))
          ~binary:(conv_with_guard (String.concat " ") split_with_guard string)
  in
  let application_encoding tag expr_encoding =
    case
      tag
      ~title:"Prim__generic"
      ~description:
        "Generic primitive (any number of args with or without annotations)"
      (obj3
         (req "prim" prim_encoding)
         (dft "args" (list expr_encoding) [])
         (dft "annots" annots_encoding []))
      (function
        | Prim (_, prim, args, annots) -> Some (prim, args, annots) | _ -> None)
      (fun (prim, args, annots) -> Prim (0, prim, args, annots))
  in
  let node_encoding =
    mu
      ("micheline." ^ variant ^ ".expression")
      (fun expr_encoding ->
        splitted
          ~json:
            (union
               ~tag_size:`Uint8
               [
                 int_encoding Json_only;
                 string_encoding Json_only;
                 bytes_encoding Json_only;
                 seq_encoding Json_only expr_encoding;
                 application_encoding Json_only expr_encoding;
               ])
          ~binary:
            (union
               ~tag_size:`Uint8
               [
                 int_encoding (Tag 0);
                 string_encoding (Tag 1);
                 seq_encoding (Tag 2) expr_encoding;
                 (* No args, no annots *)
                 case
                   (Tag 3)
                   ~title:"Prim__no_args__no_annots"
                   ~description:"Primitive with no arguments and no annotations"
                   (obj1 (req "prim" prim_encoding))
                   (function Prim (_, v, [], []) -> Some v | _ -> None)
                   (fun v -> Prim (0, v, [], []));
                 (* No args, with annots *)
                 case
                   (Tag 4)
                   ~title:"Prim__no_args__some_annots"
                   ~description:
                     "Primitive with no arguments and some annotations"
                   (obj2
                      (req "prim" prim_encoding)
                      (req "annots" annots_encoding))
                   (function
                     | Prim (_, v, [], annots) -> Some (v, annots) | _ -> None)
                   (function prim, annots -> Prim (0, prim, [], annots));
                 (* Single arg, no annots *)
                 case
                   (Tag 5)
                   ~title:"Prim__1_arg__no_annots"
                   ~description:"Primitive with one argument and no annotations"
                   (obj2 (req "prim" prim_encoding) (req "arg" expr_encoding))
                   (function
                     | Prim (_, v, [arg], []) -> Some (v, arg) | _ -> None)
                   (function prim, arg -> Prim (0, prim, [arg], []));
                 (* Single arg, with annots *)
                 case
                   (Tag 6)
                   ~title:"Prim__1_arg__some_annots"
                   ~description:
                     "Primitive with one argument and some annotations"
                   (obj3
                      (req "prim" prim_encoding)
                      (req "arg" expr_encoding)
                      (req "annots" annots_encoding))
                   (function
                     | Prim (_, prim, [arg], annots) -> Some (prim, arg, annots)
                     | _ -> None)
                   (fun (prim, arg, annots) -> Prim (0, prim, [arg], annots));
                 (* Two args, no annots *)
                 case
                   (Tag 7)
                   ~title:"Prim__2_args__no_annots"
                   ~description:
                     "Primitive with two arguments and no annotations"
                   (obj3
                      (req "prim" prim_encoding)
                      (req "arg1" expr_encoding)
                      (req "arg2" expr_encoding))
                   (function
                     | Prim (_, prim, [arg1; arg2], []) ->
                         Some (prim, arg1, arg2)
                     | _ -> None)
                   (fun (prim, arg1, arg2) -> Prim (0, prim, [arg1; arg2], []));
                 (* Two args, with annots *)
                 case
                   (Tag 8)
                   ~title:"Prim__2_args__some_annots"
                   ~description:
                     "Primitive with two arguments and some annotations"
                   (obj4
                      (req "prim" prim_encoding)
                      (req "arg1" expr_encoding)
                      (req "arg2" expr_encoding)
                      (req "annots" annots_encoding))
                   (function
                     | Prim (_, prim, [arg1; arg2], annots) ->
                         Some (prim, arg1, arg2, annots)
                     | _ -> None)
                   (fun (prim, arg1, arg2, annots) ->
                     Prim (0, prim, [arg1; arg2], annots));
                 (* General case *)
                 application_encoding (Tag 9) expr_encoding;
                 bytes_encoding (Tag 10);
               ]))
  in
  conv root (fun node -> strip_locations node) node_encoding

let canonical_encoding ~variant prim_encoding =
  internal_canonical_encoding ~semantics:V2 ~variant prim_encoding

let canonical_encoding_v2 ~variant prim_encoding =
  internal_canonical_encoding ~semantics:V2 ~variant prim_encoding

let canonical_encoding_v1 ~variant prim_encoding =
  internal_canonical_encoding ~semantics:V1 ~variant prim_encoding

let canonical_encoding_v0 ~variant prim_encoding =
  internal_canonical_encoding ~semantics:V0 ~variant prim_encoding

let table_encoding ~variant location_encoding prim_encoding =
  let open Data_encoding in
  conv
    (fun node ->
      let canon, assoc = extract_locations node in
      let _, table = List.split assoc in
      (canon, table))
    (fun (canon, table) ->
      let table = Array.of_list table in
      inject_locations (fun i -> table.(i)) canon)
    (obj2
       (req "expression" (canonical_encoding ~variant prim_encoding))
       (req "locations" (list location_encoding)))

let erased_encoding ~variant default_location prim_encoding =
  let open Data_encoding in
  conv
    (fun node -> strip_locations node)
    (fun canon -> inject_locations (fun _ -> default_location) canon)
    (canonical_encoding ~variant prim_encoding)

let node_encoding =
  table_encoding
    ~variant:"generic"
    Micheline_parser.location_encoding
    Data_encoding.string

let () =
  Error_monad.register_error_kind
    `Permanent
    ~id:"micheline.parse_error.misaligned_node"
    ~title:"Micheline parser error: misaligned node"
    ~description:
      "While parsing a piece of Micheline source, an expression was not \
       aligned with its siblings of the same mother application or sequence."
    ~pp:(fun ppf node ->
      Format.fprintf
        ppf
        "%a, misaligned expression"
        Micheline_parser.print_location
        (location node))
    Data_encoding.(obj1 (req "expression" node_encoding))
    (function Micheline_parser.Misaligned node -> Some node | _ -> None)
    (fun node -> Micheline_parser.Misaligned node)
