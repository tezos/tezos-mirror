(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Encoding

(* objs check that the same field doesn't appear twice *)

module SSet = Set.Make (String)

let sset_add name fields =
  (if SSet.mem name fields then
     let s =
       Printf.sprintf "Data_encoding.objects: duplicate field name (%S)" name
     in
     raise (Invalid_argument s)) ;
  SSet.add name fields

(* This must traverse the constructors in the same fashion as `is_obj` *)
let rec check_dup_fields : type a. Mu_visited.t -> SSet.t -> a t -> SSet.t =
 fun visited fields objs ->
  match objs.encoding with
  | Obj (Req {name; _} | Opt {name; _} | Dft {name; _}) -> sset_add name fields
  | Objs {left; right; _} ->
      let fields = check_dup_fields visited fields left in
      let fields = check_dup_fields visited fields right in
      fields
  | Conv {encoding = e; _} -> check_dup_fields visited fields e
  | Dynamic_size {encoding = e; _} -> check_dup_fields visited fields e
  | Union {cases; _} ->
      let fieldss =
        List.map
          (fun (Case {encoding = e; _}) -> check_dup_fields visited fields e)
          cases
      in
      List.fold_left SSet.union fields fieldss
  | Empty -> fields
  | Ignore -> fields
  | Mu {fix; _} ->
      assert (is_obj objs) ;
      if Mu_visited.mem objs.encoding visited then fields
      else
        check_dup_fields
          (Mu_visited.add objs.encoding visited)
          fields
          (fix objs)
  | Splitted {is_obj; _} ->
      (* TL;DR: the only combinator for splitted sets [is_obj] at [false].
                  Long explanation:
                  The only combinator that can construct [Splitted] is
                  [Data_encoding.Encoding.splitted]. It is defined in
         [src/data_encoding.ml] as a wrapper around [Encoding.raw_splitted] which sets
         the field [is_obj] to [false]. No other occurrences of [Splitted] as a
         constructor (rather than a pattern) exist in the code base.
      *)
      assert (is_obj = false) ;
      (* we assert it's not an object *)
      assert false
      (* we assert false bc it's not an object *)
  | Delayed f -> check_dup_fields visited fields (f ())
  | Describe {encoding; _} -> check_dup_fields visited fields encoding
  | Padded (_encoding, _) -> assert false
  | Check_size {encoding = _; _} -> assert false
  | String_enum _ -> assert false
  | Array _ -> assert false
  | List _ -> assert false
  | Tup _ -> assert false
  | Tups _ -> assert false
  | Null -> assert false
  | Constant _ -> assert false
  | Bool -> assert false
  | Int8 -> assert false
  | Uint8 -> assert false
  | Int16 _ -> assert false
  | Uint16 _ -> assert false
  | Int31 _ -> assert false
  | Int32 _ -> assert false
  | Int64 _ -> assert false
  | N -> assert false
  | Z -> assert false
  | RangedInt _ -> assert false
  | RangedFloat _ -> assert false
  | Float -> assert false
  | Bytes _ -> assert false
  | String _ -> assert false
  | Bigstring _ -> assert false

let obj1 f1 =
  let o = obj1 f1 in
  let _ = check_dup_fields Mu_visited.empty SSet.empty o in
  o

let obj2 f2 f1 =
  let o = obj2 f2 f1 in
  let _ = check_dup_fields Mu_visited.empty SSet.empty o in
  o

let obj3 f3 f2 f1 =
  let o = obj3 f3 f2 f1 in
  let _ = check_dup_fields Mu_visited.empty SSet.empty o in
  o

let obj4 f4 f3 f2 f1 =
  let o = obj4 f4 f3 f2 f1 in
  let _ = check_dup_fields Mu_visited.empty SSet.empty o in
  o

let obj5 f5 f4 f3 f2 f1 =
  let o = obj5 f5 f4 f3 f2 f1 in
  let _ = check_dup_fields Mu_visited.empty SSet.empty o in
  o

let obj6 f6 f5 f4 f3 f2 f1 =
  let o = obj6 f6 f5 f4 f3 f2 f1 in
  let _ = check_dup_fields Mu_visited.empty SSet.empty o in
  o

let obj7 f7 f6 f5 f4 f3 f2 f1 =
  let o = obj7 f7 f6 f5 f4 f3 f2 f1 in
  let _ = check_dup_fields Mu_visited.empty SSet.empty o in
  o

let obj8 f8 f7 f6 f5 f4 f3 f2 f1 =
  let o = obj8 f8 f7 f6 f5 f4 f3 f2 f1 in
  let _ = check_dup_fields Mu_visited.empty SSet.empty o in
  o

let obj9 f9 f8 f7 f6 f5 f4 f3 f2 f1 =
  let o = obj9 f9 f8 f7 f6 f5 f4 f3 f2 f1 in
  let _ = check_dup_fields Mu_visited.empty SSet.empty o in
  o

let obj10 f10 f9 f8 f7 f6 f5 f4 f3 f2 f1 =
  let o = obj10 f10 f9 f8 f7 f6 f5 f4 f3 f2 f1 in
  let _ = check_dup_fields Mu_visited.empty SSet.empty o in
  o

let merge_objs o1 o2 =
  let o = merge_objs o1 o2 in
  let _ = check_dup_fields Mu_visited.empty SSet.empty o in
  o

(* Unions have an additional `kind` field *)

let kind_field_name = "kind"

type case_tag = Tag of (int * string)

type 't case = 't Encoding.case

let case ~title ?description (Tag (tag, kind)) e proj inj =
  if not (is_obj e) then
    raise
      (Invalid_argument
         "Data_encoding.With_JSON_discriminant.case: encoding must be an obj") ;
  let e = merge_objs (obj1 (req kind_field_name (constant kind))) e in
  case
    ~title
    ?description
    (Encoding.Tag tag)
    e
    (fun x -> match proj x with None -> None | Some p -> Some ((), p))
    (fun ((), x) -> inj x)

let matched ?tag_size (tag, kind) e v =
  if not (is_obj e) then
    raise
      (Invalid_argument
         "Data_encoding.With_JSON_discriminant.case: encoding must be an obj") ;
  let e = merge_objs (obj1 (req kind_field_name (constant kind))) e in
  matched ?tag_size tag (conv (fun x -> ((), x)) (fun ((), x) -> x) e) v

let check_case_list cases =
  List.fold_left
    (fun kinds (Case {encoding = e; _}) ->
      match e.encoding with
      | Objs
          {
            left =
              {
                encoding =
                  Obj
                    (Req
                       {
                         name = kind_field_name_found;
                         encoding = {encoding = Constant kind; _};
                         _;
                       });
                _;
              };
            _;
          }
        when kind_field_name_found = kind_field_name ->
          (if SSet.mem kind kinds then
             let s =
               Printf.sprintf
                 "Data_encoding: two identical kind fields in union: %S"
                 kind
             in
             raise (Invalid_argument s)) ;
          SSet.add kind kinds
      | _ ->
          (* although the type [case] is an alias for [Encoding.case], the type
             equality is hidden from the end-user in the library's interface
             ([data_encoding.mli]). As a result, it is not possible to construct
             [Case]s with encodings other than
             [merge_objs (obj1 (req "kind" (constant kind_field_name)))]. *)
          assert false)
    SSet.empty
    cases

let union ?tag_size cases =
  let _ = check_case_list cases in
  union ?tag_size cases

let matching ?tag_size match_case cases =
  let _ = check_case_list cases in
  matching ?tag_size match_case cases
