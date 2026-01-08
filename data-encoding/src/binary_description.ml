(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type recursives = string list

type references = {
  descriptions : (string * Binary_schema.toplevel_encoding) list;
}
[@@unwrapped]

(* Simple Union find implementation, there are several optimizations
   that give UF it's usual time complexity that could be added.
   If this is a bottleneck, they're easy to add. *)
module UF : sig
  type t

  val add : t -> Binary_schema.description -> unit

  val find : t -> string -> Binary_schema.description

  val union :
    t -> new_canonical:Binary_schema.description -> existing:string -> unit

  val empty : unit -> t
end = struct
  open Binary_schema

  type ele = Ref of string | Root of description

  type t = (string, ele) Hashtbl.t

  let add t x = Hashtbl.replace t x.title (Root x)

  let rec find tbl key =
    match Hashtbl.find tbl key with Ref s -> find tbl s | Root desc -> desc

  let union tbl ~new_canonical ~existing =
    add tbl new_canonical ;
    let root = find tbl existing in
    if root.title <> new_canonical.title then
      Hashtbl.replace tbl root.title (Ref new_canonical.title)

  let empty () = Hashtbl.create 128
end

let fixup_references uf =
  let open Binary_schema in
  let rec fixup_layout = function
    | Ref s -> Ref (UF.find uf s).title
    | Enum (i, name) -> Enum (i, (UF.find uf name).title)
    | Seq (layout, len) -> Seq (fixup_layout layout, len)
    | ( Zero_width | Int _ | Bool
      | RangedInt (_, _, _)
      | RangedFloat (_, _)
      | Float | Bytes | String | Padding ) as enc ->
        enc
  in
  let field = function
    | Named_field (name, kind, layout) ->
        Named_field (name, kind, fixup_layout layout)
    | Anonymous_field (kind, layout) ->
        Anonymous_field (kind, fixup_layout layout)
    | (Dynamic_size_field _ | Optional_field _) as field -> field
  in
  function
  | Obj {fields} -> Obj {fields = List.map field fields}
  | Cases ({cases; _} as x) ->
      Cases
        {
          x with
          cases =
            List.map
              (fun (i, name, fields) -> (i, name, List.map field fields))
              cases;
        }
  | Int_enum _ as ie -> ie

let z_reference_name = "Z.t"

let z_reference_description =
  "A variable-length sequence of bytes encoding a Zarith integer. Each byte \
   has a running unary size bit: the most significant bit of each byte \
   indicates whether this is the last byte in the sequence (0) or whether the \
   sequence continues (1). The second most significant bit of the first byte \
   is reserved for the sign (0 for positive, 1 for negative). Size and sign \
   bits ignored, the data is the binary representation of the absolute value \
   of the number in little-endian order."

let z_encoding =
  Binary_schema.Obj {fields = [Named_field ("Z.t", `Dynamic, Bytes)]}

let add_z_reference uf {descriptions} =
  UF.add
    uf
    {title = z_reference_name; description = Some z_reference_description} ;
  {descriptions = (z_reference_name, z_encoding) :: descriptions}

let n_reference_name = "N.t"

let n_reference_description =
  "A variable-length sequence of bytes encoding a Zarith natural number. Each \
   byte has a running unary size bit: the most significant bit of each byte \
   indicates whether this is the last byte in the sequence (0) or whether the \
   sequence continues (1). Size bits ignored, the data is the binary \
   representation of the number in little-endian order."

let n_encoding =
  Binary_schema.Obj {fields = [Named_field ("N.t", `Dynamic, Bytes)]}

let add_n_reference uf {descriptions} =
  UF.add
    uf
    {title = n_reference_name; description = Some n_reference_description} ;
  {descriptions = (n_reference_name, n_encoding) :: descriptions}

let dedup_canonicalize uf l =
  let module Tbl = Hashtbl.Make (struct
    type t = Binary_schema.toplevel_encoding

    let hash = Binary_schema.hash_toplevel_encoding

    let equal a b = a = b
  end) in
  let tbl : Binary_schema.description Tbl.t = Tbl.create 100 in
  let rec run l =
    Tbl.clear tbl ;
    let new_union = ref false in
    let fixedup =
      l
      |> List.filter_map (fun (name, layout) ->
             match Tbl.find_opt tbl layout with
             | None ->
                 let desc = UF.find uf name in
                 Tbl.add tbl layout desc ;
                 Some (desc.title, layout)
             | Some original_desc ->
                 new_union := true ;
                 UF.union uf ~new_canonical:original_desc ~existing:name ;
                 None)
      |> List.map (fun (desc, layout) -> (desc, fixup_references uf layout))
    in
    if !new_union then run fixedup
    else List.map (fun (name, layout) -> (UF.find uf name, layout)) fixedup
  in
  run l

type pdesc = P : 'x Encoding.desc -> pdesc

let describe (type x) (encoding : x Encoding.t) =
  let open Encoding in
  let uf = UF.empty () in
  let uf_add_name title = UF.add uf {title; description = None} in
  let add_reference name description {descriptions} =
    {descriptions = (name, description) :: descriptions}
  in
  let new_reference =
    let x = ref ~-1 in
    fun () ->
      x := !x + 1 ;
      let name = "X_" ^ string_of_int !x in
      uf_add_name name ;
      name
  in
  let may_new_reference = function
    | None -> new_reference ()
    | Some name ->
        uf_add_name name ;
        name
  in
  let rec extract_dynamic : type x.
      string option ->
      x Encoding.desc ->
      Binary_size.length option * string option * pdesc =
   fun ref_name -> function
     | Conv {encoding; _} -> extract_dynamic ref_name encoding.encoding
     | Describe {id = ref_name; encoding; _} ->
         extract_dynamic (Some ref_name) encoding.encoding
     | Splitted {encoding; _} -> extract_dynamic ref_name encoding.encoding
     | Delayed f -> extract_dynamic ref_name (f ()).encoding
     | Dynamic_size {kind; encoding} ->
         (Some kind, ref_name, P encoding.encoding)
     | enc -> (None, ref_name, P enc)
  in
  let rec field_descr : type a.
      recursives ->
      references ->
      a Encoding.field ->
      Binary_schema.field_descr list * references =
   fun recursives references -> function
     | Req {name; encoding = {encoding; _}; _}
     | Dft {name; encoding = {encoding; _}; _} -> (
         let dynamics, ref_name, P field = extract_dynamic None encoding in
         let layout, references = layout ref_name recursives references field in
         if layout = Zero_width then ([], references)
         else
           let field_descr =
             Binary_schema.Named_field (name, classify_desc field, layout)
           in
           match dynamics with
           | Some kind ->
               ( [Dynamic_size_field (ref_name, 1, kind); field_descr],
                 references )
           | None -> ([field_descr], references))
     | Opt {kind = `Variable; name; encoding = {encoding; _}; _} ->
         let layout, references = layout None recursives references encoding in
         ([Named_field (name, `Variable, layout)], references)
     | Opt {kind = `Dynamic; name; encoding = {encoding; _}; _} ->
         let layout, references = layout None recursives references encoding in
         ( [
             Binary_schema.Optional_field name;
             Named_field (name, classify_desc encoding, layout);
           ],
           references )
  and obj fields = Binary_schema.Obj {fields}
  and union : type a.
      string option ->
      recursives ->
      references ->
      Kind.t ->
      Binary_size.tag_size ->
      a case list ->
      string * references =
   fun ref_name recursives references kind size cases ->
    let cases =
      List.sort (fun (t1, _) (t2, _) -> (compare : int -> int -> int) t1 t2)
      @@ List.fold_left
           (fun acc (Case {tag; _} as case) ->
             if Uint_option.is_some tag then (Uint_option.get tag, case) :: acc
             else acc)
           []
           cases
    in
    let tag_field =
      Binary_schema.Named_field
        ( "Tag",
          `Fixed (Binary_size.tag_size size),
          Int
            ( (size :> Binary_schema.integer_extended),
              TzEndian.default_endianness ) )
    in
    let cases, references =
      List.fold_right
        (fun (tag, Case case) (cases, references) ->
          let fields, references =
            fields None recursives references case.encoding.encoding
          in
          ((tag, Some case.title, tag_field :: fields) :: cases, references))
        cases
        ([], references)
    in
    let name = may_new_reference ref_name in
    let references =
      add_reference name (Cases {kind; tag_size = size; cases}) references
    in
    (name, references)
  and describe : type b.
      ?description:string ->
      title:string ->
      string ->
      recursives ->
      references ->
      b desc ->
      string * references =
   fun ?description ~title name recursives references encoding ->
    let new_canonical = {Binary_schema.title; description} in
    let layout, references = layout None recursives references encoding in
    match layout with
    | Ref ref_name ->
        UF.union uf ~existing:ref_name ~new_canonical ;
        (ref_name, references)
    | Zero_width -> (name, references)
    | layout ->
        UF.add uf new_canonical ;
        ( name,
          add_reference
            name
            (obj [Anonymous_field (classify_desc encoding, layout)])
            references )
  and enum : type a. (a, _) Hashtbl.t -> a array -> _ =
   fun tbl encoding_array ->
    ( Binary_size.range_to_size ~minimum:0 ~maximum:(Array.length encoding_array),
      List.map
        (fun i -> (i, fst @@ Hashtbl.find tbl encoding_array.(i)))
        (List.init (Array.length encoding_array) (fun i -> i)) )
  and fields : type b.
      string option ->
      recursives ->
      references ->
      b Encoding.desc ->
      Binary_schema.fields * references =
   fun ref_name recursives references -> function
     | Obj field -> field_descr recursives references field
     | Objs {left; right; _} ->
         let left_fields, references =
           fields None recursives references left.encoding
         in
         let right_fields, references =
           fields None recursives references right.encoding
         in
         (left_fields @ right_fields, references)
     | Null -> ([], references)
     | Empty -> ([], references)
     | Ignore -> ([], references)
     | Constant _ -> ([], references)
     | Dynamic_size {kind; encoding} ->
         let fields, refs =
           fields None recursives references encoding.encoding
         in
         (Dynamic_size_field (None, List.length fields, kind) :: fields, refs)
     | Check_size {encoding; _} ->
         fields ref_name recursives references encoding.encoding
     | Conv {encoding; _} ->
         fields ref_name recursives references encoding.encoding
     | Describe {id = name; encoding; _} ->
         fields (Some name) recursives references encoding.encoding
     | Splitted {encoding; _} ->
         fields ref_name recursives references encoding.encoding
     | Delayed func -> fields ref_name recursives references (func ()).encoding
     | List {length_limit = len; length_encoding = None; elts = {encoding; _}}
       ->
         let layout, references = layout None recursives references encoding in
         ([Anonymous_field (`Variable, Seq (layout, len))], references)
     | List
         {length_limit = len; length_encoding = Some le; elts = {encoding; _}}
       ->
         let layout_length, references =
           layout None recursives references le.encoding
         in
         let layout_elements, references =
           layout None recursives references encoding
         in
         ( [
             Anonymous_field (classify_desc le.encoding, layout_length);
             Anonymous_field (`Variable, Seq (layout_elements, len));
           ],
           references )
     | Array {length_limit = len; length_encoding = None; elts = {encoding; _}}
       ->
         let layout, references = layout None recursives references encoding in
         ([Anonymous_field (`Variable, Seq (layout, len))], references)
     | Array
         {length_limit = len; length_encoding = Some le; elts = {encoding; _}}
       ->
         let layout_length, references =
           layout None recursives references le.encoding
         in
         let layout_elements, references =
           layout None recursives references encoding
         in
         ( [
             Anonymous_field (classify_desc le.encoding, layout_length);
             Anonymous_field (`Variable, Seq (layout_elements, len));
           ],
           references )
     | Bytes (kind, _) ->
         ([Anonymous_field ((kind :> Kind.t), Bytes)], references)
     | String (kind, _) ->
         ([Anonymous_field ((kind :> Kind.t), String)], references)
     | Bigstring (kind, _) ->
         ([Anonymous_field ((kind :> Kind.t), String)], references)
     | Padded ({encoding = e; _}, n) ->
         let fields, references = fields ref_name recursives references e in
         (fields @ [Named_field ("padding", `Fixed n, Padding)], references)
     | String_enum (tbl, encoding_array) as encoding ->
         let size, cases = enum tbl encoding_array in
         let name = may_new_reference ref_name in
         ( [Anonymous_field (classify_desc encoding, Ref name)],
           add_reference name (Int_enum {size; cases}) references )
     | Tup {encoding; _} ->
         let layout, references =
           layout ref_name recursives references encoding
         in
         if layout = Zero_width then ([], references)
         else ([Anonymous_field (classify_desc encoding, layout)], references)
     | Tups {left; right; _} ->
         let fields1, references =
           fields None recursives references left.encoding
         in
         let fields2, references =
           fields None recursives references right.encoding
         in
         (fields1 @ fields2, references)
     | Union {kind; tag_size; cases; _} ->
         let name, references =
           union None recursives references kind tag_size cases
         in
         ([Anonymous_field (kind, Ref name)], references)
     | Mu {kind; name; title; description; fix} as encoding ->
         let kind = (kind :> Kind.t) in
         let title = match title with Some title -> title | None -> name in
         if List.mem name recursives then
           ([Anonymous_field (kind, Ref name)], references)
         else
           let {encoding; _} = fix {encoding; json_encoding = None} in
           let name, references =
             describe
               ~title
               ?description
               name
               (name :: recursives)
               references
               encoding
           in
           ([Anonymous_field (kind, Ref name)], references)
     | Bool as encoding ->
         let layout, references = layout None recursives references encoding in
         ([Anonymous_field (classify_desc encoding, layout)], references)
     | Int8 as encoding ->
         let layout, references = layout None recursives references encoding in
         ([Anonymous_field (classify_desc encoding, layout)], references)
     | Uint8 as encoding ->
         let layout, references = layout None recursives references encoding in
         ([Anonymous_field (classify_desc encoding, layout)], references)
     | Int16 _ as encoding ->
         let layout, references = layout None recursives references encoding in
         ([Anonymous_field (classify_desc encoding, layout)], references)
     | Uint16 _ as encoding ->
         let layout, references = layout None recursives references encoding in
         ([Anonymous_field (classify_desc encoding, layout)], references)
     | Int31 _ as encoding ->
         let layout, references = layout None recursives references encoding in
         ([Anonymous_field (classify_desc encoding, layout)], references)
     | Int32 _ as encoding ->
         let layout, references = layout None recursives references encoding in
         ([Anonymous_field (classify_desc encoding, layout)], references)
     | Int64 _ as encoding ->
         let layout, references = layout None recursives references encoding in
         ([Anonymous_field (classify_desc encoding, layout)], references)
     | N as encoding ->
         let layout, references = layout None recursives references encoding in
         ([Anonymous_field (classify_desc encoding, layout)], references)
     | Z as encoding ->
         let layout, references = layout None recursives references encoding in
         ([Anonymous_field (classify_desc encoding, layout)], references)
     | RangedInt _ as encoding ->
         let layout, references = layout None recursives references encoding in
         ([Anonymous_field (classify_desc encoding, layout)], references)
     | RangedFloat _ as encoding ->
         let layout, references = layout None recursives references encoding in
         ([Anonymous_field (classify_desc encoding, layout)], references)
     | Float as encoding ->
         let layout, references = layout None recursives references encoding in
         ([Anonymous_field (classify_desc encoding, layout)], references)
  and layout : type c.
      string option ->
      recursives ->
      references ->
      c Encoding.desc ->
      Binary_schema.layout * references =
   fun ref_name recursives references -> function
     | Null -> (Zero_width, references)
     | Empty -> (Zero_width, references)
     | Ignore -> (Zero_width, references)
     | Constant _ -> (Zero_width, references)
     | Bool -> (Bool, references)
     | Int8 -> (Int (`Int8, TzEndian.default_endianness), references)
     | Uint8 -> (Int (`Uint8, TzEndian.default_endianness), references)
     | Int16 endianness -> (Int (`Int16, endianness), references)
     | Uint16 endianness -> (Int (`Uint16, endianness), references)
     | Int31 endianness ->
         (RangedInt (~-1073741824, endianness, 1073741823), references)
     | Int32 endianness -> (Int (`Int32, endianness), references)
     | Int64 endianness -> (Int (`Int64, endianness), references)
     | N -> (Ref n_reference_name, add_n_reference uf references)
     | Z -> (Ref z_reference_name, add_z_reference uf references)
     | RangedInt {minimum; endianness; maximum} ->
         (RangedInt (minimum, endianness, maximum), references)
     | RangedFloat {minimum; maximum} ->
         (RangedFloat (minimum, maximum), references)
     | Float -> (Float, references)
     | Bytes (_kind, _) -> (Bytes, references)
     | String (_kind, _) -> (String, references)
     | Bigstring (_kind, _) -> (String, references)
     | Padded _ as enc ->
         let name = may_new_reference ref_name in
         let fields, references = fields None recursives references enc in
         let references = add_reference name (obj fields) references in
         (Ref name, references)
     | String_enum (tbl, encoding_array) ->
         let name = may_new_reference ref_name in
         let size, cases = enum tbl encoding_array in
         let references =
           add_reference name (Int_enum {size; cases}) references
         in
         (Enum (size, name), references)
     | Array {length_limit = _; length_encoding = Some _; elts = _} as encoding
       ->
         let name = may_new_reference ref_name in
         let fields, references = fields None recursives references encoding in
         UF.add uf {title = name; description = None} ;
         (Ref name, add_reference name (obj fields) references)
     | Array {length_limit = len; length_encoding = None; elts = data} ->
         let descr, references =
           layout None recursives references data.encoding
         in
         (Seq (descr, len), references)
     | List {length_limit = _; length_encoding = Some _; elts = _} as encoding
       ->
         let name = may_new_reference ref_name in
         let fields, references = fields None recursives references encoding in
         UF.add uf {title = name; description = None} ;
         (Ref name, add_reference name (obj fields) references)
     | List {length_limit = len; length_encoding = None; elts = data} ->
         let layout, references =
           layout None recursives references data.encoding
         in
         (Seq (layout, len), references)
     | Obj (Req {encoding = {encoding; _}; _})
     | Obj (Dft {encoding = {encoding; _}; _}) ->
         layout ref_name recursives references encoding
     | Obj (Opt _) as enc ->
         let name = may_new_reference ref_name in
         let fields, references = fields None recursives references enc in
         let references = add_reference name (obj fields) references in
         (Ref name, references)
     | Objs _ as descr ->
         let name = may_new_reference ref_name in
         let fields, references = fields None recursives references descr in
         if fields = [] then (Zero_width, references)
         else
           let references = add_reference name (obj fields) references in
           (Ref name, references)
     | Tup {encoding; _} -> layout ref_name recursives references encoding
     | Tups _ as descr ->
         let name = may_new_reference ref_name in
         let fields, references = fields None recursives references descr in
         if fields = [] then (Zero_width, references)
         else
           let references = add_reference name (obj fields) references in
           (Ref name, references)
     | Union {kind; tag_size; cases; _} ->
         let name, references =
           union ref_name recursives references kind tag_size cases
         in
         (Ref name, references)
     | Mu {name; title; description; fix; _} as encoding ->
         let title = match title with Some title -> title | None -> name in
         if List.mem name recursives then (Ref name, references)
         else
           let {encoding; _} = fix {encoding; json_encoding = None} in
           let name, references =
             describe
               name
               ~title
               ?description
               (name :: recursives)
               references
               encoding
           in
           (Ref name, references)
     | Conv {encoding; _} ->
         layout ref_name recursives references encoding.encoding
     | Describe {id = name; encoding; _} ->
         layout (Some name) recursives references encoding.encoding
     | Splitted {encoding; _} ->
         layout ref_name recursives references encoding.encoding
     | Dynamic_size e as encoding ->
         let name = may_new_reference ref_name in
         let fields, references = fields None recursives references encoding in
         UF.add uf {title = name; description = None} ;
         let references =
           match e.kind with
           | `N -> add_n_reference uf references
           | _ -> references
         in
         (Ref name, add_reference name (obj fields) references)
     | Check_size {encoding; _} ->
         layout ref_name recursives references encoding.encoding
     | Delayed func -> layout ref_name recursives references (func ()).encoding
  in
  let fields, references =
    fields None [] {descriptions = []} encoding.encoding
  in
  uf_add_name "" ;
  let _, toplevel = List.hd (dedup_canonicalize uf [("", obj fields)]) in
  let filtered =
    List.filter
      (fun (name, encoding) ->
        match encoding with
        | Binary_schema.Obj {fields = [Anonymous_field (_, Ref reference)]} ->
            UF.union uf ~new_canonical:(UF.find uf name) ~existing:reference ;
            false
        | Obj _ | Cases _ | Int_enum _ -> true)
      (List.rev references.descriptions)
  in
  let fields = dedup_canonicalize uf filtered in
  let toplevel = fixup_references uf toplevel in
  {Binary_schema.toplevel; fields}
