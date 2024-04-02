(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright 2014 OCamlPro                                                   *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* TODO: validator *)

(* The currently handled version *)
let version = "http://json-schema.org/draft-04/schema#"

(*-- types -----------------------------------------------------------------*)

module Def_map = Map.Make (struct
  type t = Json_query.path

  let compare = compare
end)

module Id_map = Map.Make (String)
module Id_set = Set.Make (String)

(* The root of a schema with the named definitions,
   a precomputed ID-element map and a cache for external documents. *)
type schema = {
  root : element;
  source : Uri.t;
  (* whose fragment should be empty *)
  definitions : element Def_map.t;
  ids : element Id_map.t;
  world : schema list;
}

and element = {
  title : string option;
  description : string option;
  default : Json_repr.any option;
  enum : Json_repr.any list option;
  kind : element_kind;
  format : string option;
  id : string option;
}

and element_kind =
  | Object of object_specs
  | Array of element list * array_specs
  | Monomorphic_array of element * array_specs
  | Combine of combinator * element list
  | Def_ref of Json_query.path
  | Id_ref of string
  | Ext_ref of Uri.t
  | String of string_specs
  | Integer of numeric_specs
  | Number of numeric_specs
  | Boolean
  | Null
  | Any
  | Dummy

and combinator = Any_of | One_of | All_of | Not

and array_specs = {
  min_items : int;
  max_items : int option;
  unique_items : bool;
  additional_items : element option;
}

and numeric_specs = {
  multiple_of : float option;
  minimum : (float * [`Inclusive | `Exclusive]) option;
  maximum : (float * [`Inclusive | `Exclusive]) option;
}

and object_specs = {
  properties : (string * element * bool * Json_repr.any option) list;
  pattern_properties : (string * element) list;
  additional_properties : element option;
  min_properties : int;
  max_properties : int option;
  schema_dependencies : (string * element) list;
  property_dependencies : (string * string list) list;
}

and string_specs = {
  pattern : string option;
  min_length : int;
  max_length : int option;
  str_format : string option;
}

let sort_assoc_list_by_string_key kvs =
  List.sort (fun (a, _) (b, _) -> String.compare a b) kvs

(* box an element kind without any optional field *)
let element kind =
  {
    title = None;
    description = None;
    default = None;
    kind;
    format = None;
    enum = None;
    id = None;
  }

(*-- equality --------------------------------------------------------------*)

let rec eq_element a b =
  a == b
  || a.title = b.title
     && a.description = b.description
     && Option.map Json_repr.from_any a.default
        = Option.map Json_repr.from_any b.default
     && Option.map (List.map Json_repr.from_any) a.enum
        = Option.map (List.map Json_repr.from_any) b.enum
     && eq_kind a.kind b.kind && a.format = b.format && a.id = b.id

and eq_kind a b =
  match (a, b) with
  | Object aa, Object ab -> eq_object_specs aa ab
  | Array (esa, sa), Array (esb, sb) ->
      List.compare_lengths esa esb = 0
      && List.for_all2 eq_element esa esb
      && eq_array_specs sa sb
  | Monomorphic_array (ea, sa), Monomorphic_array (eb, sb) ->
      eq_element ea eb && eq_array_specs sa sb
  | Combine (ca, esa), Combine (cb, esb) ->
      ca = cb
      && List.compare_lengths esa esb = 0
      && List.for_all2 eq_element esa esb
  | Def_ref pa, Def_ref pb -> pa = pb
  | Id_ref ra, Id_ref rb -> ra = rb
  | Ext_ref ra, Ext_ref rb -> ra = rb
  | String sa, String sb -> sa = sb
  | Integer na, Integer nb -> na = nb
  | Number na, Number nb -> na = nb
  | Boolean, Boolean -> true
  | Null, Null -> true
  | Any, Any -> true
  | Dummy, Dummy -> true
  | _ -> false

and eq_object_specs a b =
  Int.equal a.min_properties b.min_properties
  && Option.equal Int.equal a.max_properties b.max_properties
  && List.compare_lengths a.property_dependencies b.property_dependencies = 0
  && List.sort compare a.property_dependencies
     = List.sort compare b.property_dependencies
  && Option.equal eq_element a.additional_properties b.additional_properties
  && List.compare_lengths a.pattern_properties b.pattern_properties = 0
  && List.for_all2
       (fun (na, ea) (nb, eb) -> String.equal na nb && eq_element ea eb)
       (sort_assoc_list_by_string_key a.pattern_properties)
       (sort_assoc_list_by_string_key b.pattern_properties)
  && List.compare_lengths a.schema_dependencies b.schema_dependencies = 0
  && List.for_all2
       (fun (na, ea) (nb, eb) -> String.equal na nb && eq_element ea eb)
       (sort_assoc_list_by_string_key a.schema_dependencies)
       (sort_assoc_list_by_string_key b.schema_dependencies)
  && List.compare_lengths a.properties b.properties = 0
  && List.for_all2
       (fun (na, ea, ra, da) (nb, eb, rb, db) ->
         String.equal na nb && eq_element ea eb && ra = rb
         && Option.map Json_repr.from_any da = Option.map Json_repr.from_any db)
       (List.sort
          (fun (x, _, _, _) (y, _, _, _) -> String.compare x y)
          a.properties)
       (List.sort
          (fun (x, _, _, _) (y, _, _, _) -> String.compare x y)
          b.properties)

and eq_array_specs a b =
  a.min_items = b.min_items && a.max_items = b.max_items
  && a.unique_items = b.unique_items
  &&
  match (a.additional_items, b.additional_items) with
  | Some a, Some b -> eq_element a b
  | None, None -> true
  | _, _ -> false

(*-- human readable output -------------------------------------------------*)

let pp_string ppf s = Json_repr.(pp (module Ezjsonm)) ppf (`String s)

let pp_num ppf num =
  if abs_float num < 1000. then Format.fprintf ppf "%g" num
  else
    let is_positive, abs_num =
      if num < 0. then (false, ~-.num) else (true, num)
    in
    let already_printed =
      List.fold_left
        (fun already_printed delta ->
          already_printed
          ||
          let candidate = log (abs_num +. delta) /. log 2. in
          if abs_float (ceil candidate -. candidate) < 0.00001 then (
            Format.fprintf
              ppf
              "%s2^%g"
              (if is_positive then "" else "-")
              candidate ;
            if (is_positive && delta < 0.) || ((not is_positive) && delta > 0.)
            then Format.fprintf ppf "+%g" (abs_float delta) ;
            if (is_positive && delta > 0.) || ((not is_positive) && delta < 0.)
            then Format.fprintf ppf "-%g" (abs_float delta) ;
            true)
          else false)
        false
        [0.; 1.; -1.; 2.; -2.]
    in
    if not already_printed then Format.fprintf ppf "%f" abs_num

let pp_numeric_specs ppf {multiple_of; minimum; maximum} =
  Format.fprintf
    ppf
    "%a%a%a"
    (fun ppf -> function
      | None -> ()
      | Some v -> Format.fprintf ppf "multiple of %g" v)
    multiple_of
    (fun ppf -> function
      | None, _, _ | _, None, None -> ()
      | _ -> Format.fprintf ppf ", ")
    (multiple_of, minimum, maximum)
    (fun ppf -> function
      | None, None -> ()
      | minimum, maximum ->
          Format.fprintf
            ppf
            "∈ %a, %a"
            (fun ppf -> function
              | None -> Format.fprintf ppf "]∞"
              | Some (m, `Exclusive) -> Format.fprintf ppf "]%a" pp_num m
              | Some (m, `Inclusive) -> Format.fprintf ppf "[%a" pp_num m)
            minimum
            (fun ppf -> function
              | None -> Format.fprintf ppf "∞["
              | Some (m, `Exclusive) -> Format.fprintf ppf "%a[" pp_num m
              | Some (m, `Inclusive) -> Format.fprintf ppf "%a]" pp_num m)
            maximum)
    (minimum, maximum)

let pp_path ppf = function
  | [`Field "definitions"; `Field name] -> Format.fprintf ppf "%s" name
  | path -> Json_query.(print_path_as_json_path ~wildcards:true) ppf path

let pp_desc element =
  match element with
  | {title = None; description = None; _} -> None
  | {title = Some text; description = None; _}
  | {title = None; description = Some text; _} ->
      Some
        (fun ppf () ->
          Format.fprintf ppf "/* @[<hov 0>%a@] */" Format.pp_print_text text)
  | {title = Some title; description = Some description; _} ->
      Some
        (fun ppf () ->
          Format.fprintf
            ppf
            "/* @[<v 0>@[<hov 0>%a@]@,@[<hov 0>%a@]@] */"
            Format.pp_print_text
            title
            Format.pp_print_text
            description)

let rec pp_element ppf element =
  match element.id with
  | Some id -> Format.fprintf ppf "#%s" id
  | None -> (
      match element.format with
      | Some format -> Format.fprintf ppf "%s" format
      | None -> (
          match element.enum with
          | Some cases ->
              let pp_sep ppf () = Format.fprintf ppf "@ | " in
              Format.fprintf
                ppf
                "@[<hv 0>%a@]"
                (Format.pp_print_list
                   ~pp_sep
                   (Json_repr.pp_any ~compact:false ()))
                cases
          | None -> (
              match pp_desc element with
              | Some pp_desc -> (
                  let stripped =
                    {element with title = None; description = None}
                  in
                  match element.kind with
                  | Combine _ ->
                      Format.fprintf ppf "%a@,%a" pp_desc () pp_element stripped
                  | Object specs ->
                      Format.fprintf
                        ppf
                        "@[<v 2>{ %a@,%a }@]"
                        pp_desc
                        ()
                        pp_object_contents
                        specs
                  | _ ->
                      Format.fprintf ppf "%a@ %a" pp_element stripped pp_desc ()
                  )
              | None -> (
                  match element.kind with
                  | String {pattern; min_length; max_length; str_format} -> (
                      let length_pp ppf = function
                        | n, None when n > 0 ->
                            Format.fprintf ppf " (%d <= length)" n
                        | _, None -> Format.fprintf ppf ""
                        | n, Some m when n > 0 ->
                            Format.fprintf ppf " (%d <= length <= %d)" n m
                        | _, Some n -> Format.fprintf ppf " (length <= %d)" n
                      in
                      match (pattern, str_format) with
                      | None, None ->
                          Format.fprintf
                            ppf
                            "string%a"
                            length_pp
                            (min_length, max_length)
                      | Some pat, None ->
                          Format.fprintf
                            ppf
                            "/%s/%a"
                            pat
                            length_pp
                            (min_length, max_length)
                      | None, Some fmt ->
                          Format.fprintf
                            ppf
                            "%s%a"
                            fmt
                            length_pp
                            (min_length, max_length)
                      | Some pat, Some fmt ->
                          Format.fprintf
                            ppf
                            "%s (/%s/)%a"
                            fmt
                            pat
                            length_pp
                            (min_length, max_length))
                  | Integer {multiple_of = None; minimum = None; maximum = None}
                    ->
                      Format.fprintf ppf "integer"
                  | Integer specs ->
                      Format.fprintf ppf "integer %a" pp_numeric_specs specs
                  | Number {multiple_of = None; minimum = None; maximum = None}
                    ->
                      Format.fprintf ppf "number"
                  | Number specs ->
                      Format.fprintf ppf "number %a" pp_numeric_specs specs
                  | Id_ref id -> Format.fprintf ppf "#%s" id
                  | Def_ref path -> Format.fprintf ppf "$%a" pp_path path
                  | Ext_ref uri -> Format.fprintf ppf "$%a" Uri.pp_hum uri
                  | Boolean -> Format.fprintf ppf "boolean"
                  | Null -> Format.fprintf ppf "null"
                  | Any -> Format.fprintf ppf "any"
                  | Dummy -> assert false
                  | Combine (Not, [elt]) ->
                      Format.fprintf ppf "! %a" pp_element elt
                  | Combine (c, elts) ->
                      let pp_sep ppf () =
                        match c with
                        | Any_of -> Format.fprintf ppf "@ | "
                        | One_of -> Format.fprintf ppf "@ || "
                        | All_of -> Format.fprintf ppf "@ && "
                        | _ -> assert false
                      in
                      Format.fprintf
                        ppf
                        "@[<hv 0>%a@]"
                        (Format.pp_print_list ~pp_sep pp_element)
                        elts
                  | Object
                      {
                        properties = [];
                        pattern_properties = [];
                        additional_properties = None;
                        min_properties = 0;
                        max_properties = Some 0;
                        schema_dependencies = [];
                        property_dependencies = [];
                      } ->
                      Format.fprintf ppf "{}"
                  | Object specs ->
                      Format.fprintf
                        ppf
                        "@[<v 2>{ %a }@]"
                        pp_object_contents
                        specs
                  | Array (_, {max_items = Some 0; _})
                  | Monomorphic_array (_, {max_items = Some 0; _}) ->
                      Format.fprintf ppf "[]"
                  | Array (elements, {additional_items; _}) ->
                      let pp_sep =
                        let first = ref true in
                        fun ppf () ->
                          if !first then first := false
                          else Format.fprintf ppf ",@ "
                      in
                      Format.fprintf ppf "@[<hv 2>[ " ;
                      List.iter
                        (fun elt ->
                          Format.fprintf ppf "%a%a" pp_sep () pp_element elt)
                        elements ;
                      (match additional_items with
                      | None -> ()
                      | Some {kind = Any; _} ->
                          Format.fprintf ppf "%a,@ ..." pp_sep ()
                      | Some elt ->
                          Format.fprintf
                            ppf
                            "%a,@ %a ..."
                            pp_sep
                            ()
                            pp_element
                            elt) ;
                      Format.fprintf ppf " ]@]"
                  | Monomorphic_array (elt, {additional_items = None; _}) ->
                      Format.fprintf ppf "[ %a ... ]" pp_element elt
                  | Monomorphic_array
                      (elt, {additional_items = Some {kind = Any; _}; _}) ->
                      Format.fprintf
                        ppf
                        "@[<hv 2>[ %a ...,@ ... ]@]"
                        pp_element
                        elt
                  | Monomorphic_array (elt, {additional_items = Some add_elt; _})
                    ->
                      (* TODO: find a good way to print length *)
                      Format.fprintf
                        ppf
                        "@[<hv 2>[ %a ...,@ %a ... ]@]"
                        pp_element
                        elt
                        pp_element
                        add_elt))))

and pp_object_contents ppf
    {properties; pattern_properties; additional_properties; _} =
  (* TODO: find a good way to print length / dependencies *)
  let pp_sep =
    let first = ref true in
    fun ppf () -> if !first then first := false else Format.fprintf ppf ",@ "
  in
  List.iter
    (fun (name, elt, req, _) ->
      Format.fprintf
        ppf
        "%a@[<hv 2>%a%s:@ %a@]"
        pp_sep
        ()
        pp_string
        name
        (if req then "" else "?")
        pp_element
        elt)
    properties ;
  List.iter
    (fun (name, elt) ->
      Format.fprintf ppf "%a@[<hv 2>/%s/:@ %a@]" pp_sep () name pp_element elt)
    pattern_properties ;
  match additional_properties with
  | None -> ()
  | Some {kind = Any; _} -> Format.fprintf ppf "%a..." pp_sep ()
  | Some elt -> Format.fprintf ppf "%a@[<hv 2>*:@ %a@]" pp_sep () pp_element elt

let pp ppf schema =
  Format.fprintf ppf "@[<v 0>" ;
  pp_element ppf schema.root ;
  Def_map.iter
    (fun path elt ->
      match pp_desc elt with
      | None ->
          Format.fprintf ppf "@,@[<hv 2>$%a:@ %a@]" pp_path path pp_element elt
      | Some pp_desc ->
          let stripped = {elt with title = None; description = None} in
          Format.fprintf
            ppf
            "@,@[<v 2>$%a:@,%a@,%a@]"
            pp_path
            path
            pp_desc
            ()
            pp_element
            stripped)
    schema.definitions ;
  Id_map.iter
    (fun id elt ->
      match pp_desc elt with
      | None ->
          Format.fprintf
            ppf
            "@,@[<hv 2>#%s:@ %a@]"
            id
            pp_element
            {elt with id = None}
      | Some pp_desc ->
          let stripped =
            {elt with title = None; description = None; id = None}
          in
          Format.fprintf
            ppf
            "@,@[<v 2>#%s:@,%a@,%a@]"
            id
            pp_desc
            ()
            pp_element
            stripped)
    schema.ids ;
  Format.fprintf ppf "@]"

(*-- errors ----------------------------------------------------------------*)

exception Cannot_parse of Json_query.path * exn

exception Dangling_reference of Uri.t

exception Bad_reference of string

exception Unexpected of string * string

exception Duplicate_definition of Json_query.path * element * element

let rec print_error ?print_unknown ppf = function
  | Cannot_parse (path, exn) ->
      Format.fprintf
        ppf
        "@[<v 2>Schema parse error:@,At %a@,%a@]"
        (Json_query.print_path_as_json_path ~wildcards:true)
        path
        (print_error ?print_unknown)
        exn
  | Dangling_reference uri ->
      Format.fprintf ppf "Dangling reference %s" (Uri.to_string uri)
  | Bad_reference str -> Format.fprintf ppf "Illegal reference notation %s" str
  | Unexpected (unex, ex) ->
      Format.fprintf ppf "Unexpected %s instead of %s" unex ex
  | Duplicate_definition (name, elt, defelt) ->
      Format.fprintf
        ppf
        "@[<v 2>Duplicate definition %a@,\
         To be inserted:@,\
        \  @[<v 0>%a@]@,\
         Already present:@,\
        \  @[<v 0>%a@]@]"
        (Json_query.print_path_as_json_pointer ~wildcards:false)
        name
        pp_element
        elt
        pp_element
        defelt
  | exn -> Json_query.print_error ?print_unknown ppf exn

(*-- internal definition table handling ------------------------------------*)

let find_definition name defs = Def_map.find name defs

let definition_exists name defs = Def_map.mem name defs

let insert_definition name elt (defs : element Def_map.t) : element Def_map.t =
  Def_map.update
    name
    (function
      | None -> Some elt
      | Some {kind = Dummy; _} -> Some elt
      | Some defelt ->
          if not (eq_element elt defelt) then
            raise (Duplicate_definition (name, elt, defelt)) ;
          Some elt)
    defs

module Make (Repr : Json_repr.Repr) = struct
  module Query = Json_query.Make (Repr)
  open Query

  (*-- printer ---------------------------------------------------------------*)

  let to_json schema =
    (* functional JSON building combinators *)
    let obj l = Repr.repr (`O l) in
    let set_always f v rest = (f, Repr.repr v) :: rest in
    let set_if_some f v cb rest =
      match v with None -> rest | Some v -> (f, Repr.repr (cb v)) :: rest
    in
    let set_if_cons f v cb rest =
      match v with [] -> rest | v -> (f, Repr.repr (cb v)) :: rest
    in
    let set_if_neq f v v' cb rest =
      if v <> v' then (f, Repr.repr (cb v)) :: rest else rest
    in
    let set_multiple xs rest = List.rev_append xs rest in
    (* recursive encoder *)
    let rec format_element {title; description; default; enum; kind; format; _}
        =
      set_if_some "title" title (fun s -> `String s)
      @@ set_if_some "description" description (fun s -> `String s)
      @@ (fun rest ->
           match kind with
           | Object specs ->
               let required =
                 List.fold_left
                   (fun r (n, _, p, _) ->
                     if p then Repr.repr (`String n) :: r else r)
                   []
                   specs.properties
               in
               let properties =
                 List.map
                   (fun (n, elt, _, _) -> (n, obj (format_element elt)))
                   specs.properties
               in
               set_always "type" (`String "object")
               @@ set_if_cons "properties" properties (fun l -> `O l)
               @@ set_if_cons "required" required (fun l -> `A l)
               @@ set_if_cons
                    "patternProperties"
                    specs.pattern_properties
                    (fun fs ->
                      `O
                        (List.map
                           (fun (n, elt) -> (n, obj (format_element elt)))
                           fs))
               @@ set_if_neq
                    "additionalProperties"
                    specs.additional_properties
                    (Some (element Any))
                    (function
                      | None -> `Bool false
                      | Some elt -> `O (format_element elt))
               @@ set_if_neq "minProperties" specs.min_properties 0 (fun i ->
                      `Float (float i))
               @@ set_if_some "maxProperties" specs.max_properties (fun i ->
                      `Float (float i))
               @@ set_if_cons
                    "schemaDependencies"
                    specs.schema_dependencies
                    (fun fs ->
                      `O
                        (List.map
                           (fun (n, elt) -> (n, obj (format_element elt)))
                           fs))
               @@ set_if_cons
                    "propertyDependencies"
                    specs.property_dependencies
                    (fun fs ->
                      let property_dependencies =
                        let strings ls =
                          List.map (fun s -> Repr.repr (`String s)) ls
                        in
                        List.map
                          (fun (n, ls) -> (n, Repr.repr (`A (strings ls))))
                          fs
                      in
                      `O property_dependencies)
               @@ rest
           | Array (elts, specs) ->
               set_always "type" (`String "array")
               @@ set_always
                    "items"
                    (`A (List.map (fun elt -> obj (format_element elt)) elts))
               @@ set_if_neq "minItems" specs.min_items 0 (fun i ->
                      `Float (float i))
               @@ set_if_some "maxItems" specs.max_items (fun i ->
                      `Float (float i))
               @@ set_if_neq "uniqueItems" specs.unique_items false (fun b ->
                      `Bool b)
               @@ set_if_neq
                    "additionalItems"
                    specs.additional_items
                    (Some (element Any))
                    (function
                      | None -> `Bool false
                      | Some elt -> `O (format_element elt))
               @@ rest
           | Monomorphic_array (elt, {min_items; max_items; unique_items; _}) ->
               set_always "type" (`String "array")
               @@ set_always "items" (`O (format_element elt))
               @@ set_if_neq "minItems" min_items 0 (fun i -> `Float (float i))
               @@ set_if_some "maxItems" max_items (fun i -> `Float (float i))
               @@ set_if_neq "uniqueItems" unique_items false (fun b -> `Bool b)
               @@ rest
           | Combine (c, elts) ->
               let combinator = function
                 | Any_of -> "anyOf"
                 | One_of -> "oneOf"
                 | All_of -> "allOf"
                 | Not -> "not"
               in
               set_always
                 (combinator c)
                 (`A (List.map (fun elt -> obj (format_element elt)) elts))
               @@ rest
           | Def_ref path ->
               set_always
                 "$ref"
                 (`String ("#" ^ Json_query.json_pointer_of_path path))
               @@ rest
           | Id_ref name -> set_always "$ref" (`String ("#" ^ name)) @@ rest
           | Ext_ref uri ->
               set_always "$ref" (`String (Uri.to_string uri)) @@ rest
           | Integer specs ->
               set_always "type" (`String "integer")
               @@ set_if_some "multipleOf" specs.multiple_of (fun v -> `Float v)
               @@ set_multiple
                    (match specs.minimum with
                    | None -> []
                    | Some (v, `Inclusive) ->
                        [("minimum", Repr.repr (`Float v))]
                    | Some (v, `Exclusive) ->
                        [
                          ("exclusiveMinimum", Repr.repr (`Bool true));
                          ("minimum", Repr.repr (`Float v));
                        ])
               @@ set_multiple
                    (match specs.maximum with
                    | None -> []
                    | Some (v, `Inclusive) ->
                        [("maximum", Repr.repr (`Float v))]
                    | Some (v, `Exclusive) ->
                        [
                          ("exclusiveMaximum", Repr.repr (`Bool true));
                          ("maximum", Repr.repr (`Float v));
                        ])
               @@ rest
           | Number specs ->
               set_always "type" (`String "number")
               @@ set_if_some "multipleOf" specs.multiple_of (fun v -> `Float v)
               @@ set_multiple
                    (match specs.minimum with
                    | None -> []
                    | Some (v, `Inclusive) ->
                        [("minimum", Repr.repr (`Float v))]
                    | Some (v, `Exclusive) ->
                        [
                          ("exclusiveMinimum", Repr.repr (`Bool true));
                          ("minimum", Repr.repr (`Float v));
                        ])
               @@ set_multiple
                    (match specs.maximum with
                    | None -> []
                    | Some (v, `Inclusive) ->
                        [("maximum", Repr.repr (`Float v))]
                    | Some (v, `Exclusive) ->
                        [
                          ("exclusiveMaximum", Repr.repr (`Bool true));
                          ("maximum", Repr.repr (`Float v));
                        ])
               @@ rest
           | String {pattern; min_length; max_length; str_format} ->
               set_always "type" (`String "string")
               @@ set_if_neq "minLength" min_length 0 (fun i ->
                      `Float (float i))
               @@ set_if_some "maxLength" max_length (fun i -> `Float (float i))
               @@ set_if_some "pattern" pattern (fun s -> `String s)
               @@ set_if_some "format" str_format (fun s -> `String s)
               @@ rest
           | Boolean -> set_always "type" (`String "boolean") @@ rest
           | Null -> set_always "type" (`String "null") @@ rest
           | Dummy -> invalid_arg "Json_schema.to_json: remaining dummy element"
           | Any -> set_multiple [] @@ rest)
      @@ set_if_some "default" default (fun j ->
             Repr.view (Json_repr.any_to_repr (module Repr) j))
      @@ set_if_some "enum" enum (fun js ->
             `A (List.map (Json_repr.any_to_repr (module Repr)) js))
      @@ set_if_some "format" format (fun s -> `String s)
      @@ []
    in
    Def_map.fold
      (fun n elt acc -> insert n (obj (format_element elt)) acc)
      schema.definitions
      (obj
         (set_always "$schema" (`String version) @@ format_element schema.root))

  let unexpected kind expected =
    let kind =
      match kind with
      | `O [] -> "empty object"
      | `A [] -> "empty array"
      | `O _ -> "object"
      | `A _ -> "array"
      | `Null -> "null"
      | `String "" -> "empty string"
      | `String _ -> "string"
      | `Float _ -> "number"
      | `Bool _ -> "boolean"
    in
    Cannot_parse ([], Unexpected (kind, expected))

  (*-- parser ----------------------------------------------------------------*)

  let at_path p = function
    | Cannot_parse (l, err) -> Cannot_parse (List.append p l, err)
    | exn -> exn

  let at_field n = at_path [`Field n]

  let at_index i = at_path [`Index i]

  let of_json ?(definitions_path = "/definitions/") json =
    (* parser combinators *)
    let opt_field obj n =
      match Repr.view obj with
      | `O ls -> ( try Some (List.assoc n ls) with Not_found -> None)
      | _ -> None
    in
    let opt_field_view obj n =
      match Repr.view obj with
      | `O ls -> (
          try Some (Repr.view (List.assoc n ls)) with Not_found -> None)
      | _ -> None
    in
    let opt_string_field obj n =
      match opt_field_view obj n with
      | Some (`String s) -> Some s
      | Some k -> raise (at_field n @@ unexpected k "string")
      | None -> None
    in
    let opt_bool_field def obj n =
      match opt_field_view obj n with
      | Some (`Bool b) -> b
      | Some k -> raise (at_field n @@ unexpected k "bool")
      | None -> def
    in
    let opt_int_field obj n =
      match opt_field_view obj n with
      | Some (`Float f)
        when fst (modf f) = 0. && f <= 2. ** 53. && f >= -2. ** 53. ->
          Some f
      | Some k -> raise (at_field n @@ unexpected k "integer")
      | None -> None
    in
    let opt_length_field obj n =
      match opt_field_view obj n with
      | Some (`Float f) when fst (modf f) = 0. && f <= 2. ** 30. && f >= 0. ->
          Some (int_of_float f)
      | Some k -> raise (at_field n @@ unexpected k "length")
      | None -> None
    in
    let opt_float_field obj n =
      match opt_field_view obj n with
      | Some (`Float f) -> Some f
      | Some k -> raise (at_field n @@ unexpected k "number")
      | None -> None
    in
    let opt_array_field obj n =
      match opt_field_view obj n with
      | Some (`A s) -> Some s
      | Some k -> raise (at_field n @@ unexpected k "array")
      | None -> None
    in
    let opt_uri_field obj n =
      match opt_string_field obj n with
      | None -> None
      | Some uri -> (
          match Uri.canonicalize (Uri.of_string uri) with
          | exception _ ->
              raise
                (Cannot_parse ([], Bad_reference (uri ^ " is not a valid URI")))
          | uri -> Some uri)
    in
    (* local resolution of definitions *)
    let schema_source =
      match opt_uri_field json "id" with
      | Some uri -> Uri.with_fragment uri None
      | None -> Uri.empty
    in
    let collected_definitions : element Def_map.t ref = ref Def_map.empty in
    let collected_id_defs = ref Id_map.empty in
    let collected_id_refs = ref Id_set.empty in
    let rec collect_definition : Uri.t -> element_kind =
     fun uri ->
      match (Uri.host uri, Uri.fragment uri) with
      | Some _ (* Actually means: any of host, user or port is defined. *), _ ->
          Ext_ref uri
      | None, None ->
          raise
            (Cannot_parse
               ([], Bad_reference (Uri.to_string uri ^ " has no fragment")))
      | None, Some fragment when not (String.contains fragment '/') ->
          collected_id_refs := Id_set.add fragment !collected_id_refs ;
          Id_ref fragment
      | None, Some fragment -> (
          let path =
            try Json_query.path_of_json_pointer ~wildcards:false fragment
            with err -> raise (Cannot_parse ([], err))
          in
          try
            let raw = query path json in
            if not (definition_exists path !collected_definitions) then (
              (* dummy insertion so we don't recurse and we support cycles *)
              collected_definitions :=
                insert_definition path (element Dummy) !collected_definitions ;
              let elt =
                try parse_element schema_source raw
                with err -> raise (at_path path err)
              in
              (* actual insertion *)
              collected_definitions :=
                insert_definition path elt !collected_definitions) ;
            Def_ref path
          with Not_found -> raise (Cannot_parse ([], Dangling_reference uri)))
    (* recursive parser *)
    and parse_element : Uri.t -> Repr.value -> element =
     fun source json ->
      let id = opt_uri_field json "id" in
      let id, source =
        match id with
        | None -> (None, source)
        | Some uri ->
            let uri = Uri.canonicalize (Uri.resolve "http" source uri) in
            (Uri.fragment uri, Uri.with_fragment uri None)
      in
      (* We don't support inlined schemas, so we just drop elements with
         external sources and replace them with external references. *)
      if source <> schema_source then
        element (Ext_ref (Uri.with_fragment source id))
      else
        let id =
          match id with
          | None -> None
          | Some id when String.contains id '/' ->
              raise
                (at_field "id"
                @@ Cannot_parse ([], Bad_reference (id ^ " is not a valid ID"))
                )
          | Some id -> Some id
        in
        (* We parse the various element syntaxes and combine them afterwards. *)
        (* 1. An element with a known type field and associated fields. *)
        let as_kind =
          match opt_field_view json "type" with
          | Some (`String name) ->
              Some (element (parse_element_kind source json name))
          | Some (`A [] as k) ->
              raise
                (at_field "type" @@ unexpected k "type, type array or operator")
          | Some (`A l) ->
              let rec items i acc = function
                | [] ->
                    let kind = Combine (Any_of, List.rev acc) in
                    Some (element kind)
                | `String name :: tl ->
                    let kind = parse_element_kind source json name in
                    let case = element kind in
                    items (succ i) (case :: acc) tl
                | k :: _ ->
                    raise (at_field "type" @@ at_index i @@ unexpected k "type")
              in
              items 0 [] (List.map Repr.view l)
          | Some k ->
              raise
                (at_field "type" @@ unexpected k "type, type array or operator")
          | None -> None
        in
        (* 2. A reference *)
        let as_ref =
          match opt_uri_field json "$ref" with
          | Some uri ->
              let path = collect_definition uri in
              Some (element path)
          | None -> None
        in
        (* 3. Combined schemas *)
        let as_nary name combinator others =
          let build = function
            | [] -> None (* not found and no auxiliary case *)
            | [case] -> Some case (* one case -> simplify *)
            | cases ->
                (* several cases build the combination node with empty options *)
                let kind = Combine (combinator, cases) in
                Some (element kind)
          in
          let items =
            match opt_field_view json name with
            | Some (`A (_ :: _ as cases)) (* list of schemas *) ->
                let rec items i acc = function
                  | elt :: tl ->
                      let elt =
                        try parse_element source elt
                        with err -> raise (at_field name @@ at_index i @@ err)
                      in
                      items (succ i) (elt :: acc) tl
                  | [] -> List.rev acc
                in
                items 0 [] cases
            | None -> []
            | Some k ->
                raise (at_field name @@ unexpected k "a list of elements")
          in
          build (List.rev_append others items)
        in
        (* 4. Negated schema *)
        let as_not =
          match opt_field_view json "not" with
          | None -> None
          | Some elt ->
              let elt =
                try parse_element source (Repr.repr elt)
                with err -> raise (at_field "not" err)
              in
              let kind = Combine (Not, [elt]) in
              Some (element kind)
        in
        (* parse optional fields *)
        let title = opt_string_field json "title" in
        let description = opt_string_field json "description" in
        let default =
          match opt_field json "default" with
          | Some v -> Some (Json_repr.repr_to_any (module Repr) v)
          | None -> None
        in
        let enum =
          Option.map
            (fun v -> List.map (Json_repr.repr_to_any (module Repr)) v)
            (opt_array_field json "enum")
        in
        let format = opt_string_field json "format" in
        (* TODO: check format ? *)
        (* combine all specifications under a big conjunction *)
        let as_one_of = as_nary "oneOf" One_of [] in
        let as_any_of = as_nary "anyOf" Any_of [] in
        let cases =
          let ( @? ) o xs = match o with None -> xs | Some x -> x :: xs in
          (* Note: building this reversed so we can use [rev_append] *)
          as_any_of @? as_one_of @? as_not @? as_ref @? as_kind @? []
        in
        let kind =
          match as_nary "allOf" All_of cases with
          | None -> Any (* no type, ref or logical combination found *)
          | Some {kind; _} -> kind
        in
        (* add optional fields *)
        {title; description; default; format; kind; enum; id}
    and parse_element_kind source json name =
      let integer_specs json =
        let multiple_of = opt_int_field json "multipleOf" in
        let minimum =
          if opt_bool_field false json "exclusiveMinimum" then
            match opt_int_field json "minimum" with
            | None ->
                let err =
                  "minimum field required when exclusiveMinimum is true"
                in
                raise (Failure err)
            | Some v -> Some (v, `Inclusive)
          else
            match opt_int_field json "minimum" with
            | None -> None
            | Some v -> Some (v, `Exclusive)
        in
        let maximum =
          if opt_bool_field false json "exclusiveMaximum" then
            match opt_int_field json "maximum" with
            | None ->
                let err =
                  "maximum field required when exclusiveMaximum is true"
                in
                raise (Failure err)
            | Some v -> Some (v, `Inclusive)
          else
            match opt_int_field json "maximum" with
            | None -> None
            | Some v -> Some (v, `Exclusive)
        in
        {multiple_of; minimum; maximum}
      in
      let numeric_specs json =
        let multiple_of = opt_float_field json "multipleOf" in
        let minimum =
          if opt_bool_field false json "exclusiveMinimum" then
            match opt_float_field json "minimum" with
            | None ->
                let err =
                  "minimum field required when exclusiveMinimum is true"
                in
                raise (Failure err)
            | Some v -> Some (v, `Inclusive)
          else
            match opt_float_field json "minimum" with
            | None -> None
            | Some v -> Some (v, `Exclusive)
        in
        let maximum =
          if opt_bool_field false json "exclusiveMaximum" then
            match opt_float_field json "maximum" with
            | None ->
                let err =
                  "maximum field required when exclusiveMaximum is true"
                in
                raise (Failure err)
            | Some v -> Some (v, `Inclusive)
          else
            match opt_float_field json "maximum" with
            | None -> None
            | Some v -> Some (v, `Exclusive)
        in
        {multiple_of; minimum; maximum}
      in
      match name with
      | "integer" -> Integer (integer_specs json)
      | "number" -> Number (numeric_specs json)
      | "boolean" -> Boolean
      | "null" -> Null
      | "string" ->
          let specs =
            let pattern = opt_string_field json "pattern" in
            let str_format = opt_string_field json "format" in
            let min_length = opt_length_field json "minLength" in
            let max_length = opt_length_field json "maxLength" in
            let min_length = match min_length with None -> 0 | Some l -> l in
            {pattern; min_length; max_length; str_format}
          in
          String specs
      | "array" -> (
          let specs =
            let unique_items = opt_bool_field false json "uniqueItems" in
            let min_items = opt_length_field json "minItems" in
            let max_items = opt_length_field json "maxItems" in
            let min_items = match min_items with None -> 0 | Some l -> l in
            match opt_field_view json "additionalItems" with
            | Some (`Bool true) ->
                {
                  min_items;
                  max_items;
                  unique_items;
                  additional_items = Some (element Any);
                }
            | None | Some (`Bool false) ->
                {min_items; max_items; unique_items; additional_items = None}
            | Some elt ->
                let elt =
                  try parse_element source (Repr.repr elt)
                  with err -> raise (at_field "additionalItems" err)
                in
                {
                  min_items;
                  max_items;
                  unique_items;
                  additional_items = Some elt;
                }
          in
          match opt_field_view json "items" with
          | Some (`A elts) ->
              let rec elements i acc = function
                | [] -> Array (List.rev acc, specs)
                | elt :: tl ->
                    let elt =
                      try parse_element source elt
                      with err -> raise (at_field "items" @@ at_index i err)
                    in
                    elements (succ i) (elt :: acc) tl
              in
              elements 0 [] elts
          | Some elt ->
              let elt =
                try parse_element source (Repr.repr elt)
                with err -> raise (at_field "items" err)
              in
              Monomorphic_array (elt, specs)
          | None -> Monomorphic_array (element Any, specs))
      | "object" ->
          let required =
            match opt_array_field json "required" with
            | None -> []
            | Some l ->
                let rec items i acc = function
                  | `String s :: tl -> items (succ i) (s :: acc) tl
                  | [] -> List.rev acc
                  | k :: _ ->
                      raise
                        (at_field "required" @@ at_index i
                       @@ unexpected k "string")
                in
                items 0 [] (List.map Repr.view l)
          in
          let properties =
            match opt_field_view json "properties" with
            | Some (`O props) ->
                let rec items acc = function
                  | [] -> List.rev acc
                  | (n, elt) :: tl ->
                      let elt =
                        try parse_element source elt
                        with err ->
                          raise (at_field "properties" @@ at_field n @@ err)
                      in
                      let req = List.mem n required in
                      items ((n, elt, req, None) :: acc) tl
                  (* XXX: fixme *)
                in
                items [] props
            | None -> []
            | Some k -> raise (at_field "properties" @@ unexpected k "object")
          in
          let additional_properties =
            match opt_field_view json "additionalProperties" with
            | Some (`Bool false) -> None
            | None | Some (`Bool true) -> Some (element Any)
            | Some elt ->
                let elt =
                  try parse_element source (Repr.repr elt)
                  with err -> raise (at_field "additionalProperties" err)
                in
                Some elt
          in
          let property_dependencies =
            match opt_field_view json "propertyDependencies" with
            | None -> []
            | Some (`O l) ->
                let rec sets sacc = function
                  | (n, `A l) :: tl ->
                      let rec strings j acc = function
                        | [] -> sets ((n, List.rev acc) :: sacc) tl
                        | `String s :: tl -> strings (succ j) (s :: acc) tl
                        | k :: _ ->
                            raise
                              (at_field "propertyDependencies"
                              @@ at_field n @@ at_index j
                              @@ unexpected k "string")
                      in
                      strings 0 [] (List.map Repr.view l)
                  | (n, k) :: _ ->
                      raise
                        (at_field "propertyDependencies"
                        @@ at_field n
                        @@ unexpected k "string array")
                  | [] -> List.rev sacc
                in
                sets [] (List.map (fun (n, v) -> (n, Repr.view v)) l)
            | Some k ->
                raise (at_field "propertyDependencies" @@ unexpected k "object")
          in
          let parse_element_assoc field =
            match opt_field_view json field with
            | None -> []
            | Some (`O props) ->
                let rec items acc = function
                  | [] -> List.rev acc
                  | (n, elt) :: tl ->
                      let elt =
                        try parse_element source elt
                        with err -> raise (at_field field @@ at_field n err)
                      in
                      items ((n, elt) :: acc) tl
                in
                items [] props
            | Some k -> raise (at_field field @@ unexpected k "object")
          in
          let pattern_properties = parse_element_assoc "patternProperties" in
          let schema_dependencies = parse_element_assoc "schemaDependencies" in
          let min_properties =
            match opt_length_field json "minProperties" with
            | None -> 0
            | Some l -> l
          in
          let max_properties = opt_length_field json "maxProperties" in
          Object
            {
              properties;
              pattern_properties;
              additional_properties;
              min_properties;
              max_properties;
              schema_dependencies;
              property_dependencies;
            }
      | n -> raise (Cannot_parse ([], Unexpected (n, "a known type")))
    in
    (* parse recursively from the root *)
    let root = parse_element Uri.empty json in
    (* force the addition of everything inside /definitions *)
    (match Repr.view (query [`Field "definitions"] json) with
    | `O all ->
        List.iter
          (fun (n, _) ->
            let uri = Uri.of_string ("#" ^ definitions_path ^ n) in
            ignore (collect_definition uri))
          all
    | _ -> ()
    | exception Not_found -> ()) ;
    (* check the domain of IDs *)
    Id_set.iter
      (fun id ->
        if not (Id_map.mem id !collected_id_defs) then
          raise
            (Cannot_parse
               ([], Dangling_reference Uri.(with_fragment empty (Some id)))))
      !collected_id_refs ;
    let ids = !collected_id_defs in
    let source = schema_source in
    let world = [] in
    let definitions = !collected_definitions in
    {root; definitions; source; ids; world}

  (*-- creation and update ---------------------------------------------------*)

  (* Checks that all local refs and ids are defined *)
  let check_definitions root definitions =
    let collected_id_defs = ref Id_map.empty in
    let collected_id_refs = ref Id_set.empty in
    let rec check ({kind; id; _} as elt) =
      (match id with
      | None -> ()
      | Some id -> collected_id_defs := Id_map.add id elt !collected_id_defs) ;
      match kind with
      | Object
          {
            properties;
            pattern_properties;
            additional_properties;
            schema_dependencies;
            _;
          } -> (
          List.iter (fun (_, e, _, _) -> check e) properties ;
          List.iter (fun (_, e) -> check e) pattern_properties ;
          List.iter (fun (_, e) -> check e) schema_dependencies ;
          match additional_properties with Some e -> check e | None -> ())
      | Array (es, {additional_items; _}) -> (
          List.iter check es ;
          match additional_items with Some e -> check e | None -> ())
      | Monomorphic_array (e, {additional_items; _}) -> (
          check e ;
          match additional_items with Some e -> check e | None -> ())
      | Combine (_, es) -> List.iter check es
      | Def_ref path ->
          if not (definition_exists path definitions) then
            let path = Json_query.json_pointer_of_path path in
            raise (Dangling_reference (Uri.(with_fragment empty) (Some path)))
      | Id_ref id -> collected_id_refs := Id_set.add id !collected_id_refs
      | Ext_ref _ | String _ | Integer _ | Number _ | Boolean | Null | Any
      | Dummy ->
          ()
    in
    (* check the root and definitions *)
    check root ;
    Def_map.iter (fun _ root -> check root) definitions ;
    (* check the domain of IDs *)
    Id_set.iter
      (fun id ->
        if not (Id_map.mem id !collected_id_defs) then
          raise (Dangling_reference Uri.(with_fragment empty (Some id))))
      !collected_id_refs ;
    !collected_id_defs

  let create root =
    let ids = check_definitions root Def_map.empty in
    {root; definitions = Def_map.empty; world = []; ids; source = Uri.empty}

  let root {root; _} = root

  let update root sch =
    let ids = check_definitions root sch.definitions in
    {sch with root; ids}

  let any = create (element Any)

  let self =
    {
      root = element (Ext_ref (Uri.of_string version));
      definitions = Def_map.empty;
      ids = Id_map.empty;
      world = [];
      source = Uri.empty;
    }

  (* remove unused definitions from the schema *)
  let simplify schema =
    let res = ref Def_map.empty (* collected definitions *) in
    let rec collect {kind; _} =
      match kind with
      | Object
          {
            properties;
            pattern_properties;
            additional_properties;
            schema_dependencies;
            _;
          } -> (
          List.iter (fun (_, e, _, _) -> collect e) properties ;
          List.iter (fun (_, e) -> collect e) pattern_properties ;
          List.iter (fun (_, e) -> collect e) schema_dependencies ;
          match additional_properties with Some e -> collect e | None -> ())
      | Array (es, {additional_items; _}) -> (
          List.iter collect es ;
          match additional_items with Some e -> collect e | None -> ())
      | Monomorphic_array (e, {additional_items; _}) -> (
          collect e ;
          match additional_items with Some e -> collect e | None -> ())
      | Combine (_, es) -> List.iter collect es
      | Def_ref path ->
          let def = find_definition path schema.definitions in
          res := insert_definition path def !res
      | Ext_ref _ | Id_ref _ | String _ | Integer _ | Number _ | Boolean | Null
      | Any | Dummy ->
          ()
    in
    collect schema.root ;
    {schema with definitions = !res}

  let definition_path_of_name ?(definitions_path = "/definitions/") name =
    Json_query.path_of_json_pointer ~wildcards:false
    @@
    match name.[0] with
    | exception _ -> raise (Bad_reference name)
    | '/' -> name
    | _ -> definitions_path ^ name

  let find_definition ?definitions_path name schema =
    let path = definition_path_of_name ?definitions_path name in
    find_definition path schema.definitions

  let definition_ref ?definitions_path name =
    let path = definition_path_of_name ?definitions_path name in
    element (Def_ref path)

  let definition_exists ?definitions_path name schema =
    let path = definition_path_of_name ?definitions_path name in
    definition_exists path schema.definitions

  let add_definition ?definitions_path name elt schema =
    let path = definition_path_of_name ?definitions_path name in
    (* check inside def *)
    let definitions = insert_definition path elt schema.definitions in
    ({schema with definitions}, element (Def_ref path))

  let merge_definitions (sa, sb) =
    let definitions =
      Def_map.merge
        (fun name x y ->
          match (x, y) with
          | None, None -> None
          | Some x, None | None, Some x -> Some x
          | Some da, Some db ->
              if da.kind = Dummy || db.kind = Dummy || eq_element da db then
                Some da
              else raise (Duplicate_definition (name, da, db)))
        sa.definitions
        sb.definitions
    in
    ({sa with definitions}, {sb with definitions})

  let combine op schemas =
    let rec combine sacc eacc = function
      | [] -> update (element (Combine (op, eacc))) sacc
      | s :: ss ->
          let sacc, s = merge_definitions (sacc, s) in
          combine sacc (s.root :: eacc) ss
    in
    combine any [] schemas

  let is_nullable {ids; definitions; root; _} =
    let rec nullable {kind; _} =
      match kind with
      | Null | Any -> true
      | Object _ | Array _ | Monomorphic_array _ | Ext_ref _ | String _
      | Integer _ | Number _ | Boolean ->
          false
      | Combine (Not, [elt]) -> not (nullable elt)
      | Combine (All_of, elts) -> List.for_all nullable elts
      | Combine ((Any_of | One_of), elts) -> List.exists nullable elts
      | Def_ref path -> nullable (Def_map.find path definitions)
      | Id_ref id -> nullable (Id_map.find id ids)
      | Combine (Not, _) | Dummy -> assert false
    in
    nullable root

  (*-- default specs ---------------------------------------------------------*)

  let array_specs =
    {
      min_items = 0;
      max_items = None;
      unique_items = false;
      additional_items = None;
    }

  let object_specs =
    {
      properties = [];
      pattern_properties = [];
      additional_properties = Some (element Any);
      min_properties = 0;
      max_properties = None;
      schema_dependencies = [];
      property_dependencies = [];
    }

  let string_specs =
    {pattern = None; min_length = 0; max_length = None; str_format = None}

  let numeric_specs = {multiple_of = None; minimum = None; maximum = None}
end

include Make (Json_repr.Ezjsonm)
