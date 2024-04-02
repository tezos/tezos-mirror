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

exception Decoding_exception_whilst_conversion_of_lazy_encoding of bytes

exception Unexpected of string * string

exception No_case_matched of exn list

exception Bad_array_size of int * int

exception Missing_field of string

exception Unexpected_field of string

exception Bad_schema of exn

exception Cannot_destruct of (Json_query.path * exn)

(*-- types and errors --------------------------------------------------------*)

let unexpected kind expected =
  let kind =
    match kind with
    | `O [] -> "empty object"
    | `A [] -> "empty array"
    | `O _ -> "object"
    | `A _ -> "array"
    | `Null -> "null"
    | `String _ -> "string"
    | `Float _ -> "number"
    | `Bool _ -> "boolean"
  in
  Cannot_destruct ([], Unexpected (kind, expected))

type 't repr_agnostic_custom = {
  write : 'rt. (module Json_repr.Repr with type value = 'rt) -> 't -> 'rt;
  read : 'rf. (module Json_repr.Repr with type value = 'rf) -> 'rf -> 't;
  is_object : bool;
}

(* The GADT definition for encodings. This type must be kept internal
    because it does not encode all invariants. Some properties are
    checked at encoding construction time by smart constructors, since
    checking them would either be impossible, or would make the type
    too complex. In a few corners that involve custom encodings using
    user defined functions, some properties cannot be checked until
    construction/destruction time. If such a run time check fails, is
    denotes a programmer error and an [Invalid_argument] exceptions is
    thus raised. *)
type _ encoding =
  | Null : unit encoding
  | Empty : unit encoding
  | Ignore : unit encoding
  | Option : 'a encoding -> 'a option encoding
  | Constant : string -> unit encoding
  | Int : 'a int_encoding -> 'a encoding
  | Bool : bool encoding
  | String : string encoding
  | Float : bounds option -> float encoding
  | Array : 'a encoding -> 'a array encoding
  | Seq : 'a encoding -> 'a Seq.t encoding
  | Obj : 'a field -> 'a encoding
  | Objs : 'a encoding * 'b encoding -> ('a * 'b) encoding
  | Tup : 'a encoding -> 'a encoding
  | Tups : 'a encoding * 'b encoding -> ('a * 'b) encoding
  | Custom : 't repr_agnostic_custom * Json_schema.schema -> 't encoding
  | Conv :
      ('a -> 'b) * ('b -> 'a) * 'b encoding * Json_schema.schema option
      -> 'a encoding
  | Describe : {
      id : string;
      title : string option;
      description : string option;
      encoding : 'a encoding;
    }
      -> 'a encoding
  | Mu : {
      id : string;
      title : string option;
      description : string option;
      self : 'a encoding -> 'a encoding;
    }
      -> 'a encoding
  | Union : 't case list -> 't encoding

and 'a int_encoding = {
  int_name : string;
  of_float : float -> 'a;
  to_float : 'a -> float;
  lower_bound : 'a;
  upper_bound : 'a;
}

and bounds = {float_name : string; minimum : float; maximum : float}

and _ field =
  | Req : {
      name : string;
      encoding : 'a encoding;
      title : string option;
      description : string option;
    }
      -> 'a field
  | Opt : {
      name : string;
      encoding : 'a encoding;
      title : string option;
      description : string option;
    }
      -> 'a option field
  | Dft : {
      name : string;
      encoding : 'a encoding;
      title : string option;
      description : string option;
      equal : 'a -> 'a -> bool;
      default : 'a;
      construct_default : bool;
    }
      -> 'a field

and 't case =
  | Case : {
      encoding : 'a encoding;
      title : string option;
      description : string option;
      proj : 't -> 'a option;
      inj : 'a -> 't;
    }
      -> 't case

(*-- construct / destruct / schema over the main GADT forms ------------------*)

module type S = sig
  type repr_value

  val construct :
    ?include_default_fields:[`Always | `Auto | `Never] ->
    't encoding ->
    't ->
    repr_value

  val destruct :
    ?ignore_extra_fields:bool ->
    ?bson_relaxation:bool ->
    't encoding ->
    repr_value ->
    't

  val custom :
    ?is_object:bool ->
    ('t -> repr_value) ->
    (repr_value -> 't) ->
    schema:Json_schema.schema ->
    't encoding
end

let inc_field include_default_fields construct_default =
  match include_default_fields with
  | `Auto -> construct_default
  | `Never -> false
  | `Always -> true

let invalid_lazy_bytes : bytes encoding =
  let read : type tf. (module Json_repr.Repr with type value = tf) -> tf -> 't =
   fun (module Repr_f) repr ->
    match Repr_f.view repr with
    | `O [("invalid_lazy_bytes", hex_encoded_bytes)] -> (
        match Repr_f.view hex_encoded_bytes with
        | `String hex_encoded_bytes ->
            let b = Hex.to_bytes (`Hex hex_encoded_bytes) in
            b
        | u -> raise (unexpected u "string"))
    | `O _ as u -> raise (unexpected u "\"invalid_lazy_bytes\" field")
    | _ as u -> raise (unexpected u "object")
  in
  let write : type tf. (module Json_repr.Repr with type value = tf) -> 't -> tf
      =
   fun (module Repr_f) bytes ->
    Repr_f.repr
      (`O
        [
          ( "invalid_lazy_bytes",
            Repr_f.repr
              (`String
                (let (`Hex bytes) = Hex.of_bytes bytes in
                 bytes)) );
        ])
  in
  let schema =
    let open Json_schema in
    let properties =
      [("invalid_lazy_bytes", element (String string_specs), true, None)]
    in
    let additional_properties = None in
    let element =
      element (Object {object_specs with properties; additional_properties})
    in
    create element
  in
  Custom ({read; write; is_object = true}, schema)

module Make (Repr : Json_repr.Repr) : S with type repr_value = Repr.value =
struct
  type repr_value = Repr.value

  let construct ?(include_default_fields = `Auto) enc v =
    let rec construct : type t. t encoding -> t -> Repr.value = function
      | Null -> fun () -> Repr.repr `Null
      | Empty -> fun () -> Repr.repr (`O [])
      | Ignore -> fun () -> Repr.repr (`O [])
      | Option t -> (
          function None -> Repr.repr `Null | Some v -> construct t v)
      | Constant str -> fun () -> Repr.repr (`String str)
      | Int {int_name; to_float; lower_bound; upper_bound; _} ->
          fun (i : t) ->
            if i < lower_bound || i > upper_bound then
              invalid_arg
                ("Json_encoding.construct: " ^ int_name ^ " out of range") ;
            Repr.repr (`Float (to_float i))
      | Bool -> fun (b : t) -> Repr.repr (`Bool b)
      | String -> fun s -> Repr.repr (`String s)
      | Float (Some {minimum; maximum; float_name}) ->
          let err =
            "Json_encoding.construct: " ^ float_name ^ " out of range"
          in
          fun float ->
            if float < minimum || float > maximum then invalid_arg err ;
            Repr.repr (`Float float)
      | Float None -> fun float -> Repr.repr (`Float float)
      | Describe {encoding = t; _} -> construct t
      | Custom ({write; _}, _) -> fun (j : t) -> write (module Repr) j
      | Conv (ffrom, _, t, _) -> (
          fun v ->
            try construct t (ffrom v)
            with Decoding_exception_whilst_conversion_of_lazy_encoding b ->
              construct invalid_lazy_bytes b)
      | Mu {self; _} as enc -> construct (self enc)
      | Array t ->
          let w v = construct t v in
          fun arr -> Repr.repr (`A (Array.to_list (Array.map w arr)))
      | Seq t ->
          let w v = construct t v in
          fun s -> Repr.repr (`A (List.of_seq (Seq.map w s)))
      | Obj (Req {name = n; encoding = t; _}) ->
          let w v = construct t v in
          fun v -> Repr.repr (`O [(n, w v)])
      | Obj
          (Dft
            {name = n; equal; encoding = t; default = d; construct_default; _})
        ->
          let w v = construct t v in
          let inc_default =
            inc_field include_default_fields construct_default
          in
          fun v ->
            Repr.repr
              (`O (if inc_default || not (equal v d) then [(n, w v)] else []))
      | Obj (Opt {name = n; encoding = t; _}) -> (
          let w v = construct t v in
          function
          | None -> Repr.repr (`O []) | Some v -> Repr.repr (`O [(n, w v)]))
      | Objs (o1, o2) -> (
          let w1 v = construct o1 v in
          let w2 v = construct o2 v in
          function
          | v1, v2 -> (
              match (Repr.view (w1 v1), Repr.view (w2 v2)) with
              | `O l1, `O l2 -> Repr.repr (`O (List.append l1 l2))
              | `Null, `Null | _ ->
                  invalid_arg
                    "Json_encoding.construct: consequence of bad merge_objs"))
      | Tup t ->
          let w v = construct t v in
          fun v -> Repr.repr (`A [w v])
      | Tups (o1, o2) -> (
          let w1 v = construct o1 v in
          let w2 v = construct o2 v in
          function
          | v1, v2 -> (
              match (Repr.view (w1 v1), Repr.view (w2 v2)) with
              | `A l1, `A l2 -> Repr.repr (`A (List.append l1 l2))
              | _ ->
                  invalid_arg
                    "Json_encoding.construct: consequence of bad merge_tups"))
      | Union cases ->
          fun v ->
            let rec do_cases = function
              | [] ->
                  invalid_arg
                    "Json_encoding.construct: consequence of bad union"
              | Case {encoding; proj; _} :: rest -> (
                  match proj v with
                  | Some v -> construct encoding v
                  | None -> do_cases rest)
            in
            do_cases cases
    in
    construct enc v

  (* Used for bson_relaxation to convert objs to arrs *)
  let maybe_array_in_disguise fs =
    let rec is_maybe_array_in_disguise rev_acc index = function
      | [] -> Some (List.rev rev_acc)
      | (s, v) :: o ->
          if string_of_int index = s then
            is_maybe_array_in_disguise (v :: rev_acc) (index + 1) o
          else None
    in
    is_maybe_array_in_disguise [] 0 fs

  (* NOTE: bson relaxation is only an issue at top-level (see comment in
     interface). Hence, we set it to false on recursive calls that are actually
     nested, no matter its value. *)
  let rec destruct :
      type t.
      ignore_extra_fields:bool ->
      bson_relaxation:bool ->
      t encoding ->
      Repr.value ->
      t =
   fun ~ignore_extra_fields ~bson_relaxation enc ->
    match enc with
    | Null -> (
        fun v ->
          match Repr.view v with
          | `Null -> ()
          | k -> raise (unexpected k "null"))
    | Empty -> (
        fun v ->
          match Repr.view v with
          | `O [] -> ()
          | `O ((f, _) :: _) ->
              if ignore_extra_fields then ()
              else raise (Cannot_destruct ([`Field f], Unexpected_field f))
          | k -> raise @@ unexpected k "an empty object")
    | Ignore -> ( fun v -> match Repr.view v with _ -> ())
    | Option t -> (
        fun v ->
          match Repr.view v with
          | `Null -> None
          | _ -> Some (destruct ~ignore_extra_fields ~bson_relaxation t v))
    | Constant str -> (
        fun v ->
          match Repr.view v with
          | `String s when s = str -> ()
          | x -> raise @@ unexpected x str)
    | Int {int_name; of_float; to_float; lower_bound; upper_bound} -> (
        let lower_bound = to_float lower_bound in
        let upper_bound = to_float upper_bound in
        fun v ->
          match Repr.view v with
          | `Float v ->
              let rest, v = modf v in
              (if rest <> 0. then
               let exn =
                 Failure (int_name ^ " cannot have a fractional part")
               in
               raise (Cannot_destruct ([], exn))) ;
              (if v < lower_bound || v > upper_bound then
               let exn = Failure (int_name ^ " out of range") in
               raise (Cannot_destruct ([], exn))) ;
              of_float v
          | k -> raise (unexpected k "number"))
    | Bool -> (
        fun v ->
          match Repr.view v with
          | `Bool b -> (b : t)
          | k -> raise (unexpected k "boolean"))
    | String -> (
        fun v ->
          match Repr.view v with
          | `String s -> s
          | k -> raise (unexpected k "string"))
    | Float None -> (
        fun v ->
          match Repr.view v with
          | `Float f -> f
          | k -> raise (unexpected k "float"))
    | Float (Some {minimum; maximum; float_name}) -> (
        fun v ->
          match Repr.view v with
          | `Float f ->
              if f < minimum || f > maximum then
                let exn = Failure (float_name ^ " out of range") in
                raise (Cannot_destruct ([], exn))
              else f
          | k -> raise (unexpected k "float"))
    | Describe {encoding = t; _} ->
        destruct ~ignore_extra_fields ~bson_relaxation t
    | Custom ({read; _}, _) -> read (module Repr)
    | Conv (_, fto, t, _) ->
        fun v -> fto (destruct ~ignore_extra_fields ~bson_relaxation t v)
    | Mu {self; _} as enc ->
        destruct ~ignore_extra_fields ~bson_relaxation (self enc)
    | Array t -> (
        let array_of_cells cells =
          Array.mapi
            (fun i cell ->
              try destruct ~ignore_extra_fields ~bson_relaxation:false t cell
              with Cannot_destruct (path, err) ->
                raise (Cannot_destruct (`Index i :: path, err)))
            (Array.of_list cells)
        in
        fun v ->
          match Repr.view v with
          | `O [] ->
              (* For backwards compatibility, we handle [[]] with the
                 [bson_relaxation] semantic even if it is not set. *)
              [||]
          | `O o when bson_relaxation -> (
              (* Weak `Repr`s like BSON don't know the difference *)
              match maybe_array_in_disguise o with
              | Some cells -> array_of_cells cells
              | None -> raise @@ unexpected (`O o) "array")
          | `A cells -> array_of_cells cells
          | k -> raise @@ unexpected k "array")
    | Seq t -> (
        let seq_of_cells cells =
          let i = ref (-1) in
          Seq.map
            (fun cell ->
              try
                incr i ;
                destruct ~ignore_extra_fields ~bson_relaxation:false t cell
              with Cannot_destruct (path, err) ->
                raise (Cannot_destruct (`Index !i :: path, err)))
            (List.to_seq cells)
        in
        fun v ->
          match Repr.view v with
          | `O [] ->
              (* For backwards compatibility, we handle [[]] with the
                 [bson_relaxation] semantic even if it is not set. *)
              Seq.empty
          | `O o when bson_relaxation -> (
              (* Weak `Repr`s like BSON don't know the difference *)
              match maybe_array_in_disguise o with
              | Some cells -> seq_of_cells cells
              | None -> raise @@ unexpected (`O o) "array")
          | `A cells -> seq_of_cells cells
          | k -> raise @@ unexpected k "array")
    | Obj _ as t -> (
        let d = destruct_obj ~ignore_extra_fields t in
        fun v ->
          match Repr.view v with
          | `O fields -> (
              let r, rest, ign = d fields in
              match rest with
              | (field, _) :: _ when not ign ->
                  raise
                  @@ Cannot_destruct ([`Field field], Unexpected_field field)
              | _ -> r)
          | k -> raise @@ unexpected k "object")
    | Objs _ as t -> (
        let d = destruct_obj ~ignore_extra_fields t in
        fun v ->
          match Repr.view v with
          | `O fields -> (
              let r, rest, ign = d fields in
              match rest with
              | (field, _) :: _ when not ign ->
                  raise
                  @@ Cannot_destruct ([`Field field], Unexpected_field field)
              | _ -> r)
          | k -> raise @@ unexpected k "object")
    | Tup _ as t -> (
        let r, i = destruct_tup ~ignore_extra_fields 0 t in
        let tup_of_cells cells =
          let cells = Array.of_list cells in
          let len = Array.length cells in
          if i <> Array.length cells then
            raise (Cannot_destruct ([], Bad_array_size (len, i)))
          else r cells
        in
        fun v ->
          match Repr.view v with
          | `O o when bson_relaxation -> (
              (* Weak `Repr`s like BSON don't know the difference  *)
              match maybe_array_in_disguise o with
              | Some cells -> tup_of_cells cells
              | None -> raise @@ unexpected (`O o) "array")
          | `A cells -> tup_of_cells cells
          | k -> raise @@ unexpected k "array")
    | Tups _ as t -> (
        let r, i = destruct_tup ~ignore_extra_fields 0 t in
        let tups_of_cells cells =
          let cells = Array.of_list cells in
          let len = Array.length cells in
          if i <> Array.length cells then
            raise (Cannot_destruct ([], Bad_array_size (len, i)))
          else r cells
        in
        fun v ->
          match Repr.view v with
          | `O o when bson_relaxation -> (
              (* Weak `Repr`s like BSON don't know the difference  *)
              match maybe_array_in_disguise o with
              | Some cells -> tups_of_cells cells
              | None -> raise @@ unexpected (`O o) "array")
          | `A cells -> tups_of_cells cells
          | k -> raise @@ unexpected k "array")
    | Union cases ->
        fun v ->
          let rec do_cases errs = function
            | [] ->
                raise (Cannot_destruct ([], No_case_matched (List.rev errs)))
            | Case {encoding; inj; _} :: rest -> (
                try
                  inj
                    (destruct ~ignore_extra_fields ~bson_relaxation encoding v)
                with err -> do_cases (err :: errs) rest)
          in
          do_cases [] cases

  and destruct_tup :
      type t.
      ignore_extra_fields:bool ->
      int ->
      t encoding ->
      (Repr.value array -> t) * int =
   fun ~ignore_extra_fields i t ->
    match t with
    | Tup t ->
        ( (fun arr ->
            try destruct ~ignore_extra_fields ~bson_relaxation:false t arr.(i)
            with Cannot_destruct (path, err) ->
              raise (Cannot_destruct (`Index i :: path, err))),
          succ i )
    | Tups (t1, t2) ->
        let r1, i = destruct_tup ~ignore_extra_fields i t1 in
        let r2, i = destruct_tup ~ignore_extra_fields i t2 in
        ((fun arr -> (r1 arr, r2 arr)), i)
    | Conv (_, fto, t, _) ->
        let r, i = destruct_tup ~ignore_extra_fields i t in
        ((fun arr -> fto (r arr)), i)
    | Mu {self; _} as enc -> destruct_tup ~ignore_extra_fields i (self enc)
    | Describe {encoding; _} -> destruct_tup ~ignore_extra_fields i encoding
    | _ -> invalid_arg "Json_encoding.destruct: consequence of bad merge_tups"

  and destruct_obj :
      type t.
      ignore_extra_fields:bool ->
      t encoding ->
      (string * Repr.value) list ->
      t * (string * Repr.value) list * bool =
   fun ~ignore_extra_fields t ->
    let rec assoc acc n = function
      | [] -> raise Not_found
      | (f, v) :: rest when n = f -> (v, List.rev_append acc rest)
      | oth :: rest -> assoc (oth :: acc) n rest
    in
    match t with
    | Empty -> fun fields -> ((), fields, ignore_extra_fields)
    | Ignore -> fun fields -> ((), fields, true)
    | Obj (Req {name = n; encoding = t; _}) -> (
        fun fields ->
          try
            let v, rest = assoc [] n fields in
            ( destruct ~ignore_extra_fields ~bson_relaxation:false t v,
              rest,
              ignore_extra_fields )
          with
          | Not_found -> raise (Cannot_destruct ([], Missing_field n))
          | Cannot_destruct (path, err) ->
              raise (Cannot_destruct (`Field n :: path, err)))
    | Obj (Opt {name = n; encoding = t; _}) -> (
        fun fields ->
          try
            let v, rest = assoc [] n fields in
            ( Some (destruct ~ignore_extra_fields ~bson_relaxation:false t v),
              rest,
              ignore_extra_fields )
          with
          | Not_found -> (None, fields, ignore_extra_fields)
          | Cannot_destruct (path, err) ->
              raise (Cannot_destruct (`Field n :: path, err)))
    | Obj (Dft {name = n; encoding = t; default = d; _}) -> (
        fun fields ->
          try
            let v, rest = assoc [] n fields in
            ( destruct ~ignore_extra_fields ~bson_relaxation:false t v,
              rest,
              ignore_extra_fields )
          with
          | Not_found -> (d, fields, ignore_extra_fields)
          | Cannot_destruct (path, err) ->
              raise (Cannot_destruct (`Field n :: path, err)))
    | Objs (o1, o2) ->
        let d1 = destruct_obj ~ignore_extra_fields o1 in
        let d2 = destruct_obj ~ignore_extra_fields o2 in
        fun fields ->
          let r1, rest, ign1 = d1 fields in
          let r2, rest, ign2 = d2 rest in
          ((r1, r2), rest, ign1 || ign2)
    | Conv (_, fto, t, _) ->
        let d = destruct_obj ~ignore_extra_fields t in
        fun fields ->
          let r, rest, ign = d fields in
          (fto r, rest, ign)
    | Mu {self; _} as enc -> destruct_obj ~ignore_extra_fields (self enc)
    | Describe {encoding; _} -> destruct_obj ~ignore_extra_fields encoding
    | Union cases ->
        fun fields ->
          let rec do_cases errs = function
            | [] ->
                raise (Cannot_destruct ([], No_case_matched (List.rev errs)))
            | Case {encoding; inj; _} :: rest -> (
                try
                  let r, rest, ign =
                    destruct_obj ~ignore_extra_fields encoding fields
                  in
                  (inj r, rest, ign)
                with err -> do_cases (err :: errs) rest)
          in
          do_cases [] cases
    | _ -> invalid_arg "Json_encoding.destruct: consequence of bad merge_objs"

  let destruct ?(ignore_extra_fields = false) ?(bson_relaxation = false) e v =
    destruct ~ignore_extra_fields ~bson_relaxation e v

  let custom ?(is_object = false) write read ~schema =
    let read : type tf. (module Json_repr.Repr with type value = tf) -> tf -> 't
        =
     fun (module Repr_f) repr ->
      read (Json_repr.convert (module Repr_f) (module Repr) repr)
    in
    let write :
        type tf. (module Json_repr.Repr with type value = tf) -> 't -> tf =
     fun (module Repr_f) v ->
      Json_repr.convert (module Repr) (module Repr_f) (write v)
    in
    Custom ({read; write; is_object}, schema)
end

module Ezjsonm_encoding = Make (Json_repr.Ezjsonm)

let patch_description ?title ?description (elt : Json_schema.element) =
  match (title, description) with
  | None, None -> elt
  | Some _, None -> {elt with title}
  | None, Some _ -> {elt with description}
  | Some _, Some _ -> {elt with title; description}

let schema ?definitions_path encoding =
  let open Json_schema in
  let sch = ref any in
  let prod l1 l2 =
    List.concat_map
      (fun (l1, b1, e1) ->
        List.map
          (fun (l2, b2, e2) ->
            ( List.append l1 l2,
              b1 || b2,
              match (e1, e2) with Some e, _ | _, Some e -> Some e | _ -> None ))
          l2)
      l1
  in
  let rec object_schema :
      type t.
      t encoding ->
      ((string * element * bool * Json_repr.any option) list
      * bool
      * element option)
      list = function
    | Conv (_, _, o, None) -> object_schema o
    | Empty -> [([], false, None)]
    | Ignore -> [([], true, None)]
    | Obj (Req {name = n; encoding = t; title; description}) ->
        [
          ( [(n, patch_description ?title ?description (schema t), true, None)],
            false,
            None );
        ]
    | Obj (Opt {name = n; encoding = t; title; description}) ->
        [
          ( [(n, patch_description ?title ?description (schema t), false, None)],
            false,
            None );
        ]
    | Obj (Dft {name = n; encoding = t; title; description; default = d; _}) ->
        let d =
          Json_repr.repr_to_any
            (module Json_repr.Ezjsonm)
            (Ezjsonm_encoding.construct t d)
        in
        [
          ( [
              ( n,
                patch_description ?title ?description (schema t),
                false,
                Some d );
            ],
            false,
            None );
        ]
    | Objs (o1, o2) -> prod (object_schema o1) (object_schema o2)
    | Union [] -> invalid_arg "Json_encoding.schema: empty union in object"
    | Union cases ->
        List.concat_map
          (fun (Case {encoding = o; title; description; _}) ->
            let elt = patch_description ?title ?description (schema o) in
            match object_schema o with
            | [(l, b, _)] -> [(l, b, Some elt)]
            | l -> l)
          cases
    | Mu {self; _} as enc -> object_schema (self enc)
    | Describe {title; description; encoding; _} -> (
        let elt = patch_description ?title ?description (schema encoding) in
        match object_schema encoding with
        | [(l, b, _)] -> [(l, b, Some elt)]
        | l -> l)
    | Conv (_, _, _, Some _) (* FIXME: We could do better *) | _ ->
        invalid_arg "Json_encoding.schema: consequence of bad merge_objs"
  and array_schema_acc : type t. element list -> t encoding -> element list =
   fun acc enc ->
    match enc with
    | Conv (_, _, o, None) -> array_schema_acc acc o
    | Tup t -> schema t :: acc
    | Tups (t1, t2) ->
        let acc = array_schema_acc acc t1 in
        array_schema_acc acc t2
    | Mu {self; _} as enc -> array_schema_acc acc (self enc)
    | Describe {encoding = t; _} -> array_schema_acc acc t
    | Conv (_, _, _, Some _) (* FIXME: We could do better *) | _ ->
        invalid_arg "Json_encoding.schema: consequence of bad merge_tups"
  and array_schema : type t. t encoding -> element list =
   fun enc -> List.rev (array_schema_acc [] enc)
  and schema : type t. t encoding -> element = function
    | Null -> element Null
    | Empty -> element (Object {object_specs with additional_properties = None})
    | Ignore -> element Any
    | Option t -> element (Combine (One_of, [schema t; element Null]))
    | Int {to_float; lower_bound; upper_bound; _} ->
        let minimum = Some (to_float lower_bound, `Inclusive) in
        let maximum = Some (to_float upper_bound, `Inclusive) in
        element (Integer {multiple_of = None; minimum; maximum})
    | Bool -> element Boolean
    | Constant str ->
        {
          (element (String string_specs)) with
          enum = Some [Json_repr.to_any (`String str)];
        }
    | String -> element (String string_specs)
    | Float (Some {minimum; maximum; _}) ->
        element
          (Number
             {
               multiple_of = None;
               minimum = Some (minimum, `Inclusive);
               maximum = Some (maximum, `Inclusive);
             })
    | Float None -> element (Number numeric_specs)
    | Describe {id = name; title; description; encoding} ->
        let schema = patch_description ?title ?description (schema encoding) in
        let s, def = add_definition ?definitions_path name schema !sch in
        sch := fst (merge_definitions (!sch, s)) ;
        def
    | Custom (_, s) ->
        sch := fst (merge_definitions (!sch, s)) ;
        root s
    | Conv (_, _, _, Some s) ->
        sch := fst (merge_definitions (!sch, s)) ;
        root s
    | Conv (_, _, t, None) -> schema t
    | Mu {id = name; title; description; self = f} ->
        let fake_schema =
          if definition_exists ?definitions_path name !sch then
            update (definition_ref ?definitions_path name) !sch
          else
            let sch, elt =
              add_definition ?definitions_path name (element Dummy) !sch
            in
            update elt sch
        in
        let fake_self =
          Custom
            ( {
                write = (fun _ _ -> assert false);
                read = (fun _ -> assert false);
                is_object = false;
              },
              fake_schema )
        in
        let root =
          patch_description ?title ?description (schema (f fake_self))
        in
        let nsch, def = add_definition ?definitions_path name root !sch in
        sch := nsch ;
        def
    | Array t -> element (Monomorphic_array (schema t, array_specs))
    | Seq t -> element (Monomorphic_array (schema t, array_specs))
    | Objs _ as o -> (
        match object_schema o with
        | [(properties, ext, elt)] -> (
            let additional_properties =
              if ext then Some (element Any) else None
            in
            match elt with
            | None ->
                element
                  (Object {object_specs with properties; additional_properties})
            | Some elt ->
                {
                  (element
                     (Object
                        {object_specs with properties; additional_properties}))
                  with
                  title = elt.title;
                  description = elt.description;
                })
        | more ->
            let elements =
              List.map
                (fun (properties, ext, elt) ->
                  let additional_properties =
                    if ext then Some (element Any) else None
                  in
                  match elt with
                  | None ->
                      element
                        (Object
                           {object_specs with properties; additional_properties})
                  | Some elt ->
                      {
                        (element
                           (Object
                              {
                                object_specs with
                                properties;
                                additional_properties;
                              }))
                        with
                        title = elt.title;
                        description = elt.description;
                      })
                more
            in
            element (Combine (One_of, elements)))
    | Obj _ as o -> (
        match object_schema o with
        | [(properties, ext, elt)] -> (
            let additional_properties =
              if ext then Some (element Any) else None
            in
            match elt with
            | None ->
                element
                  (Object {object_specs with properties; additional_properties})
            | Some elt ->
                {
                  (element
                     (Object
                        {object_specs with properties; additional_properties}))
                  with
                  title = elt.title;
                  description = elt.description;
                })
        | more ->
            let elements =
              List.map
                (fun (properties, ext, elt) ->
                  let additional_properties =
                    if ext then Some (element Any) else None
                  in
                  match elt with
                  | None ->
                      element
                        (Object
                           {object_specs with properties; additional_properties})
                  | Some elt ->
                      {
                        (element
                           (Object
                              {
                                object_specs with
                                properties;
                                additional_properties;
                              }))
                        with
                        title = elt.title;
                        description = elt.description;
                      })
                more
            in
            element (Combine (One_of, elements)))
    | Tup _ as t -> element (Array (array_schema t, array_specs))
    | Tups _ as t -> element (Array (array_schema t, array_specs))
    | Union cases ->
        (* FIXME: smarter merge *)
        let elements =
          List.map
            (fun (Case {encoding; title; description; _}) ->
              patch_description ?title ?description (schema encoding))
            cases
        in
        element (Combine (One_of, elements))
  and schema_specialization_first_pass : type t. t encoding -> element =
   (* This function is needed as to not create a level of inderaction when creating
      a top-level def. *)
   fun encoding ->
    match encoding with
    | Describe {title; description; encoding; _} ->
        let schema = patch_description ?title ?description (schema encoding) in
        schema
    | _ -> schema encoding
  in
  let schema = schema_specialization_first_pass encoding in
  update schema !sch

(*-- utility wrappers over the GADT ------------------------------------------*)

let req ?title ?description n t =
  Req {name = n; encoding = t; title; description}

let opt ?title ?description n t =
  Opt {name = n; encoding = t; title; description}

let dft ?title ?description ?(equal = ( = )) ?(construct = false) n t d =
  Dft
    {
      name = n;
      encoding = t;
      title;
      description;
      equal;
      default = d;
      construct_default = construct;
    }

let mu name ?title ?description self =
  let mem = ref None in
  let self e =
    match !mem with
    | Some (e_param, e_result) when e_param == e -> e_result
    | _ ->
        let e_result = self e in
        mem := Some (e, e_result) ;
        e_result
  in
  Mu {id = name; title; description; self}

let null = Null

let int =
  Int
    {
      int_name = "int";
      of_float = int_of_float;
      to_float = float_of_int;
      (* cross-platform consistent OCaml ints *)
      lower_bound = -(1 lsl 30);
      upper_bound = (1 lsl 30) - 1;
    }

let ranged_int ~minimum:lower_bound ~maximum:upper_bound name =
  if
    Sys.word_size = 64
    && (lower_bound < -(1 lsl 30) || upper_bound > (1 lsl 30) - 1)
  then
    invalid_arg "Json_encoding.ranged_int: bounds out of portable int31 range" ;
  Int
    {
      int_name = name;
      of_float = int_of_float;
      to_float = float_of_int;
      lower_bound;
      upper_bound;
    }

let int53 =
  Int
    {
      int_name = "int53";
      of_float = Int64.of_float;
      to_float = Int64.to_float;
      lower_bound = Int64.neg (Int64.shift_left 1L 53);
      upper_bound = Int64.shift_left 1L 53;
    }

let ranged_int53 ~minimum:lower_bound ~maximum:upper_bound name =
  if
    lower_bound < Int64.neg (Int64.shift_left 1L 53)
    || upper_bound > Int64.shift_left 1L 53
  then
    invalid_arg
      "Json_encoding.ranged_int53: bounds out of JSON-representable integers" ;
  Int
    {
      int_name = name;
      of_float = Int64.of_float;
      to_float = Int64.to_float;
      lower_bound;
      upper_bound;
    }

let int32 =
  Int
    {
      int_name = "int32";
      of_float = Int32.of_float;
      to_float = Int32.to_float;
      lower_bound = Int32.min_int;
      upper_bound = Int32.max_int;
    }

let ranged_int32 ~minimum:lower_bound ~maximum:upper_bound name =
  Int
    {
      int_name = name;
      of_float = Int32.of_float;
      to_float = Int32.to_float;
      lower_bound;
      upper_bound;
    }

let ranged_float ~minimum ~maximum float_name =
  Float (Some {minimum; maximum; float_name})

let float = Float None

let string = String

let conv ffrom fto ?schema t = Conv (ffrom, fto, t, schema)

let bytes = Conv (Bytes.to_string, Bytes.of_string, string, None)

let bool = Bool

let array t = Array t

let seq t = Seq t

let obj1 f1 = Obj f1

let obj2 f1 f2 = Objs (Obj f1, Obj f2)

let obj3 f1 f2 f3 =
  conv
    (fun (a, b, c) -> (a, (b, c)))
    (fun (a, (b, c)) -> (a, b, c))
    (Objs (Obj f1, Objs (Obj f2, Obj f3)))

let obj4 f1 f2 f3 f4 =
  conv
    (fun (a, b, c, d) -> (a, (b, (c, d))))
    (fun (a, (b, (c, d))) -> (a, b, c, d))
    (Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Obj f4))))

let obj5 f1 f2 f3 f4 f5 =
  conv
    (fun (a, b, c, d, e) -> (a, (b, (c, (d, e)))))
    (fun (a, (b, (c, (d, e)))) -> (a, b, c, d, e))
    (Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Obj f5)))))

let obj6 f1 f2 f3 f4 f5 f6 =
  conv
    (fun (a, b, c, d, e, f) -> (a, (b, (c, (d, (e, f))))))
    (fun (a, (b, (c, (d, (e, f))))) -> (a, b, c, d, e, f))
    (Objs
       ( Obj f1,
         Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Objs (Obj f5, Obj f6)))) ))

let obj7 f1 f2 f3 f4 f5 f6 f7 =
  conv
    (fun (a, b, c, d, e, f, g) -> (a, (b, (c, (d, (e, (f, g)))))))
    (fun (a, (b, (c, (d, (e, (f, g)))))) -> (a, b, c, d, e, f, g))
    (let rest = Objs (Obj f6, Obj f7) in
     Objs
       (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Objs (Obj f5, rest))))))

let obj8 f1 f2 f3 f4 f5 f6 f7 f8 =
  conv
    (fun (a, b, c, d, e, f, g, h) -> (a, (b, (c, (d, (e, (f, (g, h))))))))
    (fun (a, (b, (c, (d, (e, (f, (g, h))))))) -> (a, b, c, d, e, f, g, h))
    (let rest = Objs (Obj f6, Objs (Obj f7, Obj f8)) in
     Objs
       (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Objs (Obj f5, rest))))))

let obj9 f1 f2 f3 f4 f5 f6 f7 f8 f9 =
  conv
    (fun (a, b, c, d, e, f, g, h, i) ->
      (a, (b, (c, (d, (e, (f, (g, (h, i)))))))))
    (fun (a, (b, (c, (d, (e, (f, (g, (h, i)))))))) ->
      (a, b, c, d, e, f, g, h, i))
    (let rest = Objs (Obj f6, Objs (Obj f7, Objs (Obj f8, Obj f9))) in
     Objs
       (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Objs (Obj f5, rest))))))

let obj10 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 =
  conv
    (fun (a, b, c, d, e, f, g, h, i, j) ->
      (a, (b, (c, (d, (e, (f, (g, (h, (i, j))))))))))
    (fun (a, (b, (c, (d, (e, (f, (g, (h, (i, j))))))))) ->
      (a, b, c, d, e, f, g, h, i, j))
    (let rest =
       Objs (Obj f6, Objs (Obj f7, Objs (Obj f8, Objs (Obj f9, Obj f10))))
     in
     Objs
       (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Objs (Obj f5, rest))))))

let tup1 f1 = Tup f1

let tup2 f1 f2 = Tups (Tup f1, Tup f2)

let tup3 f1 f2 f3 =
  conv
    (fun (a, b, c) -> (a, (b, c)))
    (fun (a, (b, c)) -> (a, b, c))
    (Tups (Tup f1, Tups (Tup f2, Tup f3)))

let tup4 f1 f2 f3 f4 =
  conv
    (fun (a, b, c, d) -> (a, (b, (c, d))))
    (fun (a, (b, (c, d))) -> (a, b, c, d))
    (Tups (Tup f1, Tups (Tup f2, Tups (Tup f3, Tup f4))))

let tup5 f1 f2 f3 f4 f5 =
  conv
    (fun (a, b, c, d, e) -> (a, (b, (c, (d, e)))))
    (fun (a, (b, (c, (d, e)))) -> (a, b, c, d, e))
    (Tups (Tup f1, Tups (Tup f2, Tups (Tup f3, Tups (Tup f4, Tup f5)))))

let tup6 f1 f2 f3 f4 f5 f6 =
  conv
    (fun (a, b, c, d, e, f) -> (a, (b, (c, (d, (e, f))))))
    (fun (a, (b, (c, (d, (e, f))))) -> (a, b, c, d, e, f))
    (Tups
       ( Tup f1,
         Tups (Tup f2, Tups (Tup f3, Tups (Tup f4, Tups (Tup f5, Tup f6)))) ))

let tup7 f1 f2 f3 f4 f5 f6 f7 =
  conv
    (fun (a, b, c, d, e, f, g) -> (a, (b, (c, (d, (e, (f, g)))))))
    (fun (a, (b, (c, (d, (e, (f, g)))))) -> (a, b, c, d, e, f, g))
    (let rest = Tups (Tup f6, Tup f7) in
     Tups
       (Tup f1, Tups (Tup f2, Tups (Tup f3, Tups (Tup f4, Tups (Tup f5, rest))))))

let tup8 f1 f2 f3 f4 f5 f6 f7 f8 =
  conv
    (fun (a, b, c, d, e, f, g, h) -> (a, (b, (c, (d, (e, (f, (g, h))))))))
    (fun (a, (b, (c, (d, (e, (f, (g, h))))))) -> (a, b, c, d, e, f, g, h))
    (let rest = Tups (Tup f6, Tups (Tup f7, Tup f8)) in
     Tups
       (Tup f1, Tups (Tup f2, Tups (Tup f3, Tups (Tup f4, Tups (Tup f5, rest))))))

let tup9 f1 f2 f3 f4 f5 f6 f7 f8 f9 =
  conv
    (fun (a, b, c, d, e, f, g, h, i) ->
      (a, (b, (c, (d, (e, (f, (g, (h, i)))))))))
    (fun (a, (b, (c, (d, (e, (f, (g, (h, i)))))))) ->
      (a, b, c, d, e, f, g, h, i))
    (let rest = Tups (Tup f6, Tups (Tup f7, Tups (Tup f8, Tup f9))) in
     Tups
       (Tup f1, Tups (Tup f2, Tups (Tup f3, Tups (Tup f4, Tups (Tup f5, rest))))))

let tup10 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 =
  conv
    (fun (a, b, c, d, e, f, g, h, i, j) ->
      (a, (b, (c, (d, (e, (f, (g, (h, (i, j))))))))))
    (fun (a, (b, (c, (d, (e, (f, (g, (h, (i, j))))))))) ->
      (a, b, c, d, e, f, g, h, i, j))
    (let rest =
       Tups (Tup f6, Tups (Tup f7, Tups (Tup f8, Tups (Tup f9, Tup f10))))
     in
     Tups
       (Tup f1, Tups (Tup f2, Tups (Tup f3, Tups (Tup f4, Tups (Tup f5, rest))))))

let repr_agnostic_custom {write; read; is_object} ~schema =
  Custom ({write; read; is_object}, schema)

let constant s = Constant s

let string_enum cases =
  let schema =
    let specs = Json_schema.string_specs in
    let enum =
      List.map
        (fun (s, _) -> Json_repr.(repr_to_any (module Ezjsonm)) (`String s))
        cases
    in
    Json_schema.(update {(element (String specs)) with enum = Some enum} any)
  in
  let len = List.length cases in
  let mcases = Hashtbl.create len and rcases = Hashtbl.create len in
  let cases_str =
    match cases with
    | [] -> ""
    | c :: cs ->
        let b = Buffer.create 128 in
        Buffer.add_char b '\'' ;
        Buffer.add_string b (fst c) ;
        Buffer.add_char b '\'' ;
        List.iter
          (fun c ->
            Buffer.add_char b ' ' ;
            Buffer.add_char b '\'' ;
            Buffer.add_string b (fst c) ;
            Buffer.add_char b '\'')
          cs ;
        Buffer.contents b
  in
  List.iter
    (fun (s, c) ->
      if Hashtbl.mem mcases s then
        invalid_arg "Json_encoding.string_enum: duplicate case" ;
      Hashtbl.add mcases s c ;
      Hashtbl.add rcases c s)
    cases ;
  conv
    (fun v ->
      try Hashtbl.find rcases v
      with Not_found ->
        invalid_arg
          (Format.sprintf
             "Json_encoding.construct: consequence of non exhaustive \
              Json_encoding.string_enum. Strings are: %s"
             cases_str))
    (fun s ->
      try Hashtbl.find mcases s
      with Not_found ->
        let rec orpat ppf = function
          | [] -> assert false
          | [(last, _)] -> Format.fprintf ppf "%S" last
          | [(prev, _); (last, _)] -> Format.fprintf ppf "%S or %S" prev last
          | (prev, _) :: rem -> Format.fprintf ppf "%S , %a" prev orpat rem
        in
        let unexpected = Format.asprintf "string value %S" s in
        let expected = Format.asprintf "%a" orpat cases in
        raise (Cannot_destruct ([], Unexpected (unexpected, expected))))
    ~schema
    string

let def id ?title ?description encoding =
  Describe {id; title; description; encoding}

let assoc :
    type t. ?definitions_path:string -> t encoding -> (string * t) list encoding
    =
 fun ?definitions_path t ->
  Ezjsonm_encoding.custom
    ~is_object:true
    (fun l ->
      `O (List.map (fun (n, v) -> (n, Ezjsonm_encoding.construct t v)) l))
    (fun v ->
      match v with
      | `O l ->
          let destruct n t v =
            try Ezjsonm_encoding.destruct t v
            with Cannot_destruct (p, exn) ->
              raise (Cannot_destruct (`Field n :: p, exn))
          in
          List.map (fun (n, v) -> (n, destruct n t v)) l
      | #Json_repr.ezjsonm as k -> raise (unexpected k "asssociative object"))
    ~schema:
      (let s = schema ?definitions_path t in
       Json_schema.(
         update
           (element
              (Object {object_specs with additional_properties = Some (root s)}))
           s))

let rec is_nullable : type t. t encoding -> bool = function
  | Constant _ -> false
  | Int _ -> false
  | Float _ -> false
  | Array _ -> false
  | Seq _ -> false
  | Empty -> false
  | String -> false
  | Bool -> false
  | Obj _ -> false
  | Tup _ -> false
  | Objs _ -> false
  | Tups _ -> false
  | Null -> true
  | Ignore -> true
  | Option _ -> true
  | Conv (_, _, t, _) -> is_nullable t
  | Union cases ->
      List.exists (fun (Case {encoding = t; _}) -> is_nullable t) cases
  | Describe {encoding = t; _} -> is_nullable t
  | Mu {self; _} as enc -> is_nullable (self enc)
  | Custom (_, sch) -> Json_schema.is_nullable sch

let option : type t. t encoding -> t option encoding =
 fun t ->
  if is_nullable t then
    invalid_arg "Json_encoding.option: cannot nest nullable encodings" ;
  Option t

let any_value =
  let read repr v = Json_repr.repr_to_any repr v in
  let write repr v = Json_repr.any_to_repr repr v in
  Custom ({read; write; is_object = false}, Json_schema.any)

let any_ezjson_value =
  let read repr v = Json_repr.convert repr (module Json_repr.Ezjsonm) v in
  let write repr v = Json_repr.convert (module Json_repr.Ezjsonm) repr v in
  Custom ({read; write; is_object = false}, Json_schema.any)

let any_document =
  let read :
      type tt.
      (module Json_repr.Repr with type value = tt) -> tt -> Json_repr.any =
   fun (module Repr) v ->
    match Repr.view v with
    | `A _ | `O _ -> Json_repr.repr_to_any (module Repr) v
    | k -> raise @@ unexpected k "array or object"
  in
  let write repr v = Json_repr.any_to_repr repr v in
  Custom ({read; write; is_object = false}, Json_schema.any)

let any_object =
  let read :
      type tt.
      (module Json_repr.Repr with type value = tt) -> tt -> Json_repr.any =
   fun (module Repr) v ->
    match Repr.view v with
    | `O _ -> Json_repr.repr_to_any (module Repr) v
    | k -> raise @@ unexpected k "object"
  in
  let write :
      type tt.
      (module Json_repr.Repr with type value = tt) -> Json_repr.any -> tt =
   fun (module Repr) v ->
    let r = Json_repr.any_to_repr (module Repr) v in
    match Repr.view r with `O _ -> r | k -> raise @@ unexpected k "object"
  in
  Custom ({read; write; is_object = true}, Json_schema.any)

let any_ezjson_object =
  let read :
      type tt.
      (module Json_repr.Repr with type value = tt) -> tt -> Json_repr.ezjsonm =
   fun (module Repr) v ->
    match Repr.view v with
    | `O _ -> Json_repr.convert (module Repr) (module Json_repr.Ezjsonm) v
    | k -> raise @@ unexpected k "object"
  in
  let write repr v =
    match v with
    | `O _ -> Json_repr.convert (module Json_repr.Ezjsonm) repr v
    | k -> raise @@ unexpected k "object"
  in
  Custom ({read; write; is_object = true}, Json_schema.any)

let any_schema =
  Ezjsonm_encoding.custom
    ~is_object:true
    Json_schema.to_json
    (fun j ->
      try Json_schema.of_json j
      with err -> raise (Cannot_destruct ([], Bad_schema err)))
    ~schema:Json_schema.self

let merge_tups t1 t2 =
  let rec is_tup : type t. t encoding -> bool = function
    | Tup _ -> true
    | Tups _ (* by construction *) -> true
    | Conv (_, _, t, None) -> is_tup t
    | Mu {self; _} as enc -> is_tup (self enc)
    | Describe {encoding = t; _} -> is_tup t
    | _ -> false
  in
  if is_tup t1 && is_tup t2 then Tups (t1, t2)
  else invalid_arg "Json_encoding.merge_tups"

let list t = Conv (Array.of_list, Array.to_list, Array t, None)

let merge_objs o1 o2 =
  (* FIXME: check fields unicity *)
  let rec is_obj : type t. t encoding -> bool = function
    | Obj _ -> true
    | Objs _ (* by construction *) -> true
    | Conv (_, _, t, None) -> is_obj t
    | Empty -> true
    | Ignore -> true
    | Union cases ->
        List.for_all (fun (Case {encoding = o; _}) -> is_obj o) cases
    | Mu {self; _} as enc -> is_obj (self enc)
    | Describe {encoding = t; _} -> is_obj t
    | Custom ({is_object; _}, _) -> is_object
    | _ -> false
  in
  if is_obj o1 && is_obj o2 then Objs (o1, o2)
  else invalid_arg "Json_encoding.merge_objs"

let empty = Empty

let unit = Ignore

let case ?title ?description encoding proj inj =
  Case {encoding; proj; inj; title; description}

let union = function
  | [] -> invalid_arg "Json_encoding.union"
  | cases ->
      (* FIXME: check mutual exclusion *)
      Union cases

let rec print_error ?print_unknown ppf = function
  | Cannot_destruct ([], exn) -> print_error ?print_unknown ppf exn
  | Cannot_destruct (path, Unexpected (unex, ex)) ->
      Format.fprintf
        ppf
        "At %a, unexpected %s instead of %s"
        (Json_query.print_path_as_json_path ~wildcards:true)
        path
        unex
        ex
  | Cannot_destruct (path, No_case_matched errs) ->
      Format.fprintf
        ppf
        "@[<v 2>At %a, no case matched:@,%a@]"
        (Json_query.print_path_as_json_path ~wildcards:true)
        path
        (Format.pp_print_list (print_error ?print_unknown))
        errs
  | Cannot_destruct (path, Bad_array_size (unex, ex)) ->
      Format.fprintf
        ppf
        "At %a, unexpected array of size %d instead of %d"
        (Json_query.print_path_as_json_path ~wildcards:true)
        path
        unex
        ex
  | Cannot_destruct (path, Missing_field n) ->
      Format.fprintf
        ppf
        "At %a, missing object field %s"
        (Json_query.print_path_as_json_path ~wildcards:true)
        path
        n
  | Cannot_destruct (path, Unexpected_field n) ->
      Format.fprintf
        ppf
        "At %a, unexpected object field %s"
        (Json_query.print_path_as_json_path ~wildcards:true)
        path
        n
  | Cannot_destruct (path, Bad_schema exn) ->
      Format.fprintf
        ppf
        "@[<v 2>At %a, bad custom schema:@,%a@]"
        (Json_query.print_path_as_json_path ~wildcards:true)
        path
        (print_error ?print_unknown)
        exn
  | Unexpected (unex, ex) ->
      Format.fprintf ppf "Unexpected %s instead of %s" unex ex
  | No_case_matched errs ->
      Format.fprintf
        ppf
        "@[<v 2>No case matched:@,%a@]"
        (Format.pp_print_list (print_error ?print_unknown))
        errs
  | Bad_array_size (unex, ex) ->
      Format.fprintf ppf "Unexpected array of size %d instead of %d" unex ex
  | Missing_field n -> Format.fprintf ppf "Missing object field %s" n
  | Unexpected_field n -> Format.fprintf ppf "Unexpected object field %s" n
  | Bad_schema exn ->
      Format.fprintf
        ppf
        "@[<v 2>bad custom schema:@,%a@]"
        (print_error ?print_unknown)
        exn
  | Cannot_destruct (path, exn) ->
      Format.fprintf
        ppf
        "@[<v 2>At %a:@,%a@]"
        (Json_query.print_path_as_json_path ~wildcards:true)
        path
        (print_error ?print_unknown)
        exn
  | exn -> Json_schema.print_error ?print_unknown ppf exn

include Ezjsonm_encoding

(* An alternative construction method that produces a [Seq.t] of Json lexeme
   (compatible with [Jsonm.lexeme Seq.t]).

   This alternative gives a lazy construction where the consumer of the returned
   value requests further chunks as needed. This in turns allows for yielding in
   Lwt/Async contexts. *)

(* [jsonm_lexeme] is the type of json lexeme compatible with [Jsonm.lexeme].
   Note that [Jsonm] was made before the [Seq.t] type was available in the
   OCaml's standard-library.

   @see https://erratique.ch/software/jsonm/doc/Jsonm/ Jsonm documentation *)
type jsonm_lexeme =
  [ `Null
  | `Bool of bool
  | `String of string
  | `Float of float
  | `Name of string
  | `As
  | `Ae
  | `Os
  | `Oe ]

module JsonmLexemeSeq = struct
  (* First, a few helper functions for operating on [Seq.t] *)

  (* [++] is a constructor: [x ++ xs] is the sequence that starts with [x] and
      continues with [xs], it is equivalent to [Seq.cons] (available in OCaml
      4.11). *)
  let ( ++ ) v s () = Seq.Cons (v, s)

  (* [@] concatenates two sequences together. [xs @ ys] is a sequence that
      contains the elements from the seuqence [xs] followed by the elements of
      the sequence [ys]. It is equivalent to [Seq.append] (available in OCaml
      4.11). *)
  let rec ( @ ) (s1 : 'a Seq.t) (s2 : 'a Seq.t) : 'a Seq.t =
   fun () ->
    match s1 () with
    | Seq.Nil -> s2 ()
    | Seq.Cons (v, s1) -> Seq.Cons (v, s1 @ s2)

  (* [s +< c +> e] is a sequence that starts with [s], continues with [c] and
      ends with [e]. Below, this form is used to add object (resp. array)
      delimiters ([`Os]/[`Oe]) (resp. ([`As]/[`Ae])) around the sequence of
      lexemes that represents the contents of the object (resp. array). *)
  let ( +< ) = ( ++ )

  let ( +@ ) = ( @ )

  let ( +> ) s v = s @ Seq.return v

  (* [null] is a lexeme sequence representation of the null json value ([null]) *)
  let null = Seq.return `Null

  (* [empty_obj] is a lexeme sequence representation of the empty json object ([{}]). *)
  let empty_obj =
    let open Seq in
    fun () -> Cons (`Os, fun () -> Cons (`Oe, empty))

  (* [empty_arr] is a lexeme sequence representation of the empty json array ([[]]). *)
  let empty_arr =
    let open Seq in
    fun () -> Cons (`As, fun () -> Cons (`Ae, empty))

  (* convert an ezjsonm object into a lexeme sequence. This is useful for the
     [Custom] tag.

     An alternative is to have a [Json_repr.JsonmLexemeSeq] module to have a
     direct [Custom]-to-jsonm-lexeme function. However, the specifics of the
     [Repr] is not friendly to the jsonm-lexeme. Specifically the function
     [view: value -> value view] requires to force the whole of the sequence. We
     do not use [view] for writing so we might provide this in the future.

     The implementation is rather straightforward, except that empty objects and
     empty arrays are special-cased for performance. In the future, more
     specific objects/arrays might also be (e.g., objects that contain a single
     field with an immediate (non-nested) value). *)
  let rec jsonm_lexeme_seq_of_ezjson ezj =
    match ezj with
    | `O [] -> empty_obj
    | `O kvs -> `Os +< jsonm_lexeme_seq_of_ezjson_kvs kvs +> `Oe
    | `A [] -> empty_arr
    | `A vs -> `As +< jsonm_lexeme_seq_of_ezjson_vs vs +> `Ae
    | `Bool b -> Seq.return (`Bool b)
    | `Float f -> Seq.return (`Float f)
    | `String s -> Seq.return (`String s)
    | `Null -> null

  (* we extract the two following sub-functions because we need them for
     special cases when constructing objects/tups *)
  and jsonm_lexeme_seq_of_ezjson_kvs kvs =
    Seq.flat_map
      (fun (k, v) -> `Name k ++ jsonm_lexeme_seq_of_ezjson v)
      (List.to_seq kvs)

  and jsonm_lexeme_seq_of_ezjson_vs vs =
    Seq.flat_map (fun v -> jsonm_lexeme_seq_of_ezjson v) (List.to_seq vs)

  let construct_seq ?(include_default_fields = `Auto) enc v =
    (* The main entry-point, it is mutually recursive with some other entry
       points for specific "states" of the "state-machine" that this function
       represents.
       Note that this function mimics the {!Make}[.construct] function above in
       this module. There are a few entries that differ, this is due to the
       different target of the function (a sequence of lexeme vs an AST). In
       those cases, comments are provided. *)
    let rec construct : type t. t encoding -> t -> jsonm_lexeme Seq.t = function
      | Null -> fun (() : t) -> null
      | Empty -> fun () -> empty_obj
      | Ignore -> fun () -> empty_obj
      | Option t -> (
          function None -> null | Some v -> (construct [@ocaml.tailcall]) t v)
      | Constant str -> fun () -> Seq.return (`String str)
      | Int {int_name; to_float; lower_bound; upper_bound; _} ->
          fun (i : t) ->
            if i < lower_bound || i > upper_bound then
              invalid_arg
                ("Json_encoding.construct_seq: " ^ int_name ^ " out of range") ;
            Seq.return (`Float (to_float i))
      | Bool -> fun (b : t) -> Seq.return (`Bool b)
      | String -> fun s -> Seq.return (`String s)
      | Float (Some {minimum; maximum; float_name}) ->
          fun float ->
            if float < minimum || float > maximum then
              invalid_arg
                ("Json_encoding.construct_seq: " ^ float_name ^ " out of range") ;
            Seq.return (`Float float)
      | Float None -> fun float -> Seq.return (`Float float)
      | Describe {encoding = t; _} -> fun v -> (construct [@ocaml.tailcall]) t v
      | Custom ({write; _}, _) ->
          fun v ->
            let ezjson = write (module Json_repr.Ezjsonm) v in
            jsonm_lexeme_seq_of_ezjson ezjson
      | Conv (ffrom, _, t, _) ->
          fun v -> (construct [@ocaml.tailcall]) t (ffrom v)
      | Mu {self; _} as enc ->
          fun v -> (construct [@ocaml.tailcall]) (self enc) v
      | Array t -> (
          function [||] -> empty_arr | vs -> `As +< construct_arr t vs +> `Ae)
      | Seq t -> fun s -> `As +< construct_seq_ t s +> `Ae
      | Obj (Req {name = n; encoding = t; _}) ->
          fun v -> `Os +< construct_named n t v +> `Oe
      | Obj
          (Dft
            {name = n; equal; encoding = t; default = d; construct_default; _})
        ->
          fun v ->
            let inc_default =
              inc_field include_default_fields construct_default
            in
            if inc_default || not (equal v d) then
              `Os +< construct_named n t v +> `Oe
            else empty_obj
      | Obj (Opt {name = n; encoding = t; _}) -> (
          function
          | None -> empty_obj | Some v -> `Os +< construct_named n t v +> `Oe)
      | Objs (o1, o2) ->
          (* For the objects inside an [Objs] we go to a different state of
             the state-machine: we call the entry-point [construct_obj].
             Note that the non-seq construction simply builds the
             sub-objects and pops the content out of the object AST node.
             This is not viable here because it'd force the sequence to
             remove the last lexeme ([`Oe]). Trying a hybrid approach of
             doing standard construction followed by a lazy Object delimiter
             popping is more complicated than shifting to a different
             state/entry-point. *)
          fun (v1, v2) ->
           `Os +< construct_obj o1 v1 +@ construct_obj o2 v2 +> `Oe
      | Tup t -> fun v -> `As +< construct t v +> `Ae
      | Tups (o1, o2) ->
          fun (v1, v2) ->
            (* Similar to the Objs construction *)
            `As +< construct_tup o1 v1 +@ construct_tup o2 v2 +> `Ae
      | Union cases ->
          fun v ->
            let rec do_cases = function
              | [] ->
                  invalid_arg
                    "Json_encoding.construct_seq: consequence of bad union"
              | Case {encoding; proj; _} :: rest -> (
                  match proj v with
                  | Some v -> (construct [@ocaml.tailcall]) encoding v
                  | None -> do_cases rest)
            in
            do_cases cases
    and construct_arr : type t. t encoding -> t array -> jsonm_lexeme Seq.t =
     fun t vs ->
      (* TODO: optimise this one for tailcall ? *)
      Seq.flat_map (fun v -> construct t v) (Array.to_seq vs)
    and construct_seq_ : type t. t encoding -> t Seq.t -> jsonm_lexeme Seq.t =
     fun t vs -> Seq.flat_map (fun v -> construct t v) vs
    and construct_named :
        type t. string -> t encoding -> t -> jsonm_lexeme Seq.t =
     fun n t v -> `Name n ++ construct t v
    and construct_obj
        (* NOTE: we recurse on [construct_obj] (i.e., we stay in the same state
           of the same machine) for all the constructors present in [is_obj]. *) :
        type t. t encoding -> t -> jsonm_lexeme Seq.t = function
      | Obj (Req {name = n; encoding = t; _}) -> fun v -> construct_named n t v
      | Obj
          (Dft
            {name = n; equal; encoding = t; default = d; construct_default; _})
        ->
          fun v ->
            let inc_default =
              inc_field include_default_fields construct_default
            in
            if inc_default || not (equal v d) then construct_named n t v
            else Seq.empty
      | Obj (Opt {name = n; encoding = t; _}) -> (
          function None -> Seq.empty | Some v -> construct_named n t v)
      | Obj _ ->
          .
          (* asserting we have covered all Obj cases to ensure that it cannot
             go through the wildcard [_] below. *)
      | Objs (o1, o2) ->
          fun (v1, v2) -> construct_obj o1 v1 @ construct_obj o2 v2
      | Conv (ffrom, _, t, _) -> fun v -> construct_obj t (ffrom v)
      | Empty -> fun () -> Seq.empty
      | Ignore -> fun () -> Seq.empty
      | Mu {self; _} as enc -> fun v -> construct_obj (self enc) v
      | Describe {encoding = t; _} -> fun v -> construct_obj t v
      | Union cases ->
          fun v ->
            let rec do_cases = function
              | [] ->
                  invalid_arg
                    "Json_encoding.construct_seq: consequence of bad union"
              | Case {encoding; proj; _} :: rest -> (
                  match proj v with
                  | Some v -> construct_obj encoding v
                  | None -> do_cases rest)
            in
            do_cases cases
      | Custom ({write; _}, _) -> (
          fun v ->
            (* NOTE: This constructor is not in [is_obj] (because it is not
               possible to statically determine whether it always produces
               object) but it must be special-cased anyway. *)
            match write (module Json_repr.Ezjsonm) v with
            | `O kvs -> jsonm_lexeme_seq_of_ezjson_kvs kvs
            | `A _ | `Bool _ | `Float _ | `String _ | `Null ->
                invalid_arg
                  "Json_encoding.construct_seq: consequence of bad merge_objs")
      | _ ->
          (* In all other cases we raise a runtime exception. This is similar
             to the way vanilla [construct] handles recursive calls returning
             non-objects in the construction of an [Objs]. *)
          invalid_arg
            "Json_encoding.construct_seq: consequence of bad merge_objs"
    and construct_tup (* Similar to construct_obj, but for tups *) :
        type t. t encoding -> t -> jsonm_lexeme Seq.t = function
      | Tup t -> fun v -> (construct [@ocaml.tailcall]) t v
      | Tups (o1, o2) ->
          fun (v1, v2) -> construct_tup o1 v1 @ construct_tup o2 v2
      | Conv (ffrom, _, t, _) -> fun v -> construct_tup t (ffrom v)
      | Mu {self; _} as enc -> fun v -> construct_tup (self enc) v
      | Describe {encoding = t; _} -> fun v -> construct_tup t v
      | Custom ({write; _}, _) -> (
          fun v ->
            match write (module Json_repr.Ezjsonm) v with
            | `A vs -> jsonm_lexeme_seq_of_ezjson_vs vs
            | `O _ | `Bool _ | `Float _ | `String _ | `Null ->
                invalid_arg
                  "Json_encoding.construct_seq: consequence of bad merge_tups")
      | _ ->
          invalid_arg
            "Json_encoding.construct_seq: consequence of bad merge_tups"
    in
    construct enc v
end

(* Exporting the important values from [JsonmLexemeSeq] *)

let construct_seq :
    ?include_default_fields:[`Always | `Auto | `Never] ->
    't encoding ->
    't ->
    jsonm_lexeme Seq.t =
  JsonmLexemeSeq.construct_seq

let jsonm_lexeme_seq_of_ezjson = JsonmLexemeSeq.jsonm_lexeme_seq_of_ezjson
