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

(* ---- Constants ----------------------------------------------------------- *)

let max_uint8 = Binary_size.max_int `Uint8

let max_uint16 = Binary_size.max_int `Uint16

let max_uint8_l = Int32.of_int max_uint8

let max_uint16_l = Int32.of_int max_uint16

let max_uint8_L = Int64.of_int max_uint8

let max_uint16_L = Int64.of_int max_uint16

let max_uint32 = Int32.(to_int max_int)

let max_uint32_L = 0xFFFF_FFFFL

(* ---- Tags ---------------------------------------------------------------- *)

(* Ultimately compact-tags are translated to encoding-tag of which uint8 and
   uint16 are supported. Thus, we can always encode those tags in int. *)
type tag = int

let join_tags tags =
  let tag_value, tag_len =
    List.fold_left
      (fun (res, ofs) (tag_value, tag_len) ->
        (res lor (tag_value lsl ofs), ofs + tag_len))
      (0, 0)
      tags
  in
  if tag_len > 16 then
    raise @@ Invalid_argument "join_tags: total tag_len shouldn't be over 16" ;
  tag_value

(* ---- Encoding helpers ---------------------------------------------------- *)

(** [conv_partial f g encoding] is the counterpart of
    [conv_with_guard]. It allows to define an encoding which is able
    to encode only a subset of the input type.

    @raise Write_error on any attempt to encode data in the unsupported
    subset of the input type. *)
let conv_partial f g encoding =
  Encoding.conv
    (fun x ->
      match f x with
      | Some x -> x
      | None -> raise Binary_error_types.(Write_error No_case_matched))
    g
    encoding

(* ---- Compact encoding definition ----------------------------------------- *)

module type S = sig
  type input

  type layout

  val layouts : layout list

  val tag_len : int

  val tag : layout -> tag

  val title : layout -> string option

  val partial_encoding : layout -> input Encoding.t

  val classify : input -> layout

  val json_encoding : input Encoding.t
end

type 'a t = (module S with type input = 'a)

let tag_bit_count : type a. a t -> int =
 fun (module C : S with type input = a) -> C.tag_len

let make : type a. ?tag_size:[`Uint0 | `Uint8 | `Uint16] -> a t -> a Encoding.t
    =
 fun ?(tag_size = `Uint0) (module C : S with type input = a) ->
  let tag_len_limit =
    match tag_size with `Uint0 -> 0 | `Uint8 -> 8 | `Uint16 -> 16
  in

  if C.tag_len > tag_len_limit then
    raise @@ Invalid_argument "Compact_encoding.make: tags do not fit" ;

  let tag layout =
    let candidate = C.tag layout in
    if candidate >= 1 lsl C.tag_len then
      raise @@ Invalid_argument "Compact_encoding.make.tag: tags do not fit" ;
    candidate
  in

  match tag_size with
  | `Uint0 -> (
      (* INVARIANT: when [tag_len = 0] then either:
         - it's void and [layouts = []], or
         - [layouts] has a single element and [partial_encoding] is total *)
      match C.layouts with
      | [] -> C.json_encoding
      | [single_layout] -> C.partial_encoding single_layout
      | _ ->
          raise
          @@ Invalid_argument
               "Data_encoding.Compact.make: 0-tag encoding has more than one \
                layout")
  | (`Uint8 | `Uint16) as tag_size ->
      Encoding.raw_splitted
        ~json:(Json.convert C.json_encoding)
        ~binary:
          (Encoding.matching ~tag_size (fun x ->
               let layout = C.classify x in
               Encoding.matched
                 ~tag_size
                 (C.tag layout)
                 (C.partial_encoding layout)
                 x)
          @@ List.map
               (fun layout ->
                 let tag = tag layout in
                 let title =
                   match C.title layout with
                   | None -> Format.sprintf "case_%d" tag
                   | Some s -> s
                 in
                 (* Note: the projection function is never used. This is
                    because [matching] uses the list of cases for decoding
                    only, not encoding. *)
                 Encoding.case
                   ~title
                   (Encoding.Tag tag)
                   (C.partial_encoding layout)
                   (fun x -> Some x)
                   (fun x -> x))
               C.layouts)

let splitted : type a. json:a Encoding.t -> compact:a t -> a t =
 fun ~json ~compact:(module C : S with type input = a) ->
  (module struct
    include C

    let json_encoding = json
  end)

(* ---- Combinators --------------------------------------------------------- *)

module List_syntax = struct
  let ( let* ) l f = List.concat_map f l

  let return x = [x]
end

type void = |

let refute = function (_ : void) -> .

let void : void t =
  (module struct
    type input = void

    type layout = void

    let tag_len = 0

    let layouts = []

    let title = refute

    let classify = refute

    let partial_encoding = refute

    let tag = refute

    let json_encoding =
      Encoding.conv_with_guard
        refute
        (fun _ -> Error "void has no inhabitant")
        Encoding.unit
  end)

type ('a, 'b, 'layout) case_open = {
  title : string;
  description : string option;
  proj : 'a -> 'b option;
  inj : 'b -> 'a;
  compact : (module S with type input = 'b and type layout = 'layout);
}

type ('a, 'b, 'layout) case_layout_open = {
  tag : int; (* The tag which identifies this specific case out of the others *)
  title : string;
  proj : 'a -> 'b option;
  inj : 'b -> 'a;
  compact : (module S with type input = 'b and type layout = 'layout);
  layout : 'layout;
}

type 'a case = Case : ('a, 'b, 'layout) case_open -> 'a case [@@unboxed]

let case : type a b.
    title:string ->
    ?description:string ->
    b t ->
    (a -> b option) ->
    (b -> a) ->
    a case =
 fun ~title ?description compact proj inj ->
  let (module C : S with type input = b) = compact in
  Case {title; description; proj; inj; compact = (module C)}

type 'a case_layout =
  | Case_layout : ('a, 'b, 'layout) case_layout_open -> 'a case_layout
[@@unboxed]

let case_to_layout_open : type a b layout c.
    tag ->
    (a, b, layout) case_open ->
    ((a, b, layout) case_layout_open -> c) ->
    c list =
 fun tag {proj; inj; compact; title; _} f ->
  let (module C : S with type input = b and type layout = layout) = compact in
  List.map (fun layout -> f {tag; proj; inj; compact; layout; title}) C.layouts

let case_to_layout : type a. tag -> a case -> a case_layout list =
 fun tag (Case case) -> case_to_layout_open tag case (fun x -> Case_layout x)

let cases_to_layouts : type a. a case list -> a case_layout list =
 fun cases -> List.mapi (fun i -> case_to_layout i) cases |> List.concat

let classify_with_case_open : type a b layout.
    tag ->
    (a, b, layout) case_open ->
    a ->
    (a, b, layout) case_layout_open option =
 fun tag {compact; proj; inj; title; _} input ->
  let (module C : S with type input = b and type layout = layout) = compact in
  match proj input with
  | Some input' ->
      let layout = C.classify input' in
      Some {proj; inj; tag; layout; title; compact}
  | None -> None

let classify_with_case : type a. tag -> a case -> a -> a case_layout option =
 fun tag (Case case) input ->
  match classify_with_case_open tag case input with
  | Some layout -> Some (Case_layout layout)
  | None -> None

let classify_with_cases_exn : type a. (int * a case) list -> a -> a case_layout
    =
 fun icases input ->
  let rec classify_aux = function
    | [] -> raise (Invalid_argument "classify_exn")
    | (tag, case) :: rst -> (
        match classify_with_case tag case input with
        | Some layout -> layout
        | None -> classify_aux rst)
  in
  classify_aux icases

let tag_with_case_layout_open : type a b layout.
    int -> (a, b, layout) case_layout_open -> tag =
 fun inner_tag_len {tag; compact; layout; _} ->
  let (module C : S with type input = b and type layout = layout) = compact in
  (tag lsl inner_tag_len) lor C.tag layout

let tag_with_case_layout : type a. int -> a case_layout -> tag =
 fun inner_tag_len (Case_layout case) ->
  tag_with_case_layout_open inner_tag_len case

let title_with_case_layout_open : type a b layout.
    (a, b, layout) case_layout_open -> string =
 fun {title; _} -> title

let title_with_case_layout : type a. a case_layout -> string =
 fun (Case_layout case) -> title_with_case_layout_open case

let tag_len_of_case_open : type a b layout. (a, b, layout) case_open -> int =
 fun {compact; _} ->
  let (module C : S with type input = b and type layout = layout) = compact in
  C.tag_len

let tag_len_of_case : type a. a case -> int =
 fun (Case case) -> tag_len_of_case_open case

let partial_encoding_of_case_layout_open : type a b layout.
    (a, b, layout) case_layout_open -> a Encoding.t =
 fun {proj; inj; compact; layout; _} ->
  let (module C : S with type input = b and type layout = layout) = compact in
  (* TODO: introduce a [def] combinator. Problem: needs an [id]. *)
  conv_partial proj inj @@ C.partial_encoding layout

let partial_encoding_of_case_layout : type a. a case_layout -> a Encoding.t =
 fun (Case_layout layout) -> partial_encoding_of_case_layout_open layout

let case_to_json_data_encoding_case_open : type a b layout.
    (a, b, layout) case_open -> a Encoding.case =
 fun {title; description; proj; inj; compact} ->
  let (module C : S with type input = b and type layout = layout) = compact in
  Encoding.case ~title ?description Encoding.Json_only C.json_encoding proj inj

let case_to_json_data_encoding_case : type a. a case -> a Encoding.case =
 fun (Case layout) -> case_to_json_data_encoding_case_open layout

let void_case : type a. title:string -> a case =
 fun ~title ->
  case
    ~title
    ~description:"This case is void. No data is accepted."
    void
    (fun _ -> None)
    refute

let is_void_case : type a. a case -> bool =
 fun (Case {compact; _}) -> Obj.repr compact == Obj.repr void

let union_bits title min = function
  | Some choice when min <= choice -> choice
  | None -> min
  | Some _ ->
      raise
        (Invalid_argument (Format.sprintf "union: not enough %s bits" title))

let union : type a.
    ?union_tag_bits:int -> ?cases_tag_bits:int -> a case list -> a t =
 fun ?union_tag_bits ?cases_tag_bits cases ->
  if cases = [] then
    raise
    @@ Invalid_argument "Data_encoding.Compact.union: empty list of cases." ;
  (module struct
    type input = a

    (* [union_tag_len] is the number of bits introduced by [union] to
       distinguish between cases, while [inner_tag] is the greatest
       number of bits used by the cases themselves. *)
    let union_tag_len, cases_tag_len =
      let min_union, min_cases =
        match cases with
        | [] -> assert false
        | case :: rst ->
            List.fold_left
              (fun (bound, size, acc_extra, acc_len) case ->
                let size = 1 + size in
                let acc_len = max acc_len (tag_len_of_case case) in
                if bound < size then (2 * bound, size, acc_extra + 1, acc_len)
                else (bound, size, acc_extra, acc_len))
              (1, 1, 0, tag_len_of_case case)
              rst
            |> fun (_, _, extra, len) -> (extra, len)
      in
      ( union_bits "tag" min_union union_tag_bits,
        union_bits "inner" min_cases cases_tag_bits )

    let tag_len =
      let r = union_tag_len + cases_tag_len in
      if r >= 16 then
        raise @@ Invalid_argument "Compact_encoding.union: tags do not fit" ;
      r

    type layout = a case_layout

    let layouts = cases_to_layouts cases

    let classify =
      let cleaned_cases =
        let rec aux acc idx = function
          | [] -> List.rev acc
          | case :: cases ->
              if is_void_case case then aux acc (idx + 1) cases
              else aux ((idx, case) :: acc) (idx + 1) cases
        in
        aux [] 0 cases
      in
      classify_with_cases_exn cleaned_cases

    let partial_encoding = partial_encoding_of_case_layout

    let tag layout = tag_with_case_layout cases_tag_len layout

    let title layout = Some (title_with_case_layout layout)

    let json_encoding : input Encoding.t =
      Encoding.union @@ List.map case_to_json_data_encoding_case cases
  end)

let payload : type a. a Encoding.t -> a t =
 fun encoding : (module S with type input = a) ->
  (module struct
    type input = a

    type layout = unit

    let layouts = [()]

    let tag_len = 0

    let tag () = 0

    let title () = None

    let classify (_ : input) = ()

    let partial_encoding () = encoding

    let json_encoding = encoding
  end)

let unit = payload Encoding.unit

let null = payload Encoding.null

let conv : type a b. ?json:a Encoding.t -> (a -> b) -> (b -> a) -> b t -> a t =
 fun ?json f g (module B : S with type input = b) ->
  (module struct
    type input = a

    type layout = B.layout

    let layouts = B.layouts

    let tag_len = B.tag_len

    let tag = B.tag

    let title = B.title

    let classify b = B.classify (f b)

    let partial_encoding l = Encoding.conv f g (B.partial_encoding l)

    let json_encoding =
      match json with
      | None -> Encoding.conv f g B.json_encoding
      | Some encoding -> encoding
  end)

let option compact =
  union
    ~union_tag_bits:1
    [
      case
        ~title:"none"
        null
        (function None -> Some () | _ -> None)
        (fun () -> None);
      case ~title:"some" compact (fun x -> x) (fun x -> Some x);
    ]

let tup1 : type a. a t -> a t =
 fun (module A : S with type input = a) : (module S with type input = a) ->
  (module struct
    type input = A.input

    type layout = A.layout

    let tag_len = A.tag_len

    let layouts = A.layouts

    let classify a = A.classify a

    let partial_encoding la = Encoding.tup1 (A.partial_encoding la)

    let tag a = A.tag a

    let title = A.title

    let json_encoding = Encoding.tup1 A.json_encoding
  end)

let tup2 : type a b. a t -> b t -> (a * b) t =
 fun (module A : S with type input = a)
     (module B : S with type input = b)
     :
     (module S with type input = a * b) ->
  (module struct
    type input = A.input * B.input

    type layout = A.layout * B.layout

    let tag_len =
      let r = A.tag_len + B.tag_len in
      if r >= 16 then
        raise @@ Invalid_argument "Compact_encoding.tup2: tags do not fit" ;
      r

    let layouts =
      let open List_syntax in
      let* a = A.layouts in
      let* b = B.layouts in
      return (a, b)

    let classify (a, b) = (A.classify a, B.classify b)

    let partial_encoding (la, lb) =
      Encoding.tup2 (A.partial_encoding la) (B.partial_encoding lb)

    let tag (a, b) = join_tags [(A.tag a, A.tag_len); (B.tag b, B.tag_len)]

    let title _ = None

    let json_encoding = Encoding.tup2 A.json_encoding B.json_encoding
  end)

let tup3 : type a b c. a t -> b t -> c t -> (a * b * c) t =
 fun (module A : S with type input = a)
     (module B : S with type input = b)
     (module C : S with type input = c)
     :
     (module S with type input = a * b * c) ->
  (module struct
    type input = A.input * B.input * C.input

    type layout = A.layout * B.layout * C.layout

    let tag_len =
      let r = A.tag_len + B.tag_len + C.tag_len in
      if r >= 16 then
        raise @@ Invalid_argument "Compact_encoding.tup3: tags do not fit" ;
      r

    let layouts =
      let open List_syntax in
      let* a = A.layouts in
      let* b = B.layouts in
      let* c = C.layouts in
      return (a, b, c)

    let classify (a, b, c) = (A.classify a, B.classify b, C.classify c)

    let partial_encoding (la, lb, lc) =
      Encoding.tup3
        (A.partial_encoding la)
        (B.partial_encoding lb)
        (C.partial_encoding lc)

    let tag (a, b, c) =
      join_tags
        [(A.tag a, A.tag_len); (B.tag b, B.tag_len); (C.tag c, C.tag_len)]

    let title _ = None

    let json_encoding =
      Encoding.tup3 A.json_encoding B.json_encoding C.json_encoding
  end)

let tup4 : type a b c d. a t -> b t -> c t -> d t -> (a * b * c * d) t =
 fun (module A : S with type input = a)
     (module B : S with type input = b)
     (module C : S with type input = c)
     (module D : S with type input = d)
     :
     (module S with type input = a * b * c * d) ->
  (module struct
    type input = A.input * B.input * C.input * D.input

    type layout = A.layout * B.layout * C.layout * D.layout

    let tag_len =
      let r = A.tag_len + B.tag_len + C.tag_len + D.tag_len in
      if r >= 16 then
        raise @@ Invalid_argument "Compact_encoding.tup4: tags do not fit" ;
      r

    let layouts =
      let open List_syntax in
      let* a = A.layouts in
      let* b = B.layouts in
      let* c = C.layouts in
      let* d = D.layouts in
      return (a, b, c, d)

    let classify (a, b, c, d) =
      (A.classify a, B.classify b, C.classify c, D.classify d)

    let partial_encoding (la, lb, lc, ld) =
      Encoding.tup4
        (A.partial_encoding la)
        (B.partial_encoding lb)
        (C.partial_encoding lc)
        (D.partial_encoding ld)

    let tag (a, b, c, d) =
      join_tags
        [
          (A.tag a, A.tag_len);
          (B.tag b, B.tag_len);
          (C.tag c, C.tag_len);
          (D.tag d, D.tag_len);
        ]

    let title _ = None

    let json_encoding =
      Encoding.tup4
        A.json_encoding
        B.json_encoding
        C.json_encoding
        D.json_encoding
  end)

let tup5 : type a b c d e.
    a t -> b t -> c t -> d t -> e t -> (a * b * c * d * e) t =
 fun (module A : S with type input = a)
     (module B : S with type input = b)
     (module C : S with type input = c)
     (module D : S with type input = d)
     (module E : S with type input = e)
     :
     (module S with type input = a * b * c * d * e) ->
  (module struct
    type input = A.input * B.input * C.input * D.input * E.input

    type layout = A.layout * B.layout * C.layout * D.layout * E.layout

    let tag_len =
      let r = A.tag_len + B.tag_len + C.tag_len + D.tag_len + E.tag_len in
      if r >= 16 then
        raise @@ Invalid_argument "Compact_encoding.tup5: tags do not fit" ;
      r

    let layouts =
      let open List_syntax in
      let* a = A.layouts in
      let* b = B.layouts in
      let* c = C.layouts in
      let* d = D.layouts in
      let* e = E.layouts in
      return (a, b, c, d, e)

    let classify (a, b, c, d, e) =
      (A.classify a, B.classify b, C.classify c, D.classify d, E.classify e)

    let partial_encoding (la, lb, lc, ld, le) =
      Encoding.tup5
        (A.partial_encoding la)
        (B.partial_encoding lb)
        (C.partial_encoding lc)
        (D.partial_encoding ld)
        (E.partial_encoding le)

    let tag (a, b, c, d, e) =
      join_tags
        [
          (A.tag a, A.tag_len);
          (B.tag b, B.tag_len);
          (C.tag c, C.tag_len);
          (D.tag d, D.tag_len);
          (E.tag e, E.tag_len);
        ]

    let title _ = None

    let json_encoding =
      Encoding.tup5
        A.json_encoding
        B.json_encoding
        C.json_encoding
        D.json_encoding
        E.json_encoding
  end)

let tup6 : type a b c d e f.
    a t -> b t -> c t -> d t -> e t -> f t -> (a * b * c * d * e * f) t =
 fun (module A : S with type input = a)
     (module B : S with type input = b)
     (module C : S with type input = c)
     (module D : S with type input = d)
     (module E : S with type input = e)
     (module F : S with type input = f)
     :
     (module S with type input = a * b * c * d * e * f) ->
  (module struct
    type input = A.input * B.input * C.input * D.input * E.input * F.input

    type layout =
      A.layout * B.layout * C.layout * D.layout * E.layout * F.layout

    let tag_len =
      let r =
        A.tag_len + B.tag_len + C.tag_len + D.tag_len + E.tag_len + F.tag_len
      in
      if r >= 16 then
        raise @@ Invalid_argument "Compact_encoding.tup6: tags do not fit" ;
      r

    let layouts =
      let open List_syntax in
      let* a = A.layouts in
      let* b = B.layouts in
      let* c = C.layouts in
      let* d = D.layouts in
      let* e = E.layouts in
      let* f = F.layouts in
      return (a, b, c, d, e, f)

    let classify (a, b, c, d, e, f) =
      ( A.classify a,
        B.classify b,
        C.classify c,
        D.classify d,
        E.classify e,
        F.classify f )

    let partial_encoding (la, lb, lc, ld, le, lf) =
      Encoding.tup6
        (A.partial_encoding la)
        (B.partial_encoding lb)
        (C.partial_encoding lc)
        (D.partial_encoding ld)
        (E.partial_encoding le)
        (F.partial_encoding lf)

    let tag (a, b, c, d, e, f) =
      join_tags
        [
          (A.tag a, A.tag_len);
          (B.tag b, B.tag_len);
          (C.tag c, C.tag_len);
          (D.tag d, D.tag_len);
          (E.tag e, E.tag_len);
          (F.tag f, F.tag_len);
        ]

    let title _ = None

    let json_encoding =
      Encoding.tup6
        A.json_encoding
        B.json_encoding
        C.json_encoding
        D.json_encoding
        E.json_encoding
        F.json_encoding
  end)

let tup7 : type a b c d e f g.
    a t ->
    b t ->
    c t ->
    d t ->
    e t ->
    f t ->
    g t ->
    (a * b * c * d * e * f * g) t =
 fun (module A : S with type input = a)
     (module B : S with type input = b)
     (module C : S with type input = c)
     (module D : S with type input = d)
     (module E : S with type input = e)
     (module F : S with type input = f)
     (module G : S with type input = g)
     :
     (module S with type input = a * b * c * d * e * f * g) ->
  (module struct
    type input =
      A.input * B.input * C.input * D.input * E.input * F.input * G.input

    type layout =
      A.layout * B.layout * C.layout * D.layout * E.layout * F.layout * G.layout

    let tag_len =
      let r =
        A.tag_len + B.tag_len + C.tag_len + D.tag_len + E.tag_len + F.tag_len
        + G.tag_len
      in
      if r >= 16 then
        raise @@ Invalid_argument "Compact_encoding.tup7: tags do not fit" ;
      r

    let layouts =
      let open List_syntax in
      let* a = A.layouts in
      let* b = B.layouts in
      let* c = C.layouts in
      let* d = D.layouts in
      let* e = E.layouts in
      let* f = F.layouts in
      let* g = G.layouts in
      return (a, b, c, d, e, f, g)

    let classify (a, b, c, d, e, f, g) =
      ( A.classify a,
        B.classify b,
        C.classify c,
        D.classify d,
        E.classify e,
        F.classify f,
        G.classify g )

    let partial_encoding (la, lb, lc, ld, le, lf, lg) =
      Encoding.tup7
        (A.partial_encoding la)
        (B.partial_encoding lb)
        (C.partial_encoding lc)
        (D.partial_encoding ld)
        (E.partial_encoding le)
        (F.partial_encoding lf)
        (G.partial_encoding lg)

    let tag (a, b, c, d, e, f, g) =
      join_tags
        [
          (A.tag a, A.tag_len);
          (B.tag b, B.tag_len);
          (C.tag c, C.tag_len);
          (D.tag d, D.tag_len);
          (E.tag e, E.tag_len);
          (F.tag f, F.tag_len);
          (G.tag g, G.tag_len);
        ]

    let title _ = None

    let json_encoding =
      Encoding.tup7
        A.json_encoding
        B.json_encoding
        C.json_encoding
        D.json_encoding
        E.json_encoding
        F.json_encoding
        G.json_encoding
  end)

let tup8 : type a b c d e f g h.
    a t ->
    b t ->
    c t ->
    d t ->
    e t ->
    f t ->
    g t ->
    h t ->
    (a * b * c * d * e * f * g * h) t =
 fun (module A : S with type input = a)
     (module B : S with type input = b)
     (module C : S with type input = c)
     (module D : S with type input = d)
     (module E : S with type input = e)
     (module F : S with type input = f)
     (module G : S with type input = g)
     (module H : S with type input = h)
     :
     (module S with type input = a * b * c * d * e * f * g * h) ->
  (module struct
    type input =
      A.input
      * B.input
      * C.input
      * D.input
      * E.input
      * F.input
      * G.input
      * H.input

    type layout =
      A.layout
      * B.layout
      * C.layout
      * D.layout
      * E.layout
      * F.layout
      * G.layout
      * H.layout

    let tag_len =
      let r =
        A.tag_len + B.tag_len + C.tag_len + D.tag_len + E.tag_len + F.tag_len
        + G.tag_len + H.tag_len
      in
      if r >= 16 then
        raise @@ Invalid_argument "Compact_encoding.tup8: tags do not fit" ;
      r

    let layouts =
      let open List_syntax in
      let* a = A.layouts in
      let* b = B.layouts in
      let* c = C.layouts in
      let* d = D.layouts in
      let* e = E.layouts in
      let* f = F.layouts in
      let* g = G.layouts in
      let* h = H.layouts in
      return (a, b, c, d, e, f, g, h)

    let classify (a, b, c, d, e, f, g, h) =
      ( A.classify a,
        B.classify b,
        C.classify c,
        D.classify d,
        E.classify e,
        F.classify f,
        G.classify g,
        H.classify h )

    let partial_encoding (la, lb, lc, ld, le, lf, lg, lh) =
      Encoding.tup8
        (A.partial_encoding la)
        (B.partial_encoding lb)
        (C.partial_encoding lc)
        (D.partial_encoding ld)
        (E.partial_encoding le)
        (F.partial_encoding lf)
        (G.partial_encoding lg)
        (H.partial_encoding lh)

    let tag (a, b, c, d, e, f, g, h) =
      join_tags
        [
          (A.tag a, A.tag_len);
          (B.tag b, B.tag_len);
          (C.tag c, C.tag_len);
          (D.tag d, D.tag_len);
          (E.tag e, E.tag_len);
          (F.tag f, F.tag_len);
          (G.tag g, G.tag_len);
          (H.tag h, H.tag_len);
        ]

    let title _ = None

    let json_encoding =
      Encoding.tup8
        A.json_encoding
        B.json_encoding
        C.json_encoding
        D.json_encoding
        E.json_encoding
        F.json_encoding
        G.json_encoding
        H.json_encoding
  end)

let tup9 : type a b c d e f g h i.
    a t ->
    b t ->
    c t ->
    d t ->
    e t ->
    f t ->
    g t ->
    h t ->
    i t ->
    (a * b * c * d * e * f * g * h * i) t =
 fun (module A : S with type input = a)
     (module B : S with type input = b)
     (module C : S with type input = c)
     (module D : S with type input = d)
     (module E : S with type input = e)
     (module F : S with type input = f)
     (module G : S with type input = g)
     (module H : S with type input = h)
     (module I : S with type input = i)
     :
     (module S with type input = a * b * c * d * e * f * g * h * i) ->
  (module struct
    type input =
      A.input
      * B.input
      * C.input
      * D.input
      * E.input
      * F.input
      * G.input
      * H.input
      * I.input

    type layout =
      A.layout
      * B.layout
      * C.layout
      * D.layout
      * E.layout
      * F.layout
      * G.layout
      * H.layout
      * I.layout

    let tag_len =
      let r =
        A.tag_len + B.tag_len + C.tag_len + D.tag_len + E.tag_len + F.tag_len
        + G.tag_len + H.tag_len + I.tag_len
      in
      if r >= 16 then
        raise @@ Invalid_argument "Compact_encoding.tup9: tags do not fit" ;
      r

    let layouts =
      let open List_syntax in
      let* a = A.layouts in
      let* b = B.layouts in
      let* c = C.layouts in
      let* d = D.layouts in
      let* e = E.layouts in
      let* f = F.layouts in
      let* g = G.layouts in
      let* h = H.layouts in
      let* i = I.layouts in
      return (a, b, c, d, e, f, g, h, i)

    let classify (a, b, c, d, e, f, g, h, i) =
      ( A.classify a,
        B.classify b,
        C.classify c,
        D.classify d,
        E.classify e,
        F.classify f,
        G.classify g,
        H.classify h,
        I.classify i )

    let partial_encoding (la, lb, lc, ld, le, lf, lg, lh, li) =
      Encoding.tup9
        (A.partial_encoding la)
        (B.partial_encoding lb)
        (C.partial_encoding lc)
        (D.partial_encoding ld)
        (E.partial_encoding le)
        (F.partial_encoding lf)
        (G.partial_encoding lg)
        (H.partial_encoding lh)
        (I.partial_encoding li)

    let tag (a, b, c, d, e, f, g, h, i) =
      join_tags
        [
          (A.tag a, A.tag_len);
          (B.tag b, B.tag_len);
          (C.tag c, C.tag_len);
          (D.tag d, D.tag_len);
          (E.tag e, E.tag_len);
          (F.tag f, F.tag_len);
          (G.tag g, G.tag_len);
          (H.tag h, H.tag_len);
          (I.tag i, I.tag_len);
        ]

    let title _ = None

    let json_encoding =
      Encoding.tup9
        A.json_encoding
        B.json_encoding
        C.json_encoding
        D.json_encoding
        E.json_encoding
        F.json_encoding
        G.json_encoding
        H.json_encoding
        I.json_encoding
  end)

let tup10 : type a b c d e f g h i j.
    a t ->
    b t ->
    c t ->
    d t ->
    e t ->
    f t ->
    g t ->
    h t ->
    i t ->
    j t ->
    (a * b * c * d * e * f * g * h * i * j) t =
 fun (module A : S with type input = a)
     (module B : S with type input = b)
     (module C : S with type input = c)
     (module D : S with type input = d)
     (module E : S with type input = e)
     (module F : S with type input = f)
     (module G : S with type input = g)
     (module H : S with type input = h)
     (module I : S with type input = i)
     (module J : S with type input = j)
     :
     (module S with type input = a * b * c * d * e * f * g * h * i * j) ->
  (module struct
    type input =
      A.input
      * B.input
      * C.input
      * D.input
      * E.input
      * F.input
      * G.input
      * H.input
      * I.input
      * J.input

    type layout =
      A.layout
      * B.layout
      * C.layout
      * D.layout
      * E.layout
      * F.layout
      * G.layout
      * H.layout
      * I.layout
      * J.layout

    let tag_len =
      let r =
        A.tag_len + B.tag_len + C.tag_len + D.tag_len + E.tag_len + F.tag_len
        + G.tag_len + H.tag_len + I.tag_len + J.tag_len
      in
      if r >= 16 then
        raise @@ Invalid_argument "Compact_encoding.tup10: tags do not fit" ;
      r

    let layouts =
      let open List_syntax in
      let* a = A.layouts in
      let* b = B.layouts in
      let* c = C.layouts in
      let* d = D.layouts in
      let* e = E.layouts in
      let* f = F.layouts in
      let* g = G.layouts in
      let* h = H.layouts in
      let* i = I.layouts in
      let* j = J.layouts in
      return (a, b, c, d, e, f, g, h, i, j)

    let classify (a, b, c, d, e, f, g, h, i, j) =
      ( A.classify a,
        B.classify b,
        C.classify c,
        D.classify d,
        E.classify e,
        F.classify f,
        G.classify g,
        H.classify h,
        I.classify i,
        J.classify j )

    let partial_encoding (la, lb, lc, ld, le, lf, lg, lh, li, lj) =
      Encoding.tup10
        (A.partial_encoding la)
        (B.partial_encoding lb)
        (C.partial_encoding lc)
        (D.partial_encoding ld)
        (E.partial_encoding le)
        (F.partial_encoding lf)
        (G.partial_encoding lg)
        (H.partial_encoding lh)
        (I.partial_encoding li)
        (J.partial_encoding lj)

    let tag (a, b, c, d, e, f, g, h, i, j) =
      join_tags
        [
          (A.tag a, A.tag_len);
          (B.tag b, B.tag_len);
          (C.tag c, C.tag_len);
          (D.tag d, D.tag_len);
          (E.tag e, E.tag_len);
          (F.tag f, F.tag_len);
          (G.tag g, G.tag_len);
          (H.tag h, H.tag_len);
          (I.tag i, I.tag_len);
          (J.tag j, J.tag_len);
        ]

    let title _ = None

    let json_encoding =
      Encoding.tup10
        A.json_encoding
        B.json_encoding
        C.json_encoding
        D.json_encoding
        E.json_encoding
        F.json_encoding
        G.json_encoding
        H.json_encoding
        I.json_encoding
        J.json_encoding
  end)

type 'a field_contents = {name : string; compact : 'a t}

type ('a, 'b) field_open =
  | Req : 'a field_contents -> ('a, 'a) field_open
  | Opt : 'a field_contents -> ('a, 'a option) field_open

let field_to_compact_open : type a b. (a, b) field_open -> a t = function
  | Req f1 -> f1.compact
  | Opt f1 -> f1.compact

let field_to_inner_compact : type a b. (a, b) field_open -> b t = function
  | Req f1 -> f1.compact
  | Opt f1 -> option f1.compact

type 'a field = Field : ('b, 'a) field_open -> 'a field [@@unboxed]

let field_to_data_encoding_open : type a b.
    (a, b) field_open -> b Encoding.field = function
  | Req {name; compact} ->
      let (module A) = compact in
      Encoding.req name A.json_encoding
  | Opt {name; compact} ->
      let (module A) = compact in
      Encoding.opt name A.json_encoding

let req : string -> 'a t -> 'a field =
 fun name compact -> Field (Req {name; compact})

let opt : string -> 'a t -> 'a option field =
 fun name compact -> Field (Opt {name; compact})

let obj1_open : type a b. (a, b) field_open -> (module S with type input = b) =
 fun f1 ->
  let (module C) = field_to_compact_open f1 in
  let (module C_in) = field_to_inner_compact f1 in
  (module struct
    include C_in

    let json_encoding = Encoding.obj1 @@ field_to_data_encoding_open f1
  end)

let obj1 (Field f1) = obj1_open f1

let obj2_open : type a b c d.
    (a, b) field_open -> (c, d) field_open -> (module S with type input = b * d)
    =
 fun f1 f2 ->
  let (module Tup) =
    tup2 (field_to_inner_compact f1) (field_to_inner_compact f2)
  in
  (module struct
    include Tup

    let json_encoding =
      Encoding.obj2
        (field_to_data_encoding_open f1)
        (field_to_data_encoding_open f2)
  end)

let obj2 (Field f1) (Field f2) = obj2_open f1 f2

let obj3_open : type a b c d e f.
    (a, b) field_open ->
    (c, d) field_open ->
    (e, f) field_open ->
    (module S with type input = b * d * f) =
 fun f1 f2 f3 ->
  let (module Tup) =
    tup3
      (field_to_inner_compact f1)
      (field_to_inner_compact f2)
      (field_to_inner_compact f3)
  in
  (module struct
    include Tup

    let json_encoding =
      Encoding.obj3
        (field_to_data_encoding_open f1)
        (field_to_data_encoding_open f2)
        (field_to_data_encoding_open f3)
  end)

let obj3 (Field f1) (Field f2) (Field f3) = obj3_open f1 f2 f3

let obj4_open : type a b c d e f g h.
    (a, b) field_open ->
    (c, d) field_open ->
    (e, f) field_open ->
    (g, h) field_open ->
    (module S with type input = b * d * f * h) =
 fun f1 f2 f3 f4 ->
  let (module Tup) =
    tup4
      (field_to_inner_compact f1)
      (field_to_inner_compact f2)
      (field_to_inner_compact f3)
      (field_to_inner_compact f4)
  in
  (module struct
    include Tup

    let json_encoding =
      Encoding.obj4
        (field_to_data_encoding_open f1)
        (field_to_data_encoding_open f2)
        (field_to_data_encoding_open f3)
        (field_to_data_encoding_open f4)
  end)

let obj4 (Field f1) (Field f2) (Field f3) (Field f4) = obj4_open f1 f2 f3 f4

let obj5_open : type t1a t1b t2a t2b t3a t3b t4a t4b t5a t5b.
    (t1a, t1b) field_open ->
    (t2a, t2b) field_open ->
    (t3a, t3b) field_open ->
    (t4a, t4b) field_open ->
    (t5a, t5b) field_open ->
    (module S with type input = t1b * t2b * t3b * t4b * t5b) =
 fun f1 f2 f3 f4 f5 ->
  let (module Tup) =
    tup5
      (field_to_inner_compact f1)
      (field_to_inner_compact f2)
      (field_to_inner_compact f3)
      (field_to_inner_compact f4)
      (field_to_inner_compact f5)
  in
  (module struct
    include Tup

    let json_encoding =
      Encoding.obj5
        (field_to_data_encoding_open f1)
        (field_to_data_encoding_open f2)
        (field_to_data_encoding_open f3)
        (field_to_data_encoding_open f4)
        (field_to_data_encoding_open f5)
  end)

let obj5 (Field f1) (Field f2) (Field f3) (Field f4) (Field f5) =
  obj5_open f1 f2 f3 f4 f5

let obj6_open : type t1a t1b t2a t2b t3a t3b t4a t4b t5a t5b t6a t6b.
    (t1a, t1b) field_open ->
    (t2a, t2b) field_open ->
    (t3a, t3b) field_open ->
    (t4a, t4b) field_open ->
    (t5a, t5b) field_open ->
    (t6a, t6b) field_open ->
    (module S with type input = t1b * t2b * t3b * t4b * t5b * t6b) =
 fun f1 f2 f3 f4 f5 f6 ->
  let (module Tup) =
    tup6
      (field_to_inner_compact f1)
      (field_to_inner_compact f2)
      (field_to_inner_compact f3)
      (field_to_inner_compact f4)
      (field_to_inner_compact f5)
      (field_to_inner_compact f6)
  in
  (module struct
    include Tup

    let json_encoding =
      Encoding.obj6
        (field_to_data_encoding_open f1)
        (field_to_data_encoding_open f2)
        (field_to_data_encoding_open f3)
        (field_to_data_encoding_open f4)
        (field_to_data_encoding_open f5)
        (field_to_data_encoding_open f6)
  end)

let obj6 (Field f1) (Field f2) (Field f3) (Field f4) (Field f5) (Field f6) =
  obj6_open f1 f2 f3 f4 f5 f6

let obj7_open : type t1a t1b t2a t2b t3a t3b t4a t4b t5a t5b t6a t6b t7a t7b.
    (t1a, t1b) field_open ->
    (t2a, t2b) field_open ->
    (t3a, t3b) field_open ->
    (t4a, t4b) field_open ->
    (t5a, t5b) field_open ->
    (t6a, t6b) field_open ->
    (t7a, t7b) field_open ->
    (module S with type input = t1b * t2b * t3b * t4b * t5b * t6b * t7b) =
 fun f1 f2 f3 f4 f5 f6 f7 ->
  let (module Tup) =
    tup7
      (field_to_inner_compact f1)
      (field_to_inner_compact f2)
      (field_to_inner_compact f3)
      (field_to_inner_compact f4)
      (field_to_inner_compact f5)
      (field_to_inner_compact f6)
      (field_to_inner_compact f7)
  in
  (module struct
    include Tup

    let json_encoding =
      Encoding.obj7
        (field_to_data_encoding_open f1)
        (field_to_data_encoding_open f2)
        (field_to_data_encoding_open f3)
        (field_to_data_encoding_open f4)
        (field_to_data_encoding_open f5)
        (field_to_data_encoding_open f6)
        (field_to_data_encoding_open f7)
  end)

let obj7 (Field f1) (Field f2) (Field f3) (Field f4) (Field f5) (Field f6)
    (Field f7) =
  obj7_open f1 f2 f3 f4 f5 f6 f7

let obj8_open :
    type t1a t1b t2a t2b t3a t3b t4a t4b t5a t5b t6a t6b t7a t7b t8a t8b.
    (t1a, t1b) field_open ->
    (t2a, t2b) field_open ->
    (t3a, t3b) field_open ->
    (t4a, t4b) field_open ->
    (t5a, t5b) field_open ->
    (t6a, t6b) field_open ->
    (t7a, t7b) field_open ->
    (t8a, t8b) field_open ->
    (module S with type input = t1b * t2b * t3b * t4b * t5b * t6b * t7b * t8b) =
 fun f1 f2 f3 f4 f5 f6 f7 f8 ->
  let (module Tup) =
    tup8
      (field_to_inner_compact f1)
      (field_to_inner_compact f2)
      (field_to_inner_compact f3)
      (field_to_inner_compact f4)
      (field_to_inner_compact f5)
      (field_to_inner_compact f6)
      (field_to_inner_compact f7)
      (field_to_inner_compact f8)
  in
  (module struct
    include Tup

    let json_encoding =
      Encoding.obj8
        (field_to_data_encoding_open f1)
        (field_to_data_encoding_open f2)
        (field_to_data_encoding_open f3)
        (field_to_data_encoding_open f4)
        (field_to_data_encoding_open f5)
        (field_to_data_encoding_open f6)
        (field_to_data_encoding_open f7)
        (field_to_data_encoding_open f8)
  end)

let obj8 (Field f1) (Field f2) (Field f3) (Field f4) (Field f5) (Field f6)
    (Field f7) (Field f8) =
  obj8_open f1 f2 f3 f4 f5 f6 f7 f8

let obj9_open :
    type t1a t1b t2a t2b t3a t3b t4a t4b t5a t5b t6a t6b t7a t7b t8a t8b t9a t9b.
    (t1a, t1b) field_open ->
    (t2a, t2b) field_open ->
    (t3a, t3b) field_open ->
    (t4a, t4b) field_open ->
    (t5a, t5b) field_open ->
    (t6a, t6b) field_open ->
    (t7a, t7b) field_open ->
    (t8a, t8b) field_open ->
    (t9a, t9b) field_open ->
    (module S
       with type input = t1b * t2b * t3b * t4b * t5b * t6b * t7b * t8b * t9b) =
 fun f1 f2 f3 f4 f5 f6 f7 f8 f9 ->
  let (module Tup) =
    tup9
      (field_to_inner_compact f1)
      (field_to_inner_compact f2)
      (field_to_inner_compact f3)
      (field_to_inner_compact f4)
      (field_to_inner_compact f5)
      (field_to_inner_compact f6)
      (field_to_inner_compact f7)
      (field_to_inner_compact f8)
      (field_to_inner_compact f9)
  in
  (module struct
    include Tup

    let json_encoding =
      Encoding.obj9
        (field_to_data_encoding_open f1)
        (field_to_data_encoding_open f2)
        (field_to_data_encoding_open f3)
        (field_to_data_encoding_open f4)
        (field_to_data_encoding_open f5)
        (field_to_data_encoding_open f6)
        (field_to_data_encoding_open f7)
        (field_to_data_encoding_open f8)
        (field_to_data_encoding_open f9)
  end)

let obj9 (Field f1) (Field f2) (Field f3) (Field f4) (Field f5) (Field f6)
    (Field f7) (Field f8) (Field f9) =
  obj9_open f1 f2 f3 f4 f5 f6 f7 f8 f9

let obj10_open :
    type t1a t1b t2a t2b t3a t3b t4a t4b t5a t5b t6a t6b t7a t7b t8a t8b t9a t9b t10a t10b.
    (t1a, t1b) field_open ->
    (t2a, t2b) field_open ->
    (t3a, t3b) field_open ->
    (t4a, t4b) field_open ->
    (t5a, t5b) field_open ->
    (t6a, t6b) field_open ->
    (t7a, t7b) field_open ->
    (t8a, t8b) field_open ->
    (t9a, t9b) field_open ->
    (t10a, t10b) field_open ->
    (module S
       with type input =
         t1b * t2b * t3b * t4b * t5b * t6b * t7b * t8b * t9b * t10b) =
 fun f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 ->
  let (module Tup) =
    tup10
      (field_to_inner_compact f1)
      (field_to_inner_compact f2)
      (field_to_inner_compact f3)
      (field_to_inner_compact f4)
      (field_to_inner_compact f5)
      (field_to_inner_compact f6)
      (field_to_inner_compact f7)
      (field_to_inner_compact f8)
      (field_to_inner_compact f9)
      (field_to_inner_compact f10)
  in
  (module struct
    include Tup

    let json_encoding =
      Encoding.obj10
        (field_to_data_encoding_open f1)
        (field_to_data_encoding_open f2)
        (field_to_data_encoding_open f3)
        (field_to_data_encoding_open f4)
        (field_to_data_encoding_open f5)
        (field_to_data_encoding_open f6)
        (field_to_data_encoding_open f7)
        (field_to_data_encoding_open f8)
        (field_to_data_encoding_open f9)
        (field_to_data_encoding_open f10)
  end)

let obj10 (Field f1) (Field f2) (Field f3) (Field f4) (Field f5) (Field f6)
    (Field f7) (Field f8) (Field f9) (Field f10) =
  obj10_open f1 f2 f3 f4 f5 f6 f7 f8 f9 f10

module Compact_bool = struct
  type input = bool

  type layout = bool

  let layouts = [true; false]

  let tag_len = 1

  let tag = function true -> 1 | false -> 0

  let title _ = None

  let partial_encoding : layout -> bool Encoding.t =
   fun b ->
    conv_partial
      (function b' when Bool.equal b b' -> Some () | _ -> None)
      (fun () -> b)
      Encoding.unit

  let classify x = x

  let json_encoding = Encoding.bool
end

let bool : bool t = (module Compact_bool)

let int32_cases =
  [
    case
      ~title:"small"
      ~description:"An int32 which fits within a uint8"
      (payload Encoding.uint8)
      (fun i ->
        if 0l <= i && i <= max_uint8_l then Some (Int32.to_int i) else None)
      (fun i -> Int32.of_int i);
    case
      ~title:"medium"
      ~description:"An int32 which fits within a uint16"
      (payload Encoding.uint16)
      (fun i ->
        if max_uint8_l < i && i <= max_uint16_l then Some (Int32.to_int i)
        else None)
      (fun i ->
        if i <= max_uint8 then
          raise
            (Binary_error_types.Read_error
               (Invalid_int {min = max_uint8 + 1; v = i; max = max_uint16})) ;
        let r = Int32.of_int i in
        r);
    case
      ~title:"big"
      ~description:"An int32 which doesn't fit within a uint16"
      (payload Encoding.int32)
      (fun i -> if max_uint16_l < i || i < 0l then Some i else None)
      (fun i ->
        if 0l <= i && i <= max_uint16_l then
          raise
            (Binary_error_types.Read_error
               (Invalid_int {min = max_uint16 + 1; v = Int32.to_int i; max = 0})) ;
        i);
  ]

let int32 =
  splitted
    ~json:Encoding.int32
    ~compact:(union ~union_tag_bits:2 ~cases_tag_bits:0 int32_cases)

let int64 =
  splitted
    ~json:Encoding.int64
    ~compact:
      (union
         ~union_tag_bits:2
         ~cases_tag_bits:0
         [
           case
             ~title:"small"
             ~description:"An int64 which fits within a uint8"
             (payload Encoding.uint8)
             (fun i ->
               if 0L <= i && i <= max_uint8_L then Some (Int64.to_int i)
               else None)
             (fun i -> Int64.of_int i);
           case
             ~title:"medium"
             ~description:"An int64 which fits within a uint16"
             (payload Encoding.uint16)
             (fun i ->
               if max_uint8_L < i && i <= max_uint16_L then
                 Some (Int64.to_int i)
               else None)
             (fun i ->
               if i <= max_uint8 then
                 raise
                   (Binary_error_types.Read_error
                      (Invalid_int
                         {min = max_uint8 + 1; v = i; max = max_uint16})) ;
               Int64.of_int i);
           case
             ~title:"biggish"
             ~description:"An int64 which fits within a uint32"
             (payload Encoding.int32)
             (fun i ->
               if max_uint16_L < i && i <= max_uint32_L then
                 Some (Int64.to_int32 i)
               else None)
             (fun x ->
               let r = Int64.(logand 0xFFFF_FFFFL (of_int32 x)) in
               if r <= max_uint16_L then
                 raise
                   (Binary_error_types.Read_error
                      (Invalid_int
                         {
                           min = max_uint16 + 1;
                           v = Int32.to_int x;
                           max = max_uint32;
                         })) ;
               r);
           case
             ~title:"bigger"
             ~description:"An int64 which doesn't fit within a uint32"
             (payload Encoding.int64)
             (fun i -> if max_uint32_L < i || i < 0L then Some i else None)
             (fun i ->
               if 0L <= i && i <= max_uint32_L then
                 raise
                   (Binary_error_types.Read_error
                      (Invalid_int
                         {min = max_uint32 + 1; v = Int64.to_int i; max = 0})) ;
               i);
         ])

module Compact_list = struct
  type layout = Small_list of int | Big_list

  let layouts bits =
    let bits = pred (1 lsl bits) in
    let rec aux m acc =
      if m < bits then aux (succ m) (Small_list m :: acc) else acc
    in
    List.rev @@ (Big_list :: aux 0 [])

  (** ---- Tag -------------------------------------------------------------- *)

  let tag bits = function Small_list m -> m | Big_list -> pred (1 lsl bits)

  (** ---- Partial encoding ------------------------------------------------- *)

  let specialised_list bits encoding =
    match bits with
    | 0 ->
        conv_partial
          (function [] -> Some () | _ -> None)
          (fun () -> [])
          Encoding.unit
    | n -> Encoding.Fixed.list n encoding

  let partial_encoding : 'a Encoding.t -> layout -> 'a list Encoding.t =
   fun encoding -> function
    | Small_list bits -> specialised_list bits encoding
    | Big_list -> Encoding.list encoding

  let json_encoding = Encoding.list

  (** ---- Classifier ------------------------------------------------------- *)

  let classify bits l =
    let m = pred (1 lsl bits) in
    let rec aux bits l =
      if bits < m then
        match l with [] -> Small_list bits | _ :: rst -> aux (bits + 1) rst
      else Big_list
    in
    aux 0 l
end

let list : type a. bits:int -> a Encoding.t -> a list t =
 fun ~bits encoding ->
  if bits < 0 then
    raise (Invalid_argument "Data_encoding.Compact.list: negative bit-length") ;
  (module struct
    type input = a list

    include Compact_list

    let layouts = layouts bits

    let tag_len = bits

    let tag = tag bits

    let title _ = None

    let classify = classify bits

    let partial_encoding = partial_encoding encoding

    let json_encoding = json_encoding encoding
  end)

let or_int32 : type a.
    int32_title:string ->
    alt_title:string ->
    ?alt_description:string ->
    a Encoding.t ->
    (int32, a) Either.t t =
 fun ~int32_title ~alt_title ?alt_description alt_encoding ->
  let left_cases =
    List.map
      (fun (Case {title; description; proj; inj; compact}) ->
        let title = Printf.sprintf "%s_%s" int32_title title in
        let proj = function
          | Either.Left i32 -> proj i32
          | Either.Right _ -> None
        in
        let inj i = Either.Left (inj i) in
        Case {title; description; proj; inj; compact})
      int32_cases
  in
  let right_case =
    case
      ~title:alt_title
      ?description:alt_description
      (payload alt_encoding)
      (function Either.Right a -> Some a | Either.Left _ -> None)
      (fun a -> Either.Right a)
  in
  union ~union_tag_bits:2 ~cases_tag_bits:0 (left_cases @ [right_case])

module Custom = struct
  module type S = S

  type tag = int

  let join_tags = join_tags

  let make x = x
end
