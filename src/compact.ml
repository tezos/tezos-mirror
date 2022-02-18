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

let ( lor ) = Int32.logor

let ( << ) x y = Int32.shift_left x y

(* ---- Constants ----------------------------------------------------------- *)

let max_uint8_l = 0xFFl

let max_uint16_l = 0xFFFFl

let max_uint8_L = Int64.of_int32 max_uint8_l

let max_uint16_L = Int64.of_int32 max_uint16_l

let max_uint32_L = 0xFFFF_FFFFL

(* ---- Tags ---------------------------------------------------------------- *)

type tag = int32

let join_tags tags =
  let tag_value, tag_len =
    List.fold_left
      (fun (res, ofs) (tag_value, tag_len) ->
        (res lor (tag_value << ofs), ofs + tag_len))
      (0l, 0)
      tags
  in
  if tag_len > 16 then
    raise @@ Invalid_argument "join_tags: total tag_len shouldn't be over 16" ;
  tag_value

let to_int tag_value = Int32.to_int tag_value

(* ---- Encoding helpers ---------------------------------------------------- *)

(** [conv_partial f g encoding] is the counterpart of
    [conv_with_guard]. It allows to define an encoding which is able
    to encode only a subset of the input type.

    @raise Write_error on any attempt to encode data in the unsupported
    subset of the input type. *)
let conv_partial f g encoding =
  Encoding.(
    conv
      (fun x ->
        match f x with
        | Some x -> x
        | None -> raise Binary_error_types.(Write_error No_case_matched))
      g
      encoding)

(* ---- Compact encoding definition ----------------------------------------- *)

module type S = sig
  type input

  type layout

  val layouts : layout list

  val tag_len : int

  val tag : layout -> tag

  val partial_encoding : layout -> input Encoding.t

  val classify : input -> layout

  val json_encoding : input Encoding.t
end

type 'a t = (module S with type input = 'a)

let make : type a. ?tag_size:[`Uint8 | `Uint16] -> a t -> a Encoding.t =
 fun ?(tag_size = `Uint8) (module C : S with type input = a) ->
  let tag_len_limit = match tag_size with `Uint8 -> 8 | `Uint16 -> 16 in

  if C.tag_len > tag_len_limit then
    raise @@ Invalid_argument "Compact_encoding.make: tags do not fit" ;

  let tag layout =
    let candidate = C.tag layout in
    if candidate >= (1l << C.tag_len) then
      raise @@ Invalid_argument "Compact_encoding.make: tags do not fit" ;
    to_int candidate
  in

  Encoding.(
    raw_splitted
      ~json:(Json.convert C.json_encoding)
      ~binary:
        (matching ~tag_size (fun x ->
             let layout = C.classify x in
             matched
               ~tag_size
               (C.tag layout |> to_int)
               (C.partial_encoding layout)
               x)
        @@ List.map
             (fun layout ->
               let tag = tag layout in
               (*
                 Note: the projection function is never used. This is
                 because [matching] uses the list of cases for
                 decoding only, not encoding.
                *)
               case
                 ~title:(Format.sprintf "case %d" tag)
                 (Tag tag)
                 (C.partial_encoding layout)
                 (fun x -> Some x)
                 (fun x -> x))
             C.layouts))

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

    let classify = refute

    let partial_encoding = refute

    let tag = refute

    let json_encoding =
      Encoding.(
        conv_with_guard refute (fun _ -> Error "void has no inhabitant") empty)
  end)

type ('a, 'b, 'c) case_open = {
  kind : string;
  proj : 'a -> 'b option;
  inj : 'b -> 'a;
  compact : (module S with type input = 'b and type layout = 'c);
}

type ('a, 'b, 'c) case_layout_open = {
  extra_tag : int32;
  proj : 'a -> 'b option;
  inj : 'b -> 'a;
  compact : (module S with type input = 'b and type layout = 'c);
  layout : 'c;
}

type 'a case = Case : ('a, 'b, 'c) case_open -> 'a case [@@unboxed]

let case : type a b. string -> (a -> b option) -> (b -> a) -> b t -> a case =
 fun kind proj inj compact ->
  let (module C : S with type input = b) = compact in
  Case {kind; proj; inj; compact = (module C)}

type 'a case_layout =
  | Case_layout : ('a, 'b, 'c) case_layout_open -> 'a case_layout
[@@unboxed]

let case_to_layout_open :
    type a b layout c.
    tag ->
    (a, b, layout) case_open ->
    ((a, b, layout) case_layout_open -> c) ->
    c list =
 fun extra_tag {proj; inj; kind = _; compact} f ->
  let (module C : S with type input = b and type layout = layout) = compact in
  List.map (fun layout -> f {extra_tag; proj; inj; compact; layout}) C.layouts

let case_to_layout : type a. tag -> a case -> a case_layout list =
 fun extra_tag (Case case) ->
  case_to_layout_open extra_tag case (fun x -> Case_layout x)

let cases_to_layouts : type a. a case list -> a case_layout list =
 fun cases ->
  List.mapi (fun i -> case_to_layout @@ Int32.of_int i) cases |> List.concat

let classify_with_case_open :
    type a b layout.
    tag ->
    (a, b, layout) case_open ->
    a ->
    (a, b, layout) case_layout_open option =
 fun extra_tag {compact; proj; inj; _} input ->
  let (module C : S with type input = b and type layout = layout) = compact in
  match proj input with
  | Some input' ->
      let layout = C.classify input' in
      Some {proj; inj; extra_tag; layout; compact}
  | None -> None

let classify_with_case : type a. tag -> a case -> a -> a case_layout option =
 fun extra_tag (Case case) input ->
  match classify_with_case_open extra_tag case input with
  | Some layout -> Some (Case_layout layout)
  | None -> None

let classify_with_cases_exn : type a. a case list -> a -> a case_layout =
 fun cases input ->
  let rec classify_aux extra_tag = function
    | [] -> raise (Invalid_argument "classify_exn")
    | case :: rst -> (
        match classify_with_case extra_tag case input with
        | Some layout -> layout
        | None -> classify_aux (Int32.succ extra_tag) rst)
  in
  classify_aux 0l cases

let tag_with_case_layout_open :
    type a b layout. int -> (a, b, layout) case_layout_open -> tag =
 fun inner_tag_len {extra_tag; compact; layout; _} ->
  let (module C : S with type input = b and type layout = layout) = compact in
  (extra_tag << inner_tag_len) lor C.tag layout

let tag_with_case_layout : type a. int -> a case_layout -> tag =
 fun inner_tag_len (Case_layout case) ->
  tag_with_case_layout_open inner_tag_len case

let tag_len_of_case_open : type a b layout. (a, b, layout) case_open -> int =
 fun {compact; _} ->
  let (module C : S with type input = b and type layout = layout) = compact in
  C.tag_len

let tag_len_of_case : type a. a case -> int =
 fun (Case case) -> tag_len_of_case_open case

let partial_encoding_of_case_layout_open :
    type a b layout. (a, b, layout) case_layout_open -> a Encoding.t =
 fun {proj; inj; compact; layout; _} ->
  let (module C : S with type input = b and type layout = layout) = compact in
  conv_partial proj inj @@ C.partial_encoding layout

let partial_encoding_of_case_layout : type a. a case_layout -> a Encoding.t =
 fun (Case_layout layout) -> partial_encoding_of_case_layout_open layout

let case_to_data_encoding_case_open :
    type a b layout. int -> (a, b, layout) case_open -> a Encoding.case =
 fun tag {kind; proj; inj; compact} ->
  let (module C : S with type input = b and type layout = layout) = compact in
  Encoding.(
    case
      (Tag tag)
      ~title:(Format.sprintf "case %d" tag)
      (obj2 (req "kind" string) (req "value" C.json_encoding))
      (fun x -> match proj x with Some x -> Some (kind, x) | None -> None)
      (function
        | kind', x when String.equal kind' kind -> inj x
        | _ ->
            raise
            @@ Invalid_argument
                 "case_to_data_encoding_case_open: Incorrect kind"))

let case_to_data_encoding_case : type a. int -> a case -> a Encoding.case =
 fun tag (Case layout) -> case_to_data_encoding_case_open tag layout

let union :
    type a. ?union_tag_bits:int -> ?cases_tag_bits:int -> a case list -> a t =
 fun ?union_tag_bits ?cases_tag_bits cases ->
  if cases = [] then
    invalid_arg "Data_encoding.Compact.union: empty list of cases." ;
  (module struct
    type input = a

    let bits title min = function
      | Some choice when min <= choice -> choice
      | None -> min
      | Some _ ->
          raise
            (Invalid_argument (Format.sprintf "union: not enough %s bits" title))

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
      ( bits "tag" min_union union_tag_bits,
        bits "inner" min_cases cases_tag_bits )

    let tag_len =
      let r = union_tag_len + cases_tag_len in
      if r >= 16 then
        raise @@ Invalid_argument "Compact_encoding.union: tags do not fit" ;
      r

    type layout = a case_layout

    let layouts = cases_to_layouts cases

    let classify = classify_with_cases_exn cases

    let partial_encoding = partial_encoding_of_case_layout

    let tag layout = tag_with_case_layout cases_tag_len layout

    let json_encoding : input Encoding.t =
      Encoding.union @@ List.mapi case_to_data_encoding_case cases
  end)

let payload : type a. a Encoding.t -> a t =
 fun encoding : (module S with type input = a) ->
  (module struct
    type input = a

    type layout = unit

    let layouts = [()]

    let tag_len = 0

    let tag _ = 0l

    let classify _ = ()

    let partial_encoding _ = encoding

    let json_encoding = encoding
  end)

let empty = payload Encoding.empty

let conv : type a b. ?json:a Encoding.t -> (a -> b) -> (b -> a) -> b t -> a t =
 fun ?json f g (module B : S with type input = b) ->
  (module struct
    type input = a

    type layout = B.layout

    let layouts = B.layouts

    let tag_len = B.tag_len

    let tag = B.tag

    let classify b = B.classify (f b)

    let partial_encoding l = Encoding.conv f g (B.partial_encoding l)

    let json_encoding =
      match json with
      | None -> Encoding.conv f g B.json_encoding
      | Some encoding -> encoding
  end)

let option compact =
  union
    [
      case "some" (fun x -> x) (fun x -> Some x) compact;
      case
        "none"
        (function None -> Some () | _ -> None)
        (fun () -> None)
        empty;
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

    let json_encoding = Encoding.tup1 A.json_encoding
  end)

let tup2 : type a b. a t -> b t -> (a * b) t =
 fun (module A : S with type input = a) (module B : S with type input = b) :
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

    let json_encoding = Encoding.tup2 A.json_encoding B.json_encoding
  end)

let tup3 : type a b c. a t -> b t -> c t -> (a * b * c) t =
 fun (module A : S with type input = a)
     (module B : S with type input = b)
     (module C : S with type input = c) : (module S with type input = a * b * c) ->
  (module struct
    type input = A.input * B.input * C.input

    type layout = A.layout * B.layout * C.layout

    let tag_len =
      let r = A.tag_len + B.tag_len + C.tag_len in
      if r >= 16 then
        raise @@ Invalid_argument "Compact_encoding.tup2: tags do not fit" ;
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

    let json_encoding =
      Encoding.tup3 A.json_encoding B.json_encoding C.json_encoding
  end)

let tup4 : type a b c d. a t -> b t -> c t -> d t -> (a * b * c * d) t =
 fun (module A : S with type input = a)
     (module B : S with type input = b)
     (module C : S with type input = c)
     (module D : S with type input = d) :
     (module S with type input = a * b * c * d) ->
  (module struct
    type input = A.input * B.input * C.input * D.input

    type layout = A.layout * B.layout * C.layout * D.layout

    let tag_len =
      let r = A.tag_len + B.tag_len + C.tag_len + D.tag_len in
      if r >= 16 then
        raise @@ Invalid_argument "Compact_encoding.tup2: tags do not fit" ;
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

    let json_encoding =
      Encoding.tup4
        A.json_encoding
        B.json_encoding
        C.json_encoding
        D.json_encoding
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

type 'a field = Field : ('b, 'a) field_open -> 'a field

let field_to_data_encoding_open :
    type a b. (a, b) field_open -> b Encoding.field =
  let open Encoding in
  function
  | Req {name; compact} ->
      let (module A) = compact in
      req name A.json_encoding
  | Opt {name; compact} ->
      let (module A) = compact in
      opt name A.json_encoding

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

    let json_encoding = Encoding.(obj1 @@ field_to_data_encoding_open f1)
  end)

let obj1 (Field f1) = obj1_open f1

let obj2_open :
    type a b c d.
    (a, b) field_open -> (c, d) field_open -> (module S with type input = b * d)
    =
 fun f1 f2 ->
  let (module Tup) =
    tup2 (field_to_inner_compact f1) (field_to_inner_compact f2)
  in
  (module struct
    include Tup

    let json_encoding =
      Encoding.(
        obj2 (field_to_data_encoding_open f1) (field_to_data_encoding_open f2))
  end)

let obj2 (Field f1) (Field f2) = obj2_open f1 f2

let obj3_open :
    type a b c d e f.
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
      Encoding.(
        obj3
          (field_to_data_encoding_open f1)
          (field_to_data_encoding_open f2)
          (field_to_data_encoding_open f3))
  end)

let obj3 (Field f1) (Field f2) (Field f3) = obj3_open f1 f2 f3

let obj4_open :
    type a b c d e f g h.
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
      Encoding.(
        obj4
          (field_to_data_encoding_open f1)
          (field_to_data_encoding_open f2)
          (field_to_data_encoding_open f3)
          (field_to_data_encoding_open f4))
  end)

let obj4 (Field f1) (Field f2) (Field f3) (Field f4) = obj4_open f1 f2 f3 f4

module Compact_bool = struct
  type input = bool

  type layout = bool

  let layouts = [true; false]

  let tag_len = 1

  let tag = function true -> 1l | false -> 0l

  let partial_encoding : layout -> bool Encoding.t =
   fun b ->
    Encoding.(
      conv_partial
        (function b' when Bool.equal b b' -> Some () | _ -> None)
        (fun () -> b)
        empty)

  let classify x = x

  let json_encoding = Encoding.bool
end

let bool : bool t = (module Compact_bool)

module Compact_int32 = struct
  type input = int32

  type layout = Int8 | Int16 | Int32

  let layouts = [Int8; Int16; Int32]

  (** ---- Tag -------------------------------------------------------------- *)

  let tag_len = 2

  let tag = function Int8 -> 0l | Int16 -> 1l | Int32 -> 2l

  let unused_tag = 3l

  (** ---- Partial encoding ------------------------------------------------- *)

  let int8_l : int32 Encoding.t =
    Encoding.(conv Int32.to_int Int32.of_int uint8)

  let int16_l : int32 Encoding.t =
    Encoding.(conv Int32.to_int Int32.of_int uint16)

  let int32_l : int32 Encoding.t = Encoding.int32

  let partial_encoding : layout -> int32 Encoding.t = function
    | Int8 -> int8_l
    | Int16 -> int16_l
    | Int32 -> int32_l

  (** ---- Classifier ------------------------------------------------------- *)

  let classify = function
    | i when 0l <= i && i <= max_uint8_l -> Int8
    | i when max_uint8_l < i && i <= max_uint16_l -> Int16
    | _ -> Int32

  let json_encoding = Encoding.int32
end

let int32 : int32 t = (module Compact_int32)

module Compact_int64 = struct
  type input = int64

  type layout = Int64 | Int32 | Int16 | Int8

  let layouts = [Int64; Int32; Int16; Int8]

  (** ---- Tag -------------------------------------------------------------- *)

  let tag_len = 2

  let tag = function Int8 -> 0l | Int16 -> 1l | Int32 -> 2l | Int64 -> 3l

  (** ---- Partial encoding ------------------------------------------------- *)

  let int8_L : int64 Encoding.t =
    Encoding.(conv Int64.to_int Int64.of_int uint8)

  let int16_L : int64 Encoding.t =
    Encoding.(conv Int64.to_int Int64.of_int uint16)

  let int32_L : int64 Encoding.t =
    Encoding.(
      conv
        Int64.to_int32
        (fun x -> Int64.(logand 0xFFFF_FFFFL (of_int32 x)))
        int32)

  let int64_L : int64 Encoding.t = Encoding.int64

  let partial_encoding : layout -> int64 Encoding.t = function
    | Int8 -> int8_L
    | Int16 -> int16_L
    | Int32 -> int32_L
    | Int64 -> int64_L

  (** ---- Classifier ------------------------------------------------------- *)

  let classify = function
    | i when 0L <= i && i <= max_uint8_L -> Int8
    | i when max_uint8_L < i && i <= max_uint16_L -> Int16
    | i when max_uint16_L < i && i <= max_uint32_L -> Int32
    | _ -> Int64

  let json_encoding = Encoding.int64
end

let int64 : int64 t = (module Compact_int64)

module Compact_list = struct
  type layout = Small_list of int32 | Big_list

  let layouts bits =
    let bits = Int32.(shift_left 1l bits |> pred) in
    let rec aux m acc =
      if m < bits then aux (Int32.succ m) (Small_list m :: acc) else acc
    in
    List.rev @@ (Big_list :: aux 0l [])

  (** ---- Tag -------------------------------------------------------------- *)

  let tag bits = function
    | Small_list m -> m
    | Big_list -> Int32.(pred @@ shift_left 1l bits)

  (** ---- Partial encoding ------------------------------------------------- *)

  let list bits encoding =
    let rec aux m =
      Encoding.(
        match m with
        | 0 ->
            conv_partial
              (function [] -> Some () | _ -> None)
              (fun () -> [])
              empty
        | 1 ->
            conv_partial
              (function [x] -> Some x | _ -> None)
              (fun x -> [x])
              (tup1 encoding)
        | 2 ->
            conv_partial
              (function [x1; x2] -> Some (x1, x2) | _ -> None)
              (fun (x1, x2) -> [x1; x2])
              (tup2 encoding encoding)
        | 3 ->
            conv_partial
              (function [x1; x2; x3] -> Some (x1, x2, x3) | _ -> None)
              (fun (x1, x2, x3) -> [x1; x2; x3])
              (tup3 encoding encoding encoding)
        | 4 ->
            conv_partial
              (function [x1; x2; x3; x4] -> Some (x1, x2, x3, x4) | _ -> None)
              (fun (x1, x2, x3, x4) -> [x1; x2; x3; x4])
              (tup4 encoding encoding encoding encoding)
        | 5 ->
            conv_partial
              (function
                | [x1; x2; x3; x4; x5] -> Some (x1, x2, x3, x4, x5) | _ -> None)
              (fun (x1, x2, x3, x4, x5) -> [x1; x2; x3; x4; x5])
              (tup5 encoding encoding encoding encoding encoding)
        | 6 ->
            conv_partial
              (function
                | [x1; x2; x3; x4; x5; x6] -> Some (x1, x2, x3, x4, x5, x6)
                | _ -> None)
              (fun (x1, x2, x3, x4, x5, x6) -> [x1; x2; x3; x4; x5; x6])
              (tup6 encoding encoding encoding encoding encoding encoding)
        | 7 ->
            conv_partial
              (function
                | [x1; x2; x3; x4; x5; x6; x7] ->
                    Some (x1, x2, x3, x4, x5, x6, x7)
                | _ -> None)
              (fun (x1, x2, x3, x4, x5, x6, x7) -> [x1; x2; x3; x4; x5; x6; x7])
              (tup7
                 encoding
                 encoding
                 encoding
                 encoding
                 encoding
                 encoding
                 encoding)
        | 8 ->
            conv_partial
              (function
                | [x1; x2; x3; x4; x5; x6; x7; x8] ->
                    Some (x1, x2, x3, x4, x5, x6, x7, x8)
                | _ -> None)
              (fun (x1, x2, x3, x4, x5, x6, x7, x8) ->
                [x1; x2; x3; x4; x5; x6; x7; x8])
              (tup8
                 encoding
                 encoding
                 encoding
                 encoding
                 encoding
                 encoding
                 encoding
                 encoding)
        | m ->
            conv_partial
              (function
                | x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: rst ->
                    Some ((x1, x2, x3, x4, x5, x6, x7, x8), rst)
                | _ -> None)
              (fun ((x1, x2, x3, x4, x5, x6, x7, x8), rst) ->
                x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: rst)
              (merge_tups
                 (tup8
                    encoding
                    encoding
                    encoding
                    encoding
                    encoding
                    encoding
                    encoding
                    encoding)
                 (aux (m - 8))))
    in
    aux bits

  let partial_encoding : 'a Encoding.t -> layout -> 'a list Encoding.t =
   fun encoding -> function
    | Small_list bits -> list (Int32.to_int bits) encoding
    | Big_list -> Encoding.list encoding

  let json_encoding = Encoding.list

  (** ---- Classifier ------------------------------------------------------- *)

  let classify bits l =
    let m = Int32.(shift_left 1l bits |> pred |> to_int) in
    let rec aux bits l =
      if bits < m then
        match l with
        | [] -> Small_list (Int32.of_int bits)
        | _ :: rst -> aux (bits + 1) rst
      else Big_list
    in
    aux 0 l
end

let list : type a. bits:int -> a Encoding.t -> a list t =
 fun ~bits encoding ->
  (module struct
    type input = a list

    include Compact_list

    let layouts = layouts bits

    let tag_len = bits

    let tag = tag bits

    let classify = classify bits

    let partial_encoding = partial_encoding encoding

    let json_encoding = json_encoding encoding
  end)

module Compact_either_int32 = struct
  open Either

  type layout = Compact_int32.layout option

  let layouts = None :: List.map Option.some Compact_int32.layouts

  (** ---- Tag -------------------------------------------------------------- *)

  let tag_len = Compact_int32.tag_len

  let tag = function
    | Some i -> Compact_int32.tag i
    | None -> Compact_int32.unused_tag

  (** ---- Partial encoding ------------------------------------------------- *)

  let partial_encoding val_encoding = function
    | Some id ->
        conv_partial
          (function Left i -> Some i | _ -> None)
          (fun i -> Left i)
          (Compact_int32.partial_encoding id)
    | None ->
        conv_partial
          (function Right v -> Some v | _ -> None)
          (fun v -> Right v)
          val_encoding

  (** ---- Classifier ------------------------------------------------------- *)

  let classify : (int32, 'a) Either.t -> layout = function
    | Left i -> Some (Compact_int32.classify i)
    | _ -> None
end

let or_int32 :
    type a.
    int32_kind:string ->
    alt_kind:string ->
    a Encoding.t ->
    (int32, a) Either.t t =
 fun ~int32_kind ~alt_kind encoding ->
  (module struct
    open Either

    type input = (int32, a) Either.t

    include Compact_either_int32

    let partial_encoding = partial_encoding encoding

    let json_encoding =
      Encoding.(
        conv_with_guard
          (function
            | Left _ as expr -> (int32_kind, expr)
            | Right _ as expr -> (alt_kind, expr))
          (function
            | kind, (Left _ as x) when String.equal kind int32_kind -> Ok x
            | kind, (Right _ as x) when String.equal kind alt_kind -> Ok x
            | _ -> Error "not a valid kind")
          (obj2
             (req "kind" string)
             (req "value"
             @@ union
                  [
                    case
                      (Tag 0)
                      ~title:int32_kind
                      int32
                      (function Left x -> Some x | _ -> None)
                      (fun x -> Left x);
                    case
                      (Tag 1)
                      ~title:alt_kind
                      encoding
                      (function Right x -> Some x | _ -> None)
                      (fun x -> Right x);
                  ])))
  end)

module Custom = struct
  module type S = S

  type tag = int32

  let join_tags = join_tags

  let make x = x
end
