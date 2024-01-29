(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Hash_builtin

type limit = No_limit | At_most of int | Exactly of int [@@deriving hash]

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Kind = struct
  type t = [`Fixed of int | `Dynamic | `Variable] [@@deriving hash]

  type length = [`Fixed of int | `Variable]

  type enum = [`Dynamic | `Variable]

  let combine name : t -> t -> t =
   fun k1 k2 ->
    match (k1, k2) with
    | `Fixed n1, `Fixed n2 -> `Fixed (n1 + n2)
    | `Dynamic, `Dynamic | `Fixed _, `Dynamic | `Dynamic, `Fixed _ -> `Dynamic
    | `Variable, `Fixed _ | (`Dynamic | `Fixed _), `Variable -> `Variable
    | `Variable, `Dynamic ->
        Printf.ksprintf
          invalid_arg
          "Cannot merge two %s when the left element is of variable length and \
           the right one of dynamic length. You should use the reverse order, \
           or wrap the second one with Data_encoding.dynamic_size."
          name
    | `Variable, `Variable ->
        Printf.ksprintf
          invalid_arg
          "Cannot merge two %s with variable length. You should wrap one of \
           them with Data_encoding.dynamic_size."
          name

  let merge : t -> t -> t =
   fun k1 k2 ->
    match (k1, k2) with
    | `Fixed n1, `Fixed n2 when n1 = n2 -> `Fixed n1
    | `Fixed _, `Fixed _ -> `Dynamic
    | `Dynamic, `Dynamic | `Fixed _, `Dynamic | `Dynamic, `Fixed _ -> `Dynamic
    | `Variable, (`Dynamic | `Fixed _)
    | (`Dynamic | `Fixed _), `Variable
    | `Variable, `Variable ->
        `Variable

  let merge_list sz : t list -> t = function
    | [] -> assert false (* should be rejected by Data_encoding.union *)
    | k :: ks -> (
        match List.fold_left merge k ks with
        | `Fixed n -> `Fixed (n + Binary_size.tag_size sz)
        | k -> k)
end

type case_tag = Tag of int | Json_only

(* [case_tag_internal] is an optimised representation of case_tag. The
   idea is to encode values of type [case_tag] to remove a level of
   indirection. *)
type case_tag_internal = Uint_option.t

let json_only = Uint_option.none

let make_tag = Uint_option.some

let is_tag = Uint_option.is_some

let get_tag = Uint_option.get

type string_json_repr = Hex | Plain

type 'a desc =
  | Null : unit desc
  | Empty : unit desc
  | Ignore : unit desc
  | Constant : string -> unit desc
  | Bool : bool desc
  | Int8 : int desc
  | Uint8 : int desc
  | Int16 : TzEndian.endianness -> int desc
  | Uint16 : TzEndian.endianness -> int desc
  | Int31 : TzEndian.endianness -> int desc
  | Int32 : TzEndian.endianness -> Int32.t desc
  | Int64 : TzEndian.endianness -> Int64.t desc
  | N : Z.t desc
  | Z : Z.t desc
  | RangedInt : {
      minimum : int;
      endianness : TzEndian.endianness;
      maximum : int;
    }
      -> int desc
  | RangedFloat : {minimum : float; maximum : float} -> float desc
  | Float : float desc
  | Bytes : Kind.length * string_json_repr -> Bytes.t desc
  | String : Kind.length * string_json_repr -> string desc
  | Bigstring : Kind.length * string_json_repr -> bigstring desc
  | Padded : 'a t * int -> 'a desc
  | String_enum : ('a, string * int) Hashtbl.t * 'a array -> 'a desc
  | Array : {
      length_limit : limit;
      length_encoding : int t option;
      elts : 'a t;
    }
      -> (* invariant: the field [length_encoding] is never set if [length_limit]
            is [No_limit] or [Exactly]. *)
      'a array desc
  | List : {
      length_limit : limit;
      length_encoding : int t option;
      elts : 'a t;
    }
      -> (* invariant: the field [length_encoding] is never set if [length_limit]
            is [No_limit] or [Exactly]. *)
      'a list desc
  | Obj : 'a field -> 'a desc
  | Objs : {kind : Kind.t; left : 'a t; right : 'b t} -> ('a * 'b) desc
  | Tup : 'a t -> 'a desc
  | Tups : {kind : Kind.t; left : 'a t; right : 'b t} -> ('a * 'b) desc
  | Union : {
      kind : Kind.t;
      tag_size : Binary_size.tag_size;
      tagged_cases : 'a case array;
      match_case : 'a -> match_result;
      cases : 'a case list;
    }
      -> 'a desc
  | Mu : {
      kind : Kind.enum;
      name : string;
      title : string option;
      description : string option;
      fix : 'a t -> 'a t;
    }
      -> 'a desc
  | Conv : {
      proj : 'a -> 'b;
      inj : 'b -> 'a;
      encoding : 'b t;
      schema : Json_schema.schema option;
    }
      -> 'a desc
  | Describe : {
      id : string;
      title : string option;
      description : string option;
      encoding : 'a t;
    }
      -> 'a desc
  | Splitted : {
      encoding : 'a t;
      json_encoding : 'a Json_encoding.encoding;
      is_obj : bool;
      is_tup : bool;
    }
      -> 'a desc
  | Dynamic_size : {kind : Binary_size.length; encoding : 'a t} -> 'a desc
  | Check_size : {limit : int; encoding : 'a t} -> 'a desc
  | Delayed : (unit -> 'a t) -> 'a desc

and _ field =
  | Req : {
      name : string;
      encoding : 'a t;
      title : string option;
      description : string option;
    }
      -> 'a field
  | Opt : {
      name : string;
      kind : Kind.enum;
      encoding : 'a t;
      title : string option;
      description : string option;
    }
      -> 'a option field
  | Dft : {
      name : string;
      encoding : 'a t;
      default : 'a;
      title : string option;
      description : string option;
    }
      -> 'a field

and 'a case =
  | Case : {
      title : string;
      description : string option;
      encoding : 'a t;
      proj : 't -> 'a option;
      inj : 'a -> 't;
      tag : case_tag_internal;
    }
      -> 't case

and match_result = Matched : int * 'b t * 'b -> match_result

and 'a t = {
  encoding : 'a desc;
  mutable json_encoding : 'a Json_encoding.encoding option;
}

type 'a encoding = 'a t

let rec classify : type a. a t -> Kind.t = fun e -> classify_desc e.encoding

and classify_desc : type a. a desc -> Kind.t =
 fun e ->
  match e with
  (* Fixed *)
  | Null -> `Fixed 0
  | Empty -> `Fixed 0
  | Constant _ -> `Fixed 0
  | Bool -> `Fixed Binary_size.bool
  | Int8 -> `Fixed Binary_size.int8
  | Uint8 -> `Fixed Binary_size.uint8
  | Int16 _ -> `Fixed Binary_size.int16
  | Uint16 _ -> `Fixed Binary_size.uint16
  | Int31 _ -> `Fixed Binary_size.int31
  | Int32 _ -> `Fixed Binary_size.int32
  | Int64 _ -> `Fixed Binary_size.int64
  | N -> `Dynamic
  | Z -> `Dynamic
  | RangedInt {minimum; endianness = _; maximum} ->
      `Fixed Binary_size.(integer_to_size @@ range_to_size ~minimum ~maximum)
  | Float -> `Fixed Binary_size.float
  | RangedFloat _ -> `Fixed Binary_size.float
  (* Tagged *)
  | Bytes (kind, _repr) -> (kind :> Kind.t)
  | String (kind, _repr) -> (kind :> Kind.t)
  | Bigstring (kind, _repr) -> (kind :> Kind.t)
  | Padded ({encoding; _}, n) -> (
      match classify_desc encoding with
      | `Fixed m -> `Fixed (n + m)
      | `Dynamic | `Variable ->
          assert false (* by construction (see [Fixed.padded]) *))
  | String_enum (_, cases) ->
      `Fixed Binary_size.(integer_to_size @@ enum_size cases)
  | Obj (Opt {kind; _}) -> (kind :> Kind.t)
  | Objs {kind; _} -> kind
  | Tups {kind; _} -> kind
  | Union {kind; _} -> (kind :> Kind.t)
  | Mu {kind; _} -> (kind :> Kind.t)
  (* Variable *)
  | Ignore -> `Fixed 0
  | Array {length_limit; length_encoding = Some _; elts} ->
      (* when [length_encoding] is set the encoding can only be dynamic, we
         still perform assertion checks. *)
      assert (match length_limit with At_most _ -> true | _ -> false) ;
      assert (
        match classify_desc elts.encoding with
        | `Fixed _ | `Dynamic -> true
        | `Variable -> false) ;
      `Dynamic
  | Array {length_limit = Exactly l; length_encoding = None; elts} -> (
      match classify_desc elts.encoding with
      | `Fixed e -> `Fixed (l * e)
      | `Dynamic -> `Dynamic
      | `Variable -> assert false)
  | Array _ -> `Variable
  | List {length_limit; length_encoding = Some _; elts} ->
      (* when [length_encoding] is set the encoding can only be dynamic, we
         still perform assertion checks. *)
      assert (match length_limit with At_most _ -> true | _ -> false) ;
      assert (
        match classify_desc elts.encoding with
        | `Fixed _ | `Dynamic -> true
        | `Variable -> false) ;
      `Dynamic
  | List {length_limit = Exactly l; length_encoding = None; elts} -> (
      match classify_desc elts.encoding with
      | `Fixed e -> `Fixed (l * e)
      | `Dynamic -> `Dynamic
      | `Variable -> assert false)
  | List _ -> `Variable
  (* Recursive *)
  | Obj (Req {encoding; _}) -> classify encoding
  | Obj (Dft {encoding; _}) -> classify encoding
  | Tup encoding -> classify encoding
  | Conv {encoding; _} -> classify encoding
  | Describe {encoding; _} -> classify encoding
  | Splitted {encoding; _} -> classify encoding
  | Dynamic_size _ -> `Dynamic
  | Check_size {encoding; _} -> classify encoding
  | Delayed f -> classify (f ())

let check_not_variable name e =
  match classify e with
  | `Variable ->
      Printf.ksprintf
        invalid_arg
        "Cannot insert variable length element in %s. You should wrap the \
         contents using Data_encoding.dynamic_size."
        name
  | `Dynamic | `Fixed _ -> ()

let n_length value =
  let bits = Z.numbits value in
  if bits = 0 then 1 else (bits + 6) / 7

let z_length value = (Z.numbits value + 1 + 6) / 7

(* [Mu_visited] is intended for internal use only. It is used to record visit
   to recursion nodes ([Mu]) to avoid infinite recursion. See [is_zeroable] for
   an example of use. *)
module Mu_visited : sig
  type t

  val empty : t

  val mem : _ desc -> t -> bool

  (* Raise an exception if called with a node different than [Mu]. *)
  val add : _ desc -> t -> t
end = struct
  type t = Obj.t list

  let empty = []

  let mem x m =
    match x with Mu _ -> List.memq (Obj.repr x) m | _ -> assert false

  let add x m = match x with Mu _ -> Obj.repr x :: m | _ -> assert false
end

let rec is_zeroable : type t. Mu_visited.t -> t encoding -> bool =
 fun visited e ->
  (* Whether an encoding can ever produce zero-byte of encoding. It is dangerous
     to place zero-size elements in a collection (list/array) because
     they are indistinguishable from the absence of elements. *)
  match e.encoding with
  (* trivially true *)
  | Null -> true (* always true *)
  | Empty -> true (* always true *)
  | Ignore -> true (* always true *)
  | Constant _ -> true (* always true *)
  (* trivially false *)
  | Bool -> false
  | Int8 -> false
  | Uint8 -> false
  | Int16 _ -> false
  | Uint16 _ -> false
  | Int31 _ -> false
  | Int32 _ -> false
  | Int64 _ -> false
  | N -> false
  | Z -> false
  | RangedInt _ -> false
  | RangedFloat _ -> false
  | Float -> false
  | Bytes _ -> false
  | String _ -> false
  | Bigstring _ -> false
  | Padded _ -> false
  | String_enum _ -> false
  (* true in some cases, but in practice always protected by Dynamic *)
  | Array {length_limit; length_encoding = Some le; elts = _} ->
      assert (match length_limit with At_most _ -> true | _ -> false) ;
      (* length-encoding is n, uint8, uint16, or uint30 none of which are
         zeroable *)
      assert (not (is_zeroable visited le)) ;
      false
  | Array {length_limit = Exactly l; length_encoding = None; elts = _} ->
      assert (l > 0) ;
      false
  | Array
      {length_limit = No_limit | At_most _; length_encoding = None; elts = _} ->
      true (* 0-element array, no length prefix *)
  | List {length_limit; length_encoding = Some le; elts = _} ->
      assert (match length_limit with At_most _ -> true | _ -> false) ;
      (* length-encoding is n, uint8, uint16, or uint30 none of which are
         zeroable *)
      assert (not (is_zeroable visited le)) ;
      false
  | List {length_limit = Exactly l; length_encoding = None; elts = _} ->
      assert (l > 0) ;
      false
  | List {length_limit = No_limit | At_most _; length_encoding = None; elts = _}
    ->
      true (* 0-element array, no length prefix *)
  (* represented as whatever is inside: truth mostly propagates *)
  | Obj (Req {encoding = e; _}) -> is_zeroable visited e (* represented as-is *)
  | Obj (Opt {kind = `Variable; _}) -> true (* optional field omitted *)
  | Obj (Dft {encoding = e; _}) -> is_zeroable visited e (* represented as-is *)
  | Obj _ -> false
  | Objs {left; right; _} ->
      is_zeroable visited left && is_zeroable visited right
  | Tup e -> is_zeroable visited e
  | Tups {left; right; _} ->
      is_zeroable visited left && is_zeroable visited right
  | Union _ -> false (* includes a tag *)
  (* other recursive cases: truth propagates *)
  | Mu {kind = `Dynamic; _} -> false (* size prefix *)
  | Mu {kind = `Variable; fix; _} ->
      if Mu_visited.mem e.encoding visited then true
      else is_zeroable (Mu_visited.add e.encoding visited) (fix e)
  | Conv {encoding; _} -> is_zeroable visited encoding
  | Describe {encoding; _} -> is_zeroable visited encoding
  | Splitted {encoding; _} -> is_zeroable visited encoding
  | Check_size {encoding; _} -> is_zeroable visited encoding
  (* Unscrutable: true by default *)
  | Delayed f -> is_zeroable visited (f ())
  (* Protected against zeroable *)
  | Dynamic_size _ ->
      (* always some data for size *)
      false

let is_zeroable e = is_zeroable Mu_visited.empty e

let check_not_zeroable name e =
  if is_zeroable e then
    Printf.ksprintf
      invalid_arg
      "Cannot insert potentially zero-sized element in %s."
      name

let make ?json_encoding encoding = {encoding; json_encoding}

module Fixed = struct
  let string' json_repr n =
    if n <= 0 then
      invalid_arg
        "Cannot create a string encoding of negative or null fixed length." ;
    make @@ String (`Fixed n, json_repr)

  let string n = string' Plain n

  let bytes' json_repr n =
    if n <= 0 then
      invalid_arg
        "Cannot create a byte encoding of negative or null fixed length." ;
    make @@ Bytes (`Fixed n, json_repr)

  let bytes n = bytes' Hex n

  let bigstring ?(string_json_repr = Hex) n =
    if n <= 0 then
      invalid_arg
        "Cannot create a bigstring encoding of negative or null fixed length." ;
    make @@ Bigstring (`Fixed n, string_json_repr)

  let add_padding e n =
    if n <= 0 then
      invalid_arg "Cannot create a padding of negative or null fixed length." ;
    match classify e with
    | `Fixed _ -> make @@ Padded (e, n)
    | `Dynamic | `Variable -> invalid_arg "Cannot pad non-fixed size encoding"

  let list n e =
    if n <= 0 then
      invalid_arg
        "Cannot create a list encoding of negative or null fixed length." ;
    check_not_variable "a fixed-length list" e ;
    check_not_zeroable "a fixed-length list" e ;
    make @@ List {length_limit = Exactly n; length_encoding = None; elts = e}

  let array n e =
    if n <= 0 then
      invalid_arg
        "Cannot create an array encoding of negative or null fixed length." ;
    check_not_variable "a fixed-length array" e ;
    check_not_zeroable "a fixed-length array" e ;
    make @@ Array {length_limit = Exactly n; length_encoding = None; elts = e}
end

module Variable = struct
  let string' json_repr = make @@ String (`Variable, json_repr)

  let string = string' Plain

  let bytes' json_repr = make @@ Bytes (`Variable, json_repr)

  let bytes = bytes' Hex

  let bigstring ?(string_json_repr = Hex) () =
    make @@ Bigstring (`Variable, string_json_repr)

  let array ?max_length e =
    check_not_variable "an array" e ;
    check_not_zeroable "an array" e ;
    let length_limit =
      match max_length with None -> No_limit | Some l -> At_most l
    in
    let encoding =
      make @@ Array {length_limit; length_encoding = None; elts = e}
    in
    match (classify e, max_length) with
    | `Fixed n, Some max_length ->
        let limit = n * max_length in
        make @@ Check_size {limit; encoding}
    | `Fixed _, None -> encoding
    | `Dynamic, (Some _ | None) -> encoding
    | `Variable, _ -> (* checked by check_not_variable *) assert false

  let list ?max_length e =
    check_not_variable "a list" e ;
    check_not_zeroable "a list" e ;
    let length_limit =
      match max_length with None -> No_limit | Some l -> At_most l
    in
    let encoding =
      make @@ List {length_limit; length_encoding = None; elts = e}
    in
    match (classify e, max_length) with
    | `Fixed n, Some max_length ->
        let limit = n * max_length in
        make @@ Check_size {limit; encoding}
    | `Fixed _, None -> encoding
    | `Dynamic, (Some _ | None) -> encoding
    | `Variable, _ -> (* checked by check_not_variable *) assert false
end

let dynamic_size ?(kind = `Uint30) e = make @@ Dynamic_size {kind; encoding = e}

let check_size limit encoding =
  if limit < 0 then
    raise (Invalid_argument "Data_encoding.check_size: negative limit") ;
  make @@ Check_size {limit; encoding}

let delayed f = make @@ Delayed f

let null = make @@ Null

let empty = make @@ Empty

let unit = make @@ Ignore

let constant s = make @@ Constant s

let bool = make @@ Bool

let int8 = make @@ Int8

let uint8 = make @@ Uint8

module Big_endian = struct
  let int16 = make @@ Int16 TzEndian.Big_endian

  let uint16 = make @@ Uint16 TzEndian.Big_endian

  let int31 = make @@ Int31 TzEndian.Big_endian

  let int32 = make @@ Int32 TzEndian.Big_endian

  let ranged_int minimum maximum =
    (* NOTE: all [ranged_*] combinator, support out-of-order arguments. E.g.,
       [ranged_int 1000 0] is a valid encoding equivalent to [ranged_int 0 1000] *)
    let minimum = min minimum maximum and maximum = max minimum maximum in
    if
      minimum < Binary_size.min_int `Int31
      || Binary_size.max_int `Int31 < maximum
    then invalid_arg "Data_encoding.ranged_int" ;
    make @@ RangedInt {minimum; endianness = TzEndian.Big_endian; maximum}

  let int64 = make @@ Int64 TzEndian.Big_endian
end

include Big_endian

let ranged_float minimum maximum =
  let minimum = min minimum maximum and maximum = max minimum maximum in
  make @@ RangedFloat {minimum; maximum}

module Little_endian = struct
  let int16 = make @@ Int16 TzEndian.Little_endian

  let uint16 = make @@ Uint16 TzEndian.Little_endian

  let int31 = make @@ Int31 TzEndian.Little_endian

  let int32 = make @@ Int32 TzEndian.Little_endian

  let ranged_int minimum maximum =
    let minimum = min minimum maximum and maximum = max minimum maximum in
    if
      minimum < Binary_size.min_int `Int31
      || Binary_size.max_int `Int31 < maximum
    then invalid_arg "Data_encoding.ranged_int" ;
    make @@ RangedInt {minimum; endianness = TzEndian.Little_endian; maximum}

  let int64 = make @@ Int64 TzEndian.Little_endian
end

let n = make @@ N

let z = make @@ Z

let float = make @@ Float

let string' ?length_kind json_repr =
  dynamic_size ?kind:length_kind (Variable.string' json_repr)

let string = string' Plain

let bytes' ?length_kind json_repr =
  dynamic_size ?kind:length_kind (Variable.bytes' json_repr)

let bytes = bytes' Hex

let bigstring ?length_kind ?(string_json_repr = Hex) () =
  dynamic_size ?kind:length_kind (Variable.bigstring ~string_json_repr ())

let array ?max_length e = dynamic_size (Variable.array ?max_length e)

let list ?max_length e = dynamic_size (Variable.list ?max_length e)

let string_enum = function
  | [] -> invalid_arg "data_encoding.string_enum: cannot have zero cases"
  | [_case] ->
      invalid_arg
        "data_encoding.string_enum: cannot have a single case, use constant \
         instead"
  | _ :: _ as cases ->
      let arr = Array.of_list (List.map snd cases) in
      let tbl = Hashtbl.create (Array.length arr) in
      List.iteri (fun ind (str, a) -> Hashtbl.add tbl a (str, ind)) cases ;
      make @@ String_enum (tbl, arr)

let conv proj inj ?schema encoding = make @@ Conv {proj; inj; encoding; schema}

let conv_with_guard proj inj_guard ?schema encoding =
  let inj x =
    match inj_guard x with
    | Ok y -> y
    | Error s -> raise (Binary_error_types.Invariant_guard s)
  in
  conv proj inj ?schema encoding

let with_decoding_guard guard encoding =
  conv
    (fun x -> x)
    (fun y ->
      match guard y with
      | Ok () -> y
      | Error s -> raise (Binary_error_types.Invariant_guard s))
    encoding

let int_like_n_or_z ~min_value ~max_value name sizer like =
  if max_value < min_value then invalid_arg name ;
  let z_max_value = Z.of_int max_value in
  let z_min_value = Z.of_int min_value in
  let max_size = max (sizer z_min_value) (sizer z_max_value) in
  check_size
    max_size
    (conv
       (fun i ->
         if i < min_value || i > max_value then
           raise
             Binary_error_types.(
               Write_error
                 (Invalid_int {min = min_value; v = i; max = max_value})) ;
         Z.of_int i)
       (fun z ->
         (if Z.compare z z_min_value < 0 then
          let i =
            (* here and in the next check, we want to make sure that the error
               message is consistent across any platform. To that end, we only
               convert [z] to [int] if it would fit on a 32 bit machine. *)
            if Z.compare z (Z.of_int (Binary_size.min_int `Int31)) < 0 then
              Binary_size.min_int `Int31
            else Z.to_int z
          in
          raise
            Binary_error_types.(
              Read_error (Invalid_int {min = min_value; v = i; max = max_value}))) ;
         (if Z.compare z z_max_value > 0 then
          let i =
            if Z.compare z (Z.of_int (Binary_size.max_int `Int31)) > 0 then
              Binary_size.max_int `Int31
            else Z.to_int z
          in
          raise
            Binary_error_types.(
              Read_error (Invalid_int {min = min_value; v = i; max = max_value}))) ;
         Z.to_int z)
       like)

let uint_like_n ~max_value =
  int_like_n_or_z ~min_value:0 ~max_value "Data_encoding.uint_like_n" n_length n

let int_like_z ~min_value ~max_value =
  int_like_n_or_z ~min_value ~max_value "Data_encoding.int_like_z" z_length z

let def id ?title ?description encoding =
  make @@ Describe {id; title; description; encoding}

let req ?title ?description n t =
  Req {name = n; encoding = t; title; description}

let opt ?title ?description n encoding =
  let kind =
    match classify encoding with
    | `Variable -> `Variable
    | `Fixed _ | `Dynamic -> `Dynamic
  in
  Opt {name = n; kind; encoding; title; description}

let varopt ?title ?description n encoding =
  Opt {name = n; kind = `Variable; encoding; title; description}

let dft ?title ?description n t d =
  Dft {name = n; encoding = t; default = d; title; description}

let raw_splitted ~json ~binary =
  make
  @@ Splitted
       {encoding = binary; json_encoding = json; is_obj = false; is_tup = false}

let rec is_obj : type a. Mu_visited.t -> a t -> bool =
 fun visited e ->
  match e.encoding with
  | Obj _ -> true
  | Objs _ (* by construction *) -> true
  | Conv {encoding = e; _} -> is_obj visited e
  | Dynamic_size {encoding = e; _} -> is_obj visited e
  | Union {cases; _} ->
      List.for_all (fun (Case {encoding = e; _}) -> is_obj visited e) cases
  | Empty -> true
  | Ignore -> true
  | Mu {fix; _} ->
      if Mu_visited.mem e.encoding visited then false
      else is_obj (Mu_visited.add e.encoding visited) (fix e)
  | Splitted {is_obj; _} -> is_obj
  | Delayed f -> is_obj visited (f ())
  | Describe {encoding; _} -> is_obj visited encoding
  | Padded (_encoding, _) ->
      (* TODO: This should be fixed or documented *) false
  | Check_size {encoding = _; _} ->
      (* TODO: This should be fixed or documented *) false
  | String_enum _ -> false
  | Array _ -> false
  | List _ -> false
  | Tup _ -> false
  | Tups _ -> false
  | Null -> false
  | Constant _ -> false
  | Bool -> false
  | Int8 -> false
  | Uint8 -> false
  | Int16 _ -> false
  | Uint16 _ -> false
  | Int31 _ -> false
  | Int32 _ -> false
  | Int64 _ -> false
  | N -> false
  | Z -> false
  | RangedInt _ -> false
  | RangedFloat _ -> false
  | Float -> false
  | Bytes _ -> false
  | String _ -> false
  | Bigstring _ -> false

let is_obj e = is_obj Mu_visited.empty e

let rec is_tup : type a. Mu_visited.t -> a t -> bool =
 fun visited e ->
  match e.encoding with
  | Tup _ -> true
  | Tups _ (* by construction *) -> true
  | Conv {encoding = e; _} -> is_tup visited e
  | Dynamic_size {encoding = e; _} -> is_tup visited e
  | Union {cases; _} ->
      List.for_all (function Case {encoding = e; _} -> is_tup visited e) cases
  | Mu {fix; _} ->
      if Mu_visited.mem e.encoding visited then false
      else is_tup (Mu_visited.add e.encoding visited) (fix e)
  | Splitted {is_tup; _} -> is_tup
  | Delayed f -> is_tup visited (f ())
  | Describe {encoding; _} -> is_tup visited encoding
  | Padded (_encoding, _) ->
      (* TODO: This should be fixed or documented *)
      false
  | Check_size {encoding = _; _} ->
      (* TODO: This should be fixed or documented *)
      false
  | String_enum _ -> false
  | Array _ -> false
  | List _ -> false
  | Obj _ -> false
  | Objs _ -> false
  | Empty -> false
  | Ignore -> false
  | Null -> false
  | Constant _ -> false
  | Bool -> false
  | Int8 -> false
  | Uint8 -> false
  | Int16 _ -> false
  | Uint16 _ -> false
  | Int31 _ -> false
  | Int32 _ -> false
  | Int64 _ -> false
  | N -> false
  | Z -> false
  | RangedInt _ -> false
  | RangedFloat _ -> false
  | Float -> false
  | Bytes _ -> false
  | String _ -> false
  | Bigstring _ -> false

let is_tup e = is_tup Mu_visited.empty e

let raw_merge_objs left right =
  let kind = Kind.combine "objects" (classify left) (classify right) in
  make @@ Objs {kind; left; right}

let obj1 f1 = make @@ Obj f1

let obj2 f2 f1 = raw_merge_objs (obj1 f2) (obj1 f1)

let obj3 f3 f2 f1 = raw_merge_objs (obj1 f3) (obj2 f2 f1)

let obj4 f4 f3 f2 f1 = raw_merge_objs (obj2 f4 f3) (obj2 f2 f1)

let obj5 f5 f4 f3 f2 f1 = raw_merge_objs (obj1 f5) (obj4 f4 f3 f2 f1)

let obj6 f6 f5 f4 f3 f2 f1 = raw_merge_objs (obj2 f6 f5) (obj4 f4 f3 f2 f1)

let obj7 f7 f6 f5 f4 f3 f2 f1 =
  raw_merge_objs (obj3 f7 f6 f5) (obj4 f4 f3 f2 f1)

let obj8 f8 f7 f6 f5 f4 f3 f2 f1 =
  raw_merge_objs (obj4 f8 f7 f6 f5) (obj4 f4 f3 f2 f1)

let obj9 f9 f8 f7 f6 f5 f4 f3 f2 f1 =
  raw_merge_objs (obj1 f9) (obj8 f8 f7 f6 f5 f4 f3 f2 f1)

let obj10 f10 f9 f8 f7 f6 f5 f4 f3 f2 f1 =
  raw_merge_objs (obj2 f10 f9) (obj8 f8 f7 f6 f5 f4 f3 f2 f1)

let merge_objs o1 o2 =
  if is_obj o1 && is_obj o2 then raw_merge_objs o1 o2
  else invalid_arg "Json_encoding.merge_objs"

let raw_merge_tups left right =
  let kind = Kind.combine "tuples" (classify left) (classify right) in
  make @@ Tups {kind; left; right}

let tup1 e1 = make @@ Tup e1

let tup2 e2 e1 = raw_merge_tups (tup1 e2) (tup1 e1)

let tup3 e3 e2 e1 = raw_merge_tups (tup1 e3) (tup2 e2 e1)

let tup4 e4 e3 e2 e1 = raw_merge_tups (tup2 e4 e3) (tup2 e2 e1)

let tup5 e5 e4 e3 e2 e1 = raw_merge_tups (tup1 e5) (tup4 e4 e3 e2 e1)

let tup6 e6 e5 e4 e3 e2 e1 = raw_merge_tups (tup2 e6 e5) (tup4 e4 e3 e2 e1)

let tup7 e7 e6 e5 e4 e3 e2 e1 =
  raw_merge_tups (tup3 e7 e6 e5) (tup4 e4 e3 e2 e1)

let tup8 e8 e7 e6 e5 e4 e3 e2 e1 =
  raw_merge_tups (tup4 e8 e7 e6 e5) (tup4 e4 e3 e2 e1)

let tup9 e9 e8 e7 e6 e5 e4 e3 e2 e1 =
  raw_merge_tups (tup1 e9) (tup8 e8 e7 e6 e5 e4 e3 e2 e1)

let tup10 e10 e9 e8 e7 e6 e5 e4 e3 e2 e1 =
  raw_merge_tups (tup2 e10 e9) (tup8 e8 e7 e6 e5 e4 e3 e2 e1)

let merge_tups t1 t2 =
  if is_tup t1 && is_tup t2 then raw_merge_tups t1 t2
  else invalid_arg "Tezos_serial.Encoding.merge_tups"

let conv3 ty =
  conv (fun (c, b, a) -> (c, (b, a))) (fun (c, (b, a)) -> (c, b, a)) ty

let obj3 f3 f2 f1 = conv3 (obj3 f3 f2 f1)

let tup3 f3 f2 f1 = conv3 (tup3 f3 f2 f1)

let conv4 ty =
  conv
    (fun (d, c, b, a) -> ((d, c), (b, a)))
    (fun ((d, c), (b, a)) -> (d, c, b, a))
    ty

let obj4 f4 f3 f2 f1 = conv4 (obj4 f4 f3 f2 f1)

let tup4 f4 f3 f2 f1 = conv4 (tup4 f4 f3 f2 f1)

let conv5 ty =
  conv
    (fun (e, d, c, b, a) -> (e, ((d, c), (b, a))))
    (fun (e, ((d, c), (b, a))) -> (e, d, c, b, a))
    ty

let obj5 f5 f4 f3 f2 f1 = conv5 (obj5 f5 f4 f3 f2 f1)

let tup5 f5 f4 f3 f2 f1 = conv5 (tup5 f5 f4 f3 f2 f1)

let conv6 ty =
  conv
    (fun (f, e, d, c, b, a) -> ((f, e), ((d, c), (b, a))))
    (fun ((f, e), ((d, c), (b, a))) -> (f, e, d, c, b, a))
    ty

let obj6 f6 f5 f4 f3 f2 f1 = conv6 (obj6 f6 f5 f4 f3 f2 f1)

let tup6 f6 f5 f4 f3 f2 f1 = conv6 (tup6 f6 f5 f4 f3 f2 f1)

let conv7 ty =
  conv
    (fun (g, f, e, d, c, b, a) -> ((g, (f, e)), ((d, c), (b, a))))
    (fun ((g, (f, e)), ((d, c), (b, a))) -> (g, f, e, d, c, b, a))
    ty

let obj7 f7 f6 f5 f4 f3 f2 f1 = conv7 (obj7 f7 f6 f5 f4 f3 f2 f1)

let tup7 f7 f6 f5 f4 f3 f2 f1 = conv7 (tup7 f7 f6 f5 f4 f3 f2 f1)

let conv8 ty =
  conv
    (fun (h, g, f, e, d, c, b, a) -> (((h, g), (f, e)), ((d, c), (b, a))))
    (fun (((h, g), (f, e)), ((d, c), (b, a))) -> (h, g, f, e, d, c, b, a))
    ty

let obj8 f8 f7 f6 f5 f4 f3 f2 f1 = conv8 (obj8 f8 f7 f6 f5 f4 f3 f2 f1)

let tup8 f8 f7 f6 f5 f4 f3 f2 f1 = conv8 (tup8 f8 f7 f6 f5 f4 f3 f2 f1)

let conv9 ty =
  conv
    (fun (i, h, g, f, e, d, c, b, a) ->
      (i, (((h, g), (f, e)), ((d, c), (b, a)))))
    (fun (i, (((h, g), (f, e)), ((d, c), (b, a)))) ->
      (i, h, g, f, e, d, c, b, a))
    ty

let obj9 f9 f8 f7 f6 f5 f4 f3 f2 f1 = conv9 (obj9 f9 f8 f7 f6 f5 f4 f3 f2 f1)

let tup9 f9 f8 f7 f6 f5 f4 f3 f2 f1 = conv9 (tup9 f9 f8 f7 f6 f5 f4 f3 f2 f1)

let conv10 ty =
  conv
    (fun (j, i, h, g, f, e, d, c, b, a) ->
      ((j, i), (((h, g), (f, e)), ((d, c), (b, a)))))
    (fun ((j, i), (((h, g), (f, e)), ((d, c), (b, a)))) ->
      (j, i, h, g, f, e, d, c, b, a))
    ty

let obj10 f10 f9 f8 f7 f6 f5 f4 f3 f2 f1 =
  conv10 (obj10 f10 f9 f8 f7 f6 f5 f4 f3 f2 f1)

let tup10 f10 f9 f8 f7 f6 f5 f4 f3 f2 f1 =
  conv10 (tup10 f10 f9 f8 f7 f6 f5 f4 f3 f2 f1)

let undefined_encoding = delayed (fun _ -> assert false)

let undefined_proj _ = None

let undefined_inj _ = assert false

let undefined_case : type a. a case =
  Case
    {
      title = "<YOU SHOULD NEVER SEE THAT>";
      description = None;
      encoding = undefined_encoding;
      proj = undefined_proj;
      inj = undefined_inj;
      tag = Uint_option.none;
    }

let is_undefined_case c = c == undefined_case

let valid_tag tag_size t =
  let max_tag = Binary_size.max_int tag_size in
  if t > max_tag then
    Format.kasprintf
      invalid_arg
      "The tag %d is invalid because it should be less than %d."
      t
      max_tag

let matching ?(tag_size = `Uint8) match_case cases =
  if cases = [] then invalid_arg "Data_encoding.union: empty list of cases." ;
  let tagged_cases_list =
    List.filter
      (fun (Case {tag; _}) ->
        is_tag tag
        &&
        (valid_tag tag_size (get_tag tag) ;
         true))
      cases
  in
  (* In [tagged_cases_list] all tags are [some] so [get] cannot fail *)
  let max_used_tag =
    List.fold_left
      (fun m (Case {tag; _}) -> max (Uint_option.get tag) m)
      (-1)
      tagged_cases_list
  in
  let tagged_cases = Array.make (max_used_tag + 1) undefined_case in
  List.iter
    (fun (Case {tag; _} as case) ->
      let tag = Uint_option.get tag in
      if not (is_undefined_case tagged_cases.(tag)) then
        Format.kasprintf invalid_arg "The tag %d appears twice in an union." tag ;
      tagged_cases.(tag) <- case)
    tagged_cases_list ;
  let classify_case (Case {encoding; _}) = classify encoding in
  let kinds = List.map classify_case cases in
  let kind = Kind.merge_list tag_size kinds in
  make @@ Union {kind; tag_size; tagged_cases; match_case; cases}

let union ?(tag_size = `Uint8) cases =
  let match_case =
    let acases =
      Array.of_list @@ List.filter (fun (Case {tag; _}) -> is_tag tag) cases
    in
    fun x ->
      let rec find i =
        if i >= Array.length acases then
          raise Binary_error_types.(Write_error No_case_matched)
        else
          let (Case {tag; encoding; proj; _}) = acases.(i) in
          match proj x with
          | None -> find (i + 1)
          | Some v ->
              (* By definition of [acases], the following [get] cannot fail. *)
              Matched (Uint_option.get tag, encoding, v)
      in
      find 0
  in
  matching ~tag_size match_case cases

let case ~title ?description tag encoding proj inj =
  let tag =
    match tag with
    | Tag t ->
        if t < 0 then raise (Invalid_argument "Data_encoding.tag: negative tag")
        else make_tag t
    | Json_only -> json_only
  in
  Case {title; description; encoding; proj; inj; tag}

let matched ?(tag_size : [`Uint8 | `Uint16] = `Uint8) tag encoding v =
  if tag < 0 then raise (Invalid_argument "Data_encoding.matched: negative tag") ;
  valid_tag tag_size tag ;
  Matched (tag, encoding, v)

let rec is_nullable : type t. Mu_visited.t -> t encoding -> bool =
 fun visited e ->
  match e.encoding with
  | Null -> true
  | Empty -> false
  | Ignore -> true
  | Constant _ -> false
  | Bool -> false
  | Int8 -> false
  | Uint8 -> false
  | Int16 _ -> false
  | Uint16 _ -> false
  | Int31 _ -> false
  | Int32 _ -> false
  | Int64 _ -> false
  | N -> false
  | Z -> false
  | RangedInt _ -> false
  | RangedFloat _ -> false
  | Float -> false
  | Bytes _ -> false
  | String _ -> false
  | Bigstring _ -> false
  | Padded (e, _) -> is_nullable visited e
  | String_enum _ -> false
  | Array _ -> false
  | List _ -> false
  | Obj _ -> false
  | Objs _ -> false
  | Tup _ -> false
  | Tups _ -> false
  | Union {cases; _} ->
      List.exists (fun (Case {encoding = e; _}) -> is_nullable visited e) cases
  | Mu {fix; _} ->
      if Mu_visited.mem e.encoding visited then false
      else is_nullable (Mu_visited.add e.encoding visited) (fix e)
  | Conv {encoding = e; _} -> is_nullable visited e
  | Describe {encoding = e; _} -> is_nullable visited e
  | Splitted {json_encoding; _} -> Json_encoding.is_nullable json_encoding
  | Dynamic_size {encoding = e; _} -> is_nullable visited e
  | Check_size {encoding = e; _} -> is_nullable visited e
  | Delayed _ -> true

let is_nullable e = is_nullable Mu_visited.empty e

let option ty =
  if is_nullable ty then
    invalid_arg "Data_encoding.option: cannot nest nullable encodings" ;
  (* TODO add a special construct `Option` in the GADT *)
  matching
    ~tag_size:`Uint8
    (function Some x -> Matched (1, ty, x) | None -> Matched (0, null, ()))
    [
      case (Tag 1) ty ~title:"Some" (fun x -> x) (fun x -> Some x);
      case
        (Tag 0)
        null
        ~title:"None"
        (function None -> Some () | Some _ -> None)
        (fun () -> None);
    ]

let mu name ?title ?description fix =
  (* The latest application of [fix] is memoized to avoid recomputing
     it each time a value encoded by [mu] is processed. [fix] may
     sometimes be applied to distinct arguments if the encoding is used
     in different contexts. Hence, we remember the last input of [fix]
     in the closure, so that we can recompute [fix] if this argument
     has changed.

     This partial memoization only takes a bounded amount of memory
     and is useful because in practice we decode many values before
     applying [fix] to a new argument. *)
  let fixing = ref 0 in
  let self = ref None in
  let fix e =
    match !self with
    | Some (e0, e') when e == e0 -> e'
    | Some _ | None ->
        (* The limit is 2 because we can be forcing it once in binary and once
           in json "at the same time" in case of a splitted encoding. *)
        if !fixing >= 2 then
          invalid_arg "infinite recursion in mu initialisation"
        else (
          incr fixing ;
          let e' = fix e in
          self := Some (e, e') ;
          e')
  in
  let fix e = Fun.protect ~finally:(fun () -> fixing := 0) (fun () -> fix e) in
  (* Attempt to determine the kind. Note that this can result in memoisation
     misses: the [fix] function might be called multiple times. *)
  try
    let precursor =
      make @@ Mu {kind = `Dynamic; name; title; description; fix}
    in
    let fixed_precursor = fix precursor in
    match classify fixed_precursor with
    | `Fixed _ | `Dynamic ->
        (*
            One could wonder why we return [precursor] instead of [fixed_precursor].
            For full historical context, check out
            https://gitlab.com/nomadic-labs/data-encoding/-/merge_requests/88
            TL;DR: when [mu] is used in conjunction with a [union] (common case),
            the [fixed_precursor] has one level expanded which makes for an
            unecessarily large [encoding] value.
        *)
        precursor
    | `Variable -> raise Exit
  with
  | (Out_of_memory | Stack_overflow) as e -> raise e
  | Exit | _ (* TODO variability error *) ->
      let precursor =
        make @@ Mu {kind = `Variable; name; title; description; fix}
      in
      let fixed_precursor = fix precursor in
      ignore (classify fixed_precursor : Kind.t) ;
      (* See comment above about [precursor] versus [fixed_precursor] *)
      precursor

let result ok_enc error_enc =
  let ok_enc = obj1 (req "ok" ok_enc) in
  let error_enc = obj1 (req "error" error_enc) in
  matching
    ~tag_size:`Uint8
    (function
      | Ok x -> Matched (1, ok_enc, x) | Error x -> Matched (0, error_enc, x))
    [
      case
        (Tag 1)
        ok_enc
        ~title:"Ok"
        (function Ok x -> Some x | Error _ -> None)
        (fun x -> Ok x);
      case
        (Tag 0)
        error_enc
        ~title:"Result"
        (function Ok _ -> None | Error x -> Some x)
        (fun x -> Error x);
    ]

let length_encoding_of_length_encoding_parameter max_length = function
  | `N -> uint_like_n ~max_value:max_length
  | `Uint8 -> uint8
  | `Uint16 -> uint16
  | `Uint30 -> ranged_int 0 (Binary_size.max_int `Uint30)

let array_with_length ?max_length length_encoding e =
  let effective_max_length =
    match length_encoding with
    | `N | `Uint30 -> Binary_size.max_int `Uint30
    | `Uint16 -> Binary_size.max_int `Uint16
    | `Uint8 -> Binary_size.max_int `Uint8
  in
  let max_length =
    match max_length with
    | None -> effective_max_length
    | Some l ->
        if l > effective_max_length then
          raise
            (Invalid_argument
               "Data_encoding.array_with_length: explicit max_length higher \
                than effective length range") ;
        l
  in
  let length_encoding =
    length_encoding_of_length_encoding_parameter max_length length_encoding
  in
  let length_limit = At_most max_length in
  check_not_variable "an array" e ;
  check_not_zeroable "an array" e ;
  make @@ Array {length_limit; length_encoding = Some length_encoding; elts = e}

let list_with_length ?max_length length_encoding e =
  let effective_max_length =
    match length_encoding with
    | `N | `Uint30 -> Binary_size.max_int `Uint30
    | `Uint16 -> Binary_size.max_int `Uint16
    | `Uint8 -> Binary_size.max_int `Uint8
  in
  let max_length =
    match max_length with
    | None -> effective_max_length
    | Some l ->
        if l > effective_max_length then
          raise
            (Invalid_argument
               "Data_encoding.list_with_length: explicit max_length higher \
                than effective length range") ;
        l
  in
  let length_encoding =
    length_encoding_of_length_encoding_parameter max_length length_encoding
  in
  let length_limit = At_most max_length in
  check_not_variable "a list" e ;
  check_not_zeroable "a list" e ;
  make @@ List {length_limit; length_encoding = Some length_encoding; elts = e}
