(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, Inc. <contact@nomadic-labs.com>          *)
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

(* NOTE: the current release of Crowbar, v0.1, is quite limited. Several
 * improvements have been made to the dev version which will make it possible to
 * simplify this file and increase coverage.
 * For now, this is a limited test-suite. *)

[@@@ocaml.warning "-32"]

let char = Crowbar.map [Crowbar.uint8] Char.chr

let string = Crowbar.bytes

(* The v0.1 of Crowbar doesn't have fixed-size string generation. When we
 * update Crowbar, we can improve this generator. *)
let short_string =
  let open Crowbar in
  choose
    [
      const "";
      map [char] (fun c -> String.make 1 c);
      map [char; char; char; char] (fun c1 c2 c3 c4 ->
          let s = Bytes.make 4 c1 in
          Bytes.set s 1 c2 ;
          Bytes.set s 2 c3 ;
          Bytes.set s 3 c4 ;
          Bytes.to_string s);
    ]

let short_string1 =
  let open Crowbar in
  choose
    [
      map [char] (fun c -> String.make 1 c);
      map [char; char; char; char] (fun c1 c2 c3 c4 ->
          let s = Bytes.make 4 c1 in
          Bytes.set s 1 c2 ;
          Bytes.set s 2 c3 ;
          Bytes.set s 3 c4 ;
          Bytes.to_string s);
    ]

let mbytes = Crowbar.map [Crowbar.bytes] Bytes.of_string

let short_mbytes = Crowbar.map [short_string] Bytes.of_string

let short_mbytes1 = Crowbar.map [short_string1] Bytes.of_string

(* We need to hide the type parameter of `Encoding.t` to avoid the generator
 * combinator `choose` from complaining about different types. We use first
 * class modules (for now) to encode existentials.
 *
 * An alternative is used in https://gitlab.com/gasche/fuzz-data-encoding *)

(* The type for first-class modules used to encode existentials as expalined
   above. *)
module type TESTABLE = sig
  type t

  val v : t

  val ding : t Json_encoding.encoding

  val pp : t Crowbar.printer

  val eq : t -> t -> bool
end

type testable = (module TESTABLE)

let null : testable =
  (module struct
    type t = unit

    let v = ()

    let ding = Json_encoding.null

    let pp ppf () = Crowbar.pp ppf "(null)"

    let eq () () = true
  end)

let empty : testable =
  (module struct
    type t = unit

    let v = ()

    let ding = Json_encoding.empty

    let pp ppf () = Crowbar.pp ppf "(empty)"

    let eq () () = true
  end)

let unit : testable =
  (module struct
    type t = unit

    let v = ()

    let ding = Json_encoding.unit

    let pp ppf () = Crowbar.pp ppf "(unit)"

    let eq () () = true
  end)

let map_constant (s : string) : testable =
  (module struct
    type t = unit

    let v = ()

    let ding = Json_encoding.constant s

    let pp ppf () = Crowbar.pp ppf "\"%s\"" s

    let eq () () = true
  end)

let map_int32 (i : int32) : testable =
  (module struct
    type t = int32

    let v = i

    let ding = Json_encoding.int32

    let pp = Crowbar.pp_int32

    let eq = Int32.equal
  end)

let map_int32_conv (i : int32) : testable =
  (module struct
    type t = int32

    let v = i

    let ding = Json_encoding.(conv Int32.succ Int32.pred int32)

    let pp = Crowbar.pp_int32

    let eq = Int32.equal
  end)

let map_int32_list (i : int32) : testable =
  (module struct
    type t = int32 list

    let v =
      let open Int32 in
      [i; add i 1l; add i 2l; add i 4l; add i 8l; add i 16l]

    let ding = Json_encoding.(list int32)

    let pp = Crowbar.(pp_list pp_int32)

    let eq = List.for_all2 Int32.equal
  end)

let map_int32_seq (i : int32) : testable =
  (module struct
    type t = int32 Seq.t

    let v =
      let open Int32 in
      List.to_seq [i; add i 1l; add i 2l; add i 4l; add i 8l; add i 16l]

    let ding = Json_encoding.(seq int32)

    let pp fmt s = Crowbar.(pp_list pp_int32) fmt (List.of_seq s)

    let eq s1 s2 = List.for_all2 Int32.equal (List.of_seq s1) (List.of_seq s2)
  end)

let lower_bound_53 = Int64.(neg @@ shift_left 1L 53)

let upper_bound_53 = Int64.shift_left 1L 53

let map_int53 (i : int64) : testable =
  let clipped = max lower_bound_53 (min i upper_bound_53) in
  (module struct
    type t = int64

    let v = clipped

    let ding = Json_encoding.int53

    let pp = Crowbar.pp_int64

    let eq = Int64.equal
  end)

let map_range_int a b c : testable =
  let small, middle, big =
    match List.sort compare [a; b; c] with
    | [small; middle; big] ->
        assert (small <= middle) ;
        assert (middle <= big) ;
        (small, middle, big)
    | _ -> assert false
  in
  (module struct
    type t = int

    let v = middle

    let name = Format.asprintf "ranged(%d-%d-%d)" small middle big

    let ding = Json_encoding.ranged_int ~minimum:small ~maximum:big name

    let pp ppf i = Crowbar.pp ppf "(%d :[%d;%d])" i small big

    let eq = Int.equal
  end)

let map_range_float a b c : testable =
  if compare a nan = 0 || compare b nan = 0 || compare c nan = 0 then
    (* copout *)
    null
  else
    let small, middle, big =
      match List.sort compare [a; b; c] with
      | [small; middle; big] ->
          assert (small <= middle) ;
          assert (middle <= big) ;
          (small, middle, big)
      | _ -> assert false
    in
    (module struct
      type t = float

      let v = middle

      let name = Format.asprintf "ranged(%f-%f-%f)" small middle big

      let ding = Json_encoding.ranged_float ~minimum:small ~maximum:big name

      let pp ppf i = Crowbar.pp ppf "(%f :[%f;%f])" i small big

      let eq = Float.equal
    end)

let map_bool b : testable =
  (module struct
    type t = bool

    let v = b

    let ding = Json_encoding.bool

    let pp = Crowbar.pp_bool

    let eq = Bool.equal
  end)

let map_string s : testable =
  (module struct
    type t = string

    let v = s

    let ding = Json_encoding.string

    let pp = Crowbar.pp_string

    let eq = String.equal
  end)

let map_bytes s : testable =
  (module struct
    type t = Bytes.t

    let v = s

    let ding = Json_encoding.bytes

    let pp fmt b = Crowbar.pp_string fmt (Bytes.to_string b)

    let eq = Bytes.equal
  end)

let map_float f : testable =
  (module struct
    type t = float

    let v = f

    let ding = Json_encoding.float

    let pp = Crowbar.pp_float

    let eq = Float.equal
  end)

(* And now combinators *)

(* To avoid name collisions we have this generator. *)
let new_name =
  let r = ref 0 in
  fun () ->
    incr r ;
    "n" ^ string_of_int !r

let enum (n : int) : testable =
  assert (0 <= n) ;
  assert (n < 3) ;
  (module struct
    type t = Zilch | Yi | Dos

    let v = if n = 0 then Zilch else if n = 1 then Yi else Dos

    let ding =
      Json_encoding.string_enum
        [(new_name (), Zilch); (new_name (), Yi); (new_name (), Dos)]

    let pp ppf = function
      | Zilch -> Crowbar.pp_string ppf "Zilch"
      | Yi -> Crowbar.pp_string ppf "Yi"
      | Dos -> Crowbar.pp_string ppf "Dos"

    let eq = ( = )
  end)

let map_def (t : testable) : testable =
  let module T = (val t) in
  let name = new_name () in
  (module struct
    type t = T.t

    let v = T.v

    let ding = Json_encoding.def name T.ding

    let pp = T.pp

    let eq = T.eq
  end)

let map_conv_id (t : testable) : testable =
  let module T = (val t) in
  (module struct
    type t = T.t

    let v = T.v

    let ding = Json_encoding.conv Fun.id Fun.id T.ding

    let pp = T.pp

    let eq = T.eq
  end)

let map_conv_obj (t : testable) : testable =
  let module T = (val t) in
  (module struct
    type t = T.t

    let v = T.v

    let ding =
      let open Json_encoding in
      conv
        (fun v -> (v, ()))
        (fun (v, ()) -> v)
        (obj2 (req (new_name ()) T.ding) (req (new_name ()) empty))

    let pp = T.pp

    let eq = T.eq
  end)

let map_conv_obj_dft (t : testable) : testable =
  let module T = (val t) in
  (module struct
    type t = T.t

    let v = T.v

    let ding =
      let open Json_encoding in
      conv
        (fun v -> (v, ()))
        (fun (v, ()) -> v)
        (obj2
           (dft ~equal:T.eq (new_name ()) T.ding T.v)
           (req (new_name ()) empty))

    let pp = T.pp

    let eq = T.eq
  end)

let map_conv_obj_dft_construct (t : testable) : testable =
  let module T = (val t) in
  (module struct
    type t = T.t

    let v = T.v

    let ding =
      let open Json_encoding in
      conv
        (fun v -> (v, ()))
        (fun (v, ()) -> v)
        (obj2
           (dft ~equal:T.eq ~construct:true (new_name ()) T.ding T.v)
           (req (new_name ()) empty))

    let pp = T.pp

    let eq = T.eq
  end)

let map_conv_singleton_union (t : testable) : testable =
  let module T = (val t) in
  (module struct
    type t = T.t

    let v = T.v

    let ding =
      let open Json_encoding in
      union [case T.ding (fun x -> Some x) (fun x -> x)]

    let pp = T.pp

    let eq = T.eq
  end)

let map_mu_dup_list (t : testable) : testable =
  let module T = (val t) in
  (module struct
    type t = T.t list

    let v = [T.v; T.v]

    let ding =
      let open Json_encoding in
      mu (new_name ()) (fun mul ->
          union
            [
              case
                null
                (function [] -> Some () | _ :: _ -> None)
                (function () -> []);
              case
                (tup2 T.ding mul)
                (function x :: xs -> Some (x, xs) | [] -> None)
                (function x, xs -> x :: xs);
            ])

    let pp fmt = function
      | [v; w] -> Format.fprintf fmt "[%a; %a]" T.pp v T.pp w
      | _ -> assert false

    let eq = List.for_all2 T.eq
  end)

let map_singleton_list (t : testable) : testable =
  let module T = (val t) in
  (module struct
    type t = T.t list

    let v = [T.v]

    let ding = Json_encoding.list T.ding

    let pp fmt = function
      | [v] -> Format.fprintf fmt "[%a]" T.pp v
      | _ -> assert false

    let eq = List.for_all2 T.eq
  end)

let map_dup_list (t : testable) : testable =
  let module T = (val t) in
  (module struct
    type t = T.t list

    let v = [T.v; T.v]

    let ding = Json_encoding.list T.ding

    let pp fmt = function
      | [v; w] -> Format.fprintf fmt "[%a; %a]" T.pp v T.pp w
      | _ -> assert false

    let eq = List.for_all2 T.eq
  end)

let map_singleton_seq (t : testable) : testable =
  let module T = (val t) in
  (module struct
    type t = T.t Seq.t

    let v = List.to_seq [T.v]

    let ding = Json_encoding.seq T.ding

    let pp fmt s =
      match List.of_seq s with
      | [v] -> Format.fprintf fmt "[%a]" T.pp v
      | _ -> assert false

    let eq s1 s2 = List.for_all2 T.eq (List.of_seq s1) (List.of_seq s2)
  end)

let map_dup_seq (t : testable) : testable =
  let module T = (val t) in
  (module struct
    type t = T.t Seq.t

    let v = List.to_seq [T.v; T.v]

    let ding = Json_encoding.seq T.ding

    let pp fmt s =
      match List.of_seq s with
      | [v; w] -> Format.fprintf fmt "[%a; %a]" T.pp v T.pp w
      | _ -> assert false

    let eq s1 s2 = List.for_all2 T.eq (List.of_seq s1) (List.of_seq s2)
  end)

let map_some (t : testable) : testable =
  let module T = (val t) in
  (module struct
    type t = T.t option

    let v = Some T.v

    let ding =
      try Json_encoding.option T.ding
      with Invalid_argument _ -> Crowbar.bad_test ()

    let pp ppf o =
      Crowbar.pp
        ppf
        "@[<hv 1>%a@]"
        (fun fmt v ->
          match v with
          | None -> Format.fprintf fmt "None"
          | Some v -> Format.fprintf fmt "Some(%a)" T.pp v)
        o

    let eq = Option.equal T.eq
  end)

let map_none (t : testable) : testable =
  let module T = (val t) in
  (module struct
    type t = T.t option

    let v = None

    let ding =
      try Json_encoding.option T.ding
      with Invalid_argument _ -> Crowbar.bad_test ()

    let pp ppf o =
      Crowbar.pp
        ppf
        "@[<hv 1>%a@]"
        (fun fmt v ->
          match v with
          | None -> Format.fprintf fmt "None"
          | Some v -> Format.fprintf fmt "Some(%a)" T.pp v)
        o

    let eq = Option.equal T.eq
  end)

let map_list (t : testable) (ts : testable list) : testable =
  let module T = (val t) in
  (module struct
    type t = T.t list

    let ding = Json_encoding.list T.ding

    let v =
      List.fold_left
        (fun acc (t : testable) ->
          let module T = (val t) in
          (* We can get rid of this Obj when we update Crowbar *)
          Obj.magic T.v :: acc)
        []
        ts

    let pp = Crowbar.pp_list T.pp

    let eq = List.for_all2 T.eq
  end)

let map_array (t : testable) (ts : testable array) : testable =
  let module T = (val t) in
  (module struct
    type t = T.t array

    let ding = Json_encoding.array T.ding

    let v =
      Array.of_list
        (Array.fold_left
           (fun acc (t : testable) ->
             let module T = (val t) in
             Obj.magic T.v :: acc)
           []
           ts)

    let pp ppf a =
      if Array.length a > 40 then
        Crowbar.pp
          ppf
          "@[<hv 1>[|%a … (%d more elements)|]@]"
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
             T.pp)
          (Array.to_list (Array.sub a 0 30))
          (Array.length a)
      else
        Crowbar.pp
          ppf
          "@[<hv 1>[|%a|]@]"
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
             T.pp)
          (Array.to_list a)

    let eq a1 a2 =
      Array.length a1 = Array.length a2
      &&
      try
        for i = 0 to Array.length a1 - 1 do
          if not (T.eq a1.(i) a2.(i)) then raise Exit
        done ;
        true
      with Exit -> false
  end)

let map_obj1 (t1 : testable) : testable =
  let module T1 = (val t1) in
  (module struct
    include T1

    let name1 = new_name ()

    let ding = Json_encoding.(obj1 (req name1 T1.ding))

    let pp ppf v1 = Crowbar.pp ppf "@[<hv 1>(%s: %a)@]" name1 T1.pp v1
  end)

let map_obj2 (t1 : testable) (t2 : testable) : testable =
  let module T1 = (val t1) in
  let module T2 = (val t2) in
  (module struct
    type t = T1.t * T2.t

    let v = (T1.v, T2.v)

    let name1 = new_name ()

    let name2 = new_name ()

    let ding = Json_encoding.(obj2 (req name1 T1.ding) (req name2 T2.ding))

    let pp ppf (v1, v2) =
      Crowbar.pp ppf "@[<hv 1>(%s: %a, %s: %a)@]" name1 T1.pp v1 name2 T2.pp v2

    let eq (a1, a2) (b1, b2) = T1.eq a1 b1 && T2.eq a2 b2
  end)

let map_tup1 (t1 : testable) : testable =
  let module T1 = (val t1) in
  (module struct
    include T1

    let ding = Json_encoding.tup1 T1.ding

    let pp ppf v1 = Crowbar.pp ppf "@[<hv 1>(%a)@]" T1.pp v1
  end)

let map_tup2 (t1 : testable) (t2 : testable) : testable =
  let module T1 = (val t1) in
  let module T2 = (val t2) in
  (module struct
    type t = T1.t * T2.t

    let ding = Json_encoding.tup2 T1.ding T2.ding

    let v = (T1.v, T2.v)

    let pp ppf (v1, v2) = Crowbar.pp ppf "@[<hv 1>(%a, %a)@]" T1.pp v1 T2.pp v2

    let eq (a1, a2) (b1, b2) = T1.eq a1 b1 && T2.eq a2 b2
  end)

let map_tup3 (t1 : testable) (t2 : testable) (t3 : testable) : testable =
  let module T1 = (val t1) in
  let module T2 = (val t2) in
  let module T3 = (val t3) in
  (module struct
    type t = T1.t * T2.t * T3.t

    let ding = Json_encoding.tup3 T1.ding T2.ding T3.ding

    let v = (T1.v, T2.v, T3.v)

    let pp ppf (v1, v2, v3) =
      Crowbar.pp ppf "@[<hv 1>(%a, %a, %a)@]" T1.pp v1 T2.pp v2 T3.pp v3

    let eq (a1, a2, a3) (b1, b2, b3) = T1.eq a1 b1 && T2.eq a2 b2 && T3.eq a3 b3
  end)

let map_tup4 (t1 : testable) (t2 : testable) (t3 : testable) (t4 : testable) :
    testable =
  let module T1 = (val t1) in
  let module T2 = (val t2) in
  let module T3 = (val t3) in
  let module T4 = (val t4) in
  (module struct
    type t = T1.t * T2.t * T3.t * T4.t

    let ding = Json_encoding.tup4 T1.ding T2.ding T3.ding T4.ding

    let v = (T1.v, T2.v, T3.v, T4.v)

    let pp ppf (v1, v2, v3, v4) =
      Crowbar.pp
        ppf
        "@[<hv 1>(%a, %a, %a, %a)@]"
        T1.pp
        v1
        T2.pp
        v2
        T3.pp
        v3
        T4.pp
        v4

    let eq (a1, a2, a3, a4) (b1, b2, b3, b4) =
      T1.eq a1 b1 && T2.eq a2 b2 && T3.eq a3 b3 && T4.eq a4 b4
  end)

let map_tup5 (t1 : testable) (t2 : testable) (t3 : testable) (t4 : testable)
    (t5 : testable) : testable =
  let module T1 = (val t1) in
  let module T2 = (val t2) in
  let module T3 = (val t3) in
  let module T4 = (val t4) in
  let module T5 = (val t5) in
  (module struct
    type t = T1.t * T2.t * T3.t * T4.t * T5.t

    let ding = Json_encoding.tup5 T1.ding T2.ding T3.ding T4.ding T5.ding

    let v = (T1.v, T2.v, T3.v, T4.v, T5.v)

    let pp ppf (v1, v2, v3, v4, v5) =
      Crowbar.pp
        ppf
        "@[<hv 1>(%a, %a, %a, %a, %a)@]"
        T1.pp
        v1
        T2.pp
        v2
        T3.pp
        v3
        T4.pp
        v4
        T5.pp
        v5

    let eq (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5) =
      T1.eq a1 b1 && T2.eq a2 b2 && T3.eq a3 b3 && T4.eq a4 b4 && T5.eq a5 b5
  end)

let map_tup6 (t1 : testable) (t2 : testable) (t3 : testable) (t4 : testable)
    (t5 : testable) (t6 : testable) : testable =
  let module T1 = (val t1) in
  let module T2 = (val t2) in
  let module T3 = (val t3) in
  let module T4 = (val t4) in
  let module T5 = (val t5) in
  let module T6 = (val t6) in
  (module struct
    type t = T1.t * T2.t * T3.t * T4.t * T5.t * T6.t

    let ding =
      Json_encoding.tup6 T1.ding T2.ding T3.ding T4.ding T5.ding T6.ding

    let v = (T1.v, T2.v, T3.v, T4.v, T5.v, T6.v)

    let pp ppf (v1, v2, v3, v4, v5, v6) =
      Crowbar.pp
        ppf
        "@[<hv 1>(%a, %a, %a, %a, %a, %a)@]"
        T1.pp
        v1
        T2.pp
        v2
        T3.pp
        v3
        T4.pp
        v4
        T5.pp
        v5
        T6.pp
        v6

    let eq (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6) =
      T1.eq a1 b1 && T2.eq a2 b2 && T3.eq a3 b3 && T4.eq a4 b4 && T5.eq a5 b5
      && T6.eq a6 b6
  end)

let map_tup7 (t1 : testable) (t2 : testable) (t3 : testable) (t4 : testable)
    (t5 : testable) (t6 : testable) (t7 : testable) : testable =
  let module T1 = (val t1) in
  let module T2 = (val t2) in
  let module T3 = (val t3) in
  let module T4 = (val t4) in
  let module T5 = (val t5) in
  let module T6 = (val t6) in
  let module T7 = (val t7) in
  (module struct
    type t = T1.t * T2.t * T3.t * T4.t * T5.t * T6.t * T7.t

    let ding =
      Json_encoding.tup7 T1.ding T2.ding T3.ding T4.ding T5.ding T6.ding T7.ding

    let v = (T1.v, T2.v, T3.v, T4.v, T5.v, T6.v, T7.v)

    let pp ppf (v1, v2, v3, v4, v5, v6, v7) =
      Crowbar.pp
        ppf
        "@[<hv 1>(%a, %a, %a, %a, %a, %a, %a)@]"
        T1.pp
        v1
        T2.pp
        v2
        T3.pp
        v3
        T4.pp
        v4
        T5.pp
        v5
        T6.pp
        v6
        T7.pp
        v7

    let eq (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7) =
      T1.eq a1 b1 && T2.eq a2 b2 && T3.eq a3 b3 && T4.eq a4 b4 && T5.eq a5 b5
      && T6.eq a6 b6 && T7.eq a7 b7
  end)

let map_tup8 (t1 : testable) (t2 : testable) (t3 : testable) (t4 : testable)
    (t5 : testable) (t6 : testable) (t7 : testable) (t8 : testable) : testable =
  let module T1 = (val t1) in
  let module T2 = (val t2) in
  let module T3 = (val t3) in
  let module T4 = (val t4) in
  let module T5 = (val t5) in
  let module T6 = (val t6) in
  let module T7 = (val t7) in
  let module T8 = (val t8) in
  (module struct
    type t = T1.t * T2.t * T3.t * T4.t * T5.t * T6.t * T7.t * T8.t

    let ding =
      Json_encoding.tup8
        T1.ding
        T2.ding
        T3.ding
        T4.ding
        T5.ding
        T6.ding
        T7.ding
        T8.ding

    let v = (T1.v, T2.v, T3.v, T4.v, T5.v, T6.v, T7.v, T8.v)

    let pp ppf (v1, v2, v3, v4, v5, v6, v7, v8) =
      Crowbar.pp
        ppf
        "@[<hv 1>(%a, %a, %a, %a, %a, %a, %a, %a)@]"
        T1.pp
        v1
        T2.pp
        v2
        T3.pp
        v3
        T4.pp
        v4
        T5.pp
        v5
        T6.pp
        v6
        T7.pp
        v7
        T8.pp
        v8

    let eq (a1, a2, a3, a4, a5, a6, a7, a8) (b1, b2, b3, b4, b5, b6, b7, b8) =
      T1.eq a1 b1 && T2.eq a2 b2 && T3.eq a3 b3 && T4.eq a4 b4 && T5.eq a5 b5
      && T6.eq a6 b6 && T7.eq a7 b7 && T8.eq a8 b8
  end)

let map_tup9 (t1 : testable) (t2 : testable) (t3 : testable) (t4 : testable)
    (t5 : testable) (t6 : testable) (t7 : testable) (t8 : testable)
    (t9 : testable) : testable =
  let module T1 = (val t1) in
  let module T2 = (val t2) in
  let module T3 = (val t3) in
  let module T4 = (val t4) in
  let module T5 = (val t5) in
  let module T6 = (val t6) in
  let module T7 = (val t7) in
  let module T8 = (val t8) in
  let module T9 = (val t9) in
  (module struct
    type t = T1.t * T2.t * T3.t * T4.t * T5.t * T6.t * T7.t * T8.t * T9.t

    let ding =
      Json_encoding.tup9
        T1.ding
        T2.ding
        T3.ding
        T4.ding
        T5.ding
        T6.ding
        T7.ding
        T8.ding
        T9.ding

    let v = (T1.v, T2.v, T3.v, T4.v, T5.v, T6.v, T7.v, T8.v, T9.v)

    let pp ppf (v1, v2, v3, v4, v5, v6, v7, v8, v9) =
      Crowbar.pp
        ppf
        "@[<hv 1>(%a, %a, %a, %a, %a, %a, %a, %a, %a)@]"
        T1.pp
        v1
        T2.pp
        v2
        T3.pp
        v3
        T4.pp
        v4
        T5.pp
        v5
        T6.pp
        v6
        T7.pp
        v7
        T8.pp
        v8
        T9.pp
        v9

    let eq (a1, a2, a3, a4, a5, a6, a7, a8, a9)
        (b1, b2, b3, b4, b5, b6, b7, b8, b9) =
      T1.eq a1 b1 && T2.eq a2 b2 && T3.eq a3 b3 && T4.eq a4 b4 && T5.eq a5 b5
      && T6.eq a6 b6 && T7.eq a7 b7 && T8.eq a8 b8 && T9.eq a9 b9
  end)

let map_tup10 (t1 : testable) (t2 : testable) (t3 : testable) (t4 : testable)
    (t5 : testable) (t6 : testable) (t7 : testable) (t8 : testable)
    (t9 : testable) (t10 : testable) : testable =
  let module T1 = (val t1) in
  let module T2 = (val t2) in
  let module T3 = (val t3) in
  let module T4 = (val t4) in
  let module T5 = (val t5) in
  let module T6 = (val t6) in
  let module T7 = (val t7) in
  let module T8 = (val t8) in
  let module T9 = (val t9) in
  let module T10 = (val t10) in
  (module struct
    type t =
      T1.t * T2.t * T3.t * T4.t * T5.t * T6.t * T7.t * T8.t * T9.t * T10.t

    let ding =
      Json_encoding.tup10
        T1.ding
        T2.ding
        T3.ding
        T4.ding
        T5.ding
        T6.ding
        T7.ding
        T8.ding
        T9.ding
        T10.ding

    let v = (T1.v, T2.v, T3.v, T4.v, T5.v, T6.v, T7.v, T8.v, T9.v, T10.v)

    let pp ppf (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) =
      Crowbar.pp
        ppf
        "@[<hv 1>(%a, %a, %a, %a, %a, %a, %a, %a, %a, %a)@]"
        T1.pp
        v1
        T2.pp
        v2
        T3.pp
        v3
        T4.pp
        v4
        T5.pp
        v5
        T6.pp
        v6
        T7.pp
        v7
        T8.pp
        v8
        T9.pp
        v9
        T10.pp
        v10

    let eq (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
        (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10) =
      T1.eq a1 b1 && T2.eq a2 b2 && T3.eq a3 b3 && T4.eq a4 b4 && T5.eq a5 b5
      && T6.eq a6 b6 && T7.eq a7 b7 && T8.eq a8 b8 && T9.eq a9 b9
      && T10.eq a10 b10
  end)

let map_merge_tups (t1 : testable) (t2 : testable) : testable =
  let module T1 = (val t1) in
  let module T2 = (val t2) in
  (module struct
    type t = T1.t * T2.t

    let ding = Json_encoding.merge_tups T1.ding T2.ding

    let v = (T1.v, T2.v)

    let pp ppf (v1, v2) = Crowbar.pp ppf "@[<hv 1>(%a, %a)@]" T1.pp v1 T2.pp v2

    let eq (a1, a2) (b1, b2) = T1.eq a1 b1 && T2.eq a2 b2
  end)

let map_schema (t : testable) : testable =
  let module T = (val t) in
  (module struct
    type t = Json_schema.schema

    let ding = Json_encoding.any_schema

    let v = Json_encoding.schema T.ding

    let pp ppf schema = Json_schema.pp ppf schema

    let eq = ( = )
  end)

let testable_printer : testable Crowbar.printer =
 fun ppf (t : testable) ->
  let module T = (val t) in
  T.pp ppf T.v

(* helpers to construct values tester values *)

(* Generator for testable values *)

let tup_gen (tgen : testable Crowbar.gen) : testable Crowbar.gen =
  let open Crowbar in
  (* Stack overflow if there are more levels *)
  with_printer testable_printer
  @@ choose
       [
         map [tgen] map_tup1;
         map [tgen; tgen] map_tup2;
         map [tgen; tgen; tgen] map_tup3;
         map [tgen; tgen; tgen; tgen] map_tup4;
         map [tgen; tgen; tgen; tgen; tgen] map_tup5;
         map [tgen; tgen; tgen; tgen; tgen; tgen] map_tup6;
       ]

let gen =
  let open Crowbar in
  let g : testable Crowbar.gen =
    fix (fun g ->
        choose
          [
            const null;
            const empty;
            const unit;
            map [short_string] map_constant;
            map [int32] map_int32;
            map [int32] map_int32_conv;
            map [int32] map_int32_list;
            map [int32] map_int32_seq;
            map [int64] map_int53;
            map [float; float; float] map_range_float;
            map [bool] map_bool;
            map [short_string] map_string;
            map [short_mbytes] map_bytes;
            map [float] map_float;
            map [short_string] map_string;
            map [short_mbytes] map_bytes;
            map [g] map_some;
            map [g] map_none;
            map [g] map_tup1;
            map [g; g] map_tup2;
            map [g; g; g] map_tup3;
            map [g; g; g; g] map_tup4;
            map [g; g; g; g; g] map_tup5;
            map [g; g; g; g; g; g] map_tup6;
            map [g; g] (fun t1 t2 -> map_merge_tups (map_tup1 t1) (map_tup1 t2));
            map [g; g; g] (fun t1 t2 t3 ->
                map_merge_tups (map_tup2 t1 t2) (map_tup1 t3));
            map [g; g; g] (fun t1 t2 t3 ->
                map_merge_tups (map_tup1 t1) (map_tup2 t2 t3));
            map [range 3] enum;
            map [g] map_singleton_list;
            map [g] map_dup_list;
            map [g] map_singleton_seq;
            map [g] map_dup_seq;
            map [g] map_def;
            map [g] map_conv_id;
            map [g] map_conv_obj;
            map [g] map_conv_obj_dft;
            map [g] map_conv_obj_dft_construct;
            map [g] map_conv_singleton_union;
            map [g] map_mu_dup_list;
            map [g] map_obj1;
            map [g; g] map_obj2;
          ])
  in
  with_printer testable_printer g

module Ezjsonm_construct = Json_encoding.Make (Json_repr.Ezjsonm)
module Yojson_construct = Json_encoding.Make (Json_repr.Yojson)

(* Basic functions for executing tests on a given input *)
let roundtrip (module M : Json_encoding.S) name pp eq ding v =
  let json =
    try M.construct ding v
    with Invalid_argument m ->
      Crowbar.fail
        (Format.asprintf "Cannot construct (%s): %a (%s)" name pp v m)
  in
  let vv =
    try M.destruct ding json
    with Invalid_argument s ->
      Format.kasprintf Crowbar.fail "Cannot destruct (%s): %s" name s
  in
  Crowbar.check_eq ~pp ~eq v vv

(* Setting up the actual tests *)
let test_testable_ezjsonm (testable : testable) =
  let module T = (val testable) in
  roundtrip (module Ezjsonm_construct) "ez" T.pp T.eq T.ding T.v

let test_testable_yojson (testable : testable) =
  let module T = (val testable) in
  roundtrip (module Yojson_construct) "yo" T.pp T.eq T.ding T.v

let pp_jsonm_lexeme fmt = function
  | `Null -> Format.pp_print_string fmt "null"
  | `Bool true -> Format.pp_print_string fmt "true"
  | `Bool false -> Format.pp_print_string fmt "false"
  | `String _ -> Format.pp_print_string fmt "\"..\""
  | `Float _ -> Format.pp_print_string fmt "(float)"
  | `Name _ -> Format.pp_print_string fmt "(name):"
  | `As -> Format.pp_print_string fmt "["
  | `Ae -> Format.pp_print_string fmt "]"
  | `Os -> Format.pp_print_string fmt "{"
  | `Oe -> Format.pp_print_string fmt "}"

let pp_jsonm_lexeme_list fmt jls =
  Format.pp_print_string fmt "[ " ;
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " ")
    pp_jsonm_lexeme
    fmt
    jls ;
  Format.pp_print_string fmt " ]"

let test_testable_jsonm_lexeme_seq (testable : testable) =
  let module T = (val testable) in
  let direct_seq = Json_encoding.construct_seq T.ding T.v in
  let ezjsonm = Json_encoding.construct T.ding T.v in
  let indirect_seq = Json_encoding.jsonm_lexeme_seq_of_ezjson ezjsonm in
  Crowbar.check_eq
    ~pp:pp_jsonm_lexeme_list
    (List.of_seq direct_seq)
    (List.of_seq indirect_seq)

let test_testable_schema_jsonm_lexeme_seq (testable : testable) =
  let schema = map_schema testable in
  let module T = (val schema) in
  let direct_seq = Json_encoding.construct_seq T.ding T.v in
  let ezjsonm = Json_encoding.construct T.ding T.v in
  let indirect_seq = Json_encoding.jsonm_lexeme_seq_of_ezjson ezjsonm in
  Crowbar.check_eq
    ~pp:pp_jsonm_lexeme_list
    (List.of_seq direct_seq)
    (List.of_seq indirect_seq)

let () =
  (* roundtrip tests: constructions and destructions are inverses of each other *)
  Crowbar.add_test ~name:"ezjsonm roundtrips" [gen] test_testable_ezjsonm ;
  Crowbar.add_test ~name:"yojson roundtrips" [gen] test_testable_yojson ;
  (* sequence construction test: direct sequence construction and construction
     via ezjsonm yield the same result *)
  Crowbar.add_test
    ~name:"->seq ~ ->ezjson->seq"
    [gen]
    test_testable_jsonm_lexeme_seq ;
  Crowbar.add_test
    ~name:"schema->seq ~ schema->ezjson->seq"
    [gen]
    test_testable_schema_jsonm_lexeme_seq ;
  ()
