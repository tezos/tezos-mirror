(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* This module is a library for fuzzing Lwtreslib. It allows to write tests
      comparing Lwtreslib variants to Stdlib reference. E.g., to test all the
      variants of [List.map] we simply write

   {[
   let map =
     test_of_ty_with_p           (* the main function exported by this module *)
       "List.map"                       (* the name of the test for reporting *)
       DSL.([[data] @-> monad data; list data] --> monad (list data))
                             (* the type of the functions being tested; here:
                                           [('a -> 'a) -> 'a list -> 'a list] *)
       Stdlib.List.map                      (* the reference (stdlib) version *)
       Support.Lib.List.map             (* the vanilla version from Lwtreslib *)
       Support.Lib.List.map_s          (* the Lwt(_s) versions from Lwtreslib *)
       Support.Lib.List.map_p          (* the Lwt(_p) versions from Lwtreslib *)
       Support.Lib.List.map_e            (* the result version from Lwtreslib *)
       Support.Lib.List.map_es  (* the Lwt(_s)+result versions from Lwtreslib *)
       Support.Lib.List.map_ep  (* the Lwt(_p)+result versions from Lwtreslib *)
   ]} *)

(* for testing purpose, we use a unique failure value *)
type error = unit

(* During tests, we occasionally need to transform [('a, 'e) result] into plain
   ['a] values.
   For transforming [Error _], we cannot produce this ['a] and so we raise this
   exception. *)
exception ErrorUnit

(* [catch_stdlib_error_*] are wrappers to catch exceptions during tests. *)
let catch_stdlib_error_1 f x = try Ok (f x) with _ -> Error ()

let catch_stdlib_error_2 f x y = try Ok (f x y) with _ -> Error ()

let catch_stdlib_error_3 f x y z = try Ok (f x y z) with _ -> Error ()

let catch_stdlib_error_4 f x y z w = try Ok (f x y z w) with _ -> Error ()

(** An [int_gen_kind] describes different choices for generating integers.
    [Tiny] means the generated integers have small absolute values (which is
    useful for, e.g., testing [List.init] where the cost of the test is
    proportional to the input value). [Nat] means the generated integers are
    non-negative. *)
type int_gen_kind = TinyInt | TinyNat | PlainInt | PlainNat

(* [ty] is a type-descriptor type. Values of the type [ty] describe OCaml types.
   Specifically they describe types used in the interfaces of Lwtreslib.

   There are multiple type parameters to the [ty] type. Specifically, in the
   type [(v, s, pures, r, sr, puresr) ty] the parameters have the following roles:

   - [v] is the vanilla type: the basic type as used in the Stdlib.

   - [s] is the Lwt type: the type as used in the [*_s] functions of Lwtreslib.

   - [pures] is an additional type needed only to satisfy the type checker in the
   definition of [ty]. It has no use outside of this definition.

   - [r] is the Result type: the type as used in the [*_e] functions of
   Lwtreslib.

   - [sr] is the Lwt+Result type: the type as used in the [*_es] functions of
   Lwtreslib.

   - [puresr] is an additional type needed only to satisfy the type checker in the
   definition of [ty]. It has no use outside of this definition.

   The [Monads] constructor is the constructor which introduces the monadic type
   constructors. For example, the value [Monads Bool] has the type
   [(bool, bool Lwt.t, _, (bool, unit) result, (bool, unit) result Lwt.t, _) ty].

   The types [pures] and [puresr] are necessary to avoid the type-checker complaining
   that “a type variable cannot be deduced from the type parameters.”
   Essentially, they are added to convince the type checker that the type
   parameters attached to the constructors in [s] and [sr] are not erased (e.g.,
   if it turned out later that ['a Lwt.t = int]).
*)
type (_, _, _, _, _, _) ty =
  | Unit :
      (* the constructor for the [unit] type *)
      (unit, unit, unit, unit, unit, unit) ty
  | Char :
      (* the constructor for the [char] type; in the tests, the [char] type is
           used for the elements of the lists (and other collections) being tested;
           the DSL exports the value [data] for this [Char] constructor *)
      (char, char, char, char, char, char) ty
  | Int :
      (* the constructor for the [int] type; the [int_gen_kind] is used to tweak
         the generator function (but not any of the others uses of [ty]) (e.g.,
         [PlainNat] will only generate non-negative integers) *)
      int_gen_kind
      -> (int, int, int, int, int, int) ty
  | Bool :
      (* the constructor for the [bool] type *)
      (bool, bool, bool, bool, bool, bool) ty
  | Monads :
      (* the constructor for the monads of Lwtreslib; this is used to describe the
         different types for the different monads: Lwt, Result, LwtResult *)
      ('v, 's, 'pures, 'r, 'sr, 'puresr) ty
      -> ( 'v,
           's Lwt.t,
           's,
           ('r, error) result,
           ('sr, error) result Lwt.t,
           'sr )
         ty
  | ResultMonad :
      (* the constructor for monads of Lwtreslib, but with error management
         integrated; this is useful for functions such as [init] which handle
         invalid arguments without exceptions: this integration adds the result
         type where it is not present *)
      ('v, 's, 'pures, 'r, 'sr, 'puresr) ty
      -> ( ('v, error) result,
           ('s, error) result Lwt.t,
           's,
           ('r, error) result,
           ('sr, error) result Lwt.t,
           'sr )
         ty
  | Tup2 :
      (* the constructor for 2-elements tuples ([*]) *)
      ('va, 'sa, 'puresa, 'ra, 'sra, 'puresra) ty
      * ('vb, 'sb, 'puresrb, 'rb, 'srb, 'puresrb) ty
      -> ( 'va * 'vb,
           'sa * 'sb,
           'puresa * 'puresb,
           'ra * 'rb,
           'sra * 'srb,
           'puresra * 'puresrb )
         ty
  | Option :
      (* the constructor for the [option] type constructor *)
      ('v, 's, 'pures, 'r, 'sr, 'puresr) ty
      -> ('v option, 's option, 'pures, 'r option, 'sr option, 'puresr) ty
  | Either :
      (* the constructor for the [either] type constructor *)
      ('va, 'sa, 'puresa, 'ra, 'sra, 'puresra) ty
      * ('vb, 'sb, 'puresb, 'rb, 'srb, 'puresrb) ty
      -> ( ('va, 'vb) Either.t,
           ('sa, 'sb) Either.t,
           ('puresa, 'puresb) Either.t,
           ('ra, 'rb) Either.t,
           ('sra, 'srb) Either.t,
           ('puresra, 'puresrb) Either.t )
         ty
  | Result :
      (* the constructor for the [result] type constructor *)
      ('va, 'sa, 'puresa, 'ra, 'sra, 'puresra) ty
      * ('vb, 'sb, 'puresb, 'rb, 'srb, 'puresrb) ty
      -> ( ('va, 'vb) result,
           ('sa, 'sb) result,
           ('puresa, 'puresb) result,
           ('ra, 'rb) result,
           ('sra, 'srb) result,
           ('puresra, 'puresrb) result )
         ty
  | Lambda :
      (* the constructor for the lambdas ([->]); this constructor takes two
         arguments: params and return; the params type describes all the
         parameters of the function (using the [params] type constructor, see
         below); the return type describes the return of the function *)
      ('vk, 'vr, 'sk, 'sr, 'puresk, 'rk, 'rr, 'srk, 'srr, 'puresrk) params
      * ('vr, 'sr, 'puresr, 'rr, 'srr, 'puresrr) ty
      -> ('vk, 'sk, 'puresk, 'rk, 'srk, 'puresrk) ty
  | List :
      (* the constructor for the [list] type constructor *)
      ('v, 's, 'pures, 'r, 'sr, 'puresr) ty
      -> ('v list, 's list, 'pures list, 'r list, 'sr list, 'puresr list) ty
  | Seq :
      (* the constructor for the [Seq.t] type constructor *)
      ('v, 's, 'pures, 'r, 'sr, 'puresr) ty
      -> ( 'v Seq.t,
           's Seq.t,
           'pures Seq.t,
           'r Seq.t,
           'sr Seq.t,
           'puresr Seq.t )
         ty

(* The [params] type is a type-descriptor type specifically for the lambda
      types.

      There are multiple type parameter to the [params] type constructor.
      Specifically, in the type
      [(vk, vr, sk, sr, puresk, rk, rr, srk, srr, puresrk) params] the parameters have
      the following role:

      - [vk] is the vanilla lambda type

      - [vr] is the vanilla return type of the lambda; this type also appears at
      the end of the [vk] type, but it is also needed on its own

      - [sk] is the Lwt lambda type

      - [sr] is the Lwt return type of the lambda; this type also appears at
      the end of the [sk] type, but it is also needed on its own

      - [puresk] is an additional type needed only to satisfy the type checker in the
      definition of [ty]. It has no use outside of this definition.

      - [rk] is the Result lambda type

      - [rr] is the Result return type of the lambda; this type also appears at
      the end of the [rk] type, but it is also needed on its own

      - [srk] is the Lwt+Result lambda type

      - [srr] is the Lwt+Result return type of the lambda; this type also appears at
      the end of the [srk] type, but it is also needed on its own

      - [puresrk] is an additional type needed only to satisfy the type checker in the
      definition of [ty]. It has no use outside of this definition.

      The [params] type re-uses the list constructors ([[]] and [(::)]). This gives
      a more compact syntax.

      For example, in the value [Lambda ([Char], Monad Bool)], the value
      [[Char]] has type
   {[
   ((char -> bool), bool,
    (char -> bool Lwt.t), bool Lwt.t, _
    (char -> (bool, unit) result), (bool, unit) result,
    (char -> (bool, unit) result Lwt.t), (bool, unit) result Lwt.t, _
   ) params
   ]} *)
and (_, _, _, _, _, _, _, _, _, _) params =
  | [] :
      (* [[]] is the constructor for zero-parameters lambda; there are no
           zero-parameters lambda, instead this constructor simply ties up the
           return-type and the last of the lambda-type ([bool] in the example
           above) *)
      ('vr, 'vr, 'sr, 'sr, 'sr, 'rr, 'rr, 'srr, 'srr, 'srr) params
  | ( :: ) :
      (* [(::)] is the constructor for the [->] type constructor; it takes two
         parameters: a [ty] to describe the left-hand side of the [->] and a
         [params] to describe the right-hand side of [->] *)
      ('vp, 'sp, 'pures, 'rp, 'srp, 'puresr) ty
      * ('vk, 'vr, 'sk, 'sr, 'puresk, 'rk, 'rr, 'srk, 'srr, 'puresrk) params
      -> ( 'vp -> 'vk,
           'vr,
           'sp -> 'sk,
           'sr,
           'sp -> 'puresk,
           'rp -> 'rk,
           'rr,
           'srp -> 'srk,
           'srr,
           'srp -> 'puresrk )
         params

(* The [DSL] module helps to define [ty] values with a friendlier and more
   compact syntax. *)
module DSL = struct
  let unit = Unit

  let data =
    (* [Char] is used to describe data: the elements of the lists/sequences/etc. *)
    Char

  let int = Int PlainInt

  let tiny_int = Int TinyInt

  let tiny_nat = Int TinyNat

  let nat = Int PlainNat

  let bool = Bool

  let ( * ) a b = Tup2 (a, b)

  let option a = Option a

  let either a b = Either (a, b)

  let result a b = Result (a, b)

  let list a = List a

  let seq a = Seq a

  let monad a = Monads a

  let resultmonad a = ResultMonad a

  let ( @-> ) p r = Lambda (p, r) (* used for inner lambda *)

  let ( --> ) p r =
    (* used for top-level lambdas *)
    let _ =
      (* this imposes some restrictions on the types:
         the return type for [p] must be equal to the type of [r] *)
      Lambda (p, r)
    in
    (* we don't want it wrapped in a [Lambda] constructor for practical reasons
       (see use in [test_of_*])*)
    (p, r)
end

(* When testing we never yield, never pause, never sleep: the promises are all
   immediately resolved! The [unlwt] function just gets the value from a
   promise. It is used to lift functions into the Lwt monad (see below). *)
let unlwt p = match Lwt.state p with Lwt.Return v -> v | _ -> assert false

let unresult = function Error () -> raise ErrorUnit | Ok v -> v

(** An [anyval] is a value from either of the four monads with a constructor
    indicating which of the monads it is. *)
type (_, _, _, _) anyval =
  | ValV : 'v -> ('v, _, _, _) anyval
  | ValS : 's -> (_, 's, _, _) anyval
  | ValR : 'r -> (_, _, 'r, _) anyval
  | ValSR : 'sr -> (_, _, _, 'sr) anyval

(** An [anym] is a constructor pointing to a specific monad. *)
type (_, _, _, _, _) anym =
  | MonV : ('v, _, _, _, 'v) anym
  | MonS : (_, 's, _, _, 's) anym
  | MonR : (_, _, 'r, _, 'r) anym
  | MonSR : (_, _, _, 'sr, 'sr) anym

let rec eq_of_ty : type v s ps r sr psr.
    (v, s, ps, r, sr, psr) ty -> v -> (v, s, r, sr) anyval -> bool =
 fun ty v anyval ->
  match ty with
  | Unit -> true
  | Char ->
      let (ValV c | ValS c | ValR c | ValSR c) = anyval in
      Char.equal v c
  | Int _ ->
      let (ValV i | ValS i | ValR i | ValSR i) = anyval in
      Int.equal v i
  | Bool ->
      let (ValV b | ValS b | ValR b | ValSR b) = anyval in
      Bool.equal v b
  | Tup2 (tyx, tyy) ->
      let anyvalx, anyvaly =
        match anyval with
        | ValV (x, y) -> (ValV x, ValV y)
        | ValS (x, y) -> (ValS x, ValS y)
        | ValR (x, y) -> (ValR x, ValR y)
        | ValSR (x, y) -> (ValSR x, ValSR y)
      in
      let xa, ya = v in
      eq_of_ty tyx xa anyvalx && eq_of_ty tyy ya anyvaly
  | Option ty -> (
      match (v, anyval) with
      | Some a, ValV (Some b) -> eq_of_ty ty a (ValV b)
      | Some a, ValS (Some b) -> eq_of_ty ty a (ValS b)
      | Some a, ValR (Some b) -> eq_of_ty ty a (ValR b)
      | Some a, ValSR (Some b) -> eq_of_ty ty a (ValSR b)
      | None, (ValV None | ValS None | ValR None | ValSR None) -> true
      | _ -> false)
  | Either (tya, tyb) -> (
      match (v, anyval) with
      | Either.Left x, ValV (Either.Left y) -> eq_of_ty tya x (ValV y)
      | Either.Left x, ValS (Either.Left y) -> eq_of_ty tya x (ValS y)
      | Either.Left x, ValR (Either.Left y) -> eq_of_ty tya x (ValR y)
      | Either.Left x, ValSR (Either.Left y) -> eq_of_ty tya x (ValSR y)
      | Either.Right x, ValV (Either.Right y) -> eq_of_ty tyb x (ValV y)
      | Either.Right x, ValS (Either.Right y) -> eq_of_ty tyb x (ValS y)
      | Either.Right x, ValR (Either.Right y) -> eq_of_ty tyb x (ValR y)
      | Either.Right x, ValSR (Either.Right y) -> eq_of_ty tyb x (ValSR y)
      | _ -> false)
  | Result (tya, tyb) -> (
      match (v, anyval) with
      | Ok x, ValV (Ok y) -> eq_of_ty tya x (ValV y)
      | Ok x, ValS (Ok y) -> eq_of_ty tya x (ValS y)
      | Ok x, ValR (Ok y) -> eq_of_ty tya x (ValR y)
      | Ok x, ValSR (Ok y) -> eq_of_ty tya x (ValSR y)
      | Error x, ValV (Error y) -> eq_of_ty tyb x (ValV y)
      | Error x, ValS (Error y) -> eq_of_ty tyb x (ValS y)
      | Error x, ValR (Error y) -> eq_of_ty tyb x (ValR y)
      | Error x, ValSR (Error y) -> eq_of_ty tyb x (ValSR y)
      | _ -> false)
  | Lambda _ -> invalid_arg "eq_of_ty.Lambda"
  | List tyx -> (
      match (v, anyval) with
      | [], (ValV [] | ValS [] | ValR [] | ValSR []) -> true
      | x :: xs, ValV (v :: vs) ->
          eq_of_ty tyx x (ValV v) && eq_of_ty ty xs (ValV vs)
      | x :: xs, ValS (v :: vs) ->
          eq_of_ty tyx x (ValS v) && eq_of_ty ty xs (ValS vs)
      | x :: xs, ValR (v :: vs) ->
          eq_of_ty tyx x (ValR v) && eq_of_ty ty xs (ValR vs)
      | x :: xs, ValSR (v :: vs) ->
          eq_of_ty tyx x (ValSR v) && eq_of_ty ty xs (ValSR vs)
      | _ -> false)
  | Seq tyx -> (
      match v () with
      | Seq.Nil -> (
          match anyval with
          | ValV vs -> (
              match vs () with Seq.Nil -> true | Seq.Cons _ -> false)
          | ValS vs -> (
              match vs () with Seq.Nil -> true | Seq.Cons _ -> false)
          | ValR vs -> (
              match vs () with Seq.Nil -> true | Seq.Cons _ -> false)
          | ValSR vs -> (
              match vs () with Seq.Nil -> true | Seq.Cons _ -> false))
      | Seq.Cons (x, xs) -> (
          match anyval with
          | ValV vs -> (
              match vs () with
              | Seq.Nil -> false
              | Seq.Cons (v, vs) ->
                  eq_of_ty tyx x (ValV v) && eq_of_ty ty xs (ValV vs))
          | ValS vs -> (
              match vs () with
              | Seq.Nil -> false
              | Seq.Cons (v, vs) ->
                  eq_of_ty tyx x (ValS v) && eq_of_ty ty xs (ValS vs))
          | ValR vs -> (
              match vs () with
              | Seq.Nil -> false
              | Seq.Cons (v, vs) ->
                  eq_of_ty tyx x (ValR v) && eq_of_ty ty xs (ValR vs))
          | ValSR vs -> (
              match vs () with
              | Seq.Nil -> false
              | Seq.Cons (v, vs) ->
                  eq_of_ty tyx x (ValSR v) && eq_of_ty ty xs (ValSR vs))))
  | Monads ty -> (
      match anyval with
      | ValV vv -> eq_of_ty ty v (ValV vv)
      | ValS s ->
          let s = unlwt s in
          eq_of_ty ty v (ValS s)
      | ValR r ->
          let r = unresult r in
          eq_of_ty ty v (ValR r)
      | ValSR sr ->
          let sr = unlwt sr in
          let sr = unresult sr in
          eq_of_ty ty v (ValSR sr))
  | ResultMonad ty -> (
      match anyval with
      | ValV (Ok vv) -> (
          match v with Ok v -> eq_of_ty ty v (ValV vv) | Error () -> false)
      | ValV (Error ()) -> ( match v with Ok _ -> false | Error () -> true)
      | ValS s -> (
          match unlwt s with
          | Ok s -> (
              match v with Ok v -> eq_of_ty ty v (ValS s) | Error () -> false)
          | Error () -> ( match v with Ok _ -> false | Error () -> true))
      | ValR (Ok r) -> (
          match v with Ok v -> eq_of_ty ty v (ValR r) | Error () -> false)
      | ValR (Error ()) -> ( match v with Ok _ -> false | Error () -> true)
      | ValSR sr -> (
          match unlwt sr with
          | Ok sr -> (
              match v with
              | Ok v -> eq_of_ty ty v (ValSR sr)
              | Error () -> false)
          | Error () -> ( match v with Ok _ -> false | Error () -> true)))

let rec monadify_of_ty : type v s ps r sr psr t.
    (v, s, ps, r, sr, psr) ty -> (v, s, r, sr, t) anym -> v -> t =
 fun ty anym v ->
  match (ty, anym) with
  | _, MonV -> v
  | Unit, MonS -> ()
  | Unit, MonR -> ()
  | Unit, MonSR -> ()
  | Char, MonS -> v
  | Char, MonR -> v
  | Char, MonSR -> v
  | Int _, MonS -> v
  | Int _, MonR -> v
  | Int _, MonSR -> v
  | Bool, MonS -> v
  | Bool, MonR -> v
  | Bool, MonSR -> v
  | Tup2 (tyx, tyy), MonS ->
      let x, y = v in
      (monadify_of_ty tyx MonS x, monadify_of_ty tyy MonS y)
  | Tup2 (tyx, tyy), MonR ->
      let x, y = v in
      (monadify_of_ty tyx MonR x, monadify_of_ty tyy MonR y)
  | Tup2 (tyx, tyy), MonSR ->
      let x, y = v in
      (monadify_of_ty tyx MonSR x, monadify_of_ty tyy MonSR y)
  | Option ty, MonS -> (
      match v with Some a -> Some (monadify_of_ty ty MonS a) | None -> None)
  | Option ty, MonR -> (
      match v with Some a -> Some (monadify_of_ty ty MonR a) | None -> None)
  | Option ty, MonSR -> (
      match v with Some a -> Some (monadify_of_ty ty MonSR a) | None -> None)
  | Either (tya, tyb), MonS -> (
      match v with
      | Either.Left a -> Either.Left (monadify_of_ty tya MonS a)
      | Either.Right b -> Either.Right (monadify_of_ty tyb MonS b))
  | Either (tya, tyb), MonR -> (
      match v with
      | Either.Left a -> Either.Left (monadify_of_ty tya MonR a)
      | Either.Right b -> Either.Right (monadify_of_ty tyb MonR b))
  | Either (tya, tyb), MonSR -> (
      match v with
      | Either.Left a -> Either.Left (monadify_of_ty tya MonSR a)
      | Either.Right b -> Either.Right (monadify_of_ty tyb MonSR b))
  | Result (tya, tyb), MonS -> (
      match v with
      | Ok a -> Ok (monadify_of_ty tya MonS a)
      | Error b -> Error (monadify_of_ty tyb MonS b))
  | Result (tya, tyb), MonR -> (
      match v with
      | Ok a -> Ok (monadify_of_ty tya MonR a)
      | Error b -> Error (monadify_of_ty tyb MonR b))
  | Result (tya, tyb), MonSR -> (
      match v with
      | Ok a -> Ok (monadify_of_ty tya MonSR a)
      | Error b -> Error (monadify_of_ty tyb MonSR b))
  | Lambda _, _ ->
      (* [monadify_of_ty] is never called on a lambda because we don't have
         [(_ -> _) Lwt.t] in the interfaces of Lwtreslib *)
      invalid_arg "monadify_of_ty.Lambda"
  | List ty, MonS -> List.map (fun v -> monadify_of_ty ty MonS v) v
  | List ty, MonR -> List.map (fun v -> monadify_of_ty ty MonR v) v
  | List ty, MonSR -> List.map (fun v -> monadify_of_ty ty MonSR v) v
  | Seq ty, MonS -> Seq.map (fun v -> monadify_of_ty ty MonS v) v
  | Seq ty, MonR -> Seq.map (fun v -> monadify_of_ty ty MonR v) v
  | Seq ty, MonSR -> Seq.map (fun v -> monadify_of_ty ty MonSR v) v
  | Monads ty, MonS -> Lwt.return (monadify_of_ty ty MonS v)
  | Monads ty, MonR -> Ok (monadify_of_ty ty MonR v)
  | Monads ty, MonSR -> Lwt.return_ok (monadify_of_ty ty MonSR v)
  | ResultMonad ty, MonS -> (
      match v with
      | Ok v -> Lwt.return_ok (monadify_of_ty ty MonS v)
      | Error () -> Lwt.return_error ())
  | ResultMonad ty, MonR -> (
      match v with
      | Ok v -> Ok (monadify_of_ty ty MonR v)
      | Error () -> Error ())
  | ResultMonad ty, MonSR -> (
      match v with
      | Ok v -> Lwt.return_ok (monadify_of_ty ty MonSR v)
      | Error () -> Lwt.return_error ())

let rec unmonadify_of_ty : type v s ps r sr psr.
    (v, s, ps, r, sr, psr) ty -> (v, s, r, sr) anyval -> v =
 fun ty anyval ->
  match ty with
  | Unit -> ()
  | Char ->
      let (ValV c | ValS c | ValR c | ValSR c) = anyval in
      c
  | Int _ ->
      let (ValV i | ValS i | ValR i | ValSR i) = anyval in
      i
  | Bool ->
      let (ValV b | ValS b | ValR b | ValSR b) = anyval in
      b
  | Tup2 (tyx, tyy) -> (
      match anyval with
      | ValV (x, y) -> (x, y)
      | ValS (x, y) ->
          (unmonadify_of_ty tyx (ValS x), unmonadify_of_ty tyy (ValS y))
      | ValR (x, y) ->
          (unmonadify_of_ty tyx (ValR x), unmonadify_of_ty tyy (ValR y))
      | ValSR (x, y) ->
          (unmonadify_of_ty tyx (ValSR x), unmonadify_of_ty tyy (ValSR y)))
  | Monads ty -> (
      match anyval with
      | ValV vv -> vv
      | ValS s ->
          let s = unlwt s in
          unmonadify_of_ty ty (ValS s)
      | ValR r ->
          let r = unresult r in
          unmonadify_of_ty ty (ValR r)
      | ValSR sr ->
          let sr = unlwt sr in
          let sr = unresult sr in
          unmonadify_of_ty ty (ValSR sr))
  | ResultMonad ty -> (
      match anyval with
      | ValV vv -> vv
      | ValS s ->
          let s = unlwt s in
          let s = unresult s in
          Ok (unmonadify_of_ty ty (ValS s))
      | ValR r ->
          let r = unresult r in
          Ok (unmonadify_of_ty ty (ValR r))
      | ValSR sr ->
          let sr = unlwt sr in
          let sr = unresult sr in
          Ok (unmonadify_of_ty ty (ValSR sr)))
  | Option ty -> (
      match anyval with
      | ValV vv -> vv
      | ValS None | ValR None | ValSR None -> None
      | ValS (Some s) -> Some (unmonadify_of_ty ty (ValS s))
      | ValR (Some r) -> Some (unmonadify_of_ty ty (ValR r))
      | ValSR (Some sr) -> Some (unmonadify_of_ty ty (ValSR sr)))
  | Either (tya, tyb) -> (
      match anyval with
      | ValV v -> v
      | ValS (Either.Left y) -> Either.Left (unmonadify_of_ty tya (ValS y))
      | ValR (Either.Left y) -> Either.Left (unmonadify_of_ty tya (ValR y))
      | ValSR (Either.Left y) -> Either.Left (unmonadify_of_ty tya (ValSR y))
      | ValS (Either.Right y) -> Either.Right (unmonadify_of_ty tyb (ValS y))
      | ValR (Either.Right y) -> Either.Right (unmonadify_of_ty tyb (ValR y))
      | ValSR (Either.Right y) -> Either.Right (unmonadify_of_ty tyb (ValSR y)))
  | Result (tya, tyb) -> (
      match anyval with
      | ValV v -> v
      | ValS (Ok y) -> Ok (unmonadify_of_ty tya (ValS y))
      | ValR (Ok y) -> Ok (unmonadify_of_ty tya (ValR y))
      | ValSR (Ok y) -> Ok (unmonadify_of_ty tya (ValSR y))
      | ValS (Error y) -> Error (unmonadify_of_ty tyb (ValS y))
      | ValR (Error y) -> Error (unmonadify_of_ty tyb (ValR y))
      | ValSR (Error y) -> Error (unmonadify_of_ty tyb (ValSR y)))
  | Lambda _ ->
      (* there are no [(_ -> _) monad] in Lwtreslib interfaces so there is no
         need to handle lambdas *)
      raise (Invalid_argument "unmonadify_of_ty.Lambda")
  | List tyx -> (
      match anyval with
      | ValV v -> v
      | ValS vs -> List.map (fun v -> unmonadify_of_ty tyx (ValS v)) vs
      | ValR vs -> List.map (fun v -> unmonadify_of_ty tyx (ValR v)) vs
      | ValSR vs -> List.map (fun v -> unmonadify_of_ty tyx (ValSR v)) vs)
  | Seq tyx -> (
      match anyval with
      | ValV v -> v
      | ValS vs -> Seq.map (fun v -> unmonadify_of_ty tyx (ValS v)) vs
      | ValR vs -> Seq.map (fun v -> unmonadify_of_ty tyx (ValR v)) vs
      | ValSR vs -> Seq.map (fun v -> unmonadify_of_ty tyx (ValSR v)) vs)

(* QCheck-related functions (building off of the helpers above) *)

(* in order to generate lambdas, we need observables; we only need observable
   for the vanilla variant so that's what this function returns *)
let rec observable_of_ty : type v s ps r sr psr.
    (v, s, ps, r, sr, psr) ty -> v QCheck2.Observable.t = function
  | Unit -> QCheck2.Observable.unit
  | Char -> QCheck2.Observable.char
  | Int _ -> QCheck2.Observable.int
  | Bool -> QCheck2.Observable.bool
  | Tup2 (tyx, tyy) ->
      QCheck2.Observable.pair (observable_of_ty tyx) (observable_of_ty tyy)
  | Option ty -> QCheck2.Observable.option (observable_of_ty ty)
  | Either (tya, tyb) ->
      let oa = observable_of_ty tya in
      let ob = observable_of_ty tyb in
      QCheck2.Observable.make
        ~eq:
          (Either.equal
             ~left:(QCheck2.Observable.equal oa)
             ~right:(QCheck2.Observable.equal ob))
        (function
          | Either.Left a ->
              Format.asprintf "Left (%s)" (QCheck2.Observable.print oa a)
          | Either.Right b ->
              Format.asprintf "Right (%s)" (QCheck2.Observable.print ob b))
  | Result (tya, tyb) ->
      let oa = observable_of_ty tya in
      let ob = observable_of_ty tyb in
      QCheck2.Observable.make
        ~eq:
          (Result.equal
             ~ok:(QCheck2.Observable.equal oa)
             ~error:(QCheck2.Observable.equal ob))
        (function
          | Ok a -> Format.asprintf "Ok (%s)" (QCheck2.Observable.print oa a)
          | Error b ->
              Format.asprintf "Error (%s)" (QCheck2.Observable.print ob b))
  | Lambda _ ->
      (* never appears inside another lambda so we don't need it *)
      invalid_arg "observable_of_ty.Lambda"
  | List ty -> QCheck2.Observable.list (observable_of_ty ty)
  | Seq ty ->
      QCheck2.Observable.contramap
        List.of_seq
        (QCheck2.Observable.list (observable_of_ty ty))
  | Monads ty -> observable_of_ty ty
  | ResultMonad ty ->
      let o = observable_of_ty ty in
      QCheck2.Observable.make
        ~eq:(Result.equal ~ok:(QCheck2.Observable.equal o) ~error:Unit.equal)
        (function
          | Error () -> Format.asprintf "Error ()"
          | Ok v -> Format.asprintf "Ok (%s)" (QCheck2.Observable.print o v))

(* for showing test results and such, we need to be able to pretty-print
   values; we print a monad-neutral summary *)
let rec pp_of_ty : type v s ps r sr psr.
    (v, s, ps, r, sr, psr) ty -> Format.formatter -> v -> unit =
 fun ty fmt v ->
  match ty with
  | Unit -> Format.fprintf fmt "()"
  | Char -> Format.pp_print_char fmt v
  | Int _ -> Format.pp_print_int fmt v
  | Bool -> Format.pp_print_bool fmt v
  | Tup2 (tyx, tyy) ->
      let x, y = v in
      Format.fprintf fmt "(%a,%a)" (pp_of_ty tyx) x (pp_of_ty tyy) y
  | Option ty -> (
      match v with
      | None -> Format.fprintf fmt "None"
      | Some x -> Format.fprintf fmt "Some(%a)" (pp_of_ty ty) x)
  | Either (tya, tyb) -> (
      match v with
      | Either.Left x -> Format.fprintf fmt "Left(%a)" (pp_of_ty tya) x
      | Either.Right x -> Format.fprintf fmt "Right(%a)" (pp_of_ty tyb) x)
  | Result (tya, tyb) -> (
      match v with
      | Ok x -> Format.fprintf fmt "Ok(%a)" (pp_of_ty tya) x
      | Error x -> Format.fprintf fmt "Error(%a)" (pp_of_ty tyb) x)
  | Lambda (_, _) -> Format.fprintf fmt "(\\lambda)"
  | List ty ->
      Format.fprintf
        fmt
        "[%a]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ';')
           (pp_of_ty ty))
        v
  | Seq ty ->
      Format.fprintf
        fmt
        "SEQ[%a]"
        (Format.pp_print_seq
           ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ';')
           (pp_of_ty ty))
        v
  | Monads ty -> Format.fprintf fmt "Monad(%a)" (pp_of_ty ty) v
  | ResultMonad ty -> (
      match v with
      | Error () -> Format.fprintf fmt "ResultMonad:Error()"
      | Ok v -> Format.fprintf fmt "ResultMonad:Ok(%a)" (pp_of_ty ty) v)

(* helper function to get four lists out of a list of 4-tuples; this is used
   when generating different lists of inputs for the tests *)
let rec list_split_4 acca accb accc accd = function
  | Stdlib.List.[] ->
      (List.rev acca, List.rev accb, List.rev accc, List.rev accd)
  | (a, b, c, d) :: more ->
      list_split_4 (a :: acca) (b :: accb) (c :: accc) (d :: accd) more

let list_split_4 l = list_split_4 [] [] [] [] l

(* helper but for seq *)
let rec seq_split_4 acca accb accc accd = function
  | Stdlib.List.[] ->
      ( List.to_seq @@ List.rev acca,
        List.to_seq @@ List.rev accb,
        List.to_seq @@ List.rev accc,
        List.to_seq @@ List.rev accd )
  | (a, b, c, d) :: more ->
      seq_split_4 (a :: acca) (b :: accb) (c :: accc) (d :: accd) more

let seq_split_4 l = seq_split_4 [] [] [] [] l

(* QCheck2 generators, derived from [ty] descriptions. *)
let rec gen_of_ty : type v s ps r sr psr.
    (v, s, ps, r, sr, psr) ty -> (v * s * r * sr) QCheck2.Gen.t =
 fun ty ->
  match ty with
  | Unit -> QCheck2.Gen.pure ((), (), (), ())
  | Char ->
      (* here like everywhere in this function we have to be careful to generate
         tuples where all the elements are equal-modulo-the-monads; hence the
         map to duplicate the character *)
      QCheck2.Gen.map (fun x -> (x, x, x, x)) QCheck2.Gen.printable
  | Int kind ->
      let g =
        match kind with
        | PlainInt -> QCheck2.Gen.int
        | PlainNat -> QCheck2.Gen.nat
        | TinyInt -> QCheck2.Gen.int_range (-2) 4
        | TinyNat -> QCheck2.Gen.int_range 0 4
      in
      QCheck2.Gen.map (fun x -> (x, x, x, x)) g
  | Bool -> QCheck2.Gen.map (fun x -> (x, x, x, x)) QCheck2.Gen.bool
  | Tup2 (tyx, tyy) ->
      let gx = gen_of_ty tyx in
      let gy = gen_of_ty tyy in
      QCheck2.Gen.map2
        (fun (vx, sx, rx, srx) (vy, sy, ry, sry) ->
          ((vx, vy), (sx, sy), (rx, ry), (srx, sry)))
        gx
        gy
  | Option ty ->
      QCheck2.Gen.oneof
        [
          QCheck2.Gen.pure (None, None, None, None);
          QCheck2.Gen.map
            (fun (v, s, r, sr) -> (Some v, Some s, Some r, Some sr))
            (gen_of_ty ty);
        ]
  | Either (tya, tyb) ->
      QCheck2.Gen.oneof
        [
          QCheck2.Gen.map
            (fun (v, s, r, sr) ->
              (Either.Left v, Either.Left s, Either.Left r, Either.Left sr))
            (gen_of_ty tya);
          QCheck2.Gen.map
            (fun (v, s, r, sr) ->
              (Either.Right v, Either.Right s, Either.Right r, Either.Right sr))
            (gen_of_ty tyb);
        ]
  | Result (tya, tyb) ->
      QCheck2.Gen.oneof
        [
          QCheck2.Gen.map
            (fun (v, s, r, sr) -> (Ok v, Ok s, Ok r, Ok sr))
            (gen_of_ty tya);
          QCheck2.Gen.map
            (fun (v, s, r, sr) -> (Error v, Error s, Error r, Error sr))
            (gen_of_ty tyb);
        ]
  | Lambda (params, tyr) ->
      (* lambdas are more complicated to deal with because monads can appear in
         covariant and contravariant positions; it is dealt with in the
         [gen_of_params] function *)
      gen_of_params params tyr
  | List ty ->
      let g = gen_of_ty ty in
      QCheck2.Gen.map list_split_4 (QCheck2.Gen.list g)
  | Seq ty ->
      let g = gen_of_ty ty in
      QCheck2.Gen.map seq_split_4 (QCheck2.Gen.list g)
  | Monads ty ->
      QCheck2.Gen.map
        (fun (v, s, r, sr) -> (v, Lwt.return s, Ok r, Lwt.return_ok sr))
        (gen_of_ty ty)
  | ResultMonad ty ->
      QCheck2.Gen.map
        (fun (v, s, r, sr) -> (Ok v, Lwt.return_ok s, Ok r, Lwt.return_ok sr))
        (gen_of_ty ty)

and gen_of_params : type vk vr sk sr psr psk rk rr srk srr psrr psrk.
    (vk, vr, sk, sr, psk, rk, rr, srk, srr, psrk) params ->
    (vr, sr, psr, rr, srr, psrr) ty ->
    (vk * sk * rk * srk) QCheck2.Gen.t =
 fun params tyr ->
  match params with
  | [] ->
      (* zero-parameters functions don't exist *)
      invalid_arg "gen_of_params.[]"
  | [tya] ->
      (* step1: generate a function for the vanilla variant *)
      let gvr =
        (* a generator for the vanilla return values *)
        QCheck2.Gen.map (fun (x, _, _, _) -> x) @@ gen_of_ty tyr
      in
      let aov =
        (* an observable for the vanilla parameter (see [observable_of_ty]: it
           generates the observable for the vanilla variant only) *)
        observable_of_ty tya
      in
      let fg =
        (* a generator for a function from the parameter to the return type *)
        QCheck2.fun1 aov gvr
      in
      (* step2: make all the variants based on the vanilla variant *)
      QCheck2.Gen.map
        (fun (QCheck2.Fun (_, f)) ->
          (* each variant is generated by sinking the parameter type and lifting
             the return type *)
          let fs a =
            monadify_of_ty tyr MonS (f (unmonadify_of_ty tya (ValS a)))
          in
          let fr a =
            monadify_of_ty tyr MonR (f (unmonadify_of_ty tya (ValR a)))
          in
          let fsr a =
            monadify_of_ty tyr MonSR (f (unmonadify_of_ty tya (ValSR a)))
          in
          (f, fs, fr, fsr))
        fg
  | [tya; tyb] ->
      (* the two-parameter case is the same as the one-parameter case; the
         additional parameter simply adds some boilerplate *)
      let gvr = QCheck2.Gen.map (fun (x, _, _, _) -> x) @@ gen_of_ty tyr in
      let aov = observable_of_ty tya in
      let bov = observable_of_ty tyb in
      let fg = QCheck2.fun2 aov bov gvr in
      QCheck2.Gen.map
        (fun (QCheck2.Fun (_, f)) ->
          let fs a b =
            monadify_of_ty
              tyr
              MonS
              (f
                 (unmonadify_of_ty tya (ValS a))
                 (unmonadify_of_ty tyb (ValS b)))
          in
          let fr a b =
            monadify_of_ty
              tyr
              MonR
              (f
                 (unmonadify_of_ty tya (ValR a))
                 (unmonadify_of_ty tyb (ValR b)))
          in
          let fsr a b =
            monadify_of_ty
              tyr
              MonSR
              (f
                 (unmonadify_of_ty tya (ValSR a))
                 (unmonadify_of_ty tyb (ValSR b)))
          in
          (f, fs, fr, fsr))
        fg
  | [tya; tyb; tyc] ->
      (* the three-parameter case is the same as the one-parameter case; the
         additional parameters simply add some boilerplate *)
      let gvr = QCheck2.Gen.map (fun (x, _, _, _) -> x) @@ gen_of_ty tyr in
      let aov = observable_of_ty tya in
      let bov = observable_of_ty tyb in
      let cov = observable_of_ty tyc in
      let fg = QCheck2.fun3 aov bov cov gvr in
      QCheck2.Gen.map
        (fun (QCheck2.Fun (_, f)) ->
          let fs a b c =
            monadify_of_ty
              tyr
              MonS
              (f
                 (unmonadify_of_ty tya (ValS a))
                 (unmonadify_of_ty tyb (ValS b))
                 (unmonadify_of_ty tyc (ValS c)))
          in
          let fr a b c =
            monadify_of_ty
              tyr
              MonR
              (f
                 (unmonadify_of_ty tya (ValR a))
                 (unmonadify_of_ty tyb (ValR b))
                 (unmonadify_of_ty tyc (ValR c)))
          in
          let fsr a b c =
            monadify_of_ty
              tyr
              MonSR
              (f
                 (unmonadify_of_ty tya (ValSR a))
                 (unmonadify_of_ty tyb (ValSR b))
                 (unmonadify_of_ty tyc (ValSR c)))
          in
          (f, fs, fr, fsr))
        fg
  | [tya; tyb; tyc; tyd] ->
      (* the four-parameter case is the same as the one-parameter case; the
         additional parameters simply add some boilerplate *)
      let gvr = QCheck2.Gen.map (fun (x, _, _, _) -> x) @@ gen_of_ty tyr in
      let aov = observable_of_ty tya in
      let bov = observable_of_ty tyb in
      let cov = observable_of_ty tyc in
      let dov = observable_of_ty tyd in
      let fg = QCheck2.fun4 aov bov cov dov gvr in
      QCheck2.Gen.map
        (fun (QCheck2.Fun (_, f)) ->
          let fs a b c d =
            monadify_of_ty
              tyr
              MonS
              (f
                 (unmonadify_of_ty tya (ValS a))
                 (unmonadify_of_ty tyb (ValS b))
                 (unmonadify_of_ty tyc (ValS c))
                 (unmonadify_of_ty tyd (ValS d)))
          in
          let fr a b c d =
            monadify_of_ty
              tyr
              MonR
              (f
                 (unmonadify_of_ty tya (ValR a))
                 (unmonadify_of_ty tyb (ValR b))
                 (unmonadify_of_ty tyc (ValR c))
                 (unmonadify_of_ty tyd (ValR d)))
          in
          let fsr a b c d =
            monadify_of_ty
              tyr
              MonSR
              (f
                 (unmonadify_of_ty tya (ValSR a))
                 (unmonadify_of_ty tyb (ValSR b))
                 (unmonadify_of_ty tyc (ValSR c))
                 (unmonadify_of_ty tyd (ValSR d)))
          in
          (f, fs, fr, fsr))
        fg
  | _ ->
      (* the maximum arity of lambdas in the Lwtreslib interface is 4 so we
         don't need more *)
      invalid_arg "gen_of_params._"

(* [gen_test_of_ty name params return ref v s r sr] is a test that checks that the
   functions [v], [s], [r], [sr] are all equivalent to [ref] modulo the monads.

   The intended use is to pass a [Stdlib] function as [ref] and all the variants
   from Lwtreslib as [v], [s], [r], [sr].

   Note that [v], [s], [r], and [sr] are all lists of functions. That's in order
   to all different number of variants to be tested (e.g., for when Lwtreslib
   exports both a sequential variant ([_s]) and a concurrent ([_p]) variants of
   a function). This is then hidden via the more precise wrappers below. *)
let gen_test_of_ty : type vk vr sk psk sr psr rk rr srk psrk srr psrr.
    string ->
    (vk, vr, sk, sr, psk, rk, rr, srk, srr, psrk) params ->
    (vr, sr, psr, rr, srr, psrr) ty ->
    vk ->
    vk list ->
    sk list ->
    rk list ->
    srk list ->
    QCheck2.Test.t =
 fun name params tyr vfref vf sf rf srf ->
  match params with
  | [] ->
      (* there are no lambdas with zero parameters *)
      invalid_arg "gen_test_of_ty.minimum-arity-deficiency"
  | [tya] ->
      QCheck2.Test.make
        ~name
        ~print:(fun (va, _, _, _) -> Format.asprintf "(%a)" (pp_of_ty tya) va)
        (gen_of_ty tya)
        (fun (va, sa, ra, sra) ->
          let v = vfref va in
          List.for_all (fun vf -> eq_of_ty tyr v (ValV (vf va))) vf
          && List.for_all (fun sf -> eq_of_ty tyr v (ValS (sf sa))) sf
          && List.for_all (fun rf -> eq_of_ty tyr v (ValR (rf ra))) rf
          && List.for_all (fun srf -> eq_of_ty tyr v (ValSR (srf sra))) srf)
  | [tya; tyb] ->
      let ag = gen_of_ty tya in
      let bg = gen_of_ty tyb in
      QCheck2.Test.make
        ~name
        ~print:(fun ((va, _, _, _), (vb, _, _, _)) ->
          Format.asprintf "(%a, %a)" (pp_of_ty tya) va (pp_of_ty tyb) vb)
        (QCheck2.Gen.tup2 ag bg)
        (fun ((va, sa, ra, sra), (vb, sb, rb, srb)) ->
          let v = vfref va vb in
          List.for_all (fun vf -> eq_of_ty tyr v (ValV (vf va vb))) vf
          && List.for_all (fun sf -> eq_of_ty tyr v (ValS (sf sa sb))) sf
          && List.for_all (fun rf -> eq_of_ty tyr v (ValR (rf ra rb))) rf
          && List.for_all (fun srf -> eq_of_ty tyr v (ValSR (srf sra srb))) srf)
  | [tya; tyb; tyc] ->
      let ag = gen_of_ty tya in
      let bg = gen_of_ty tyb in
      let cg = gen_of_ty tyc in
      QCheck2.Test.make
        ~name
        ~print:(fun ((va, _, _, _), (vb, _, _, _), (vc, _, _, _)) ->
          Format.asprintf
            "(%a, %a, %a)"
            (pp_of_ty tya)
            va
            (pp_of_ty tyb)
            vb
            (pp_of_ty tyc)
            vc)
        (QCheck2.Gen.tup3 ag bg cg)
        (fun ((va, sa, ra, sra), (vb, sb, rb, srb), (vc, sc, rc, src)) ->
          let v = vfref va vb vc in
          List.for_all (fun vf -> eq_of_ty tyr v (ValV (vf va vb vc))) vf
          && List.for_all (fun sf -> eq_of_ty tyr v (ValS (sf sa sb sc))) sf
          && List.for_all (fun rf -> eq_of_ty tyr v (ValR (rf ra rb rc))) rf
          && List.for_all
               (fun srf -> eq_of_ty tyr v (ValSR (srf sra srb src)))
               srf)
  | [tya; tyb; tyc; tyd] ->
      let ag = gen_of_ty tya in
      let bg = gen_of_ty tyb in
      let cg = gen_of_ty tyc in
      let dg = gen_of_ty tyd in
      QCheck2.Test.make
        ~name
        ~print:(fun
            ((va, _, _, _), (vb, _, _, _), (vc, _, _, _), (vd, _, _, _)) ->
          Format.asprintf
            "(%a, %a, %a, %a)"
            (pp_of_ty tya)
            va
            (pp_of_ty tyb)
            vb
            (pp_of_ty tyc)
            vc
            (pp_of_ty tyd)
            vd)
        (QCheck2.Gen.tup4 ag bg cg dg)
        (fun ( (va, sa, ra, sra),
               (vb, sb, rb, srb),
               (vc, sc, rc, src),
               (vd, sd, rd, srd) )
           ->
          let v = vfref va vb vc vd in
          List.for_all (fun vf -> eq_of_ty tyr v (ValV (vf va vb vc vd))) vf
          && List.for_all (fun sf -> eq_of_ty tyr v (ValS (sf sa sb sc sd))) sf
          && List.for_all (fun rf -> eq_of_ty tyr v (ValR (rf ra rb rc rd))) rf
          && List.for_all
               (fun srf -> eq_of_ty tyr v (ValSR (srf sra srb src srd)))
               srf)
  | _ ->
      (* the maximum arity of Seq interface item is 4 so we don't need more *)
      invalid_arg "gen_test_of_ty.maximum-arity-exceeded"

let test_of_ty name (params, tyr) vfref vf sf rf srf =
  QCheck_alcotest.to_alcotest
  @@ gen_test_of_ty name params tyr vfref [vf] [sf] [rf] [srf]

let test_of_ty_with_p name (params, tyr) vfref vf sf pf rf srf prf =
  QCheck_alcotest.to_alcotest
  @@ gen_test_of_ty name params tyr vfref [vf] [sf; pf] [rf] [srf; prf]

let test_vanilla_of_ty name (params, tyr) vfref vf =
  QCheck_alcotest.to_alcotest
  @@ gen_test_of_ty name params tyr vfref [vf] [] [] []
