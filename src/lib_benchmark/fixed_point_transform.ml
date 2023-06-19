(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 DaiLambda, Inc. <contact@dailambda.jp>                 *)
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

(* Transform multiplications by constants in a costlang expression to fixed
   point arithmetic. Allows to make cost functions protocol-compatible. *)

(* Modes of casting of float to int *)
type cast_mode = Ceil | Floor | Round

(* Parameters for conversion to fixed point *)
type options = {
  precision : int;
      (** Number of bits to consider when decomposing the
                          mantissa *)
  max_relative_error : float;
      (** Percentage of admissible relative error when casting floats to ints  *)
  cast_mode : cast_mode;
  inverse_scaling : int;
      (** The constant prettification will consider 1/inverse_scaling digits to
          be not significant. *)
  resolution : int;
      (** Resolution of the grid using when prettifying constants.  *)
}

(* Handling bad floating point values.  *)
type fp_error = Bad_fpclass of Float.fpclass | Negative_or_zero_fp

(* Handling codegen errors. *)
type fixed_point_transform_error = Term_is_not_closed of Free_variable.t

exception Bad_floating_point_number of fp_error

exception Fixed_point_transform_error of fixed_point_transform_error

(* ------------------------------------------------------------------------- *)

let default_options =
  {
    precision = 3;
    max_relative_error = 0.1;
    cast_mode = Round;
    inverse_scaling = 3;
    resolution = 5;
  }

(* ------------------------------------------------------------------------- *)
(* Printers, encodings, etc. *)

let pp_fixed_point_transform_error fmtr (err : fixed_point_transform_error) =
  match err with
  | Term_is_not_closed s ->
      Format.fprintf
        fmtr
        "Fixed_point_transform: Term is not closed (free variable %a \
         encountered)"
        Free_variable.pp
        s

let cast_mode_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"Ceil"
        (Tag 0)
        (constant "Ceil")
        (function Ceil -> Some () | _ -> None)
        (fun () -> Ceil);
      case
        ~title:"Floor"
        (Tag 1)
        (constant "Floor")
        (function Floor -> Some () | _ -> None)
        (fun () -> Floor);
      case
        ~title:"Round"
        (Tag 2)
        (constant "Round")
        (function Round -> Some () | _ -> None)
        (fun () -> Round);
    ]

let options_encoding =
  let open Data_encoding in
  conv
    (fun {precision; max_relative_error; cast_mode; inverse_scaling; resolution} ->
      (precision, max_relative_error, cast_mode, inverse_scaling, resolution))
    (fun (precision, max_relative_error, cast_mode, inverse_scaling, resolution) ->
      {precision; max_relative_error; cast_mode; inverse_scaling; resolution})
    (obj5
       (dft "precision" int31 default_options.precision)
       (dft "max_relative_error" float default_options.max_relative_error)
       (dft "cast_mode" cast_mode_encoding default_options.cast_mode)
       (dft "inverse_scaling" int31 default_options.inverse_scaling)
       (dft "resolution" int31 default_options.resolution))

(* ------------------------------------------------------------------------- *)
(* Error registration *)

let () =
  Printexc.register_printer (fun exn ->
      match exn with
      | Bad_floating_point_number error ->
          let s =
            match error with
            | Bad_fpclass fpcl -> (
                match fpcl with
                | FP_subnormal -> "FP_subnormal"
                | FP_infinite -> "FP_infinite"
                | FP_nan -> "FP_nan"
                | _ -> assert false)
            | Negative_or_zero_fp -> "<= 0"
          in
          Some
            (Printf.sprintf
               "Fixed_point_transform: Bad floating point number: %s"
               s)
      | Fixed_point_transform_error err ->
          let s = Format.asprintf "%a" pp_fixed_point_transform_error err in
          Some s
      | _ -> None)

(* ------------------------------------------------------------------------- *)
(* Constant prettification *)

let rec log10 x =
  if x <= 0 then invalid_arg "log10"
  else if x <= 10 then 1
  else 1 + log10 (x / 10)

let rec pow x n =
  if n < 0 then invalid_arg "pow"
  else if n = 0 then 1
  else if n = 1 then x
  else x * pow x (n - 1)

let snap_to_grid ~inverse_scaling ~resolution x =
  if x = 0 then 0
  else
    let not_significant = log10 x / inverse_scaling in
    let grid = resolution * pow 10 not_significant in
    (x + grid - 1) / grid * grid

(* ------------------------------------------------------------------------- *)
(* Helpers *)

let int_of_float mode x =
  match mode with
  | Ceil -> int_of_float (Float.ceil x)
  | Floor -> int_of_float (Float.floor x)
  | Round -> int_of_float (Float.round x)

(* Checks that a floating point number is 'good' *)
let assert_fp_is_correct (x : float) =
  let fpcl = Float.classify_float x in
  match fpcl with
  | FP_subnormal | FP_infinite | FP_nan ->
      raise (Bad_floating_point_number (Bad_fpclass fpcl))
  | FP_normal when x <= 0.0 ->
      raise (Bad_floating_point_number Negative_or_zero_fp)
  | _ -> fpcl

let cast_to_int max_relative_error mode f : int =
  let i = int_of_float mode f in
  let fi = float_of_int i in
  let re = abs_float (f -. fi) /. abs_float f in
  if re > max_relative_error then
    Format.eprintf
      "Warning: Fixed_point_transform: Imprecise integer cast of %f to %d: %f \
       %% relative error@."
      f
      i
      (re *. 100.) ;
  i

(* ------------------------------------------------------------------------- *)

(* A minimal language in which to perform fixed-point multiplication of
   a 'size repr' by a float *)
module type Fixed_point_lang_sig = sig
  type 'a repr

  type size

  val shift_right : size repr -> int -> size repr

  val shift_left : size repr -> int -> size repr

  val ( + ) : size repr -> size repr -> size repr

  val int : int -> size repr
end

module Fixed_point_arithmetic (Lang : Fixed_point_lang_sig) = struct
  (* IEEE754 redux
     -------------
     Format of a (full-precision, ie 64 bit) floating point number:
     . 1 bit of sign
     . 11 bits of exponent, implicitly centered on 0 (-1022 to 1023)
     . 52 bits of mantissa (+ 1 implicit)

     value of a fp number = sign * mantissa * 2^{exponent - 1023}
     (with the exceptions of nans, infinities and denormalised numbers) *)

  (* Extract ith bit from a float. *)
  let bit (x : float) (i : int) =
    assert (not (i < 0 || i > 63)) ;
    let bits = Int64.bits_of_float x in
    Int64.(logand (shift_right bits i) one)

  (* All bits of a float:
     all_bits x = [sign] @ exponent @ mantissa *)
  let all_bits (x : float) : int64 list =
    List.init ~when_negative_length:() 64 (fun i -> bit x i)
    |> (* 64 >= 0 *) WithExceptions.Result.get_ok ~loc:__LOC__
    |> List.rev

  (* take n first elements from a list *)
  let take n l =
    let rec take n l acc =
      if n <= 0 then (List.rev acc, l)
      else
        match l with
        | [] -> Stdlib.failwith "take"
        | hd :: tl -> take (n - 1) tl (hd :: acc)
    in
    take n l []

  (* Split a float into sign/exponent/mantissa *)
  let split bits =
    let sign, rest = take 1 bits in
    let expo, rest = take 11 rest in
    let mant, _ = take 52 rest in
    (sign, expo, mant)

  (* Convert bits of exponent to int. *)
  let exponent_bits_to_int (l : int64 list) =
    let rec exponent_to_int (l : int64 list) (index : int) : int64 =
      match l with
      | [] -> -1023L
      | bit :: tail ->
          let tail = exponent_to_int tail (index + 1) in
          Int64.(add (shift_left bit index) tail)
    in
    exponent_to_int (List.rev l) 0

  (* Decompose a float into sign/exponent/mantissa *)
  let decompose (x : float) = split (all_bits x)

  (* Generate fixed-precision multiplication by positive constants. *)
  let approx_mult (precision : int) (i : Lang.size Lang.repr) (x : float) :
      Lang.size Lang.repr =
    assert (precision > 0) ;
    let fpcl = assert_fp_is_correct x in
    match fpcl with
    | FP_zero -> Lang.int 0
    | _ -> (
        let _sign, exp, mant = decompose x in
        let exp = Int64.to_int @@ exponent_bits_to_int exp in
        let bits, _ = take precision mant in
        (* The mantissa is always implicitly prefixed by one (except for
           denormalized numbers, excluded here).
           Therefore, we have at most precision + 1 ones in [bits]. *)
        let bits = 1L :: bits in
        (* convert mantissa to sum of powers of 2 computed with shifts *)
        let _, terms =
          List.fold_left
            (fun (k, acc) bit ->
              let acc =
                if bit = 1L then
                  let new_term =
                    if exp - k < 0 then Lang.shift_right i (k - exp)
                    else if exp - k > 0 then Lang.shift_left i (exp - k)
                    else i
                  in
                  new_term :: acc
                else acc
              in
              (k + 1, acc))
            (0, [])
            bits
        in
        match List.rev terms with
        | [] -> assert false (* impossible *)
        | t :: ts -> List.fold_left (fun sum t -> Lang.(sum + t)) t ts)
end

(* ------------------------------------------------------------------------- *)
(* [Prettify_constants] map float and int constants to an integer grid. *)

module Prettify_constants (P : sig
  val options : options
end)
(X : Costlang.S) :
  Costlang.S with type 'a repr = 'a X.repr and type size = X.size = struct
  let {max_relative_error; cast_mode; inverse_scaling; resolution; _} =
    P.options

  include X

  let int i = X.int (snap_to_grid ~inverse_scaling ~resolution i)

  let float f =
    let int = cast_to_int max_relative_error cast_mode f in
    let pretty_int = snap_to_grid ~inverse_scaling ~resolution int in
    X.int pretty_int
end

(* [Convert_mult] approximates multiplications of the form [float * term] or
   [term * float] to integer-only expressions. It is assumed that the term
   is _closed_, ie contains no free variables.

   Note that this does _not_ convert fp constants to int. *)
module Convert_mult (P : sig
  val options : options
end)
(X : Costlang.S) : sig
  include Costlang.S with type size = X.size

  val prj : 'a repr -> 'a X.repr
end = struct
  type size = X.size

  type 'a repr = Term : 'a X.repr -> 'a repr | Const : float -> X.size repr

  let prj (type a) (term : a repr) : a X.repr =
    match term with Term t -> t | Const f -> X.float f

  module FPA = Fixed_point_arithmetic (X)

  let {precision; max_relative_error; cast_mode; _} = P.options

  let cast_to_int = cast_to_int max_relative_error

  (* Cast float to int verifying that the relative error is
     not too high. *)
  let cast_safe (type a) (x : a repr) : a repr =
    match x with
    | Term _ -> x
    | Const f -> Term (X.int (cast_to_int cast_mode f))

  let lift_unop op x = match x with Term x -> Term (op x) | _ -> cast_safe x

  let rec lift_binop op x y =
    match (x, y) with
    | Term x, Term y -> Term (op x y)
    | _ -> lift_binop op (cast_safe x) (cast_safe y)

  let gensym : unit -> string =
    let x = ref 0 in
    fun () ->
      let v = !x in
      incr x ;
      "v" ^ string_of_int v

  let false_ = Term X.false_

  let true_ = Term X.true_

  let float f = Const f

  let int i = Term (X.int i)

  let ( + ) = lift_binop X.( + )

  let sat_sub = lift_binop X.sat_sub

  let ( * ) x y =
    match (x, y) with
    | Term x, Term y -> Term X.(x * y)
    | Term x, Const y | Const y, Term x ->
        (* let-bind the non-constant term to avoid copying it. *)
        Term
          (X.let_ ~name:(gensym ()) x (fun x -> FPA.approx_mult precision x y))
    | Const x, Const y -> Const (x *. y)

  let ( / ) = lift_binop X.( / )

  let max = lift_binop X.max

  let min = lift_binop X.min

  let shift_left x i = lift_unop (fun x -> X.shift_left x i) x

  let shift_right x i = lift_unop (fun x -> X.shift_right x i) x

  let log2 = lift_unop X.log2

  let sqrt = lift_unop X.sqrt

  let free ~name = raise (Fixed_point_transform_error (Term_is_not_closed name))

  let lt = lift_binop X.lt

  let eq = lift_binop X.eq

  let lam (type a b) ~name (f : a repr -> b repr) : (a -> b) repr =
    Term
      (X.lam ~name (fun x ->
           match f (Term x) with Term y -> y | Const f -> X.float f))

  let app (type a b) (fn : (a -> b) repr) (arg : a repr) : b repr =
    match (fn, arg) with
    | Term fn, Term arg -> Term (X.app fn arg)
    | Term fn, Const f -> Term (X.app fn (X.float f))
    | Const _, _ -> assert false

  let let_ (type a b) ~name (m : a repr) (fn : a repr -> b repr) : b repr =
    match m with
    | Term m ->
        Term
          (X.let_ ~name m (fun x ->
               match fn (Term x) with Term y -> y | Const f -> X.float f))
    | Const f ->
        Term
          (X.let_ ~name (X.float f) (fun x ->
               match fn (Term x) with Term y -> y | Const f -> X.float f))

  let if_ cond ift iff = Term (X.if_ (prj cond) (prj ift) (prj iff))
end

module Apply (P : sig
  val options : options
end) : Costlang.Transform =
functor (X : Costlang.S) -> Convert_mult (P) (Prettify_constants (P) (X))
