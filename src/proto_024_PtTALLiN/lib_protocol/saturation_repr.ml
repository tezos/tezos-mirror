(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(* let () = assert (Sys.int_size = 63) *)

type _ t = int

type mul_safe

type may_saturate

let may_saturate : _ t -> may_saturate t = fun x -> x

let to_int x = x

let ( < ) : _ t -> _ t -> bool = Compare.Int.( < )

let ( <= ) : _ t -> _ t -> bool = Compare.Int.( <= )

let ( > ) : _ t -> _ t -> bool = Compare.Int.( > )

let ( >= ) : _ t -> _ t -> bool = Compare.Int.( >= )

let ( = ) : _ t -> _ t -> bool = Compare.Int.( = )

let equal = ( = )

let ( <> ) : _ t -> _ t -> bool = Compare.Int.( <> )

let max : _ t -> _ t -> _ t = fun x y -> if x >= y then x else y

let min : _ t -> _ t -> _ t = fun x y -> if x >= y then y else x

let compare : _ t -> _ t -> _ t = Compare.Int.compare

let saturated = max_int

let ( >! ) : _ t -> int -> bool = Compare.Int.( > )

let of_int_opt t = if t >= 0 && t < saturated then Some t else None

let of_z_opt z =
  match Z.to_int z with int -> of_int_opt int | exception Z.Overflow -> None

let to_z x = Z.of_int x

let saturate_if_undef = function None -> saturated | Some x -> x

let safe_z z = saturate_if_undef @@ of_z_opt z

let safe_int x = of_int_opt x |> saturate_if_undef

let numbits x =
  let x = ref x and n = ref 0 in
  (let y = !x lsr 32 in
   if y <> 0 then (
     n := !n + 32 ;
     x := y)) ;
  (let y = !x lsr 16 in
   if y <> 0 then (
     n := !n + 16 ;
     x := y)) ;
  (let y = !x lsr 8 in
   if y <> 0 then (
     n := !n + 8 ;
     x := y)) ;
  (let y = !x lsr 4 in
   if y <> 0 then (
     n := !n + 4 ;
     x := y)) ;
  (let y = !x lsr 2 in
   if y <> 0 then (
     n := !n + 2 ;
     x := y)) ;
  if !x lsr 1 <> 0 then !n + 2 else !n + !x

let zero = 0

let one = 1

let small_enough z =
  (* The following literal triggers an error if compiled under 32-bit
     architectures, please do not modify it. This is a static way to
     ensure that this file is compiled under a 64-bit architecture. *)
  z land 0x7fffffff80000000 = 0

let mul_safe x = if small_enough x then Some x else None

let mul_safe_exn x =
  if small_enough x then x
  else failwith (Format.sprintf "mul_safe_exn: %d must be below 2147483648" x)

let mul_safe_of_int_exn x =
  Option.bind (of_int_opt x) mul_safe |> function
  | None ->
      failwith
        (Format.sprintf "mul_safe_of_int_exn: %d must be below 2147483648" x)
  | Some x -> x

(* If [x] is positive, shifting to the right will produce a number
   which is positive and is less than [x]. *)
let shift_right x y = (x :> int) lsr y

let shift_left x y =
  if shift_right saturated y < x then saturated else (x :> int) lsl y

let mul x y =
  (* assert (x >= 0 && y >= 0); *)
  match x with
  | 0 -> 0
  | x ->
      if small_enough x && small_enough y then x * y
      else if Compare.Int.(y > saturated / x) then saturated
      else x * y

let mul_fast x y = x * y

let scale_fast x y =
  if x = 0 then 0
  else if small_enough y then x * y
  else if Compare.Int.(y > saturated / x) then saturated
  else x * y

let add x y =
  let z = x + y in
  if Compare.Int.(z >= 0) then z else saturated

let succ x = add one x

let sub x y = Compare.Int.max (x - y) 0

let sub_opt x y =
  let s = x - y in
  if Compare.Int.(s >= 0) then Some s else None

(* Notice that Z.erem does not behave as mod on negative numbers.
   Fortunately, the inhabitant of [t] are non-negative. *)
let erem x y = x mod y

let ediv x y = x / y

let sqrt x =
  of_int_opt x
  |> Option.map (fun x -> Z.of_int x |> Z.sqrt |> Z.to_int)
  |> saturate_if_undef

let t_to_z_exn z =
  match of_z_opt z with
  | None ->
      (* since the encoding is applied to values of type [t]. *) assert false
  | Some x -> x

let z_encoding = Data_encoding.(check_size 9 (conv to_z t_to_z_exn z))

let n_encoding = Data_encoding.(check_size 9 (conv to_z t_to_z_exn n))

let pp fmt x = Format.pp_print_int fmt x

module Syntax = struct
  (* This is a good enough approximation. S.log2 0 = 1 *)
  let log2 x = safe_int (1 + numbits x)

  let sqrt = sqrt

  let ( + ) = add

  let ( - ) = sub

  let ( * ) = mul

  let ( < ) = ( < )

  let ( = ) = ( = )

  let ( lsr ) = shift_right

  let ( lsl ) = shift_left
end
