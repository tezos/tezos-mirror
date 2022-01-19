(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

module Int32 = struct
  module type BOUNDS = sig
    val min_int : int32

    val max_int : int32
  end

  module type S = sig
    type t

    include BOUNDS

    include Compare.S with type t := t

    type bound_error = Out_of_bounds

    val bound_error_to_string : bound_error -> string

    val encoding : t Data_encoding.t

    val to_int32 : t -> int32

    val of_int32 : int32 -> (t, bound_error) result
  end

  module Make (B : BOUNDS) = struct
    include Compare.Int32 (* This includes [type t = int32] *)

    include B

    type bound_error = Out_of_bounds

    let bound_error_to_string x =
      match x with Out_of_bounds -> "Out_of_bounds"

    let to_int32 x = x

    let of_int32 n =
      if Compare.Int32.(n < B.min_int) then Error Out_of_bounds
      else if Compare.Int32.(n > B.max_int) then Error Out_of_bounds
      else Ok n

    let encoding =
      Data_encoding.(
        conv_with_guard
          to_int32
          (fun x -> Result.map_error bound_error_to_string @@ of_int32 x)
          int32)
  end
end
