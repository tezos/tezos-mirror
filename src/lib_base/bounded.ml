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

    val encoding : t Data_encoding.t

    val to_int32 : t -> int32

    val of_int32 : int32 -> t option
  end

  module Make (B : BOUNDS) = struct
    include Compare.Int32 (* This includes [type t = int32] *)
    include B

    let to_int32 x = x

    let of_int32 n =
      if Compare.Int32.(n < B.min_int) then None
      else if Compare.Int32.(n > B.max_int) then None
      else Some n

    let encoding =
      Data_encoding.(
        conv_with_guard
          to_int32
          (fun x ->
            match of_int32 x with
            | None -> Error "Out of bounds"
            | Some x -> Ok x)
          int32)
  end
end
