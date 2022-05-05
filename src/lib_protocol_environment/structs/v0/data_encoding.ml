(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

include Data_encoding

module Json = struct
  include Data_encoding.Json

  (* Suppress some optional parameters *)

  let construct encoding v = construct encoding v

  let destruct encoding j = destruct encoding j
end

module Binary = struct
  include Binary

  (* Some errors did not exist before env-v3, they are transformed into raised
     exceptions for backwards compatibility. *)

  let read encoding bytes offset length =
    match read_exn encoding (Bytes.unsafe_to_string bytes) offset length with
    | v -> Some v
    | exception
        Read_error (User_invariant_guard s | Exception_raised_in_user_function s)
      ->
        failwith s
    | exception Read_error _ -> None

  let write encoding value bytes offset allowed_bytes =
    Stdlib.Option.bind
      (make_writer_state bytes ~offset ~allowed_bytes)
      (fun state ->
        match write_exn encoding value state with
        | v -> Some v
        | exception Write_error (Exception_raised_in_user_function s) ->
            failwith s
        | exception Write_error _ -> None)

  let of_bytes e b =
    match of_bytes_exn e b with
    | v -> Some v
    | exception
        Read_error (User_invariant_guard s | Exception_raised_in_user_function s)
      ->
        failwith s
    | exception Read_error _ -> None

  (* Also removes [?buffer_size] by eta-expanding. *)
  let to_bytes encoding value =
    match to_bytes_exn encoding value with
    | v -> Some v
    | exception Write_error (Exception_raised_in_user_function s) -> failwith s
    | exception Write_error _ -> None

  let to_bytes_exn encoding value =
    match to_bytes_exn encoding value with
    | v -> v
    | exception Write_error (Exception_raised_in_user_function s) -> failwith s
end
