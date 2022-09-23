(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let progress_bar_str ~progress ~width =
  (* 0 <= progress <= 1 *)
  let progress = min 1. (max 0. progress) in
  let filled_width = progress *. float_of_int width in
  let whole_width = int_of_float filled_width in
  let remainder_width = filled_width -. float_of_int whole_width in
  let part_width = int_of_float (remainder_width *. 8.) in
  let empty_width = width - whole_width - 1 in
  let part_char =
    if empty_width < 0 then ""
    else
      match part_width with
      | 0 -> " "
      | 1 -> "▏"
      | 2 -> "▎"
      | 3 -> "▍"
      | 4 -> "▌"
      | 5 -> "▋"
      | 6 -> "▊"
      | _ -> "▉"
  in
  let empty_width = max empty_width 0 in
  let filled =
    String.concat ""
    @@
    match List.init ~when_negative_length:[] whole_width (fun _ -> "█") with
    | Error _ -> []
    | Ok l -> l
  in
  String.concat "" ["["; filled; part_char; String.make empty_width ' '; "]"]

let pp ppf ~width progress =
  Format.fprintf
    ppf
    "%s %5.1f %%"
    (progress_bar_str ~progress ~width)
    (progress *. 100.)
