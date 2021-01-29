(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

let load_json file =
  let ic =
    try open_in file
    with Sys_error _ ->
      Format.eprintf "load_json: file %s not found, exiting" file ;
      exit 1
  in
  let json =
    try Ezjsonm.from_channel ic
    with Ezjsonm.Parse_error _ ->
      close_in ic ;
      Format.eprintf "load_json: file %s could not be parsed, exiting" file ;
      exit 1
  in
  close_in ic ; json

let make_progress_printer fmtr total message =
  let counter = ref 1 in
  fun () ->
    Format.fprintf fmtr "\r%s %d/%d%!" message !counter total ;
    incr counter

let int_encoding : int Data_encoding.encoding =
  let open Data_encoding in
  conv (fun i -> Int64.of_int i) (fun l -> Int64.to_int l) int64

(* -------------------------------------------------------------------------- *)
(* Enumerate all ways of picking n elements from a list (ie all injections
   from [n] to [l]) *)

let rec enumerate_injections n l current_pick acc =
  if n = 0 then List.rev current_pick :: acc
  else enumerate_picks n l current_pick acc

and enumerate_picks n l current_pick acc =
  match l with
  | [] ->
      acc
  | x :: tl ->
      let extended_pick = x :: current_pick in
      let acc = enumerate_injections (n - 1) tl extended_pick acc in
      enumerate_picks n tl current_pick acc

let enumerate_subsets n l = enumerate_injections n l [] []
