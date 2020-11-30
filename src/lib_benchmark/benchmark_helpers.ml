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
  try
    let json =
      Lwt_main.run (Tezos_stdlib_unix.Lwt_utils_unix.Json.read_file file)
    in
    match json with
    | Error _e ->
        Format.eprintf "load_json (%s): failed" file ;
        exit 1
    | Ok json ->
        json
  with Unix.Unix_error (Unix.ENOENT, _, _) ->
    Format.eprintf "load_json: file %s not found, exiting" file ;
    exit 1

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
