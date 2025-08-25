(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(* An override file stores a mapping from variables to inferred values. *)

type t = float Free_variable.Map.t

let add_into_map name duration map =
  let name = Free_variable.of_string name in
  if Free_variable.Map.mem name map then
    Format.eprintf
      "Override.add_into_map: warning, %a already bound@."
      Free_variable.pp
      name ;
  Free_variable.Map.add name duration map

let load_file ~filename map =
  let lines = Csv.import ~filename () in
  let header, values =
    match lines with
    | [] | [_] | _ :: _ :: _ :: _ ->
        Stdlib.failwith "Override.load: invalid csv"
    | [header; overrides] -> (header, overrides)
  in
  List.fold_left2
    ~when_different_lengths:()
    (fun map name coeff ->
      let coeff_float =
        try float_of_string coeff
        with Failure _ ->
          Stdlib.failwith ("Override.load: invalid coeff " ^ coeff)
      in
      add_into_map name coeff_float map)
    map
    header
    values
  |>
  (* {!Csv.import} fails before this can *)
  WithExceptions.Result.get_ok ~loc:__LOC__

let load ~filenames : t =
  List.fold_left
    (fun map filename -> load_file ~filename map)
    Free_variable.Map.empty
    filenames
