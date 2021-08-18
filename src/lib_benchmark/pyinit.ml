(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(* -------------------------------------------------------------------------- *)
(* Initiate necromantic rite *)

let handle_python_error msg closure =
  match closure () with
  | result -> result
  | exception Py.E (x, y) ->
      let s =
        Printf.sprintf
          "%s\n%s\n%s\n"
          msg
          (Py.Object.to_string x)
          (Py.Object.to_string y)
      in
      Stdlib.failwith s

let pyinit () =
  let interpreter =
    if Sys.file_exists "venv/bin/python3" then "venv/bin/python3"
    else if Sys.file_exists "/usr/bin/python3" then "/usr/bin/python3"
    else Stdlib.failwith "python3 interpreter not found"
  in
  if not (Py.is_initialized ()) then (
    Printf.eprintf "Initializing python... " ;
    Py.initialize ~interpreter ~version:3 () ;
    Printf.eprintf "Done.\n%!")

let numpy () =
  pyinit () ;
  handle_python_error "While initializing numpy" @@ fun () ->
  Py.Import.import_module "numpy"

let linear_model () =
  pyinit () ;
  handle_python_error "While initializing sklearn.linear_model" @@ fun () ->
  Py.Import.import_module "sklearn.linear_model"

let scipy_optimize () =
  pyinit () ;
  handle_python_error "While initializing scipy.optimize" @@ fun () ->
  Py.Import.import_module "scipy.optimize"
