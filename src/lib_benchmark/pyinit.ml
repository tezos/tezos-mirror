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

open Pytools

let pyinit () =
  if not (Py.is_initialized ()) then (
    Printf.eprintf "Initializing python... \n%!" ;
    Py.initialize ~version:3 () ;
    Format.eprintf
      "Python library used: (%a)\n%!"
      Format.(pp_print_option pp_print_string)
      (Py.get_library_filename ()))

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

let sklearn_metrics () =
  pyinit () ;
  handle_python_error "While initializing sklearn.metrics" @@ fun () ->
  Py.Import.import_module "sklearn.metrics"

let statsmodels_api () =
  pyinit () ;
  handle_python_error "While initializing statsmodels.api" @@ fun () ->
  Py.Import.import_module "statsmodels.api"

let statsmodels_stats () =
  pyinit () ;
  handle_python_error "While initializing statsmodels.stats" @@ fun () ->
  Py.Import.import_module "statsmodels.stats"

let%expect_test "pyinit can be called twice without failing" =
  (* Paths are environment specific, do not leak into tests *)
  let discard_python_path str =
    let re = Str.regexp {|\(Python library used:\).*|} in
    Str.replace_first re {|\1REPLACED_FOR_TEST|} str
  in
  pyinit () ;
  [%expect.output] |> discard_python_path |> print_endline ;
  [%expect
    {|
      Initializing python...
      Python library used:REPLACED_FOR_TEST|}] ;
  (* Second run is empty, but no failure *)
  pyinit () ;
  [%expect {| |}]
