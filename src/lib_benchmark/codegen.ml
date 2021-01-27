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

(* ------------------------------------------------------------------------- *)

(* Precompose pretty-printing by let-lifting *)
module Lift_then_print = Costlang.Let_lift (Costlang.Pp)

(* ------------------------------------------------------------------------- *)
(* The data required to perform code generation is a map from variables to
   (floating point) coefficients. *)
type solution = float Free_variable.Map.t

let load_solution (fn : string) : solution =
  let infile = open_in fn in
  try
    let res = Marshal.from_channel infile in
    close_in infile ; res
  with exn ->
    close_in infile ;
    Format.eprintf "Codegen.load_solution: could not load %s@." fn ;
    raise exn

let save_solution (s : solution) (fn : string) =
  let outfile = open_out fn in
  Marshal.to_channel outfile s [] ;
  close_out outfile

(* ------------------------------------------------------------------------- *)

let codegen (type workload) (model : workload Model.t) (sol : solution)
    (transform : Costlang.transform) =
  let subst fv =
    match Free_variable.Map.find fv sol with
    | None ->
        raise (Fixed_point_transform.Codegen_error (Variable_not_found fv))
    | Some f ->
        f
  in
  let module T = (val transform) in
  let module Impl = T (Lift_then_print) in
  let module Subst_impl =
    Costlang.Subst
      (struct
        let subst = subst
      end)
      (Impl)
  in
  match model with
  | Model.Preapplied _ ->
      None
  | Model.Packaged {conv = _; model} ->
      let module M = (val model) in
      let module M = M.Def (Subst_impl) in
      let expr = Lift_then_print.prj @@ Impl.prj @@ Subst_impl.prj M.model in
      Some expr
