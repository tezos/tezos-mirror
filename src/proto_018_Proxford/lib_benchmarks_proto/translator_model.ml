(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let ns = Namespace.make Registration_helpers.ns "translator"

let fv s = Free_variable.of_namespace (ns s)

let gas_full t_kind code_or_data =
  let name =
    Format.asprintf
      "%a_%a_gas"
      Translator_workload.pp_kind
      t_kind
      Translator_workload.pp_code_or_data
      code_or_data
  in
  let intercept = fv (Format.asprintf "%s_const" name) in
  let coeff = fv (Format.asprintf "%s_coeff" name) in
  Model.affine ~name:(ns name) ~intercept ~coeff

let size_full t_kind code_or_data =
  let name =
    Format.asprintf
      "%a_%a_size"
      Translator_workload.pp_kind
      t_kind
      Translator_workload.pp_code_or_data
      code_or_data
  in
  let coeff1 = fv (Format.asprintf "%s_traversal" name) in
  let coeff2 = fv (Format.asprintf "%s_int_bytes" name) in
  let coeff3 = fv (Format.asprintf "%s_string_bytes" name) in
  Model.trilinear ~name:(ns name) ~coeff1 ~coeff2 ~coeff3

let gas_based_model t_kind code_or_data =
  Model.make
    ~conv:(function
      | Translator_workload.Typechecker_workload {consumed; _} -> (consumed, ()))
    ~model:(gas_full t_kind code_or_data)

let size_based_model t_kind code_or_data =
  Model.make
    ~conv:(function
      | Translator_workload.Typechecker_workload {micheline_size; _} -> (
          match micheline_size with
          | {traversal; int_bytes; string_bytes} ->
              (traversal, (int_bytes, (string_bytes, ())))))
    ~model:(size_full t_kind code_or_data)
