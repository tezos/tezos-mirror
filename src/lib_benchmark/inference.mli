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

module NMap : Stats.Finbij.S with type elt = Free_variable.t

type measure = Measure of Maths.vector

type constrnt = Full of Costlang.affine * measure

type problem =
  | Non_degenerate of {
      lines : constrnt list;
      input : Maths.matrix;
      output : Maths.matrix;
      nmap : NMap.t;
    }
  | Degenerate of {predicted : Maths.matrix; measured : Maths.matrix}

type scores = {
  (* R2 score is uninformative when the input is constant. (e.g. constant model)
     We use `None` for the R2 score of such models. *)
  r2_score : float option;
  rmse_score : float;
  tvalues : (Free_variable.t * float) list;
}

val pp_scores : Format.formatter -> scores -> unit

val scores_to_csv_column : string * Namespace.t -> scores -> Csv.csv

type solution = {
  mapping : (Free_variable.t * float) list;
  weights : Maths.matrix;
  scores : scores;
}

type solver =
  | Ridge of {alpha : float}
  | Lasso of {alpha : float; positive : bool}
  | NNLS

type error_statistics

val pp_error_statistics : Format.formatter -> error_statistics -> unit

(** Compute prediction error *)
val compute_error_statistics :
  predicted:Maths.matrix -> measured:Maths.matrix -> error_statistics

(** [make_problem ~data ~model ~overrides] makes a benchmark problem for a solver
    from the workload [data] and the model [model]. [overrides] specify the variables
    whose values are already known.
*)
val make_problem :
  data:'workload Measure.workload_data ->
  model:'workload Model.t ->
  overrides:(Free_variable.t -> float option) ->
  problem

(** [solve_problem problem ~is_constant_input solver] solves [problem] using
    [solver]. When [is_constant_input = true], the computation of the R^2 score
    is skipped since it is meaningless.
*)
val solve_problem : problem -> is_constant_input:bool -> solver -> solution

val problem_to_csv : problem -> Csv.csv

val solution_to_csv : solution -> Csv.csv option
