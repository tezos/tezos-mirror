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

(** Benchmark parameters. *)
type 'config parameters = {bench_number : int; config : 'config}

(* The module type of benchmarks *)
module type S = sig
  (** Name of the benchmark *)
  val name : Namespace.t

  (** Description of the benchmark *)
  val info : string

  (** File where the benchmark module is defined *)
  val module_filename : string

  (** Tags of the benchmark *)
  val tags : string list

  (** Configuration of the benchmark (eg sampling parameters, paths, etc) *)
  type config

  (** Default configuration of the benchmark *)
  val default_config : config

  (** Configuration encoding *)
  val config_encoding : config Data_encoding.t

  (** Benchmark workload *)
  type workload

  (** Workload encoding *)
  val workload_encoding : workload Data_encoding.t

  (** Optional conversion to vector, for report generation purposes *)
  val workload_to_vector : workload -> Sparse_vec.String.t

  (** Cost models, with a given local name (string) for reference *)
  val models : (string * workload Model.t) list

  (** Benchmark generator *)
  include Generator.S with type config := config and type workload := workload
end

type t = (module S)

type ('cfg, 'workload) poly =
  (module S with type config = 'cfg and type workload = 'workload)

type packed = Ex : ('cfg, 'workload) poly -> packed

(** Get the name of a benchmark *)
let name ((module B) : t) = B.name

(** Get the description of a benchmark *)
let info ((module B) : t) = B.info

(** Get the tags of a benchmark *)
let tags ((module B) : t) = B.tags

let ex_unpack : t -> packed = fun (module Bench) -> Ex ((module Bench) : _ poly)

let get_free_variable_set (module Bench : S) =
  List.fold_left
    (fun acc (_, model) ->
      Free_variable.Set.union acc @@ Model.get_free_variable_set_of_t model)
    Free_variable.Set.empty
    Bench.models
