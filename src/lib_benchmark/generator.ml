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

(** A [probe] implements an instrumented [apply] function.
    The implementation of [apply] is meant to record in a side-effecting way
    the result of a benchmark for the given closure. [get] allows to retrieve
    the results. *)
type 'aspect probe = {
  apply : 'a. 'aspect -> (unit -> 'a) -> 'a;
  aspects : unit -> 'aspect list;
  get : 'aspect -> float list;
}

(** The type of benchmarks.
    Measurements of benchmarks are directly calculated by [measure]
    or measured as the execution time of [closure].
    Some benchmark requires to set-up/cleanup artifacts when being run,
    in that case use [With_context] with a proper implementation of the
    [with_context] function. *)
type 'workload benchmark =
  | Calculated : {
      workload : 'workload;
      measure : unit -> float;
    }
      -> 'workload benchmark
      (** Calculated provides its own [measure] function, unlike the other
      benchmarks where [Measure] module times their closures *)
  | Plain : {
      workload : 'workload;
      closure : unit -> unit;
    }
      -> 'workload benchmark
  | With_context : {
      workload : 'workload;
      closure : 'context -> unit;
      with_context : 'a. ('context -> 'a) -> 'a;
    }
      -> 'workload benchmark
  | With_probe : {
      workload : 'aspect -> 'workload;
      probe : 'aspect probe;
      closure : 'aspect probe -> unit;
    }
      -> 'workload benchmark

module type S = sig
  (** Configuration of the benchmark generator *)
  type config

  (** Workload corresponding to a benchmark run *)
  type workload

  (** Creates a list of benchmarks, ready to be run. The [bench_num] option
      corresponds to the argument command-line specified by the user on command
      line, but it can be overriden by the [config]-specific settings.
      This is the case for instance when the benchmarks are performed on
      external artifacts.
      The benchmarks are thunked to prevent evaluating the workload until
      needed. *)
  val create_benchmarks :
    rng_state:Random.State.t ->
    bench_num:int ->
    config ->
    (unit -> workload benchmark) list
end
