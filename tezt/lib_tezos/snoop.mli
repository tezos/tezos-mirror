(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Run tezos-snoop commands. *)

(** tezos-snoop states. *)
type t

(** Create a tezos-snoop state *)
val create : ?path:string -> ?color:Log.Color.t -> unit -> t

(** Empirical timing distributions must be converted to definite times, using
    either [Percentile i] for [i] in [0; 100], or by taking the [Mean]. *)
type determinizer = Percentile of int | Mean

(** Runs the benchmark command.

    This performs benchmark [bench_name], asking for [bench_num] points.
    Each point is obtained by measuring execution time [nsamples] times and
    determinizing the resulting timing distribution using [determinizer].
    [determinizer] defaults to [Percentile 50], ie the median.
    The result of benchmarking is saved to [save_to].

    For the meaning of the other optional parameters, see the documentation
    of [tezos-snoop].
 *)
val benchmark :
  bench_name:string ->
  bench_num:int ->
  nsamples:int ->
  determinizer:determinizer ->
  save_to:string ->
  ?seed:int ->
  ?config_dir:string ->
  ?csv_dump:string ->
  t ->
  unit Lwt.t

(** Several regression methods are proposed. [Lasso] gives good result. When
    the variables to be inferred must be constrained to be positive (as is
    typical with cost models that are monotonically increasing in the size
    of the inputs) then the [positive] field can be set to [true]. *)
type regression_method =
  | Lasso of {positive : bool}
  | Ridge of {positive : bool}
  | NNLS

(** Infers parameters for a model on some benchmark data. *)
val infer_parameters :
  model_name:string ->
  workload_data:string ->
  regression_method:regression_method ->
  dump_csv:string ->
  solution:string ->
  ?report:string ->
  ?graph:string ->
  t ->
  unit Lwt.t

(** Generate a bunch of sapling transactions *)
val sapling_generate :
  ?protocol:Protocol.t ->
  tx_count:int ->
  max_inputs:int ->
  max_outputs:int ->
  file:string ->
  ?max_nullifiers:int ->
  ?max_additional_commitments:int ->
  ?seed:int ->
  t ->
  unit Lwt.t

type michelson_term_kind = Data | Code

(** Generate a bunch of Michelson terms *)
val spawn_michelson_generate :
  ?protocol:Protocol.t ->
  terms_count:int ->
  kind:michelson_term_kind ->
  file:string ->
  ?min_size:int ->
  ?max_size:int ->
  ?burn_in:int ->
  ?seed:int ->
  t ->
  Process.t

val michelson_generate :
  ?protocol:Protocol.t ->
  terms_count:int ->
  kind:michelson_term_kind ->
  file:string ->
  ?min_size:int ->
  ?max_size:int ->
  ?burn_in:int ->
  ?seed:int ->
  t ->
  unit Lwt.t

(** Concatenate files containing Michelson terms *)
val michelson_concat :
  ?protocol:Protocol.t ->
  file1:string ->
  file2:string ->
  target:string ->
  t ->
  unit Lwt.t

(** List all benchmarks matching provided tags according to the chosen mode. *)

type tag =
  | Proto of Protocol.t
  | Interpreter
  | Translator
  | Sapling
  | Encoding
  | Io
  | Misc
  | Builtin
  | Gtoc
  | Cache
  | Carbonated_map

type list_mode = All | Any | Exactly

val list_benchmarks : mode:list_mode -> tags:tag list -> t -> string list Lwt.t
