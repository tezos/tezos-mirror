(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** This module implements a schema for a json file that contains the
    configuration for all the benchmarks registered in Snoop.
    This configuration file has a tree-like structure that follows the Namespace
    hierarchy of the benchmarks. Its root element is always ".". *)

(** The type of the configuration file *)
type t

(** Its encoding *)
val encoding : t Data_encoding.t

(** The minimal config file *)
val empty : t

(** Given a benchmark [b] and a config [c], [get_config b c] returns the
    configuration for the benchmark [b] stored in [c], in json form. *)
val get_config : Benchmark.t -> t -> Data_encoding.json

(** Given a benchmark [b] and a file path [p], [parse_config b (Some p)]
    returns the configuration for the benchmark [b] stored in the configuration
    file located in path [p]. And [parse_config b None] returns the default
    configuration for [b] instead.
    If [print] is specified, the configuration is printed out in [print].
    Otherwise nothing is printed *)
val parse_config :
  ?print:out_channel -> ('c, 't) Benchmark.poly -> string option -> 'c

(** Given a list of benchmarks [l], [generate_default l]
    generates a default configuration containing all the given benchmarks,
    and filled with the default configuration for each benchmark *)
val generate_default : Benchmark.t list -> t

(** [merge_config_files dst src ()] merges the configuration files found at
    paths [dst] and [src], in [dst]. If case of conflict, [src] overwrites [dst].
    If [delete_src] (default [false]) is set to [true], then the file [src] is
    deleted after merging. *)
val merge_config_files :
  ?delete_src:bool -> dst:string -> src:string -> unit -> unit

(** [edit_config path namespace] edits the config file found at [path], at the
    location [namespace] in the file.
    [input] is the method the json file is edited, which can be either
      - [`Stdin], reading from the standard input (default)
      - [`String s], parsing a string as a Json document
      - [`File f], using a Json file at path [f]
      - [`Edit e], using the given text editor [e]
*)
val edit_config :
  ?input:
    [< `Stdin
    | `String of string
    | `File of string
    | `Edit of string > `Edit
    `Stdin ] ->
  string ->
  Namespace.t ->
  unit
