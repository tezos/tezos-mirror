(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018-2022 Tarides <contact@tarides.com>                     *)
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

(* Determines the policy used to determine whether to add new
   objects to Irmin's index whenever they are exported to the
   data file. *)
type indexing_strategy =
  [ `Minimal (* only newly-exported commit objects are added to the index *)
  | `Always (* all newly-exported objects are added to the index *) ]

type t = {
  verbosity : [`Default | `Info | `Debug];
  index_log_size : int;
  lru_size : int;
  auto_flush : int;
  indexing_strategy : indexing_strategy;
}

(* Caps the number of entries stored in the Irmin's index. As a
   trade-off, increasing this value will delay index merges, and thus,
   make them more expensive in terms of disk usage, memory usage and
   computation time.*)
let index_log_size = 2_500_000

(* Caps the number of entries stored in the Irmin's LRU cache. As a
   trade-off, increasing this value will increase the memory
   consumption.*)
let lru_size = 15_000

(* This limit ensures that no trees with more than [auto_flush]
   mutations can exist in memory, bounding the memory usage of a
   single commit performed by a read-write process. As a trade-off,
   the intermediate flushed trees to the store might be unused and
   will have to be garbage collected later on to save space. *)
let auto_flush = 10_000

let default =
  {
    verbosity = `Default;
    index_log_size;
    lru_size;
    auto_flush;
    indexing_strategy = `Minimal;
  }

let max_verbosity a b =
  match (a, b) with
  | `Debug, _ | _, `Debug -> `Debug
  | `Info, _ | _, `Info -> `Info
  | _ -> `Default

let v =
  match Unix.getenv "TEZOS_CONTEXT" with
  | exception Not_found -> default
  | v ->
      List.fold_left
        (fun acc s ->
          match String.trim s with
          | "v" | "verbose" ->
              {acc with verbosity = max_verbosity acc.verbosity `Info}
          | "vv" -> {acc with verbosity = `Debug}
          | v -> (
              match String.split '=' v |> List.map String.trim with
              | ["index-log-size"; n] -> (
                  match int_of_string_opt n with
                  | None ->
                      Fmt.epr
                        "[WARNING] Trying to convert %s into an integer for \
                         index-log-size, but the conversion failed. Using \
                         default settings."
                        n ;
                      acc
                  | Some v -> {acc with index_log_size = v})
              | ["lru-size"; n] -> (
                  match int_of_string_opt n with
                  | None ->
                      Fmt.epr
                        "[WARNING] Trying to convert %s into an integer for \
                         lru-size, but the conversion failed. Using default \
                         settings."
                        n ;
                      acc
                  | Some v -> {acc with lru_size = v})
              | ["auto-flush"; n] -> (
                  match int_of_string_opt n with
                  | None ->
                      Fmt.epr
                        "[WARNING] Trying to convert %s into an integer for \
                         auto-flush, but the conversion failed. Using default \
                         settings."
                        n ;
                      acc
                  | Some v -> {acc with auto_flush = v})
              | ["indexing-strategy"; n] -> (
                  match n with
                  | "always" -> {acc with indexing_strategy = `Always}
                  | "minimal" -> {acc with indexing_strategy = `Minimal}
                  | x ->
                      Fmt.epr
                        "[WARNING]  Unable to parse indexing strategy '%s'. \
                         Expected one of { 'always', 'minimal' }."
                        x ;
                      acc)
              | unknown :: _ ->
                  Fmt.epr
                    "[WARNING] Unknow option %s detected in the environment \
                     variable TEZOS_CONTEXT."
                    unknown ;
                  acc
              | [] ->
                  Fmt.epr
                    "[WARNING] Empty string detected in the environment \
                     variable TEZOS_CONTEXT." ;
                  acc))
        default
        (String.split ',' v)
