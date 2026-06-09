(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Michelson runtime capacity benchmark (generic)
   Invocation:   dune exec etherlink/tezt/benchmarks/main.exe -- \
                   --file michelson_call.ml --contract <alias|path.tz> \
                   -T manual:0.5 [opts]
   Subject:      Generic capacity benchmark for a Michelson contract whose
                 storage is `unit` and whose default entrypoint takes `unit`.
                 Reuses the shared procedure in [Michelson_capacity].

                 The contract is selected with [--contract], which accepts
                 either a bundled contract alias (e.g. stress_INT_nat,
                 stress_DIPN) or a path to a `.tz` file on disk. The scenario
                 registers nothing when the flag is absent.
*)

open Etherlink_benchmark_lib
open Benchmark_utils

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

(* Resolve the [--contract] argument to a [(name, code)] pair: first try a
   bundled contract alias, otherwise treat the argument as a path to a .tz
   file on disk. [code] is a thunk read lazily at test time. *)
let resolve arg =
  match Contracts.Michelson.by_alias arg with
  | Some code -> (arg, code)
  | None ->
      ( Filename.remove_extension (Filename.basename arg),
        fun () -> read_file arg )

let register () =
  match parameters.contract with
  | None ->
      (* No [--contract] given: nothing to benchmark generically. *)
      ()
  | Some arg ->
      let name, code = resolve arg in
      Michelson_capacity.register_benchmark
        ~__FILE__
        ~name
        ~extra_tags:["michelson_call"]
        ~code
        ()
