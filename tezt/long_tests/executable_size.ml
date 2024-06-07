(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let team = Team.infrastructure

let test = "executable size"

let measurement = "executable size"

let field = "size"

let tag = "executable"

let executables =
  let read filename =
    read_file filename |> String.split_on_char '\n' |> List.map String.trim
    |> List.filter (( <> ) "")
  in
  (* We could also read experimental-executables and dev-executables but:
     - they may not all be built;
     - they do not affect users;
     - we should already cover most regressions with released executables
       if we assume that those link with basically all dependencies. *)
  read "script-inputs/released-executables"

let grafana_panels : Grafana.panel list =
  [
    Row "Test: executable size";
    Grafana.graphs_per_tags
      ~yaxis_format:"bytes"
      ~measurement
      ~test
      ~field
      ~tags:
        (List.map
           (fun executable -> (tag, Filename.basename executable))
           executables)
      ();
  ]

let register ~executors () =
  Long_test.register
    ~__FILE__
    ~title:test
    ~tags:["executable"; "size"]
    ~team
    ~timeout:(Minutes 2)
    ~executors
    ~uses_admin_client:false
    ~uses_client:false
    ~uses_node:false
  @@ fun () ->
  Fun.flip Lwt_list.iter_s executables @@ fun executable ->
  (* The size of executables should be rather stable.
     So we use a [~margin] of 5% for alerts instead of the 20% default. *)
  Long_test.measure_and_check_regression
    ~margin:0.05
    ~tags:[(tag, Filename.basename executable)]
    measurement
    ~field
  @@ fun () -> float (Unix.stat executable).st_size
