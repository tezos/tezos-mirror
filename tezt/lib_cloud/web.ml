(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t = {process : Process.t; dir : string}

let monitoring ~agents =
  if Cli.monitoring then
    let str =
      agents
      |> List.map (fun agent ->
             let address =
               Agent.runner agent |> fun runner -> Runner.address (Some runner)
             in
             Format.asprintf
               "- [%s](http://%s:19999)"
               (Agent.name agent)
               address)
      |> String.concat "\n"
    in
    Format.asprintf "# Monitoring@.%s\n" str
  else
    "# Monitoring\n Monitoring disabled. Use `--monitoring` to activate it.\n"

let prometheus () =
  if Cli.prometheus then
    "# Prometheus\n [Prometheus dashboard](http://localhost:9090)"
  else "Prometheus disabled. Use `--prometheus` to activate it."

let markdown_content ~agents =
  [monitoring ~agents; prometheus ()] |> String.concat "\n"

let index dir = dir // "index.md"

let write t ~agents =
  (* The content is formatted in markdown and will be rendered in html via
     pandoc. *)
  let content = markdown_content ~agents in
  let dir = t.dir in
  let index = index dir in
  Base.with_open_out index (fun oc -> output_string oc content) ;
  Process.run
    "docker"
    [
      "run";
      "--rm";
      "--volume";
      Format.asprintf "%s:/data" dir;
      "pandoc/core";
      "index.md";
      "-o";
      "index.html";
      "-s";
    ]

let run ~port =
  let dir = Temp.dir "website" in
  let index = index dir in
  let process =
    Process.spawn
      "python3"
      [
        "-m";
        "http.server";
        string_of_int port;
        "--directory";
        Filename.dirname index;
      ]
  in
  Lwt.return {process; dir}

let start ~port ~agents =
  let* t = run ~port in
  let* () = write t ~agents in
  Lwt.return t

let shutdown t =
  Log.info "Shutting down the website..." ;
  Process.terminate t.process ;
  (* Do not fail if something happened during shutdown. *)
  let* _ = Process.wait t.process in
  Lwt.return_unit

let push_metric =
  let table = Hashtbl.create 11 in
  fun t ?(labels = []) ~name value ->
    let i = Unix.gettimeofday () in
    let labels_str =
      let inner =
        labels
        |> List.map (fun (name, value) ->
               Format.asprintf "%s = \"%s\"" name value)
        |> String.concat ","
      in
      match labels with [] -> "" | _ -> Format.asprintf "{ %s }" inner
    in
    let str =
      Format.asprintf
        "%s %s %d %d"
        name
        labels_str
        value
        (int_of_float (i *. 1_000.))
    in
    Hashtbl.replace table (name, labels) str ;
    (* Extension '.txt' is important so that the http server can infer a
       content mime for this page. *)
    let filename = t.dir // "metrics.txt" in
    let str =
      Hashtbl.to_seq table
      |> Seq.map (fun (_, v) -> v)
      |> List.of_seq |> String.concat "\n"
    in
    with_open_out filename (fun oc -> output_string oc str)
