(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let name = "docker"

let color = Log.Color.FG.yellow

let build ?image_name ?alias ?(tag = "latest") ?dockerfile ~args () =
  let build_args =
    args
    |> List.map (fun (key, value) ->
           ["--build-arg"; Format.asprintf "%s=%s" key value])
    |> List.concat
  in
  let alias = Option.value ~default:Env.dockerfile_alias alias in
  let dockerfile = Option.value ~default:(Path.dockerfile ~alias) dockerfile in
  let image_name = Option.value ~default:alias image_name in
  let tag = ["-t"; Format.asprintf "%s:%s" image_name tag] in
  let args = ["build"; "-f"; dockerfile] @ build_args @ tag @ ["."] in
  Process.spawn ~name ~color "docker" args

let tag ?image_name ?alias ?(tag = "latest") ~registry_uri () =
  let alias = Option.value ~default:Env.dockerfile_alias alias in
  let image_name = Option.value ~default:alias image_name in
  let args =
    [
      "tag";
      Format.asprintf "%s:%s" image_name tag;
      Format.asprintf "%s/%s:%s" registry_uri image_name tag;
    ]
  in
  Process.spawn ~name ~color "docker" args

let push ?image_name ?alias ?(tag = "latest") ~registry_uri () =
  let alias = Option.value ~default:Env.dockerfile_alias alias in
  let image_name = Option.value ~default:alias image_name in
  let args = ["push"; Format.asprintf "%s/%s:%s" registry_uri image_name tag] in
  Process.spawn ~name ~color "docker" args

let pull ?image_name ?alias ?(tag = "latest") ~registry_uri () =
  let alias = Option.value ~default:Env.dockerfile_alias alias in
  let image_name = Option.value ~default:alias image_name in
  let args = ["pull"; Format.asprintf "%s/%s:%s" registry_uri image_name tag] in
  Process.spawn ~name ~color "docker" args

let run ?(rm = false) ?name ?network ?publish_ports image args =
  let publish_ports =
    match publish_ports with
    | None -> []
    | Some (hstart, hstop, rstart, rstop) ->
        ["-p"; Format.asprintf "%s-%s:%s-%s" hstart hstop rstart rstop]
  in
  let network =
    match network with None -> [] | Some network -> ["--network"; network]
  in
  let name = match name with None -> [] | Some name -> ["--name"; name] in
  let rm = if rm then ["--rm"] else [] in
  (* [init] can be used to ensure signals are forwarded properly to
     the entrypoint run by the container. *)
  Process.spawn
    ~color
    "docker"
    (["run"] @ rm @ name @ network @ publish_ports
    @ [Format.asprintf "%s" image]
    @ args)

let kill container_name = Process.spawn ~color "docker" ["kill"; container_name]

let rm container_name = Process.spawn ~color "docker" ["rm"; container_name]

let cp container_name ~kind ~source ~destination =
  match kind with
  | `From_host ->
      Process.spawn
        ~color
        "docker"
        ["cp"; source; Format.asprintf "%s:%s" container_name destination]
  | `To_host ->
      Process.spawn
        ~color
        "docker"
        ["cp"; Format.asprintf "%s:%s" container_name source; destination]
