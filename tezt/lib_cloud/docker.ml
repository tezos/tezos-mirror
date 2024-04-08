(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)
let name = "docker"

let color = Log.Color.FG.yellow

let build ?(tag = "latest") ?dockerfile ~args () =
  let build_args =
    args
    |> List.map (fun (key, value) ->
           ["--build-arg"; Format.asprintf "%s=%s" key value])
    |> List.concat
  in
  let workspace = Lazy.force Env.workspace in
  let dockerfile =
    Option.value ~default:(Path.docker // workspace // ".Dockerfile") dockerfile
  in
  let tag = ["-t"; Format.asprintf "%s:%s" workspace tag] in
  let args = ["build"; "-f"; dockerfile] @ build_args @ tag @ ["."] in
  let value = Process.spawn ~name ~color "docker" args in
  let run = Process.check in
  {value; run}

let tag ?(tag = "latest") docker_registry =
  let workspace = Lazy.force Env.workspace in
  let args =
    [
      "tag";
      Format.asprintf "%s:%s" workspace tag;
      Format.asprintf "%s/%s:%s" docker_registry workspace tag;
    ]
  in
  let value = Process.spawn ~name ~color "docker" args in
  let run = Process.check in
  {value; run}

let push ?(tag = "latest") docker_registry =
  let workspace = Lazy.force Env.workspace in
  let args =
    ["push"; Format.asprintf "%s/%s:%s" docker_registry workspace tag]
  in
  let value = Process.spawn ~name ~color "docker" args in
  let run = Process.check in
  {value; run}

let pull ?(tag = "latest") docker_registry =
  let workspace = Lazy.force Env.workspace in
  let args =
    ["pull"; Format.asprintf "%s/%s:%s" docker_registry workspace tag]
  in
  let value = Process.spawn ~name ~color "docker" args in
  let run = Process.check in
  {value; run}

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
  let value =
    Process.spawn
      ~color
      "docker"
      (["run"] @ rm @ name @ network @ publish_ports
      @ [Format.asprintf "%s" image]
      @ args)
  in
  let run = Process.check in
  {value; run}

let kill container_name =
  let value = Process.spawn ~color "docker" ["kill"; container_name] in
  let run = Process.check in
  {value; run}

let rm container_name =
  let value = Process.spawn ~color "docker" ["rm"; container_name] in
  let run = Process.check in
  {value; run}

let cp container_name ~kind ~source ~destination =
  let value =
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
  in
  let run = Process.check in
  {value; run}
