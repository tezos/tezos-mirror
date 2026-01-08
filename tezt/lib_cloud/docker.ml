(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let name = "docker"

let color = Log.Color.FG.yellow

let macos_platform_arg =
  if Env.macosx then ["--platform"; "linux/amd64"] else []

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
  let args =
    ["build"; "-f"; dockerfile] @ macos_platform_arg @ build_args @ tag @ ["."]
  in
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

let pull ?runner ?image_name ?alias ?(tag = "latest") ~registry_uri () =
  let alias = Option.value ~default:Env.dockerfile_alias alias in
  let image_name = Option.value ~default:alias image_name in
  let args = ["pull"; Format.asprintf "%s/%s:%s" registry_uri image_name tag] in
  Process.spawn ?runner ~name ~color "docker" args

let network ~command ~network_name =
  Process.spawn ~color "docker" (["network"] @ [command] @ [network_name])

let run ?runner ?(rm = false) ?name ?(detach = false) ?network ?publish_ports
    ?custom_docker_options ?volumes image args =
  let publish_ports =
    match publish_ports with
    | None -> []
    | Some (hstart, hstop, rstart, rstop) ->
        ["-p"; Format.asprintf "%s-%s:%s-%s" hstart hstop rstart rstop]
  in
  (* NET_ADMIN capability is currently used in order to
     tweak network latency with [tc] *)
  let net_admin =
    (* Let's prevent the user to accidentally mess with their network configuration *)
    let enable = not (Cli.localhost && Cli.docker_host_network) in
    if enable then (
      Log.warn "Running docker containers with NET_ADMIN capability" ;
      ["--cap-add"; "NET_ADMIN"])
    else []
  in
  let network =
    match network with None -> [] | Some network -> ["--network"; network]
  in
  let name = match name with None -> [] | Some name -> ["--name"; name] in
  let rm = if rm then ["--rm"] else [] in
  let detach = if detach then ["-d"] else [] in
  let volumes =
    match volumes with
    | None -> []
    | Some volumes ->
        let arg_of_volume (host, container) =
          ["-v"; Format.asprintf "%s:%s" host container]
        in
        List.map arg_of_volume volumes |> List.flatten
  in
  (* [init] can be used to ensure signals are forwarded properly to
     the entrypoint run by the container. *)
  Process.spawn
    ?runner
    ~color
    "docker"
    (["run"] @ detach @ rm @ name @ macos_platform_arg @ volumes @ network
   @ net_admin @ publish_ports
    @ Option.value ~default:[] custom_docker_options
    @ [Format.asprintf "%s" image]
    @ args)

let kill container_name = Process.spawn ~color "docker" ["kill"; container_name]

let rm ?runner ?(force = false) container_name =
  Log.info "Docker.rm '%s'" container_name ;
  Process.spawn
    ?runner
    ~color
    "docker"
    (["rm"] @ (if force then ["--force"] else []) @ [container_name])

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
