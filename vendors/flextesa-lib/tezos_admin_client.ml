open Internal_pervasives

type t = {id: string; port: int; exec: Tezos_executable.t}

let base_dir t ~state = Paths.root state // sprintf "Admin-client-base-%s" t.id

let of_client ~exec n =
  let id = sprintf "A-%s" n.Tezos_client.id in
  let port = n.Tezos_client.port in
  {id; port; exec}

let of_node ~exec n =
  let id = sprintf "C-%s" n.Tezos_node.id in
  let port = n.Tezos_node.rpc_port in
  {id; port; exec}

let make_command state t args =
  let open Tezos_executable.Make_cli in
  Tezos_executable.call state t.exec
    ~path:(base_dir t ~state // "exec-admin")
    (optf "port" "%d" t.port @ opt "base-dir" (base_dir ~state t) @ args)

module Command_error = struct
  let failf ?client ?args fmt =
    let attach =
      Option.value_map ~default:[] args ~f:(fun l ->
          [("arguments", `String_list l)])
      @ Option.value_map ~default:[] client ~f:(fun c ->
            [("client-id", `String_value c.id)]) in
    Process_result.Error.wrong_behavior ~attach fmt
end

open Command_error

let successful_command admin state args =
  Running_processes.run_cmdf state "sh -c %s"
    ( make_command state admin args
    |> Genspio.Compile.to_one_liner |> Caml.Filename.quote )
  >>= fun res ->
  Console.display_errors_of_command state res
  >>= function
  | true -> return res
  | false ->
      failf ~args "Admin-command failure: %s" (String.concat ~sep:" " args)

let inject_protocol admin state ~path =
  successful_command admin state ["inject"; "protocol"; path]
  >>= fun res ->
  String.concat ~sep:" " res#out
  |> String.split ~on:' ' |> List.map ~f:String.strip
  |> (function
       | _ :: _ :: hash :: _ when Char.equal hash.[0] 'P' -> return hash
       | _ ->
           failf "inject protocol: cannot parse hash of protocol: %s"
             (String.concat ~sep:", " (List.map ~f:(sprintf "%S") res#out)))
  >>= fun hash -> return (res, hash)
