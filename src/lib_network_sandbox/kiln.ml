open Internal_pervasives

type t =
  { run: [`Docker of string | `Dev_mode of string * string]
  ; port: int
  ; postgres: [`Docker of int] option
  ; pause_for_user: bool }

let make ~run ~port ?postgres ~pause_for_user () =
  {run; port; postgres; pause_for_user}

let default_docker_image = "obsidiansystems/tezos-bake-monitor:0.4.0"
let default_postgres_port = 4_532

let default =
  make ~run:(`Docker default_docker_image) ~port:8086
    ~postgres:(`Docker default_postgres_port) ~pause_for_user:false ()

let start ?(network_id = "zeronet") state {run; port; postgres; pause_for_user}
    ~node_uris ~bakers =
  let name nonbase = sprintf "flxts-%s" nonbase in
  let kiln_port = port in
  ( match postgres with
  | Some (`Docker pg_port) ->
      let pg_password = Tezos_protocol.Key.Of_name.pubkey "pg-password" in
      let pg =
        Running_processes.Process.docker_run (name "kiln-postgres-db")
          ~image:"postgres"
          ~options:
            [ "-p"; sprintf "%d:5432" pg_port; "-e"
            ; sprintf "POSTGRES_PASSWORD=%s" pg_password ]
          ~args:[]
      in
      let pg_cli_option =
        sprintf
          "--pg-connection=host=localhost port=%d dbname=postgres \
           user=postgres password=%s"
          pg_port pg_password
      in
      Running_processes.start state pg
      >>= fun pg_process ->
      Helpers.wait_for state ~attempts:20 ~seconds:8. (fun attempt ->
          Running_processes.run_cmdf state
            "docker run --rm -e PGPASSWORD=%s --network host -it postgres \
             psql -h localhost -p %d -U postgres -w -c '\\l'"
            pg_password pg_port
          >>= fun res ->
          Console.display_errors_of_command state res
          >>= function
          | true -> return (`Done ())
          | false ->
              return
                (`Not_done
                  (sprintf "Waiting for postgres to be ready (%d)" attempt)) )
      >>= fun () -> return (Some (pg_process, pg_cli_option))
  | None -> return None )
  >>= fun pg_opt ->
  (* We need to use /tmp and not the root-path because of Docker access rights. *)
  let tmp = "/tmp" // sprintf "kiln-config-%d" port in
  Running_processes.run_cmdf state
    "rm -fr %s ; mkdir -p %s/config ; chmod -R 777 %s" tmp tmp tmp
  >>= fun _ ->
  System.write_file state ~perm:0o777 (tmp // "config/loggers")
    ~content:
      {json|[
  { "logger":{"Stderr":{}} , "filters": { "SQL":"Error" , "":"Info"}}
]|json}
  >>= fun () ->
  System.write_file state ~perm:0o777
    (tmp // "config/kiln-node-custom-args")
    ~content:
      (sprintf
         "--net-addr 0.0.0.0:10000 --private-mode --no-bootstrap-peers %s  \
          --bootstrap-threshold 0 --connections 5 --sandbox \
          /home/smondet/tmp/metetests//0_mininet-test-data/protocol-default-and-command-line/sandbox.json"
         ( List.map
             (List.init 5 ~f:(fun i -> 20_001 + (2 * i)))
             ~f:(sprintf "--peer 127.0.0.1:%d")
         |> String.concat ~sep:" " ))
  >>= fun () ->
  Running_processes.run_cmdf state " chmod -R 777 %s" tmp
  >>= fun _ ->
  let kiln =
    let args =
      (match pg_opt with None -> [] | Some (_, cli) -> [cli])
      @ [ "--nodes"
        ; String.concat ~sep:"," node_uris
        ; "--bakers"
        ; String.concat ~sep:","
            (List.map bakers ~f:(fun (n, pkh) -> sprintf "%s@%s" pkh n))
        ; "--network"; network_id; "--"; "--port"; Int.to_string kiln_port ]
    in
    match run with
    | `Docker image ->
        Running_processes.Process.docker_run (name "kiln-backend") ~image
          ~options:
            ["--network"; "host"; "-v"; sprintf "%s:/var/run/bake-monitor" tmp]
          ~args
    | `Dev_mode (dir, cmd) ->
        Running_processes.Process.genspio (name "kiln-dev-backend")
          Genspio.EDSL.(
            seq
              [ exec ["cd"; tmp]
              ; exec ["echo"; sprintf "tmp is %s" tmp]
              ; call [str "echo"; getenv (str "PATH")]
              ; exec ["sh"; "-c"; sprintf "ln -sf %s/* %s" dir tmp]
              ; exec ["ls"; "-la"]
              ; exec (cmd :: sprintf "--kiln-data-dir=%s" tmp :: args) ])
  in
  Running_processes.start state kiln
  >>= fun kiln_process ->
  Console.say state
    EF.(
      wf
        "Kiln was started (cf. <http://localhost:%d>, Data-dir: %s) with \
         nodes: %s, and network-id: %s"
        kiln_port tmp
        (List.map node_uris ~f:(sprintf "`%s`") |> String.concat ~sep:", ")
        network_id)
  >>= fun () ->
  ( match bakers with
  | ([] | _) when not pause_for_user -> return ()
  | _ ->
      Interactive_test.Pauser.generic state ~force:true
        EF.
          [ wf "Started Kiln with Nodes and Bakers."
          ; wf "You may open <http://localhost:%d> and quit this prompt (`q`)."
              kiln_port ] )
  >>= fun () -> return (Option.map ~f:fst pg_opt, kiln_process)

let cli_term () =
  let open Cmdliner in
  Term.(
    pure (fun run_docker run_dev_opt port postgres pause_for_user -> function
      | true ->
          let run = Option.value run_dev_opt ~default:run_docker in
          Some (make ~run ?postgres ~port ~pause_for_user ())
      | false -> None )
    $ Arg.(
        let doc = "Set the Kiln docker image." in
        pure (fun docker_image -> `Docker docker_image)
        $ value
            (opt string default_docker_image (info ["kiln-docker-image"] ~doc)))
    $ Arg.(
        let doc =
          "Set the path to the directory containing Kiln's `./backend`."
        in
        pure (Option.map ~f:(fun dir -> `Dev_mode (dir, "./backend")))
        $ value (opt (some string) None (info ["kiln-dev-mode"] ~doc)))
    $ Arg.(
        value
          (opt int default.port (info ["kiln-port"] ~doc:"Set the kiln port.")))
    $ Arg.(
        pure (function
          | false -> fun port -> Some (`Docker port)
          | true -> fun _ -> None )
        $ value
            (flag
               (info ["kiln-without-postgres"]
                  ~doc:"Let Kiln run its own Postgres."))
        $ value
            (opt int default_postgres_port
               (info ["kiln-pg-port"] ~doc:"Set the Postgres port for Kiln.")))
    $ Arg.(
        value
          (flag
             (info ["pause-to-display-kiln"]
                ~doc:
                  "Add an interactive pause to show the user the URI of \
                   Kiln's GUI.")))
    $ Arg.(
        value
          (flag
             (info ["with-kiln"]
                ~doc:
                  "Add Kiln to the network (may make the test partially \
                   interactive)."))))

module Configuration_directory = struct
  type t = {path: string; clean: bool; p2p_port: int}

  let generate state t ~peers ~sandbox_json ~nodes ~bakers ~network_string
      ~node_exec ~client_exec ~protocol_execs =
    (* For now, client-exec in Kiln is not protocol dependent, this
     should be fixed soon. *)
    let {path; clean; p2p_port} = t in
    ( if clean then
      Running_processes.run_cmdf state
        "rm -fr %s ; mkdir -p %s ; chmod -R 777 %s" path path path
      >>= fun _ -> return ()
    else return () )
    >>= fun _ ->
    System.write_file state ~perm:0o777 (path // "loggers")
      ~content:
        Ezjsonm.(
          `A
            [ dict
                [ ("logger", dict [("Stderr", dict [])])
                ; ( "filters"
                  , dict [("SQL", string "Error"); ("", string "Info")] ) ] ]
          |> to_string)
    (* {json|[{ "logger":{"Stderr":{}} , "filters": { "SQL":"Error" , "":"Info"}}]|json} *)
    >>= fun () ->
    let node_config = path // "node-config.json" in
    System.write_file state ~perm:0o777 node_config
      ~content:
        Ezjsonm.(
          dict
            [ ("data-dir", string (path // "node-data-dir-unused"))
            ; ("rpc", dict [("listen-addr", string "127.0.0.1")])
            ; ("p2p", dict [("expected-proof-of-work", int 1)]) ]
          |> to_string)
    >>= fun () ->
    System.write_file state ~perm:0o777
      (path // "kiln-node-net-port")
      ~content:(sprintf "%d" p2p_port)
    >>= fun () ->
    let pwd = Sys.getenv "PWD" in
    let absolutize path =
      if Filename.is_relative path then pwd // path else path
    in
    System.write_file state ~perm:0o777
      (path // "kiln-node-custom-args")
      ~content:
        (sprintf
           "--config-file %s --private-mode --no-bootstrap-peers %s  \
            --bootstrap-threshold 0 --connections %d --sandbox %s"
           (absolutize node_config)
           ( List.map peers ~f:(sprintf "--peer 127.0.0.1:%d")
           |> String.concat ~sep:" " )
           (List.length peers - 1)
           sandbox_json)
    >>= fun () ->
    System.write_file state ~perm:0o777 (path // "nodes")
      ~content:(String.concat ~sep:"," nodes)
    >>= fun () ->
    System.write_file state ~perm:0o777 (path // "bakers")
      ~content:
        ( List.map bakers ~f:(fun (n, addr) -> sprintf "%s@%s" addr n)
        |> String.concat ~sep:"," )
    >>= fun () ->
    System.write_file state ~perm:0o777 (path // "network")
      ~content:network_string
    >>= fun () ->
    System.write_file state ~perm:0o777 (path // "binary-paths")
      ~content:
        Ezjsonm.(
          let absolutize exec =
            let path = Tezos_executable.get exec in
            absolutize path
          in
          dict
            [ ("node-path", string (absolutize node_exec))
            ; ("client-path", string (absolutize client_exec))
            ; ( "baker-endorser-paths"
              , list
                  (fun (p, bak, endo) ->
                    strings [p; absolutize bak; absolutize endo] )
                  protocol_execs ) ]
          |> to_string ~minify:false)
    >>= fun () ->
    Running_processes.run_cmdf state " chmod -R 777 %s" path
    >>= fun _ -> return ()

  let cli_term () =
    let open Cmdliner in
    Term.(
      pure (fun x clean ->
          Option.map x ~f:(fun (path, p2p_port) -> {path; p2p_port; clean}) )
      $ Arg.(
          value
            (opt
               (some (pair ~sep:',' string int))
               None
               (info
                  ["generate-kiln-configuration-path"]
                  ~docv:"PATH,PORT"
                  ~doc:"Generate a kiln configuration at $(docv)")))
      $ Arg.(
          value
            (flag
               (info
                  ["clean-kiln-configuration"]
                  ~doc:"Delete configuration path before generating it"))))
end
