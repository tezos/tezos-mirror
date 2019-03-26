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
  Lwt_exception.catch
    (fun () ->
      Lwt_io.with_file ~perm:0o777 ~mode:Lwt_io.output (tmp // "config/loggers")
        (fun out ->
          Lwt_io.write out
            {json|[
  { "logger":{"Stderr":{}} , "filters": { "SQL":"Error" , "":"Info"}},
  { "logger":{"File":{"file":"/var/run/bake-monitor/kiln.log"}}, "filters": { "": "Debug" } }
]|json}
      ) )
    ()
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
          Genspio.EDSL.(seq [exec ["cd"; dir]; exec (cmd :: args)])
  in
  Running_processes.start state kiln
  >>= fun kiln_process ->
  Console.say state
    EF.(
      wf "Kiln was started with nodes: %s, and network-id: %s"
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
