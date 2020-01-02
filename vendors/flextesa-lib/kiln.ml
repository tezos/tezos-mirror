open Internal_pervasives

module Configuration_directory = struct
  type t = {path: string; clean: bool; p2p_port: int}

  let generate state ?(protocol_execs = []) t ~peers ~sandbox_json ~nodes
      ~bakers ~network_string ~node_exec ~client_exec =
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
    let pwd = Caml.Sys.getenv "PWD" in
    let absolutize path =
      if Caml.Filename.is_relative path then pwd // path else path in
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
    System.write_file state ~perm:0o777
      (path // "enable-obsidian-node")
      ~content:(sprintf "%b" false)
    >>= fun () ->
    System.write_file state ~perm:0o777 (path // "binary-paths")
      ~content:
        Ezjsonm.(
          let absolutize exec =
            let path = Tezos_executable.get exec in
            absolutize path in
          dict
            [ ("node-path", string (absolutize node_exec))
            ; ("client-path", string (absolutize client_exec))
            ; ( "baker-endorser-paths"
              , list
                  (fun (p, bak, endo) ->
                    strings [p; absolutize bak; absolutize endo])
                  protocol_execs ) ]
          |> to_string ~minify:false)
    >>= fun () ->
    Running_processes.run_cmdf state " chmod -R 777 %s" path
    >>= fun _ -> return ()

  let cli_term state =
    let open Cmdliner in
    let docs = Manpage_builder.section state ~rank:4 ~name:"TOOLS: KILN" in
    Term.(
      pure (fun x clean ->
          Option.map x ~f:(fun (path, p2p_port) -> {path; p2p_port; clean}))
      $ Arg.(
          value
            (opt
               (some (pair ~sep:',' string int))
               None
               (info ~docs
                  ["generate-kiln-configuration-path"]
                  ~docv:"PATH,PORT"
                  ~doc:"Generate a kiln configuration at $(docv)")))
      $ Arg.(
          value
            (flag
               (info ~docs
                  ["clean-kiln-configuration"]
                  ~doc:"Delete configuration path before generating it"))))
end
