open Internal_pervasives
open Console

let dump_connection =
  let open EF in
  let open Tezos_node in
  function
  | `Duplex (na, nb) ->
      af "%s:%d <-> %s:%d" na.id na.p2p_port nb.id nb.p2p_port
  | `From_to (na, nb) ->
      haf "%s:%d --> %s:%d" na.id na.p2p_port nb.id nb.p2p_port
  | `Missing (na, int) ->
      ksprintf shout "%s:%d --> ???:%d" na.id na.p2p_port int

let dump_connections state nodes =
  let conns = Tezos_node.connections nodes in
  say state
    (let open EF in
    desc_list (haf "Connections:") (List.map conns ~f:dump_connection))

let clear_root state =
  let root = Paths.(root state) in
  System_error.catch
    (fun () -> ksprintf Lwt_unix.system "rm -fr %s" (Caml.Filename.quote root))
    ()
  >>= function
  | Unix.WEXITED 0 -> return ()
  | _ -> System_error.fail_fatalf "cannot delete root path (%S)" root

let wait_for ?(attempts_factor = 0.) state ~attempts ~seconds f =
  let rec attempt nth =
    let again () = attempt (nth + 1) in
    f nth
    >>= function
    | `Done x -> return x
    | `Not_done msg when nth < attempts ->
        let sleep_time = Float.((attempts_factor * of_int nth) + seconds) in
        say state
          EF.(
            wf "%s: attempt %d/%d, sleeping %.02f seconds" msg nth attempts
              sleep_time)
        >>= fun () -> System.sleep sleep_time >>= fun () -> again ()
    | `Not_done msg -> fail (`Waiting_for (msg, `Time_out)) in
  attempt 1

let kill_node state nod =
  Running_processes.find_process_by_id ~only_running:true state
    ~f:(String.( = ) nod.Tezos_node.id)
  >>= fun states ->
  ( match states with
  | [one] -> return one
  | _ ->
      System_error.fail_fatalf "Expecting one state for node %s"
        nod.Tezos_node.id )
  >>= fun node_state_0 -> Running_processes.kill state node_state_0

let restart_node ~client_exec state nod =
  Running_processes.start state (Tezos_node.process state nod)
  >>= fun _ ->
  let client = Tezos_client.of_node nod ~exec:client_exec in
  say state
    EF.(wf "Started node %s, waiting for bootstrap …" nod.Tezos_node.id)
  >>= fun () -> Tezos_client.wait_for_node_bootstrap state client

module Counter_log = struct
  type t = (string * int) list ref

  let create () = ref []
  let add t s n = t := (s, n) :: !t
  let incr t s = add t s 1
  let sum t = List.fold !t ~init:0 ~f:(fun prev (_, s) -> prev + s)

  let to_table_string t =
    let total = "**Total:**" in
    let longest =
      List.fold !t ~init:total ~f:(fun p (n, _) ->
          if String.length p < String.length n then n else p) in
    List.rev_map
      ((total, sum t) :: !t)
      ~f:(fun (cmt, n) ->
        sprintf "| %s %s|% 8d|" cmt
          (String.make (String.length longest - String.length cmt + 2) '.')
          n)
    |> String.concat ~sep:"\n"
end

module Netstat = struct
  let check_version state =
    Running_processes.run_cmdf state "netstat --version"
    >>= fun version_res ->
    match version_res#status with
    | Unix.WEXITED 0 ->
        (* This is a linux-ish netstat: *)
        return `Fine
    | _ -> return `Not_right

  let netstat_dash_nut state =
    check_version state
    >>= function
    | `Fine ->
        Running_processes.run_cmdf state "netstat -nut"
        >>= fun res ->
        Process_result.Error.fail_if_non_zero res "netstat -nut command"
        >>= fun () ->
        let rows =
          List.filter_mapi res#out ~f:(fun idx line ->
              match
                String.split line ~on:' '
                |> List.filter_map ~f:(fun s ->
                       match String.strip s with "" -> None | s -> Some s)
              with
              | ("tcp" | "tcp6") :: _ as row -> Some (`Tcp (idx, row))
              | _ -> Some (`Wrong (idx, line))) in
        return rows
    | `Not_right ->
        Console.say state
          EF.(
            desc (shout "Warning:")
              (wf
                 "This does not look like a linux-netstat; port-availability \
                  checks are hence disabled."))
        >>= fun () -> return []

  let all_listening_ports rows =
    List.filter_map rows ~f:(function
      | `Tcp (_, _ :: _ :: _ :: addr :: _) as row -> (
        match String.split addr ~on:':' with
        | [_; port] -> ( try Some (Int.of_string port, row) with _ -> None )
        | _ -> None )
      | _ -> None)

  let used_listening_ports state =
    netstat_dash_nut state
    >>= fun rows ->
    let all_used = all_listening_ports rows in
    return all_used
end

module System_dependencies = struct
  module Error = struct
    type t = [`Precheck_failure of string]

    let pp fmt (`Precheck_failure f) =
      Caml.Format.fprintf fmt "Failed precheck: %S" f

    let failf fmt =
      Caml.Format.kasprintf (fun s -> fail (`Precheck_failure s)) fmt
  end

  open Error

  let precheck ?(using_docker = false) ?(protocol_paths = [])
      ?(executables : Tezos_executable.t list = []) state how_to_react =
    let commands_to_check =
      (if using_docker then ["docker"] else [])
      @ ["setsid"; "curl"; "netstat"]
      @ List.map executables ~f:Tezos_executable.get in
    List.fold ~init:(return []) commands_to_check ~f:(fun prev_m cmd ->
        prev_m
        >>= fun prev ->
        Running_processes.run_cmdf state "type %s" (Caml.Filename.quote cmd)
        >>= fun result ->
        match result#status with
        | Unix.WEXITED 0 -> return prev
        | _ -> return (`Missing_exec (cmd, result) :: prev))
    >>= fun errors_or_warnings ->
    Netstat.check_version state
    >>= (function
          | `Fine -> return errors_or_warnings
          | `Not_right -> return (`Wrong_netstat :: errors_or_warnings))
    >>= fun errors_or_warnings ->
    List.fold protocol_paths ~init:(return errors_or_warnings)
      ~f:(fun prev_m path ->
        prev_m
        >>= fun prev ->
        System_error.catch Lwt_unix.file_exists (path // "TEZOS_PROTOCOL")
        >>= function
        | true -> return prev
        | false -> return (`Not_a_protocol_path path :: prev))
    >>= fun errors_or_warnings ->
    match (errors_or_warnings, how_to_react) with
    | [], _ -> return ()
    | more, `Or_fail -> (
        let is_not_just_a_warning = function
          | `Wrong_netstat | `Missing_exec ("netstat", _) -> false
          | `Missing_exec _ | `Not_a_protocol_path _ -> true in
        Console.sayf state
          Fmt.(
            fun ppf () ->
              vbox ~indent:2
                (fun ppf () ->
                  string ppf "System dependencies failed precheck:" ;
                  List.iter more ~f:(fun item ->
                      cut ppf () ;
                      box ~indent:2
                        (fun ppf () ->
                          pf ppf "* %s "
                            ( if is_not_just_a_warning item then "Fatal-error:"
                            else "Warning:" ) ;
                          match item with
                          | `Missing_exec (path, _) ->
                              (* pp_open_hovbox ppf 0 ; *)
                              kstr (text ppf) "Missing executable `%s`." path
                          | `Wrong_netstat -> text ppf "Wrong netstat version."
                          | `Not_a_protocol_path path ->
                              kstr (text ppf) "`%s` is not a protocol." path)
                        ppf ()))
                ppf ())
        >>= fun () ->
        ( if
          List.exists more ~f:(function
            | `Wrong_netstat | `Missing_exec ("setsid", _) -> true
            | _ -> false)
        then
          Console.say state
            EF.(
              desc (shout "Warning:")
                (wf
                   "This does not look like a standard Linux-ish environment. \
                    If you are on MacOSX, see \
                    https://gitlab.com/tezos/flextesa/blob/master/README.md#macosx-users "))
        else return () )
        >>= fun () ->
        ( if
          List.exists more ~f:(function
            | `Missing_exec ("tezos-node", _)
              when Caml.Sys.file_exists ("." // "tezos-node") ->
                true
            | _ -> false)
        then
          Console.say state
            EF.(
              desc (prompt "Tip:")
                (wf
                   "The `tezos-node` executable is missing but there seems to \
                    be one in the current directory, maybe you can pass \
                    `./tezos-node` with the right option (see `--help`) or \
                    simply add `export PATH=.:$PATH` to allow unix tools to \
                    find it."))
        else return () )
        >>= fun () ->
        let non_warning_errors = List.filter more ~f:is_not_just_a_warning in
        match non_warning_errors with
        | [] ->
            Console.say state
              EF.(
                wf "Pre-check noticed only %d warnings, no errors"
                  (List.length more))
        | _ ->
            failf "%d errors were raised during precheck."
              (List.length non_warning_errors) )
end

module Shell_environement = struct
  type t = {aliases: (string * string * string) list}

  let make ~aliases = {aliases}

  let build state ~clients =
    let aliases =
      List.concat_mapi clients ~f:(fun i c ->
          let call =
            List.map ~f:Caml.Filename.quote
              (Tezos_client.client_call state c []) in
          let cmd exec = String.concat ~sep:" " (exec :: call) in
          let extra =
            let help = "Call the tezos-client used by the sandbox." in
            match Tezos_executable.get c.Tezos_client.exec with
            | "tezos-client" -> []
            | f when Caml.Filename.is_relative f ->
                [(sprintf "c%d" i, cmd (Caml.Sys.getcwd () // f), help)]
            | f -> [(sprintf "c%d" i, cmd (Caml.Sys.getcwd () // f), help)]
          in
          [ ( sprintf "tc%d" i
            , cmd "tezos-client"
            , "Call the `tezos-client` from the path." ) ]
          @ extra) in
    make ~aliases

  let write state {aliases} ~path =
    let content =
      String.concat ~sep:"\n"
        ( ["# Shell-environment generated by a flextesa-sandbox"]
        @ List.concat_map aliases ~f:(fun (k, v, doc) ->
              [ sprintf "echo %s"
                  (sprintf "Defining alias %s: %s" k doc |> Caml.Filename.quote)
              ; sprintf "alias %s=%s" k (Caml.Filename.quote v) ]) ) in
    System.write_file state path ~content

  let help_command state t ~path =
    Console.Prompt.unit_and_loop
      ~description:"Show help about the shell-environment." ["help-env"]
      (fun _sexps ->
        Console.sayf state
          More_fmt.(
            fun ppf () ->
              let pick_and_alias ppf () =
                let k, _, _ = List.random_element_exn t.aliases in
                string ppf k in
              vertical_box ~indent:2 ppf (fun ppf ->
                  tag "prompt" ppf (fun ppf -> wf ppf "Shell Environment") ;
                  cut ppf () ;
                  wf ppf "* A loadable shell environment is available at `%s`."
                    path ;
                  cut ppf () ;
                  wf ppf
                    "* It contains %d POSIX-shell aliases (compatible with \
                     `bash`, etc.)."
                    (List.length t.aliases) ;
                  cut ppf () ;
                  cut ppf () ;
                  wf ppf "Example:" ;
                  cut ppf () ;
                  cut ppf () ;
                  pf ppf "    . %s" path ;
                  cut ppf () ;
                  pf ppf "    %a list known addresses" pick_and_alias () ;
                  cut ppf () ;
                  pf ppf "    %a rpc get /chains/main/blocks/head/metadata"
                    pick_and_alias () ;
                  cut ppf ())))
end
