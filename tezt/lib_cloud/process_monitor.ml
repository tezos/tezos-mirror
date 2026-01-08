(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  listening_port : int;
  mutable monitored_processes : (string * string) list;
  mutable checked_in_path : bool;
}

let executable = "prometheus-process-exporter"

let init ~listening_port =
  Log.report "Process_monitor: listening on %d" listening_port ;
  {listening_port; monitored_processes = []; checked_in_path = false}

let encoding =
  let open Data_encoding in
  conv
    (fun {listening_port; _} -> listening_port)
    (fun listening_port -> init ~listening_port)
    (obj1 (req "listening_port" int31))

let add_binary t ~group ~name =
  if List.mem (group, name) t.monitored_processes then false
  else (
    Log.report "Process_monitor: enable watching %s in group %s" name group ;
    t.monitored_processes <- (group, name) :: t.monitored_processes ;
    true)

let get_binaries t = t.monitored_processes

let get_port t = t.listening_port

let reload t run_cmd =
  let* () =
    (* Check if in path only the first time *)
    if not t.checked_in_path then
      let* r =
        run_cmd ~detach:false "sh" ["-c"; "command -v " ^ executable]
        |> Process.wait
      in
      let () =
        match r with
        | WEXITED 0 -> t.checked_in_path <- true
        | _ -> Log.warn "Process_monitor: cannot find executable: %s" executable
      in
      Lwt.return_unit
    else Lwt.return_unit
  in
  let processes_names = List.map snd (get_binaries t) in
  let processes = String.concat "," processes_names in
  Log.report "Process_monitor: restarting prometheus-process-exporter" ;
  Log.report "Process_monitor: monitored processes : {%s}" processes ;
  let* _ = run_cmd ~detach:false "pkill" ["-f"; executable] |> Process.wait in
  let cmd, args =
    ( executable,
      ["-web.listen-address"; Format.asprintf ":%d" t.listening_port]
      @ ["--procnames"; processes] )
  in
  Log.report "Process_monitor: [exec] %s %s" cmd (String.concat " " args) ;
  let* _ = run_cmd ~detach:true cmd args |> Process.wait in
  Lwt.return_unit
