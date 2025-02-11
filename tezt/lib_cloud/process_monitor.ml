(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  listening_port : int;
  mutable monitored_processes : (string * string) list;
}

let executable = "prometheus-process-exporter"

let encoding =
  let open Data_encoding in
  conv
    (fun {listening_port; monitored_processes = _} -> listening_port)
    (fun listening_port -> {listening_port; monitored_processes = []})
    (obj1 (req "listening_port" int31))

let init ~listening_port = {listening_port; monitored_processes = []}

let add_binary t ~group ~name =
  if List.mem (group, name) t.monitored_processes then false
  else (
    t.monitored_processes <- (group, name) :: t.monitored_processes ;
    true)

let get_binaries t = t.monitored_processes

let get_port t = t.listening_port

let reload t run_cmd =
  let processes_names = List.map snd (get_binaries t) in
  let processes = String.concat "," processes_names in
  let* r = run_cmd ~detach:false "command" ["-v"; executable] |> Process.wait in
  let () =
    match r with
    | WEXITED 0 -> ()
    | _ -> Test.fail "Cannot find executable: %s" executable
  in
  Log.warn
    "Restarting prometheus-process-exporter; monitored processes = {%s}"
    processes ;
  let* _ = run_cmd ~detach:false "pkill" ["-f"; executable] |> Process.wait in
  let* () =
    run_cmd
      ~detach:true
      executable
      (["-web.listen-address"; Format.asprintf ":%d" t.listening_port]
      @ ["--procnames"; processes])
    |> Process.check
  in
  Lwt.return_unit
