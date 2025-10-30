(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = {process : Process.t; stdin : Lwt_io.output_channel}

exception Could_not_connect

exception Connection_closed

exception Timeout of {timeout : float; origin : string}

let get_unique_name =
  let name_counts = ref String_map.empty in
  fun name ->
    let index =
      match String_map.find_opt name !name_counts with None -> 0 | Some i -> i
    in
    name_counts := String_map.add name (index + 1) !name_counts ;
    name ^ "#" ^ string_of_int index

let connect ?runner ?hooks ?name url =
  let name =
    match name with Some n -> n | None -> get_unique_name "websocket_client"
  in
  let url = Uri.with_scheme (Uri.of_string url) (Some "ws") |> Uri.to_string in
  let process, stdin =
    Process.spawn_with_stdin ~name ?runner ?hooks "websocat" [url; "-E"]
  in
  let () =
    try Unix.kill (Process.pid process) 0
    with _ ->
      Log.error "%s could not connect to %s" name url ;
      raise Could_not_connect
  in
  return {process; stdin}

let send_raw {stdin; _} msg =
  let* () = Lwt_io.write stdin (msg ^ "\n") in
  unit

let read_json ~origin {process; _} =
  let max_size =
    10 * 1024 * 1024
    (* 10MB *)
  in
  let ch = Process.stdout process in
  let buff = Buffer.create 256 in
  let rec loop () =
    let* line = Lwt_io.read_line_opt ch in
    match line with
    | None -> raise Connection_closed
    | Some line -> (
        Buffer.add_string buff line ;
        match JSON.parse_opt ~origin (Buffer.contents buff) with
        | None when Buffer.length buff >= max_size ->
            Format.ksprintf
              failwith
              "Could not parse JSON from websocket %d bytes."
              max_size
        | None -> loop ()
        | Some json -> return json)
  in
  loop ()

let read_json ~origin ?timeout ws =
  match timeout with
  | None -> read_json ~origin ws
  | Some timeout ->
      Lwt.pick
        [
          read_json ~origin ws;
          (let* () = Lwt_unix.sleep timeout in
           raise (Timeout {timeout; origin}));
        ]

let close ws =
  let* () = Lwt_io.close ws.stdin in
  Process.terminate ws.process ;
  let* _ = Process.wait ws.process in
  unit

let send =
  let cpt = ref 0 in
  fun ws json ->
    incr cpt ;
    let msg = JSON.unannotate json |> Ezjsonm.value_to_string ~minify:true in
    Log.debug
      ~color:Log.Color.bold
      "%s(%d): > %s"
      (Process.name ws.process)
      !cpt
      msg ;
    send_raw ws msg

let recv =
  let cpt = ref 0 in
  fun ?timeout ws ->
    incr cpt ;
    let origin = Format.sprintf "%s(%d)" (Process.name ws.process) !cpt in
    read_json ~origin ?timeout ws

let send_recv ?timeout ws json =
  let* () = send ws json in
  recv ?timeout ws

let pause ws = Unix.kill (Process.pid ws.process) Sys.sigstop

let resume ws = Unix.kill (Process.pid ws.process) Sys.sigcont
