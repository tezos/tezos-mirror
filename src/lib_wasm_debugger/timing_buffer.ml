(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>               *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  elapsed : float ref;  (** Elapsed time at the start of the current line *)
  base : float;  (** Time at the start *)
  buffer : Buffer.t;  (** Current line being buffered *)
}

let create () =
  {elapsed = ref 0.0; base = Unix.gettimeofday (); buffer = Buffer.create 1024}

let line_encoding =
  let open Data_encoding in
  let elapsed_encoding =
    conv
      (fun timestamp ->
        (* The timestamp is in seconds, but we need to split it into seconds and nanoseconds. *)
        let secs = Float.floor timestamp in
        let nanos = (timestamp -. secs) *. 1_000_000_000.0 |> Float.round in
        (secs, nanos))
      (fun (secs, nanos) -> secs +. (nanos /. 1_000_000_000.0))
      (obj2 (req "secs" float) (req "nanos" float))
  in
  obj2 (req "elapsed" elapsed_encoding) (req "message" string)

let flush_buffer console out =
  (* Add a line of JSON representing the timed message to the output channel. *)
  let add_json elapsed line =
    let json =
      let open Data_encoding in
      Json.construct line_encoding (elapsed, line)
      |> Json.to_string ~newline:false ~minify:true
    in
    Printf.fprintf out "%s\n%!" json
  in
  (* There is no point in flushing anything if the message is going to be empty. *)
  if Buffer.length console.buffer > 0 then (
    let line = Buffer.contents console.buffer in
    let elapsed = !(console.elapsed) in
    add_json elapsed line ;
    (* The buffer must be emptied to avoid sending the same message prefix multiple times. *)
    Buffer.reset console.buffer)

let check_elapsed_time console =
  (* If we're starting a new buffered line, we need to track when it began. *)
  if Buffer.length console.buffer = 0 then
    console.elapsed := Unix.gettimeofday () -. console.base

let rec add_lines console out lines =
  check_elapsed_time console ;
  match lines with
  | [] -> ()
  | [message] ->
      (* There are no line endings in the message, so we don't flush. *)
      Buffer.add_string console.buffer message
  | first :: remaining_lines ->
      Buffer.add_string console.buffer first ;
      (* There is a line ending after `first`, so we flush it. *)
      flush_buffer console out ;
      add_lines console out remaining_lines

let add_message console out line =
  add_lines console out (String.split_on_char '\n' line)
