(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 G.B. Fefe  <gb.fefe@protonmail.com>                    *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

let spawn_activate_protocol ?endpoint ?(fitness = 1)
    ?(key = Constant.activator.alias) ?timestamp
    ?(timestamp_delay = 3600. *. 24. *. 365.) ~parameter_file client =
  let timestamp =
    match timestamp with
    | Some timestamp -> timestamp
    | None ->
        let tm = Unix.gmtime (Unix.time () -. timestamp_delay) in
        Printf.sprintf
          "%04d-%02d-%02dT%02d:%02d:%02dZ"
          (tm.tm_year + 1900)
          (tm.tm_mon + 1)
          tm.tm_mday
          tm.tm_hour
          tm.tm_min
          tm.tm_sec
  in
  Client.spawn_command
    ?endpoint
    client
    [
      "activate";
      "protocol";
      Protocol.demo_counter_hash;
      "with";
      "fitness";
      string_of_int fitness;
      "and";
      "key";
      key;
      "and";
      "parameters";
      parameter_file;
      "--timestamp";
      timestamp;
    ]

let activate ?endpoint ?fitness ?key ?timestamp ?timestamp_delay client =
  Lwt_io.with_temp_file (fun (parameter_file, ch) ->
      let* () = Lwt_io.write_line ch "{}" in
      spawn_activate_protocol
        ?endpoint
        ?fitness
        ?key
        ?timestamp
        ?timestamp_delay
        ~parameter_file
        client
      |> Process.check)

let spawn_demo_bake ?(msg = "hello world") client =
  Client.spawn_command client ["bake"; msg]

let bake ?msg client = spawn_demo_bake ?msg client |> Process.check

let spawn_get client name = Client.spawn_command client ["get"; name]

let get client name =
  let extract_key (client_output : string) : int =
    let value =
      client_output =~* rex "The counter value is ?(\\w*)" |> mandatory "value"
    in
    match int_of_string_opt value with
    | Some i -> i
    | None -> failwith "Unexpected counter value."
  in
  let* output = spawn_get client name |> Process.check_and_read_stdout in
  return @@ extract_key output

let get_a client = get client "a"

let get_b client = get client "b"

let spawn_increment client name =
  Client.spawn_command client ["increment"; name]

let increment client name = spawn_increment client name |> Process.check

let increment_a client = increment client "a"

let increment_b client = increment client "b"

let spawn_transfer client amount =
  Client.spawn_command client ["transfer"; string_of_int amount]

let transfer client amount = spawn_transfer client amount |> Process.check
