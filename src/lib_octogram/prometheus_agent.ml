(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let greetings = "Ready to receive new metrics sources."

type input = Quit | Record of Prometheus_server.metrics_source

let input_encoding =
  let c = Helpers.make_mk_case () in
  Data_encoding.(
    union
      [
        c.mk_case
          "quit"
          (obj1 (req "quit" null))
          (function Quit -> Some () | _ -> None)
          (fun () -> Quit);
        c.mk_case
          "record"
          (obj1 (req "record" Prometheus_server.metrics_source_encoding))
          (function Record source -> Some source | _ -> None)
          (fun source -> Record source);
      ])

let run ~input =
  let prometheus = Prometheus_server.create () in
  let* () = Prometheus_server.run ~kill_running_instances:true prometheus in
  let* () = Prometheus_server.wait_for_ready prometheus in

  Log.info "%s" greetings ;

  let rec run () =
    Log.info "waiting for input" ;
    let* source_str = Lwt_io.read_line input in
    Log.info "[prometheus] %s" source_str ;
    let input = Helpers.of_json_string input_encoding source_str in
    match input with
    | Record source ->
        let* () = Prometheus_server.record_metrics_source prometheus source in
        run ()
    | Quit -> unit
  in
  let* () = run () in
  unit
