(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 TQ Tezos <contact@tqtezos.com>                         *)
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

open Base

let change_logging_configuration =
  Protocol.register_test
    ~__FILE__
    ~title:"logging configuration RPCs"
    ~tags:["rpc"; "node"; "logging"]
  @@ fun protocol ->
  let* main_node = Node.init ~name:"main_node" [] in
  let* client = Client.init ~endpoint:(Node main_node) () in
  let* () =
    Client.activate_protocol
      ~protocol
      ~timestamp:(Ago (Ptime.Span.of_int_s 1_000_000))
      client
  in
  let* () = repeat 2 (fun () -> Client.bake_for_and_wait client) in
  let should_fail msg f =
    let* actually_failed =
      Lwt.catch
        (fun () ->
          let* _ = f () in
          return false)
        (fun _ -> return true)
    in
    if actually_failed then return () else Test.fail "Expecting failure: %s" msg
  in
  let call_config data =
    let* _ = Client.rpc ~data Client.PUT ["config"; "logging"] client in
    Lwt.return_unit
  in
  let* () =
    should_fail "wrong-json" (fun () ->
        call_config (Data Ezjsonm.(dict [("wrong", `Null)])))
  in
  (* Empty just works: *)
  let* () = call_config (Data Ezjsonm.(dict [])) in
  let call_config_activate ?(use_deprecated_key = false) l =
    let key_name = if use_deprecated_key then "activate" else "active_sinks" in
    call_config (Data Ezjsonm.(dict [(key_name, strings l)]))
  in
  let tmp0 = Temp.file "tezt-rpc-logging0.log" in
  let tmp1 = Temp.file "tezt-rpc-logging1.log" in
  let* () =
    call_config_activate
      ~use_deprecated_key:true
      [sf "file-descriptor-path://%s?level-at-least=debug" tmp0]
  in
  (* Let's make some noise: *)
  (* Can't use [bake_for_and_wait] from now on because it relies on
     events on stdout. *)
  let* () = Client.bake_for client in
  let tmp0_content = read_file tmp0 in
  if String.length tmp0_content < 100 then
    Test.fail "File %s should have more data" tmp0 ;
  (* We reopen the same one plus another one with the same configuration: *)
  let* () =
    call_config_activate
      [
        sf "file-descriptor-path://%s?level-at-least=debug" tmp1;
        sf "file-descriptor-path://%s?level-at-least=debug" tmp0;
      ]
  in
  (* More noise *)
  let* () = Client.bake_for client in
  let tmp1_content_1 = read_file tmp1 in
  let tmp0_content_2 = read_file tmp0 in
  let lines s = String.split_on_char '\n' s in
  if lines tmp0_content_2 > lines tmp1_content_1 then
    Test.fail "%s is not smaller than %s" tmp1 tmp0 ;
  let* () =
    (* Now just the second one *)
    call_config_activate
      [sf "file-descriptor-path://%s?level-at-least=debug" tmp1]
  in
  let tmp0_content_3 = read_file tmp0 in
  let* () =
    (* More noise *)
    Client.bake_for client
  in
  let tmp0_content_4 = read_file tmp0 in
  if tmp0_content_3 <> tmp0_content_4 then
    Test.fail "Sink %s has not stopped growing." tmp0 ;
  (* We now configure the sink to output only consume events in the `rpc`
     and `validator` top-level sections, and then we check that only those events
     can be found in the resulting file: *)
  let* () =
    call_config_activate
      [
        sf
          "file-descriptor-path://%s?section-prefix=rpc:debug&section-prefix=validator:debug&section-prefix=:none&fresh=true"
          tmp1;
      ]
  in
  (* More noise *)
  let* () = Client.bake_for client in
  let tmp1_content = read_file tmp1 in
  (* Let's check they are all from the RPC or validator sections: *)
  let () =
    let check_line ith line =
      let origin = sf "%s:%d" tmp1 ith in
      JSON.(
        let json = parse ~origin line in
        match
          List.map
            as_string
            (json |-> "fd-sink-item.v0" |-> "section" |> as_list)
        with
        | "rpc" :: _ | "validator" :: _ -> ()
        | _ -> Test.fail "Event not in 'rpc' section: %S" line)
    in
    List.iteri
      check_line
      (String.split_on_char '\n' tmp1_content |> List.filter (( <> ) ""))
  in
  let* () =
    (* We check here that the argument `with-pid` forces the creation of
       2 files, ones for the main process, and one for the external
       validator: *)
    let tmpdir = Temp.dir "tezt-rpc-logging-pid" in
    let interesting_prefix = "log-file-prefix" in
    let* () =
      call_config_activate
        [
          sf
            "file-descriptor-path://%s/%s.log?section-prefix=:debug&with-pid=true&fresh=true"
            tmpdir
            interesting_prefix;
        ]
    in
    (* More noise *)
    let* () = Client.bake_for client in
    let* files = Lwt_unix.files_of_directory tmpdir |> Lwt_stream.to_list in
    let should_be_two =
      List.fold_left
        (fun count f ->
          match String.sub f 0 (String.length interesting_prefix) with
          | some when some = interesting_prefix -> count + 1
          | _ | (exception _) -> count)
        0
        files
    in
    if should_be_two = 2 then ()
    else Test.fail "with-pid created %d files, not 2" should_be_two ;
    Lwt.return_unit
  in
  Lwt.return_unit

let register ~protocols = change_logging_configuration protocols
