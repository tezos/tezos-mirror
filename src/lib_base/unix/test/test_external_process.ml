(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Base, Unix
   Invocation:   dune exec src/lib_base/unix/test/main.exe -- --file test_external_process.ml
   Subject:      Check behavior of external process
*)

open TzPervasives

module Parameters = struct
  type parameters = unit

  let name = "test"

  type _ request =
    | Echo : {x : int; sleep : float} -> int request
    | Reconfigure_event_logging :
        Tezos_base_unix.Internal_event_unix.Configuration.t
        -> unit request
    | Shutdown : Empty.t request

  type packed_request = Erequest : _ request -> packed_request

  let request_pp : type a. Format.formatter -> a request -> unit =
   fun ppf -> function
    | Echo {x; sleep} -> Format.fprintf ppf "echo %d (sleep %f)" x sleep
    | Reconfigure_event_logging _ ->
        Format.fprintf ppf "reconfigure event logging"
    | Shutdown -> Format.fprintf ppf "shutdown"

  let parameters_encoding = Data_encoding.unit

  let request_encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"reconfigure_event_logging"
          Tezos_base_unix.Internal_event_unix.Configuration.encoding
          (function
            | Erequest (Reconfigure_event_logging c) -> Some c | _ -> None)
          (fun c -> Erequest (Reconfigure_event_logging c));
        case
          ~title:"shutdown"
          (Tag 1)
          (constant "shutdown")
          (function Erequest Shutdown -> Some () | _ -> None)
          (fun () -> Erequest Shutdown);
        case
          ~title:"echo"
          (Tag 2)
          (obj2 (req "echo" int31) (req "sleep" float))
          (function Erequest (Echo {x; sleep}) -> Some (x, sleep) | _ -> None)
          (fun (x, sleep) -> Erequest (Echo {x; sleep}));
      ]

  let result_encoding : type a. a request -> a Data_encoding.t = function
    | Echo _ -> Data_encoding.int31
    | Reconfigure_event_logging _ -> Data_encoding.unit
    | Shutdown -> assert false

  let magic = Bytes.of_string "TEST_EXTERNAL_PROCESS"

  let socket_path_prefix = "test-external-process-"

  let socket_path ~socket_dir ~pid =
    let filename = Format.sprintf "%s%d" socket_path_prefix pid in
    Filename.concat socket_dir filename

  let internal_events () =
    let verbosity =
      match Cli.Logs.level with
      | Quiet | Error | Warn -> Internal_event.Fatal
      | Report | Info -> Notice
      | Debug -> Debug
    in
    Internal_event_unix.make_with_defaults ~verbosity ()

  let reconfigure_event_logging_request config =
    Reconfigure_event_logging config

  let terminate_request = Erequest Shutdown

  let external_process_name = "interpreter"

  let command_line_args ~socket_dir =
    (external_process_name, ["--socket-dir"; socket_dir])

  let hypervisor_name = external_process_name ^ "-hypervisor"

  let share_sink = true
end

module Processing = struct
  type state = unit

  let initial_state _params =
    let open Lwt_result_syntax in
    return_unit

  let handle_request : type a.
      Parameters.parameters ->
      state ->
      a Parameters.request ->
      [ `Continue of
        (a
        * (Tezos_profiler.Profiler.report option
          * Tezos_profiler.Profiler.report option)
          option)
        tzresult
        * state
      | `Stop ]
      Lwt.t =
   fun _params state ->
    let open Lwt_syntax in
    let continue res =
      let res =
        match res with Error errs -> Error errs | Ok res -> Ok (res, None)
      in
      return (`Continue (res, state))
    in
    function
    | Echo {x; sleep} ->
        let* () = Lwt_unix.sleep sleep in
        continue (Ok x)
    | Shutdown ->
        let* () = Lwt_io.flush_all () in
        return `Stop
    | Reconfigure_event_logging config ->
        let* res =
          Tezos_base_unix.Internal_event_unix.Configuration.reapply config
        in
        continue res
end

module External = External_process_main.Make (Parameters) (Processing)
module Hypervisor = Hypervisor_process_main.Make (Parameters)
module Process = External_process.Make (Parameters)

module Command_line = struct
  let parse_args name =
    let socket_dir = ref None in
    let args =
      Arg.
        [
          ( "--socket-dir",
            String
              (fun s ->
                if not (Sys.file_exists s && Sys.is_directory s) then
                  raise
                    (Arg.Bad
                       (Format.sprintf "File '%s' is not a valid directory" s))
                else socket_dir := Some s),
            Format.sprintf
              {|<dir>
      When provided, the %s will communicate through a socket located
      in 'dir>. By default, the %s will
      communicate through its standard input and output.|}
              name
              name );
        ]
    in
    let usage_msg = Format.sprintf "%s [--socket-dir <dir>]" Sys.argv.(0) in
    Arg.parse
      args
      (fun s -> raise (Arg.Bad (Format.sprintf "Unexpected argument: %s" s)))
      usage_msg ;
    match !socket_dir with
    | Some s -> s
    | None ->
        raise (Arg.Bad (Format.sprintf "%s: please provide --socket-dir" name))

  let run name main =
    let socket_dir = parse_args name in
    let main_promise = main ~socket_dir in
    Stdlib.exit
      (let open Lwt_syntax in
       Lwt.Exception_filter.(set handle_all_except_runtime) ;
       Lwt_main.run
         (let* r = Lwt_exit.wrap_and_exit main_promise in
          match r with
          | Ok () -> Lwt_exit.exit_and_wait 0
          | Error err ->
              Format.eprintf "%a\n%!" pp_print_trace err ;
              Lwt_exit.exit_and_wait 1))

  let () =
    if Filename.basename Sys.argv.(0) = Parameters.external_process_name then
      run Parameters.external_process_name External.main

  let () =
    if Filename.basename Sys.argv.(0) = Parameters.hypervisor_name then
      run Parameters.hypervisor_name Hypervisor.main
end

let lift p =
  let open Lwt_syntax in
  let* r = p in
  match r with
  | Ok r -> return r
  | Error e -> Test.fail "Failed with %a" pp_print_trace e

let () =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"External process: test send receive messages"
    ~tags:["external_process"]
  @@ fun () ->
  lift
  @@
  let open Lwt_result_syntax in
  let* process = Process.init () ~process_path:Sys.executable_name in
  let* a, _ = Process.send_request process (Echo {x = 1; sleep = 0.3})
  and* b, _ = Process.send_request process (Echo {x = 2; sleep = 0.}) in
  Tezt.Check.((a = 1) int) ~error_msg:"First request answered %L instead of %R" ;
  Tezt.Check.((b = 2) int) ~error_msg:"Second request answered %L instead of %R" ;
  Log.info "Shutting down external process" ;
  let*! () = Process.close process in
  return_unit

let () =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"External process: test hypervisor restarts on kill"
    ~tags:["external_process"; "hypervisor"; "kill"]
  @@ fun () ->
  lift
  @@
  let open Lwt_result_syntax in
  let* process = Process.init () ~process_path:Sys.executable_name in
  let* a, _ = Process.send_request process (Echo {x = 1; sleep = 0.}) in
  Tezt.Check.((a = 1) int) ~error_msg:"First request answered %L instead of %R" ;
  Log.info "Killing external process" ;
  Unix.kill (Process.pid process) Sys.sigkill ;
  let* b, _ = Process.send_request process (Echo {x = 2; sleep = 0.}) in
  Tezt.Check.((b = 2) int)
    ~error_msg:"Request after kill answered %L instead of %R" ;
  Log.info "Shutting down external process" ;
  let*! () = Process.close process in
  return_unit

let () =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"External process: test hypervisor restarts"
    ~tags:["external_process"; "hypervisor"; "restart"]
  @@ fun () ->
  lift
  @@
  let open Lwt_result_syntax in
  let* process = Process.init () ~process_path:Sys.executable_name in
  let* a, _ = Process.send_request process (Echo {x = 1; sleep = 0.}) in
  Tezt.Check.((a = 1) int) ~error_msg:"First request answered %L instead of %R" ;
  let pid1 = Process.pid process in
  Log.info "Restart external process through hypervisor" ;
  let* () = Process.restart_hypervisee process in
  let pid2 = Process.pid process in
  Tezt.Check.((pid1 <> pid2) int)
    ~error_msg:"External process PID is identical after restart" ;
  let* b, _ = Process.send_request process (Echo {x = 2; sleep = 0.}) in
  Tezt.Check.((b = 2) int)
    ~error_msg:"Request after restart answered %L instead of %R" ;
  Log.info "Shutting down external process" ;
  let*! () = Process.close process in
  return_unit

let () =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"External process: test hypervisor race condition"
    ~tags:["external_process"; "hypervisor"; "race"]
  @@ fun () ->
  lift
  @@
  let open Lwt_result_syntax in
  let* process = Process.init () ~process_path:Sys.executable_name in
  let* a, _ = Process.send_request process (Echo {x = 1; sleep = 0.}) in
  Tezt.Check.((a = 1) int) ~error_msg:"First request answered %L instead of %R" ;
  Log.info
    "Restart external process through hypervisor and send message at the same \
     time" ;
  let* () =
    Lwt_unix.with_timeout 10. @@ fun () -> Process.restart_hypervisee process
  and* b, _ =
    Lwt_unix.with_timeout 10. @@ fun () ->
    Process.send_request process (Echo {x = 2; sleep = 0.})
  in
  Tezt.Check.((b = 2) int)
    ~error_msg:"Request after restart answered %L instead of %R" ;
  Log.info "Shutting down external process" ;
  let*! () = Process.close process in
  return_unit

let () =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"External process: test hypervisor race condition 2"
    ~tags:["external_process"; "hypervisor"; "race"]
  @@ fun () ->
  lift
  @@
  let open Lwt_result_syntax in
  let* process = Process.init () ~process_path:Sys.executable_name in
  let* a, _ = Process.send_request process (Echo {x = 1; sleep = 0.}) in
  Tezt.Check.((a = 1) int) ~error_msg:"First request answered %L instead of %R" ;
  Log.info "Killing external process" ;
  Unix.kill (Process.pid process) Sys.sigkill ;
  Log.info
    "Send message to dead external process and restart through hypervisor at \
     the same time" ;
  let* b, _ =
    Lwt_unix.with_timeout 10. @@ fun () ->
    Process.send_request process (Echo {x = 2; sleep = 0.})
  and* () =
    Lwt_unix.with_timeout 10. @@ fun () -> Process.restart_hypervisee process
  in
  Tezt.Check.((b = 2) int)
    ~error_msg:"Request after restart answered %L instead of %R" ;
  Log.info "Shutting down external process" ;
  let*! () = Process.close process in
  return_unit
