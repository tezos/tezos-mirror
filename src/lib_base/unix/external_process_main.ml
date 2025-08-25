(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

module type EXTERNAL_PROCESSING = sig
  type parameters

  type 'response request

  type state

  val initial_state : parameters -> state tzresult Lwt.t

  val handle_request :
    parameters ->
    state ->
    'response request ->
    [ `Continue of
      ('response * (Profiler.report option * Profiler.report option) option)
      tzresult
      * state
    | `Stop ]
    Lwt.t
end

module Make
    (Params : External_process_parameters.S)
    (Processing :
      EXTERNAL_PROCESSING
        with type parameters := Params.parameters
         and type 'response request := 'response Params.request) =
struct
  type error += Inconsistent_handshake of string

  let () =
    Error_monad.register_error_kind
      `Temporary
      ~id:(Params.name ^ ".inconsistent_handshake")
      ~title:"Inconsistent handshake"
      ~description:
        (Format.sprintf "Inconsistent handshake with %s." Params.name)
      ~pp:(fun ppf msg ->
        Format.fprintf ppf "Inconsistent handshake with %s: %s." Params.name msg)
      Data_encoding.(obj1 (req "msg" string))
      (function Inconsistent_handshake msg -> Some msg | _ -> None)
      (fun msg -> Inconsistent_handshake msg)

  module Events = struct
    open Internal_event.Simple

    let section = ["external_" ^ Params.name]

    let declare_0 ~level ~name ~msg () =
      declare_0
        ~section
        ~level
        ~name:(String.concat "_" [Params.name; name])
        ~msg
        ()

    let declare_1 ~level ~name ~msg ~pp1 =
      declare_1
        ~section
        ~level
        ~name:(String.concat "_" [Params.name; name])
        ~msg
        ~pp1

    let initialized =
      declare_1
        ~level:Info
        ~name:"initialized"
        ~msg:(Params.name ^ " initialized and listening with pid {pid}")
        ~pp1:Format.pp_print_int
        ("pid", Data_encoding.int31)

    let terminated =
      declare_0
        ~level:Info
        ~name:"terminated_request"
        ~msg:(Params.name ^ " terminated")
        ()

    let request =
      declare_1
        ~level:Info
        ~name:"request"
        ~msg:"Received request {request}"
        ~pp1:(fun fmt (Params.Erequest r) -> Params.request_pp fmt r)
        ("request", Params.request_encoding)

    let emit = Internal_event.Simple.emit
  end

  (* Handshake with the external process. See
     [External_process.process_handshake] for the handshake
     scenario. *)
  let handshake input output =
    let open Lwt_syntax in
    let* () =
      Lwt_unix_socket.send output Data_encoding.Variable.bytes Params.magic
    in
    let* magic = Lwt_unix_socket.recv input Data_encoding.Variable.bytes in
    fail_when
      (not (Bytes.equal magic Params.magic))
      (Inconsistent_handshake "bad magic")

  (* Initialization of the external process thanks to the parameters
     sent by the caller. This is expected to be run after the
     [handshake]. See [External_process.process_init] for
     the init scenario. *)
  let init input output =
    let open Lwt_result_syntax in
    let*! parameters = Lwt_unix_socket.recv input Params.parameters_encoding in
    let* state = Processing.initial_state parameters in
    (* It is necessary to send the ok result, as a blocking promise for
       the caller (see [External_process.process_init]), after a
       complete initialization. *)
    let*! () =
      Lwt_unix_socket.send
        output
        (Error_monad.result_encoding Data_encoding.empty)
        (Ok ())
    in
    return (parameters, state)

  let run input output =
    let open Lwt_result_syntax in
    let* () = handshake input output in
    let* parameters, state = init input output in
    let*! () =
      Internal_event_unix.init ~config:(Params.internal_events parameters) ()
    in
    let*! () = Events.(emit initialized (Unix.getpid ())) in
    let rec loop state =
      let*! (Params.Erequest recved) =
        Lwt_unix_socket.recv input Params.request_encoding
      in
      let*! () = Events.(emit request) (Params.Erequest recved) in
      let*! res = Processing.handle_request parameters state recved in
      match res with
      | `Continue (res, state) ->
          let*! () =
            Lwt_unix_socket.send
              output
              (Error_monad.result_encoding
                 Data_encoding.(
                   tup2
                     (Params.result_encoding recved)
                     (option
                        (tup2
                           (option Profiler.report_encoding)
                           (option Profiler.report_encoding)))))
              res
          in
          loop state
      | `Stop -> return_unit
    in
    loop state

  let main ~socket_dir =
    let open Lwt_result_syntax in
    let canceler = Lwt_canceler.create () in
    let* in_channel, out_channel =
      let pid = Unix.getpid () in
      let socket_path = Params.socket_path ~socket_dir ~pid in
      let* socket_process =
        Lwt_unix_socket.create_socket_connect ~canceler ~socket_path
      in
      let socket_in = Lwt_io.of_fd ~mode:Input socket_process in
      let socket_out = Lwt_io.of_fd ~mode:Output socket_process in
      return (socket_in, socket_out)
    in
    let*! r =
      Error_monad.catch_es (fun () ->
          let* () = run in_channel out_channel in
          let*! r = Lwt_canceler.cancel canceler in
          match r with
          | Ok () | Error [] -> return_unit
          | Error (exc :: excs) ->
              let texc = TzTrace.make (Error_monad.Exn exc) in
              let texcs =
                List.map (fun exc -> TzTrace.make (Error_monad.Exn exc)) excs
              in
              let t = TzTrace.conp_list texc texcs in
              Lwt.return (Error t))
    in
    match r with
    | Ok () ->
        let*! () = Events.(emit terminated ()) in
        return_unit
    | Error _ as errs ->
        let*! () =
          Lwt_unix_socket.send
            out_channel
            (Error_monad.result_encoding Data_encoding.unit)
            errs
        in
        Lwt.return errs
end
