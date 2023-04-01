module type CustomerLogger = sig
  val msg : Logs.level -> 'a Logs_lwt.log
end

module IO_LWT (L : CustomerLogger) = struct
  type 'a t = 'a Lwt.t

  type 'a stream = 'a Lwt_stream.t

  type input_channel = Lwt_io.input_channel

  type output_channel = Lwt_io.output_channel

  type server = Lwt_io.server

  type level = Debug | Info | Warning | Error

  exception Timeout = Lwt_unix.Timeout

  let lib_name = "Distributed_lwt"

  let lib_version = "0.2.0"

  let lib_description = "A Lwt based implementation."

  let return = Lwt.return

  let ( >>= ) = Lwt.( >>= )

  let fail = Lwt.fail

  let catch = Lwt.catch

  let async = Lwt.async

  let create_stream = Lwt_stream.create

  let get = Lwt_stream.get

  let stream_append = Lwt_stream.append

  let close_input = Lwt_io.close

  let close_output = Lwt_io.close

  let read_value = Lwt_io.read_value

  let write_value = Lwt_io.write_value

  let open_connection sock_addr = Lwt_io.open_connection sock_addr

  let establish_server ?backlog sock_addr server_fn =
    Lwt_io.establish_server_with_client_address ?backlog sock_addr server_fn

  let of_logs_lwt_level = function
    | Debug -> Logs.Debug
    | Info -> Logs.Info
    | Warning -> Logs.Warning
    | Error -> Logs.Error

  let log (level : level) (msg_fmtter : unit -> string) =
    L.msg (of_logs_lwt_level level) (fun m -> m "%s" @@ msg_fmtter ())
    >>= fun _ -> return ()

  let shutdown_server = Lwt_io.shutdown_server

  let sleep = Lwt_unix.sleep

  let timeout = Lwt_unix.timeout

  let pick = Lwt.pick

  let at_exit = Lwt_main.at_exit
end

module Make (M : Distributed.Message_type) (L : CustomerLogger) :
  Distributed.Process with type 'a io = 'a Lwt.t and type message_type = M.t =
  Distributed.Make (IO_LWT (L)) (M)
