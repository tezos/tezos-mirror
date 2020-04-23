(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Error_monad

let () = Lwt_unix.set_default_async_method Async_none [@@ocaml.warning "-3"]

let section = Lwt_log.Section.make "process"

let log_f ~level format =
  if level < Lwt_log.Section.level section then
    Format.ikfprintf (fun _ -> Lwt.return_unit) Format.std_formatter format
  else Format.kasprintf (fun msg -> Lwt_log.log ~section ~level msg) format

let _lwt_debug fmt = log_f ~level:Lwt_log.Debug fmt

let lwt_log_notice fmt = log_f ~level:Lwt_log.Notice fmt

let lwt_log_info fmt = log_f ~level:Lwt_log.Info fmt

let lwt_log_error fmt = log_f ~level:Lwt_log.Error fmt

exception Exited of int

exception Signaled of int

exception Stopped of int

let dummy_encoding flags : 'a Data_encoding.encoding =
  let flags = Option.value ~default:[] flags in
  Data_encoding.conv
    (fun a -> Marshal.to_bytes a flags)
    (fun buf -> Marshal.from_bytes buf 0)
    Data_encoding.bytes

let write ~value_encoding ~flags outch v =
  let value_encoding =
    Option.value ~default:(dummy_encoding flags) value_encoding
  in
  protect
    (fun () ->
      ( match Data_encoding.Binary.to_bytes value_encoding v with
      | Ok encoded_v ->
          Lwt_io.write_value outch encoded_v
      | Error err ->
          Stdlib.failwith
          @@ Format.asprintf
               "Value encoding failed %a"
               Data_encoding.Binary.pp_write_error
               err )
      >>= return)
    ~on_error:(fun err ->
      Lwt.return @@ Error (Exn (Failure ("write error " ^ __LOC__)) :: err))

let read ~value_encoding ~flags inch =
  let value_encoding =
    Option.value ~default:(dummy_encoding flags) value_encoding
  in
  protect
    (fun () ->
      Lwt_io.read_value inch
      >>= fun encoded_v ->
      ( match Data_encoding.Binary.of_bytes value_encoding encoded_v with
      | Ok decoded_v ->
          Lwt.return decoded_v
      | Error err ->
          Stdlib.failwith
          @@ Format.asprintf
               "Value encoding failed %a"
               Data_encoding.Binary.pp_read_error
               err )
      >>= return)
    ~on_error:(fun err ->
      Lwt.return @@ Error (Exn (Failure ("read error " ^ __LOC__)) :: err))

let received_result ~value_encoding ~flags child_exit =
  let value_encoding =
    Option.some @@ Error_monad.result_encoding
    @@ Option.value ~default:(dummy_encoding flags) value_encoding
  in
  read ~value_encoding ~flags child_exit
  >>= function
  | Ok (Ok _ as res) | (Error _ as res) | Ok (Error _ as res) -> Lwt.return res

let send_result ~value_encoding ~flags child_exit result =
  let value_encoding =
    Option.some @@ Error_monad.result_encoding
    @@ Option.value ~default:(dummy_encoding flags) value_encoding
  in
  write ~value_encoding ~flags child_exit result

let handle_result ~value_encoding ~flags canceler f child_exit =
  protect
    ~canceler
    (fun () ->
      f ()
      >>=? fun v ->
      send_result ~value_encoding ~flags child_exit (Ok v)
      >>=? fun () -> return 0)
    ~on_error:(fun err ->
      lwt_log_error
        "@[<v 2>Detached process ended with error.@[%a@]@]@."
        pp_print_error
        err
      >>= fun () ->
      send_result ~value_encoding ~flags child_exit (Error err)
      >>=? fun () -> return 0)
  >>= function
  | Ok exit_code ->
      Lwt.return exit_code
  | Error err ->
      lwt_log_error
        "@[<v 2>Unexpected error when handling detached function result: \
         @[%a@]@]@."
        Error_monad.pp_print_error
        err
      >>= fun () -> Lwt.return 255

module Channel = struct
  type ('a, 'b) t = {
    inch : Lwt_io.input_channel;
    outch : Lwt_io.output_channel;
    input_encoding : 'b Data_encoding.encoding option;
    output_encoding : 'a Data_encoding.encoding option;
    flags : Marshal.extern_flags list option;
  }

  (** Creating the endpoint from input and output channel.
     If no encoding is given, the values will be serialized using hte
     Marshal module, with the given flags (if any is provided)
   *)
  let make ?input_encoding ?output_encoding ?flags inch outch =
    {inch; outch; input_encoding; output_encoding; flags}

  let push {outch; output_encoding; flags; _} v =
    Lwt.catch
      (fun () ->
        let value_encoding = output_encoding in
        write ~value_encoding ~flags outch v)
      (fun exn -> Lwt.return_error [Exn exn])

  let pop {inch; input_encoding; flags; _} =
    Lwt.catch
      (fun () ->
        let value_encoding = input_encoding in
        read ~value_encoding ~flags inch)
      (fun exn -> Lwt.return_error [Exn exn])
end

let terminate pid =
  (try Unix.kill pid Sys.sigkill with _ -> ()) ;
  ignore (Lwt_unix.waitpid [] pid)

let wait ~value_encoding ~flags pid result_ch =
  Lwt.catch
    (fun () ->
      Lwt_unix.waitpid [] pid
      >>= function
      | (_, Lwt_unix.WEXITED 0) ->
          received_result ~value_encoding ~flags result_ch
      | (_, Lwt_unix.WEXITED n) ->
          Lwt.return (error (Exn (Exited n)))
      | (_, Lwt_unix.WSIGNALED n) ->
          Lwt.return (error (Exn (Signaled n)))
      | (_, Lwt_unix.WSTOPPED n) ->
          Lwt.return (error (Exn (Stopped n))))
    (function
      | Lwt.Canceled ->
          terminate pid ; Error_monad.fail Canceled
      | exn ->
          Error_monad.fail (Exn exn))

type ('a, 'b, 'c) t = {
  termination : 'c tzresult Lwt.t;
  channel : ('b, 'a) Channel.t;
  prefix : string;
  input_encoding : 'b Data_encoding.encoding option;
  output_encoding : 'a Data_encoding.encoding option;
  value_encoding : 'c Data_encoding.encoding option;
}

let template = "$(date) - $(section): $(message)"

let detach ?(prefix = "") ?canceler ?input_encoding ?output_encoding
    ?value_encoding ?flags
    (f : ('sent, 'received) Channel.t -> 'result tzresult Lwt.t) :
    ('sent, 'received, 'result) t tzresult Lwt.t =
  let canceler = Option.value ~default:(Lwt_canceler.create ()) canceler in
  Lwt_io.flush_all ()
  >>= fun () ->
  protect
    ~canceler
    (fun () ->
      let (main_in, child_out) = Lwt_io.pipe () in
      let (child_in, main_out) = Lwt_io.pipe () in
      let (main_result, child_exit) = Lwt_io.pipe () in
      match Lwt_unix.fork () with
      | 0 ->
          Lwt_log.default :=
            Lwt_log.channel
              ~template
              ~close_mode:`Keep
              ~channel:Lwt_io.stderr
              () ;
          Random.self_init () ;
          Lwt_main.run
            (let template = Format.asprintf "%s$(message)" prefix in
             Lwt_io.close main_in
             >>= fun () ->
             Lwt_io.close main_out
             >>= fun () ->
             Lwt_io.close main_result
             >>= fun () ->
             Lwt_log.default :=
               Lwt_log.channel
                 ~template
                 ~close_mode:`Keep
                 ~channel:Lwt_io.stderr
                 () ;
             lwt_log_notice "PID: %d" (Unix.getpid ())
             >>= fun () ->
             handle_result
               ~value_encoding
               ~flags
               canceler
               (fun () ->
                 let chans =
                   Channel.make
                     ?input_encoding
                     ?output_encoding
                     child_in
                     child_out
                 in
                 f chans)
               child_exit)
          |> exit
      | pid ->
          Lwt_canceler.on_cancel canceler (fun () ->
              terminate pid ; Lwt.return_unit) ;
          let termination = wait ~value_encoding ~flags pid main_result in
          Lwt_io.close child_in
          >>= fun () ->
          Lwt_io.close child_out
          >>= fun () ->
          Lwt_io.close child_exit
          >>= fun () ->
          return
            {
              termination;
              channel =
                Channel.make
                  ?input_encoding:output_encoding
                  ?output_encoding:input_encoding
                  main_in
                  main_out;
              prefix;
              input_encoding;
              output_encoding;
              value_encoding;
            })
    ~on_error:(fun err ->
      Lwt_canceler.cancel canceler >>= fun _ -> Lwt.return (Error err))

let signal_name =
  let names =
    [ (Sys.sigabrt, "ABRT");
      (Sys.sigalrm, "ALRM");
      (Sys.sigfpe, "FPE");
      (Sys.sighup, "HUP");
      (Sys.sigill, "ILL");
      (Sys.sigint, "INT");
      (Sys.sigkill, "KILL");
      (Sys.sigpipe, "PIPE");
      (Sys.sigquit, "QUIT");
      (Sys.sigsegv, "SEGV");
      (Sys.sigterm, "TERM");
      (Sys.sigusr1, "USR1");
      (Sys.sigusr2, "USR2");
      (Sys.sigchld, "CHLD");
      (Sys.sigcont, "CONT");
      (Sys.sigstop, "STOP");
      (Sys.sigtstp, "TSTP");
      (Sys.sigttin, "TTIN");
      (Sys.sigttou, "TTOU");
      (Sys.sigvtalrm, "VTALRM");
      (Sys.sigprof, "PROF");
      (Sys.sigbus, "BUS");
      (Sys.sigpoll, "POLL");
      (Sys.sigsys, "SYS");
      (Sys.sigtrap, "TRAP");
      (Sys.sigurg, "URG");
      (Sys.sigxcpu, "XCPU");
      (Sys.sigxfsz, "XFSZ") ]
  in
  fun n -> List.assoc n names

let print_errors plist =
  Lwt_list.partition_p
    (fun (_i, _prefix, p) ->
      match p with Ok _ -> Lwt.return_true | _ -> Lwt.return_false)
    plist
  >>= fun (ok_list, errlist) ->
  lwt_log_error
    "@[Processes @[%a@] finished successfully.@]"
    (fun ppf ->
      List.iter (fun (i, pref, _) ->
          Format.fprintf ppf "%d(%s)" i (String.trim pref)))
    ok_list
  >>= fun () ->
  let (exnlist, errlist) =
    List.partition
      (fun (_i, _prefix, p) ->
        match p with Error [Exn _] -> true | _ -> false)
      errlist
  in
  Lwt_list.iter_s
    (fun (i, prefix, p) ->
      let prefix = String.trim prefix in
      match p with
      | Ok _ ->
          Lwt.return_unit
      | Error [Exn (Exited n)] ->
          lwt_log_error
            "@[Process %d (%s) finished with exit code %d@]"
            i
            prefix
            n
      | Error [Exn (Signaled n)] ->
          lwt_log_error
            "@[Process %d (%s) was killed by a SIG%s !@]"
            i
            prefix
            (signal_name n)
      | Error [Exn (Stopped n)] ->
          lwt_log_error
            "@[Process %d (%s) was stopped by a SIG%s !@]"
            i
            prefix
            (signal_name n)
      | Error err ->
          lwt_log_error
            "@[<v 2>Process %d (%s) failed with error:@ @[ %a @]@]"
            i
            prefix
            pp_print_error
            err)
    exnlist
  >>= fun () ->
  let (canceled_list, errlist) =
    List.partition
      (fun (_i, _prefix, p) ->
        match p with
        | Error [(Canceled | Exn Lwt.Canceled)] ->
            true
        | _ ->
            false)
      errlist
  in
  ( match canceled_list with
  | [] ->
      Lwt.return_unit
  | _ ->
      lwt_log_error
        "@[<v 2> Following processes have been canceled @[%a@].@]"
        (fun ppf ->
          List.iter (fun (i, pref, _) ->
              Format.fprintf ppf "@ %d(%s)" i (String.trim pref)))
        canceled_list )
  >>= fun () ->
  Lwt_list.iter_s
    (fun (i, prefix, p) ->
      let prefix = String.trim prefix in
      match p with
      | Ok _ ->
          Lwt.return_unit
      | Error err ->
          lwt_log_error
            "@[<v 2>Process %d (%s) failed with error:@ @[ %a @]@]"
            i
            prefix
            pp_print_error
            err)
    errlist

let send {channel; _} v = Channel.push channel v

let receive {channel; _} = Channel.pop channel

let wait_result {termination; _} : 'result tzresult Lwt.t = termination

type error += Par of (int * string * error) list

let () =
  register_recursive_error_kind
    `Temporary
    ~id:"parallel_errors"
    ~title:"Parallel errors"
    ~description:
      "An error occured in at least one thread of a paralle\n  execution."
    ~pp:(fun ppf s ->
      Format.fprintf
        ppf
        "@[<v 2>%a@]@."
        (Format.pp_print_list (fun ppf (i, prefix, err) ->
             let prefix = String.trim prefix in
             Format.fprintf
               ppf
               "@[<v 2>at process %d (%s),@ @[%a@]@]"
               i
               prefix
               pp_print_error
               [err]))
        s)
    Data_encoding.(
      fun error_encoding ->
        obj1
          (req
             "error_list"
             (list
                (obj3
                   (req "processor" int16)
                   (req "prefix" string)
                   (req "error" error_encoding)))))
    (function Par lst -> Some lst | _ -> None)
    (fun lst -> Par lst)

let join (plist : 'a Lwt.t list) =
  Lwt_list.map_s (fun (p : 'a Lwt.t) -> p) plist

let join_process (plist : ('a, 'b, 'c) t list) =
  Lwt_list.map_p
    (fun {termination; prefix; _} ->
      termination >>= fun t -> Lwt.return (prefix, t))
    plist

(** Wait for all processes to terminate.

    If a node terminates with an error, all remaining processes
    are canceled and an exception is raised *)
let wait_all_results (processes : ('a, 'b, 'c) t list) =
  let rec loop processes =
    match processes with
    | [] ->
        Lwt.return_none
    | processes -> (
        Lwt.nchoose_split processes
        >>= function
        | (finished, remaining) ->
            let rec handle = function
              | [] ->
                  loop remaining
              | Ok _ :: finished ->
                  handle finished
              | Error err :: _ ->
                  Lwt.return_some
                    ( err,
                      List.map
                        (fun remain -> remain >>= fun _ -> return_unit)
                        remaining )
            in
            handle finished )
  in
  let terminations = List.map (fun p -> p.termination) processes in
  loop terminations
  >>= function
  | None ->
      lwt_log_info "All done!"
      >>= fun () ->
      join terminations
      >>= fun terminated ->
      return
      @@ List.map (function Ok a -> a | Error _ -> assert false) terminated
  | Some (_err, remaining) ->
      lwt_log_error "Early error! Canceling remaining process."
      >>= fun () ->
      List.iter Lwt.cancel remaining ;
      join_process processes
      >>= fun terminated ->
      let terminated =
        List.mapi (fun i (prefix, a) -> (i, prefix, a)) terminated
      in
      let errors =
        List.filter_map
          (function
            | (_, _, Ok _) ->
                None
            | (i, prefix, Error (err :: _)) ->
                Some (i, prefix, err)
            | (i, prefix, Error []) ->
                Some
                  ( i,
                    prefix,
                    Exn
                      (Invalid_argument "process returned an empty error trace")
                  ))
          terminated
      in
      print_errors terminated >>= fun _ -> Lwt.return @@ error (Par errors)

let wait_all pl =
  wait_all_results pl
  >>= function
  | Ok _ -> return_unit | Error err -> failwith "%a" pp_print_error err
