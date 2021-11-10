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
      (match Data_encoding.Binary.to_bytes value_encoding v with
      | Ok encoded_v -> Lwt_io.write_value outch encoded_v
      | Error err ->
          Stdlib.failwith
          @@ Format.asprintf
               "Value encoding failed %a"
               Data_encoding.Binary.pp_write_error
               err)
      >>= return)
    ~on_error:(fun err ->
      Lwt.return @@ Error (Exn (Failure ("write error " ^ __LOC__)) :: err))

let read ~value_encoding ~flags inch =
  let value_encoding =
    Option.value ~default:(dummy_encoding flags) value_encoding
  in
  protect
    (fun () ->
      Lwt_io.read_value inch >>= fun encoded_v ->
      (match Data_encoding.Binary.of_bytes value_encoding encoded_v with
      | Ok decoded_v -> Lwt.return decoded_v
      | Error err ->
          Stdlib.failwith
          @@ Format.asprintf
               "Value encoding failed %a"
               Data_encoding.Binary.pp_read_error
               err)
      >>= return)
    ~on_error:(fun err ->
      Lwt.return @@ Error (Exn (Failure ("read error " ^ __LOC__)) :: err))

let received_result ~value_encoding ~flags child_exit =
  let value_encoding =
    Option.some @@ Error_monad.result_encoding
    @@ Option.value ~default:(dummy_encoding flags) value_encoding
  in
  read ~value_encoding ~flags child_exit >>= function
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
      f () >>=? fun v ->
      send_result ~value_encoding ~flags child_exit (Ok v) >>=? fun () ->
      return 0)
    ~on_error:(fun err ->
      lwt_log_error
        "@[<v 2>Detached process ended with error.@[%a@]@]@."
        pp_print_trace
        err
      >>= fun () ->
      send_result ~value_encoding ~flags child_exit (Error err) >>=? fun () ->
      return 0)
  >>= function
  | Ok exit_code -> Lwt.return exit_code
  | Error err ->
      lwt_log_error
        "@[<v 2>Unexpected error when handling detached function result: \
         @[%a@]@]@."
        Error_monad.pp_print_trace
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
      Lwt_unix.waitpid [] pid >>= function
      | (_, Lwt_unix.WEXITED 0) ->
          received_result ~value_encoding ~flags result_ch
      | (_, Lwt_unix.WEXITED n) -> Lwt.return (error (Exn (Exited n)))
      | (_, Lwt_unix.WSIGNALED n) -> Lwt.return (error (Exn (Signaled n)))
      | (_, Lwt_unix.WSTOPPED n) -> Lwt.return (error (Exn (Stopped n))))
    (function
      | Lwt.Canceled ->
          terminate pid ;
          Error_monad.fail Canceled
      | exn -> Error_monad.fail (Exn exn))

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
  Lwt_io.flush_all () >>= fun () ->
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
          (* Lwt_main.run *)
          (let template = Format.asprintf "%s$(message)" prefix in
           Lwt_io.close main_in >>= fun () ->
           Lwt_io.close main_out >>= fun () ->
           Lwt_io.close main_result >>= fun () ->
           Lwt_log.default :=
             Lwt_log.channel
               ~template
               ~close_mode:`Keep
               ~channel:Lwt_io.stderr
               () ;
           lwt_log_notice "PID: %d" (Unix.getpid ()) >>= fun () ->
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
          >>= exit
      | pid ->
          Lwt_canceler.on_cancel canceler (fun () ->
              terminate pid ;
              Lwt.return_unit) ;
          let termination = wait ~value_encoding ~flags pid main_result in
          Lwt_io.close child_in >>= fun () ->
          Lwt_io.close child_out >>= fun () ->
          Lwt_io.close child_exit >>= fun () ->
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

let signal_names =
  [
    (Sys.sigabrt, "SIGABRT");
    (Sys.sigalrm, "SIGALRM");
    (Sys.sigfpe, "SIGFPE");
    (Sys.sighup, "SIGHUP");
    (Sys.sigill, "SIGILL");
    (Sys.sigint, "SIGINT");
    (Sys.sigkill, "SIGKILL");
    (Sys.sigpipe, "SIGPIPE");
    (Sys.sigquit, "SIGQUIT");
    (Sys.sigsegv, "SIGSEGV");
    (Sys.sigterm, "SIGTERM");
    (Sys.sigusr1, "SIGUSR1");
    (Sys.sigusr2, "SIGUSR2");
    (Sys.sigchld, "SIGCHLD");
    (Sys.sigcont, "SIGCONT");
    (Sys.sigstop, "SIGSTOP");
    (Sys.sigtstp, "SIGTSTP");
    (Sys.sigttin, "SIGTTIN");
    (Sys.sigttou, "SIGTTOU");
    (Sys.sigvtalrm, "SIGVTALRM");
    (Sys.sigprof, "SIGPROF");
    (Sys.sigbus, "SIGBUS");
    (Sys.sigpoll, "SIGPOLL");
    (Sys.sigsys, "SIGSYS");
    (Sys.sigtrap, "SIGTRAP");
    (Sys.sigurg, "SIGURG");
    (Sys.sigxcpu, "SIGXCPU");
    (Sys.sigxfsz, "SIGXFSZ");
  ]

let signal_name n = List.assoc ~equal:Int.equal n signal_names

(* Associate a value to a list of values  *)
module Assoc = struct
  let add ~equal k v t =
    match List.assoc ~equal k t with
    | None -> (k, [v]) :: t
    | Some l -> (k, v :: l) :: List.remove_assoc ~equal k t

  let iter f t = List.iter f t
end

(* [group_by f h l] for all elements [e] of [l] groups all [g e] that have the same value
   for [f e]  *)
let group_by ~equal f g l =
  let rec aux l res =
    match l with
    | [] -> res
    | h :: t -> aux t (Assoc.add ~equal (f h) (g h) res)
  in
  aux l []

(* Print the list of processes from a list of results of detached process *)
let pp_process_list ppf plist =
  List.iter
    (fun (i, prefix, _) -> Format.fprintf ppf "%d(%s)@ " i (String.trim prefix))
    (List.sort (fun (i, _, _) (j, _, _) -> Int.compare i j) plist)

let is_plural = function [] | [_] -> false | _ -> true

let plural str = function [] | [_] -> "" | _ -> str

(* Print the trace of an error from detached process *)
let pp_trace plural ppf err =
  match err with
  | (Canceled | Exn Lwt.Canceled) :: _ ->
      Format.fprintf ppf " %s been canceled." (if plural then "have" else "has")
  | Exn (Exited n) :: _ -> Format.fprintf ppf "finished with exit code %d." n
  | Exn (Signaled n) :: _ ->
      Format.fprintf
        ppf
        "%s killed by a %s !"
        (if plural then "were" else "was")
        (Option.value ~default:"Unknown signal" (signal_name n))
  | Exn (Stopped n) :: _ ->
      Format.fprintf
        ppf
        "%s stopped by a %s !"
        (if plural then "were" else "was")
        (Option.value ~default:"Unknown signal" (signal_name n))
  | err -> Format.fprintf ppf "failed with error:@ @[%a@]" pp_print_trace err

(* Print the trace of multiple detached process, grouped by identical traces *)
let pp_grouped ppf plist pp_trace =
  let grouped =
    group_by ~equal:Stdlib.( = ) (fun (_, _, res) -> res) (fun p -> p) plist
  in
  Assoc.iter
    (fun (err, plist) ->
      Format.fprintf
        ppf
        "@[<v 4>The process%s @[%a@]@ @[%a@]@]@;"
        (plural "es" plist)
        pp_process_list
        plist
        (pp_trace (is_plural plist))
        err)
    grouped

(* Print the status of a list of detached process.
   Grouped by final result.
   TODO: either print the OK result, or ignore the result
   value when Ok.  *)
let pp_results ppf plist =
  let pp_res plural ppf res =
    match res with
    | Ok _ -> Format.fprintf ppf "finished successfully."
    | Error err -> pp_trace plural ppf err
  in
  pp_grouped ppf plist pp_res

(* print the list of results from detached process *)
let print_results plist = lwt_log_error "@[%a@]@." pp_results plist

let send {channel; _} v = Channel.push channel v

let receive {channel; _} = Channel.pop channel

let wait_result {termination; _} : 'result tzresult Lwt.t = termination

type error += Par of (int * string * error) list

let pp_par_error ppf plist =
  pp_grouped ppf plist (fun plural ppf err -> pp_trace plural ppf [err])

let () =
  register_recursive_error_kind
    `Temporary
    ~id:"parallel_errors"
    ~title:"Parallel errors"
    ~description:
      "An error occured in at least one thread of a paralle\n  execution."
    ~pp:(fun ppf s -> Format.fprintf ppf "@[%a@]@." pp_par_error s)
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

let join_process (plist : ('a, 'b, 'c) t list) =
  List.map_p
    (fun {termination; prefix; _} ->
      termination >>= fun t -> Lwt.return (prefix, t))
    plist

(** Wait for all processes to terminate.

    If a node terminates with an error, all remaining processes
    are canceled and an exception is raised *)
let wait_all_results (processes : ('a, 'b, 'c) t list) =
  let rec loop processes =
    match processes with
    | [] -> Lwt.return_none
    | processes -> (
        Lwt.nchoose_split processes >>= function
        | (finished, remaining) ->
            let rec handle = function
              | [] -> loop remaining
              | Ok _ :: finished -> handle finished
              | Error err :: _ ->
                  Lwt.return_some
                    ( err,
                      List.map
                        (fun remain -> remain >>= fun _ -> return_unit)
                        remaining )
            in
            handle finished)
  in
  let terminations = List.map (fun p -> p.termination) processes in
  loop terminations >>= function
  | None ->
      lwt_log_info "All done!" >>= fun () ->
      Error_monad.Lwt_syntax.all terminations >>= fun terminated ->
      return
      @@ List.map (function Ok a -> a | Error _ -> assert false) terminated
  | Some (_err, remaining) ->
      lwt_log_error "Early error! Canceling remaining process." >>= fun () ->
      List.iter Lwt.cancel remaining ;
      join_process processes >>= fun terminated ->
      let terminated =
        List.mapi (fun i (prefix, a) -> (i, prefix, a)) terminated
      in
      let errors =
        List.filter_map
          (function
            | (_, _, Ok _) -> None
            | (i, prefix, Error (err :: _)) -> Some (i, prefix, err)
            | (i, prefix, Error []) ->
                Some
                  ( i,
                    prefix,
                    Exn
                      (Invalid_argument "process returned an empty error trace")
                  ))
          terminated
      in
      print_results terminated >>= fun _ -> Lwt.return @@ error (Par errors)

let wait_all pl =
  wait_all_results pl >>= function
  | Ok _ -> return_unit
  | Error err -> Lwt.return_error err
