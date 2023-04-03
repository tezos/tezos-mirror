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

let lwt_log_debug fmt = log_f ~level:Lwt_log.Debug fmt

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
  let open Lwt_result_syntax in
  let value_encoding =
    Option.value ~default:(dummy_encoding flags) value_encoding
  in
  protect
    (fun () ->
      let*! () =
        match Data_encoding.Binary.to_bytes value_encoding v with
        | Ok encoded_v -> Lwt_io.write_value outch encoded_v
        | Error err ->
            Stdlib.failwith
            @@ Format.asprintf
                 "Value encoding failed %a"
                 Data_encoding.Binary.pp_write_error
                 err
      in
      return_unit)
    ~on_error:(fun trace ->
      Lwt.return_error
      @@ TzTrace.cons (error_of_fmt "write error %s" __LOC__) trace)

let read ~value_encoding ~flags inch =
  let open Lwt_result_syntax in
  let value_encoding =
    Option.value ~default:(dummy_encoding flags) value_encoding
  in
  protect
    (fun () ->
      let*! encoded_v = Lwt_io.read_value inch in
      let*! v =
        match Data_encoding.Binary.of_bytes value_encoding encoded_v with
        | Ok decoded_v -> Lwt.return decoded_v
        | Error err ->
            Stdlib.failwith
            @@ Format.asprintf
                 "Value encoding failed %a"
                 Data_encoding.Binary.pp_read_error
                 err
      in
      return v)
    ~on_error:(fun trace ->
      Lwt.return_error
      @@ TzTrace.cons (error_of_fmt "read error %s" __LOC__) trace)

let received_result ~value_encoding ~flags child_exit =
  let open Lwt_syntax in
  let value_encoding =
    Option.some @@ Error_monad.result_encoding
    @@ Option.value ~default:(dummy_encoding flags) value_encoding
  in
  let+ r = read ~value_encoding ~flags child_exit in
  Result.join r

let send_result ~value_encoding ~flags child_exit result =
  let value_encoding =
    Option.some @@ Error_monad.result_encoding
    @@ Option.value ~default:(dummy_encoding flags) value_encoding
  in
  write ~value_encoding ~flags child_exit result

let handle_result ~value_encoding ~flags canceler f child_exit =
  let open Lwt_result_syntax in
  let*! r =
    protect
      ~canceler
      (fun () ->
        let* v = f () in
        let* () = send_result ~value_encoding ~flags child_exit (Ok v) in
        return 0)
      ~on_error:(fun err ->
        let*! () =
          lwt_log_error
            "@[<v 2>Detached process ended with error.@[%a@]@]@."
            pp_print_trace
            err
        in
        let* () = send_result ~value_encoding ~flags child_exit (Error err) in
        return 0)
  in
  match r with
  | Ok exit_code -> Lwt.return exit_code
  | Error err ->
      let*! () =
        lwt_log_error
          "@[<v 2>Unexpected error when handling detached function result: \
           @[%a@]@]@."
          Error_monad.pp_print_trace
          err
      in
      Lwt.return 255

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
    Error_monad.catch_es (fun () ->
        let value_encoding = output_encoding in
        write ~value_encoding ~flags outch v)

  let pop {inch; input_encoding; flags; _} =
    Error_monad.catch_es (fun () ->
        let value_encoding = input_encoding in
        read ~value_encoding ~flags inch)
end

let terminate pid =
  let open Lwt_syntax in
  (try Unix.kill pid Sys.sigterm with _ -> ()) ;
  let* _pid, _status = Lwt_unix.waitpid [] pid in
  Lwt.return_unit

let wait ~value_encoding ~flags pid result_ch =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let*! s = Lwt_unix.waitpid [] pid in
      match s with
      | _, Lwt_unix.WEXITED 0 ->
          received_result ~value_encoding ~flags result_ch
      | _, Lwt_unix.WEXITED n -> fail_with_exn (Exited n)
      | _, Lwt_unix.WSIGNALED n -> fail_with_exn (Signaled n)
      | _, Lwt_unix.WSTOPPED n -> fail_with_exn (Stopped n))
    (function
      | Lwt.Canceled ->
          let*! () = terminate pid in
          tzfail Canceled
      | exn -> fail_with_exn exn)

type ('a, 'b, 'c) t = {
  after_termination : unit -> unit Lwt.t;
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
  let open Lwt_syntax in
  let canceler = Option.value_f ~default:Lwt_canceler.create canceler in
  let* () = Lwt_io.flush_all () in
  protect
    ~canceler
    (fun () ->
      let main_in, child_out = Lwt_io.pipe ~cloexec:true () in
      let child_in, main_out = Lwt_io.pipe ~cloexec:true () in
      let main_result, child_exit = Lwt_io.pipe ~cloexec:true () in
      match Lwt_unix.fork () with
      | 0 ->
          (* I am the child process. *)
          Lwt_log.default :=
            Lwt_log.channel
              ~template
              ~close_mode:`Keep
              ~channel:Lwt_io.stderr
              () ;
          Random.self_init () ;
          (* Lwt_main.run *)
          let* i =
            let template = Format.asprintf "%s$(message)" prefix in
            let* () = Lwt_io.close main_in in
            let* () = Lwt_io.close main_out in
            let* () = Lwt_io.close main_result in
            Lwt_log.default :=
              Lwt_log.channel
                ~template
                ~close_mode:`Keep
                ~channel:Lwt_io.stderr
                () ;
            let* () = lwt_log_debug "PID: %d" (Unix.getpid ()) in
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
              child_exit
          in
          let* () = Lwt_io.close child_in in
          let* () = Lwt_io.close child_out in
          let* () = Lwt_io.close child_exit in
          exit i
      | pid ->
          (* I am the main process. *)
          Lwt_canceler.on_cancel canceler (fun () -> terminate pid) ;
          let termination = wait ~value_encoding ~flags pid main_result in
          let after_termination () =
            let* () = Lwt_io.close main_in in
            let* () = Lwt_io.close main_out in
            let* () = Lwt_io.close main_result in
            Lwt.return_unit
          in
          let* () = Lwt_io.close child_in in
          let* () = Lwt_io.close child_out in
          let* () = Lwt_io.close child_exit in
          return_ok
            {
              after_termination;
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
      let* (_ : (unit, exn list) result) = Lwt_canceler.cancel canceler in
      return_error err)

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
   for [f e] *)
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
   value when Ok. *)
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

let join_process (plist : ('a, 'b, 'c) t list) =
  let open Lwt_syntax in
  List.map_p
    (fun {termination; prefix; _} ->
      let* t = termination in
      return (prefix, t))
    plist

(** Wait for all processes to terminate.

    If a node terminates with an error, all remaining processes
    are canceled and an exception is raised *)
let wait_all_results (processes : ('a, 'b, 'c) t list) =
  let rec loop processes =
    let open Lwt_syntax in
    match processes with
    | [] -> return_none
    | processes ->
        let* finished, remaining = Lwt.nchoose_split processes in
        let rec handle = function
          | [] -> loop remaining
          | Ok _ :: finished -> handle finished
          | Error err :: _ ->
              return_some
                ( err,
                  List.map
                    (fun remain ->
                      let* _ = remain in
                      return_ok_unit)
                    remaining )
        in
        handle finished
  in
  let open Lwt_syntax in
  let terminations = List.map (fun p -> p.termination) processes in
  let* o = loop terminations in
  match o with
  | None -> (
      let* () = lwt_log_debug "All done!" in
      let* terminated = all terminations in
      match List.partition_result terminated with
      | _, _ :: _ -> assert false
      | terminated, [] -> return_ok terminated)
  | Some (_err, remaining) -> (
      let* () = lwt_log_error "Early error! Canceling remaining process." in
      List.iter Lwt.cancel remaining ;
      let* terminated = join_process processes in
      let terminated =
        List.mapi (fun i (prefix, a) -> (i, prefix, a)) terminated
      in
      let errors =
        List.filter_map
          (function
            | _, _, Ok _ -> None
            | i, prefix, Error [] ->
                Some
                  (TzTrace.make
                     (Exn
                        (Invalid_argument
                           (Format.asprintf
                              "process %d(%s) returned an empty error trace"
                              i
                              (String.trim prefix)))))
            | i, prefix, Error trace ->
                Some
                  (TzTrace.cons
                     (Exn
                        (Failure
                           (Format.asprintf "%d(%s)" i (String.trim prefix))))
                     trace))
          terminated
      in
      let* _ = print_results terminated in
      match errors with
      | [] -> assert false
      | err :: errs -> Lwt.return_error (TzTrace.conp_list err errs))

let wait_all pl =
  let open Lwt_result_syntax in
  let* _ = wait_all_results pl in
  let*! () = List.iter_s (fun p -> p.after_termination ()) pl in
  return_unit
