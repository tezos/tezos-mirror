(******************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type t = {
  output : Lwt_unix.file_descr;
  format : [`One_per_line | `Netstring];
  (* Hopefully temporary hack to handle event which are emitted with
     the non-cooperative log functions in `Legacy_logging`: *)
  lwt_bad_citizen_hack : Data_encoding.json list ref;
  level_at_least : Internal_event.Level.t;
}

let hostname =
  Option.value
    (Sys.getenv_opt "TEZOS_NODE_HOSTNAME")
    ~default:(Unix.gethostname ())

type 'event wrapped = {
  time_stamp : float;
  section : Internal_event.Section.t;
  event : 'event;
}

let wrap time_stamp section event = {time_stamp; section; event}

let wrapped_encoding event_encoding =
  let open Data_encoding in
  let v0 =
    conv
      (fun {time_stamp; section; event} ->
        (hostname, time_stamp, section, event))
      (fun (_, time_stamp, section, event) -> {time_stamp; section; event})
      (obj4
         (req "hostname" string)
         (req "time_stamp" float)
         (req "section" Internal_event.Section.encoding)
         (req "event" event_encoding))
  in
  With_version.(encoding ~name:"fd-sink-item" (first_version v0))

module Make_sink (K : sig
  val kind : [`Path | `Stdout | `Stderr]
end) : Internal_event.SINK with type t = t = struct
  type nonrec t = t

  let uri_scheme =
    match K.kind with
    | `Path ->
        "file-descriptor-path"
    | `Stdout ->
        "file-descriptor-stdout"
    | `Stderr ->
        "file-descriptor-stderr"

  let configure uri =
    let level_at_least =
      let ( >?? ) = Option.bind in
      Uri.get_query_param uri "level-at-least"
      >?? Internal_event.Level.of_string
      |> Option.value ~default:Internal_event.Level.default
    in
    let fail_parsing fmt =
      Format.kasprintf (failwith "Parsing URI: %s: %s" (Uri.to_string uri)) fmt
    in
    ( match Uri.get_query_param uri "format" with
    | Some "netstring" ->
        return `Netstring
    | None | Some "one-per-line" ->
        return `One_per_line
    | Some other ->
        fail_parsing "Unknown format: %S" other )
    >>=? fun format ->
    ( match K.kind with
    | `Path -> (
        let flag name =
          match Uri.get_query_param uri name with
          | Some "true" ->
              true
          | _ ->
              false
        in
        let with_pid = flag "with-pid" in
        let fresh = flag "fresh" in
        ( match Uri.get_query_param uri "chmod" with
        | Some n -> (
          try return (int_of_string n)
          with _ ->
            fail_parsing "Access-rights parameter should be an integer: %S" n )
        | None ->
            return 0o600 )
        >>=? fun rights ->
        match Uri.path uri with
        | "" | "/" ->
            fail_parsing "Missing path configuration."
        | path ->
            let fixed_path =
              if with_pid then
                let ext = Filename.extension path in
                let chopped =
                  if ext = "" then path else Filename.chop_extension path
                in
                Fmt.strf "%s-%d%s" chopped (Unix.getpid ()) ext
              else path
            in
            protect (fun () ->
                Lwt_unix.(
                  let flags =
                    [O_WRONLY; O_CREAT]
                    @ if fresh then [O_TRUNC] else [O_APPEND]
                  in
                  openfile fixed_path flags rights)
                >>= fun fd -> return fd) )
    | `Stdout ->
        return Lwt_unix.stdout
    | `Stderr ->
        return Lwt_unix.stderr )
    >>=? fun output ->
    let t = {output; lwt_bad_citizen_hack = ref []; level_at_least; format} in
    return t

  let output_one output format event_json =
    let to_write =
      match format with
      | `One_per_line ->
          Ezjsonm.value_to_string ~minify:true event_json ^ "\n"
      | `Netstring ->
          let bytes = Ezjsonm.value_to_string ~minify:true event_json in
          Fmt.str "%d:%s," (String.length bytes) bytes
    in
    protect (fun () ->
        (*
           If the write does happen at once (i.e. returns the same number of bytes),
           POSIX (and Linux >= 3.14) ensure this is atomic.
           Cf. http://man7.org/linux/man-pages/man2/write.2.html#BUGS
           and `https://github.com/ocsigen/lwt/blob/master/src/unix/unix_c/unix_write.c` *)
        Lwt_utils_unix.write_string output to_write >>= fun () -> return_unit)

  let handle (type a) {output; lwt_bad_citizen_hack; level_at_least; format; _}
      m ?(section = Internal_event.Section.empty) (v : unit -> a) =
    let module M = (val m : Internal_event.EVENT_DEFINITION with type t = a) in
    let now = Unix.gettimeofday () in
    let forced_event = v () in
    let level = M.level forced_event in
    if Internal_event.Level.compare level level_at_least >= 0 then (
      let wrapped_event = wrap now section forced_event in
      let event_json =
        Data_encoding.Json.construct
          (wrapped_encoding M.encoding)
          wrapped_event
      in
      lwt_bad_citizen_hack := event_json :: !lwt_bad_citizen_hack ;
      output_one output format event_json
      >>= function
      | Error [Exn (Unix.Unix_error (Unix.EBADF, _, _))] ->
          (* The file descriptor was closed before the event arrived,
             ignore it. *)
          return_unit
      | Error _ as err ->
          Lwt.return err
      | Ok () ->
          lwt_bad_citizen_hack :=
            List.filter (( = ) event_json) !lwt_bad_citizen_hack ;
          return_unit )
    else return_unit

  let close {lwt_bad_citizen_hack; output; format; _} =
    List.iter_es
      (fun event_json -> output_one output format event_json)
      !lwt_bad_citizen_hack
    >>=? fun () -> Lwt_unix.close output >>= fun () -> return_unit
end

module Sink_implementation_path = Make_sink (struct
  let kind = `Path
end)

module Sink_implementation_stdout = Make_sink (struct
  let kind = `Stdout
end)

module Sink_implementation_stderr = Make_sink (struct
  let kind = `Stderr
end)

let () = Internal_event.All_sinks.register (module Sink_implementation_path)

let () = Internal_event.All_sinks.register (module Sink_implementation_stdout)

let () = Internal_event.All_sinks.register (module Sink_implementation_stderr)
