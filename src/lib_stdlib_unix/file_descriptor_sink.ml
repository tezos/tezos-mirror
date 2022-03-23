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
  format : [`One_per_line | `Netstring | `Pp];
  (* Hopefully temporary hack to handle event which are emitted with
     the non-cooperative log functions in `Legacy_logging`: *)
  lwt_bad_citizen_hack : string list ref;
  filter :
    [ `Level_at_least of Internal_event.Level.t
    | `Per_section_prefix of
      (Internal_event.Section.t * Internal_event.Level.t) list ];
}

let hostname =
  Option.value_f
    (Sys.getenv_opt "TEZOS_EVENT_HOSTNAME")
    ~default:Unix.gethostname

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
    | `Path -> "file-descriptor-path"
    | `Stdout -> "file-descriptor-stdout"
    | `Stderr -> "file-descriptor-stderr"

  let configure uri =
    let open Lwt_tzresult_syntax in
    let fail_parsing fmt =
      Format.kasprintf (failwith "Parsing URI: %s: %s" (Uri.to_string uri)) fmt
    in
    let section_prefixes =
      let all =
        List.filter_map
          (function ("section-prefix", l) -> Some l | _ -> None)
          (Uri.query uri)
      in
      match all with [] -> None | more -> Some (List.concat more)
    in
    let* filter =
      match (Uri.get_query_param uri "level-at-least", section_prefixes) with
      | (None, None) -> return (`Level_at_least Internal_event.Level.default)
      | (Some l, None) -> (
          match Internal_event.Level.of_string l with
          | Some l -> return (`Level_at_least l)
          | None -> fail_parsing "Wrong level: %S" l)
      | (base_level, Some l) -> (
          try
            let sections =
              let parse_section s =
                match String.split_on_char ':' s with
                | [one] ->
                    ( Internal_event.Section.make_sanitized
                        (String.split_on_char '.' one),
                      Internal_event.Level.default )
                | [one; two] ->
                    let lvl =
                      match Internal_event.Level.of_string two with
                      | Some s -> s
                      | None ->
                          Format.kasprintf
                            Stdlib.failwith
                            "Wrong level name: %S in argument %S"
                            two
                            s
                    in
                    let section =
                      match one with
                      | "" -> Internal_event.Section.empty
                      | _ ->
                          Internal_event.Section.make_sanitized
                            (String.split_on_char '.' one)
                    in
                    (section, lvl)
                | _ ->
                    Format.kasprintf
                      Stdlib.failwith
                      "Wrong section-level entry: %S"
                      s
              in
              let pairs = List.map parse_section l in
              match base_level with
              | None -> pairs
              | Some lvl -> (
                  match Internal_event.Level.of_string lvl with
                  | Some l -> (Internal_event.Section.empty, l) :: pairs
                  | None ->
                      Format.kasprintf
                        Stdlib.failwith
                        "Wrong level name %S in level-at-least argument"
                        lvl)
            in
            return (`Per_section_prefix sections)
          with Failure s -> fail_parsing "%s" s)
    in
    let* format =
      match Uri.get_query_param uri "format" with
      | Some "netstring" -> return `Netstring
      | Some "pp" -> return `Pp
      | None | Some "one-per-line" -> return `One_per_line
      | Some other -> fail_parsing "Unknown format: %S" other
    in
    let* output =
      match K.kind with
      | `Path -> (
          let flag name =
            match Uri.get_query_param uri name with
            | Some "true" -> true
            | _ -> false
          in
          let with_pid = flag "with-pid" in
          let fresh = flag "fresh" in
          let* rights =
            match Uri.get_query_param uri "chmod" with
            | Some n -> (
                match int_of_string_opt n with
                | Some i -> return i
                | None ->
                    fail_parsing
                      "Access-rights parameter should be an integer: %S"
                      n)
            | None -> return 0o600
          in
          match Uri.path uri with
          | "" | "/" -> fail_parsing "Missing path configuration."
          | path ->
              let fixed_path =
                if with_pid then
                  let ext = Filename.extension path in
                  let chopped =
                    if ext = "" then path else Filename.chop_extension path
                  in
                  Format.asprintf "%s-%d%s" chopped (Unix.getpid ()) ext
                else path
              in
              protect (fun () ->
                  Lwt_result.ok
                  @@ Lwt_unix.(
                       let flags =
                         [O_WRONLY; O_CREAT]
                         @ if fresh then [O_TRUNC] else [O_APPEND]
                       in
                       openfile fixed_path flags rights)))
      | `Stdout -> return Lwt_unix.stdout
      | `Stderr -> return Lwt_unix.stderr
    in
    let t = {output; lwt_bad_citizen_hack = ref []; filter; format} in
    return t

  let write_mutex = Lwt_mutex.create ()

  let output_one output to_write =
    protect (fun () ->
        Lwt_result.ok
        @@ Lwt_mutex.with_lock write_mutex (fun () ->
               Lwt_utils_unix.write_string output to_write))

  let handle (type a) {output; lwt_bad_citizen_hack; filter; format; _} m
      ?(section = Internal_event.Section.empty) (v : unit -> a) =
    let open Lwt_tzresult_syntax in
    let module M = (val m : Internal_event.EVENT_DEFINITION with type t = a) in
    let now = Unix.gettimeofday () in
    let forced_event = v () in
    let level = M.level forced_event in
    let filter_run =
      match filter with
      | `Level_at_least level_at_least ->
          Internal_event.Level.compare level level_at_least >= 0
      | `Per_section_prefix kvl ->
          List.exists
            (fun (prefix, lvl) ->
              Internal_event.(
                Section.is_prefix ~prefix section
                && Level.compare level lvl >= 0))
            kvl
    in
    if filter_run then (
      let wrapped_event = wrap now section forced_event in
      let to_write =
        let json () =
          Data_encoding.Json.construct
            (wrapped_encoding M.encoding)
            wrapped_event
        in
        match format with
        | `Pp ->
            let s =
              String.map
                (function '\n' -> ' ' | c -> c)
                (Format.asprintf "%a" (M.pp ~short:false) forced_event)
            in
            (* See https://tools.ietf.org/html/rfc5424#section-6 *)
            Format.asprintf
              "%a [%s%s] %s\n"
              (Ptime.pp_rfc3339 ~frac_s:3 ())
              (match Ptime.of_float_s wrapped_event.time_stamp with
              | Some s -> s
              | None -> Ptime.min)
              (Internal_event.Section.to_string_list wrapped_event.section
              |> String.concat ".")
              M.name
              s
        | `One_per_line -> Ezjsonm.value_to_string ~minify:true (json ()) ^ "\n"
        | `Netstring ->
            let bytes = Ezjsonm.value_to_string ~minify:true (json ()) in
            Format.asprintf "%d:%s," (String.length bytes) bytes
      in
      lwt_bad_citizen_hack := to_write :: !lwt_bad_citizen_hack ;
      let*! r = output_one output to_write in
      match r with
      | Error [Exn (Unix.Unix_error (Unix.EBADF, _, _))] ->
          (* The file descriptor was closed before the event arrived,
             ignore it. *)
          return_unit
      | Error _ as err -> Lwt.return err
      | Ok () ->
          lwt_bad_citizen_hack :=
            List.filter (( = ) to_write) !lwt_bad_citizen_hack ;
          return_unit)
    else return_unit

  let close {lwt_bad_citizen_hack; output; _} =
    let open Lwt_tzresult_syntax in
    let* () =
      List.iter_es
        (fun event_string -> output_one output event_string)
        !lwt_bad_citizen_hack
    in
    match K.kind with
    | `Path -> Lwt_result.ok @@ Lwt_unix.close output
    | `Stdout | `Stderr -> return_unit
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
