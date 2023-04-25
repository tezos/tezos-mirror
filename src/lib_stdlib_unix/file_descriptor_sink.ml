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

type current = {day : int * int * int; fd : Lwt_unix.file_descr}

type rotating = {
  rights : int;
  days_kept : int;
  base_path : string;
  current : current ref;
}

type output = Static of Lwt_unix.file_descr | Rotating of rotating

type t = {
  output : output;
  format :
    [ `One_per_line
    | `Netstring
    | (* See https://tools.ietf.org/html/rfc5424#section-6 *)
      `Pp_RFC5424
    | `Pp_short ];
  (* Hopefully temporary hack to handle event which are emitted with
     the non-cooperative log functions in `Legacy_logging`: *)
  lwt_bad_citizen_hack : string list ref;
  filter :
    [ `Level_at_least of Internal_event.Level.t
    | `Per_section_prefix of
      (Internal_event.Section.t * Internal_event.Level.t option) list ];
}

let hostname =
  Option.value_f
    (Sys.getenv_opt "TEZOS_EVENT_HOSTNAME")
    ~default:Unix.gethostname

type 'event wrapped = {
  time_stamp : Ptime.t;
  section : Internal_event.Section.t;
  event : 'event;
}

let wrap time_stamp section event = {time_stamp; section; event}

let wrapped_encoding event_encoding =
  let open Data_encoding in
  let ptime_encoding =
    conv
      Ptime.to_float_s
      (fun f ->
        match Ptime.of_float_s f with
        | None -> invalid_arg "Time.System.Span.encoding"
        | Some s -> s)
      float
  in
  let v0 =
    conv
      (fun {time_stamp; section; event} ->
        (hostname, time_stamp, section, event))
      (fun (_, time_stamp, section, event) -> {time_stamp; section; event})
      (obj4
         (req "hostname" string)
         (req "time_stamp" ptime_encoding)
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

  let fail_parsing uri fmt =
    Format.kasprintf (failwith "Parsing URI: %s: %s" (Uri.to_string uri)) fmt

  let day_of_the_year ts =
    let today =
      match Ptime.of_float_s ts with Some s -> s | None -> Ptime.min
    in
    let (y, m, d), _ = Ptime.to_date_time today in
    (y, m, d)

  let string_of_day_of_the_year (y, m, d) = Format.sprintf "%d%02d%02d" y m d

  let check_file_format_with_date base_filename s =
    let name_no_ext = Filename.remove_extension base_filename in
    let ext = Filename.extension base_filename in
    let open Re.Perl in
    let re_ext = "(." ^ ext ^ ")?" in
    let re_date = "-\\d{4}\\d{2}\\d{2}" in
    let re = compile @@ re (name_no_ext ^ re_date ^ re_ext) in
    Re.execp re s

  let filename_insert_before_ext ~path s =
    let ext = Filename.extension path in
    let chopped = if ext = "" then path else Filename.chop_extension path in
    Format.asprintf "%s-%s%s" chopped s ext

  let configure uri =
    let open Lwt_result_syntax in
    let section_prefixes =
      let all =
        List.filter_map
          (function "section-prefix", l -> Some l | _ -> None)
          (Uri.query uri)
      in
      match all with [] -> None | more -> Some (List.concat more)
    in
    let* filter =
      match (Uri.get_query_param uri "level-at-least", section_prefixes) with
      | None, None -> return (`Level_at_least Internal_event.Level.default)
      | Some l, None -> (
          match Internal_event.Level.of_string l with
          | Some l -> return (`Level_at_least l)
          | None -> fail_parsing uri "Wrong level: %S" l)
      | base_level, Some l -> (
          try
            let sections =
              let parse_section s =
                match String.split_on_char ':' s with
                | [one] ->
                    ( Internal_event.Section.make_sanitized
                        (String.split_on_char '.' one),
                      Some Internal_event.Level.default )
                | [one; two] ->
                    let lvl =
                      match String.lowercase_ascii two with
                      | "none" -> None
                      | s -> (
                          match Internal_event.Level.of_string s with
                          | Some s -> Some s
                          | None ->
                              Format.kasprintf
                                Stdlib.failwith
                                "Wrong level name: %S in argument %S"
                                two
                                s)
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
                  | Some l ->
                      (* establish default for all sections *)
                      (Internal_event.Section.empty, Some l) :: pairs
                  | None ->
                      Format.kasprintf
                        Stdlib.failwith
                        "Wrong level name %S in level-at-least argument"
                        lvl)
            in
            return (`Per_section_prefix sections)
          with Failure s -> fail_parsing uri "%s" s)
    in
    let* format =
      match Uri.get_query_param uri "format" with
      | Some "netstring" -> return `Netstring
      | Some "pp-short" -> return `Pp_short
      | Some "pp-rfc5424" -> return `Pp_RFC5424
      | Some "pp" -> return `Pp_RFC5424
      | None | Some "one-per-line" -> return `One_per_line
      | Some other -> fail_parsing uri "Unknown format: %S" other
    in
    let* output =
      match K.kind with
      | `Path ->
          let* rotate =
            match Uri.get_query_param uri "daily-logs" with
            | Some n -> (
                match int_of_string_opt n with
                | Some n -> return_some n
                | None ->
                    fail_parsing uri "daily-logs should be an integer : %S" n)
            | None -> return_none
          in
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
                      uri
                      "Access-rights parameter should be an integer: %S"
                      n)
            | None -> return 0o600
          in
          let* path =
            match Uri.path uri with
            | "" | "/" -> fail_parsing uri "Missing path configuration."
            | path -> return path
          in
          let allow_create_dir = flag "create-dirs" in
          let*! () =
            if allow_create_dir then
              Lwt_utils_unix.create_dir (Filename.dirname path)
            else Lwt.return_unit
          in
          let open Lwt_result_syntax in
          let time_ext, rotation =
            match rotate with
            | Some days_kept ->
                let today = day_of_the_year (Unix.gettimeofday ()) in
                (string_of_day_of_the_year today, Some (days_kept, today))
            | None -> ("", None)
          in
          let base_path =
            if with_pid then
              filename_insert_before_ext ~path (string_of_int (Unix.getpid ()))
            else path
          in
          let fixed_path =
            if rotate <> None then filename_insert_before_ext ~path time_ext
            else base_path
          in
          protect (fun () ->
              Lwt_result.ok
              @@ Lwt_unix.(
                   let flags =
                     [O_WRONLY; O_CREAT]
                     @ if fresh then [O_TRUNC] else [O_APPEND]
                   in
                   let*! fd = openfile fixed_path flags rights in
                   match rotation with
                   | Some (days_kept, cur_day) ->
                       Lwt.return
                         (Rotating
                            {
                              rights;
                              base_path;
                              days_kept;
                              current = ref {fd; day = cur_day};
                            })
                   | None -> Lwt.return (Static fd)))
      | `Stdout -> return (Static Lwt_unix.stdout)
      | `Stderr -> return (Static Lwt_unix.stderr)
    in
    let t = {output; lwt_bad_citizen_hack = ref []; filter; format} in
    return t

  let write_mutex = Lwt_mutex.create ()

  let list_rotation_files base_path =
    let open Lwt_syntax in
    let dirname = Filename.dirname base_path in
    let base_filename = Filename.basename base_path in
    let file_stream = Lwt_unix.files_of_directory dirname in
    let rec explore acc =
      let* filename = Lwt_stream.get file_stream in
      match filename with
      | None -> Lwt.return acc
      | Some filename ->
          if check_file_format_with_date base_filename filename then
            explore (filename :: acc)
          else explore acc
    in
    explore []

  let remove_older_files dirname n_kept base_path =
    let open Lwt_syntax in
    let* files = list_rotation_files base_path in
    let sorted = List.sort (fun x y -> -compare x y) files in
    List.iteri_s
      (fun i file ->
        if i >= n_kept then Lwt_unix.unlink (Filename.concat dirname file)
        else Lwt.return_unit)
      sorted

  let output_one_with_rotation {rights; base_path; current; days_kept} now
      to_write =
    let open Lwt_result_syntax in
    let {day; fd} = !current in
    let today = Ptime.to_date now in
    let should_rotate_output = day <> today in
    let* () =
      Lwt_mutex.with_lock write_mutex (fun () ->
          let* output =
            if not should_rotate_output then return fd
            else
              let*! () = Lwt_unix.close fd in
              let path =
                filename_insert_before_ext
                  ~path:base_path
                  (string_of_day_of_the_year today)
              in
              let* fd =
                protect (fun () ->
                    Lwt_result.ok
                    @@ Lwt_unix.(
                         let flags = [O_WRONLY; O_CREAT; O_APPEND] in
                         openfile path flags rights))
              in
              current := {fd; day = today} ;
              return fd
          in
          Lwt_result.ok @@ Lwt_utils_unix.write_string output to_write)
    in
    let*! () =
      if should_rotate_output then
        remove_older_files (Filename.dirname base_path) days_kept base_path
      else Lwt.return_unit
    in
    return_unit

  let output_one now output to_write =
    match output with
    | Static output ->
        protect (fun () ->
            Lwt_result.ok
            @@ Lwt_mutex.with_lock write_mutex (fun () ->
                   Lwt_utils_unix.write_string output to_write))
    | Rotating output -> output_one_with_rotation output now to_write

  let should_handle (type a) ?(section = Internal_event.Section.empty)
      {filter; _} m =
    let module M = (val m : Internal_event.EVENT_DEFINITION with type t = a) in
    match filter with
    | `Level_at_least level_at_least ->
        Internal_event.Level.compare M.level level_at_least >= 0
    | `Per_section_prefix kvl -> (
        match
          List.find
            (fun (prefix, _) ->
              Internal_event.Section.is_prefix ~prefix section)
            kvl
        with
        | None ->
            (* default *)
            Internal_event.Level.compare M.level Internal_event.Level.default
            >= 0
        | Some (_, None) -> (* exclude list *) false
        | Some (_, Some lvl) -> Internal_event.Level.compare M.level lvl >= 0)

  let make_with_pp_rfc5424 pp event wrapped_event name =
    (* See https://tools.ietf.org/html/rfc5424#section-6 *)
    Format.asprintf
      "%a [%s.%s] %a\n"
      (Ptime.pp_rfc3339 ~frac_s:3 ())
      wrapped_event.time_stamp
      (Internal_event.Section.to_string_list wrapped_event.section
      |> String.concat ".")
      name
      (pp ~all_fields:false ~block:false)
      event

  let make_with_pp_short pp event wrapped_event =
    let pp_date fmt time =
      let time = Ptime.to_float_s time in
      let tm = Unix.localtime time in
      let month_string =
        match tm.Unix.tm_mon with
        | 0 -> "Jan"
        | 1 -> "Feb"
        | 2 -> "Mar"
        | 3 -> "Apr"
        | 4 -> "May"
        | 5 -> "Jun"
        | 6 -> "Jul"
        | 7 -> "Aug"
        | 8 -> "Sep"
        | 9 -> "Oct"
        | 10 -> "Nov"
        | 11 | _ -> "Dec"
      in
      let ms = mod_float (time *. 1000.) 1000. in
      Format.fprintf
        fmt
        "%s %2d %02d:%02d:%02d.%03.0f"
        month_string
        tm.Unix.tm_mday
        tm.Unix.tm_hour
        tm.Unix.tm_min
        tm.Unix.tm_sec
        ms
    in
    let lines =
      String.split_on_char
        '\n'
        (Format.asprintf "%a" (pp ~all_fields:false ~block:true) event)
    in
    let timestamp = Format.asprintf "%a: " pp_date wrapped_event.time_stamp in
    let timestamp_size = String.length timestamp in
    let lines_size =
      List.fold_left
        (fun acc s ->
          (* total length of a line is (timestamp + message + \n *)
          acc + timestamp_size + String.length s + 1)
        0
        lines
    in
    let buf = Bytes.create lines_size in
    let prev_pos = ref 0 in
    let () =
      List.iter
        (fun s ->
          Bytes.blit_string timestamp 0 buf !prev_pos timestamp_size ;
          prev_pos := !prev_pos + timestamp_size ;
          let s_len = String.length s in
          Bytes.blit_string s 0 buf !prev_pos s_len ;
          prev_pos := !prev_pos + s_len ;
          Bytes.set buf !prev_pos '\n' ;
          prev_pos := !prev_pos + 1)
        lines
    in
    Bytes.unsafe_to_string buf

  let handle (type a) {output; lwt_bad_citizen_hack; format; _} m
      ?(section = Internal_event.Section.empty) (event : a) =
    let open Lwt_result_syntax in
    let module M = (val m : Internal_event.EVENT_DEFINITION with type t = a) in
    let now = Ptime_clock.now () in
    let wrapped_event = wrap now section event in
    let to_write =
      let json () =
        Data_encoding.Json.construct (wrapped_encoding M.encoding) wrapped_event
      in
      match format with
      | `Pp_RFC5424 -> make_with_pp_rfc5424 M.pp event wrapped_event M.name
      | `Pp_short -> make_with_pp_short M.pp event wrapped_event
      | `One_per_line -> Ezjsonm.value_to_string ~minify:true (json ()) ^ "\n"
      | `Netstring ->
          let bytes = Ezjsonm.value_to_string ~minify:true (json ()) in
          Format.asprintf "%d:%s," (String.length bytes) bytes
    in
    lwt_bad_citizen_hack := to_write :: !lwt_bad_citizen_hack ;
    let*! r = output_one now output to_write in
    match r with
    | Error [Exn (Unix.Unix_error (Unix.EBADF, _, _))] ->
        (* The file descriptor was closed before the event arrived,
           ignore it. *)
        return_unit
    | Error _ as err -> Lwt.return err
    | Ok () ->
        lwt_bad_citizen_hack :=
          List.filter (( = ) to_write) !lwt_bad_citizen_hack ;
        return_unit

  let close {lwt_bad_citizen_hack; output; _} =
    let open Lwt_result_syntax in
    let* () =
      List.iter_es
        (fun event_string ->
          let now = Ptime_clock.now () in
          output_one now output event_string)
        !lwt_bad_citizen_hack
    in
    match K.kind with
    | `Path -> (
        match output with
        | Rotating output ->
            let*! () = Lwt_unix.close !(output.current).fd in
            return_unit
        | Static output -> Lwt_result.ok @@ Lwt_unix.close output)
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
