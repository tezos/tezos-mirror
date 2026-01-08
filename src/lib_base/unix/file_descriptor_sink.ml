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

type output =
  | Static of Lwt_unix.file_descr
  | Rotating of rotating
  | Syslog of Syslog.t

type t = {
  output : output;
  format :
    [ `One_per_line
    | `Netstring
    | (* See https://tools.ietf.org/html/rfc5424#section-6 *)
      `Pp_RFC5424
    | `Pp_short ];
  colors : bool;
  filter :
    [ `Level_at_least of Internal_event.Level.t
    | `Per_section_prefix of
      (Internal_event.Section.t * Internal_event.Level.t option) list ];
  advertise_levels : bool;
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

let is_syslog o = match o with Syslog _ -> true | _ -> false

module Color = struct
  let reset = "\027[0m"

  let reset_len = 4

  let color_len = 5

  let bold = "\027[1m"

  let bold_len = 4

  module FG = struct
    (* Error level default color *)
    let red = "\027[31m"

    (* Warning level default color *)
    let yellow = "\027[33m"

    (* Alternative colors *)

    let green = "\027[32m"

    let blue = "\027[34m"

    let cyan = "\027[36m"

    let magenta = "\027[35m"
  end

  module Inverse = struct
    let red = "\027[7;31m"
  end
end

let wrapped_encoding ~advertise_levels event_encoding =
  let open Data_encoding in
  let ptime_encoding =
    conv
      Ptime.to_float_s
      (fun f ->
        match Ptime.of_float_s f with
        | None -> invalid_arg "File-descriptor-sink: invalid timestamp"
        | Some s -> s)
      float
  in
  let v0 =
    conv
      (fun (level, {time_stamp; section; event}) ->
        ( (if advertise_levels then Some level else None),
          hostname,
          time_stamp,
          section,
          event ))
      (fun (level, _, time_stamp, section, event) ->
        ( Option.value ~default:Internal_event.Debug level,
          {time_stamp; section; event} ))
      (obj5
         (opt "level" Internal_event.Level.encoding)
         (req "hostname" string)
         (req "time_stamp" ptime_encoding)
         (req "section" Internal_event.Section.encoding)
         (req "event" event_encoding))
  in
  With_version.(encoding ~name:"fd-sink-item" (first_version v0))

let make_with_pp_rfc5424 ~advertise_levels ~level pp wrapped_event name =
  (* See https://tools.ietf.org/html/rfc5424#section-6 *)
  let priority_str =
    if advertise_levels then
      (* See https://datatracker.ietf.org/doc/html/rfc5424#section-6.2.1 *)
      Format.sprintf
        "<00%d> "
        (match level with
        | Internal_event.Debug -> 7
        | Info -> 6
        | Notice -> 5
        | Warning -> 4
        | Error -> 3
        | Fatal -> 0)
    else ""
  in
  Format.asprintf
    "%s%a [%a.%s] %a\n"
    priority_str
    (Ptime.pp_rfc3339 ~frac_s:3 ())
    wrapped_event.time_stamp
    Internal_event.Section.pp
    wrapped_event.section
    name
    (pp ~all_fields:false ~block:false)
    wrapped_event.event

type color_setting = Enabled of string option | Disabled

let make_with_pp_short ?cols ~color ~tag_color ~advertise_levels ~level pp
    wrapped_event =
  let string_of_level = function
    | Internal_event.Fatal -> "FATAL"
    | Debug -> "DEBUG"
    | Info -> "INFO"
    | Notice -> "NOTICE"
    | Warning -> "WARN"
    | Error -> "ERROR"
  in
  let level_string, visible_level_size =
    if advertise_levels then
      let color, reset =
        match tag_color with Some c -> (c, Color.reset) | None -> ("", "")
      in
      let l_str = string_of_level level in
      let level_width =
        8
        (* NOTICE + 2 spaces *)
      in
      let tab = String.make (level_width - String.length l_str - 1) ' ' in
      (String.concat "" [tab; color; l_str; reset; " "], level_width)
    else ("", 0)
  in
  let timestamp =
    let time = Ptime.to_float_s wrapped_event.time_stamp in
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
      | 11 -> "Dec"
      | _ -> assert false
      (* `tm` is built locally, so it should contain invalid month code *)
    in
    let ms = (time -. floor time) *. 1000. |> floor in
    Format.sprintf
      "%s %02d %02d:%02d:%02d.%03.0f"
      month_string
      tm.Unix.tm_mday
      tm.Unix.tm_hour
      tm.Unix.tm_min
      tm.Unix.tm_sec
      ms
  in
  let timestamp_size = String.length timestamp in
  let separator = if advertise_levels then "│ " else ": " in
  let prefix = String.concat "" [timestamp; level_string; separator] in
  let visible_prefix_size = timestamp_size + visible_level_size + 2 in
  let prefix_size = String.length prefix in
  let available_cols =
    Option.map (fun cols -> max 1 (cols - visible_prefix_size)) cols
  in
  let lines =
    String.split_on_char
      '\n'
      (Format.asprintf
         "%a"
         (fun ppf ->
           Option.iter (Format.pp_set_margin ppf) available_cols ;
           pp ~all_fields:false ~block:true ppf)
         wrapped_event.event)
  in
  let color_total_size, bold_total_size =
    match color with
    | Enabled color_opt ->
        let color_total_size =
          if Option.is_some color_opt then Color.(color_len + reset_len) else 0
        in
        let bold_total_size i =
          if i = 0 then Color.(bold_len + reset_len) else 0
        in
        (color_total_size, bold_total_size)
    | Disabled -> (0, fun _i -> 0)
  in

  let lines_size =
    List.fold_left_i
      (fun i acc s ->
        (* computing the total length of a line *)
        acc + prefix_size + String.length s + 1 + color_total_size
        + bold_total_size i)
      0
      lines
  in
  let buf = Bytes.create lines_size in
  let prev_pos = ref 0 in
  let blit s len =
    Bytes.blit_string s 0 buf !prev_pos len ;
    prev_pos := !prev_pos + len
  in
  let enable_color, color_tag_opt =
    match color with
    | Enabled color_tag -> (true, color_tag)
    | Disabled -> (false, None)
  in
  let () =
    List.iteri
      (fun i s ->
        let bold_first_header = enable_color && i = 0 in
        let s_len = String.length s in
        if bold_first_header then blit Color.bold Color.bold_len ;
        blit prefix prefix_size ;
        if bold_first_header then blit Color.reset Color.reset_len ;
        Option.iter (fun tag -> blit tag Color.color_len) color_tag_opt ;
        blit s s_len ;
        if Option.is_some color_tag_opt then blit Color.reset Color.reset_len ;
        blit "\n" 1)
      lines
  in
  Bytes.unsafe_to_string buf

let%expect_test _ =
  let pp_string ~all_fields:_ ~block:_ = Format.pp_print_text in
  let make_timestamp flt =
    match Ptime.of_float_s flt with None -> assert false | Some v -> v
  in
  let ts = 1682584149.77736807 in
  let local_dependant_ts = fst @@ Unix.mktime @@ Unix.gmtime ts in
  let time_stamp = make_timestamp local_dependant_ts in
  let ev =
    {
      event = "toto";
      time_stamp;
      section = Internal_event.Section.make_sanitized ["my"; "section"];
    }
  in
  print_endline
    (make_with_pp_short
       ~color:Disabled
       ~tag_color:None
       ~advertise_levels:false
       ~level:Debug
       pp_string
       ev) ;
  [%expect {| Apr 27 08:29:09.000: toto |}] ;
  let ev_line_cut =
    {ev with event = "totototototototototototototototo before_cut after_cut"}
  in
  print_endline
    (String.escaped
    @@ make_with_pp_short
         ~color:(Enabled None)
         ~tag_color:None
         ~advertise_levels:false
         ~level:Debug
         pp_string
         ev_line_cut) ;
  [%expect
    {|
       \027[1mApr 27 08:29:09.000: \027[0mtotototototototototototototototo before_cut\nApr 27 08:29:09.000: after_cut\n|}] ;
  print_endline
    (String.escaped
    @@ make_with_pp_short
         ~color:(Enabled None)
         ~tag_color:None
         ~advertise_levels:false
         ~level:Debug
         pp_string
         ev) ;
  [%expect {| \027[1mApr 27 08:29:09.000: \027[0mtoto\n|}] ;
  print_endline
    (String.escaped
    @@ make_with_pp_short
         ~color:(Enabled (Some Color.FG.red))
         ~tag_color:None
         ~advertise_levels:false
         ~level:Debug
         pp_string
         ev) ;
  [%expect {| \027[1mApr 27 08:29:09.000: \027[0m\027[31mtoto\027[0m\n|}] ;
  let ev = {ev with time_stamp = make_timestamp ts} in
  print_endline
    (make_with_pp_rfc5424
       ~advertise_levels:false
       ~level:Debug
       pp_string
       ev
       "my-event") ;
  [%expect {| 2023-04-27T08:29:09.777-00:00 [my.section.my-event] toto |}] ;
  ()

let make_for_syslog pp wrapped_event =
  (* Syslog is handling the formating. Only the message is printed. *)
  Format.asprintf "%a" (pp ~all_fields:false ~block:false) wrapped_event.event

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

let%expect_test _ =
  print_endline (Bool.to_string (check_file_format_with_date ".out" "a.out")) ;
  [%expect {| false |}] ;
  print_endline
    (Bool.to_string
       (check_file_format_with_date "some-name.log" "some-name-19991231.log")) ;
  [%expect {| true |}] ;
  print_endline
    (Bool.to_string (check_file_format_with_date "hello." "hello-19991231.")) ;
  [%expect {| true |}] ;
  print_endline
    (Bool.to_string (check_file_format_with_date ".log" "19991231.log")) ;
  [%expect {| false |}] ;
  print_endline
    (Bool.to_string
       (check_file_format_with_date ".log.log" ".log-19991231.log")) ;
  [%expect {| true |}] ;
  print_endline
    (Bool.to_string (check_file_format_with_date "file" "file-19991231")) ;
  [%expect {| true |}] ;
  ()

let filename_insert_before_ext ~path s =
  let ext = Filename.extension path in
  let chopped = if ext = "" then path else Filename.chop_extension path in
  Format.asprintf "%s-%s%s" chopped s ext

let%expect_test _ =
  print_endline (filename_insert_before_ext ~path:"foo.bar" "baz") ;
  [%expect {| foo-baz.bar |}] ;
  print_endline (filename_insert_before_ext ~path:"/tmp/log.out" "11") ;
  [%expect {| /tmp/log-11.out |}] ;
  print_endline (filename_insert_before_ext ~path:"/dev/null" "XXX") ;
  [%expect {| /dev/null-XXX |}] ;
  ()

let overwrite_syslog_tag sys_logger section =
  Syslog.
    {
      sys_logger with
      tag = Format.asprintf "%a" Internal_event.Section.pp section;
    }

module Make_sink (K : sig
  val kind : [`Path | `Stdout | `Stderr | `Syslog]
end) : Internal_event.SINK with type t = t = struct
  type nonrec t = t

  let uri_scheme =
    match K.kind with
    | `Path -> "file-descriptor-path"
    | `Stdout -> "file-descriptor-stdout"
    | `Stderr -> "file-descriptor-stderr"
    | `Syslog -> "file-descriptor-syslog"

  let fail_parsing uri fmt =
    Format.kasprintf (failwith "Parsing URI: %s: %s" (Uri.to_string uri)) fmt

  let configure uri =
    let flag name =
      match Uri.get_query_param uri name with Some "true" -> true | _ -> false
    in
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
          match Internal_event.Level.of_string_exn l with
          | Some l -> return (`Level_at_least l)
          | None | (exception Internal_event.Level.Not_a_level _) ->
              fail_parsing uri "Wrong level: %S" l)
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
                      match
                        Internal_event.Level.of_string_exn
                          (String.lowercase_ascii two)
                      with
                      | level -> level
                      | exception Internal_event.Level.Not_a_level _ ->
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
                  match Internal_event.Level.of_string_exn lvl with
                  | level ->
                      (* establish default for all sections *)
                      pairs @ [(Internal_event.Section.empty, level)]
                  | exception Internal_event.Level.Not_a_level _ ->
                      Format.kasprintf
                        Stdlib.failwith
                        "Wrong level name %S in level-at-least argument"
                        lvl)
            in
            return (`Per_section_prefix sections)
          with Failure s -> fail_parsing uri "%s" s)
    in
    let* advertise_levels =
      match Uri.get_query_param uri "advertise-levels" with
      | Some "false" | None -> return false
      | Some "true" -> return true
      | Some other -> fail_parsing uri "Expected bool, got %S" other
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
    let colors = flag "colors" in
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
                     [O_WRONLY; O_CREAT; O_CLOEXEC]
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
      | `Syslog ->
          let* facility =
            match Uri.get_query_param uri "facility" with
            | None -> return Syslog.User
            | Some facility -> (
                match Syslog.facility_of_string_opt facility with
                | None -> fail_parsing uri "Invalid syslog facility."
                | Some f -> return f)
          in
          let path =
            match Uri.path uri with "" | "/" -> None | path -> Some path
          in
          (* Syslog tag correspond to the event section, so it is added
             afterwards *)
          let*! logger = Syslog.create ?path ~tag:"octez" facility in
          return (Syslog logger)
      | `Stdout -> return (Static Lwt_unix.stdout)
      | `Stderr -> return (Static Lwt_unix.stderr)
    in
    let t = {output; filter; format; colors; advertise_levels} in
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
    let* () =
      Lwt_mutex.with_lock write_mutex (fun () ->
          let {day; fd} = !current in
          let today = Ptime.to_date now in
          let should_rotate_output = day <> today in
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
                         let flags = [O_WRONLY; O_CREAT; O_APPEND; O_CLOEXEC] in
                         openfile path flags rights))
              in
              current := {fd; day = today} ;
              return fd
          in
          let*! () = Lwt_utils_unix.write_string output to_write in
          let*! () =
            if should_rotate_output then
              remove_older_files
                (Filename.dirname base_path)
                days_kept
                base_path
            else Lwt.return_unit
          in
          return_unit)
    in
    return_unit

  let output_one now output section level to_write =
    protect @@ fun () ->
    match output with
    | Static output ->
        Lwt_result.ok
        @@ Lwt_mutex.with_lock write_mutex (fun () ->
               Lwt_utils_unix.write_string output to_write)
    | Syslog sys_logger ->
        Lwt_result.ok
        @@ Lwt_mutex.with_lock write_mutex (fun () ->
               Syslog.syslog
                 ~timestamp:(Ptime.to_float_s now)
                 (overwrite_syslog_tag sys_logger section)
                 level
                 to_write)
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
              Internal_event.Section.is_prefix
                ~prefix
                (Internal_event.Section.append section M.simple_name))
            kvl
        with
        | None ->
            (* default *)
            Internal_event.Level.compare M.level Internal_event.Level.default
            >= 0
        | Some (_, None) -> (* exclude list *) false
        | Some (_, Some lvl) -> Internal_event.Level.compare M.level lvl >= 0)

  let color = function
    | Internal_event.Blue -> Some Color.FG.blue
    | Internal_event.Cyan -> Some Color.FG.cyan
    | Internal_event.Green -> Some Color.FG.green
    | Internal_event.Magenta -> Some Color.FG.magenta

  let level_color = function
    | Internal_event.Warning -> Some Color.FG.yellow
    | Error | Fatal -> Some Color.FG.red
    | Info | Notice | Debug -> None

  let level_tag_color = function
    | Internal_event.Warning -> Color.FG.yellow
    | Error -> Color.FG.red
    | Fatal -> Color.Inverse.red
    | Info -> Color.FG.cyan
    | Notice -> Color.FG.blue
    | Debug -> Color.FG.magenta

  let output_color_compatible out =
    let open Lwt_syntax in
    match out with
    | Static fd ->
        let* is_a_tty = Lwt_unix.isatty fd in
        return (is_a_tty && Sys.getenv_opt "TERM" <> Some "dumb")
    | Syslog _ | Rotating _ -> return_false

  let output_columns out =
    let open Lwt_syntax in
    match out with
    | Static fd ->
        let* is_a_tty = Lwt_unix.isatty fd in
        if is_a_tty then return (Terminal.Size.get_columns ()) else return_none
    | Syslog _ | Rotating _ -> return_none

  let handle (type a) {output; format; colors; advertise_levels; _} m
      ?(section = Internal_event.Section.empty) (event : a) =
    let open Lwt_syntax in
    let module M = (val m : Internal_event.EVENT_DEFINITION with type t = a) in
    let now = Ptime_clock.now () in
    let wrapped_event = wrap now section event in
    let* to_write =
      if is_syslog output then Lwt.return @@ make_for_syslog M.pp wrapped_event
      else
        let json () =
          Data_encoding.Json.construct
            (wrapped_encoding ~advertise_levels M.encoding)
            (M.level, wrapped_event)
        in
        match format with
        | `Pp_RFC5424 ->
            return
            @@ make_with_pp_rfc5424
                 ~advertise_levels
                 ~level:M.level
                 M.pp
                 wrapped_event
                 M.name
        | `Pp_short ->
            let* color, tag_color =
              if colors then
                let+ color_compatible = output_color_compatible output in
                if color_compatible then
                  let color =
                    match M.alternative_color with
                    | None -> Enabled (level_color M.level)
                    | Some c -> Enabled (color c)
                  in
                  (color, Some (level_tag_color M.level))
                else (Disabled, None)
              else return (Disabled, None)
            in
            let+ cols = output_columns output in
            make_with_pp_short
              ?cols
              ~color
              ~tag_color
              ~advertise_levels
              ~level:M.level
              M.pp
              wrapped_event
        | `One_per_line ->
            return @@ Ezjsonm.value_to_string ~minify:true (json ()) ^ "\n"
        | `Netstring ->
            let str = Ezjsonm.value_to_string ~minify:true (json ()) in
            return @@ Printf.sprintf "%d:%s," (String.length str) str
    in
    let* r = output_one now output section M.level to_write in
    match r with
    | Error [Exn (Unix.Unix_error (Unix.EBADF, _, _))] ->
        (* The file descriptor was closed before the event arrived,
           ignore it. *)
        return_ok_unit
    | Error _ as err -> return err
    | Ok () -> return_ok_unit

  let close {output; _} =
    let open Lwt_result_syntax in
    match K.kind with
    | `Path | `Syslog -> (
        match output with
        | Syslog sys_logger ->
            protect (fun () -> Lwt_result.ok @@ Syslog.close sys_logger)
        | Rotating output -> Lwt_utils_unix.safe_close !(output.current).fd
        | Static output -> Lwt_utils_unix.safe_close output)
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

module Sink_implementation_syslog = Make_sink (struct
  let kind = `Syslog
end)

let () = Internal_event.All_sinks.register (module Sink_implementation_path)

let () = Internal_event.All_sinks.register (module Sink_implementation_stdout)

let () = Internal_event.All_sinks.register (module Sink_implementation_stderr)

let () = Internal_event.All_sinks.register (module Sink_implementation_syslog)
