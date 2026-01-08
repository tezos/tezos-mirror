(******************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

module Micro_seconds : sig
  (** Module with time-stamps with “at least micro-seconds” precision. *)
  type t = private float

  val now : unit -> t

  val of_float : float -> t

  val encoding : t Data_encoding.t

  val date_string : t -> string * string
end = struct
  (* Time.t is in seconds, we want more precision. *)
  type t = float

  let now () = Unix.gettimeofday ()

  let of_float f = f

  let encoding =
    let open Data_encoding in
    conv (* Cf. https://github.com/OCamlPro/ocplib-json-typed/issues/25 *)
      (fun f -> f *. 1_000_000. |> Int64.of_float)
      (fun i64 -> Int64.to_float i64 /. 1_000_000.)
      int64

  let date_string time_value =
    let open Unix in
    let open Printf in
    let tm = gmtime time_value in
    ( sprintf "%04d%02d%02d" (1900 + tm.tm_year) (tm.tm_mon + 1) tm.tm_mday,
      sprintf
        "%02d%02d%02d-%06d"
        tm.tm_hour
        tm.tm_min
        tm.tm_sec
        ((time_value -. floor time_value) *. 1_000_000. |> int_of_float) )
end

module Event_filter = struct
  type t =
    | True
    | False
    | Or of t list
    | And of t list
    | Name of string
    | Name_matches of Re.re
    | Level_in of Internal_event.level list
    | Section_in of Internal_event.Section.t list

  let rec run ~section ~level ~name filter =
    let continue = run ~section ~level ~name in
    match filter with
    | True -> true
    | False -> false
    | Or l -> List.exists continue l
    | And l -> List.for_all continue l
    | Name s -> String.equal s name
    | Name_matches re -> Re.execp re name
    | Level_in l -> List.mem ~equal:Internal_event.Level.equal level l
    | Section_in l -> List.mem ~equal:Internal_event.Section.equal section l

  let rec pp fmt filter =
    let open Format in
    match filter with
    | True -> pp_print_string fmt "true"
    | False -> pp_print_string fmt "false"
    | Or l ->
        fprintf
          fmt
          "(or@ @[<2>%a@]"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ ") pp)
          l
    | And l ->
        fprintf
          fmt
          "(and@ @[<2>%a@]"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ ") pp)
          l
    | Name s -> fprintf fmt "(name-is@ %S)" s
    | Name_matches re -> fprintf fmt "(name-matches@ %a)" Re.pp_re re
    | Level_in l ->
        fprintf
          fmt
          "(level-in@ [%s])"
          (String.concat "," (List.map Internal_event.Level.to_string l))
    | Section_in l ->
        fprintf
          fmt
          "(section-in@ [%a])"
          (pp_print_list
             ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
             (fun fmt s -> fprintf fmt "(Some %a)" Internal_event.Section.pp s))
          l
  [@@warning "-32"]

  (* -> The "unused value" warning. *)

  let t = True

  let f = False [@@warning "-32"]

  (* -> The "unused value" warning. *)

  let any l = Or l

  let all l = And l [@@warning "-32"]

  (* -> The "unused value" warning. *)

  let name_is s = Name s

  let name_matches s = Name_matches s

  let name_matches_posix s = name_matches (Re.Posix.compile_pat s)

  let level_in l = Level_in l

  let section_in l = Section_in l

  let levels_in_order =
    Internal_event.[Debug; Info; Notice; Warning; Error; Fatal]

  let level_at_least lvl =
    List.fold_left
      (fun acc l ->
        match acc with
        | [] -> if l = lvl then [l] else []
        | _ :: _ as acc -> l :: acc)
      []
      levels_in_order
    |> function
    | [] -> raise (Failure "level_at_least not found")
    | _ :: _ as levels -> level_in levels
end

type t = {path : string; event_filter : Event_filter.t}

type 'event wrapped = {
  time_stamp : Micro_seconds.t;
  section : Internal_event.Section.t;
  event : 'event;
}

let wrap time_stamp section event = {time_stamp; section; event}

let wrapped_encoding event_encoding =
  let open Data_encoding in
  let v0 =
    conv
      (fun {time_stamp; section; event} -> (time_stamp, section, event))
      (fun (time_stamp, section, event) -> {time_stamp; section; event})
      (obj3
         (req "time_stamp" Micro_seconds.encoding)
         (req "section" Internal_event.Section.encoding)
         (req "event" event_encoding))
  in
  With_version.(encoding ~name:"file-event-sink-item" (first_version v0))

module Section_dir = struct
  let of_section (section : Internal_event.Section.t) =
    Format.asprintf "%a" Internal_event.Section.pp section

  let section_name = function
    | "no-section" -> Ok None
    | other -> (
        match TzString.remove_prefix ~prefix:"section-" other with
        | None -> Error "wrong-dir-name"
        | Some s -> Ok (Some s))
end

module Sink_implementation : Internal_event.SINK with type t = t = struct
  type nonrec t = t

  let uri_scheme = "unix-files"

  let configure uri =
    let event_filter =
      let name_res =
        Uri.get_query_param' uri "name-matches" |> Option.value ~default:[]
      in
      let names = Uri.get_query_param' uri "name" |> Option.value ~default:[] in
      let level_o =
        let open Option_syntax in
        let* lal = Uri.get_query_param uri "level-at-least" in
        (* This should not raise an exception because the URI has been
           created after parsing the rules and an exception would have been
           raised earlier *)
        let* lal = Internal_event.Level.of_string_exn lal in
        return (Event_filter.level_at_least lal)
      in
      let levels = Option.to_list level_o in
      let sections =
        let somes =
          Uri.get_query_param' uri "section"
          |> Option.fold
               ~none:[]
               ~some:
                 (List.map (fun s ->
                      Internal_event.Section.make_sanitized
                        (String.split_on_char '.' s)))
        in
        let none =
          match Uri.get_query_param uri "no-section" with
          | Some "true" -> [Internal_event.Section.empty]
          | _ -> []
        in
        match somes @ none with
        | [] -> []
        | more -> [Event_filter.section_in more]
      in
      Event_filter.(
        match
          levels @ sections
          @ List.map name_matches_posix name_res
          @ List.map name_is names
        with
        | [] -> t
        | more -> any more)
    in
    let t = {path = Uri.path uri; event_filter} in
    Lwt.return_ok t

  let output_json ~pp file_path event_json =
    let open Lwt_syntax in
    Lwt.catch
      (fun () ->
        let* () =
          Lwt_utils_unix.create_dir ~perm:0o700 (Filename.dirname file_path)
        in
        let* ru = Lwt_utils_unix.Json.write_file file_path event_json in
        match ru with
        | Ok () -> return_ok_unit
        | Error el ->
            failwith
              "ERROR while Handling %a,@ cannot write JSON to %s:@ %a\n%!"
              pp
              ()
              file_path
              Error_monad.pp_print_trace
              el)
      (function
        | e ->
        failwith "ERROR while Handling %a: %s\n%!" pp () (Printexc.to_string e))

  let should_handle (type a) ?(section = Internal_event.Section.empty)
      {event_filter; _} m =
    let module M = (val m : Internal_event.EVENT_DEFINITION with type t = a) in
    Event_filter.run ~section ~level:M.level ~name:M.name event_filter

  let handle (type a) {path; _} m ?(section = Internal_event.Section.empty)
      (event : a) =
    let open Lwt_result_syntax in
    let module M = (val m : Internal_event.EVENT_DEFINITION with type t = a) in
    let now = Micro_seconds.now () in
    let date, time = Micro_seconds.date_string now in
    let event_json =
      Data_encoding.Json.construct
        (wrapped_encoding M.encoding)
        (wrap now section event)
    in
    let tag =
      let hash =
        Marshal.to_string event_json [] |> Digest.string |> Digest.to_hex
      in
      String.sub hash 0 8
    in
    let section_dir = Section_dir.of_section section in
    let dir_path =
      List.fold_left Filename.concat path [section_dir; M.name; date; time]
    in
    let file_path =
      Filename.concat dir_path (Printf.sprintf "%s_%s_%s.json" date time tag)
    in
    let* () =
      output_json file_path event_json ~pp:(fun fmt () ->
          M.pp ~block:true ~all_fields:true fmt event)
    in
    return_unit

  let close _ = Lwt_result_syntax.return_unit
end

let () = Internal_event.All_sinks.register (module Sink_implementation)

open Sink_implementation

module Query = struct
  let with_file_kind dir p =
    let open Lwt_result_syntax in
    let* kind =
      protect (fun () ->
          let* {Lwt_unix.st_kind; _} =
            Lwt_result.ok @@ Lwt_unix.stat (Filename.concat dir p)
          in
          return st_kind)
    in
    match kind with
    | Unix.S_DIR -> return (`Directory p)
    | Unix.S_REG -> return (`Regular_file p)
    | (Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO | Unix.S_SOCK) as k ->
        return (`Special (k, p))

  let fold_directory path ~init ~f =
    let open Lwt_result_syntax in
    let* dirhandle =
      protect (fun () -> Lwt_result.ok @@ Lwt_unix.opendir path)
    in
    let rec iter prev =
      let* opt =
        protect (fun () ->
            Lwt.catch
              (fun () ->
                let* d = Lwt_result.ok @@ Lwt_unix.readdir dirhandle in
                let* wk = with_file_kind path d in
                return_some wk)
              (function
                | End_of_file ->
                    let* () = Lwt_result.ok @@ Lwt_unix.closedir dirhandle in
                    return_none
                | (e : exn) ->
                    failwith
                      "ERROR while folding %s: %s"
                      path
                      (Printexc.to_string e)))
      in
      let* p = prev in
      match opt with Some more -> iter (f p more) | None -> prev
    in
    iter init

  open TzFilename.Infix

  module Time_constraint = struct
    type op = [`Lt | `Le | `Ge | `Gt]

    type t =
      [ `Date of op * float
      | `Time of op * float
      | `And of t * t
      | `Or of t * t
      | `All ]

    let rec check_logic check_terminal (t : t) string =
      let continue = check_logic check_terminal in
      match t with
      | `All -> true
      | `And (a, b) -> continue a string && continue b string
      | `Or (a, b) -> continue a string || continue b string
      | (`Date _ | `Time _) as term -> check_terminal term

    let op_with_string = function
      | `Lt -> fun a b -> String.compare a b > 0
      | `Gt -> fun a b -> String.compare a b < 0
      | `Le -> fun a b -> String.compare a b >= 0
      | `Ge -> fun a b -> String.compare a b <= 0

    let check_date (t : t) date_string =
      check_logic
        (function
          | `Date (op, f) ->
              let s = Micro_seconds.(date_string (of_float f) |> fst) in
              op_with_string op s date_string
          | `Time _ -> true)
        t
        date_string

    let check_time (t : t) string =
      check_logic
        (function
          | `Time (op, f) ->
              let s = Micro_seconds.(date_string (of_float f) |> snd) in
              op_with_string op s string
          | `Date _ -> true)
        t
        Micro_seconds.date_string
  end

  module Report = struct
    type item =
      [ `Error of
        [ `Parsing_event of
          [`Encoding of string * exn | `Json of string * error list]
        | `Cannot_recognize_section of string ]
      | `Warning of
        [ `Expecting_regular_file_at of string
        | `Expecting_directory_at of string
        | `Unknown_event_name_at of string * string ] ]

    let pp fmt (x : item) =
      let open Format in
      let error fmt = function
        | `Parsing_event e -> (
            match e with
            | `Encoding (path, exn) ->
                fprintf
                  fmt
                  "@[Parse error:@ wrong encoding for %S: %s@]"
                  path
                  (Printexc.to_string exn)
            | `Json (path, el) ->
                fprintf
                  fmt
                  "@[Parse error:@ wrong JSON for %S: %a@]"
                  path
                  pp_print_trace
                  el)
        | `Cannot_recognize_section sec ->
            fprintf
              fmt
              "@[Directory error:@ cannot recognize section directory@ %S@]"
              sec
      in
      let warning fmt = function
        | `Expecting_regular_file_at path ->
            fprintf fmt "%S@ is not a regular file" path
        | `Expecting_directory_at path ->
            fprintf fmt "%S@ is not a directory" path
        | `Unknown_event_name_at (name, path) ->
            fprintf fmt "Unknown event name@ %S@ at@ %S" name path
      in
      match x with
      | `Error e -> fprintf fmt "@[Error:@ %a@]" error e
      | `Warning e -> fprintf fmt "@[Warning:@ %a@]" warning e

    let make_return m ((prev : item list), value) warning =
      Lwt.return_ok (m warning :: prev, value)

    let return_with_warning v e = make_return (fun e -> `Warning e) v e

    let return_with_error v e = make_return (fun e -> `Error e) v e
  end

  open Report

  let fold_event_kind_directory ~time_query path ~init ~f =
    let open Lwt_result_syntax in
    fold_directory path ~init:(return init) ~f:(fun previous -> function
      | `Directory "." | `Directory ".." -> return previous
      | `Directory date when Time_constraint.check_date time_query date ->
          fold_directory
            (path // date)
            ~init:(return previous)
            ~f:(fun previous -> function
            | `Directory "." | `Directory ".." -> return previous
            | `Directory time when Time_constraint.check_time time_query time ->
                fold_directory
                  (path // date // time)
                  ~init:(return previous)
                  ~f:(fun previous -> function
                    | `Directory "." | `Directory ".." -> return previous
                    | `Regular_file file ->
                        f previous (path // date // time // file)
                    | `Directory p | `Special (_, p) ->
                        return_with_warning
                          previous
                          (`Expecting_regular_file_at (path // date // time // p)))
            | `Directory _ (* filtered out *) -> return previous
            | `Regular_file p | `Special (_, p) ->
                return_with_warning
                  previous
                  (`Expecting_directory_at (path // date // p)))
      | `Directory _ (* filtered out *) -> return previous
      | `Regular_file p | `Special (_, p) ->
          return_with_warning previous (`Expecting_directory_at (path // p)))

  let handle_event_kind_directory (type a) ~time_query ~section_path ~init ~f ev
      =
    let module Event =
      (val ev : Internal_event.EVENT_DEFINITION with type t = a)
    in
    let handle_event_file previous path =
      let open Lwt_result_syntax in
      let*! r = Lwt_utils_unix.Json.read_file path in
      match r with
      | Ok json -> (
          try
            let {time_stamp; event; _} =
              Data_encoding.Json.destruct (wrapped_encoding Event.encoding) json
            in
            let* user_return =
              f
                (snd previous)
                ~time_stamp:(time_stamp :> float)
                (Internal_event.Generic.Event (Event.name, ev, event))
            in
            return (fst previous, user_return)
          with e ->
            return_with_error previous (`Parsing_event (`Encoding (path, e))))
      | Error el ->
          return_with_error previous (`Parsing_event (`Json (path, el)))
    in
    fold_event_kind_directory
      ~time_query
      (section_path // Event.name)
      ~init
      ~f:(fun prev file -> handle_event_file prev file)

  let fold ?on_unknown ?only_sections ?only_names ?(time_query = `All) uri ~init
      ~f =
    let open Lwt_result_syntax in
    let name_matches =
      match only_names with
      | None -> fun _ -> true
      | Some l -> fun name -> List.mem ~equal:String.equal name l
    in
    let section_matches =
      match only_sections with
      | None -> fun _ -> true
      | Some l -> fun name -> List.mem ~equal:(Option.equal String.equal) name l
    in
    let* {path = sink_path; _} = configure uri in
    fold_directory
      sink_path
      ~init:(return ([], init))
      ~f:(fun previous -> function
        | `Directory ("." | "..") -> return previous
        | `Directory dir -> (
            match Section_dir.section_name dir with
            | Ok sec when section_matches sec ->
                fold_directory
                  (sink_path // dir)
                  ~init:(return ([], init))
                  ~f:(fun previous -> function
                    | `Directory ("." | "..") -> return previous
                    | `Directory event_name when name_matches event_name -> (
                        let open Internal_event in
                        match All_definitions.find (( = ) event_name) with
                        | Some (Generic.Definition (_, _, ev)) ->
                            handle_event_kind_directory
                              ~time_query
                              ev
                              ~section_path:(sink_path // dir)
                              ~init:previous
                              ~f
                        | None -> (
                            match on_unknown with
                            | None ->
                                return_with_warning
                                  previous
                                  (`Unknown_event_name_at
                                     (event_name, sink_path // dir))
                            | Some f ->
                                fold_event_kind_directory
                                  ~time_query
                                  (sink_path // dir // event_name)
                                  ~init:previous
                                  ~f:(fun prev file ->
                                    let* () = f file in
                                    return prev)))
                    | `Directory _ (* filtered out *) -> return previous
                    | `Regular_file p | `Special (_, p) ->
                        return_with_warning
                          previous
                          (`Expecting_directory_at (sink_path // p)))
            | Ok _ (* section does not match *) -> return previous
            | Error _ ->
                return_with_error previous (`Cannot_recognize_section dir))
        | `Regular_file p | `Special (_, p) ->
            return_with_warning
              previous
              (`Expecting_directory_at (sink_path // p)))
end
