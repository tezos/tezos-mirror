(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

type filter = Internal_event.Section.t option

type t = unit

type event = {
  level : Internal_event.Level.t;
  section : Internal_event.Section.t option;
  name : string;
  message : string;
  json : Data_encoding.json;
}

let pp_event ?(short = false) fmt e =
  Format.fprintf
    fmt
    "Event[%s](%a:%s)"
    (Internal_event.Level.to_string e.level)
    Format.(
      pp_print_option
      @@ pp_print_list
           ~pp_sep:(fun fmt () -> pp_print_string fmt ":")
           pp_print_string)
    (Option.map Internal_event.Section.to_string_list e.section)
    e.name ;
  if not short then Format.fprintf fmt " %a" Data_encoding.Json.pp e.json

module Pattern = struct
  type t = {
    level : Internal_event.level option;
    section : Internal_event.Section.t option option;
    name : string;
  }

  let pp fmt pattern =
    Format.fprintf
      fmt
      "Pattern[%a](%a:%s)"
      Format.(pp_print_option pp_print_string)
      (Option.map Internal_event.Level.to_string pattern.level)
      Format.(
        pp_print_option ~none:(fun fmt () -> pp_print_string fmt "<ANY>")
        @@ pp_print_option
        @@ pp_print_list
             ~pp_sep:(fun fmt () -> pp_print_string fmt ":")
             pp_print_string)
      (Option.map
         (Option.map Internal_event.Section.to_string_list)
         pattern.section)
      pattern.name

  let is_unspecified_or_equal_to pattern value =
    Option.fold ~none:true ~some:(( = ) value) pattern

  let match_event (pattern : t) (event : event) =
    is_unspecified_or_equal_to pattern.level event.level
    && is_unspecified_or_equal_to pattern.section event.section
    && pattern.name = event.name

  let assert_event pattern event =
    let msg =
      Format.asprintf
        "event %a matches pattern %a"
        (pp_event ~short:true)
        event
        pp
        pattern
    in
    Alcotest.(check bool) msg (match_event pattern event) true
end

type pattern = Pattern.t

let recorded_events : event list ref = ref []

let activated : bool ref = ref false

let uri_scheme = "mock-log"

let configure _ =
  activated := true ;
  Lwt_result_syntax.return_unit

let is_activated () = !activated

let close (_ : t) : unit tzresult Lwt.t = Lwt_result_syntax.return_unit

let should_handle ?section:_ (_ : t) _m = true

let handle (type a) (_ : t) m ?section ev =
  let module M = (val m : Internal_event.EVENT_DEFINITION with type t = a) in
  let event =
    {
      level = M.level;
      section;
      name = M.name;
      message = Format.asprintf "%a" (M.pp ~all_fields:true ~block:true) ev;
      json = Data_encoding.Json.construct M.encoding ev;
    }
  in
  recorded_events := !recorded_events @ [event] ;
  Lwt_result_syntax.return_unit

(** testing stuff *)

(** [get_events filter] returns the list of recorded events, in chronological
    order. *)
let get_events ?filter () : event list =
  match filter with
  | None -> !recorded_events
  | Some f -> List.filter (fun {section; _} -> section = f) !recorded_events

let clear_events () : unit = recorded_events := []

(** Pretty prints the list of recorded events *)
let pp_state ?filter () =
  Format.printf "-- State of Mock_sink --\n" ;
  List.iteri
    (fun i {level = lvl; message = msg; section; _} ->
      let section_pp =
        match section with
        | None -> "[~]"
        | Some section -> Format.asprintf "%a" Internal_event.Section.pp section
      in
      let lvl_pp = Internal_event.Level.to_string lvl in
      Format.printf
        "Registered event{%d}:\n   %s (%s): \"%s\"\n"
        i
        section_pp
        lvl_pp
        msg)
    (get_events ?filter ()) ;
  Format.print_flush ()

let pp_events_json =
  Format.(
    pp_print_list
      ~pp_sep:(fun fmt () -> pp_print_string fmt ";\n")
      (fun fmt {json; _} -> Data_encoding.Json.pp fmt json))

let testable_level : Internal_event.Level.t Alcotest.testable =
  let level_pp fmt level =
    Format.pp_print_string fmt (Internal_event.Level.to_string level)
  in
  let level_eq l1 l2 = 0 == Internal_event.Level.compare l1 l2 in
  Alcotest.testable level_pp level_eq

let testable_section : Internal_event.Section.t Alcotest.testable =
  Alcotest.of_pp Internal_event.Section.pp

let testable_event : event Alcotest.testable =
  (module struct
    type t = event

    let equal (e1 : event) (e2 : event) =
      e1.level = e2.level && e1.section = e2.section && e1.name = e2.name
      && e1.json = e2.json

    let pp fmt (event : event) =
      Format.fprintf
        fmt
        "(%s, %a, %a)"
        (Internal_event.Level.to_string event.level)
        Format.(pp_print_list pp_print_string)
        (Option.fold
           ~some:Internal_event.Section.to_string_list
           ~none:[]
           event.section)
        Data_encoding.Json.pp
        event.json
  end : Alcotest.TESTABLE
    with type t = event)

let assert_has_events msg ?filter ?(strict = true) (pats : Pattern.t list) =
  let events = get_events ?filter () in
  if strict then
    match List.combine_with_leftovers pats events with
    | pes, None -> List.iter (fun (p, e) -> Pattern.assert_event p e) pes
    | _, Some (Either.Left pats) ->
        Alcotest.fail
          (Format.asprintf
             "Missing events in sink: %a"
             (Format.pp_print_list Pattern.pp)
             pats)
    | _, Some (Either.Right events) ->
        Alcotest.fail
          (Format.asprintf
             "Excess events in sink: %a"
             (Format.pp_print_list pp_event)
             events)
  else
    List.iter
      (fun pattern ->
        Assert.check_any ~msg (Pattern.match_event pattern) events)
      pats

let assert_has_event msg ?filter ?(strict = true) (pattern : Pattern.t) =
  let log = get_events ?filter () in
  if strict then assert_has_events msg ?filter ~strict [pattern]
  else Assert.check_any ~msg (Pattern.match_event pattern) log
