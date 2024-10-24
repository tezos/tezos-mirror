(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
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

open Profiler

let time () = {wall = Unix.gettimeofday (); cpu = Sys.time ()}

type stack_item = {
  id : string;
  time : time;
  report : report;
  verbosity : verbosity;
}

type stack = Toplevel of report | Cons of stack_item * stack

type scope = {id : string; verbosity : verbosity; time : time}

type state = {stack : stack; scopes : scope list; max_verbosity : verbosity}

let empty max_verbosity =
  {
    stack = Toplevel {aggregated = StringMap.empty; recorded = []};
    scopes = [];
    max_verbosity;
  }

let aggregate state verbosity id =
  {state with scopes = {id; verbosity; time = time ()} :: state.scopes}

let record state verbosity id =
  if state.scopes <> [] then aggregate state verbosity id
  else
    let stack =
      Cons
        ( {
            id;
            time = time ();
            report = {recorded = []; aggregated = StringMap.empty};
            verbosity;
          },
          state.stack )
    in
    {state with stack}

let rec merge
    {
      count = na;
      total = Span ta;
      children = contentsa;
      node_verbosity = verbosity_a;
    }
    {
      count = nb;
      total = Span tb;
      children = contentsb;
      node_verbosity = verbosity_b;
    } =
  {
    count = na + nb;
    total = Span (ta +* tb);
    children = merge_maps contentsa contentsb;
    node_verbosity = min verbosity_a verbosity_b;
  }

and merge_maps amap bmap =
  StringMap.merge
    (fun _ a b ->
      match (a, b) with
      | Some v, None | None, Some v -> Some v
      | None, None -> None
      | Some a, Some b -> Some (merge a b))
    amap
    bmap

let rec filter_verbosity_to_aggregated verbosity aggregated =
  StringMap.fold
    (fun id node acc ->
      let children = filter_verbosity_to_aggregated verbosity node.children in
      if node.node_verbosity <= verbosity then
        StringMap.add id {node with children} acc
      else merge_maps acc children)
    aggregated
    StringMap.empty

let rec aggregate_report {aggregated; recorded} =
  List.fold_left
    (fun acc (id, {start = _; duration; item_verbosity; contents}) ->
      let children = aggregate_report contents in
      let node =
        {count = 1; total = duration; node_verbosity = item_verbosity; children}
      in
      StringMap.add id node acc)
    aggregated
    recorded

let rec filter_verbosity verbosity {aggregated; recorded} =
  let aggregated = filter_verbosity_to_aggregated verbosity aggregated in
  let aggregated, recorded =
    List.fold_left
      (fun (aggregated, recorded) (id, item) ->
        let filtered_contents = filter_verbosity verbosity item.contents in
        if item.item_verbosity <= verbosity then
          ( aggregated,
            (id, {item with contents = filtered_contents}) :: recorded )
        else
          (merge_maps aggregated (aggregate_report filtered_contents), recorded))
      (aggregated, [])
      recorded
  in
  {aggregated; recorded = List.rev recorded}

let inc state report =
  let inc_report report preport =
    {
      aggregated = merge_maps report.aggregated preport.aggregated;
      recorded = List.rev_append report.recorded preport.recorded;
    }
  in
  let stack =
    match state.stack with
    | Toplevel preport -> Toplevel (inc_report report preport)
    | Cons (item, rest) ->
        Cons ({item with report = inc_report report item.report}, rest)
  in
  {state with stack}

let span state verbosity d ids =
  let tids =
    List.rev_append
      (List.map (fun {id; _} -> (id, zero_time)) state.scopes)
      (List.map (fun id -> (id, d)) ids)
  in
  match ids with
  | [] -> (* Shhh, everything will be alright. *) state
  | _ :: _ ->
      let rec build_node = function
        | [] -> StringMap.empty
        | (id, d) :: tids ->
            let children = build_node tids in
            let count, total = if tids = [] then (1, d) else (0, zero_time) in
            StringMap.singleton
              id
              {count; total; children; node_verbosity = verbosity}
      in
      inc state {recorded = []; aggregated = build_node tids}

let mark state verbosity ids = span state verbosity zero_time ids

let stop_aggregate state verbosity d id scopes =
  let tids =
    let s_scopes = List.map (fun {id; _} -> (id, d)) scopes in
    List.rev ((id, d) :: s_scopes)
  in
  let rec build_node = function
    | [] -> StringMap.empty
    | (id, d) :: tids ->
        let children = build_node tids in
        let count, total = if tids = [] then (1, d) else (0, zero_time) in
        StringMap.singleton
          id
          {count; total; children; node_verbosity = verbosity}
  in
  inc state {recorded = []; aggregated = build_node tids}

let stop state =
  match state.scopes with
  | {id; verbosity; time = t0} :: scopes ->
      let d = Span (time () -* t0) in
      let state = {state with scopes} in
      stop_aggregate state verbosity d id scopes
  | [] ->
      let stop_report id start contents report item_verbosity =
        let contents = {contents with recorded = List.rev contents.recorded} in
        let duration = Span (time () -* start) in
        let recorded =
          (id, {start; duration; contents; item_verbosity}) :: report.recorded
        in
        {report with recorded}
      in
      let stack =
        match state.stack with
        | Cons
            ( {id; time = start; report = contents; verbosity},
              Cons
                ({id = pid; time = pt0; report; verbosity = p_verbosity}, rest)
            ) ->
            Cons
              ( {
                  id = pid;
                  time = pt0;
                  report = stop_report id start contents report verbosity;
                  verbosity = p_verbosity;
                },
                rest )
        | Cons
            ({id; time = start; report = contents; verbosity}, Toplevel report)
          ->
            Toplevel (stop_report id start contents report verbosity)
        | Toplevel _ -> (* Shhh, everything will be alright. *) state.stack
      in
      {state with stack}

let stamp state verbosity id = stop (record state verbosity id)

let pp_delta_t ppf t =
  let t = int_of_float (t *. 1000000.) in
  let t =
    if t >= 0 then t
    else (
      Format.fprintf ppf "-" ;
      -t)
  in
  let mus = t mod 1000 and t = t / 1000 in
  let ms = t mod 1000 and t = t / 1000 in
  let s = t mod 60 and t = t / 60 in
  let m = t mod 60 and h = t / 60 in
  if h <> 0 then Format.fprintf ppf "%dh" h ;
  if m <> 0 || h <> 0 then Format.fprintf ppf "%dm" m ;
  if s <> 0 || m <> 0 || h <> 0 then Format.fprintf ppf "%ds" s ;
  Format.fprintf ppf "%d.%03dms" ms mus

let pp_line ?toplevel_timestamp nindent ppf id n t t0 =
  let indent = Stdlib.List.init nindent (fun _ -> "  ") in
  let () =
    Option.iter
      (fun t ->
        let time =
          WithExceptions.Option.get ~loc:__LOC__ (Ptime.of_float_s t.wall)
        in
        Format.fprintf ppf "%a@," Time.System.pp_hum time)
      toplevel_timestamp
  in
  let indentsym =
    String.concat
      ""
      (indent
      @ [
          id;
          " ......................................................";
          "......................................................";
          "......................................................";
        ])
  in
  Format.fprintf ppf "%s %-7i " (String.sub indentsym 0 80) n ;
  if t.wall = 0. then Format.fprintf ppf "                 "
  else
    Format.fprintf
      ppf
      "% 10.3fms %3d%%"
      (t.wall *. 1000.)
      (int_of_float (ceil (100. *. (t.cpu /. t.wall)))) ;
  match t0 with
  | None -> Format.fprintf ppf "@,"
  | Some t0 -> Format.fprintf ppf " +%a@," pp_delta_t t0.wall

let rec pp_report ?(toplevel_call = true) t0 nident ppf {aggregated; recorded} =
  StringMap.iter
    (fun id {count = n; total = Span d; children; node_verbosity = _} ->
      pp_line nident ppf id n d None ;
      pp_report
        ~toplevel_call:false
        t0
        (nident + 1)
        ppf
        {recorded = []; aggregated = children})
    aggregated ;
  List.iter
    (fun (id, {start = t; duration = Span d; contents; item_verbosity = _}) ->
      let toplevel_timestamp = if toplevel_call then Some t else None in
      pp_line ?toplevel_timestamp nident ppf id 1 d (Some (t -* t0)) ;
      pp_report ~toplevel_call:false t (nident + 1) ppf contents)
    recorded

let pp_report ?t0 ppf report =
  let t0 =
    match t0 with
    | Some t0 -> t0
    | None -> (
        match report.recorded with
        | (_, {start; _}) :: _ -> start
        | [] -> {wall = 0.; cpu = 0.})
  in
  Format.fprintf ppf "@[<v 0>%a@]" (pp_report t0 0) report

(** The [Base] functor helps to define other backend
    without having to write the same functions again and again.

    Given a way to get and set a [state] and an output function,
    the functor will produce a module implementing the [DRIVER]
    interface.

    Note that the produced module will try to [output] report every
    time [stamp], [inc], [mark], [span] or [stop] is used.
    *)
module Base (P : sig
  type t

  val get_state : t -> state

  val set_state : t -> state -> unit

  val output_report : (t -> report -> unit) option
end) =
struct
  let time _ = time ()

  let record t verbosity id =
    P.set_state t @@ record (P.get_state t) verbosity id

  let aggregate t verbosity id =
    P.set_state t @@ aggregate (P.get_state t) verbosity id

  let report t =
    match (P.get_state t).stack with
    | Toplevel {aggregated; recorded}
      when StringMap.cardinal aggregated > 0 || recorded <> [] ->
        P.set_state t @@ empty (P.get_state t).max_verbosity ;
        let report = {aggregated; recorded = List.rev recorded} in
        Some (filter_verbosity (P.get_state t).max_verbosity report)
    | _ -> None

  let may_output =
    match P.output_report with
    | Some fn -> fun t -> Option.iter (fun r -> fn t r) (report t)
    | None -> fun _ -> ()

  let stamp t verbosity id =
    P.set_state t @@ stamp (P.get_state t) verbosity id ;
    may_output t

  let inc t report =
    P.set_state t @@ inc (P.get_state t) report ;
    may_output t

  let mark t verbosity id =
    P.set_state t @@ mark (P.get_state t) verbosity id ;
    may_output t

  let span t verbosity d id =
    P.set_state t @@ span (P.get_state t) verbosity d id ;
    may_output t

  let stop t =
    P.set_state t @@ stop (P.get_state t) ;
    may_output t
end

type (_, _) Profiler.kind += Headless : (verbosity, state ref) Profiler.kind

module Headless = struct
  type nonrec state = state ref

  type config = verbosity

  let kind = Headless

  let create verbosity = ref (empty verbosity)

  include Base (struct
    type t = state

    let[@inline] get_state t = !t

    let[@inline] set_state t s = t := s

    let output_report = None
  end)

  let close _ = ()
end

let headless = (module Headless : DRIVER with type config = verbosity)

type output =
  | Closed of string
  | Open of string * out_channel * Format.formatter

type auto_writer_state = {
  mutable profiler_state : state;
  mutable output : output;
  time : time;
}

type (_, _) Profiler.kind +=
  | Auto_write_to_file : (string * verbosity, auto_writer_state) Profiler.kind

type file_format = Plain_text | Json

let make_driver ~file_format =
  (module struct
    type nonrec state = auto_writer_state

    type config = string * verbosity

    let file_format = file_format

    let kind = Auto_write_to_file

    let create (file_name, verbosity) =
      let file_name =
        match file_format with
        | Plain_text -> file_name ^ ".txt"
        | Json -> file_name ^ ".js"
      in
      {
        profiler_state = empty verbosity;
        time = time ();
        output = Closed file_name;
      }

    include Base (struct
      type t = state

      let[@inline] get_state t = t.profiler_state

      let[@inline] set_state t s = t.profiler_state <- s

      let writer_of =
        match file_format with
        | Plain_text ->
            fun formatter report time ->
              Format.fprintf formatter "%a%!" (pp_report ~t0:time) report
        | Json ->
            fun formatter report _time ->
              let encoded_report =
                Data_encoding.Json.construct Profiler.report_encoding report
              in
              Data_encoding.Json.pp formatter encoded_report ;
              Format.pp_print_newline formatter ()

      let output_report =
        Some
          (fun state report ->
            let ppf =
              match state.output with
              | Open (_, _, ppf) -> ppf
              | Closed fn ->
                  let fp = open_out fn in
                  let ppf = Format.formatter_of_out_channel fp in
                  state.output <- Open (fn, fp, ppf) ;
                  ppf
            in
            writer_of ppf report state.time)
    end)

    let close ({output; _} as state) =
      match output with
      | Open (fn, fp, ppf) ->
          close_out fp ;
          Format.pp_print_newline ppf () ;
          state.output <- Closed fn
      | Closed _ -> ()
  end : DRIVER
    with type config = string * verbosity)

let auto_write_to_txt_file = make_driver ~file_format:Plain_text

let auto_write_to_json_file = make_driver ~file_format:Json

let default_driver =
  match Sys.getenv "PROFILING_BACKEND" |> String.lowercase_ascii with
  | "json" -> auto_write_to_json_file
  | "text" | "txt" -> auto_write_to_txt_file
  | _ | (exception Not_found) -> auto_write_to_txt_file

let instantiate_default_driver = Profiler.instance default_driver
