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

type stack_item = {id : string; time : time; report : report; lod : lod}

type stack = Toplevel of report | Cons of stack_item * stack

type scope = {id : string; lod : lod; time : time}

type state = {stack : stack; scopes : scope list; max_lod : lod}

let empty max_lod =
  {
    stack = Toplevel {aggregated = StringMap.empty; recorded = []};
    scopes = [];
    max_lod;
  }

let aggregate state lod id =
  {state with scopes = {id; lod; time = time ()} :: state.scopes}

let record state lod id =
  if state.scopes <> [] then aggregate state lod id
  else
    let stack =
      Cons
        ( {
            id;
            time = time ();
            report = {recorded = []; aggregated = StringMap.empty};
            lod;
          },
          state.stack )
    in
    {state with stack}

let rec merge
    {count = na; total = Span ta; children = contentsa; node_lod = loda}
    {count = nb; total = Span tb; children = contentsb; node_lod = lodb} =
  {
    count = na + nb;
    total = Span (ta +* tb);
    children = merge_maps contentsa contentsb;
    node_lod = min loda lodb;
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

let rec apply_lod_to_aggregated lod aggregated =
  StringMap.fold
    (fun id node acc ->
      let children = apply_lod_to_aggregated lod node.children in
      if node.node_lod <= lod then StringMap.add id {node with children} acc
      else merge_maps acc children)
    aggregated
    StringMap.empty

let rec aggregate_report {aggregated; recorded} =
  List.fold_left
    (fun acc (id, {start = _; duration; item_lod; contents}) ->
      let children = aggregate_report contents in
      let node = {count = 1; total = duration; node_lod = item_lod; children} in
      StringMap.add id node acc)
    aggregated
    recorded

let rec apply_lod lod {aggregated; recorded} =
  let aggregated = apply_lod_to_aggregated lod aggregated in
  let aggregated, recorded =
    List.fold_left
      (fun (aggregated, recorded) (id, item) ->
        if item.item_lod <= lod then
          ( aggregated,
            (id, {item with contents = apply_lod lod item.contents}) :: recorded
          )
        else (merge_maps aggregated (aggregate_report item.contents), recorded))
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

let span state lod d ids =
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
            StringMap.singleton id {count; total; children; node_lod = lod}
      in
      inc state {recorded = []; aggregated = build_node tids}

let mark state lod ids = span state lod zero_time ids

let stop_aggregate state lod d id scopes =
  let tids =
    let s_scopes = List.map (fun {id; _} -> (id, d)) scopes in
    List.rev ((id, d) :: s_scopes)
  in
  let rec build_node = function
    | [] -> StringMap.empty
    | (id, d) :: tids ->
        let children = build_node tids in
        let count, total = if tids = [] then (1, d) else (0, zero_time) in
        StringMap.singleton id {count; total; children; node_lod = lod}
  in
  inc state {recorded = []; aggregated = build_node tids}

let stop state =
  match state.scopes with
  | {id; lod; time = t0} :: scopes ->
      let d = Span (time () -* t0) in
      let state = {state with scopes} in
      stop_aggregate state lod d id scopes
  | [] ->
      let stop_report id start contents report item_lod =
        let contents = {contents with recorded = List.rev contents.recorded} in
        let duration = Span (time () -* start) in
        let recorded =
          (id, {start; duration; contents; item_lod}) :: report.recorded
        in
        {report with recorded}
      in
      let stack =
        match state.stack with
        | Cons
            ( {id; time = start; report = contents; lod},
              Cons ({id = pid; time = pt0; report; lod = plod}, rest) ) ->
            Cons
              ( {
                  id = pid;
                  time = pt0;
                  report = stop_report id start contents report lod;
                  lod = plod;
                },
                rest )
        | Cons ({id; time = start; report = contents; lod}, Toplevel report) ->
            Toplevel (stop_report id start contents report lod)
        | Toplevel _ -> (* Shhh, everything will be alright. *) state.stack
      in
      {state with stack}

let stamp state lod id = stop (record state lod id)

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

let pp_line nindent ppf id n t t0 =
  let indent = Stdlib.List.init nindent (fun _ -> "  ") in
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

let rec pp_report t0 nident ppf {aggregated; recorded} =
  StringMap.iter
    (fun id {count = n; total = Span d; children; node_lod = _} ->
      pp_line nident ppf id n d None ;
      pp_report t0 (nident + 1) ppf {recorded = []; aggregated = children})
    aggregated ;
  List.iter
    (fun (id, {start = t; duration = Span d; contents; item_lod = _}) ->
      pp_line nident ppf id 1 d (Some (t -* t0)) ;
      pp_report t (nident + 1) ppf contents)
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

type (_, _) Profiler.kind += Headless : (lod, state ref) Profiler.kind

module Headless = struct
  let time _ = time ()

  type nonrec state = state ref

  type config = lod

  let kind = Headless

  let create lod = ref (empty lod)

  let stamp state lod id = state := stamp !state lod id

  let record state lod id = state := record !state lod id

  let aggregate state lod id = state := aggregate !state lod id

  let inc state report = state := inc !state report

  let mark state lod id = state := mark !state lod id

  let span state lod d id = state := span !state lod d id

  let stop state = state := stop !state

  let report state =
    match !state.stack with
    | Toplevel {aggregated; recorded}
      when StringMap.cardinal aggregated > 0 || recorded <> [] ->
        state := empty !state.max_lod ;
        let report = {aggregated; recorded = List.rev recorded} in
        Some (apply_lod !state.max_lod report)
    | _ -> None

  let close _ = ()
end

let headless = (module Headless : DRIVER with type config = lod)

type output =
  | Closed of string
  | Open of string * out_channel * Format.formatter

type auto_writer_state = {
  mutable profiler_state : state;
  mutable output : output;
  time : time;
}

type (_, _) Profiler.kind +=
  | Auto_write_to_file : (string * lod, auto_writer_state) Profiler.kind

type file_format = Plain_text | Json

let make_driver ~file_format =
  (module struct
    type nonrec state = auto_writer_state

    type config = string * lod

    let file_format = file_format

    let kind = Auto_write_to_file

    let create (fn, lod) =
      {profiler_state = empty lod; time = time (); output = Closed fn}

    let time _ = time ()

    let record state lod id =
      state.profiler_state <- record state.profiler_state lod id

    let aggregate state lod id =
      state.profiler_state <- aggregate state.profiler_state lod id

    let report ({profiler_state; _} as state) =
      match profiler_state.stack with
      | Toplevel {aggregated; recorded}
        when StringMap.cardinal aggregated > 0 || recorded <> [] ->
          state.profiler_state <- empty profiler_state.max_lod ;
          let report = {aggregated; recorded = List.rev recorded} in
          Some (apply_lod profiler_state.max_lod report)
      | _ -> None

    let writer_of file_format formatter report time =
      match file_format with
      | Plain_text ->
          Format.fprintf formatter "%a%!" (pp_report ~t0:time) report
      | Json ->
          let encoded_report =
            Data_encoding.Json.construct Profiler.report_encoding report
          in
          Data_encoding.Json.pp formatter encoded_report

    let may_write ({time = t0; output; _} as state) =
      match report state with
      | None -> ()
      | Some report ->
          let ppf =
            match output with
            | Open (_, _, ppf) -> ppf
            | Closed fn ->
                let fp = open_out fn in
                let ppf = Format.formatter_of_out_channel fp in
                state.output <- Open (fn, fp, ppf) ;
                ppf
          in
          writer_of file_format ppf report t0

    let inc state report =
      state.profiler_state <- inc state.profiler_state report ;
      may_write state

    let mark state lod id =
      state.profiler_state <- mark state.profiler_state lod id ;
      may_write state

    let stamp state lod id =
      state.profiler_state <- stamp state.profiler_state lod id ;
      may_write state

    let span state lod d id =
      state.profiler_state <- span state.profiler_state lod d id ;
      may_write state

    let stop ({profiler_state; _} as state) =
      state.profiler_state <- stop profiler_state ;
      may_write state

    let close ({output; _} as state) =
      match output with
      | Open (fn, fp, _) ->
          close_out fp ;
          state.output <- Closed fn
      | Closed _ -> ()
  end : DRIVER
    with type config = string * lod)

let auto_write_to_txt_file = make_driver ~file_format:Plain_text

let auto_write_to_json_file = make_driver ~file_format:Json
