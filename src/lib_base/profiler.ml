(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module StringMap = Map.Make (String)

type time = {wall : float; cpu : float}

type span = Span of time

let zero_time = Span {wall = 0.; cpu = 0.}

let ( +* ) {wall = walla; cpu = cpua} {wall = wallb; cpu = cpub} =
  {wall = walla +. wallb; cpu = cpua +. cpub}

let ( -* ) {wall = walla; cpu = cpua} {wall = wallb; cpu = cpub} =
  {wall = walla -. wallb; cpu = cpua -. cpub}

type lod = Terse | Detailed | Verbose

type aggregated_node = {
  count : int;
  total : span;
  children : aggregated_node StringMap.t;
  node_lod : lod;
}

type seq_item = {
  start : time;
  duration : span;
  contents : report;
  item_lod : lod;
}

and report = {
  aggregated : aggregated_node StringMap.t;
  recorded : (string * seq_item) list;
}

let time_encoding =
  let open Data_encoding in
  conv
    (fun {wall; cpu} -> (wall, cpu))
    (fun (wall, cpu) -> {wall; cpu})
    (obj2 (req "wall" float) (req "cpu" float))

let span_encoding =
  let open Data_encoding in
  conv (fun (Span t) -> t) (fun t -> Span t) time_encoding

let lod_encoding =
  let open Data_encoding in
  string_enum [("terse", Terse); ("detailed", Detailed); ("verbose", Verbose)]

let aggregated_encoding =
  let open Data_encoding in
  mu
    "aggregated"
    ~title:"aggregated"
    ~description:"Aggregated node"
    (fun aggregated_encoding ->
      conv
        StringMap.bindings
        (fun l -> StringMap.of_seq (List.to_seq l))
        (list
           (merge_objs
              (obj1 (req "name" string))
              (conv
                 (fun {count; total; children; node_lod} ->
                   (count, total, children, node_lod))
                 (fun (count, total, children, node_lod) ->
                   {count; total; children; node_lod})
                 (obj4
                    (req "count" int31)
                    (req "total" span_encoding)
                    (dft "children" aggregated_encoding StringMap.empty)
                    (req "lod" lod_encoding))))))

let recorded_encoding report_encoding =
  let open Data_encoding in
  list
    (conv
       (fun (n, {start; duration; contents; item_lod}) ->
         (n, start, duration, contents, item_lod))
       (fun (n, start, duration, contents, item_lod) ->
         (n, {start; duration; contents; item_lod}))
       (obj5
          (req "name" string)
          (req "start" time_encoding)
          (req "duration" span_encoding)
          (req "report" report_encoding)
          (req "lod" lod_encoding)))

let report_encoding =
  let open Data_encoding in
  mu "report" ~title:"report" ~description:"Report node" (fun report_encoding ->
      conv
        (fun {aggregated; recorded} -> (aggregated, recorded))
        (fun (aggregated, recorded) -> {aggregated; recorded})
        (obj2
           (req "aggregated" aggregated_encoding)
           (req "recorded" (recorded_encoding report_encoding))))

type (_, _) kind = ..

module type DRIVER = sig
  type config

  type state

  val kind : (config, state) kind

  val create : config -> state

  val time : state -> time

  val record : state -> lod -> string -> unit

  val aggregate : state -> lod -> string -> unit

  val stop : state -> unit

  val stamp : state -> lod -> string -> unit

  val mark : state -> lod -> string list -> unit

  val span : state -> lod -> span -> string list -> unit

  val inc : state -> report -> unit

  val report : state -> report option

  val close : state -> unit
end

type 'a driver = (module DRIVER with type config = 'a)

module type INSTANCE = sig
  module Driver : DRIVER

  val state : Driver.state

  val id : int

  val report_task : (report Lwt.t * report Lwt.u) option ref
end

type instance = (module INSTANCE)

let ids = ref 0

let instance (type a) (module D : DRIVER with type config = a) (config : a) =
  let state = D.create config in
  let module I = struct
    module Driver = D

    let state = state

    let id =
      incr ids ;
      !ids

    let report_task = ref None
  end in
  (module I : INSTANCE)

let time (module I : INSTANCE) = I.Driver.time I.state

let close (module I : INSTANCE) = I.Driver.close I.state

type profiler = (int, instance) Stdlib.Hashtbl.t

let may_wakeup_report (module I : INSTANCE) =
  match !I.report_task with
  | Some (_, u) -> (
      match I.Driver.report I.state with
      | None -> ()
      | Some r ->
          Lwt.wakeup_later u r ;
          I.report_task := None)
  | None -> ()

let report_s (module I : INSTANCE) =
  match !I.report_task with
  | Some (t, _) -> t
  | None ->
      let t, u = Lwt.task () in
      I.report_task := Some (t, u) ;
      t

let plug p ((module I : INSTANCE) as i) = Stdlib.Hashtbl.replace p I.id i

let unplug p (module I : INSTANCE) = Stdlib.Hashtbl.remove p I.id

let close_and_unplug p i =
  close i ;
  unplug p i

let plugged p = List.of_seq (Stdlib.Hashtbl.to_seq_values p)

let close_and_unplug_all p =
  Stdlib.Hashtbl.iter (fun _ i -> close_and_unplug p i) p

let iter (p : profiler) f =
  Stdlib.Hashtbl.iter
    (fun _ i ->
      let r = f i in
      may_wakeup_report i ;
      r)
    p

let record p ?(lod = Terse) id =
  iter p (fun (module I) -> I.Driver.record I.state lod id)

let aggregate p ?(lod = Terse) id =
  iter p (fun (module I) -> I.Driver.aggregate I.state lod id)

let stamp p ?(lod = Terse) ids =
  iter p (fun (module I) -> I.Driver.stamp I.state lod ids)

let mark p ?(lod = Terse) ids =
  iter p (fun (module I) -> I.Driver.mark I.state lod ids)

let span p ?(lod = Terse) d ids =
  iter p (fun (module I) -> I.Driver.span I.state lod d ids)

let inc p report = iter p (fun (module I) -> I.Driver.inc I.state report)

let report (module I : INSTANCE) = I.Driver.report I.state

let stop p = iter p (fun (module I) -> I.Driver.stop I.state)

let section p start id f =
  start p id ;
  let r = try Ok (f ()) with exn -> Error exn in
  stop p ;
  match r with Ok r -> r | Error exn -> raise exn

let record_f p ?lod id f = section p (fun p -> record p ?lod) id f

let aggregate_f p ?lod id f = section p (fun p -> aggregate p ?lod) id f

let span_f p ?(lod = Terse) ids f =
  let is = plugged p in
  let t0s = List.map (fun i -> (i, time i)) is in
  let r = try Ok (f ()) with exn -> Error exn in
  List.iter
    (fun (((module I : INSTANCE) as i), t0) ->
      let t = time i in
      I.Driver.span I.state lod (Span (t -* t0)) ids)
    t0s ;
  match r with Ok r -> r | Error exn -> raise exn

let section_s p start id f =
  start p id ;
  Lwt.catch
    (fun () ->
      Lwt.bind (f ()) (fun r ->
          stop p ;
          Lwt.return r))
    (fun exn ->
      stop p ;
      Lwt.fail exn)

let record_s p ?lod id f = section_s p (fun p -> record p ?lod) id f

let aggregate_s p ?lod id f = section_s p (fun p -> aggregate p ?lod) id f

let span_s p ?(lod = Terse) ids f =
  let is = plugged p in
  let t0s = List.map (fun i -> (i, time i)) is in
  Lwt.catch
    (fun () ->
      Lwt.bind (f ()) (fun r ->
          List.iter
            (fun (((module I : INSTANCE) as i), t0) ->
              let t = time i in
              I.Driver.span I.state lod (Span (t -* t0)) ids)
            t0s ;
          Lwt.return r))
    (fun exn ->
      List.iter
        (fun (((module I : INSTANCE) as i), t0) ->
          let t = time i in
          I.Driver.span I.state lod (Span (t -* t0)) ids)
        t0s ;
      Lwt.fail exn)

let unplugged () = Stdlib.Hashtbl.create 10

let with_new_profiler driver state f =
  let p = unplugged () in
  let i = instance driver state in
  plug p i ;
  let r =
    try f p
    with exn ->
      close i ;
      raise exn
  in
  let rec collect acc =
    match report i with None -> List.rev acc | Some r -> collect (r :: acc)
  in
  let reports = collect [] in
  close i ;
  (r, reports)

let with_new_profiler_s driver state f =
  let p = unplugged () in
  let i = instance driver state in
  plug p i ;
  Lwt.bind
    (Lwt.catch
       (fun () -> Lwt.bind (f p) (fun r -> Lwt.return (Ok r)))
       (fun exn -> Lwt.return (Error exn)))
    (function
      | Ok r ->
          let rec collect acc =
            match report i with
            | None -> List.rev acc
            | Some r -> collect (r :: acc)
          in
          let reports = collect [] in
          close i ;
          Lwt.return (r, reports)
      | Error exn ->
          close i ;
          Lwt.fail exn)

let main = unplugged ()

module type GLOBAL_PROFILER = sig
  type nonrec lod = lod = Terse | Detailed | Verbose

  val plug : instance -> unit

  val unplug : instance -> unit

  val close_and_unplug : instance -> unit

  val close_and_unplug_all : unit -> unit

  val plugged : unit -> instance list

  val record : ?lod:lod -> string -> unit

  val aggregate : ?lod:lod -> string -> unit

  val stop : unit -> unit

  val stamp : ?lod:lod -> string -> unit

  val mark : ?lod:lod -> string list -> unit

  val span : ?lod:lod -> span -> string list -> unit

  val inc : report -> unit

  val record_f : ?lod:lod -> string -> (unit -> 'a) -> 'a

  val record_s : ?lod:lod -> string -> (unit -> 'a Lwt.t) -> 'a Lwt.t

  val aggregate_f : ?lod:lod -> string -> (unit -> 'a) -> 'a

  val aggregate_s : ?lod:lod -> string -> (unit -> 'a Lwt.t) -> 'a Lwt.t

  val span_f : ?lod:lod -> string list -> (unit -> 'a) -> 'a

  val span_s : ?lod:lod -> string list -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end

let wrap profiler =
  let module Wrapped = struct
    type nonrec lod = lod = Terse | Detailed | Verbose

    let plug i = plug profiler i

    let unplug i = unplug profiler i

    let close_and_unplug i = close_and_unplug profiler i

    let close_and_unplug_all () = close_and_unplug_all profiler

    let plugged () = plugged profiler

    let record ?lod id = record profiler ?lod id

    let record_f ?lod id f = record_f profiler ?lod id f

    let record_s ?lod id f = record_s profiler ?lod id f

    let aggregate ?lod id = aggregate profiler ?lod id

    let aggregate_f ?lod id f = aggregate_f profiler ?lod id f

    let aggregate_s ?lod id f = aggregate_s profiler ?lod id f

    let stop () = stop profiler

    let stamp ?lod id = stamp profiler ?lod id

    let mark ?lod ids = mark profiler ?lod ids

    let span ?lod d ids = span profiler ?lod d ids

    let inc r = inc profiler r

    let span_f ?lod ids f = span_f ?lod profiler ids f

    let span_s ?lod ids f = span_s ?lod profiler ids f
  end in
  (module Wrapped : GLOBAL_PROFILER)
