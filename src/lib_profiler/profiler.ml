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

type metadata = (string * string) list

type id = string * metadata

type ids = string list * metadata

module IdMap = Map.Make (struct
  type t = id

  let compare = Stdlib.compare
end)

type time = {wall : float; cpu : float}

type span = Span of time

let zero_time = Span {wall = 0.; cpu = 0.}

let ( +* ) {wall = walla; cpu = cpua} {wall = wallb; cpu = cpub} =
  {wall = walla +. wallb; cpu = cpua +. cpub}

let ( -* ) {wall = walla; cpu = cpua} {wall = wallb; cpu = cpub} =
  {wall = walla -. wallb; cpu = cpua -. cpub}

type verbosity = Notice | Info | Debug

type aggregated_node = {
  count : int;
  total : span;
  children : aggregated_node IdMap.t;
  node_verbosity : verbosity;
}

type seq_item = {
  start : time;
  duration : span;
  contents : report;
  item_verbosity : verbosity;
}

and report = {
  aggregated : aggregated_node IdMap.t;
  recorded : (id * seq_item) list;
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

let verbosity_encoding =
  let open Data_encoding in
  string_enum [("notice", Notice); ("info", Info); ("debug", Debug)]

let id_encoding =
  let open Data_encoding in
  tup2 string (list @@ tup2 string string)

let aggregated_encoding =
  let open Data_encoding in
  mu
    "aggregated"
    ~title:"aggregated"
    ~description:"Aggregated node"
    (fun aggregated_encoding ->
      conv
        IdMap.bindings
        (fun l -> IdMap.of_seq (List.to_seq l))
        (list
           (merge_objs
              (obj1 (req "name" id_encoding))
              (conv
                 (fun {count; total; children; node_verbosity} ->
                   (count, total, children, node_verbosity))
                 (fun (count, total, children, node_verbosity) ->
                   {count; total; children; node_verbosity})
                 (obj4
                    (req "count" int31)
                    (req "total" span_encoding)
                    (dft "children" aggregated_encoding IdMap.empty)
                    (req "verbosity" verbosity_encoding))))))

let recorded_encoding report_encoding =
  let open Data_encoding in
  list
    (conv
       (fun (n, {start; duration; contents; item_verbosity}) ->
         (n, start, duration, contents, item_verbosity))
       (fun (n, start, duration, contents, item_verbosity) ->
         (n, {start; duration; contents; item_verbosity}))
       (obj5
          (req "name" id_encoding)
          (req "start" time_encoding)
          (req "duration" span_encoding)
          (req "report" report_encoding)
          (req "verbosity" verbosity_encoding)))

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

type view = View : ('config, 'state) kind -> view

module type DRIVER = sig
  type config

  type state

  val kind : (config, state) kind

  val encoding_case : view Data_encoding.case

  val create : config -> state

  val time : state -> time

  val record : state -> verbosity -> id -> unit

  val aggregate : state -> verbosity -> id -> unit

  val stop : state -> unit

  val stamp : state -> verbosity -> id -> unit

  val mark : state -> verbosity -> ids -> unit

  val span : state -> verbosity -> span -> ids -> unit

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

let record p verbosity id =
  iter p (fun (module I) -> I.Driver.record I.state verbosity id)

let aggregate p verbosity id =
  iter p (fun (module I) -> I.Driver.aggregate I.state verbosity id)

let stamp p verbosity ids =
  iter p (fun (module I) -> I.Driver.stamp I.state verbosity ids)

let mark p verbosity ids =
  iter p (fun (module I) -> I.Driver.mark I.state verbosity ids)

let span p verbosity d ids =
  iter p (fun (module I) -> I.Driver.span I.state verbosity d ids)

let inc p report = iter p (fun (module I) -> I.Driver.inc I.state report)

let report (module I : INSTANCE) = I.Driver.report I.state

let stop p = iter p (fun (module I) -> I.Driver.stop I.state)

let section p start id f =
  start p id ;
  let r = try Ok (f ()) with exn -> Error exn in
  stop p ;
  match r with Ok r -> r | Error exn -> raise exn

let record_f p verbosity id f = section p (fun p -> record p verbosity) id f

let aggregate_f p verbosity id f =
  section p (fun p -> aggregate p verbosity) id f

let span_f p verbosity ids f =
  let is = plugged p in
  let t0s = List.map (fun i -> (i, time i)) is in
  let r = try Ok (f ()) with exn -> Error exn in
  List.iter
    (fun (((module I : INSTANCE) as i), t0) ->
      let t = time i in
      I.Driver.span I.state verbosity (Span (t -* t0)) ids)
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

let record_s p verbosity id f = section_s p (fun p -> record p verbosity) id f

let aggregate_s p verbosity id f =
  section_s p (fun p -> aggregate p verbosity) id f

let span_s p verbosity ids f =
  let is = plugged p in
  let t0s = List.map (fun i -> (i, time i)) is in
  Lwt.catch
    (fun () ->
      Lwt.bind (f ()) (fun r ->
          List.iter
            (fun (((module I : INSTANCE) as i), t0) ->
              let t = time i in
              I.Driver.span I.state verbosity (Span (t -* t0)) ids)
            t0s ;
          Lwt.return r))
    (fun exn ->
      List.iter
        (fun (((module I : INSTANCE) as i), t0) ->
          let t = time i in
          I.Driver.span I.state verbosity (Span (t -* t0)) ids)
        t0s ;
      Lwt.fail exn)

let unplugged () = Stdlib.Hashtbl.create 10

let main = unplugged ()

module type GLOBAL_PROFILER = sig
  type nonrec metadata = metadata

  type nonrec id = id

  type nonrec ids = ids

  type nonrec verbosity = verbosity = Notice | Info | Debug

  val plug : instance -> unit

  val unplug : instance -> unit

  val close_and_unplug : instance -> unit

  val close_and_unplug_all : unit -> unit

  val plugged : unit -> instance list

  val record : verbosity -> id -> unit

  val aggregate : verbosity -> id -> unit

  val stop : unit -> unit

  val stamp : verbosity -> id -> unit

  val mark : verbosity -> ids -> unit

  val span : verbosity -> span -> ids -> unit

  val inc : report -> unit

  val record_f : verbosity -> id -> (unit -> 'a) -> 'a

  val record_s : verbosity -> id -> (unit -> 'a Lwt.t) -> 'a Lwt.t

  val aggregate_f : verbosity -> id -> (unit -> 'a) -> 'a

  val aggregate_s : verbosity -> id -> (unit -> 'a Lwt.t) -> 'a Lwt.t

  val span_f : verbosity -> ids -> (unit -> 'a) -> 'a

  val span_s : verbosity -> ids -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end

let wrap profiler =
  let module Wrapped = struct
    type nonrec metadata = metadata

    type nonrec id = id

    type nonrec ids = ids

    type nonrec verbosity = verbosity = Notice | Info | Debug

    let plug i = plug profiler i

    let unplug i = unplug profiler i

    let close_and_unplug i = close_and_unplug profiler i

    let close_and_unplug_all () = close_and_unplug_all profiler

    let plugged () = plugged profiler

    let record verbosity id = record profiler verbosity id

    let record_f verbosity id f = record_f profiler verbosity id f

    let record_s verbosity id f = record_s profiler verbosity id f

    let aggregate verbosity id = aggregate profiler verbosity id

    let aggregate_f verbosity id f = aggregate_f profiler verbosity id f

    let aggregate_s verbosity id f = aggregate_s profiler verbosity id f

    let stop () = stop profiler

    let stamp verbosity id = stamp profiler verbosity id

    let mark verbosity ids = mark profiler verbosity ids

    let span verbosity d ids = span profiler verbosity d ids

    let inc r = inc profiler r

    let span_f verbosity ids f = span_f profiler verbosity ids f

    let span_s verbosity ids f = span_s profiler verbosity ids f
  end in
  (module Wrapped : GLOBAL_PROFILER)

type 'a section_maker = 'a * metadata -> unit

let section_maker ?(verbosity = Notice) equal to_string profiler :
    'a section_maker =
  let last = ref None in
  let () = at_exit (fun () -> Option.iter (fun _ -> stop profiler) !last) in
  fun (id, metadata) ->
    match !last with
    | None ->
        record profiler verbosity (to_string id, metadata) ;
        last := Some id
    | Some id' when equal id' id -> ()
    | Some _ ->
        stop profiler ;
        record profiler verbosity (to_string id, metadata) ;
        last := Some id
