(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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
type time = Measure.time

type datum = {
  scenario : string;
  section : string;
  label : string;
  ticks : Z.t;
  time : time;
}

let make_datum scenario section label ticks time =
  {scenario; section; label; ticks; time}

type benchmark = {
  verbose : bool;
  totals : bool;
  irmin : bool;
  current_scenario : string;
  current_section : string;
  data : datum list;
  total_time : time;
  total_tick : Z.t;
}

let empty_benchmark ?(verbose = false) ?(totals = true) ?(irmin = true) () =
  {
    verbose;
    totals;
    irmin;
    current_scenario = "";
    current_section = "";
    data = [];
    total_time = Measure.zero;
    total_tick = Z.zero;
  }

let init_scenario scenario benchmark =
  {
    benchmark with
    current_scenario = scenario;
    current_section = "Booting " ^ scenario;
  }

let switch_section current_section benchmark = {benchmark with current_section}

let verbose_datum name ticks time =
  Printf.printf
    "%s finished in %s ticks %f s\n%!"
    name
    (Z.to_string ticks)
    (Measure.to_seconds time)

let add_datum name ticks time benchmark =
  if benchmark.verbose then verbose_datum name ticks time ;
  if (not benchmark.totals) && benchmark.current_section = name then benchmark
  else if (not benchmark.irmin) && ticks = Z.zero then benchmark
  else
    let datum =
      make_datum
        benchmark.current_scenario
        benchmark.current_section
        name
        ticks
        time
    in
    {benchmark with data = datum :: benchmark.data}

let add_final_info total_time total_tick benchmark =
  if benchmark.totals then
    let datum =
      make_datum
        benchmark.current_scenario
        "all steps"
        "total"
        total_tick
        total_time
    in
    {benchmark with data = datum :: benchmark.data}
  else benchmark

let add_tickless_datum label time benchmark =
  add_datum label Z.zero time benchmark

module Csv = struct
  let pp_line oc scenario section label ticks time =
    Printf.fprintf
      oc
      "\"%s\" , \"%s\" , \"%s\" ,  %s ,  %f \n%!"
      scenario
      section
      label
      (Z.to_string ticks)
      (Measure.to_seconds time)

  let pp_datum oc {scenario; section; label; ticks; time} =
    if section != label then pp_line oc scenario section label ticks time
    else pp_line oc scenario section "all phases" ticks time

  let pp_benchmark oc benchmark =
    List.iter (pp_datum oc) (List.rev benchmark.data) ;
    close_out oc
end

let cycle_infos benchmark =
  let folder (cycles, acc_ticks, acc_time) datum =
    match datum.label with
    | "Padding" -> ((acc_ticks, acc_time) :: cycles, Z.zero, 0.0)
    | "Decoding" | "Linking" | "Initialising" | "Evaluating" ->
        ( cycles,
          Z.add acc_ticks datum.ticks,
          acc_time +. Measure.to_seconds datum.time )
    | _ -> (cycles, acc_ticks, acc_time)
  in
  let cycles, _, _ =
    List.fold_left folder ([], Z.zero, 0.0) (List.rev benchmark.data)
  in
  List.rev cycles

let pp_analysis fmt benchmark =
  let cycles = cycle_infos benchmark in
  Format.fprintf fmt "Ran for %d kernel_run calls:" (List.length cycles) ;
  let pp_cycle (ticks, time) =
    Format.fprintf fmt "\n%s ticks in %f seconds" (Z.to_string ticks) time
  in
  List.iter pp_cycle cycles
