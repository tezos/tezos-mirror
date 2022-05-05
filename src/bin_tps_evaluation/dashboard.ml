(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Test = struct
  let benchmark_tps = "tezos_tps_benchmark"

  let gas_tps = "tezos_gas_tps"

  let estimate_average_block = "tezos_tps_estimate_avearage"
end

module Measurement = struct
  let gas_tps_evaluation = "gas_tps_evaluation"

  let defacto_tps_of_injection = "defacto_tps_of_injection"

  let empirical_tps = "empirical_tps"
end

module Tag = struct
  let lifted_protocol_limits = "lifted_protocol_limits"
end

let update_grafana_dashboard () =
  let field = "duration" in
  let graph test measurement tags =
    Grafana.simple_graph ~yaxis_format:"ops" ~tags ~measurement ~test ~field ()
  in
  Long_test.update_grafana_dashboard
    {
      uid = "tps";
      title = "TPS Evaluation";
      description = "TPS Evaluation data";
      panels =
        [
          Row "Gas TPS";
          graph Test.gas_tps Measurement.gas_tps_evaluation [];
          Row "Empirical TPS (with protocol limits)";
          graph
            Test.benchmark_tps
            Measurement.defacto_tps_of_injection
            [(Tag.lifted_protocol_limits, "false")];
          graph
            Test.benchmark_tps
            Measurement.empirical_tps
            [(Tag.lifted_protocol_limits, "false")];
          Row "Empirical TPS (without protocol limits)";
          graph
            Test.benchmark_tps
            Measurement.defacto_tps_of_injection
            [(Tag.lifted_protocol_limits, "true")];
          graph
            Test.benchmark_tps
            Measurement.empirical_tps
            [(Tag.lifted_protocol_limits, "true")];
        ];
    }
