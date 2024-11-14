(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Metrics = struct
  let namespace = "teztale"

  let durations =
    Prometheus.Summary.v_label
      ~label_name:"requests"
      ~namespace
      ~help:"Time spent in these SQL requests"
      "requests"
end

let sql name =
  Prometheus.Summary.time (Metrics.durations name) Unix.gettimeofday
