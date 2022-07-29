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

type metrics = {
  checkpoint_level : Prometheus.Gauge.t;
  savepoint_level : Prometheus.Gauge.t;
  caboose_level : Prometheus.Gauge.t;
  alternate_heads_count : Prometheus.Gauge.t;
  last_written_block_size : Prometheus.Gauge.t;
  last_store_merge_time : Prometheus.Gauge.t;
  invalid_blocks : Prometheus.Gauge.t;
}

let namespace = Tezos_version.Node_version.namespace

let metrics =
  let subsystem = Some "store" in
  let checkpoint_level =
    let help = "Current checkpoint level" in
    Prometheus.Gauge.v ~help ~namespace ?subsystem "checkpoint_level"
  in
  let savepoint_level =
    let help = "Current savepoint level" in
    Prometheus.Gauge.v ~help ~namespace ?subsystem "savepoint_level"
  in
  let caboose_level =
    let help = "Current caboose level" in
    Prometheus.Gauge.v ~help ~namespace ?subsystem "caboose_level"
  in
  let alternate_heads_count =
    let help = "Current number of alternated heads known" in
    Prometheus.Gauge.v ~help ~namespace ?subsystem "alternate_heads_count"
  in
  let last_written_block_size =
    let help = "Size, in bytes, of the last block written in store" in
    Prometheus.Gauge.v ~help ~namespace ?subsystem "last_written_block_size"
  in
  let last_store_merge_time =
    let help = "Time, in seconds, for the completion of the last store merge" in
    Prometheus.Gauge.v ~help ~namespace ?subsystem "last_merge_time"
  in
  let invalid_blocks =
    let help = "Number of blocks known to be invalid stored on disk" in
    Prometheus.Gauge.v ~help ~namespace ?subsystem "invalid_blocks"
  in
  {
    checkpoint_level;
    savepoint_level;
    caboose_level;
    alternate_heads_count;
    last_written_block_size;
    last_store_merge_time;
    invalid_blocks;
  }
