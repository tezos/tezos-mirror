(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Cache structures used to memoize costly RPCs/computations. *)

open Protocol.Alpha_context

type round = Round.t

module Block_cache =
  Aches.Vache.Map (Aches.Vache.LRU_Precise) (Aches.Vache.Strong) (Block_hash)

(** The [Timestamp_of_round_tbl] module allows to create memoization tables
    to store function calls of [Round.timestamp_of_round]. *)
module Timestamp_of_round_cache =
  Aches.Vache.Map (Aches.Vache.LRU_Precise) (Aches.Vache.Strong)
    (struct
      (* The type of keys is a tuple that corresponds to the arguments
         of [Round.timestamp_of_round]. *)
      type t = Timestamp.time * round * round

      let hash k = Hashtbl.hash k

      let equal (ts, r1, r2) (ts', r1', r2') =
        Timestamp.(ts = ts') && Round.(r1 = r1') && Round.(r2 = r2')
    end)

module Round_cache_key = struct
  type ts_interval = Timestamp.time * Timestamp.time

  (** The values that are intended to be used here are the
          arguments are: predecessor_timestamp * predecessor_round *
          timestamp_interval *)
  type t = {
    predecessor_timestamp : Timestamp.time;
    predecessor_round : round;
    time_interval : ts_interval;
  }

  let hash {predecessor_timestamp; predecessor_round; _} =
    Stdlib.Hashtbl.hash (predecessor_timestamp, predecessor_round)

  let equal
      {
        predecessor_timestamp = pred_t;
        predecessor_round = pred_r;
        time_interval = t_beg, t_end;
      }
      {
        predecessor_timestamp = pred_t';
        predecessor_round = pred_r';
        time_interval = t_beg', t_end';
      } =
    Timestamp.(pred_t = pred_t')
    && Round.(pred_r = pred_r')
    && Timestamp.(t_beg' <= t_beg)
    && Timestamp.(t_end < t_end')
end

module Round_timestamp_interval_cache =
  Aches.Vache.Map (Aches.Vache.LRU_Precise) (Aches.Vache.Strong)
    (Round_cache_key)
