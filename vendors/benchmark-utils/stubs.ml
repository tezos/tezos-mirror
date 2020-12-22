(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <nomadic@tezcore.com>                    *)
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

let wrap_exn s f =
  try f ()
  with _ -> Stdlib.failwith s

let wrap_cmd f =
  try
    `Ok (f ())
  with
  | Failure s -> `Error (false, s)

let stabilize_gc () =
  let rec loop failsafe last_heap_live_words =
    if failsafe <= 0 then
      Stdlib.failwith "unable to stabilize the number of live words in the major heap";
    Gc.compact ();
    let stat = Gc.stat () in
    if stat.live_words <> last_heap_live_words
    then loop (failsafe - 1) stat.live_words
  in
  loop 10 0

module Cache = struct

  external flush_cache : int64 -> unit = "stub_flush_cache"

end

module Affinity = struct

  exception Failed_to_set_affinity of int

  external raw_sched_setaffinity : int -> int = "stub_sched_setaffinity"

  let set pid =
    if not (raw_sched_setaffinity pid = 0) then
      raise (Failed_to_set_affinity pid)

end

module Time = struct

  external get_time_ns :
    unit -> int64 = "stub_get_time_ns"

  let duration f =
    let before' = get_time_ns () in
    let res = f () in
    let after' = get_time_ns () in
    let diff = Int64.(to_int (sub after' before')) in
    res, diff

  let fold duration_ns acc f =
    let start = get_time_ns () in
    let diff () =
      let _end = get_time_ns () in
      Int64.(to_int (sub _end start)) in
    let acc = ref acc in
    while Stdlib.((diff ()) < duration_ns) do
      acc := f !acc
    done;
    !acc

end
