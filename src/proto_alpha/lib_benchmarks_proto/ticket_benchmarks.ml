(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.com>                        *)
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

open Protocol
open Alpha_context

(** A benchmark for {!Ticket_costs.Constants.cost_compare_ticket_hash}. *)
module Compare_ticket_hash_benchmark : Benchmark.S = struct
  type config = unit

  let config_encoding = Data_encoding.unit

  let default_config = ()

  type workload = unit

  let tags = ["tickets"]

  let workload_encoding = Data_encoding.unit

  let workload_to_vector () = Sparse_vec.String.of_list []

  let name = "COMPARE_TICKET_HASH"

  let info = "Compare cost for Ticket_hash"

  let models =
    [
      ( "compare",
        Model.make
          ~conv:(fun () -> ())
          ~model:
            (Model.unknown_const2
               ~const1:Builtin_benchmarks.timer_variable
               ~const2:(Free_variable.of_string "compare_ticket_hash")) );
    ]

  let benchmark rng_state _conf () =
    let bytes = Base_samplers.bytes rng_state ~size:{min = 1; max = 64} in
    let hash =
      Ticket_hash.of_script_expr_hash @@ Script_expr_hash.hash_bytes [bytes]
    in
    let workload = () in
    let closure () = ignore (Ticket_hash.compare hash hash) in
    Generator.Plain {workload; closure}

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (benchmark rng_state config)
end

let () = Registration_helpers.register (module Compare_ticket_hash_benchmark)
