(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Testing
    _______

    Invocation: dune exec src/lib_store/unix/test/bench.exe --file bench.ml
    Subject: Store locator bench. These tests are not run in the CI
*)

module Store = Tezos_store_unix.Store

(** Test if the linear and exponential locator are the same and outputs
    their timing.
    Run the test with:
    $ dune build @runtest_locator
    Copy the output to a file timing.dat and plot it with:
    $ generate_locator_plot.sh timing.dat
*)
let bench_locator base_dir =
  let open Test_locator in
  let open Lwt_result_syntax in
  let size_chain = 80000 in
  (* timing locators with average over [runs] times *)
  let runs = 10 in
  let _ = Printf.printf "#runs %i\n" runs in
  (* limit after which exp should go linear *)
  let exp_limit = Test_locator.compute_size_chain 120 in
  let _ = Printf.printf "#exp_limit %i\n" exp_limit in
  (* size after which locator always reaches genesis *)
  let locator_limit = compute_size_locator size_chain in
  let _ = Printf.printf "#locator_limit %i\n" locator_limit in
  let*! store = init_chain base_dir in
  let chain_store = Store.main_chain_store store in
  time1 (fun () -> make_empty_chain chain_store size_chain)
  |> fun (res, t_chain) ->
  let _ =
    Printf.printf
      "#size_chain %i built in %f sec\n#      size      exp       lins\n"
      size_chain
      t_chain
  in
  let*! head = res in
  let check_locator max_size : unit tzresult Lwt.t =
    let*! caboose, _ = Store.Chain.caboose chain_store in
    let* block = Store.Block.read_block chain_store head in
    time ~runs (fun () ->
        Store.Chain.compute_locator chain_store ~max_size block seed)
    |> fun (l_exp, t_exp) ->
    time ~runs (fun () ->
        compute_linear_locator chain_store ~caboose ~max_size block)
    |> fun (l_lin, t_lin) ->
    let*! {Block_locator.history = l_exp; _} = l_exp in
    let*! {Block_locator.history = l_lin; _} = l_lin in
    let _ = Printf.printf "%10i %f %f\n" max_size t_exp t_lin in
    Lwt.return
    @@ List.iter2
         ~when_different_lengths:(TzTrace.make @@ Exn (Failure __LOC__))
         (fun hn ho ->
           if not (Block_hash.equal hn ho) then
             Assert.fail_msg "Invalid locator %i" max_size)
         l_exp
         l_lin
  in
  let stop = locator_limit + 20 in
  let rec loop size =
    if size < stop then
      let* _ = check_locator size in
      loop (size + 5)
    else return_unit
  in
  loop 1

let bench = [Test_locator.wrap "bench locator" bench_locator]

let () =
  let open Lwt_syntax in
  Lwt_main.run
    (let* () = Tezos_base_unix.Internal_event_unix.init () in
     Alcotest_lwt.run ~__FILE__ "tezos-store-bench" [("locator bench", bench)])

let () = Tezt.Test.run ()
