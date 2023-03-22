(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let int_list_opt = Check.(option @@ list @@ int)

let int_opt = Check.(option int)

module N_events = struct
  let tags = ["daemon"; "n_events"]

  let should_evaluate_in_the_list_of_given_events () =
    Test.register
      ~__FILE__
      ~title:"Daemon.n_events should evaluate in the list of given events"
      ~tags
    @@ fun () ->
    let filter = Daemon.n_events 5 Option.some in
    let msg nth = Printf.sprintf "%s call should give %%R. Got %%L" nth in
    Check.(filter 1 = None) int_list_opt ~error_msg:(msg "1st") ;
    Check.(filter 2 = None) int_list_opt ~error_msg:(msg "2nd") ;
    Check.(filter 3 = None) int_list_opt ~error_msg:(msg "3rd") ;
    Check.(filter 4 = None) int_list_opt ~error_msg:(msg "4th") ;
    Check.(filter 5 = Some [1; 2; 3; 4; 5]) int_list_opt ~error_msg:(msg "5th") ;
    unit

  let calls_should_be_independent () =
    Test.register
      ~__FILE__
      ~title:"Daemon.n_events calls should independent"
      ~tags
    @@ fun () ->
    let filter1 = Daemon.n_events 2 Option.some in
    let filter2 = Daemon.n_events 2 Option.some in
    let msg nth fth =
      Printf.sprintf "%s call to filter%s should give %%R. Got %%L" nth fth
    in
    Check.(filter1 1 = None) int_list_opt ~error_msg:(msg "1st" "1") ;
    Check.(filter2 2 = None) int_list_opt ~error_msg:(msg "1st" "2") ;
    Check.(filter1 3 = Some [1; 3]) int_list_opt ~error_msg:(msg "2nd" "1") ;
    Check.(filter2 4 = Some [2; 4]) int_list_opt ~error_msg:(msg "2nd" "2") ;
    unit

  let should_fail_if_n_inf_to_zero () =
    Test.register ~__FILE__ ~title:"Daemon.n_events should fail if n < 0" ~tags
    @@ fun () ->
    let run () = ignore @@ Daemon.n_events (-1) Option.some () in
    Check.raises
      (Invalid_argument "Base.n_events_rev: n must be > 0.")
      run
      ~error_msg:"expected run to raise %L, got %R" ;
    unit

  let should_fail_if_n_eq_zero () =
    Test.register ~__FILE__ ~title:"Daemon.n_events should fail if n = 0" ~tags
    @@ fun () ->
    let run () = ignore @@ Daemon.n_events 0 Option.some () in
    Check.raises
      (Invalid_argument "Base.n_events_rev: n must be > 0.")
      run
      ~error_msg:"expected run to raise %L, got %R" ;
    unit
end

module Nth_event = struct
  let tags = ["daemon"; "nth_event"]

  let should_evaluate_in_the_nth_given_event () =
    Test.register
      ~__FILE__
      ~title:"Daemon.nth_event should evaluate in the nth given event"
      ~tags
    @@ fun () ->
    let filter = Daemon.nth_event 5 Option.some in
    let msg nth = Printf.sprintf "%s call should give %%R. Got %%L" nth in
    Check.(filter 1 = None) int_opt ~error_msg:(msg "1st") ;
    Check.(filter 2 = None) int_opt ~error_msg:(msg "2nd") ;
    Check.(filter 3 = None) int_opt ~error_msg:(msg "3rd") ;
    Check.(filter 4 = None) int_opt ~error_msg:(msg "4th") ;
    Check.(filter 5 = Some 5) int_opt ~error_msg:(msg "5th") ;
    unit

  let calls_should_be_independent () =
    Test.register
      ~__FILE__
      ~title:"Daemon.nth_event calls should independent"
      ~tags
    @@ fun () ->
    let filter1 = Daemon.nth_event 2 Option.some in
    let filter2 = Daemon.nth_event 2 Option.some in
    let msg nth fth =
      Printf.sprintf "%s call to filter%s should give %%R. Got %%L" nth fth
    in
    Check.(filter1 1 = None) int_opt ~error_msg:(msg "1st" "1") ;
    Check.(filter2 2 = None) int_opt ~error_msg:(msg "1st" "2") ;
    Check.(filter1 3 = Some 3) int_opt ~error_msg:(msg "2nd" "1") ;
    Check.(filter2 4 = Some 4) int_opt ~error_msg:(msg "2nd" "2") ;
    unit

  let should_fail_if_n_inf_to_zero () =
    Test.register ~__FILE__ ~title:"Daemon.nth_event should fail if n < 0" ~tags
    @@ fun () ->
    let run () = ignore @@ Daemon.nth_event (-1) Option.some () in
    Check.raises
      (Invalid_argument "Base.n_events_rev: n must be > 0.")
      run
      ~error_msg:"expected run to raise %L, got %R" ;
    unit

  let should_fail_if_n_eq_zero () =
    Test.register ~__FILE__ ~title:"Daemon.nth_event should fail if n = 0" ~tags
    @@ fun () ->
    let run () = ignore @@ Daemon.nth_event 0 Option.some () in
    Check.raises
      (Invalid_argument "Base.n_events_rev: n must be > 0.")
      run
      ~error_msg:"expected run to raise %L, got %R" ;
    unit
end

let () =
  N_events.should_evaluate_in_the_list_of_given_events () ;
  N_events.calls_should_be_independent () ;
  N_events.should_fail_if_n_inf_to_zero () ;
  N_events.should_fail_if_n_eq_zero () ;
  Nth_event.should_evaluate_in_the_nth_given_event () ;
  Nth_event.calls_should_be_independent () ;
  Nth_event.should_fail_if_n_inf_to_zero () ;
  Nth_event.should_fail_if_n_eq_zero ()
