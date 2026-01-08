(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let sf = Printf.sprintf

let echo x = Printf.ksprintf print_endline x

let top count tests ~by ~cmp ~get ~show =
  echo "Top %i (by %s):" count by ;
  let tests = List.sort (fun a b -> cmp (get b) (get a)) tests in
  let rec list index = function
    | [] -> ()
    | (head : Record.test) :: tail ->
        echo "%d) %s (%s)" index head.title (show (get head)) ;
        if index < count then list (index + 1) tail
  in
  list 1 tests

let run ~recursive ~filter ~paths =
  let tests = Record.input ~recursive paths in
  let tests =
    match filter with
    | None -> tests
    | Some filter -> List.filter (Record.matches filter) tests
  in
  let stats = Stats.make tests in
  echo "test_count = %d" stats.count ;
  echo
    "total_duration = %.3f seconds = %.2f minutes = %.2f hours"
    (Stats.Duration.seconds stats.total_duration)
    (Stats.Duration.minutes stats.total_duration)
    (Stats.Duration.hours stats.total_duration) ;
  echo
    "average_duration = %.3f seconds"
    (Stats.Duration.seconds stats.average_duration) ;
  echo "" ;
  top
    10
    tests
    ~by:"duration"
    ~cmp:Float.compare
    ~get:Record.duration_minutes
    ~show:(sf "%.2f minutes")
