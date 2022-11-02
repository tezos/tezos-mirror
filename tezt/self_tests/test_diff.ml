(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let show_diff_report = function
  | Diff.Kept (a, b) -> sf "Kept (%d, %d)" a b
  | Added a -> sf "Added %d" a
  | Removed a -> sf "Removed %d" a

(* The type of output values of Diff.arrays for int arrays. *)
let diff_result_type =
  let report_type =
    Check.comparable
      (fun pp report -> Format.pp_print_string pp (show_diff_report report))
      Stdlib.compare
  in
  Check.(tuple2 bool (array (tuple2 report_type int)))

let ( -- ) a b = (a, b)

let show x = x |> Array.map string_of_int |> Array.to_list |> String.concat "; "

let ( => ) (a, b) expected =
  let result = Diff.arrays a b in
  Check.((result.different, result.merged) = expected)
    diff_result_type
    ~error_msg:
      ("Diff.arrays [| " ^ show a ^ " |] [| " ^ show b
     ^ " |]: expected %R, got %L")

let simple_cases () =
  Test.register ~__FILE__ ~title:"diff: simple cases" ~tags:["diff"; "simple"]
  @@ fun () ->
  [||] -- [||] => (false, [||]) ;
  [|1|] -- [||] => (true, [|(Removed 0, 1)|]) ;
  [||] -- [|1|] => (true, [|(Added 0, 1)|]) ;
  [|1|] -- [|1|] => (false, [|(Kept (0, 0), 1)|]) ;
  [||] -- [|1; 2|] => (true, [|(Added 0, 1); (Added 1, 2)|]) ;
  [|1|] -- [|1; 2|] => (true, [|(Kept (0, 0), 1); (Added 1, 2)|]) ;
  [|2|] -- [|1; 2|] => (true, [|(Added 0, 1); (Kept (0, 1), 2)|]) ;
  [|1; 2|] -- [|1; 2|] => (false, [|(Kept (0, 0), 1); (Kept (1, 1), 2)|]) ;
  [|1; 2|] -- [|2; 1|]
  => (true, [|(Removed 0, 1); (Kept (1, 0), 2); (Added 1, 1)|]) ;
  unit

let random_int_array () = Array.init (Random.int 200) (fun _ -> Random.int 20)

let remove_from_merged to_remove merged =
  Array.to_list merged
  |> List.filter_map (function report, item ->
         if to_remove report then None else Some item)
  |> Array.of_list

let random_cases count =
  Test.register ~__FILE__ ~title:"diff: random cases" ~tags:["diff"; "random"]
  @@ fun () ->
  for _ = 1 to count do
    let a = random_int_array () in
    let b = random_int_array () in
    let max_sync_distance = Random.int 100 in
    let diff = Diff.arrays a b ~max_sync_distance in
    (* Check [different]. *)
    let expect_different = a <> b in
    let show_inputs () =
      Log.info "a = [| %s |]" (show a) ;
      Log.info "b = [| %s |]" (show b)
    in
    if expect_different && not diff.different then (
      show_inputs () ;
      Test.fail
        "Diff.arrays returned false (not different), expected true (different)") ;
    if (not expect_different) && diff.different then (
      show_inputs () ;
      Test.fail
        "Diff.arrays returned true (different), expected false (not different)") ;
    (* Check [merged].
       The property we test is that [a] is [merged] without [Added] items,
       and [b] is [merged] without [Removed] items. *)
    let show_merged () =
      Log.info
        "merged = [| %s |]"
        (diff.merged
        |> Array.map (fun (report, item) ->
               show_diff_report report ^ ", " ^ string_of_int item)
        |> Array.to_list |> String.concat "; ")
    in
    if
      a
      <> remove_from_merged
           (function Diff.Added _ -> true | _ -> false)
           diff.merged
    then (
      show_inputs () ;
      show_merged () ;
      Test.fail "a <> remove_from_merged Added merged") ;
    if
      b
      <> remove_from_merged
           (function Diff.Removed _ -> true | _ -> false)
           diff.merged
    then (
      show_inputs () ;
      show_merged () ;
      Test.fail "a <> remove_from_merged Removed merged")
  done ;
  unit

let test_log () =
  Regression.register ~__FILE__ ~title:"diff: log" ~tags:["diff"; "log"]
  @@ fun () ->
  let before = Array.init 100 string_of_int in
  let after =
    Array.init 90 @@ fun i ->
    let i = if i > 80 then i + 100 else if i > 70 then i + 10 else i in
    string_of_int i
  in
  after.(50) <- "fifty" ;
  after.(52) <- "fifty-two" ;
  Diff.arrays before after |> Diff.reduce_context
  |> Diff.output (fun _color -> Regression.capture) Fun.id ;
  unit

let register () =
  simple_cases () ;
  random_cases 1000 ;
  test_log ()
