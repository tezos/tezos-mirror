(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

[@@@ocaml.warning "-32"]

module Pp = struct
  type t = [`Ok | `Fail | `Skip | `Todo | `Assert] Fmt.t

  let colour_of_tag = function
    | `Ok -> `Green
    | `Fail -> `Red
    | `Skip | `Todo | `Assert -> `Yellow

  let string_of_tag = function
    | `Ok -> "OK"
    | `Fail -> "FAIL"
    | `Skip -> "SKIP"
    | `Todo -> "TODO"
    | `Assert -> "ASSERT"

  let pp_tag ~wrapped ppf typ =
    let colour = colour_of_tag typ in
    let tag = string_of_tag typ in
    let tag = if wrapped then "[" ^ tag ^ "]" else tag in
    Fmt.(styled colour string) ppf tag

  let tag : t = pp_tag ~wrapped:false
end

module type TESTABLE = sig
  type t

  val pp : t Fmt.t

  val equal : t -> t -> bool
end

type 'a testable = (module TESTABLE with type t = 'a)

let pp (type a) (t : a testable) =
  let (module T) = t in
  T.pp

let equal (type a) (t : a testable) =
  let (module T) = t in
  fun a b -> T.equal (a : T.t) (b : T.t)

let isnan f = FP_nan = classify_float f

let testable (type a) (pp : a Fmt.t) (equal : a -> a -> bool) : a testable =
  let module M = struct
    type t = a

    let pp = pp

    let equal = equal
  end in
  (module M)

let int32 = testable Fmt.int32 ( = )

let int64 = testable Fmt.int64 ( = )

let int = testable Fmt.int ( = )

let float eps =
  let same x y =
    (isnan x && isnan y)
    (* compare infinities *)
    || x = y
    || abs_float (x -. y) <= eps
  in
  testable Fmt.float same

let char = testable Fmt.char ( = )

let string = testable Fmt.string ( = )

let bytes =
  testable (fun fmt bytes -> Fmt.fmt "%S" fmt (Bytes.to_string bytes)) ( = )

let bool = testable Fmt.bool ( = )

let unit = testable (Fmt.unit "()") ( = )

let option e =
  let eq x y =
    match (x, y) with
    | (Some a, Some b) -> equal e a b
    | (None, None) -> true
    | _ -> false
  in
  testable (Fmt.Dump.option (pp e)) eq

let show_assert = function
  | "" -> ()
  | msg ->
      Fmt.(flush stdout) () (* Flush any test stdout preceding the assert *) ;
      Format.eprintf "%a %s\n%!" Pp.tag `Assert msg

module Source_code_position = struct
  type here = Lexing.position

  type pos = string * int * int * int
end

type 'a extra_info =
  ?here:Source_code_position.here -> ?pos:Source_code_position.pos -> 'a

type return = unit

type speed_level = [`Quick | `Slow]

type 'a test_case = string * speed_level * ('a -> return)

type 'a test = string * 'a test_case list

exception Test_error of unit Fmt.t

let test_case name _ fn = (name, fn)

let check_err fmt = raise (Test_error fmt)

let fail ?here:_ ?pos:_ msg = check_err (fun ppf () -> Fmt.pf ppf "%s" msg)

let failf ?here ?pos fmt = Fmt.kstr (fun msg -> fail ?here ?pos msg) fmt

let check ?here:_ ?pos:_ (type a) (t : a testable) msg (expected : a)
    (actual : a) =
  if not (equal t expected actual) then
    let open Fmt in
    let s = const string in
    let pp_error =
      match msg with
      | "" -> nop
      | _ -> const Pp.tag `Fail ++ s (" " ^ msg) ++ cut
    and pp_expected ppf () =
      Fmt.pf ppf "   Expected: `%a'" (pp t) expected ;
      Format.pp_print_if_newline ppf () ;
      Fmt.cut ppf () ;
      ()
    and pp_actual ppf () = Fmt.pf ppf "   Received: `%a'" (pp t) actual in
    check_err (fun ppf () ->
        Fmt.pf ppf "%a - %a %a" pp_error () pp_actual () pp_expected ())
(*else show_assert (msg ^ " Pass")*)

let run_test_group (group_name, (tests : unit test_case list)) =
  let open Js_of_ocaml in
  Firebug.console##log
    (Js.string @@ Printf.sprintf "---> Running test group %s" group_name) ;
  List.iter
    (fun (test_name, _, (test : unit -> return)) ->
      Firebug.console##log
        (Js.string @@ Printf.sprintf "-------> Running test %s" test_name) ;
      try test () with
      | Test_error error ->
          Firebug.console##log (Js.string @@ Fmt.str "%a" error ())
      | ex ->
          Firebug.console##log
            (Js.string @@ Fmt.str "Unexpected Fail: %s" (Printexc.to_string ex)))
    tests

type 'a with_options =
  ?and_exit:bool ->
  ?verbose:bool ->
  ?compact:bool ->
  ?tail_errors:[`Unlimited | `Limit of int] ->
  ?quick_only:bool ->
  ?show_errors:bool ->
  ?json:bool ->
  ?filter:Re.re option * int list option ->
  ?log_dir:string ->
  ?bail:bool ->
  'a

let run ?and_exit:_ ?verbose:_ ?compact:_ ?tail_errors:_ ?quick_only:_
    ?show_errors:_ ?json:_ ?filter:_ ?log_dir:_ ?bail:_ ?argv:_ name
    (test_group : unit test list) =
  let open Js_of_ocaml in
  Firebug.console##log (Js.string @@ Printf.sprintf "Running test suite %s" name) ;
  List.iter (fun t -> match run_test_group t with _ -> ()) test_group
