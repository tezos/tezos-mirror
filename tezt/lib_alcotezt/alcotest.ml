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

open Tezt_core

type return = unit

type speed_level = [`Quick | `Slow]

type 'a test_case = string * speed_level * ('a -> return)

let test_case name speed_level body = (name, speed_level, body)

exception Test_error

type 'a test = string * 'a test_case list

let run ~__FILE__ library_name tests =
  tests
  |> List.iter @@ fun (test_name, test_cases) ->
     test_cases
     |> List.iter @@ fun (test_case_name, speed_level, body) ->
        let tags =
          "alcotezt"
          :: (match speed_level with `Quick -> ["quick"] | `Slow -> [])
        in
        Test.register
          ~__FILE__
          ~title:(library_name ^ ": " ^ test_name ^ " (" ^ test_case_name ^ ")")
          ~tags
        @@ fun () ->
        body () ;
        Base.unit

type 'a testable = (module Tezt_core.Check.EQUALABLE with type t = 'a)

let testable (type a) (pp : Format.formatter -> a -> return)
    (eq : a -> a -> bool) : a testable =
  (module struct
    type t = a

    let pp = pp

    let equal = eq
  end)

let pp (type a) (t : a testable) =
  let module T = (val t) in
  T.pp

let of_pp pp = testable pp ( = )

let equal (type a) (t : a testable) =
  let module T = (val t) in
  T.equal

let string : string testable =
  (module struct
    include String

    let pp = Format.pp_print_string
  end)

let bool : bool testable =
  (module struct
    include Bool

    let pp = Format.pp_print_bool
  end)

let bytes : bytes testable =
  (module struct
    include Bytes

    let pp = Format.pp_print_bytes
  end)

let int64 : int64 testable =
  (module struct
    include Int64

    let equal = Int64.equal

    let pp fmt i = Format.pp_print_string fmt (Int64.to_string i)
  end)

let int32 : int32 testable =
  (module struct
    include Int32

    let equal = Int32.equal

    let pp fmt i = Format.pp_print_string fmt (Int32.to_string i)
  end)

let unit : unit testable =
  (module struct
    type t = unit

    let equal () () = true

    let pp fmt () = Format.fprintf fmt "()"
  end)

let int : int testable =
  (module struct
    type t = int

    let equal = ( = )

    let pp = Format.pp_print_int
  end)

let float eps : float testable =
  (module struct
    type t = float

    let isnan f = FP_nan = classify_float f

    let equal x y =
      (isnan x && isnan y)
      (* compare infinities *)
      || x = y
      || abs_float (x -. y) <= eps

    let pp = Format.pp_print_float
  end)

let result (type a e) (ok : a testable) (error : e testable) :
    (a, e) result testable =
  let module Ok = (val ok) in
  let module Error = (val error) in
  (module struct
    type t = (a, e) result

    let pp fmt = function
      | Ok x -> Format.fprintf fmt "@[<hov 2>Ok@ (%a)@]" Ok.pp x
      | Error x -> Format.fprintf fmt "@[<hov 2>Error@ (%a)@]" Error.pp x

    let equal = Result.equal ~ok:Ok.equal ~error:Error.equal
  end)

let pp_list ?(left = "[") ?(right = "]") pp_item fmt list =
  Format.pp_print_string fmt left ;
  if list <> [] then (
    Format.pp_open_box fmt 1 ;
    Format.pp_print_char fmt ' ' ;
    let pp_sep fmt () =
      Format.pp_print_char fmt ';' ;
      Format.pp_print_space fmt ()
    in
    Format.pp_print_list ~pp_sep pp_item fmt list ;
    Format.pp_print_char fmt ' ' ;
    Format.pp_close_box fmt ()) ;
  Format.pp_print_string fmt right

let list (type a) (el : a testable) : a list testable =
  let module El = (val el) in
  (module struct
    type t = a list

    let pp fmt = pp_list El.pp fmt

    let equal = List.equal El.equal
  end)

let array (type a) (el : a testable) : a array testable =
  let module El = (val el) in
  (module struct
    type t = a array

    let pp fmt a =
      let l = Array.to_list a in
      pp_list ~left:"[|" ~right:"|]" El.pp fmt l

    let equal = Array.for_all2 El.equal
  end)

let pair (type a b) (el1 : a testable) (el2 : b testable) : (a * b) testable =
  let module El1 = (val el1) in
  let module El2 = (val el2) in
  (module struct
    type t = a * b

    let pp fmt (a, b) = Format.fprintf fmt "(%a,%a)" El1.pp a El2.pp b

    let equal (a1, b1) (a2, b2) = El1.equal a1 a2 && El2.equal b1 b2
  end)

let triple (type a b c) (el1 : a testable) (el2 : b testable) (el3 : c testable)
    : (a * b * c) testable =
  let module El1 = (val el1) in
  let module El2 = (val el2) in
  let module El3 = (val el3) in
  (module struct
    type t = a * b * c

    let pp fmt (a, b, c) =
      Format.fprintf fmt "(%a,%a,%a)" El1.pp a El2.pp b El3.pp c

    let equal (a1, b1, c1) (a2, b2, c2) =
      El1.equal a1 a2 && El2.equal b1 b2 && El3.equal c1 c2
  end)

let option (type a) (value : a testable) : a option testable =
  let module Value = (val value) in
  (module struct
    type t = a option

    let pp fmt = function
      | Some v -> Format.fprintf fmt "@[<hov 2>Some@ (%a)@]" Value.pp v
      | None -> Format.fprintf fmt "@[<hov 2>None@]"

    let equal = Option.equal Value.equal
  end)

let check testable msg expected actual =
  Check.(expected = actual)
    (Check.equalable_module testable)
    ~error_msg:(msg ^ ": expected %L, got %R")

let check' t ~msg ~expected ~actual = check t msg expected actual

let check_raises msg exn f =
  let collect_exception f =
    try
      f () ;
      None
    with e -> Some e
  in
  match collect_exception f with
  | None ->
      Test.fail
        "[check_raises] %s: expecting %s, got nothing."
        msg
        (Printexc.to_string exn)
  | Some e ->
      if e <> exn then
        Test.fail
          "[check_raises] %s: expecting %s, got %s."
          msg
          (Printexc.to_string exn)
          (Printexc.to_string e)

let fail message = Test.fail "%s" message

let failf x = Format.kasprintf fail x

(* Some Octez tests use Format.eprintf directly.
   Not even in the tests but in the libraries themselves.
   We redirect the output to Tezt.Log.

   Ideally we would do the same for the Printf module, which is in particular
   called by QCheck_alcotest to print the seed, but the Printf module cannot
   redirect its output... *)
let redirect_formatter fmt =
  let buffer = Buffer.create 256 in
  Format.pp_set_formatter_output_functions fmt (Buffer.add_substring buffer)
  @@ fun () ->
  Log.debug "%s" (String.trim (Buffer.contents buffer)) ;
  Buffer.clear buffer

let () =
  redirect_formatter Format.std_formatter ;
  redirect_formatter Format.err_formatter
