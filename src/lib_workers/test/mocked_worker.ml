(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

type void = |

type failure = Simple | Crash | RaiseExn | Shutdown

module Request = struct
  type ('a, 'b) t =
    | RqA : int -> (unit, string option) t
    | RqB : (unit, void) t
    | RqErr : failure -> (unit, [`SimpleError | `CrashError | `Shutdown]) t

  type view = View : _ t -> view

  let view req = View req

  let int_of_failure = function
    | Simple -> 0
    | Crash -> 1
    | RaiseExn -> 2
    | Shutdown -> 3

  let failure_of_int = function 1 -> Simple | 2 -> Crash | 3 | _ -> RaiseExn

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"RqA"
          (obj2 (req "request" (constant "a")) (req "val" int31))
          (function View (RqA i) -> Some ((), i) | _ -> None)
          (fun ((), i) -> View (RqA i));
        case
          (Tag 1)
          ~title:"RqB"
          (obj1 (req "request" (constant "b")))
          (function View RqB -> Some () | _ -> None)
          (fun () -> View RqB);
        case
          (Tag 2)
          ~title:"RqErr"
          (obj2
             (req "request" (constant "err"))
             (req "failure" Data_encoding.int8))
          (function
            | View (RqErr fl) -> Some ((), int_of_failure fl) | _ -> None)
          (fun ((), b) -> View (RqErr (failure_of_int b)));
      ]

  let pp ppf (View r) =
    match r with
    | RqA i -> Format.fprintf ppf "RqA %d" i
    | RqB -> Format.fprintf ppf "RqB"
    | RqErr _ -> Format.fprintf ppf "RqErr"
end

module Name = struct
  type t = string

  let encoding = Data_encoding.string

  let base = ["base"]

  let pp fmt = Format.fprintf fmt "%s"

  let equal = ( = )
end

module Dummy_event = struct
  type t = string

  let pp = Format.pp_print_string

  let encoding = Data_encoding.string

  let level _ = Internal_event.Debug
end

module Types = struct
  type parameters = int

  (** An accumulated history of completed requests modified by
      [on_completion] *)
  type state = string list ref
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)
