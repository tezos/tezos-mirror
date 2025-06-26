(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2022-2025 Nomadic Labs. <contact@nomadic-labs.com>          *)
(*                                                                           *)
(*****************************************************************************)

type void = |

type failure = Simple | Crash | RaiseExn

module Request = struct
  type ('a, 'b) t =
    | RqA : int -> (unit, string option) t
    | RqB : (unit, void) t
    | RqErr : failure -> (unit, [`SimpleError | `CrashError]) t

  type view = View : _ t -> view

  let view req = View req

  let int_of_failure = function Simple -> 0 | Crash -> 1 | RaiseExn -> 2

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

let emit_event name =
  let module E = struct
    let section = [name]

    include Internal_event.Simple

    let request_received =
      declare_1
        ~section
        ~name:"request_received"
        ~msg:"request {req} received"
        ~level:Notice
        ~pp1:Request.pp
        ("req", Request.encoding)
  end in
  E.emit E.request_received

type error += TzCrashError

let () =
  Error_monad.register_error_kind
    `Permanent
    ~id:"lib_bees.test.mocked_worker.TzCrashError"
    ~title:"Worker request crash error"
    ~description:"A request sent to the worker raised an unhandled error"
    Data_encoding.(obj1 (req "error" (constant "TzCrashError")))
    (function TzCrashError -> Some () | _ -> None)
    (fun () -> TzCrashError)
