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

open Support.Lib.Monad
open Lib_test.Qcheck_helpers

let rec log_pause n =
  if n <= 0 then Lwt.return_unit
  else
    let open Lwt_syntax in
    let* () = Lwt.pause () in
    log_pause (n / 8)

(* Generators *)

(* Function generators *)

module Fn = struct
  let lambda s l =
    let open QCheck in
    make ~print:(fun _ -> s) (Gen.return l)

  let pred =
    QCheck.oneof
      [
        lambda "(fun x _ -> x > 0)" (fun x _ -> x > 0);
        lambda "(fun _ y -> y < 0)" (fun _ y -> y < 0);
        lambda "(fun _ _ -> false)" (fun _ _ -> false);
        lambda "(fun _ _ -> true)" (fun _ _ -> true);
        lambda "(fun x y -> x < y)" (fun x y -> x < y);
      ]

  let basic_int =
    QCheck.(oneof [int; Gen.return 0 |> make; Gen.return 1 |> make])

  let arith =
    let open QCheck in
    fun2 Observable.int Observable.int int

  let predarith =
    let open QCheck in
    fun2 Observable.int Observable.int (option int)

  (* combinators *)
  let e (cond, QCheck.Fun (_, ok), QCheck.Fun (_, error)) x y =
    if cond x y then Ok (ok x y) else Error (error x y)

  let arith_e = QCheck.(map e (triple pred arith arith))

  let s (QCheck.Fun (_, pauses), QCheck.Fun (_, fn)) x y =
    let open Lwt_syntax in
    let+ () = log_pause (pauses x y) in
    fn x y

  let arith_s = QCheck.(map s (pair arith arith))

  let es
      (cond, QCheck.Fun (_, pauses), QCheck.Fun (_, ok), QCheck.Fun (_, error))
      x y =
    let open Lwt_syntax in
    let+ () = log_pause (pauses x y) in
    if cond x y then Ok (ok x y) else Error (error x y)

  let arith_es = QCheck.(map es (quad pred arith arith arith))
end

(* Wrappers for generated functions *)

(* immediate wrappers *)

module Apply = struct
  let fn fn x y = fn x y
end

module Apply2 = struct
  let fn fn x y z = fn x (fn y z)
end

module IterOf = struct
  let fn r fn y = r := fn !r y
end

module IteriOf = struct
  let fn r fn i y = r := fn !r (fn i y)
end

module Iter2Of = struct
  let fn r fn x y = r := fn !r (fn x y)
end

module FoldOf = Apply
module Fold2Of = Apply2

module MapOf = struct
  let fn const fn elt = fn const elt
end

module Map2Of = Apply
module CondOf = Apply
module CondMapOf = Apply
module Cond2Of = Apply

module FilterMapOf = struct
  let fns cond fn const elt =
    if cond const elt then Some (fn const elt) else None
end

module ConcatMapOf = struct
  let fns of_list fn consta constb elt = of_list [fn consta elt; fn constb elt]
end

(* error-aware wrappers *)

module IterEOf = struct
  open Result_syntax

  let fn r fn y =
    r := fn !r y ;
    return_unit

  let fn_e r fn y =
    let* t = fn !r y in
    r := t ;
    return_unit
end

module IteriEOf = struct
  open Result_syntax

  let fn r fn i y =
    r := fn !r (fn i y) ;
    return_unit

  let fn_e r fn i y =
    let* z = fn i y in
    let* t = fn !r z in
    r := t ;
    return_unit
end

module Iter2EOf = struct
  open Result_syntax

  let fn r fn x y =
    r := fn x y ;
    return_unit

  let fn_e r fn x y =
    let* t = fn x y in
    r := t ;
    return_unit
end

module FoldEOf = struct
  let fn fn acc elt = Ok (fn acc elt)

  let fn_e fn acc elt = fn acc elt
end

module Fold2EOf = struct
  open Result_syntax

  let fn fn acc x y = Ok (fn acc (fn x y))

  let fn_e fn acc x y =
    let* z = fn x y in
    fn acc z
end

module MapEOf = struct
  let fn const fn elt = Ok (fn const elt)

  let fn_e const fn elt = fn const elt
end

module Map2EOf = struct
  let fn fn x y = Ok (fn x y)

  let fn_e fn x y = fn x y
end

module CondEOf = struct
  let fn fn const elt = Ok (fn const elt)

  let fn_e fn const elt = fn const elt
end

module CondMapEOf = struct
  let fn fn const elt = Ok (fn const elt)

  let fn_e fn const elt = fn const elt
end

module Cond2EOf = struct
  let fn fn x y = Ok (fn x y)

  let fn_e fn x y = fn x y
end

module FilterMapEOf = struct
  open Result_syntax

  let fns cond fn const elt =
    Ok (if cond const elt then Some (fn const elt) else None)

  let fns_e cond fn const elt =
    let+ b = cond const elt in
    if b then Some (fn const elt) else None
end

module ConcatMapEOf = struct
  let fns of_list fn consta constb elt =
    Ok (of_list [fn consta elt; fn constb elt])
end

(* lwt-aware wrappers *)

module IterSOf = struct
  open Lwt_syntax

  let fn r fn y =
    r := fn !r y ;
    return_unit

  let monotonous r fn const y =
    r := !r + fn const y ;
    return_unit

  let fn_s r fn y =
    let* t = fn !r y in
    r := t ;
    return_unit
end

module IteriSOf = struct
  open Lwt_syntax

  let fn r fn i y =
    r := fn !r (fn i y) ;
    return_unit

  let fn_s r fn i y =
    let* z = fn i y in
    let* t = fn !r z in
    r := t ;
    return_unit
end

module Iter2SOf = struct
  open Lwt_syntax

  let fn r fn x y =
    r := fn x y ;
    return_unit

  let fn_s r fn x y =
    let* t = fn x y in
    r := t ;
    return_unit
end

module FoldSOf = struct
  let fn fn acc elt = Lwt.return (fn acc elt)

  let fn_s fn acc elt = fn acc elt
end

module Fold2SOf = struct
  open Lwt_syntax

  let fn fn acc x y = return (fn acc (fn x y))

  let fn_s fn acc x y =
    let* z = fn x y in
    fn acc z
end

module MapSOf = struct
  let fn const fn elt = Lwt.return (fn const elt)

  let fn_s const fn elt = fn const elt
end

module Map2SOf = struct
  let fn fn x y = Lwt.return (fn x y)

  let fn_s fn x y = fn x y
end

module CondSOf = struct
  let fn fn const elt = Lwt.return (fn const elt)

  let fn_s fn const elt = fn const elt
end

module CondMapSOf = struct
  let fn fn const elt = Lwt.return (fn const elt)

  let fn_s fn const elt = fn const elt
end

module Cond2SOf = struct
  let fn fn x y = Lwt.return (fn x y)

  let fn_s fn x y = fn x y
end

module FilterMapSOf = struct
  open Lwt_syntax

  let fns cond fn const elt =
    Lwt.return (if cond const elt then Some (fn const elt) else None)

  let fns_s cond fn const elt =
    let+ b = cond const elt in
    if b then Some (fn const elt) else None
end

module ConcatMapSOf = struct
  let fns of_list fn consta constb elt =
    Lwt.return (of_list [fn consta elt; fn constb elt])
end

(* error-lwt-aware wrappers *)

module IterESOf = struct
  open Lwt_result_syntax

  let fn r fn y =
    r := fn !r y ;
    return_unit

  let monotonous r fn const y =
    r := !r + fn const y ;
    return_unit

  let fn_e r fn y =
    let* t = Lwt.return @@ fn !r y in
    r := t ;
    return_unit

  let fn_s r fn y =
    let*! t = fn !r y in
    r := t ;
    return_unit

  let fn_es r fn y =
    let* t = fn !r y in
    r := t ;
    return_unit
end

module IteriESOf = struct
  open Lwt_result_syntax

  let fn r fn i y =
    r := fn !r (fn i y) ;
    return_unit

  let fn_e r fn i y =
    let* z = Lwt.return @@ fn i y in
    let* t = Lwt.return @@ fn !r z in
    r := t ;
    return_unit

  let fn_s r fn i y =
    let*! z = fn i y in
    let*! t = fn !r z in
    r := t ;
    return_unit

  let fn_es r fn i y =
    let* z = fn i y in
    let* t = fn !r z in
    r := t ;
    return_unit
end

module Iter2ESOf = struct
  open Lwt_result_syntax

  let fn r fn x y =
    r := fn x y ;
    return_unit

  let fn_e r fn x y =
    let* t = Lwt.return @@ fn x y in
    r := t ;
    return_unit

  let fn_s r fn x y =
    let* t = fn x y in
    r := t ;
    return_unit

  let fn_es r fn x y =
    let* t = fn x y in
    r := t ;
    return_unit
end

module FoldESOf = struct
  open Lwt_result_syntax

  let fn fn acc elt = return (fn acc elt)

  let fn_e fn acc elt = Lwt.return @@ fn acc elt

  let fn_s fn acc elt = Lwt_result.ok @@ fn acc elt

  let fn_es fn acc elt = fn acc elt
end

module Fold2ESOf = struct
  open Lwt_result_syntax

  let fn fn acc x y = return (fn acc (fn x y))

  let fn_e fn acc x y =
    let* z = Lwt.return @@ fn x y in
    Lwt.return @@ fn acc z

  let fn_s fn acc x y =
    let*! z = fn x y in
    let*! r = fn acc z in
    return r

  let fn_es fn acc x y =
    let* z = fn x y in
    fn acc z
end

module MapESOf = struct
  open Lwt_result_syntax

  let fn const fn elt = return (fn const elt)

  let fn_e const fn elt = Lwt.return @@ fn const elt

  let fn_s const fn elt = Lwt_result.ok @@ fn const elt

  let fn_es const fn elt = fn const elt
end

module MapEPOf = struct
  open Lwt_traced_result_syntax

  let fn const fn elt = return (fn const elt)

  let fn_e const fn elt =
    match fn const elt with
    | Ok _ as ok -> Lwt.return ok
    | Error err -> fail err

  let fn_s const fn elt = Lwt_result.ok @@ fn const elt

  let fn_es const fn elt =
    (* We need to transform an error into a trace. There are multiple ways to
       do so. This is one of them. *)
    let open Lwt_syntax in
    let+ r = fn const elt in
    Result.map_error Support.Test_trace.make r

  let fn_ep const fn elt =
    let open Lwt_syntax in
    let+ r = fn const elt in
    Result.map_error Support.Test_trace.make r
end

module Map2ESOf = struct
  open Lwt_traced_result_syntax

  let fn fn x y = return (fn x y)

  let fn_e fn x y = Lwt.return @@ fn x y

  let fn_s fn x y = Lwt_result.ok @@ fn x y

  let fn_es fn x y = fn x y
end

module CondESOf = struct
  let fn fn const elt = Lwt_result_syntax.return (fn const elt)

  let fn_es fn const elt = fn const elt
end

module CondMapESOf = struct
  let fn fn const elt = Lwt_result_syntax.return (fn const elt)

  let fn_es fn const elt = fn const elt
end

module Cond2ESOf = struct
  let fn fn x y = Lwt_result_syntax.return (fn x y)

  let fn_es fn x y = fn x y
end

module FilterMapESOf = struct
  open Lwt_traced_result_syntax

  let fns cond fn const elt =
    return (if cond const elt then Some (fn const elt) else None)

  let fns_es cond fn const elt =
    let+ b = cond const elt in
    if b then Some (fn const elt) else None
end

module ConcatMapESOf = struct
  open Lwt_traced_result_syntax

  let fns of_list fn consta constb elt =
    return (of_list [fn consta elt; fn constb elt])
end

(* Data generators (we use lists of integers) *)

let one = QCheck.int

let many = QCheck.(list int)

let maybe = QCheck.(option int)

let manymany =
  let open QCheck in
  oneof
    [
      map ~rev:(fun (input, _) -> input) (fun input -> (input, input)) (list int);
      pair (list int) (list int);
    ]

(* equality and lwt/error variants *)

let eq ?pp a b = qcheck_eq ?pp a b

(** [eq_e] is a duplicate of {!eq} for consistency

   example (simplified):
   {[
   eq (M.iter (IterOf.fn acc fn)) (M.fold_left fn init)
   eq_e (M.iter_e (IterEOf.fn_e acc fn)) (M.fold_left_e (fn_e fn) init)
   ]}
*)
let eq_e ?pp (a : ('a, 'trace) result) (b : ('a, 'trace) result) = eq ?pp a b

let eq_s ?pp a b =
  Lwt_main.run
    (let open Lwt_syntax in
    let+ a = a and+ b = b in
    eq ?pp a b)

(** [eq_es] is a duplicate of {!eq_s} for consistency

   example:
   {[
   eq_s
     Lwt_syntax.(
      let acc = ref init in
      let+ () = M.iter_s (IterSOf.fn_s acc fn) input in
      !acc)
     (M.fold_left_s (FoldSOf.fn_s fn) init input)

   eq_es
     Lwt_result_syntax.(
      let acc = ref init in
      let+ () = M.iter_es (IterESOf.fn acc fn) (M.of_list input) in
      !acc)
     (Lwt.return_ok @@ with_stdlib_iter (fn, init, input))
   ]}
*)
let eq_es ?pp (a : ('a, 'b) result Lwt.t) (b : ('a, 'b) result Lwt.t) =
  eq_s ?pp a b

let eq_es_ep ?pp es ep =
  Lwt_main.run
    (let open Lwt_syntax in
    let+ es = es and+ ep = ep in
    match (es, ep) with
    | (Ok ok_es, Ok ok_ep) -> eq ?pp ok_es ok_ep
    | (Error error_es, Error trace_ep) ->
        let trace_ep_has_error_es =
          Support.Test_trace.fold
            (fun has error -> has || error = error_es)
            false
            trace_ep
        in
        if trace_ep_has_error_es then true
        else
          QCheck.Test.fail_reportf
            "%d not in %a"
            error_es
            (Support.Test_trace.pp Format.pp_print_int)
            trace_ep
    | (Ok _, Error _) -> QCheck.Test.fail_report "Ok _ is not Error _"
    | (Error _, Ok _) -> QCheck.Test.fail_report "Error _ is not Ok _")

let eq_ep ?pp a b =
  Lwt_main.run
    (let open Lwt_syntax in
    let+ a = a and+ b = b in
    match (a, b) with
    | (Ok ok_es, Ok ok_ep) -> eq ?pp ok_es ok_ep
    | (Error _, Error _) ->
        true (* Not as precise as we could be, but precise enough *)
    | (Ok _, Error _) -> QCheck.Test.fail_report "Ok _ is not Error _"
    | (Error _, Ok _) -> QCheck.Test.fail_report "Error _ is not Ok _")

module PP = struct
  let int = Format.pp_print_int

  let res ok error = Format.pp_print_result ~ok ~error

  let str = Format.pp_print_string

  let list elt = Format.pp_print_list ~pp_sep:Format.pp_print_space elt

  let bool = Format.pp_print_bool

  let trace = Support.Test_trace.pp
end
