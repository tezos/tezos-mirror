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
  else Lwt.pause () >>= fun () -> log_pause (n / 8)

(* Generators *)

(* Function generators *)

module Fn = struct
  let lambda s l =
    let open QCheck in
    make ~print:(fun _ -> s) (Gen.return l)

  let pred =
    QCheck.oneof
      [ lambda "(fun x _ -> x > 0)" (fun x _ -> x > 0);
        lambda "(fun _ y -> y < 0)" (fun _ y -> y < 0);
        lambda "(fun _ _ -> false)" (fun _ _ -> false);
        lambda "(fun _ _ -> true)" (fun _ _ -> true);
        lambda "(fun x y -> x < y)" (fun x y -> x < y) ]

  let basic_int =
    QCheck.(oneof [int; Gen.return 0 |> make; Gen.return 1 |> make])

  let arith =
    let open QCheck in
    let module O = Observable in
    fun2 O.int O.int int

  (* combinators *)
  let e (cond, QCheck.Fun (_, ok), QCheck.Fun (_, error)) x y =
    if cond x y then Ok (ok x y) else Error (error x y)

  let arith_e = QCheck.(map e (triple pred arith arith))

  let s (QCheck.Fun (_, pauses), QCheck.Fun (_, fn)) x y =
    log_pause (pauses x y) >|= fun () -> fn x y

  let arith_s = QCheck.(map s (pair arith arith))

  let es
      (cond, QCheck.Fun (_, pauses), QCheck.Fun (_, ok), QCheck.Fun (_, error))
      x y =
    log_pause (pauses x y)
    >|= fun () -> if cond x y then Ok (ok x y) else Error (error x y)

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
module Cond2Of = Apply

module FilterMapOf = struct
  let fns cond fn const elt =
    if cond const elt then Some (fn const elt) else None
end

(* error-aware wrappers *)

module IterEOf = struct
  let fn r fn y =
    r := fn !r y ;
    Ok ()

  let fn_e r fn y = fn !r y >|? fun t -> r := t
end

module IteriEOf = struct
  let fn r fn i y =
    r := fn !r (fn i y) ;
    Ok ()

  let fn_e r fn i y = fn i y >>? fun z -> fn !r z >|? fun t -> r := t
end

module Iter2EOf = struct
  let fn r fn x y =
    r := fn x y ;
    Ok ()

  let fn_e r fn x y = fn x y >|? fun t -> r := t
end

module FoldEOf = struct
  let fn fn acc elt = Ok (fn acc elt)

  let fn_e fn acc elt = fn acc elt
end

module Fold2EOf = struct
  let fn fn acc x y = Ok (fn acc (fn x y))

  let fn_e fn acc x y = fn x y >>? fn acc
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

module Cond2EOf = struct
  let fn fn x y = Ok (fn x y)

  let fn_e fn x y = fn x y
end

module FilterMapEOf = struct
  let fns cond fn const elt =
    Ok (if cond const elt then Some (fn const elt) else None)

  let fns_e cond fn const elt =
    cond const elt >|? fun b -> if b then Some (fn const elt) else None
end

(* lwt-aware wrappers *)

module IterSOf = struct
  let fn r fn y =
    r := fn !r y ;
    Lwt.return_unit

  let monotonous r fn const y =
    r := !r + fn const y ;
    Lwt.return_unit

  let fn_s r fn y = fn !r y >|= fun t -> r := t
end

module IteriSOf = struct
  let fn r fn i y =
    r := fn !r (fn i y) ;
    Lwt.return_unit

  let fn_s r fn i y = fn i y >>= fun z -> fn !r z >|= fun t -> r := t
end

module Iter2SOf = struct
  let fn r fn x y =
    r := fn x y ;
    Lwt.return_unit

  let fn_s r fn x y = fn x y >|= fun t -> r := t
end

module FoldSOf = struct
  let fn fn acc elt = Lwt.return (fn acc elt)

  let fn_s fn acc elt = fn acc elt
end

module Fold2SOf = struct
  let fn fn acc x y = Lwt.return (fn acc (fn x y))

  let fn_s fn acc x y = fn x y >>= fn acc
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

module Cond2SOf = struct
  let fn fn x y = Lwt.return (fn x y)

  let fn_s fn x y = fn x y
end

module FilterMapSOf = struct
  let fns cond fn const elt =
    Lwt.return (if cond const elt then Some (fn const elt) else None)

  let fns_s cond fn const elt =
    cond const elt >|= fun b -> if b then Some (fn const elt) else None
end

(* error-lwt-aware wrappers *)

module IterESOf = struct
  let fn r fn y =
    r := fn !r y ;
    unit_es

  let monotonous r fn const y =
    r := !r + fn const y ;
    unit_es

  let fn_e r fn y = Lwt.return @@ fn !r y >|=? fun t -> r := t

  let fn_s r fn y =
    fn !r y
    >|= fun t ->
    r := t ;
    Ok ()

  let fn_es r fn y = fn !r y >|=? fun t -> r := t
end

module IteriESOf = struct
  let fn r fn i y =
    r := fn !r (fn i y) ;
    unit_es

  let fn_e r fn i y =
    Lwt.return @@ fn i y
    >>=? fun z -> Lwt.return @@ fn !r z >|=? fun t -> r := t

  let fn_s r fn i y =
    fn i y
    >>= fun z ->
    fn !r z
    >|= fun t ->
    r := t ;
    Ok ()

  let fn_es r fn i y = fn i y >>=? fun z -> fn !r z >|=? fun t -> r := t
end

module Iter2ESOf = struct
  let fn r fn x y =
    r := fn x y ;
    unit_es

  let fn_e r fn x y = Lwt.return @@ fn x y >|=? fun t -> r := t

  let fn_s r fn x y =
    fn x y
    >|= fun t ->
    r := t ;
    Ok ()

  let fn_es r fn x y = fn x y >|=? fun t -> r := t
end

module FoldESOf = struct
  let fn fn acc elt = return (fn acc elt)

  let fn_e fn acc elt = Lwt.return @@ fn acc elt

  let fn_s fn acc elt = fn acc elt >>= Lwt.return_ok

  let fn_es fn acc elt = fn acc elt
end

module Fold2ESOf = struct
  let fn fn acc x y = return (fn acc (fn x y))

  let fn_e fn acc x y = Lwt.return @@ (fn x y >>? fn acc)

  let fn_s fn acc x y = fn x y >>= fn acc >>= Lwt.return_ok

  let fn_es fn acc x y = fn x y >>=? fn acc
end

module MapESOf = struct
  let fn const fn elt = return (fn const elt)

  let fn_e const fn elt = Lwt.return @@ fn const elt

  let fn_s const fn elt = fn const elt >>= Lwt.return_ok

  let fn_es const fn elt = fn const elt
end

module MapEPOf = struct
  let fn const fn elt = return (fn const elt)

  let fn_e const fn elt =
    match fn const elt with
    | Ok _ as ok ->
        Lwt.return ok
    | Error err ->
        fail err

  let fn_s const fn elt = fn const elt >>= Lwt.return_ok

  let fn_es const fn elt =
    fn const elt >>= function Ok ok -> return ok | Error err -> fail err

  let fn_ep const fn elt =
    fn const elt
    >>= function
    | Ok ok -> return ok | Error err -> fail (Support.Test_trace.make err)
end

module Map2ESOf = struct
  let fn fn x y = return (fn x y)

  let fn_e fn x y = Lwt.return @@ fn x y

  let fn_s fn x y = fn x y >>= Lwt.return_ok

  let fn_es fn x y = fn x y
end

module CondESOf = struct
  let fn fn const elt = return (fn const elt)

  let fn_es fn const elt = fn const elt
end

module Cond2ESOf = struct
  let fn fn x y = return (fn x y)

  let fn_es fn x y = fn x y
end

module FilterMapESOf = struct
  let fns cond fn const elt =
    return (if cond const elt then Some (fn const elt) else None)

  let fns_es cond fn const elt =
    cond const elt >|=? fun b -> if b then Some (fn const elt) else None
end

(* Data generators (we use lists of integers) *)

let one = QCheck.int

let many = QCheck.(list int)

let maybe = QCheck.(option int)

let manymany =
  let open QCheck in
  oneof
    [ map
        ~rev:(fun (input, _) -> input)
        (fun input -> (input, input))
        (list int);
      pair (list int) (list int) ]

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

let eq_s ?pp a b = Lwt_main.run (a >>= fun a -> b >|= fun b -> eq ?pp a b)

(** [eq_es] is a duplicate of {!eq_s} for consistency

   example:
   {[
   eq_s
     (let acc = ref init in
     M.iter_s (IterSOf.fn_s acc fn) input >|= fun () -> !acc)
     (M.fold_left_s (FoldSOf.fn_s fn) init input)

   eq_es
     (let acc = ref init in
     M.iter_es (IterESOf.fn acc fn) (M.of_list input) >|=? fun () -> !acc)
     (Lwt.return_ok @@ with_stdlib_iter (fn, init, input))
   ]}
*)
let eq_es ?pp (a : ('a, 'b) result Lwt.t) (b : ('a, 'b) result Lwt.t) =
  eq_s ?pp a b

let eq_es_ep ?pp es ep =
  Lwt_main.run
    ( es
    >>= fun es ->
    ep
    >|= fun ep ->
    match (es, ep) with
    | (Ok ok_es, Ok ok_ep) ->
        eq ?pp ok_es ok_ep
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
    | (Ok _, Error _) ->
        QCheck.Test.fail_report "Ok _ is not Error _"
    | (Error _, Ok _) ->
        QCheck.Test.fail_report "Error _ is not Ok _" )

let eq_ep ?pp a b =
  Lwt_main.run
    ( a
    >>= fun a ->
    b
    >|= fun b ->
    match (a, b) with
    | (Ok ok_es, Ok ok_ep) ->
        eq ?pp ok_es ok_ep
    | (Error _, Error _) ->
        true (* Not as precise as we could be, but precise enough *)
    | (Ok _, Error _) ->
        QCheck.Test.fail_report "Ok _ is not Error _"
    | (Error _, Ok _) ->
        QCheck.Test.fail_report "Error _ is not Ok _" )

module PP = struct
  let int = Format.pp_print_int

  let res ok error = Format.pp_print_result ~ok ~error

  let str = Format.pp_print_string

  let list elt = Format.pp_print_list ~pp_sep:Format.pp_print_space elt

  let bool = Format.pp_print_bool

  let trace = Support.Test_trace.pp
end
