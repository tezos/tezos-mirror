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

module type S = sig
  (** for substitution *)
  type error

  (** for substitution *)
  type 'error trace

  type tztrace = error trace

  type 'a tzresult = ('a, tztrace) result

  (** You can find a lot of information about the [Lwt_syntax] module in the
      error monad tutorial: https://tezos.gitlab.io/developer/error_monad.html
  *)
  module Lwt_syntax : module type of TzLwtreslib.Monad.Lwt_syntax

  (** You can find a lot of information about the [Result_syntax] module in the
      error monad tutorial: https://tezos.gitlab.io/developer/error_monad.html
  *)
  module Result_syntax : sig
    include module type of TzLwtreslib.Monad.Result_syntax

    (** [tzfail e] is for failing into the [tzresult] type. It wraps the given
        error in a trace. This is meant as syntactic sugar for a very common
        pattern that is otherwise written [fail (TzTrace.make e)]. *)
    val tzfail : 'error -> ('a, 'error trace) result

    val ( and* ) :
      ('a, 'error trace) result ->
      ('b, 'error trace) result ->
      ('a * 'b, 'error trace) result

    val ( and+ ) :
      ('a, 'error trace) result ->
      ('b, 'error trace) result ->
      ('a * 'b, 'error trace) result

    val tzjoin : (unit, 'error trace) result list -> (unit, 'error trace) result

    val tzall : ('a, 'error trace) result list -> ('a list, 'error trace) result

    val tzboth :
      ('a, 'error trace) result ->
      ('b, 'error trace) result ->
      ('a * 'b, 'error trace) result
  end

  (** You can find a lot of information about the [Lwt_result_syntax] module in the
      error monad tutorial: https://tezos.gitlab.io/developer/error_monad.html
  *)
  module Lwt_result_syntax : sig
    include module type of TzLwtreslib.Monad.Lwt_result_syntax

    (** [tzfail e] is for failing into the [tzresult Lwt.t] type. It wraps the
        given error in a trace. This is meant as syntactic sugar for a very
        common pattern that is otherwise written [fail (TzTrace.make e)]. *)
    val tzfail : 'error -> ('a, 'error trace) result Lwt.t

    val ( and* ) :
      ('a, 'error trace) result Lwt.t ->
      ('b, 'error trace) result Lwt.t ->
      ('a * 'b, 'error trace) result Lwt.t

    val ( and+ ) :
      ('a, 'error trace) result Lwt.t ->
      ('b, 'error trace) result Lwt.t ->
      ('a * 'b, 'error trace) result Lwt.t

    val tzjoin :
      (unit, 'error trace) result Lwt.t list ->
      (unit, 'error trace) result Lwt.t

    val tzall :
      ('a, 'error trace) result Lwt.t list ->
      ('a list, 'error trace) result Lwt.t

    val tzboth :
      ('a, 'error trace) result Lwt.t ->
      ('b, 'error trace) result Lwt.t ->
      ('a * 'b, 'error trace) result Lwt.t
  end

  val classify_trace : tztrace -> Error_classification.t

  val pp_print_trace : Format.formatter -> tztrace -> unit

  val pp_print_top_error_of_trace : Format.formatter -> tztrace -> unit

  val trace_encoding : tztrace Data_encoding.t

  val result_encoding : 'a Data_encoding.t -> 'a tzresult Data_encoding.t

  val record_trace : 'err -> ('a, 'err trace) result -> ('a, 'err trace) result

  val trace :
    'err -> ('b, 'err trace) result Lwt.t -> ('b, 'err trace) result Lwt.t

  val record_trace_eval :
    (unit -> 'err) -> ('a, 'err trace) result -> ('a, 'err trace) result

  val trace_eval :
    (unit -> 'err) ->
    ('b, 'err trace) result Lwt.t ->
    ('b, 'err trace) result Lwt.t

  val error_unless : bool -> 'err -> (unit, 'err trace) result

  val error_when : bool -> 'err -> (unit, 'err trace) result

  val fail_unless : bool -> 'err -> (unit, 'err trace) result Lwt.t

  val fail_when : bool -> 'err -> (unit, 'err trace) result Lwt.t

  val unless :
    bool -> (unit -> (unit, 'trace) result Lwt.t) -> (unit, 'trace) result Lwt.t

  val when_ :
    bool -> (unit -> (unit, 'trace) result Lwt.t) -> (unit, 'trace) result Lwt.t

  val dont_wait :
    (unit -> (unit, 'trace) result Lwt.t) ->
    ('trace -> unit) ->
    (exn -> unit) ->
    unit
end

module Make
    (Error : sig
      type error = ..

      include Sig.CORE with type error := error
    end)
    (Trace : Sig.TRACE)
    (Monad :
      Tezos_lwt_result_stdlib.Lwtreslib.TRACED_MONAD
        with type 'error trace := 'error Trace.trace) :
  S with type error := Error.error and type 'error trace := 'error Trace.trace =
struct
  module Lwt_syntax = Monad.Lwt_syntax

  module Result_syntax = struct
    include Monad.Result_syntax

    let tzfail = Monad.Traced_result_syntax.fail

    let ( and* ) = Monad.Traced_result_syntax.( and* )

    let ( and+ ) = Monad.Traced_result_syntax.( and+ )

    let tzboth = Monad.Traced_result_syntax.both

    let tzall = Monad.Traced_result_syntax.all

    let tzjoin = Monad.Traced_result_syntax.join
  end

  module Lwt_result_syntax = struct
    include Monad.Lwt_result_syntax

    let tzfail = Monad.Lwt_traced_result_syntax.fail

    let ( and* ) = Monad.Lwt_traced_result_syntax.( and* )

    let ( and+ ) = Monad.Lwt_traced_result_syntax.( and+ )

    let tzboth = Monad.Lwt_traced_result_syntax.both

    let tzall = Monad.Lwt_traced_result_syntax.all

    let tzjoin = Monad.Lwt_traced_result_syntax.join
  end

  (* default (traced-everywhere) helper types *)
  type tztrace = Error.error Trace.trace

  type 'a tzresult = ('a, tztrace) result

  let trace_encoding = Trace.encoding Error.error_encoding

  let result_encoding a_encoding =
    let open Data_encoding in
    let trace_encoding = obj1 (req "error" trace_encoding) in
    let a_encoding = obj1 (req "result" a_encoding) in
    union
      ~tag_size:`Uint8
      [
        case
          (Tag 0)
          a_encoding
          ~title:"Ok"
          (function Ok x -> Some x | _ -> None)
          (function res -> Ok res);
        case
          (Tag 1)
          trace_encoding
          ~title:"Error"
          (function Error x -> Some x | _ -> None)
          (function x -> Error x);
      ]

  let pp_print_trace = Trace.pp_print Error.pp

  let pp_print_top_error_of_trace = Trace.pp_print_top Error.pp

  let classify_trace trace =
    Trace.fold
      (fun c e -> Error_classification.combine c (Error.classify_error e))
      Error_classification.default
      trace

  let record_trace err result =
    match result with
    | Ok _ as res -> res
    | Error trace -> Error (Trace.cons err trace)

  let trace err f =
    let open Monad.Lwt_syntax in
    let* r = f in
    match r with
    | Error trace -> Lwt.return_error (Trace.cons err trace)
    | ok -> Lwt.return ok

  let record_trace_eval mk_err = function
    | Error trace ->
        let err = mk_err () in
        Error (Trace.cons err trace)
    | ok -> ok

  let trace_eval mk_err f =
    let open Monad.Lwt_syntax in
    let* r = f in
    match r with
    | Error trace ->
        let err = mk_err () in
        Lwt.return_error (Trace.cons err trace)
    | ok -> Lwt.return ok

  let error_unless cond exn =
    let open Monad.Traced_result_syntax in
    if cond then return_unit else fail exn

  let error_when cond exn =
    let open Monad.Traced_result_syntax in
    if cond then fail exn else return_unit

  let fail_unless cond exn =
    let open Monad.Lwt_traced_result_syntax in
    if cond then return_unit else fail exn

  let fail_when cond exn =
    let open Monad.Lwt_traced_result_syntax in
    if cond then fail exn else return_unit

  let unless cond f =
    if cond then Monad.Lwt_traced_result_syntax.return_unit else f ()

  let when_ cond f =
    if cond then f () else Monad.Lwt_traced_result_syntax.return_unit

  let dont_wait f err_handler exc_handler =
    let open Monad.Lwt_syntax in
    Lwt.dont_wait
      (fun () ->
        let* r = f () in
        match r with
        | Ok () -> Lwt.return_unit
        | Error trace ->
            err_handler trace ;
            Lwt.return_unit)
      exc_handler
end
