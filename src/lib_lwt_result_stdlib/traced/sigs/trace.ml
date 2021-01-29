(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Traces are used when errors need to be composed. This is useful directly to
    the user: building traces of errors to keep track of successive failures. It
    is also useful to the parallel traversors of this library ([_ep]) to
    combine errors that were constructed concurrently. *)

module type S = sig
  (** [trace] are the errors as received from the monad. In other words,
      [trace] is the type of values that are seen when matching on [Error _]
      to, say, recover.

      The types [error] and ['error trace] are kept separate (although they can
      be equal) to support cases such as the following:
      - [trace] are richer than [error], such as by including a
        timestamp, a filename, or some other such metadata.
      - [trace] is slightly different and [private] and [error] is simply
        the type of argument to the functions that construct the private
        [trace].
      - [trace] is a collection of [error] and additional functions (not
        required by this library) allow additional manipulation. E.g., in the
        case of Tezos: errors are built into traces that can be grown.
  *)
  type 'error trace

  (** [make e] is a trace made of one single error. *)
  val make : 'error -> 'error trace

  (** [cons e t] is a trace made of the error [e] composed sequentially with the
      trace [t]. *)
  val cons : 'error -> 'error trace -> 'error trace

  (** [cons_list error errors] is the sequential composition of all the errors
      passed as parameters. It is equivalent to folding [cons] over
      [List.rev (error :: errors)] but more efficient.

      Note that [error] and [errors] are separated as parameters to enforce that
      empty traces cannot be constructed. The recommended use is:
{[
   match a_bunch_of_errors with
   | [] -> Ok () (* or something else depending on the context *)
   | error :: errors -> Error (cons_list error errors)
]}
  *)
  val cons_list : 'error -> 'error list -> 'error trace

  (** [conp t1 t2] is a trace made of the traces [t1] and [t2] composed
      concurrently. *)
  val conp : 'error trace -> 'error trace -> 'error trace

  (** [conp_list trace traces] is the parallel composition of all the traces
      passed as parameters. It is equivalent to
      [List.fold_left conp trace traces] but more efficient.

      Note that [trace] and [traces] are separated as parameters to enforce that
      empty traces cannot be constructed. The recommended use is:
{[
   match a_bunch_of_traces with
   | [] -> Ok () (* or something else depending on the context *)
   | trace :: traces -> Error (conp_list trace traces)
]}
  *)
  val conp_list : 'err trace -> 'err trace list -> 'err trace

  (** Note that the Lwtreslib's library does not require it, but it is
      recommended that you also make, for your own use, a pretty-printing
      function as well as some introspection functions.

      Recommneded additions include:

{[
(** [pp_print] pretty-prints a trace of errors *)
val pp_print :
  (Format.formatter -> 'err -> unit) ->
  Format.formatter ->
  'err trace ->
  unit

(** [pp_print_top] pretty-prints the top errors of the trace *)
val pp_print_top :
  (Format.formatter -> 'err -> unit) ->
  Format.formatter ->
  'err trace ->
  unit

val encoding :
  'error -> 'error Data_encoding.t -> 'error trace Data_encoding.t

(** [fold f init trace] traverses the trace (in an unspecified manner) so that
    [init] is folded over each of the error within [trace] by [f]. Typical use
    is to find the worst error, to check for the presence of a given error,
    etc. *)
val fold : ('a -> 'error -> 'a) -> 'a -> 'error trace -> 'a

(** Recovering from errors

    This module ([TRACE]) focuses on giving a sound semantic on top of
    abstract traces. The following functions are meant for the higher-level
    monad (the one that gives combinators for [('a, 'e trace) result]) to
    handle the [Error] constructor. The functions below are necessary because
    the payload of the [Error] constructor is abstract and handling it should
    be delegated to this here [TRACE] module.

    Savlage

    The function [salvage] and its variants ([salvage_s], etc.) are for
    partial recovery: you salvage what you can.

    In [salvage] (and variants), there is a single handler that returns an
    option type. The handler is ignored if [salvage] is passed [Ok _] as
    argument. Otherwise, if [salvage] is passed [Error _], the handler is
    called.

    The handler returns [Some x] to indicate a successful
    recovery and the function returns the salvaged value [x]. The handler
    returns [None] to indicate a failed recovery and the function returns the
    original failure.

    Note, for example, the following trivial cases:
    [salvage (fun _ -> Some 0) t = Ok 0],
    [salvage (fun _ -> None) t = Error t].

    When [salvage] (or variants) is passed [Error t] as argument, its handler
    is applied to each of the {e recoverable errors} from the trace [t]. There
    is a single recoverable error in every trace:
    - the single error of a singleton trace (i.e., [e] in [make e]),
    - the most recent error of a sequential trace (i.e., [e] in [cons e _]),
    - the recoverable error of one unsepcified member of either of parallel
      traces (i.e., either of the recoverable errors from [tl] or [tr] in
      [conp tl tr]).

    In the future, there will be one-or-more recoverable errors per trace.
    Specifically, it will be possible to recover from any of multiple parallel
    traces.

    Recover

    The function [recover] and its variants ([recover_s], etc.) are for total
    recovery: you simply {e recover}.

    The function [recover] and its variants are similar to [salvage] and its
    variants. However, they take one additional parameter: a total handler for
    traces. This total handler is called if the partial handler has returned
    [None] for all of the recoverable errors. The total handler returns a
    non-option type and thus it cannot indicate failed recovery.

    Because [recover] makes a full recovery, it returns a non-result type.

    Note, for example, the following trivial cases:
    [recover (fun _ -> Some 0) (fun _ -> 1) t = 0],
    [recover (fun _ -> None) (fun _ -> 1) t = 1].

 *)

val salvage :
  ('error -> 'a option) -> 'error trace -> ('a, 'error trace) result

val salvage_s :
  ('error -> 'a Lwt.t option) ->
  'error trace ->
  ('a, 'error trace) result Lwt.t

val salvage_e :
  ('error -> ('a, 'error trace) result option) ->
  'error trace ->
  ('a, 'error trace) result

val salvage_es :
  ('error -> ('a, 'error trace) result Lwt.t option) ->
  'error trace ->
  ('a, 'error trace) result Lwt.t

val recover :
  ('error -> 'a option) -> ('error trace -> 'a) -> 'error trace -> 'a

val recover_s :
  ('error -> 'a Lwt.t option) ->
  ('error trace -> 'a Lwt.t) ->
  'error trace ->
  'a Lwt.t

val recover_e :
  ('error -> ('a, 'error trace) result option) ->
  ('error trace -> ('a, 'error trace) result) ->
  'error trace ->
  ('a, 'error trace) result

val recover_es :
  ('error -> ('a, 'error trace) result Lwt.t option) ->
  ('error trace -> ('a, 'error trace) result Lwt.t) ->
  'error trace ->
  ('a, 'error trace) result Lwt.t

val wrap : ('a -> 'b) -> 'a trace -> 'b trace
]}

  *)
end
