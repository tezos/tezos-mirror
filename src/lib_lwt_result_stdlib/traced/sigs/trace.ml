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

      The types ['error] and ['error trace] are kept separate (although they can
      be equal) to support cases such as the following:
      - [trace] are richer than [error], such as by including a
        timestamp, a filename, or some other such metadata.
      - [trace] is a [private] type or an [abstract] type and [error] is the
        type of argument to the functions that construct the private/abstract
        [trace].
      - [trace] is a collection of [error] and additional functions (not
        required by this library) allow additional manipulation. E.g., in the
        case of Tezos: errors are built into traces that can be grown.

     There is {e some} leeway about what traces are, what information they
     carry, etc. Beyond this leeway, Lwtreslib is opinionated about traces.
     Specifically, Lwtreslib has a notion of {e sequential} and {e parallel}
     composition. A trace can be either of the following.
     - A {e single-error trace}, i.e., the simplest possible trace that
       corresponds to a simple failure/issue/unexpected behaviour in the
       program. See [make].
     - A {e sequential trace}, i.e., a trace of errors where one precedes
       another. This is used to contextualise failures. E.g., in a high-level
       network handshaking function, a low-level I/O error may be built into a
       trace that shows how the low-level error caused a high-level issue). See
       [cons].
     - A {e parallel trace}, i.e., a trace of errors that happened in concurrent
       (or non-causally related) parts of the program. See [conp]. *)
  type 'error trace

  (** [make e] is a trace made of one single error. *)
  val make : 'error -> 'error trace

  (** [cons e t] is a trace made of the error [e] composed sequentially with the
      trace [t].

      Typically, this is used to give context to a low-level error.

{[
let query key =
  let open Lwt_result_syntax in
  let* c = connect_to_server () in
  let* r = send_query_over_connection c key in
  let* () = check_response r in
  return r

let query key =
  let open Lwt_syntax in
  let+ r = query_key in
  match r with
  | Ok r -> Ok r
  | Error trace -> Error (cons `Query_failure trace)
]} *)
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

      One possible extension can be found in [examples/traces/traces.ml]. *)
end
