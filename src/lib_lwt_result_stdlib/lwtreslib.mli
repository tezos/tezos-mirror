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

(** Lwt- and result-aware replacements for parts of the Stdlib.

    This library aims to provide replacements to some parts of the Stdlib that:

    - do not raise exceptions (e.g., it shadows [Map.find] with [Map.find_opt]),
    - include traversal functions for Lwt (think [Lwt_list] for [List]),
      [result], and the combined Lwt-[result] monad.

    The aim is to allow the use of the standard OCaml data-structures within the
    context of Lwt and a result monad. *)

(** [Bare] is a simple combined-monad (as described above) replacement for parts
    of the Stdlib. It is intended to be opened to shadow some modules of
    [Stdlib].

    All values within the modules follow the same naming conventions:

    {ul
      {li Values and traversors with the [_e] suffix (e.g., {Bare.List.map_e})
          are within the result monad. The [e] stands for "error".}
      {li Values and traversors with the [_s] suffix (e.g., {Bare.List.map_s})
          are within the Lwt monad. The [s] stands for "sequential".}
      {li Traversors with the [_p] suffix (e.g., {Bare.List.map_p})
          are within the Lwt monad. Unlike [s]-traversors, the [p] traversors
          operate concurrently on all the elements of the collection. The [p]
          stands for "parallel" (for compatibility with Lwt, even though
          "parallel" traversors do not provide parallelism).}
      {li Values and traversors with the [_es] suffix (e.g., {Bare.List.map_es})
          are within the combined Lwt-result monad.}
    }

    In addition, traversors within the result monad ([_e]) and the combined
    Lwt-result monad ([_es]) have a consistent fail-early semantic: they stop at
    the first error. For example, the following code returns an [Error] and does
    not print anything:

{[
List.iter_e
   (fun x ->
      if x = "" then
         Error "empty string"
      else begin
         print_endline x;
         Ok ())
   [
      ""; (* This will cause the iteration to stop *)
      "this is not printed";
      "neither is this printed";
   ]
]}

    The module [WithExceptions] provides some exception-raising helpers to
    reduce the boilerplate that the library imposes. For example

{[
let auto_fold f xs =
   if operations = [] then
      raise (Invalid_argument "apply: empty list of operations")
   else
      let acc = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd xs in
      let xs = WithExceptions.Option.get ~loc:__LOC__ @@ List.tail xs in
      List.fold_left f x xs
]}
*)
module Bare : module type of Bare_structs.Structs

(** [Traced] is a functor to generate an advanced combined-monad replaceements
    for parts of the Stdlib. The generated module is similar to [Bare] with the
    addition of concurrent, result-aware traversal.

    Specifically, it adds traversors with the [_ep] suffix which operate
    concurrently on all the elements of a collection within the combined
    Lwt-result monad.

    All the [_ep] traversors have a best-effort semantic: they return a promise
    that is resolved only once all the concurrent promises have resolved, even
    if one of them fails. For example, the following code prints all the
    non-empty strings in an unspecified order before returning an [Error].

{[
List.iter_ep
   (fun x ->
      if x = "" then
         Lwt.return (Error "empty string")
      else begin
         print_endline x;
         Lwt.return (Ok ()))
   [
      ""; (* This will cause the iteration to error in the end *)
      "this is printed";
      "this is printed as well";
   ]
]}

    When an [Error] is returned in one or more of the concurrent promises, all
    the error payloads are combined into a trace. A trace is a data-structure
    that holds multiple errors together. The specific trace type and
    construction that holds the errors is specified by the functor argument.

    Example implementations of traces are provided in the [traces/] directory.
*)
module Traced (Trace : Traced_sigs.Trace.S) : sig
  module Monad :
    Traced_sigs.Monad.S with type 'error trace = 'error Trace.trace

  module Hashtbl :
    Traced_sigs.Hashtbl.LWTRESLIB_TRACED_HASHTBL_S
      with type 'error trace := 'error Trace.trace

  module List :
    Traced_sigs.List.LWTRESLIB_TRACED_LIST_S
      with type 'error trace := 'error Trace.trace

  module Map :
    Traced_sigs.Map.LWTRESLIB_TRACED_MAP_S
      with type 'error trace := 'error Trace.trace

  module Option : Traced_sigs.Option.S

  module Result : Traced_sigs.Result.S

  module Seq :
    Traced_sigs.Seq.LWTRESLIB_TRACED_SEQ_S
      with type 'error trace := 'error Trace.trace

  module Set :
    Traced_sigs.Set.LWTRESLIB_TRACED_SET_S
      with type 'error trace := 'error Trace.trace

  module WithExceptions : Traced_sigs.WithExceptions.S
end

module TzLwtreslib : sig
  module Monad :
    Traced_sigs.Monad.S
      with type 'error trace =
            'error Tezos_error_monad.Error_monad.TzTrace.trace

  module Hashtbl :
    Traced_sigs.Hashtbl.LWTRESLIB_TRACED_HASHTBL_S
      with type 'error trace :=
            'error Tezos_error_monad.Error_monad.TzTrace.trace

  module List :
    Traced_sigs.List.LWTRESLIB_TRACED_LIST_S
      with type 'error trace :=
            'error Tezos_error_monad.Error_monad.TzTrace.trace

  module Map :
    Traced_sigs.Map.LWTRESLIB_TRACED_MAP_S
      with type 'error trace :=
            'error Tezos_error_monad.Error_monad.TzTrace.trace

  module Option : Traced_sigs.Option.S

  module Result : Traced_sigs.Result.S

  module Seq :
    Traced_sigs.Seq.LWTRESLIB_TRACED_SEQ_S
      with type 'error trace :=
            'error Tezos_error_monad.Error_monad.TzTrace.trace

  module Set :
    Traced_sigs.Set.LWTRESLIB_TRACED_SET_S
      with type 'error trace :=
            'error Tezos_error_monad.Error_monad.TzTrace.trace

  module WithExceptions : Traced_sigs.WithExceptions.S
end
