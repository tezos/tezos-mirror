(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Categories of error *)
type error_category =
  [ `Branch  (** Errors that may not happen in another context *)
  | `Temporary  (** Errors that may not happen in a later context *)
  | `Permanent  (** Errors that will happen no matter the context *) ]

let string_of_category = function
  | `Permanent ->
      "permanent"
  | `Temporary ->
      "temporary"
  | `Branch ->
      "branch"

let combine_category c1 c2 =
  match (c1, c2) with
  | (`Permanent, _) | (_, `Permanent) ->
      `Permanent
  | (`Branch, _) | (_, `Branch) ->
      `Branch
  | (`Temporary, `Temporary) ->
      `Temporary

module type PREFIX = sig
  (** The identifier for parts of the code that need their own error monad. It
      is expected (but not enforced) that the identifier:
      is printable and easy to read once printed, and
      ends with a separator (typically a dot or a dash). *)
  val id : string
end

module type CORE = sig
  type error

  val error_encoding : error Data_encoding.t

  val pp : Format.formatter -> error -> unit
end

module type EXT = sig
  type error = ..

  (** The error data type is extensible. Each module can register specialized
      error serializers
      [id] unique name of this error. Ex.: overflow_time_counter
      [title] more readable name. Ex.: Overflow of time counter
      [description] human readable description. Ex.: The time counter overflowed while computing delta increase
      [pp] formatter used to pretty print additional arguments. Ex.: The time counter overflowed while computing delta increase. Previous value %d. Delta: %d
      [encoder] [decoder] data encoding for this error. If the error has no value, specify Data_encoding.empty
  *)
  val register_error_kind :
    error_category ->
    id:string ->
    title:string ->
    description:string ->
    ?pp:(Format.formatter -> 'err -> unit) ->
    'err Data_encoding.t ->
    (error -> 'err option) ->
    ('err -> error) ->
    unit

  (** Same as [register_error_kind] but allow errors to wrap other errors.

      The encoding argument is a function which will be given the encoding of
      errors as argument so that you can encode errors in errors using a fixpoint.

      Another difference with [register_error_kind] is that [pp] is mandatory. *)
  val register_recursive_error_kind :
    error_category ->
    id:string ->
    title:string ->
    description:string ->
    pp:(Format.formatter -> 'err -> unit) ->
    (error Data_encoding.t -> 'err Data_encoding.t) ->
    (error -> 'err option) ->
    ('err -> error) ->
    unit

  (** Classify an error using the registered kinds *)
  val classify_error : error -> error_category

  (** Catch all error when 'serializing' an error. *)
  type error +=
    private
    | Unclassified of string
          (** Catch all error when 'deserializing' an error. *)

  type error += private Unregistered_error of Data_encoding.json

  (** An error serializer *)
  val json_of_error : error -> Data_encoding.json

  val error_of_json : Data_encoding.json -> error

  (** {2 Error documentation} *)

  (** Error information *)
  type error_info = {
    category : error_category;
    id : string;
    title : string;
    description : string;
    schema : Data_encoding.json_schema;
  }

  val pp_info : Format.formatter -> error_info -> unit

  (** Retrieves information of registered errors *)
  val get_registered_errors : unit -> error_info list
end

module type WITH_WRAPPED = sig
  type error

  module type Wrapped_error_monad = sig
    type unwrapped = ..

    include CORE with type error := unwrapped

    include EXT with type error := unwrapped

    val unwrap : error -> unwrapped option

    val wrap : unwrapped -> error
  end

  val register_wrapped_error_kind :
    (module Wrapped_error_monad) ->
    id:string ->
    title:string ->
    description:string ->
    unit
end

module type TRACE = sig
  (** [trace] is abstract in this interface but it is made concrete in the
      instantiated error monad (see [error_monad.mli]).

      The idea of abstracting the trace is so that it can evolve more easily.
      Eventually, we can make the trace abstract in the instantiated error
      monad, we can have different notions of traces for the protocol and the
      shell, etc. *)
  type 'err trace

  (** [make e] makes a singleton trace, the simplest of traces that carries a
      single error. *)
  val make : 'error -> 'error trace

  (** [cons e t] (construct sequential) constructs a sequential trace. This is
      for tracing events/failures/things that happen one after the other,
      generally one as a consequence of the other. E.g.,

      [let file_handle =
         match attempt_open name with
         | Ok handle -> Ok handle
         | Error error ->
               let trace = make error in
               match attempt_create name with
               | Ok handle -> Ok handle
               | Error error -> Error (cons error trace)
      ]

      When you are within the error monad itself, you should build traces using
      the [record_trace], [trace], [record_trace_eval] and [trace_eval]
      functions directly. You should rarely need to build traces manually using
      [cons]. This here function can be useful in the case where you are at the
      interface of the error monad. *)
  val cons : 'error -> 'error trace -> 'error trace

  (** [cons_list error errors] is the sequential composition of all the errors
      passed as parameters. It is equivalent to folding [cons] over
      [List.rev error :: errors] but more efficient.

      Note that [error] and [errors] are separated as parameters to enforce that
      empty traces cannot be constructed. The recommended use is:
{[
   match all_errors with
   | [] -> Ok () (* or something else depending on the context *)
   | error :: errors -> Error (cons_list error errors)
]}

      When you are within the error monad itself, you should build traces using
      the [record_trace], [trace], [record_trace_eval] and [trace_eval]
      functions directly. You should rarely need to build traces manually using
      [cons_list]. This here function can be useful in the case where you are at
      the interface of the error monad. *)
  val cons_list : 'error -> 'error list -> 'error trace

  (** [conp t1 t2] (construct parallel) construct a parallel trace. This is for
      tracing events/failure/things that happen concurrently, in parallel, or
      simply independently of each other. E.g.,

      [let fetch_density () =
         let area = fetch_area () in
         let population = fetch_population () in
         match area, population with
         | Ok area, Ok population -> Ok (population / area)
         | Error trace, Ok _ | Ok _, Error trace -> Error trace
         | Error trace1, Error trace2 -> Error (conp trace1 trace2)
      ]

      When you are within the error monad itself, you should rarely need to
      build traces manually using [conp]. The result-concurrent traversors will
      return parallel traces when appropriate, and so will [join_e], [join_ep],
      [both_e], [both_ep], [all_e] and [all_ep]. *)
  val conp : 'error trace -> 'error trace -> 'error trace

  (** [conp_list trace traces] is the parallel composition of all the traces
      passed as parameters. It is equivalent to [List.fold_left conp trace traces]
      but more efficient.

      Note that [trace] and [traces] are separated as parameters to enforce that
      empty traces cannot be constructed. The recommended use is:
{[
   match all_traces with
   | [] -> Ok () (* or something else depending on the context *)
   | trace :: traces -> Error (conp_list trace traces)
]}

      When you are within the error monad itself, you should rarely need to
      build traces manually using [conp]. The result-concurrent traversors will
      return parallel traces when appropriate, and so will [join_e], [join_ep],
      [both_e], [both_ep], [all_e] and [all_ep]. *)
  val conp_list : 'err trace -> 'err trace list -> 'err trace

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

  val encoding : 'error Data_encoding.t -> 'error trace Data_encoding.t

  (** [fold f init trace] traverses the trace (in an unspecified manner) so that
      [init] is folded over each of the error within [trace] by [f]. Typical use
      is to find the worst error, to check for the presence of a given error,
      etc. *)
  val fold : ('a -> 'error -> 'a) -> 'a -> 'error trace -> 'a
end

module type MONAD = sig
  (** To be subsituted/constrained *)
  type 'err trace

  (** Successful result *)
  val ok : 'a -> ('a, 'trace) result

  val ok_unit : (unit, 'trace) result

  val ok_none : ('a option, 'trace) result

  val ok_some : 'a -> ('a option, 'trace) result

  val ok_nil : ('a list, 'trace) result

  val ok_true : (bool, 'trace) result

  val ok_false : (bool, 'trace) result

  (** Successful return *)
  val return : 'a -> ('a, 'trace) result Lwt.t

  (** Successful return of [()] *)
  val return_unit : (unit, 'trace) result Lwt.t

  (** Successful return of [None] *)
  val return_none : ('a option, 'trace) result Lwt.t

  (** [return_some x] is a successful return of [Some x] *)
  val return_some : 'a -> ('a option, 'trace) result Lwt.t

  (** Successful return of [[]] *)
  val return_nil : ('a list, 'trace) result Lwt.t

  (** Successful return of [true] *)
  val return_true : (bool, 'trace) result Lwt.t

  (** Successful return of [false] *)
  val return_false : (bool, 'trace) result Lwt.t

  (** Erroneous result *)
  val error : 'err -> ('a, 'err trace) result

  (** Erroneous return *)
  val fail : 'err -> ('a, 'err trace) result Lwt.t

  (** Infix operators for monadic binds/maps. All operators follow this naming
      convention:
      - the first character is [>]
      - the second character is [>] for [bind] and [|] for [map]
      - the next character is [=] for Lwt or [?] for Error
      - the next character (if present) is [=] for Lwt or [?] for Error, it is
      only used for operator that are within both monads.
  *)

  (** Lwt's bind reexported. Following Lwt's convention, in this operator and
      the ones below, [=] indicate we operate within Lwt. *)
  val ( >>= ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t

  (** Lwt's map reexported. The [|] indicates a map rather than a bind. *)
  val ( >|= ) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t

  (** Non-Lwt bind operator. In this operator and the ones below, [?] indicates
      that we operate within the error monad. *)
  val ( >>? ) :
    ('a, 'trace) result -> ('a -> ('b, 'trace) result) -> ('b, 'trace) result

  (** Non-Lwt map operator. *)
  val ( >|? ) : ('a, 'trace) result -> ('a -> 'b) -> ('b, 'trace) result

  (** Combined bind operator. The [=?] indicates that the operator acts within
      the combined error-lwt monad. *)
  val ( >>=? ) :
    ('a, 'trace) result Lwt.t ->
    ('a -> ('b, 'trace) result Lwt.t) ->
    ('b, 'trace) result Lwt.t

  (** Combined map operator. *)
  val ( >|=? ) :
    ('a, 'trace) result Lwt.t -> ('a -> 'b) -> ('b, 'trace) result Lwt.t

  (** Injecting bind operator. This is for transitioning from the simple Error
      monad to the combined Error-Lwt monad.

      Note the order of the character: it starts with the error monad marker [?]
      and has the Lwt monad marker later. This hints at the role of the operator
      to transition into Lwt. *)
  val ( >>?= ) :
    ('a, 'trace) result ->
    ('a -> ('b, 'trace) result Lwt.t) ->
    ('b, 'trace) result Lwt.t

  (** Injecting map operator. *)
  val ( >|?= ) :
    ('a, 'trace) result -> ('a -> 'b Lwt.t) -> ('b, 'trace) result Lwt.t

  (** Enrich an error report (or do nothing on a successful result) manually *)
  val record_trace : 'err -> ('a, 'err trace) result -> ('a, 'err trace) result

  (** Automatically enrich error reporting on stack rewind *)
  val trace :
    'err -> ('b, 'err trace) result Lwt.t -> ('b, 'err trace) result Lwt.t

  (** Same as record_trace, for unevaluated error *)
  val record_trace_eval :
    (unit -> ('err, 'err trace) result) ->
    ('a, 'err trace) result ->
    ('a, 'err trace) result

  (** Same as trace, for unevaluated Lwt error *)
  val trace_eval :
    (unit -> ('err, 'err trace) result Lwt.t) ->
    ('b, 'err trace) result Lwt.t ->
    ('b, 'err trace) result Lwt.t

  (** Error on failed assertion *)
  val error_unless : bool -> 'err -> (unit, 'err trace) result

  val error_when : bool -> 'err -> (unit, 'err trace) result

  (** Erroneous return on failed assertion *)
  val fail_unless : bool -> 'err -> (unit, 'err trace) result Lwt.t

  val fail_when : bool -> 'err -> (unit, 'err trace) result Lwt.t

  val unless :
    bool ->
    (unit -> (unit, 'trace) result Lwt.t) ->
    (unit, 'trace) result Lwt.t

  val when_ :
    bool ->
    (unit -> (unit, 'trace) result Lwt.t) ->
    (unit, 'trace) result Lwt.t

  (** Wrapper around [Lwt_utils.dont_wait] *)
  val dont_wait :
    (exn -> unit) ->
    ('trace -> unit) ->
    (unit -> (unit, 'trace) result Lwt.t) ->
    unit

  (** A few aliases for Lwt functions *)
  val join_p : unit Lwt.t list -> unit Lwt.t

  val all_p : 'a Lwt.t list -> 'a list Lwt.t

  val both_p : 'a Lwt.t -> 'b Lwt.t -> ('a * 'b) Lwt.t

  (** Similar functions in the error monad *)
  val join_e : (unit, 'err trace) result list -> (unit, 'err trace) result

  val all_e : ('a, 'err trace) result list -> ('a list, 'err trace) result

  val both_e :
    ('a, 'err trace) result ->
    ('b, 'err trace) result ->
    ('a * 'b, 'err trace) result

  (** Similar functions in the combined monad *)
  val join_ep :
    (unit, 'err trace) result Lwt.t list -> (unit, 'err trace) result Lwt.t

  val all_ep :
    ('a, 'err trace) result Lwt.t list -> ('a list, 'err trace) result Lwt.t

  val both_ep :
    ('a, 'err trace) result Lwt.t ->
    ('b, 'err trace) result Lwt.t ->
    ('a * 'b, 'err trace) result Lwt.t
end

module type MONAD_EXT = sig
  (** for substitution *)
  type error

  type 'error trace

  type tztrace = error trace

  type 'a tzresult = ('a, tztrace) result

  val classify_errors : tztrace -> error_category

  (* This is for legacy, for backwards compatibility, there are old names *)

  (* NOTE: Right now we leave this [pp_print_error] named as is. Later on we
     might rename it to [pp_print_trace]. *)
  val pp_print_error : Format.formatter -> error trace -> unit

  (** Pretty prints a trace as the message of its first error *)
  val pp_print_error_first : Format.formatter -> error trace -> unit

  val trace_encoding : error trace Data_encoding.t

  (** A serializer for result of a given type *)
  val result_encoding : 'a Data_encoding.t -> 'a tzresult Data_encoding.t
end
