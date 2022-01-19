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

module type ERROR_CATEGORY = sig
  type t

  val default_category : t

  val string_of_category : t -> string

  val classify : t -> Error_classification.t
end

module type PREFIX = sig
  (** The identifier for parts of the code that need their own error monad. It
      is expected (but not enforced) that the identifier:
      is printable and easy to read once printed, and
      ends with a separator (typically a dot or a dash). *)
  val id : string
end

module type CORE = sig
  type error_category

  type error = ..

  val string_of_category : error_category -> string

  val error_encoding : error Data_encoding.t

  val pp : Format.formatter -> error -> unit

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
  val classify_error : error -> Error_classification.t

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

  (**
     [find_info_of_error e] retrieves the `error_info` associated with the
     given error `e`.

     @raise Invalid_argument if the error is a wrapped error from another monad

     @raise Not_found if the error's constructor has not been registered
  *)
  val find_info_of_error : error -> error_info

  (** Retrieves information of registered errors *)
  val get_registered_errors : unit -> error_info list
end

module type WITH_WRAPPED = sig
  type error

  module type Wrapped_error_monad = sig
    (**
       The purpose of this module is to wrap a specific error monad [E]
       into a more general error monad [Eg].

       The user implementing such an interface is responsible to
       maintain the following assertions
       - The [Eg] error is extended locally with a specific constructor [C]
       - [unwrapped] is equal to the [error] type of [E]
       - [wrap] builds an [Eg] [error] value from an [E] [error] value
       - [unwrap] matches on [Eg] error cases and extracts [E]
         error value from [C]

       As a reference implementation,
       see src/lib_protocol_environment/environment_V3.ml
    *)

    type unwrapped = ..

    include CORE with type error := unwrapped

    (** [unwrap e] returns [Some] when [e] matches variant constructor [C]
        and [None] otherwise *)
    val unwrap : error -> unwrapped option

    (** [wrap e] returns a general [error] from a specific [unwrapped]
        error [e] *)
    val wrap : unwrapped -> error
  end

  (** Same as [register_error_kind] but for a wrapped error monad.
      The codec is defined in the module parameter. It makes the category
      of the error [Wrapped] instead of [Main].
  *)
  val register_wrapped_error_kind :
    (module Wrapped_error_monad) ->
    id:string ->
    title:string ->
    description:string ->
    unit
end

module type TRACE = sig
  (** The [trace] type (included as part of the
      [Tezos_lwt_result_stdlib.Lwtreslib.TRACE] module is abstract in this
      interface but it is made concrete in the instantiated error monad (see
      [error_monad.mli]).

      The idea of abstracting the trace is so that it can evolve more easily.
      Eventually, we can make the trace abstract in the instantiated error
      monad, we can have different notions of traces for the protocol and the
      shell, etc. *)
  include Tezos_lwt_result_stdlib.Lwtreslib.TRACE

  (** [pp_print] pretty-prints a trace of errors *)
  val pp_print :
    (Format.formatter -> 'err -> unit) -> Format.formatter -> 'err trace -> unit

  (** [pp_print_top] pretty-prints the top errors of the trace *)
  val pp_print_top :
    (Format.formatter -> 'err -> unit) -> Format.formatter -> 'err trace -> unit

  val encoding : 'error Data_encoding.t -> 'error trace Data_encoding.t

  (** [fold f init trace] traverses the trace (in an unspecified manner) so that
      [init] is folded over each of the error within [trace] by [f]. Typical use
      is to find the worst error, to check for the presence of a given error,
      etc. *)
  val fold : ('a -> 'error -> 'a) -> 'a -> 'error trace -> 'a
end

(** [MONAD_EXTENSION] is the Tezos-specific extension to the generic monad
    provided by Lwtreslib. It sets some defaults (e.g., it defaults traced
    failures), it brings some qualified identifiers into the main unqualified
    part (e.g., [return_unit]), it provides some tracing helpers and some
    in-monad assertion checks. *)
module type MONAD_EXTENSION = sig
  (** for substitution *)
  type error

  (** for substitution *)
  type 'error trace

  type tztrace = error trace

  type 'a tzresult = ('a, tztrace) result

  val classify_trace : tztrace -> Error_classification.t

  val return : 'a -> ('a, 'e) result Lwt.t

  val return_unit : (unit, 'e) result Lwt.t

  val return_none : ('a option, 'e) result Lwt.t

  val return_some : 'a -> ('a option, 'e) result Lwt.t

  val return_nil : ('a list, 'e) result Lwt.t

  val return_true : (bool, 'e) result Lwt.t

  val return_false : (bool, 'e) result Lwt.t

  (** more globals *)
  val ( >>= ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t

  val ( >|= ) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t

  val ok : 'a -> ('a, 'e) result

  val error : 'e -> ('a, 'e trace) result

  val ( >>? ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result

  val ( >|? ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result

  val fail : 'e -> ('a, 'e trace) result Lwt.t

  val ( >>=? ) :
    ('a, 'e) result Lwt.t ->
    ('a -> ('b, 'e) result Lwt.t) ->
    ('b, 'e) result Lwt.t

  val ( >|=? ) : ('a, 'e) result Lwt.t -> ('a -> 'b) -> ('b, 'e) result Lwt.t

  val ( >>?= ) :
    ('a, 'e) result -> ('a -> ('b, 'e) result Lwt.t) -> ('b, 'e) result Lwt.t

  val ( >|?= ) : ('a, 'e) result -> ('a -> 'b Lwt.t) -> ('b, 'e) result Lwt.t

  (* Pretty-prints an error trace. *)
  val pp_print_trace : Format.formatter -> error trace -> unit

  (** Pretty-prints the top error of a trace *)
  val pp_print_top_error_of_trace : Format.formatter -> error trace -> unit

  val trace_encoding : error trace Data_encoding.t

  (** A serializer for result of a given type *)
  val result_encoding : 'a Data_encoding.t -> 'a tzresult Data_encoding.t

  (** [record_trace err res] is either [res] if [res] is [Ok _], or it is
      [Error (Trace.cons err tr)] if [res] is [Error tr].

      In other words, [record_trace err res] enriches the trace that is carried
      by [res] (if it is carrying a trace) with the error [err]. It leaves [res]
      untouched if [res] is not carrying a trace.

      You can use this to add high-level information to potential low-level
      errors. E.g.,

{[
record_trace
   Failure_to_load_config
   (load_data_from_file config_encoding config_file_name)
]}

      Note that [record_trace] takes a {e fully evaluated} error [err] as
      argument. It means that, whatever the value of the result [res], the error
      [err] is evaluated. This is not an issue if the error is a simple
      expression (a literal or a constructor with simple parameters). However,
      for any expression that is more complex (e.g., one that calls a function)
      you should prefer [record_trace_eval]. *)
  val record_trace : 'err -> ('a, 'err trace) result -> ('a, 'err trace) result

  (** [trace] is identical to [record_trace] but applies to a promise. More
      formally, [trace err p] is a promise that resolves to [Ok v] if [p]
      resolves to [Ok v], or it resolves to [Error (Trace.cons err tr)] if
      [res] resolves to [Error tr].

      In other words, [trace err p] enriches the trace that [p] resolves to (if
      it does resolve to a trace) with the error [err]. It leaves the value that
      [p] resolves to untouched if it is not a trace.

      You can use this to add high-level information to potential low-level
      errors.

      Note that, like {!record_trace}, [trace] takes a fully evaluated error as
      argument. For a similar reason as explained there, you should only use
      [trace] with simple expressions (literal or constructor with simple
      parameters) and prefer [trace_eval] for any other expression (such as ones
      that include functions calls). *)
  val trace :
    'err -> ('b, 'err trace) result Lwt.t -> ('b, 'err trace) result Lwt.t

  (** [record_trace_eval] is identical to [record_trace] except that the error
      that enriches the trace is wrapped in a function that is evaluated only if
      it is needed. More formally [record_trace_eval mkerr res] is [res] if
      [res] is [Ok _], or it is [Error (Trace.cons (mkerr ()) tr)] if [res] is
      [Error tr].

      You can achieve the same effect by hand with

{[
match res with
| Ok _ -> res
| Error tr -> Error (Trace.cons (mkerr ()) tr)
]}

      Prefer [record_trace_eval] over [record_trace] when the enriching error is
      expensive to compute or heavy to allocate. *)
  val record_trace_eval :
    (unit -> 'err) -> ('a, 'err trace) result -> ('a, 'err trace) result

  (** [trace_eval] is identical to [trace] except that the error that enriches
      the trace is wrapped in a function that is evaluated only if {e and when}
      it is needed. More formally [trace_eval mkerr p] is a promise that
      resolves to [Ok v] if [p] resolves to [Ok v], or it resolves to
      [Error (Trace.cons (mkerr ()) tr)] if [p] resolves to [Error tr].

      You can achieve the same effect by hand with

{[
p >>= function
| Ok _ -> p
| Error tr ->
   Lwt.return (Error (Trace.cons (mkerr ()) tr))
]}

      Note that the evaluation of the error can be arbitrarily delayed. Avoid
      using references and other mutable values in the function [mkerr].

      Prefer [trace_eval] over [trace] when the enriching error is expensive to
      compute or heavy to allocate or when evaluating it requires the use of
      Lwt. *)
  val trace_eval :
    (unit -> 'err) ->
    ('b, 'err trace) result Lwt.t ->
    ('b, 'err trace) result Lwt.t

  (** [error_unless flag err] is [Ok ()] if [b] is [true], it is
      [Error (Trace.make err)] otherwise. *)
  val error_unless : bool -> 'err -> (unit, 'err trace) result

  (** [error_when flag err] is [Error (Trace.make err)] if [b] is [true], it is
      [Ok ()] otherwise. *)
  val error_when : bool -> 'err -> (unit, 'err trace) result

  (** [fail_unless flag err] is [Lwt.return @@ Ok ()] if [b] is [true], it is
      [Lwt.return @@ Error (Trace.make err)] otherwise. *)
  val fail_unless : bool -> 'err -> (unit, 'err trace) result Lwt.t

  (** [fail_when flag err] is [Lwt.return @@ Error (Trace.make err)] if [b] is
      [true], it is [Lwt.return @@ Ok ()] otherwise. *)
  val fail_when : bool -> 'err -> (unit, 'err trace) result Lwt.t

  (** [unless b f] is [f ()] if [b] is [false] and it is a promise already
      resolved to [Ok ()] otherwise.

      You can use [unless] to avoid having to write an [if] statement that you
      then need to populate entirely to satisfy the type-checker. E.g, you can
      write [unless b f] instead of [if not b then f () else return_unit]. *)
  val unless :
    bool -> (unit -> (unit, 'trace) result Lwt.t) -> (unit, 'trace) result Lwt.t

  (** [when_ b f] is [f ()] if [b] is [true] and it is a promise already
      resolved to [Ok ()] otherwise.

      You can use [when_] to avoid having to write an [if] statement that you
      then need to populate entirely to satisfy the type-checker. E.g, you can
      write [when_ b f] instead of [if b then f () else return_unit]. *)
  val when_ :
    bool -> (unit -> (unit, 'trace) result Lwt.t) -> (unit, 'trace) result Lwt.t

  (** Wrapper around [Lwt_utils.dont_wait] *)
  val dont_wait :
    (unit -> (unit, 'trace) result Lwt.t) ->
    ('trace -> unit) ->
    (exn -> unit) ->
    unit
end
