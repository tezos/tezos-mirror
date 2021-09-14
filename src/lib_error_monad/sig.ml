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
  | `Permanent -> "permanent"
  | `Temporary -> "temporary"
  | `Branch -> "branch"

let combine_category c1 c2 =
  match (c1, c2) with
  | (`Permanent, _) | (_, `Permanent) -> `Permanent
  | (`Branch, _) | (_, `Branch) -> `Branch
  | (`Temporary, `Temporary) -> `Temporary

module type PREFIX = sig
  (** The identifier for parts of the code that need their own error monad. It
      is expected (but not enforced) that the identifier:
      is printable and easy to read once printed, and
      ends with a separator (typically a dot or a dash). *)
  val id : string
end

module type CORE = sig
  type error = ..

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

  (**
     [find_info_of_error e] retrieves the `error_info` associated with the
     given error `e`.
     @raise [Invalid_argument] if the error is a wrapped error from another monad
     @raise [Not_found] if the error's constructor has not been registered
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

    (** [wrap e] returns a general [error] from a specific [unwrapped] error
    [e] *)
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

  val classify_trace : tztrace -> error_category

  val return : 'a -> ('a, 'e) result Lwt.t

  val return_unit : (unit, 'e) result Lwt.t

  val return_none : ('a option, 'e) result Lwt.t

  val return_some : 'a -> ('a option, 'e) result Lwt.t

  val return_nil : ('a list, 'e) result Lwt.t

  val return_true : (bool, 'e) result Lwt.t

  val return_false : (bool, 'e) result Lwt.t

  (** more defaulting to trace *)
  val fail : 'error -> ('a, 'error trace) result Lwt.t

  val error : 'error -> ('a, 'error trace) result

  (* NOTE: Right now we leave this [pp_print_error] named as is. Later on we
     might rename it to [pp_print_trace]. *)
  val pp_print_error : Format.formatter -> error trace -> unit

  (** Pretty-prints the top error of a trace *)
  val pp_print_top_error_of_trace : Format.formatter -> error trace -> unit

  val trace_encoding : error trace Data_encoding.t

  (** A serializer for result of a given type *)
  val result_encoding : 'a Data_encoding.t -> 'a tzresult Data_encoding.t

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
    bool -> (unit -> (unit, 'trace) result Lwt.t) -> (unit, 'trace) result Lwt.t

  val when_ :
    bool -> (unit -> (unit, 'trace) result Lwt.t) -> (unit, 'trace) result Lwt.t

  (** Wrapper around [Lwt_utils.dont_wait] *)
  val dont_wait :
    (unit -> (unit, 'trace) result Lwt.t) ->
    ('trace -> unit) ->
    (exn -> unit) ->
    unit
end
