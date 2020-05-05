(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Tezos Protocol Implementation - Error Monad *)

(** {2 Error classification} *)

(** Categories of error *)
type error_category =
  [ `Branch  (** Errors that may not happen in another context *)
  | `Temporary  (** Errors that may not happen in a later context *)
  | `Permanent  (** Errors that will happen no matter the context *) ]

(** Custom error handling for economic protocols. *)

type error = ..

val pp : Format.formatter -> error -> unit

(** A JSON error serializer *)
val error_encoding : error Data_encoding.t

val json_of_error : error -> Data_encoding.json

val error_of_json : Data_encoding.json -> error

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

(** For other modules to register specialized error serializers *)
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

(** Classify an error using the registered kinds *)
val classify_errors : error list -> error_category

(** {2 Monad definition} *)

(** The error monad wrapper type, the error case holds a stack of
    error, initialized by the first call to {!fail} and completed by
    each call to {!trace} as the stack is rewound. The most general
    error is thus at the top of the error stack, going down to the
    specific error that actually caused the failure. *)
type 'a tzresult = ('a, error list) result

(** A JSON serializer for result of a given type *)
val result_encoding : 'a Data_encoding.t -> 'a tzresult Data_encoding.encoding

(** Successful result *)
val ok : 'a -> 'a tzresult

val ok_unit : unit tzresult

val ok_none : 'a option tzresult

val ok_some : 'a -> 'a option tzresult

val ok_nil : 'a list tzresult

val ok_true : bool tzresult

val ok_false : bool tzresult

(** Successful return *)
val return : 'a -> 'a tzresult Lwt.t

(** Successful return of [()] *)
val return_unit : unit tzresult Lwt.t

(** Successful return of [None] *)
val return_none : 'a option tzresult Lwt.t

(** [return_some x] is a successful return of [Some x] *)
val return_some : 'a -> 'a option tzresult Lwt.t

(** Successful return of [[]] *)
val return_nil : 'a list tzresult Lwt.t

(** Successful return of [true] *)
val return_true : bool tzresult Lwt.t

(** Successful return of [false] *)
val return_false : bool tzresult Lwt.t

(** Erroneous result *)
val error : error -> 'a tzresult

(** Erroneous return *)
val fail : error -> 'a tzresult Lwt.t

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
val ( >>? ) : 'a tzresult -> ('a -> 'b tzresult) -> 'b tzresult

(** Non-Lwt map operator. *)
val ( >|? ) : 'a tzresult -> ('a -> 'b) -> 'b tzresult

(** Combined bind operator. The [=?] indicates that the operator acts within
      the combined error-lwt monad. *)
val ( >>=? ) :
  'a tzresult Lwt.t -> ('a -> 'b tzresult Lwt.t) -> 'b tzresult Lwt.t

(** Combined map operator. *)
val ( >|=? ) : 'a tzresult Lwt.t -> ('a -> 'b) -> 'b tzresult Lwt.t

(** Injecting bind operator. This is for transitioning from the simple Error
      monad to the combined Error-Lwt monad.

      Note the order of the character: it starts with the error monad marker [?]
      and has the Lwt monad marker later. This hints at the role of the operator
      to transition into Lwt. *)
val ( >>?= ) : 'a tzresult -> ('a -> 'b tzresult Lwt.t) -> 'b tzresult Lwt.t

(** Injecting map operator. *)
val ( >|?= ) : 'a tzresult -> ('a -> 'b Lwt.t) -> 'b tzresult Lwt.t

(** Enrich an error report (or do nothing on a successful result) manually *)
val record_trace : error -> 'a tzresult -> 'a tzresult

(** Automatically enrich error reporting on stack rewind *)
val trace : error -> 'b tzresult Lwt.t -> 'b tzresult Lwt.t

(** Same as record_trace, for unevaluated error *)
val record_trace_eval : (unit -> error tzresult) -> 'a tzresult -> 'a tzresult

(** Same as trace, for unevaluated Lwt error *)
val trace_eval :
  (unit -> error tzresult Lwt.t) -> 'b tzresult Lwt.t -> 'b tzresult Lwt.t

(** Error on failed assertion *)
val error_unless : bool -> error -> unit tzresult

val error_when : bool -> error -> unit tzresult

(** Erroneous return on failed assertion *)
val fail_unless : bool -> error -> unit tzresult Lwt.t

val fail_when : bool -> error -> unit tzresult Lwt.t

val unless : bool -> (unit -> unit tzresult Lwt.t) -> unit tzresult Lwt.t

val _when : bool -> (unit -> unit tzresult Lwt.t) -> unit tzresult Lwt.t

(** {2 In-monad list iterators} *)

(** A {!List.iter} in the monad *)
val iter : ('a -> unit tzresult) -> 'a list -> unit tzresult

val iter_s : ('a -> unit tzresult Lwt.t) -> 'a list -> unit tzresult Lwt.t

(** A {!List.map} in the monad *)
val map : ('a -> 'b tzresult) -> 'a list -> 'b list tzresult

val mapi : (int -> 'a -> 'b tzresult) -> 'a list -> 'b list tzresult

val map_s : ('a -> 'b tzresult Lwt.t) -> 'a list -> 'b list tzresult Lwt.t

val rev_map_s : ('a -> 'b tzresult Lwt.t) -> 'a list -> 'b list tzresult Lwt.t

val mapi_s :
  (int -> 'a -> 'b tzresult Lwt.t) -> 'a list -> 'b list tzresult Lwt.t

(** A {!List.map2} in the monad.

      @raise [Invalid_argument] if provided two lists of different lengths. *)
val map2 : ('a -> 'b -> 'c tzresult) -> 'a list -> 'b list -> 'c list tzresult

(** @raise [Invalid_argument] if provided two lists of different lengths. *)
val mapi2 :
  (int -> 'a -> 'b -> 'c tzresult) -> 'a list -> 'b list -> 'c list tzresult

(** @raise [Invalid_argument] if provided two lists of different lengths. *)
val map2_s :
  ('a -> 'b -> 'c tzresult Lwt.t) ->
  'a list ->
  'b list ->
  'c list tzresult Lwt.t

(** @raise [Invalid_argument] if provided two lists of different lengths. *)
val mapi2_s :
  (int -> 'a -> 'b -> 'c tzresult Lwt.t) ->
  'a list ->
  'b list ->
  'c list tzresult Lwt.t

(** A {!List.filter_map} in the monad *)
val filter_map_s :
  ('a -> 'b option tzresult Lwt.t) -> 'a list -> 'b list tzresult Lwt.t

(** A {!List.filter} in the monad *)
val filter : ('a -> bool tzresult) -> 'a list -> 'a list tzresult

val filter_s : ('a -> bool tzresult Lwt.t) -> 'a list -> 'a list tzresult Lwt.t

(** A {!List.fold_left} in the monad *)
val fold_left_s :
  ('a -> 'b -> 'a tzresult Lwt.t) -> 'a -> 'b list -> 'a tzresult Lwt.t

(** A {!List.fold_right} in the monad *)
val fold_right_s :
  ('a -> 'b -> 'b tzresult Lwt.t) -> 'a list -> 'b -> 'b tzresult Lwt.t

(**/**)

type shell_error

type 'a shell_tzresult = ('a, shell_error list) result
