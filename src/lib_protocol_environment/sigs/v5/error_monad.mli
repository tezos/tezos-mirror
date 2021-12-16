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

type error_category = [`Branch | `Temporary | `Permanent | `Outdated]

(** CORE : errors *)

type error = ..

val error_encoding : error Data_encoding.t

val pp : Format.formatter -> error -> unit

(** EXT : error registration/query *)

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

val json_of_error : error -> Data_encoding.json

val error_of_json : Data_encoding.json -> error

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

(** MONAD : trace, monad, etc. *)

type 'err trace

type 'a tzresult = ('a, error trace) result

val make_trace_encoding : 'error Data_encoding.t -> 'error trace Data_encoding.t

val trace_encoding : error trace Data_encoding.t

val pp_trace : Format.formatter -> error trace -> unit

val result_encoding : 'a Data_encoding.t -> 'a tzresult Data_encoding.t

val ok : 'a -> ('a, 'trace) result

val return : 'a -> ('a, 'trace) result Lwt.t

val return_unit : (unit, 'trace) result Lwt.t

val return_none : ('a option, 'trace) result Lwt.t

val return_some : 'a -> ('a option, 'trace) result Lwt.t

val return_nil : ('a list, 'trace) result Lwt.t

val return_true : (bool, 'trace) result Lwt.t

val return_false : (bool, 'trace) result Lwt.t

val error : 'err -> ('a, 'err trace) result

val trace_of_error : 'err -> 'err trace

val fail : 'err -> ('a, 'err trace) result Lwt.t

val ( >>= ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t

val ( >|= ) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t

val ( >>? ) :
  ('a, 'trace) result -> ('a -> ('b, 'trace) result) -> ('b, 'trace) result

val ( >|? ) : ('a, 'trace) result -> ('a -> 'b) -> ('b, 'trace) result

val ( >>=? ) :
  ('a, 'trace) result Lwt.t ->
  ('a -> ('b, 'trace) result Lwt.t) ->
  ('b, 'trace) result Lwt.t

val ( >|=? ) :
  ('a, 'trace) result Lwt.t -> ('a -> 'b) -> ('b, 'trace) result Lwt.t

val ( >>?= ) :
  ('a, 'trace) result ->
  ('a -> ('b, 'trace) result Lwt.t) ->
  ('b, 'trace) result Lwt.t

val ( >|?= ) :
  ('a, 'trace) result -> ('a -> 'b Lwt.t) -> ('b, 'trace) result Lwt.t

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
  (exn -> unit) ->
  ('trace -> unit) ->
  (unit -> (unit, 'trace) result Lwt.t) ->
  unit

(** [catch f] executes [f] within a try-with block and wraps exceptions within
    a [tzresult]. [catch f] is equivalent to
    [try Ok (f ()) with e -> Error (error_of_exn e)].

    If [catch_only] is set, then only exceptions [e] such that [catch_only e] is
    [true] are caught.

    Whether [catch_only] is set or not, this function never catches
    non-deterministic runtime exceptions of OCaml such as {!Stack_overflow} and
    {!Out_of_memory} nor system-exceptions such as {!Unix.Unix_error} and
    {!Sys_error}. *)
val catch : ?catch_only:(exn -> bool) -> (unit -> 'a) -> 'a tzresult

(** [catch_f f handler] is equivalent to [map_error (catch f) handler].
    In other words, it catches exceptions in [f ()] and either returns the
    value in an [Ok] or passes the exception to [handler] for the [Error].

    [catch_only] has the same use as with [catch]. The same restriction on
    catching non-deterministic runtime exceptions applies. *)
val catch_f :
  ?catch_only:(exn -> bool) -> (unit -> 'a) -> (exn -> error) -> 'a tzresult

(** [catch_s] is like [catch] but when [f] returns a promise. It is equivalent
    to

{[
Lwt.try_bind f
  (fun v -> Lwt.return (Ok v))
  (fun e -> Lwt.return (Error (error_of_exn e)))
]}

    If [catch_only] is set, then only exceptions [e] such that [catch_only e] is
    [true] are caught.

    Whether [catch_only] is set or not, this function never catches
    non-deterministic runtime exceptions of OCaml such as {!Stack_overflow} and
    {!Out_of_memory} nor system-exceptions such as {!Unix.Unix_error} and
    {!Sys_error}. *)
val catch_s :
  ?catch_only:(exn -> bool) -> (unit -> 'a Lwt.t) -> 'a tzresult Lwt.t

(* Synchronisation *)

val join_e : (unit, 'err trace) result list -> (unit, 'err trace) result

val all_e : ('a, 'err trace) result list -> ('a list, 'err trace) result

val both_e :
  ('a, 'err trace) result ->
  ('b, 'err trace) result ->
  ('a * 'b, 'err trace) result

(**/**)

(* The protocol environment needs to know about shell's tzresult because they are
   used for in-protocol RPCs. Moreover, some light processing on these results
   is done in the protocol which requires the type to be concrete.

   The type is kept private because the environment is sole responsible for
   wrapping the protocol's errors into the shell's. *)

type shell_tztrace

type 'a shell_tzresult = ('a, shell_tztrace) result
