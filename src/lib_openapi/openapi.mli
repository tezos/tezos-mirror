(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** OpenAPI specifications. *)

(** This is not a general library, in particular it does not support
    content-types other than [application/json]. It only supports
    what we actually use. *)

module Schema : sig
  (** OpenAPI Schema Objects.

      Not exactly the same as JSON schemas. *)

  (** Nullable (i.e. non-ref) schemas. *)
  type kind =
    | Boolean
    | Integer of {
        minimum : int option;
        maximum : int option;
        enum : int list option;
      }
    | Number of {minimum : float option; maximum : float option}
    | String of {enum : string list option; pattern : string option}
    | Array of t
    | Object of {properties : property list; additional_properties : t option}
    | One_of of t list
    | Any

  and t =
    | Ref of string
    | Other of {
        title : string option;
        description : string option;
        nullable : bool;
        kind : kind;
      }

  (** Object fields. *)
  and property = {name : string; required : bool; schema : t}

  type maker =
    ?title:string -> ?description:string -> ?nullable:bool -> unit -> t

  val boolean : maker

  val integer : ?minimum:int -> ?maximum:int -> ?enum:int list -> maker

  val number : ?minimum:float -> ?maximum:float -> maker

  val string : ?enum:string list -> ?pattern:string -> maker

  val array : items:t -> maker

  val obj : ?additional_properties:t -> properties:property list -> maker

  val one_of : cases:t list -> maker

  val any : maker

  val reference : string -> t
end

module Response : sig
  type t = {code : int option; description : string; schema : Schema.t}

  val make : ?code:int -> description:string -> Schema.t -> t
end

module Parameter : sig
  type t = {name : string; description : string option; schema : Schema.t}
end

module Service : sig
  type query_parameter = {required : bool; parameter : Parameter.t}

  type t = {
    description : string;
    request_body : Schema.t option;
    responses : Response.t list;
    query : query_parameter list;
  }

  val make :
    description:string ->
    ?request_body:Schema.t ->
    ?query:query_parameter list ->
    Response.t list ->
    t
end

module Path : sig
  type item = Static of string | Dynamic of Parameter.t

  val static : string -> item

  val dynamic : ?description:string -> schema:Schema.t -> string -> item

  type t = item list

  val to_string : t -> string
end

module Endpoint : sig
  (** API endpoints, i.e. paths associated with one or more HTTP methods. *)

  (** Associative lists for methods and their implementation. *)
  type methods = (Method.t * Service.t) list

  (** API endpoints specifications. *)
  type t = {path : Path.t; methods : methods}

  (** Get the service associated to a method for a given endpoint, if any. *)
  val get_method : t -> Method.t -> Service.t option

  val make :
    ?get:Service.t ->
    ?post:Service.t ->
    ?put:Service.t ->
    ?delete:Service.t ->
    ?patch:Service.t ->
    Path.t ->
    t
end

module Server : sig
  type t = {url : string; description : string option}

  val make : ?description:string -> string -> t
end

type t = {
  title : string;
  description : string option;
  version : string;
  servers : Server.t list;
  definitions : (string * Schema.t) list;
  endpoints : Endpoint.t list;
}

val make :
  title:string ->
  ?description:string ->
  version:string ->
  ?servers:Server.t list ->
  ?definitions:(string * Schema.t) list ->
  Endpoint.t list ->
  t

val to_json : t -> Json.u

val of_json : Json.t -> t
