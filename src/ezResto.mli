(**************************************************************************)
(*  resto                                                                 *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*  Copyright (C) 2020, Nomadic Labs.                                     *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

(** [EzResto] is an abstraction layer on top of {!Resto} that is simpler to use
    -- it is the *easy* Resto. We recommend that you read the documentation in
    this module as the basis for understanding EzResto and as a jumping point to
    understand Resto.

    # Overview of EzResto

    EzResto is a library for describing *services*. A service is the entry-point
    in an API: a URI/URL with some path parameters (some of the slash-separated
    segments are actually decoded as parameters), some additional query
    parameters (the part that looks like [?utm_parameter=from_email] in the
    links of marketing emails) and other attributes depending on the method of
    the service.

    For example, you can use EzResto to describe the directory of services that,
    as a whole, forms the API for a web-service. You can then use one of the
    other resto packages to implement a server that answers requests made to
    this API. Alternatively, you can use one of the other resto packages to make
    your program query such an API.

    # Comparison with Resto

    As stated earlier, EzResto is a light-weight version of Resto. Note that,
    whilst the interface is light-weight (i.e., the interfaces are simpler),
    the actual runtime cost is the same.

    For example, in [EzResto], [path]s ({!Path.t}) have a single type parameter:
    this makes the most common use (adding suffixes to an existing path) simpler
    to reason about, but preclude the use of the [prefix] functionality of
    [Resto] ({!Resto.Path.prefix}). But the runtime behavior (including
    computational and memory cost) is the same.

    In general, you should use [EzResto] if it includes the features you need,
    and resort to [Resto] only on a by-need basis.

    # Intended use of EzResto

    The intended use of [EzResto] is as follows:
    - Define arguments, paths, query fields, and queries as required by the API
      you describe.
    - Define services using the previously defined arguments, paths, query
      fields and queries.

    If you are writing a server, you can then:
    - Use {!EzResto_directory} to register those services.
    - Use [Resto_cohttp_server.]{!Server} to spin up a server that answers
      requests to these services.

    Alternatively, if you are writing a client, you can then:
    - Use [Resto_cohttp_client.]{!Client} to make requests to these services.

*)

(** The different methods that a service can be used by a service. *)
type meth = [`GET | `POST | `DELETE | `PUT | `PATCH]

(** Arguments are documented serializers-deserializers for parameters. *)
module Arg : sig
  (** The type of an argument. *)
  type 'a t = 'a Resto.Arg.arg

  type 'a arg = 'a t

  (** [make ?descr ~name ~destruct ~construct ()] is an argument. The values of
      [descr] and [name] are used for documentation purpose only. The values of
      [destruct] and [construct] are used for conversion from/to [string]s. Note
      that it is expected that [destruct] and [construct] round-trip (modulo the
      [result] error wrapping). *)
  val make :
    ?descr:string ->
    name:string ->
    destruct:(string -> ('a, string) result) ->
    construct:('a -> string) ->
    unit ->
    'a arg

  (** [descr] is a type for the documentation of a [t]. *)
  type descr = Resto.Arg.descr = {name : string; descr : string option}

  (** [descr t] is the documentation of [t]. *)
  val descr : 'a arg -> descr

  (** [int], [int32], [int64], [float], and [string] are arguments for
      the corresponding built-in types of OCaml. *)
  val int : int arg

  val int32 : int32 arg

  val int64 : int64 arg

  val float : float arg

  val string : string arg
end

(** Paths describe URIs/URLs: segments separated by slashes (/).

    Note that paths can be static (i.e., all the segments of the path are
    determined in advance) or dynamic (i.e., some segments of the path are
    actually arguments for the service -- e.g., paths can have the form
    [/user/<user-name>] where [<user-name>] is a string encoding of a user
    identifier). *)
module Path : sig
  (** The type for service's paths

      A [a path] is a path in which some segments encode a value of type [a].

      Typically a [unit path] is a static path. Also typically, a dynamic path
      has type [(((unit * a) * b) * ..) path] where different segments encode
      the different components of the tuple ([a], [b], etc.). For example the
      path [/entries-by-date/<year>/<month>/<day>] may be described as a
      [(((unit * int) * int) * int) path]. *)
  type 'params t = (unit, 'params) Resto.Path.path

  type 'params path = 'params t

  (** [root] is the [Nil] of path construction. *)
  val root : unit path

  (** [add_suffix p s] is a path in which [s] has been appended to the sequence
      of segments described by [p]. *)
  val add_suffix : 'params path -> string -> 'params path

  val ( / ) : 'params path -> string -> 'params path

  (** [add_arg p a] is a path in which a segment representing a value of type
      [a] has been appended to the sequence of segments described by [p].

      This is intended for use by services with parameters. Specifically, a
      service that is parametrized over a value of type [a] is attached to a
      path that includes a argument for a value of type [a]. When the service is
      called, the argument is encoded as by the client/decoded by the server,
      using the constructor/destructor of the argument. *)
  val add_arg : 'params path -> 'a Arg.arg -> ('params * 'a) path

  val ( /: ) : 'params path -> 'a Arg.arg -> ('params * 'a) path
end

(** Query parameters are the key-value pairs that appear as
    [?key0=value0&key1=value1&..] at the end of the path in URIs/URLs. *)
module Query : sig
  (** A type for representing query parameters. *)
  type 'a t

  type 'a query = 'a t

  (** [empty] is for services that expects no query parameters. *)
  val empty : unit query

  (** The type for key-value pairs that constitute a query. The first type
      parameter is for whole-query store and the second is for the type of value
      carried by the field. *)
  type ('a, 'b) field

  (** [field ?descr key arg default get] is a field for the query parameters,
      i.e., it describes one key-value pair.

      The key is given by [key] and the value is parsed as specified in [arg]
      or, if absent from the URI suffix, [default].

      Finally, [get] is for retrieving the value from the whole-query store.
      More on the whole-query store below. *)
  val field :
    ?descr:string -> string -> 'a Arg.t -> 'a -> ('b -> 'a) -> ('b, 'a) field

  (** Queries are constructed by adding fields to an open query and sealing it
      into a query. This is done using the functions below. Typically, it is
      done as follow:
      [query c |+ field1 |+ field2 |> seal]

      As the types require, you must provide the correct argument to the
      successive building steps. Here is an example:
      [
         query (fun timeout shade -> (timeout, shade))
         |+ field "timeout" Arg.float 10. (fun (timeout, _) -> timeout)
         |+ field "shade" Arg.string "fuchsia" (fun (_, shade) -> shade)
         |> seal
      ]

      The initial [query] function takes a whole-query store builder (a function
      that assemble all the fields into a single store of values), and each
      field takes a function to recover the value from the whole-query store. *)
  type ('a, 'b, 'c) open_query

  val query : 'b -> ('a, 'b, 'b) open_query

  val ( |+ ) :
    ('a, 'b, 'c -> 'd) open_query -> ('a, 'c) field -> ('a, 'b, 'd) open_query

  val seal : ('a, 'b, 'a) open_query -> 'a t

  (** The parsing machinery, below, is mainly for internal use. *)
  type untyped = (string * string) list

  exception Invalid of string

  val parse : 'a query -> untyped -> 'a
end

(** The section below is to declare services. *)

(** The type of services. *)
type ('meth, 'params, 'query, 'input, 'output, 'error) service =
  ( 'meth,
    unit,
    'params,
    'query,
    'input,
    'output,
    'error )
  Resto.MakeService(Resto_json.Encoding).service

(** [get_service ?description ~query ~output ~error path] is a [GET] service
    that is intended to seat at the URI described by [path] and receive the
    additional parameters described by [query]. The values [output] and [error]
    describe the representations of the two possible returns for the service.

    Note that, whilst [get_service] declares a service, the resulting service is
    not registered yet. This is handled in [EzResto_directory]. *)
val get_service :
  ?description:string ->
  query:'query Query.t ->
  output:'output Json_encoding.encoding ->
  error:'error Json_encoding.encoding ->
  'params Path.t ->
  ([`GET], 'params, 'query, unit, 'output, 'error) service

(** {!post_service}, {!delete_service}, {!put_service}, and {!patch_service} are
    similar to {!get_service} but for other methods.

    Note that some of these functions take an additional [input] argument. This
    is only for the services with methods that expect additional parameters. It
    is used internally to encode/decode additional parameters passed in a
    dedicated payload rather than in the path/query parameters. *)

val post_service :
  ?description:string ->
  query:'query Query.t ->
  input:'input Json_encoding.encoding ->
  output:'output Json_encoding.encoding ->
  error:'error Json_encoding.encoding ->
  'params Path.t ->
  ([`POST], 'params, 'query, 'input, 'output, 'error) service

val delete_service :
  ?description:string ->
  query:'query Query.t ->
  output:'output Json_encoding.encoding ->
  error:'error Json_encoding.encoding ->
  'params Path.t ->
  ([`DELETE], 'params, 'query, unit, 'output, 'error) service

val put_service :
  ?description:string ->
  query:'query Query.t ->
  input:'input Json_encoding.encoding ->
  output:'output Json_encoding.encoding ->
  error:'error Json_encoding.encoding ->
  'params Path.t ->
  ([`PUT], 'params, 'query, 'input, 'output, 'error) service

val patch_service :
  ?description:string ->
  query:'query Query.t ->
  input:'input Json_encoding.encoding ->
  output:'output Json_encoding.encoding ->
  error:'error Json_encoding.encoding ->
  'params Path.t ->
  ([`PATCH], 'params, 'query, 'input, 'output, 'error) service

(** The following section is to manipulate requests. *)

type 'input input =
  | No_input : unit input
  | Input : 'input Json_encoding.encoding -> 'input input

type 'input request = {meth : meth; uri : Uri.t; input : 'input input}

val forge_request :
  ('meth, 'params, 'query, 'input, 'output, 'error) service ->
  ?base:Uri.t ->
  'params ->
  'query ->
  'input request

(** These functions below are used to recover the components of a service. *)

val query :
  ('meth, 'params, 'query, 'input, 'output, 'error) service -> 'query Query.t

val input_encoding :
  ('meth, 'params, 'query, 'input, 'output, 'error) service -> 'input input

val output_encoding :
  ('meth, 'params, 'query, 'input, 'output, 'error) service ->
  'output Json_encoding.encoding

val error_encoding :
  ('meth, 'params, 'query, 'input, 'output, 'error) service ->
  'error Json_encoding.encoding

(** This final section is for self-documentation: a service for service
    documentation. *)

module Description = Resto.Description

type description_service =
  ( [`GET],
    unit * string list,
    Description.request,
    unit,
    Json_schema.schema Description.directory,
    unit )
  service

val description_service :
  ?description:string -> unit Path.path -> description_service
