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

open Json
module String_set = Set.Make (String)
module String_map = Map.Make (String)

(* Very thin layer over JSON constructors, in case we want to change backend
   (and to provide some convenience). *)

let bool value : Json.u = `Bool value

let int value : Json.u = `Float (float value)

let float value : Json.u = `Float value

let string value : Json.u = `String value

let array values : Json.u = `A values

let field name value : (string * Json.u) list = [(name, value)]

let field_opt name value make : (string * Json.u) list =
  match value with None -> [] | Some value -> [(name, make value)]

let field_list name value : (string * Json.u) list =
  match value with [] -> [] | _ -> [(name, `A value)]

let obj ll : Json.u = `O (List.flatten ll)

module Schema = struct
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

  and property = {name : string; required : bool; schema : t}

  (* Helper for to_json below. *)
  let typ typ = field "type" (string typ)

  let rec to_json schema =
    obj
      (match schema with
      | Ref name -> [field "$ref" (string ("#/components/schemas/" ^ name))]
      | Other {title; description; nullable; kind} ->
          field_opt "title" title string
          :: field_opt "description" description string
          :: (if nullable then field "nullable" (bool true) else [])
          ::
          (match kind with
          | Boolean -> [typ "boolean"]
          | Integer {minimum; maximum; enum} ->
              [
                typ "integer";
                field_opt "minimum" minimum int;
                field_opt "maximum" maximum int;
                (match enum with
                | None -> []
                | Some enum -> field "enum" (`A (List.map int enum)));
              ]
          | Number {minimum; maximum} ->
              [
                typ "number";
                field_opt "minimum" minimum float;
                field_opt "maximum" maximum float;
              ]
          | String {enum; pattern} ->
              [
                typ "string";
                (match enum with
                | None -> []
                | Some enum -> field "enum" (`A (List.map string enum)));
                field_opt "pattern" pattern string;
              ]
          | Array item_schema ->
              [typ "array"; field "items" (to_json item_schema)]
          | Object {properties; additional_properties} ->
              let properties =
                let rec deduplicate known acc = function
                  | [] -> List.rev acc
                  | property :: tail ->
                      if String_set.mem property.name known then (
                        Printf.eprintf
                          "Warning: field %s appears twice in the same object; \
                           ignored duplicate occurrence\n\
                           %!"
                          property.name ;
                        deduplicate known acc tail)
                      else
                        deduplicate
                          (String_set.add property.name known)
                          (property :: acc)
                          tail
                in
                deduplicate String_set.empty [] properties
              in
              [
                typ "object";
                field
                  "properties"
                  (obj
                     (List.map
                        (fun p -> field p.name (to_json p.schema))
                        properties));
                field_list
                  "required"
                  (properties
                  |> List.filter (fun p -> p.required)
                  |> List.map (fun p -> string p.name));
                field_opt "additionalProperties" additional_properties to_json;
              ]
          | One_of schemas -> [field "oneOf" (array (List.map to_json schemas))]
          | Any -> []))

  let rec of_json json =
    match unannotate json with
    | `O [("$ref", `String str)] ->
        let n = String.length "#/components/schemas/" in
        if String.sub str 0 n = "#/components/schemas/" then
          Ref (String.sub str n (String.length str - n))
        else
          error
            json
            "unsupported reference: %s - expected a reference to \
             /components/schemas/..."
            str
    | `Null -> error json "Schema not found"
    | `O _ -> (
        let title = json |-> "title" |> as_string_opt in
        let description = json |-> "description" |> as_string_opt in
        let nullable =
          let nullable = json |-> "nullable" in
          match unannotate nullable with
          | `Null -> false
          | _ -> as_bool nullable
        in
        let make kind = Other {title; description; nullable; kind} in
        match json |-> "type" |> unannotate with
        | `String "boolean" -> make Boolean
        | `String "integer" ->
            let enum =
              json |-> "enum" |> function
              | enum when is_null enum -> None
              | enum -> Some (as_list enum |> List.map as_int)
            in
            let minimum = json |-> "minimum" |> as_int_opt in
            let maximum = json |-> "maximum" |> as_int_opt in
            make (Integer {minimum; maximum; enum})
        | `String "number" ->
            let minimum = json |-> "minimum" |> as_float_opt in
            let maximum = json |-> "maximum" |> as_float_opt in
            make (Number {minimum; maximum})
        | `String "string" ->
            let pattern = json |-> "pattern" |> as_string_opt in
            let enum =
              json |-> "enum" |> function
              | enum when is_null enum -> None
              | enum -> Some (as_list enum |> List.map as_string)
            in
            make (String {enum; pattern})
        | `String "object" ->
            let properties_list =
              List.map
                (fun (name, property) -> (name, of_json property))
                (json |-> "properties" |> as_object)
            in
            let additional_properties =
              let additional_properties = json |-> "additionalProperties" in
              match unannotate additional_properties with
              | `Null -> None
              | _ -> Some (of_json additional_properties)
            in
            let required =
              json |-> "required" |> as_list |> List.map as_string
            in
            let properties =
              List.map
                (fun (name, schema) ->
                  {name; required = List.mem name required; schema})
                properties_list
            in
            make (Object {properties; additional_properties})
        | `String "array" ->
            let items =
              let items = json |-> "items" in
              match unannotate items with
              | `Null -> error items "items not found"
              | _ -> of_json items
            in
            make (Array items)
        | _ -> (
            let oneOf = json |-> "oneOf" in
            match json |-> "oneOf" |> unannotate with
            | `A _ -> make (One_of (List.map of_json (as_list oneOf)))
            | `Null -> make Any
            | _ -> error json "Invalid oneOf"))
    | _ -> error json "Invalid schema"

  type maker =
    ?title:string -> ?description:string -> ?nullable:bool -> unit -> t

  let boolean ?title ?description ?(nullable = false) () =
    Other {title; description; nullable; kind = Boolean}

  let integer ?minimum ?maximum ?enum ?title ?description ?(nullable = false) ()
      =
    Other
      {title; description; nullable; kind = Integer {minimum; maximum; enum}}

  let number ?minimum ?maximum ?title ?description ?(nullable = false) () =
    Other {title; description; nullable; kind = Number {minimum; maximum}}

  let string ?enum ?pattern ?title ?description ?(nullable = false) () =
    Other {title; description; nullable; kind = String {enum; pattern}}

  let array ~items ?title ?description ?(nullable = false) () =
    Other {title; description; nullable; kind = Array items}

  let obj ?additional_properties ~properties ?title ?description
      ?(nullable = false) () =
    Other
      {
        title;
        description;
        nullable;
        kind = Object {properties; additional_properties};
      }

  let one_of ~cases ?title ?description ?(nullable = false) () =
    Other {title; description; nullable; kind = One_of cases}

  let any ?title ?description ?(nullable = false) () =
    Other {title; description; nullable; kind = Any}

  let reference name = Ref name
end

module Response = struct
  type t = {code : int option; description : string; schema : Schema.t}

  let make ?code ~description schema = {code; description; schema}

  let to_json {code; description; schema} =
    let code =
      match code with None -> "default" | Some code -> string_of_int code
    in
    ( code,
      obj
        [
          field "description" (string description);
          field
            "content"
            (obj
               [
                 field
                   "application/json"
                   (obj [field "schema" (Schema.to_json schema)]);
               ]);
        ] )

  let of_json (code, json) =
    let description = json |-> "description" |> as_string in
    let schema_json =
      let content = json |-> "content" in
      match unannotate content with
      | `Null -> error content "content not found"
      | _ -> (
          let application = content |-> "application/json" in
          match unannotate application with
          | `Null -> error application "application/json not found"
          | _ -> (
              let schema = application |-> "schema" in
              match unannotate schema with
              | `Null -> error schema "schema not found"
              | _ -> schema))
    in
    let schema = Schema.of_json schema_json in
    let code =
      match code with
      | "default" -> None
      | _ -> (
          match int_of_string_opt code with
          | None ->
              error
                schema_json
                "expected response code to be an integer or \"default\", got %S"
                code
          | code_opt -> code_opt)
    in
    {code; description; schema}
end

module Parameter = struct
  type t = {name : string; description : string option; schema : Schema.t}

  let to_json ?(required = true) in_ dynamic =
    obj
      [
        field "name" (string dynamic.name);
        field "in" (string in_);
        field_opt "description" dynamic.description string;
        field "required" (bool required);
        field "schema" (Schema.to_json dynamic.schema);
      ]

  let of_json json =
    let name = json |-> "name" |> as_string in
    let description = json |-> "description" |> as_string_opt in
    let required = json |-> "required" |> as_bool in
    let in_ = json |-> "in" |> as_string in
    let schema = json |-> "schema" |> Schema.of_json in
    (required, {name; description; schema}, in_)
end

module Service = struct
  type query_parameter = {required : bool; parameter : Parameter.t}

  type t = {
    description : string;
    request_body : Schema.t option;
    responses : Response.t list;
    query : query_parameter list;
  }

  let compare_query_parameter =
    (Stdlib.compare : query_parameter -> query_parameter -> int)

  let make ~description ?request_body ?(query = []) responses =
    {description; request_body; responses; query}

  let schema_in_content schema =
    obj
      [
        field
          "content"
          (obj
             [
               field
                 "application/json"
                 (obj [field "schema" (Schema.to_json schema)]);
             ]);
      ]

  let to_json parameters service =
    (* TODO: do we use mandatory query parameters? Here we assume we don't. *)
    let query =
      List.map
        (fun {required; parameter} ->
          Parameter.to_json ~required "query" parameter)
        service.query
    in
    obj
      [
        field "description" (string service.description);
        field_list "parameters" (parameters @ query);
        field_opt "requestBody" service.request_body schema_in_content;
        field "responses" (obj [List.map Response.to_json service.responses]);
      ]

  let parameters_of_json in_ json =
    List.filter_map
      (fun query_json ->
        let required, parameter, in_result = Parameter.of_json query_json in
        if String.equal in_result in_ then Some {required; parameter} else None)
      json

  let of_json json =
    let description = json |-> "description" |> as_string in
    let request_body =
      try
        Some
          (json |-> "requestBody" |~> "content" |~> "application/json"
         |~> "schema" |> Schema.of_json)
      with _ -> None
    in
    let responses =
      json |-> "responses" |> as_object |> List.map Response.of_json
    in
    let query =
      json |-> "parameters" |> as_list |> parameters_of_json "query"
    in
    let parameters =
      json |-> "parameters" |> as_list |> parameters_of_json "path"
    in
    ({description; request_body; responses; query}, parameters)
end

module Path = struct
  type item = Static of string | Dynamic of Parameter.t

  let static name = Static name

  let dynamic ?description ~schema name = Dynamic {name; description; schema}

  let string_of_item = function
    | Static name -> name
    | Dynamic dynamic -> "{" ^ dynamic.name ^ "}"

  let item_of_string string (parameters : Service.query_parameter list) =
    let get_parameter str =
      List.find_map (fun {Service.required = _; parameter} ->
          if String.equal str parameter.name then Some parameter else None)
    in
    if
      String.length string >= 2
      && string.[0] = '{'
      && string.[String.length string - 1] = '}'
    then
      let n = String.sub string 1 (String.length string - 2) in
      let parameter = get_parameter n parameters in
      match parameter with
      | Some parameter ->
          let description = parameter.description in
          dynamic ?description ~schema:parameter.schema n
      | None -> static string
    else static string

  type t = item list

  let to_string path = "/" ^ String.concat "/" (List.map string_of_item path)

  let of_string path parameters =
    match String.split_on_char '/' path with
    | [] -> failwith "empty path"
    | "" :: tail -> List.map (fun str -> item_of_string str parameters) tail
    | _ -> failwith (Format.asprintf "Invalid path %s" path)

  let get_dynamics path =
    path
    |> List.map (function Static _ -> [] | Dynamic x -> [x])
    |> List.flatten
end

module Endpoint = struct
  type methods = (Method.t * Service.t) list

  type t = {path : Path.t; methods : methods}

  let make ?get ?post ?put ?delete ?patch path =
    {
      path;
      methods =
        List.filter_map
          (function (_ : Method.t), None -> None | m, Some s -> Some (m, s))
          [
            (GET, get);
            (POST, post);
            (PUT, put);
            (DELETE, delete);
            (PATCH, patch);
          ];
    }

  let get_method endpoint meth = List.assq_opt meth endpoint.methods

  let to_json endpoint =
    (* Note: we force path parameters to be the same for all methods. *)
    let encode_parameters methods parameters =
      let method_ m =
        field_opt
          (Method.to_openapi_string m)
          (List.assoc_opt m methods)
          (Service.to_json parameters)
      in
      obj (List.map method_ Method.list)
    in
    let path = Path.to_string endpoint.path in
    let rec deduplicate known acc = function
      | [] -> List.rev acc
      | (parameter : Parameter.t) :: tail -> (
          match Hashtbl.find_opt known parameter.name with
          | Some p ->
              if
                Parameter.(p.description) = parameter.description
                && Parameter.(p.schema) = parameter.schema
              then deduplicate known acc tail
              else
                failwith
                  (Format.sprintf
                     "Parameter named %s from %s is found more than once but \
                      with different desription or schema"
                     p.name
                     path)
          | _ ->
              Hashtbl.add known parameter.name parameter ;
              deduplicate known (parameter :: acc) tail)
    in
    let parameters =
      Path.get_dynamics endpoint.path
      |> deduplicate (Hashtbl.create 0) []
      |> List.map (Parameter.to_json "path")
    in
    (path, encode_parameters endpoint.methods parameters)

  let of_json (path, json) =
    let methods, p =
      let get_service method_ =
        let service = json |-> Method.to_openapi_string method_ in
        match unannotate service with
        | `Null -> []
        | _ ->
            let service, parameters = Service.of_json service in
            [((method_, service), parameters)]
      in
      get_service Method.GET @ get_service Method.POST @ get_service Method.PUT
      @ get_service Method.DELETE @ get_service Method.PATCH
      |> List.split
    in
    let parameters =
      match List.sort_uniq (List.compare Service.compare_query_parameter) p with
      | [] -> []
      | [e] -> e
      | _ ->
          error json "path %s parameters must be the same for all methods" path
    in
    let path = Path.of_string path parameters in
    {path; methods}
end

module Server = struct
  type t = {url : string; description : string option}

  let make ?description url = {url; description}

  let to_json server =
    obj
      [
        field "url" (string server.url);
        field_opt "description" server.description string;
      ]

  let of_json json =
    let url = json |-> "url" |> as_string in
    let description = json |-> "description" |> as_string_opt in
    {url; description}
end

type t = {
  title : string;
  description : string option;
  version : string;
  servers : Server.t list;
  definitions : (string * Schema.t) list;
  endpoints : Endpoint.t list;
}

let make ~title ?description ~version ?(servers = []) ?(definitions = [])
    endpoints =
  {title; description; version; servers; definitions; endpoints}

let to_json openapi =
  obj
    [
      field "openapi" (string "3.0.0");
      field
        "info"
        (obj
           [
             field "title" (string openapi.title);
             field_opt "description" openapi.description string;
             field "version" (string openapi.version);
           ]);
      field_list "servers" (List.map Server.to_json openapi.servers);
      field "paths" (obj [List.map Endpoint.to_json openapi.endpoints]);
      (match openapi.definitions with
      | [] -> []
      | _ ->
          let definitions =
            List.map
              (fun (name, schema) -> field name (Schema.to_json schema))
              openapi.definitions
          in
          field "components" (obj [field "schemas" (obj definitions)]));
    ]

let of_json (json : Json.t) =
  let title, description, version =
    let info = json |-> "info" in
    let title = info |-> "title" |> as_string in
    let description = info |-> "description" |> as_string_opt in
    let version = info |-> "version" |> as_string in
    (title, description, version)
  in
  let servers = json |-> "servers" |> as_list |> List.map Server.of_json in
  let definitions =
    let components = json |-> "components" in
    match unannotate components with
    | `Null -> []
    | _ ->
        components |-> "schemas" |> as_object_opt |> Option.value ~default:[]
        |> List.map (fun (name, schema_json) ->
               (name, Schema.of_json schema_json))
  in
  let endpoints =
    json |-> "paths" |> as_object_opt |> Option.value ~default:[]
    |> List.map Endpoint.of_json
  in
  {title; description; version; servers; definitions; endpoints}
