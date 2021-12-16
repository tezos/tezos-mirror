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
    | Integer of {minimum : int option; maximum : int option; enum : int list}
    | Number of {minimum : float option; maximum : float option}
    | String of {enum : string list; pattern : string option}
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
          ::
          field_opt "description" description string
          ::
          (if nullable then field "nullable" (bool true) else [])
          ::
          (match kind with
          | Boolean -> [typ "boolean"]
          | Integer {minimum; maximum; enum} ->
              [
                typ "integer";
                field_opt "minimum" minimum int;
                field_opt "maximum" maximum int;
                field_list "enum" (List.map int enum);
              ]
          | Number {minimum; maximum} ->
              [
                typ "integer";
                field_opt "minimum" minimum float;
                field_opt "maximum" maximum float;
              ]
          | String {enum; pattern} ->
              [
                typ "string";
                field_list "enum" (List.map string enum);
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

  type maker =
    ?title:string -> ?description:string -> ?nullable:bool -> unit -> t

  let boolean ?title ?description ?(nullable = false) () =
    Other {title; description; nullable; kind = Boolean}

  let integer ?minimum ?maximum ?(enum = []) ?title ?description
      ?(nullable = false) () =
    Other
      {title; description; nullable; kind = Integer {minimum; maximum; enum}}

  let number ?minimum ?maximum ?title ?description ?(nullable = false) () =
    Other {title; description; nullable; kind = Number {minimum; maximum}}

  let string ?(enum = []) ?pattern ?title ?description ?(nullable = false) () =
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
end

module Service = struct
  type query_parameter = {required : bool; parameter : Parameter.t}

  type t = {
    description : string;
    request_body : Schema.t option;
    responses : Response.t list;
    query : query_parameter list;
  }

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
end

module Path = struct
  type item = Static of string | Dynamic of Parameter.t

  let static name = Static name

  let dynamic ?description ~schema name = Dynamic {name; description; schema}

  let string_of_item = function
    | Static name -> name
    | Dynamic dynamic -> "{" ^ dynamic.name ^ "}"

  type t = item list

  let to_string path = "/" ^ String.concat "/" (List.map string_of_item path)

  let get_dynamics path =
    path
    |> List.map (function Static _ -> [] | Dynamic x -> [x])
    |> List.flatten
end

module Endpoint = struct
  type t = {
    path : Path.t;
    get : Service.t option;
    post : Service.t option;
    put : Service.t option;
    delete : Service.t option;
    patch : Service.t option;
  }

  let make ?get ?post ?put ?delete ?patch path =
    {path; get; post; put; delete; patch}

  let to_json endpoint =
    (* Note: we force path parameters to be the same for all methods. *)
    let parameters =
      Path.get_dynamics endpoint.path |> List.map (Parameter.to_json "path")
    in
    ( Path.to_string endpoint.path,
      obj
        [
          field_opt "get" endpoint.get (Service.to_json parameters);
          field_opt "post" endpoint.post (Service.to_json parameters);
          field_opt "put" endpoint.put (Service.to_json parameters);
          field_opt "delete" endpoint.delete (Service.to_json parameters);
          field_opt "patch" endpoint.patch (Service.to_json parameters);
        ] )
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
