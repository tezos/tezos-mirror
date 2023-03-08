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

(* Entrypoint for a program that converts a Tezos API description (JSON)
   into an OpenAPI 3 specification (JSON as well). *)

let fail x = Printf.ksprintf failwith x

let warn x = Printf.ksprintf (fun s -> prerr_endline ("Warning: " ^ s)) x

let opt_fold f acc = function None -> acc | Some x -> f acc x

(* Check our assumptions for arrays. *)
let check_array_specs
    ({min_items; max_items; unique_items; additional_items} :
      Json_schema.array_specs) =
  assert (min_items <= 0) ;
  assert (max_items = None) ;
  assert (unique_items = false) ;
  assert (additional_items = None)

let convert_enum enum convert_item =
  Option.map
    (fun enum ->
      List.map (fun json -> json |> Json_repr.from_any |> convert_item) enum)
    enum

let rec convert_element (element : Json_schema.element) : Openapi.Schema.t =
  (* Check our assumptions. *)
  assert (element.default = None) ;
  assert (element.format = None) ;
  assert (element.id = None) ;
  let maker : Openapi.Schema.maker =
    match element.kind with
    | Object specs ->
        assert (element.enum = None) ;
        assert (specs.pattern_properties = []) ;
        assert (specs.min_properties <= 0) ;
        assert (specs.max_properties = None) ;
        assert (specs.schema_dependencies = []) ;
        assert (specs.property_dependencies = []) ;
        let properties =
          let map l f = List.map f l in
          map specs.properties @@ fun (name, element, required, unknown) ->
          (* [unknown] is not documented, is it a default value? *)
          assert (unknown = None) ;
          {Openapi.Schema.name; required; schema = convert_element element}
        in
        Openapi.Schema.obj
          ?additional_properties:
            (Option.map convert_element specs.additional_properties)
          ~properties
    | Array (elements, specs) ->
        (* Tuples are not supported by OpenAPI, we have to lose some type information. *)
        assert (element.enum = None) ;
        check_array_specs specs ;
        Openapi.Schema.array
          ~items:
            (Openapi.Schema.one_of
               ~cases:(List.map convert_element elements)
               ())
    | Monomorphic_array (items, specs) ->
        assert (element.enum = None) ;
        check_array_specs specs ;
        Openapi.Schema.array ~items:(convert_element items)
    | Combine (combinator, elements) ->
        assert (element.enum = None) ;
        assert (combinator = One_of) ;
        (* Null elements in a [Combine] kind can either be represented with [Null] kind,
           or with the following [Object]:
           { /* None */
             "none": null }
           Function [is_null] catches both those cases.

           FIXME: #3484
           Remove this encoding of [null] from Octez
        *)
        let is_null e =
          let open Json_schema in
          match e with
          | {kind = Null; _}
          | {
              kind =
                Object
                  {
                    properties = [("none", {kind = Null; _}, true, None)];
                    pattern_properties = [];
                    additional_properties = None;
                    min_properties = 0;
                    max_properties = None;
                    schema_dependencies = [];
                    property_dependencies = [];
                  };
              title = Some "None";
              description = None;
              default = None;
              format = None;
              enum = None;
              id = None;
            } ->
              true
          | _ -> false
        in
        let null = List.exists is_null elements in
        let elements = List.filter (fun e -> not @@ is_null e) elements in
        fun ?title ?description ?(nullable = false) () ->
          Openapi.Schema.one_of
            ?title
            ?description
            ~nullable:(nullable || null)
            ~cases:(List.map convert_element elements)
            ()
    | Def_ref path -> (
        assert (element.enum = None) ;
        let name =
          match path with
          | `Field "definitions" :: tail ->
              let path = Json_query.json_pointer_of_path tail in
              assert (String.length path > 0 && path.[0] = '/') ;
              String.sub path 1 (String.length path - 1)
          | _ -> assert false
        in
        fun ?title ?description ?(nullable = false) () ->
          match (title, description, nullable) with
          | None, None, false -> Openapi.Schema.reference name
          | _ ->
              (* OpenAPI does not allow other fields next to "$ref" fields.
                 So we have to cheat a little bit. *)
              Openapi.Schema.one_of
                ?title
                ?description
                ~nullable
                ~cases:[Openapi.Schema.reference name]
                ())
    | Id_ref _id ->
        assert (element.enum = None) ;
        assert false
    | Ext_ref _uri ->
        assert (element.enum = None) ;
        assert false
    | String {pattern; min_length; max_length; _} ->
        assert (min_length <= 0) ;
        assert (max_length = None) ;
        let enum =
          convert_enum element.enum @@ function
          | `String s -> s
          | _ -> assert false
        in
        Openapi.Schema.string ?enum ?pattern
    | Integer {multiple_of; minimum; maximum} ->
        assert (multiple_of = None) ;
        let enum =
          convert_enum element.enum @@ function
          | `Int s -> s
          | _ -> assert false
        in
        (* Note: there is currently a bug in Json_schema:
           `Exclusive and `Inclusive are inverted... *)
        let minimum =
          Option.map
            (function
              | f, `Exclusive -> int_of_float (ceil f) | _ -> assert false)
            minimum
        in
        let maximum =
          Option.map
            (function
              | f, `Exclusive -> int_of_float (floor f) | _ -> assert false)
            maximum
        in
        Openapi.Schema.integer ?enum ?minimum ?maximum
    | Number {multiple_of; minimum; maximum} ->
        assert (element.enum = None) ;
        assert (multiple_of = None) ;
        (* Note: there is currently a bug in Json_schema:
           `Exclusive and `Inclusive are inverted... *)
        let minimum =
          Option.map (function f, `Exclusive -> f | _ -> assert false) minimum
        in
        let maximum =
          Option.map (function f, `Exclusive -> f | _ -> assert false) maximum
        in
        Openapi.Schema.number ?minimum ?maximum
    | Boolean ->
        assert (element.enum = None) ;
        Openapi.Schema.boolean
    | Null ->
        assert (element.enum = None) ;
        assert false
    | Any ->
        assert (element.enum = None) ;
        Openapi.Schema.any
    | Dummy ->
        assert (element.enum = None) ;
        assert false
  in
  maker ?title:element.title ?description:element.description ()

module String_map = Map.Make (String)

type env = Openapi.Schema.t String_map.t

let empty_env = String_map.empty

let merge_envs (a : env) (b : env) : env =
  let merge_key _name a b =
    match (a, b) with
    | None, None -> None
    | None, (Some _ as x) | (Some _ as x), None -> x
    | Some a, Some _b ->
        (* TODO: check that a and b are equivalent *)
        Some a
  in
  String_map.merge merge_key a b

let merge_env_list l = List.fold_left merge_envs empty_env l

let gather_definitions (schema : Json_schema.schema)
    (converted_schema : Openapi.Schema.t) : env =
  let rec gather (acc : env) (converted_schema : Openapi.Schema.t) : env =
    match converted_schema with
    | Ref name -> (
        if String_map.mem name acc then acc
        else
          match Json_schema.find_definition name schema with
          | exception Not_found -> assert false
          | element ->
              let converted = convert_element element in
              let acc = String_map.add name converted acc in
              gather acc converted)
    | Other x -> (
        match x.kind with
        | Boolean | Integer _ | Number _ | String _ | Any -> acc
        | Array items -> gather acc items
        | Object {properties; additional_properties} ->
            let acc =
              List.fold_left
                (fun acc p -> gather acc p.Openapi.Schema.schema)
                acc
                properties
            in
            opt_fold gather acc additional_properties
        | One_of cases -> List.fold_left gather acc cases)
  in
  gather String_map.empty converted_schema

let convert_schema (schema : Json.t) : env * Openapi.Schema.t =
  let schema = Json_schema.of_json (Json.unannotate schema) in
  let converted_schema = convert_element (Json_schema.root schema) in
  let env = gather_definitions schema converted_schema in
  (env, converted_schema)

let convert_response ?code (schemas : Api.schemas option) :
    env * Openapi.Response.t list =
  match schemas with
  | None -> (empty_env, [])
  | Some schemas ->
      let env, schema = convert_schema schemas.json_schema in
      (env, [Openapi.Response.make ?code ~description:"" schema])

let opt_map_with_env f = function
  | None -> (empty_env, None)
  | Some x ->
      let env, y = f x in
      (env, Some y)

let convert_query_parameter {Api.id = _; name; description; kind} :
    Openapi.Service.query_parameter =
  (* TODO: use more precise schemas than strings.
     [kind] contains a [name] that could be used to deduce some constraints,
     like "it is a hash" or "its length is at most 10". *)
  let required =
    match kind with Optional _ | Multi _ | Flag -> false | Single _ -> true
  in
  (* Note: OpenAPI does not seem to have a field to specify that the parameter
     is repeatable (for [Multi]). *)
  {required; parameter = {name; description; schema = Openapi.Schema.string ()}}

let convert_service expected_path expected_method
    ({meth; path; description; query; input; output; error} : Api.service) :
    env * Openapi.Service.t =
  if expected_method <> meth then
    fail
      "expected method %s but found %s"
      (Method.to_http_string expected_method)
      (Method.to_http_string meth) ;
  if expected_path <> path then
    warn
      "expected path %s but found %s"
      (Api.show_path expected_path)
      (Api.show_path path) ;
  let env_1, request_body =
    opt_map_with_env (fun x -> convert_schema x.Api.json_schema) input
  in
  (* 200 is the HTTP code for OK. *)
  let env_2, output = convert_response ~code:200 output in
  let env_3, error = convert_response error in
  let responses = List.flatten [output; error] in
  let query = List.map convert_query_parameter query in
  let service =
    Openapi.Service.make ~description ?request_body ~query responses
  in
  let env = merge_env_list [env_1; env_2; env_3] in
  (env, service)

let convert_path_item (path_item : Api.path_item) : Openapi.Path.item =
  match path_item with
  | PI_static name -> Openapi.Path.static name
  | PI_dynamic arg ->
      Openapi.Path.dynamic
        ?description:arg.descr
        ~schema:(Openapi.Schema.string ())
        arg.name

let convert_path (path : Api.path) : Openapi.Path.t =
  List.map convert_path_item path

let convert_endpoint (endpoint : Api.service Api.endpoint) :
    env * Openapi.Endpoint.t =
  let convert_service = convert_service endpoint.path in
  let env_1, get = opt_map_with_env (convert_service GET) endpoint.get in
  let env_2, post = opt_map_with_env (convert_service POST) endpoint.post in
  let env_3, put = opt_map_with_env (convert_service PUT) endpoint.put in
  let env_4, delete =
    opt_map_with_env (convert_service DELETE) endpoint.delete
  in
  let env_5, patch = opt_map_with_env (convert_service PATCH) endpoint.patch in
  let endpoint =
    Openapi.Endpoint.make
      ?get
      ?post
      ?put
      ?delete
      ?patch
      (convert_path endpoint.path)
  in
  let env = merge_env_list [env_1; env_2; env_3; env_4; env_5] in
  (env, endpoint)

let convert_api version (endpoints : Api.service Api.endpoint list) : Openapi.t
    =
  let envs, endpoints = List.map convert_endpoint endpoints |> List.split in
  Openapi.make
    ~title:"Octez RPC"
    ~description:"The RPC API served by the Octez node."
    ~version
    ~definitions:(String_map.bindings (merge_env_list envs))
    endpoints
