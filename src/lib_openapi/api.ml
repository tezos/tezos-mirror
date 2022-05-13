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

let fail x = Printf.ksprintf failwith x

(* A partially typed version of the result of /describe.
   Some parts are untyped: they are kept as JSON.
   But nothing is removed. *)

(* All types are parameterized so that this module can be used
   with partial parsing (useful to diff the JSONs) or with more parsing
   (useful for rpc_openapi). *)

(* First, we parse up to services. *)

type arg = {
  json : Json.t;
  (* arg as JSON, used by rpcdiff; we could remove this *)
  id : string;
  name : string;
  descr : string option;
}

type 'a tree = Static of 'a static | Dynamic of Json.t

and 'a static = {
  get_service : 'a option;
  post_service : 'a option;
  put_service : 'a option;
  delete_service : 'a option;
  patch_service : 'a option;
  subdirs : 'a subdirs option;
}

and 'a subdirs =
  | Suffixes of 'a suffix list
  | Dynamic_dispatch of {arg : arg; tree : 'a tree}

and 'a suffix = {name : string; tree : 'a tree}

let opt_mandatory name json = function
  | None -> Json.error json "missing mandatory value: %s" name
  | Some x -> x

let parse_arg (json : Json.t) : arg =
  Json.as_record json @@ fun get ->
  {
    json;
    id = get "id" |> opt_mandatory "id" json |> Json.as_string;
    name = get "name" |> opt_mandatory "name" json |> Json.as_string;
    descr = get "descr" |> Option.map Json.as_string;
  }

let rec parse_tree (json : Json.t) : Json.t tree =
  match Json.as_variant json with
  | "static", static ->
      Json.as_record static @@ fun get ->
      Static
        {
          get_service = get "get_service";
          post_service = get "post_service";
          put_service = get "put_service";
          delete_service = get "delete_service";
          patch_service = get "patch_service";
          subdirs = get "subdirs" |> Option.map parse_subdirs;
        }
  | "dynamic", dynamic -> Dynamic dynamic
  | name, _ -> failwith ("parse_tree: don't know what to do with: " ^ name)

and parse_subdirs (json : Json.t) : Json.t subdirs =
  match Json.as_variant json with
  | "suffixes", suffixes ->
      Suffixes (suffixes |> Json.as_list |> List.map parse_suffix)
  | "dynamic_dispatch", dynamic_dispatch ->
      Json.as_record dynamic_dispatch @@ fun get ->
      Dynamic_dispatch
        {
          arg =
            get "arg"
            |> opt_mandatory "dynamic_dispatch.arg" dynamic_dispatch
            |> parse_arg;
          tree =
            get "tree"
            |> opt_mandatory "dynamic_dispatch.tree" dynamic_dispatch
            |> parse_tree;
        }
  | name, _ -> failwith ("parse_subdir: don't know what to do with: " ^ name)

and parse_suffix (json : Json.t) : Json.t suffix =
  Json.as_record json @@ fun get ->
  {
    name = get "name" |> opt_mandatory "suffixes.name" json |> Json.as_string;
    tree = get "tree" |> opt_mandatory "suffixes.tree" json |> parse_tree;
  }

(* We also have to manipulate flattened versions of the tree. *)

type path_item = PI_static of string | PI_dynamic of arg

let show_path_item = function
  | PI_static name -> name
  | PI_dynamic arg -> "{" ^ arg.name ^ "}"

type path = path_item list

let show_path path = "/" ^ String.concat "/" (List.map show_path_item path)

type 'a endpoint = {
  path : path;
  get : 'a option;
  post : 'a option;
  put : 'a option;
  delete : 'a option;
  patch : 'a option;
}

(* [path] and [acc] are in reverse order.
   Return a list in reverse order as well (but paths are not returned in reverse order). *)
let rec flatten_tree path acc tree =
  match tree with
  | Static static -> flatten_static path acc static
  | Dynamic _ ->
      (* We ignore those for now. *)
      acc

and flatten_static path acc static =
  let acc =
    match
      ( static.get_service,
        static.post_service,
        static.put_service,
        static.delete_service,
        static.patch_service )
    with
    | None, None, None, None, None -> acc
    | _ ->
        let endpoint =
          {
            path = List.rev path;
            get = static.get_service;
            post = static.post_service;
            put = static.put_service;
            delete = static.delete_service;
            patch = static.patch_service;
          }
        in
        endpoint :: acc
  in
  match static.subdirs with
  | None -> acc
  | Some subdirs -> flatten_subdirs path acc subdirs

and flatten_subdirs path acc subdirs =
  match subdirs with
  | Suffixes suffixes -> List.fold_left (flatten_suffix path) acc suffixes
  | Dynamic_dispatch {arg; tree} ->
      flatten_tree (PI_dynamic arg :: path) acc tree

and flatten_suffix path acc suffix =
  flatten_tree (PI_static suffix.name :: path) acc suffix.tree

let flatten tree = flatten_tree [] [] tree |> List.rev

(* Second, we parse services (the part that we need). *)

type schemas = {json_schema : Json.t; binary_schema : Json.t}

type query_parameter_kind =
  | Optional of {name : string}
  | Multi of {name : string}
  | Single of {name : string}
  | Flag

type query_parameter = {
  id : string option;
  name : string;
  description : string option;
  kind : query_parameter_kind;
}

type service = {
  meth : Method.t;
  path : path_item list;
  description : string;
  query : query_parameter list;
  input : schemas option;
  output : schemas option;
  error : schemas option;
}

let parse_schemas (json : Json.t) : schemas =
  Json.as_record json @@ fun get ->
  {
    json_schema = get "json_schema" |> opt_mandatory "json_schema" json;
    binary_schema = get "binary_schema" |> opt_mandatory "binary_schema" json;
  }

let parse_path_item (json : Json.t) : path_item =
  match Json.as_string_opt json with
  | Some s -> PI_static s
  | None -> PI_dynamic (parse_arg json)

let parse_path (json : Json.t) : path =
  json |> Json.as_list |> List.map parse_path_item

let parse_query_parameter (json : Json.t) : query_parameter =
  Json.as_record json @@ fun get ->
  (* First, fetch information which is at the top level of the record. *)
  let name = get "name" |> opt_mandatory "name" json |> Json.as_string in
  let description = get "description" |> Option.map Json.as_string in
  (* Then, fetch information which is in the "kind" field. *)
  let kind, id, descr =
    (get "kind" |> opt_mandatory "kind" json |> Json.as_record) @@ fun get ->
    (* Function used for everything but kind "flag". *)
    let parse_kind_with_name make record =
      Json.as_record record @@ fun get ->
      let name =
        get "name" |> opt_mandatory "kind.name" json |> Json.as_string
      in
      ( make name,
        get "id" |> Option.map Json.as_string,
        get "descr" |> Option.map Json.as_string )
    in
    (* Field "kind" encodes a variant.
       There must be exactly one of either: "optional", "multi", "single" or "flag". *)
    match (get "optional", get "multi", get "single", get "flag") with
    | Some optional, None, None, None ->
        parse_kind_with_name (fun name -> Optional {name}) optional
    | None, Some multi, None, None ->
        parse_kind_with_name (fun name -> Multi {name}) multi
    | None, None, Some single, None ->
        parse_kind_with_name (fun name -> Single {name}) single
    | None, None, None, Some flag ->
        let () =
          Json.as_record flag @@ fun _get ->
          (* Flags have no fields. *)
          ()
        in
        (Flag, None, None)
    | _ -> fail "unsupported kind for query parameter %s" name
  in
  (* Both the top level and the kind can contain a description. Merge them. *)
  let description =
    match (description, descr) with
    | None, None -> None
    | (Some _ as x), None | None, (Some _ as x) -> x
    | Some x, Some y -> Some (y ^ " " ^ x)
  in
  {id; name; description; kind}

let parse_service (json : Json.t) : service =
  Json.as_record json @@ fun get ->
  {
    meth =
      get "meth" |> opt_mandatory "meth" json |> Json.as_string
      |> Method.of_http_string;
    path = get "path" |> opt_mandatory "path" json |> parse_path;
    description =
      get "description" |> Option.map Json.as_string
      |> Option.value ~default:"(no description)";
    query =
      get "query" |> opt_mandatory "query" json |> Json.as_list
      |> List.map parse_query_parameter;
    input = get "input" |> Option.map parse_schemas;
    output = get "output" |> Option.map parse_schemas;
    error = get "error" |> Option.map parse_schemas;
  }

let rec map_tree f tree =
  match tree with
  | Static static -> Static (map_static f static)
  | Dynamic json -> Dynamic json

and map_static f static =
  {
    get_service = Option.map f static.get_service;
    post_service = Option.map f static.post_service;
    put_service = Option.map f static.put_service;
    delete_service = Option.map f static.delete_service;
    patch_service = Option.map f static.patch_service;
    subdirs = Option.map (map_subdirs f) static.subdirs;
  }

and map_subdirs f subdirs =
  match subdirs with
  | Suffixes suffixes -> Suffixes (List.map (map_suffix f) suffixes)
  | Dynamic_dispatch {arg; tree} ->
      Dynamic_dispatch {arg; tree = map_tree f tree}

and map_suffix f suffix = {name = suffix.name; tree = map_tree f suffix.tree}

let parse_services = map_tree parse_service
