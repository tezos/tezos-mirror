(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

type t = {namespace : string; config : Data_encoding.json; children : t list}

let empty =
  {namespace = Namespace.(to_string empty); config = `Null; children = []}

(* We will need to assume an arbitrary root, which will represent the empty list of t *)
let encoding : t Data_encoding.t =
  let open Data_encoding in
  mu "config_tree" (fun x ->
      conv
        (function
          | {namespace; config; children} -> (namespace, config, children))
        (fun (namespace, config, children) -> {namespace; config; children})
        (obj3
           (req "namespace" string)
           (req "config" json)
           (req "children" (list (dynamic_size x)))))

(* [merge_on] is a Json merging method used to update the fields of a Json document with new values.
   [ja] contains the basic structure and types of the document. [ja] and [merge_on ja jb] have the
   same structure/can be destructed using the same [Data_encoding.t]. In practice, we use it to sequentially
   aggregate all the configurations along a namespace path in the configuration document, and obtain
   the final configuration used during a given benchmark, and to update a meta config file.
*)
(* We represent json documents as trees with labelled edges and labelled nodes of three kinds:
     - value nodes, with no children, labelled with a type (string, float or bool), and a value of that type
     - array nodes, labelled with their number of children n. The outgoing edges are labelled from 0 to n-1.
     - and object nodes, their outgoing edges are labelled with distinct string values.
   Let A and B be two json documents, v a value node of A, and P(A,v) its path in A. Let f(A) be the modified
   tree from A in which we ignore the values in the value nodes. We say that v "exists" in B if and only if
   the path f(P(A,v)) is a subtree (included) in f(B).
   We have [merge_on A B] = [C], with [C] the only json document such that
   - f(A) = f(C)
   - For each value node v in A,
     - if v exists in B, then P(B,v) is included in C.
     - else, P(A,v) is included in C
*)
let rec merge_on ja jb =
  match (ja, jb) with
  | `String _, `String sb -> `String sb
  | `Float _, `Float f2 -> `Float f2
  | `Bool _, `Bool b2 -> `Bool b2
  | `A l1, `A l2 -> (
      match
        List.map2 ~when_different_lengths:() (fun x y -> merge_on x y) l1 l2
      with
      | Ok l -> `A l
      | Error () -> `A l1)
  | `O l1, `O l2 ->
      let tl =
        List.fold_left
          (fun acc (n, f) ->
            match List.find (fun (x, _) -> String.equal x n) l2 with
            | None -> (n, f) :: acc
            | Some (_, o) -> (n, merge_on f o) :: acc)
          []
          l1
      in
      (* List.rev is just aesthetic, it maintains the order of the object fields.
         Convenient for fields like [min] and [max] *)
      `O (List.rev tl)
  | `Null, _ -> jb
  | _ -> ja

let get_config ((module Bench) : Benchmark.t) (config : t) : Data_encoding.json
    =
  let default =
    Data_encoding.Json.construct
      ~include_default_fields:`Always
      Bench.config_encoding
      Bench.default_config
  in
  let rec traversal config_acc name config =
    let config_acc = merge_on config_acc config.config in
    match name with
    | h :: t -> (
        match
          List.find (fun x -> String.equal h x.namespace) config.children
        with
        | None -> config_acc
        | Some c -> traversal config_acc t c)
    | [] -> config_acc
  in
  traversal
    default
    (Namespace.to_list Bench.name)
    {namespace = ""; config = `Null; children = [config]}

(* [get_config_strict name conf] returns the configuration defined exactly at the namespace [name] in [conf].
   We currently call it for when we need to edit [conf] precisely at [name] *)
let get_config_strict (name : Namespace.t) (config_full : t) :
    Data_encoding.json =
  let rec traversal name config =
    match name with
    | h :: t -> (
        match
          List.find (fun x -> String.equal h x.namespace) config.children
        with
        | None -> `Null
        | Some c -> traversal t c)
    | [] -> config.config
  in
  traversal
    (Namespace.to_list name)
    {namespace = ""; config = `Null; children = [config_full]}

let merge_config_trees (dst : t) (src : t) : t =
  let get_child namespace {children; _} : t option =
    List.find (fun x -> String.equal x.namespace namespace) children
  in
  let rec build namespace subtree1 subtree2 =
    let default = {namespace; config = `Null; children = []} in
    let subtree1 = Option.value ~default subtree1 in
    let config =
      match subtree2 with None -> subtree1.config | Some c -> c.config
    in
    let subtree2 = Option.value ~default subtree2 in
    let all_children =
      List.sort_uniq
        String.compare
        (List.map
           (fun x -> x.namespace)
           (subtree1.children @ subtree2.children))
    in
    match all_children with
    | [] -> {namespace; config; children = []}
    | _ ->
        {
          namespace;
          config;
          children =
            List.map
              (fun x -> build x (get_child x subtree1) (get_child x subtree2))
              all_children;
        }
  in
  if not (String.equal dst.namespace ".") then (
    Format.eprintf
      "Failure while merging: root namespace in destination should be \".\" \
       (got %s)@."
      dst.namespace ;
    exit 1)
  else if not (String.equal src.namespace ".") then (
    Format.eprintf
      "Failure while merging: root namespace in source should be \".\" (got \
       %s)@."
      src.namespace ;
    exit 1)
  else build "." (Some dst) (Some src)

let parse_config (type c t) ?print ((module Bench) : (c, t) Benchmark.poly)
    config_file =
  let default_config () =
    Format.eprintf
      "Using default configuration for benchmark %a@."
      Namespace.pp
      Bench.name ;
    Data_encoding.Json.construct Bench.config_encoding Bench.default_config
  in
  let try_load_custom_config path =
    let json =
      match Benchmark_helpers.load_json path with
      | Ok json ->
          Format.eprintf
            "Looking for custom configuration for benchmark %a@."
            Namespace.pp
            Bench.name ;
          Some json
      | Error (Sys_error err) ->
          Format.eprintf "Failed loading json %s (Ignoring)@." err ;
          None
      | Error exn -> raise exn
    in
    json
  in
  let decode encoding json =
    try Data_encoding.Json.destruct encoding json
    with exn ->
      Format.eprintf
        "Json deserialization error:@.  %a@."
        (Data_encoding.Json.print_error ~print_unknown:(fun _ exn -> raise exn))
        exn ;
      exit 1
  in
  match config_file with
  | None ->
      let json = default_config () in
      Format.eprintf "%a@." Data_encoding.Json.pp json ;
      Bench.default_config
  | Some file -> (
      let json = try_load_custom_config file in
      let json =
        match json with
        | None -> default_config ()
        | Some json ->
            let json = decode encoding json in
            Format.eprintf "Config file successfully parsed@." ;
            get_config (module Bench) json
      in
      match json with
      | `Null ->
          let json = default_config () in
          Format.eprintf "%a@." Data_encoding.Json.pp json ;
          Bench.default_config
      | json ->
          let config = decode Bench.config_encoding json in
          Format.eprintf
            "Loaded configuration from %s for benchmark %a@."
            file
            Namespace.pp
            Bench.name ;
          (match print with
          | None -> ()
          | Some out ->
              Format.fprintf
                (Format.formatter_of_out_channel out)
                "%a@."
                Data_encoding.Json.pp
                json) ;
          config)

let rec build_branch config name =
  match name with
  | [namespace] -> {namespace; config; children = []}
  | namespace :: t ->
      {namespace; config = `Null; children = [build_branch config t]}
  | [] -> assert false
(* This [assert false] is not reachable because this function is not exported and only called on
   the result of Namespace.to_list which is never empty. *)

let generate_one (module Bench : Benchmark.S) =
  let config =
    Data_encoding.Json.construct Bench.config_encoding Bench.default_config
  in
  build_branch config (Namespace.to_list Bench.name)

let generate_default bench_list =
  List.fold_left
    (fun acc b -> merge_config_trees acc (generate_one b))
    empty
    bench_list

let merge_config_files ?(delete_src = false) ~(dst : string) ~(src : string) ()
    =
  let dst_path = dst in
  let src_path = src in
  let dst =
    match Benchmark_helpers.load_json dst_path with
    | Ok dst -> dst
    | Error exn -> raise exn
  in
  let src =
    match Benchmark_helpers.load_json src_path with
    | Ok src -> src
    | Error exn -> raise exn
  in
  let dst = Data_encoding.Json.destruct encoding dst in
  let src = Data_encoding.Json.destruct encoding src in
  let rslt = merge_config_trees dst src in
  let rslt_str =
    Data_encoding.Json.construct encoding rslt |> Data_encoding.Json.to_string
  in
  let _nwritten =
    Lwt_main.run @@ Lwt_utils_unix.create_file dst_path rslt_str
  in
  if delete_src then Stdlib.Sys.remove src_path

let edit_config ?(input = `Stdin) config_path namespace =
  let tmpfile =
    Filename.(concat (get_temp_dir_name ()) "octez-snoop_config_edit")
  in
  let command e = Format.sprintf "%s %s" e tmpfile in
  let try_edit e =
    let ret = Stdlib.Sys.command (command e) in
    if ret != 0 then
      Format.eprintf "Config file edition failed: could not open editor@."
    else ()
  in
  let config =
    match Benchmark_helpers.load_json config_path with
    | Ok c -> Data_encoding.Json.destruct encoding c
    | Error exn -> raise exn
  in
  let base_conf =
    get_config_strict namespace config |> Data_encoding.Json.to_string
  in
  let edited =
    try
      match input with
      | `Edit e -> (
          let _nwritten =
            Lwt_main.run @@ Lwt_utils_unix.create_file tmpfile base_conf
          in
          try_edit e ;
          match Benchmark_helpers.load_json tmpfile with
          | Ok c -> c
          | Error exn -> raise exn)
      | `File i -> (
          match Benchmark_helpers.load_json i with
          | Ok c -> c
          | Error exn -> raise exn)
      | `String s -> Ezjsonm.from_string s
      | `Stdin -> Ezjsonm.from_channel In_channel.stdin
    with Ezjsonm.Parse_error (_, s) ->
      Format.eprintf "Config file edition failed: %s@." s ;
      exit 1
  in
  let new_conf_branch = build_branch edited (Namespace.to_list namespace) in
  let new_conf = merge_config_trees config new_conf_branch in
  let new_conf_str =
    Data_encoding.Json.construct encoding new_conf
    |> Data_encoding.Json.to_string
  in
  let _nwritten =
    Lwt_main.run @@ Lwt_utils_unix.create_file config_path new_conf_str
  in
  match input with `Edit _ -> Stdlib.Sys.remove tmpfile | _ -> ()
