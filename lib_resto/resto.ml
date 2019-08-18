(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)


module StringMap = Map.Make(String)
let map_option f = function
  | None -> None
  | Some x -> Some (f x)


module Internal = struct

  module Ty = struct

    type 'a witness = ..
    exception Not_equal
    type (_, _) eq = Eq : ('a, 'a) eq
    module type Ty = sig
      type t val witness : t witness
      val eq: 'a witness -> ('a, t) eq
    end
    type 'a id = (module Ty with type t = 'a)
    let new_id (type a) () =
      let module Ty = struct
        type t = a
        type 'a witness += Ty : t witness
        let witness = Ty
        let eq (type b) : b witness -> (b, t) eq =
          function Ty -> Eq | _ -> raise Not_equal
      end in
      (module Ty : Ty with type t = a)
    let eq : type a b. a id -> b id -> (a, b) eq =
      fun (module TyA) (module TyB) ->  TyB.eq TyA.witness

  end

  type descr = {
    name: string ;
    descr: string option ;
  }

  type 'a arg = {
    id: 'a Ty.id;
    destruct: string -> ('a, string) result ;
    construct: 'a -> string ;
    descr: descr ;
  }

  let from_arg x = x
  let to_arg x = x

  type (_,_) rpath =
    | Root : ('rkey, 'rkey) rpath
    | Static : ('rkey, 'key) rpath * string -> ('rkey, 'key) rpath
    | Dynamic : ('rkey, 'key) rpath * 'a arg -> ('rkey, 'key * 'a) rpath

  type (_,_) path =
    | Path: ('prefix, 'params) rpath -> ('prefix, 'params) path
    | MappedPath:
        ('prefix, 'key) rpath * ('key -> 'params) * ('params -> 'key) ->
      ('prefix, 'params) path

  let from_path x = x
  let to_path x = x

  type ('prefix, 'params, 'input, 'output) iservice = {
    description : string option ;
    path : ('prefix, 'params) path ;
    input : 'input Json_encoding.encoding ;
    output : 'output Json_encoding.encoding ;
  }

  let from_service x = x
  let to_service x = x

end

open Internal

module Ty = Internal.Ty

module Arg = struct

  type descr = Internal.descr = {
    name: string ;
    descr: string option ;
  }
  type 'a arg = 'a Internal.arg

  let eq a b = Ty.eq a.id b.id

  let make ?descr ~name ~destruct ~construct () =
    let id = Ty.new_id () in
    let descr = { name ; descr } in
    { descr ; id ; construct ; destruct }

  let descr (ty: 'a arg) = ty.descr

  let descr_encoding =
    let open Json_encoding in
    conv
      (fun {name; descr} -> (name, descr))
      (fun (name, descr) -> {name; descr})
      (obj2 (req "name" string) (opt "descr" string))

  let int =
    let int_of_string s =
      try Ok (int_of_string s)
      with Failure _ ->
        Error (Printf.sprintf "Cannot parse integer value: %S." s) in
    make "int" int_of_string string_of_int ()
  let float =
    let float_of_string s =
      try Ok (float_of_string s)
      with Failure _ ->
        Error (Printf.sprintf "Cannot parse float value: %S." s) in
    make "float" float_of_string string_of_float ()
  let int32 =
    let int32_of_string s =
      try Ok (Int32.of_string s)
      with Failure _ ->
        Error (Printf.sprintf "Cannot parse int32 value: %S." s) in
    make "int32" int32_of_string Int32.to_string ()
  let int64 =
    let int64_of_string s =
      try Ok (Int64.of_string s)
      with Failure _ ->
        Error (Printf.sprintf "Cannot parse int64 value: %S." s) in
    make "int64" int64_of_string Int64.to_string ()

end

module Path = struct

  type ('a, 'b) rpath = ('a, 'b) Internal.rpath
  type ('a, 'b) path = ('a, 'b) Internal.path

  type 'prefix context = ('prefix, 'prefix) path

  let root = Path Root

  let add_suffix path name =
    match path with
    | Path path -> Path (Static (path, name))
    | MappedPath (path, map, rmap) ->
        MappedPath (Static (path, name), map, rmap)

  let add_arg path arg =
    match path with
    | Path path -> Path (Dynamic (path, arg))
    | MappedPath (path, map, rmap) ->
        MappedPath (Dynamic (path, arg),
                    (fun (x, y) -> (map x, y)),
                    (fun (x, y) -> (rmap x, y)))

  let add_context :
    type a p. a Arg.arg -> p context -> (p * a) context =
    fun arg path ->
      match path with
      | Path Root -> Path Root
      | MappedPath (Root, map, rmap) ->
          MappedPath (Root,
                      (fun (x, y) -> (map x, y)),
                      (fun (x, y) -> (rmap x, y)))
      | _ -> failwith "Resto.Path.prefix: cannot prefix non-root path."

  let map map rmap = function
    | Path p -> MappedPath (p, map, rmap)
    | MappedPath (p, map', rmap') ->
        MappedPath (p, (fun x -> map (map' x)), (fun x -> rmap' (rmap x)))

  let prefix
    : type p pr a. (pr, a) path -> (a, p) path -> (pr, p) path
    = fun p1 p2 ->
      let rec prefix
        : type pr a k.
          (pr, a) path -> (a, k) rpath -> (pr, k) path
        = fun p1 p2 ->
          match p2 with
          | Root -> p1
          | Static (path, name) -> add_suffix (prefix p1 path) name
          | Dynamic (path, arg) -> add_arg (prefix p1 path) arg in
      match p2 with
      | Path p2 -> prefix p1 p2
      | MappedPath (p2, m, rm) -> map m rm (prefix p1 p2)

  let (/) = add_suffix
  let (/:) = add_arg
  let ( **/ ) = add_context


end

open Path

type ('prefix, 'params, 'input, 'output) service =
  ('prefix, 'params, 'input, 'output) Internal.iservice

let service ?description ~input ~output path =
  { description ; path ; input ; output }

let prefix path s = { s with path = Path.prefix path s.path }

module Make(Repr : Json_repr.Repr) = struct

  open Json_encoding
  include Make(Repr)

  type json = Repr.value

  let rec forge_request_args
    : type p. (unit, p) path -> p -> string list
    = fun path args ->
      let rec forge_request_args
        : type k. (unit, k) rpath -> k -> string list -> string list
        = fun path args acc ->
          let open Path in
          match path, args with
          | Root, _ ->
              acc
          | Static (path, name), args ->
              forge_request_args path args (name :: acc)
          | Dynamic (path, arg), (args, x) ->
              forge_request_args path args (arg.construct x :: acc) in
      match path with
      | Path path -> forge_request_args path args []
      | MappedPath (path, _, rmap) -> forge_request_args path (rmap args) []

  let forge_request
    : type p i o.
      (unit, p, i, o) service -> p -> i -> string list * Repr.value
    = fun s args arg ->
      forge_request_args s.path args,
      construct s.input arg

  let read_answer
    : type p i o.
      (unit, p, i, o) service -> Repr.value -> (o, string) result
    = fun s json ->
      try Ok (destruct s.output json)
      with exn ->
        Error
          (Format.asprintf "%a" (fun ppf -> Json_encoding.print_error ppf) exn)

end

include Make(Json_repr.Ezjsonm)

module Description = struct

  type service_descr = {
    description: string option ;
    input: Json_schema.schema ;
    output: Json_schema.schema ;
  }

  let service_descr_encoding =
    let open Json_encoding in
    conv
      (fun {description; input; output} -> (description, input, output))
      (fun (description, input, output) -> {description; input; output})
      (obj3 (opt "description" string)
         (req "input" any_schema)
         (req "output" any_schema))

  type directory_descr =
    | Static of static_directory_descr
    | Dynamic of string option

  and static_directory_descr = {
    service: service_descr option ;
    subdirs: static_subdirectories_descr option ;
  }

  and static_subdirectories_descr =
    | Suffixes of directory_descr Map.Make(String).t
    | Arg of Arg.descr * directory_descr

  let directory_descr_encoding =
    let open Json_encoding in
    mu "service_tree" @@ fun directory_descr_encoding ->
    let static_subdirectories_descr_encoding =
      union [
        case (obj1 (req  "suffixes"
                      (list (obj2 (req "name" string)
                               (req "tree" directory_descr_encoding)))))
          (function Suffixes map ->
            Some (StringMap.bindings map) | _ -> None)
          (fun m ->
             let add acc (n,t) =  StringMap.add n t acc in
             Suffixes (List.fold_left add StringMap.empty m)) ;
        case (obj1 (req "dynamic_dispatch"
                      (obj2 (req "arg" Arg.descr_encoding)
                         (req "tree" directory_descr_encoding))))
          (function Arg (ty, tree) -> Some (ty, tree) | _ -> None)
          (fun (ty, tree) -> Arg (ty, tree))
      ] in
    let static_directory_descr_encoding =
      conv
        (fun { service ; subdirs } -> (service, subdirs))
        (fun (service, subdirs) -> { service ; subdirs })
        (obj2 (opt "service" service_descr_encoding)
           (opt "subdirs" static_subdirectories_descr_encoding)) in
    union [
      case (obj1 (req "static" static_directory_descr_encoding))
        (function Static descr -> Some descr | _ -> None)
        (fun descr -> Static descr) ;
      case (obj1 (req "dynamic" (option string)))
        (function Dynamic descr -> Some descr | _ -> None)
        (fun descr -> Dynamic descr) ;
    ]

  let service ?description path =
    let description =
      match description with
      | Some descr -> descr
      | None -> "<TODO>"
    in
    service
      ~description
      ~input:Json_encoding.(obj1 (opt "recursive" bool))
      ~output:directory_descr_encoding
      path

  let rec pp_print_directory_descr ppf =
    let open Format in
    function
    | Static dir ->
        fprintf ppf "@[%a@]" pp_print_static_directory_descr dir
    | Dynamic None ->
        fprintf ppf "<dyntree>"
    | Dynamic (Some descr) ->
        fprintf ppf "<dyntree> : %s" descr

  and pp_print_static_directory_descr ppf =
    let open Format in
    function
    | { service = None ; subdirs = None } ->
        fprintf ppf "{}"
    | { service = Some service ; subdirs = None } ->
        fprintf ppf "%a"
          pp_print_dispatch_service_descr service
    | { service = None ; subdirs = Some subdirs } ->
        fprintf ppf "%a"
          pp_print_static_subdirectories_descr subdirs
    | { service = Some service ; subdirs = Some subdirs } ->
        fprintf ppf "@[<v>%a@ %a@]"
          pp_print_dispatch_service_descr service
          pp_print_static_subdirectories_descr subdirs

  and pp_print_static_subdirectories_descr ppf =
    let open Format in
    function
    | Suffixes map ->
        let print_binding ppf (name, tree) =
          fprintf ppf "@[<hov 2>%s:@ %a@]"
            name pp_print_directory_descr tree in
        fprintf ppf "@[<v>%a@]"
          (pp_print_list ~pp_sep:pp_print_cut print_binding)
          (StringMap.bindings map)
    | Arg (arg, tree) ->
        fprintf ppf "@[<hov 2>[:%s:]@ @[%a@]@]"
          (arg.name) pp_print_directory_descr tree

  and pp_print_dispatch_service_descr ppf =
    let open Format in
    function
    | { description = None} ->
        fprintf ppf "<service>"
    | { description = Some descr} ->
        fprintf ppf "<service> : %s" descr

end
