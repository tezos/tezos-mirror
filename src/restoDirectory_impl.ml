(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

open Resto_impl
open Path

let (>>=) = Lwt.bind
let (>|=) = Lwt.(>|=)
module StringMap = Map.Make(String)

type (_,_,_,_,_,_) conv =
  | Z : (unit, 'g, 'g, unit, 'f, 'f) conv
  | S : ('t, 'g, 'b * 's, 'rt, 'f, 'r) conv ->
    ('t * 'b, 'g, 's, 'a * 'rt, 'a -> 'f, 'r) conv
let reverse
  : type a b c d e f. (a, c, unit, d, e, f) conv -> a -> c
  = fun c v ->
    let rec reverse
      : type a b c d e f g. (a, c, d, e, f, g) conv -> a -> d -> c
      = fun c v acc ->
        match c, v with
        | Z, _ -> acc
        | S c, (v, x) -> reverse c v (x, acc) in
    reverse c v ()
let rec curry
  : type a b c d e f. (a, b, c, d, e, f) conv -> e -> d -> f
  = fun c f ->
    match c with
    | Z -> fun () -> f
    | S c -> (fun (v, x) -> curry c (f v) x)
let curry c f =
  let f = curry c f in
  fun x -> f (reverse c x)

let descr x = x.Arg.descr

module Answer = struct

  type 'a answer =
    { code : int ;
      body : 'a option }

  let ok json = { code = 200 ; body = Some json }
  let return json = Lwt.return { code = 200 ; body = Some json }

end

open Answer

type step =
  | Static of string
  | Dynamic of Arg.descr

type conflict =
  | CService
  | CDir
  | CBuilder
  | CCustom
  | CTypes of Arg.descr * Arg.descr
  | CType of Arg.descr * string list

exception Conflict of step list * conflict
exception Cannot_parse of Arg.descr * string * string list

module Make(Repr : Json_repr.Repr) = struct

  include Json_encoding.Make(Repr)

  type 'key directory =
    | Map :
        ('key -> 'inner_key) * 'inner_key directory -> 'key directory
    | Static : 'key static_directory -> 'key directory
    | Dynamic :
        string option * ('key -> 'key directory Lwt.t) -> 'key directory
    | Custom :
        string option * ( 'key ->  string list -> custom_lookup Lwt.t) ->
      'key directory

  and 'key static_directory = {
    service: 'key registred_service option ;
    subdirs: 'key static_subdirectories option
  }

  and _ static_subdirectories =
    | Suffixes: 'key directory StringMap.t -> 'key static_subdirectories
    | Arg: 'a Arg.arg * ('key * 'a) directory -> 'key static_subdirectories

  and _ registred_service =
    | RegistredService:
        string option *
        'i Json_encoding.encoding * 'o Json_encoding.encoding *
        ('key -> 'i -> 'o answer Lwt.t) ->
      'key registred_service

  and custom_lookup =
    | CustomService of Description.service_descr *
                       (Repr.value -> Repr.value answer Lwt.t)
    | CustomDirectory of Description.directory_descr

  let empty = Static { service = None ; subdirs = None }

  let rec map_directory
    : type a b.
      (a -> b) -> b directory -> a directory
    = fun f t ->
      match t with
      | Map (g, dir) -> Map ((fun x -> g (f x)), dir)
      | Custom (descr, lookup) ->
          let lookup a = lookup (f a) in
          Custom (descr, lookup)
      | Dynamic (descr, builder) ->
          let builder a = builder (f a) >|= map_directory f in
          Dynamic (descr, builder)
      | Static dir ->
          Static (map_static_directory f dir)

  and map_static_directory
    : type a b.
      (a -> b) -> b static_directory -> a static_directory
    = fun f t ->
      { service = map_option (map_registred_service f) t.service ;
        subdirs = map_option (map_static_subdirectories f) t.subdirs ;
      }
  and map_static_subdirectories
    : type a b.
      (a -> b) -> b static_subdirectories -> a static_subdirectories
    = fun f t ->
      match t with
      | Suffixes map ->
          Suffixes (StringMap.map (map_directory f) map)
      | Arg (arg, dir) ->
          let dir = map_directory (fun (a, x) -> f a, x) dir in
          Arg (arg, dir)
  and map_registred_service
    : type a b pr.
      (a -> b) -> b registred_service -> a registred_service
    = fun f t ->
      match t with
      | RegistredService (d,i,o,h) ->
          RegistredService (d, i, o, (fun p i -> h (f p) i))

  let map = map_directory

  let prefix
    : type p pr. (pr, p) path -> p directory -> pr directory
    = fun path dir ->
      let rec prefix
        : type k pr. (pr, k) rpath -> k directory -> pr directory
        = fun path dir ->
          match path with
          | Root -> dir
          | Static (path, name) ->
              let subdirs = Suffixes (StringMap.singleton name dir) in
              prefix path (Static { subdirs = Some subdirs ; service = None })
          | Dynamic (path, arg) ->
              let subdirs = Arg (arg, dir) in
              prefix path (Static { subdirs = Some subdirs ; service = None }) in
      match path with
      | Path path -> prefix path dir
      | MappedPath (path, map, _) -> prefix path (map_directory map dir)

  let conflict steps kind = raise (Conflict (steps, kind))

  let rec merge
    : type p.
      step list -> p directory -> p directory -> p directory
    = fun path t1 t2 ->
      match t1, t2 with
      | Map (f, x), t ->
          merge path (map_directory f x) t
      | t, Map (f, x) ->
          merge path t (map_directory f x)
      | Static { subdirs = None ; service = None } , t
      | t, Static { subdirs = None ; service = None } -> t
      | Static n1, Static n2 ->
          Static (merge_static_directory path n1 n2)
      | Dynamic _, _
      | _, Dynamic _ -> conflict path CBuilder
      | Custom _, _
      | _, Custom _ -> conflict path CCustom

  and merge_static_directory
    : type p.
      step list -> p static_directory -> p static_directory -> p static_directory
    = fun path t1 t2 ->
      let subdirs =
        match t1.subdirs, t2.subdirs with
        | None, None -> None
        | None, Some dir | Some dir, None -> Some dir
        | Some d1, Some d2 ->
            match d1, d2 with
            | Suffixes m1, Suffixes m2 ->
                let merge =
                  StringMap.fold
                    (fun n t m ->
                       let st =
                         try StringMap.find n m with Not_found -> empty in
                       StringMap.add n (merge (Static n :: path) st t) m) in
                Some (Suffixes (merge m1 m2))
            | Arg (arg1, subt1), Arg (arg2, subt2) ->
                begin
                  try let Ty.Eq = Arg.eq arg1 arg2 in
                    let subt = merge (Dynamic (descr arg1) :: path) subt1 subt2 in
                    Some (Arg (arg1, subt))
                  with Ty.Not_equal ->
                    conflict path (CTypes (descr arg1, descr arg2))
                end
            | Arg (arg, _), Suffixes m ->
                conflict path
                  (CType (descr arg, List.map fst (StringMap.bindings m)))
            | Suffixes m, Arg (arg, _) ->
                conflict path
                  (CType (descr arg, List.map fst (StringMap.bindings m))) in
      let service =
        match t1.service, t2.service with
        | None, None -> None
        | None, Some s | Some s, None -> Some s
        | Some _, Some _ -> conflict path CService
      in
      { subdirs ; service }

  let merge x y = merge [] x y

  let rec describe_directory
    : type a p. ?recurse:bool -> a directory -> Description.directory_descr
    = fun ?(recurse = true) dir ->
      match dir with
      | Map (_, dir) -> describe_directory ~recurse dir
      | Dynamic (descr,_) -> Dynamic descr
      | Custom (descr,_) -> Dynamic descr
      | Static dir -> Static (describe_static_directory recurse dir)

  and describe_static_directory
    : type a p.
      bool -> a static_directory -> Description.static_directory_descr
    = fun recurse dir ->
      let service = map_option describe_service dir.service in
      if not recurse && service = None then raise Not_found ;
      let subdirs =
        if recurse
        then map_option describe_static_subdirectories dir.subdirs
        else None in
      { service ; subdirs }

  and describe_static_subdirectories
    : type a p.
      a static_subdirectories -> Description.static_subdirectories_descr
    = fun dir ->
      match dir with
      | Suffixes map ->
          Suffixes (StringMap.map describe_directory map)
      | Arg (arg,dir) ->
          Arg (descr arg, describe_directory dir)

  and describe_service
    : type a p.
      a registred_service -> Description.service_descr
    = fun service ->
      match service with
      | RegistredService (description,input,output,_) ->
          { description ;
            input = Json_encoding.schema input ;
            output = Json_encoding.schema output }

  let pp_print_directory ppf dir =
    Format.fprintf ppf "%a@."
      Description.pp_print_directory_descr (describe_directory dir)


  (****************************************************************************
   * Lookup
   ****************************************************************************)

  type resolved_directory =
      Dir: 'a directory * 'a -> resolved_directory

  let rec resolve
    : type a p.
      string list -> a directory -> a -> string list -> resolved_directory Lwt.t
    = fun prefix dir args path ->
      match path, dir with
      | _, Map (f, dir) -> resolve prefix dir (f args) path
      | _, Dynamic (_, builder) ->
          builder args >>= fun dir -> resolve prefix dir args path
      | _, Custom(descr, lookup) ->
          let lookup () _ = lookup args path in
          Lwt.return (Dir (Custom (descr, lookup), ()))
      | [], Static _ -> Lwt.return (Dir (dir, args))
      | name :: path, Static { subdirs = None } -> raise Not_found
      | name :: path, Static { subdirs = Some (Suffixes static) } ->
          resolve
            (name :: prefix) (StringMap.find name static) args path
      | name :: path, Static { subdirs = Some (Arg (arg, dir)) } ->
          match arg.Arg.destruct name with
          | Ok x -> resolve (name :: prefix) dir (args, x) path
          | Error msg ->
              raise (Cannot_parse (arg.Arg.descr, msg, name :: prefix))

  let lookup
    : type a p.
      a directory -> a -> string list ->
      (Repr.value -> Repr.value answer Lwt.t) Lwt.t
    = fun dir args path ->
      resolve [] dir args path >>= fun (Dir (dir, args)) ->
      match dir with
      | Static dir -> begin
          match dir.service with
          | None -> raise Not_found
          | Some (RegistredService (_, input, output, handler)) ->
              let call (json: Repr.value) : Repr.value answer Lwt.t =
                match destruct input json with
                | exception exn ->
                    let body =
                      let msg =
                        Format.asprintf "%a"
                          (fun ppf -> Json_encoding.print_error ppf) exn in
                      Repr.repr @@
                      `O [ "error",
                           Repr.repr @@
                           `String "input has wrong JSON structure" ;
                           "msg", Repr.repr @@
                           `String msg ] in
                    Lwt.return { code = 400 ; body = Some body }
                | input ->
                    Lwt.map
                      (fun ans ->
                         let body = map_option (construct output) ans.body in
                         { ans with body })
                      (handler args input) in
              Lwt.return call
        end
      | Map _ | Dynamic (_,_) -> assert false
      | Custom (_,lookup) -> begin
          lookup args [] >>= function
          | CustomService (_, handler) -> Lwt.return handler
          | CustomDirectory _ -> Lwt.fail Not_found
        end

(*
let describe_service
  : type a p. a directory -> a -> string list -> Description.service_descr
  = fun dir args path ->
    let Dir (dir, args) = resolve [] dir args path in
    match dir with
    | Static dir -> begin
        match dir.service with
        | None -> raise Not_found
        | Some (RegistredService (descr,_,_)) -> descr
      end
    | Dynamic (_,_) -> assert false
    | Custom (_,lookup) -> begin
        match lookup args [] with
        | CustomService (descr, _) -> descr
        | CustomDirectory _ -> raise Not_found
      end
*)

  let describe_directory
    : type a p.
      ?recurse:bool -> a directory -> a -> string list -> Description.directory_descr Lwt.t
    = fun ?recurse dir args path ->
      resolve [] dir args path >|= fun (Dir (dir, _)) ->
      describe_directory ?recurse dir



  (****************************************************************************
   * Registration
   ****************************************************************************)

  let rec step_of_path
    : type p rk. (rk, p) rpath -> step list -> step list
    = fun path acc ->
      match path with
      | Root -> acc
      | Static (path, name) -> step_of_path path (Static name :: acc)
      | Dynamic (path, arg) -> step_of_path path (Dynamic (descr arg) :: acc)
  let step_of_path p = step_of_path p []

  let conflict path kind = raise (Conflict (step_of_path path, kind))

  let rec insert
    : type k rk.
      (rk, k) rpath -> rk directory -> k directory * (k directory -> rk directory)
    = fun path dir ->
      match path with
      | Root -> dir, (fun x -> x)
      | Static (subpath, name) -> begin
          let subdir, rebuild = insert subpath dir in
          let dirmap, service =
            match subdir with
            | Map _ -> failwith "Not implemented"
            | Static { subdirs = None ; service } ->
                StringMap.empty, service
            | Static { subdirs = Some (Suffixes m) ;
                       service } ->
                m, service
            | Static { subdirs = Some (Arg (arg, _)) } ->
                conflict path (CType (descr arg, [name]))
            | Custom _ -> conflict path CCustom
            | Dynamic _ -> conflict path CBuilder in
          let dir =
            try StringMap.find name dirmap with Not_found -> empty in
          let rebuild s =
            let subdirs =
              Some (Suffixes (StringMap.add name s dirmap)) in
            rebuild (Static { subdirs ; service }) in
          dir, rebuild
        end
      | Dynamic (subpath, arg) -> begin
          let subdir, rebuild = insert subpath dir in
          let dir, service =
            match subdir with
            | Map _ -> failwith "Not implemented"
            | Static { subdirs = None ; service } ->
                empty, service
            | Static { subdirs = Some (Arg (arg', dir)) ;
                       service } -> begin
                try
                  let Ty.Eq = Arg.eq arg arg' in
                  (dir :> k directory), service
                with Ty.Not_equal ->
                  conflict path (CTypes (descr arg, descr arg'))
              end
            | Static { subdirs = Some (Suffixes m) } ->
                conflict path
                  (CType (descr arg, List.map fst (StringMap.bindings m)))
            | Dynamic _ -> conflict path CBuilder
            | Custom _ -> conflict path CCustom in
          let rebuild s =
            let subdirs = Some (Arg (arg, s)) in
            rebuild (Static { subdirs ; service }) in
          dir, rebuild
        end

  let register
    : type p i o pr.
      pr directory -> (pr, p, i, o) service ->
      (p -> i -> o answer Lwt.t) -> pr directory =
    fun root s handler ->
      let register
        : type k. (pr, k) rpath -> (k -> i -> o answer Lwt.t) -> pr directory =
        fun path handler ->
          let dir, insert = insert path root in
          let service =
            Some (RegistredService (s.description, s.input, s.output, handler)) in
          match dir with
          | Map _ -> failwith "Not implemented"
          | Static ({ service = None } as dir) ->
              insert (Static { dir with service })
          | Static _ -> conflict path CService
          | Custom _ -> conflict path CCustom
          | Dynamic _ -> conflict path CBuilder in
      match s.path with
      | Path p -> register p handler
      | MappedPath (p, map, _) -> register p (fun p i -> handler (map p) i)

  let register_dynamic_directory
    : type pr a pr.
      ?descr:string ->
      pr directory -> (pr, a) Path.path ->
      (a -> a directory Lwt.t) -> pr directory =
    fun ?descr root path builder ->
      let register
        : type k. (pr, k) rpath -> (k -> k directory Lwt.t) -> pr directory =
        fun path builder ->
          let dir, insert = insert path root in
          match dir with
          | Map _ -> failwith "Not implemented"
          | Static ({ service = None ; subdirs = None }) ->
              insert (Dynamic (descr, builder))
          | Static ({ service = Some _ }) -> conflict path CService
          | Static ({ subdirs = Some _ }) -> conflict path CDir
          | Custom _ -> conflict path CCustom
          | Dynamic _ -> conflict path CBuilder in
      match path with
      | Path p -> register p builder
      | MappedPath (p, map, _) ->
          register p
            (fun args -> builder (map args) >|= map_directory map)

  let register_custom_lookup
    : type pr a pr.
      ?descr:string ->
      pr directory -> (pr, a) path -> (a -> string list -> custom_lookup Lwt.t) ->
      pr directory =
    fun ?descr root path lookup ->
      let register
        : type k.
          (pr, k) rpath -> (k -> string list -> custom_lookup Lwt.t) ->
          pr directory
        = fun path lookup ->
          let dir, insert = insert path root in
          match dir with
          | Map _ -> failwith "Not implemented"
          | Static ({ service = None ; subdirs = None }) ->
              insert (Custom (descr, lookup))
          | Static ({ service = Some _ }) -> conflict path CService
          | Static ({ subdirs = Some _ }) -> conflict path CDir
          | Custom _ -> conflict path CCustom
          | Dynamic _ -> conflict path CBuilder in
      match path with
      | Path p -> register p lookup
      | MappedPath (p, map, _) ->
          register p (fun args -> lookup (map args))


  let register_describe_directory_service
    : type pr.
      pr directory -> (pr, pr, bool option, Description.directory_descr) service ->
      pr directory
  = fun root { description ; path ; output ; input } ->
    let descr : Description.service_descr = {
      description ;
      input = Json_encoding.schema input ;
      output = Json_encoding.schema output ;
    } in
    let dir = ref root in
    let lookup args path =
      let handler json =
        let recurse =
          match destruct input json with
          | exception _ -> false
          | None -> false
          | Some b -> b in
        describe_directory ~recurse !dir args path >>= fun d ->
        return (construct output d) in
      Lwt.return (CustomService (descr, handler)) in
    dir := register_custom_lookup root path lookup ;
    !dir


  (****************************************************************************
   * Let's currify!
   ****************************************************************************)

  let register0 root s f = register root s Resto_impl.(curry Z f)
  let register1 root s f = register root s Resto_impl.(curry (S Z) f)
  let register2 root s f = register root s Resto_impl.(curry (S (S Z)) f)
  let register3 root s f = register root s Resto_impl.(curry (S (S (S Z))) f)
  let register4 root s f = register root s Resto_impl.(curry (S (S (S (S Z)))) f)
  let register5 root s f = register root s Resto_impl.(curry (S (S (S (S (S Z))))) f)

  let register_dynamic_directory0 ?descr root s f =
    register_dynamic_directory ?descr root s Resto_impl.(curry Z f)
  let register_dynamic_directory1 ?descr root s f =
    register_dynamic_directory ?descr root s Resto_impl.(curry (S Z) f)
  let register_dynamic_directory2 ?descr root s f =
    register_dynamic_directory ?descr root s Resto_impl.(curry (S (S Z)) f)
  let register_dynamic_directory3 ?descr root s f =
    register_dynamic_directory ?descr root s Resto_impl.(curry (S (S (S Z))) f)

  let register_custom_lookup1 ?descr root s f =
    register_custom_lookup ?descr root s Resto_impl.(curry (S Z) f)
  let register_custom_lookup2 ?descr root s f =
    register_custom_lookup ?descr root s Resto_impl.(curry (S (S Z)) f)
  let register_custom_lookup3 ?descr root s f =
    register_custom_lookup ?descr root s Resto_impl.(curry (S (S (S Z))) f)

  end

include Make (Json_repr.Ezjsonm)
