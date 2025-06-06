(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

module StringMap = Map.Make (String)

type 'key t = 'key desc_with_path

(** [desc_with_path] describes a position in the storage. It's composed
    [rev_path] which is the reverse path up to the position, and [dir] the
    position's [description]. [rev_path] is only useful in case of an error to
    print a descriptive message. [List.rev rev_path] is a storage's path that
    contains no conflict and allows the registration of a [dir]'s storage.
    NB: [rev_path] indicates the position in the tree, so once the node is
    added, it won't change; whereas [dir] is mutable because when more subtrees
    are added this may require updating it. *)
and 'key desc_with_path = {
  rev_path : string list;
  mutable dir : 'key description;
}

and 'key description =
  | Empty : 'key description
  | Value : {
      get : 'key -> 'a option tzresult Lwt.t;
      encoding : 'a Data_encoding.t;
    }
      -> 'key description
  | NamedDir : 'key t StringMap.t -> 'key description
  | IndexedDir : {
      arg : 'a RPC_arg.t;
      arg_encoding : 'a Data_encoding.t;
      list : 'key -> 'a list tzresult Lwt.t;
      subdir : ('key * 'a) t;
    }
      -> 'key description

let rec pp : type a. Format.formatter -> a t -> unit =
 fun ppf {dir; _} ->
  match dir with
  | Empty -> Format.fprintf ppf "Empty"
  | Value _e -> Format.fprintf ppf "Value"
  | NamedDir map ->
      Format.fprintf
        ppf
        "@[<v>%a@]"
        (Format.pp_print_list pp_item)
        (StringMap.bindings map)
  | IndexedDir {arg; subdir; _} ->
      let name = Format.asprintf "<%s>" (RPC_arg.descr arg).name in
      pp_item ppf (name, subdir)

and pp_item : type a. Format.formatter -> string * a t -> unit =
 fun ppf (name, desc) -> Format.fprintf ppf "@[<hv 2>%s@ %a@]" name pp desc

let pp_rev_path ppf path =
  Format.fprintf
    ppf
    "[%a]"
    Format.(
      pp_print_list
        ~pp_sep:(fun ppf () -> pp_print_string ppf " / ")
        pp_print_string)
    (List.rev path)

let rec register_named_subcontext : type r. r t -> string list -> r t =
 fun desc names ->
  match (desc.dir, names) with
  | _, [] -> desc
  | Value _, _ | IndexedDir _, _ ->
      Format.kasprintf
        invalid_arg
        "Could not register a named subcontext at %a because of an existing %a."
        pp_rev_path
        desc.rev_path
        pp
        desc
  | Empty, name :: names ->
      let subdir = {rev_path = name :: desc.rev_path; dir = Empty} in
      desc.dir <- NamedDir (StringMap.singleton name subdir) ;
      register_named_subcontext subdir names
  | NamedDir map, name :: names ->
      let subdir =
        match StringMap.find name map with
        | Some subdir -> subdir
        | None ->
            let subdir = {rev_path = name :: desc.rev_path; dir = Empty} in
            desc.dir <- NamedDir (StringMap.add name subdir map) ;
            subdir
      in
      register_named_subcontext subdir names

type (_, _, _) args =
  | One : {
      rpc_arg : 'a RPC_arg.t;
      encoding : 'a Data_encoding.t;
      compare : 'a -> 'a -> int;
    }
      -> ('key, 'a, 'key * 'a) args
  | Pair :
      ('key, 'a, 'inter_key) args * ('inter_key, 'b, 'sub_key) args
      -> ('key, 'a * 'b, 'sub_key) args

let rec unpack : type a b c. (a, b, c) args -> c -> a * b = function
  | One _ -> fun x -> x
  | Pair (l, r) ->
      let unpack_l = unpack l in
      let unpack_r = unpack r in
      fun x ->
        let c, d = unpack_r x in
        let b, a = unpack_l c in
        (b, (a, d))

let rec pack : type a b c. (a, b, c) args -> a -> b -> c = function
  | One _ -> fun b a -> (b, a)
  | Pair (l, r) ->
      let pack_l = pack l in
      let pack_r = pack r in
      fun b (a, d) ->
        let c = pack_l b a in
        pack_r c d

let rec compare : type a b c. (a, b, c) args -> b -> b -> int = function
  | One {compare; _} -> compare
  | Pair (l, r) -> (
      let compare_l = compare l in
      let compare_r = compare r in
      fun (a1, b1) (a2, b2) ->
        match compare_l a1 a2 with 0 -> compare_r b1 b2 | x -> x)

let destutter equal l =
  match l with
  | [] -> []
  | (i, _) :: l ->
      let rec loop acc i = function
        | [] -> acc
        | (j, _) :: l -> if equal i j then loop acc i l else loop (j :: acc) j l
      in
      loop [i] i l

let rec register_indexed_subcontext :
    type r a b.
    r t -> list:(r -> a list tzresult Lwt.t) -> (r, a, b) args -> b t =
 fun desc ~list path ->
  let open Lwt_result_syntax in
  match path with
  | Pair (left, right) ->
      let compare_left = compare left in
      let equal_left x y = Compare.Int.(compare_left x y = 0) in
      let list_left r =
        let+ l = list r in
        destutter equal_left l
      in
      let list_right r =
        let a, k = unpack left r in
        let+ l = list a in
        List.map snd (List.filter (fun (x, _) -> equal_left x k) l)
      in
      register_indexed_subcontext
        (register_indexed_subcontext desc ~list:list_left left)
        ~list:list_right
        right
  | One {rpc_arg = arg; encoding = arg_encoding; _} -> (
      match desc.dir with
      | Value _ | NamedDir _ ->
          Format.kasprintf
            invalid_arg
            "Could not register an indexed subcontext at %a because of an \
             existing %a."
            pp_rev_path
            desc.rev_path
            pp
            desc
      | Empty ->
          let subdir =
            {
              rev_path =
                Format.sprintf "(Maybe of %s)" RPC_arg.(descr arg).name
                :: desc.rev_path;
              dir = Empty;
            }
          in
          desc.dir <- IndexedDir {arg; arg_encoding; list; subdir} ;
          subdir
      | IndexedDir {arg = inner_arg; subdir; _} -> (
          match RPC_arg.eq arg inner_arg with
          | None ->
              Format.kasprintf
                invalid_arg
                "An indexed subcontext at %a already exists but has a \
                 different argument: `%s` <> `%s`."
                pp_rev_path
                desc.rev_path
                (RPC_arg.descr arg).name
                (RPC_arg.descr inner_arg).name
          | Some RPC_arg.Eq -> subdir))

let register_value :
    type a b.
    a t -> get:(a -> b option tzresult Lwt.t) -> b Data_encoding.t -> unit =
 fun desc ~get encoding ->
  match desc.dir with
  | Empty -> desc.dir <- Value {get; encoding}
  | _ ->
      Format.kasprintf
        invalid_arg
        "Could not register a value at %a because of an existing %a."
        pp_rev_path
        desc.rev_path
        pp
        desc

let create () = {rev_path = []; dir = Empty}

module type INDEX = sig
  type t

  include Path_encoding.S with type t := t

  val rpc_arg : t RPC_arg.t

  val encoding : t Data_encoding.t

  val compare : t -> t -> int
end

type _ handler =
  | Handler : {
      encoding : 'a Data_encoding.t;
      get : 'key -> int -> 'a tzresult Lwt.t;
    }
      -> 'key handler

type _ opt_handler =
  | Opt_handler : {
      encoding : 'a Data_encoding.t;
      get : 'key -> int -> 'a option tzresult Lwt.t;
    }
      -> 'key opt_handler

let rec combine_object =
  let open Lwt_result_syntax in
  function
  | [] ->
      Handler {encoding = Data_encoding.unit; get = (fun _ _ -> return_unit)}
  | (name, Opt_handler handler) :: fields ->
      let (Handler handlers) = combine_object fields in
      Handler
        {
          encoding =
            Data_encoding.merge_objs
              Data_encoding.(obj1 (opt name (dynamic_size handler.encoding)))
              handlers.encoding;
          get =
            (fun k i ->
              let* v1 = handler.get k i in
              let* v2 = handlers.get k i in
              return (v1, v2));
        }

type query = {depth : int}

let depth_query =
  let open RPC_query in
  query (fun depth -> {depth})
  |+ field "depth" RPC_arg.uint 0 (fun t -> t.depth)
  |> seal

let build_directory : type key. key t -> key RPC_directory.t =
 fun dir ->
  let rpc_dir = ref (RPC_directory.empty : key RPC_directory.t) in
  let register :
      type ikey.
      chunked:bool -> (key, ikey) RPC_path.t -> ikey opt_handler -> unit =
   fun ~chunked path (Opt_handler {encoding; get}) ->
    let service =
      RPC_service.get_service ~query:depth_query ~output:encoding path
    in
    rpc_dir :=
      RPC_directory.opt_register ~chunked !rpc_dir service (fun k q () ->
          get k (q.depth + 1))
  in
  let rec build_handler :
      type ikey. ikey t -> (key, ikey) RPC_path.t -> ikey opt_handler =
    let open Lwt_result_syntax in
    fun desc path ->
      match desc.dir with
      | Empty ->
          Opt_handler
            {encoding = Data_encoding.unit; get = (fun _ _ -> return_none)}
      | Value {get; encoding} ->
          let handler =
            Opt_handler
              {
                encoding;
                get =
                  (fun k i ->
                    if Compare.Int.(i < 0) then return_none else get k);
              }
          in
          register ~chunked:true path handler ;
          handler
      | NamedDir map ->
          let fields = StringMap.bindings map in
          let fields =
            List.map
              (fun (name, dir) ->
                (name, build_handler dir RPC_path.(path / name)))
              fields
          in
          let (Handler handler) = combine_object fields in
          let handler =
            Opt_handler
              {
                encoding = handler.encoding;
                get =
                  (fun k i ->
                    if Compare.Int.(i < 0) then return_none
                    else
                      let* v = handler.get k (i - 1) in
                      return_some v);
              }
          in
          register ~chunked:true path handler ;
          handler
      | IndexedDir {arg; arg_encoding; list; subdir} ->
          let (Opt_handler handler) =
            build_handler subdir RPC_path.(path /: arg)
          in
          let encoding =
            let open Data_encoding in
            union
              [
                case
                  (Tag 0)
                  ~title:"Leaf"
                  (dynamic_size arg_encoding)
                  (function key, None -> Some key | _ -> None)
                  (fun key -> (key, None));
                case
                  (Tag 1)
                  ~title:"Dir"
                  (tup2
                     (dynamic_size arg_encoding)
                     (dynamic_size handler.encoding))
                  (function key, Some value -> Some (key, value) | _ -> None)
                  (fun (key, value) -> (key, Some value));
              ]
          in
          let get k i =
            if Compare.Int.(i < 0) then return_none
            else if Compare.Int.(i = 0) then return_some []
            else
              let* keys = list k in
              let* values =
                List.map_es
                  (fun key ->
                    if Compare.Int.(i = 1) then return (key, None)
                    else
                      let+ value = handler.get (k, key) (i - 1) in
                      (key, value))
                  keys
              in
              return_some values
          in
          let handler =
            Opt_handler
              {encoding = Data_encoding.(list (dynamic_size encoding)); get}
          in
          register ~chunked:true path handler ;
          handler
  in
  ignore (build_handler dir RPC_path.open_root : key opt_handler) ;
  !rpc_dir
