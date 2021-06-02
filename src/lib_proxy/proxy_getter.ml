(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

module Local = Tezos_context_memory.Context

module Events = struct
  include Internal_event.Simple

  let section = ["proxy_getter"]

  let cache_hit =
    declare_1
      ~section
      ~name:"cache_hit"
      ~msg:"Cache hit: ({key})"
      ~level:Debug
      ("key", Data_encoding.string)

  let cache_miss =
    declare_1
      ~section
      ~name:"cache_miss"
      ~msg:"Cache miss: ({key})"
      ~level:Debug
      ("key", Data_encoding.string)
end

let rec raw_context_size = function
  | Tezos_shell_services.Block_services.Key _ | Cut -> 0
  | Dir map ->
      TzString.Map.fold (fun _key v acc -> acc + 1 + raw_context_size v) map 0

let rec raw_context_to_tree
    (raw : Tezos_shell_services.Block_services.raw_context) :
    Local.tree option Lwt.t =
  match raw with
  | Key (bytes : Bytes.t) ->
      Lwt.return (Some (Local.Tree.of_raw (`Value bytes)))
  | Cut -> Lwt.return None
  | Dir map ->
      let add_to_tree tree (string, raw_context) =
        raw_context_to_tree raw_context >>= function
        | None -> Lwt.return tree
        | Some u -> Local.Tree.add_tree tree [string] u
      in
      TzString.Map.bindings map
      |> List.fold_left_s add_to_tree (Local.Tree.empty Local.empty)
      >|= fun dir -> if Local.Tree.is_empty dir then None else Some dir

module type M = sig
  val proxy_dir_mem :
    Proxy.proxy_getter_input -> Local.key -> bool tzresult Lwt.t

  val proxy_get :
    Proxy.proxy_getter_input -> Local.key -> Local.tree option tzresult Lwt.t

  val proxy_mem : Proxy.proxy_getter_input -> Local.key -> bool tzresult Lwt.t
end

type proxy_m = (module M)

module StringMap = TzString.Map

module Tree : Proxy.TREE with type t = Local.tree with type key = Local.key =
struct
  type t = Local.tree

  type key = Local.key

  let empty = Local.Tree.empty Local.empty

  let get = Local.Tree.find_tree

  let set_leaf tree key raw_context : t Proxy.update tzresult Lwt.t =
    (raw_context_to_tree raw_context >>= function
     | None -> Lwt.return tree
     | Some sub_tree -> Local.Tree.add_tree tree key sub_tree)
    >|= fun updated_tree -> ok (Proxy.Value updated_tree)
end

module type REQUESTS_TREE = sig
  type tree = Partial of tree StringMap.t | All

  val empty : tree

  val add : tree -> string list -> tree

  val find_opt : tree -> string list -> tree option
end

module RequestsTree : REQUESTS_TREE = struct
  type tree = Partial of tree StringMap.t | All

  let empty = Partial StringMap.empty

  let rec add (t : tree) (k : string list) : tree =
    match (t, k) with
    | (_, []) | (All, _) -> All
    | (Partial map, k_hd :: k_tail) -> (
        let sub_t_opt = StringMap.find_opt k_hd map in
        match sub_t_opt with
        | None -> Partial (StringMap.add k_hd (add empty k_tail) map)
        | Some (Partial _ as sub_t) ->
            Partial (StringMap.add k_hd (add sub_t k_tail) map)
        | Some All -> t)

  let rec find_opt (t : tree) (k : string list) : tree option =
    match (t, k) with
    | (All, _) -> Some All
    | (Partial _, []) -> None
    | (Partial map, k_hd :: k_tail) -> (
        let sub_t_opt = StringMap.find_opt k_hd map in
        match sub_t_opt with
        | None -> None
        | Some All -> Some All
        | Some (Partial _ as sub_t) -> (
            match k_tail with [] -> Some sub_t | _ -> find_opt sub_t k_tail))
end

module Core
    (T : Proxy.TREE with type key = Local.key and type t = Local.tree)
    (X : Proxy_proto.PROTO_RPC) : Proxy.CORE = struct
  let store = ref None

  (** Only load the store the first time it is needed *)
  let lazy_load_store () =
    match !store with
    | None ->
        let e = T.empty in
        store := Some e ;
        Lwt.return e
    | Some e -> Lwt.return e

  let get key = lazy_load_store () >>= fun store -> T.get store key

  let do_rpc : Proxy.proxy_getter_input -> Local.key -> unit tzresult Lwt.t =
   fun pgi key ->
    X.do_rpc pgi key >>=? fun tree ->
    lazy_load_store () >>= fun current_store ->
    (* Update cache with data obtained *)
    T.set_leaf current_store key tree >>=? fun updated ->
    (match updated with Mutation -> () | Value cache' -> store := Some cache') ;
    return_unit
end

module Make (C : Proxy.CORE) (X : Proxy_proto.PROTO_RPC) : M = struct
  let requests = ref RequestsTree.empty

  let pp_key k = String.concat ";" k

  let is_all k =
    match RequestsTree.find_opt !requests k with Some All -> true | _ -> false

  (** The kind of RPC request: is it a GET (i.e. is it loading data?) or is it only a MEMbership request (i.e. is the key associated to data?). *)
  type kind = Get | Mem

  (** Handles the application of [X.split_key] to optimize queries. *)
  let do_rpc (pgi : Proxy.proxy_getter_input) (kind : kind) (key : Local.key) :
      unit tzresult Lwt.t =
    let (key, split) =
      match kind with
      | Mem ->
          (* If the value is not going to be used, don't request a parent *)
          (key, false)
      | Get -> (
          match X.split_key key with
          | None ->
              (* There's no splitting for this key *)
              (key, false)
          | Some (prefix, _) ->
              (* Splitting triggers: a parent key will be requested *)
              (prefix, true))
    in
    (* [is_all] has been checked (by the caller: [generic_call])
       for the key received as parameter. Hence it only makes sense
       to check it if a parent key is being retrieved ('split' = true
       and hence 'key' here differs from the key received as parameter) *)
    if split && is_all key then return_unit
    else
      C.do_rpc pgi key >>=? fun () ->
      (* Remember request was done: map [key] to [All] in [!requests]
         (see [REQUESTS_TREE]'s mli for further details) *)
      requests := RequestsTree.add !requests key ;
      return_unit

  (* [generic_call] and [do_rpc] above go hand in hand. [do_rpc] takes
     care of performing the RPC call and updating [cache].
     [generic_call] calls [do_rpc] to make sure the cache is filled, and
     then queries the cache to return the desired value.
     Having them separate allows to avoid mixing the logic of
     [X.split_key] (confined to [do_rpc]) and the logic of getting
     the key's value. *)
  let generic_call :
      kind ->
      Proxy.proxy_getter_input ->
      Local.key ->
      Local.tree option tzresult Lwt.t =
   fun (kind : kind) (pgi : Proxy.proxy_getter_input) (key : Local.key) ->
    let pped_key = pp_key key in
    (if is_all key then
     (* This exact request was done already.
        So data was obtained already. Note that this does not imply
        that this function will return [Some] (maybe the node doesn't
        map this key). *)
     Events.(emit cache_hit pped_key) >>= fun () -> return_unit
    else
      (* This exact request was NOT done already (either a longer request
         was done or no related request was done at all).
         An RPC MUST be done. *)
      Events.(emit cache_miss pped_key) >>= fun () -> do_rpc pgi kind key)
    >>=? fun () -> C.get key >>= return

  let proxy_get pgi key = generic_call Get pgi key

  let proxy_dir_mem pgi key =
    generic_call Mem pgi key
    >|=? Option.fold ~none:false ~some:(fun tree ->
             Local.Tree.kind tree = `Tree)

  let proxy_mem pgi key =
    generic_call Mem pgi key >>=? fun tree_opt ->
    match tree_opt with
    | None -> return_false
    | Some tree -> (
        match Local.Tree.kind tree with
        | `Tree -> return_false
        | `Value -> return_true)
end

module MakeProxy (X : Proxy_proto.PROTO_RPC) : M = Make (Core (Tree) (X)) (X)

module Internal = struct
  module Tree = Tree
end
