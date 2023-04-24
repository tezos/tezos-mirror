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
module Proof = Tezos_context_sigs.Context.Proof_types

(** The kind of RPC request: is it a GET (i.e. is it loading data?) or
    is it only a MEMbership request (i.e. is the key associated to data?). *)
type kind = Get | Mem

let kind_encoding : kind Data_encoding.t =
  let open Data_encoding in
  conv
    (function Get -> true | Mem -> false)
    (function true -> Get | false -> Mem)
    bool

let pp_kind fmt kind =
  Format.fprintf fmt "%s" (match kind with Get -> "get" | Mem -> "mem")

module Events = struct
  include Internal_event.Simple

  let section = ["proxy_getter"]

  let pp_key =
    let pp_sep fmt () = Format.fprintf fmt "/" in
    Format.pp_print_list ~pp_sep Format.pp_print_string

  let cache_hit =
    declare_2
      ~section
      ~name:"cache_hit"
      ~msg:"Cache hit ({kind}): ({key})"
      ~level:Debug
      ~pp1:pp_kind
      ~pp2:pp_key
      ("kind", kind_encoding)
      ("key", Data_encoding.(list string))

  let cache_miss =
    declare_2
      ~section
      ~name:"cache_miss"
      ~msg:"Cache miss ({kind}): ({key})"
      ~level:Debug
      ~pp1:pp_kind
      ~pp2:pp_key
      ("kind", kind_encoding)
      ("key", Data_encoding.(list string))

  let split_key_triggers =
    declare_2
      ~section
      ~level:Debug
      ~name:"split_key_triggers"
      ~msg:"split_key heuristic triggers, getting {parent} instead of {leaf}"
      ~pp1:pp_key
      ~pp2:pp_key
      ("parent", Data_encoding.(list string))
      ("leaf", Data_encoding.(list string))
end

let rec raw_context_size = function
  | Proof.Key _ | Cut -> 0
  | Dir map ->
      String.Map.fold (fun _key v acc -> acc + 1 + raw_context_size v) map 0

let rec raw_context_to_tree (raw : Proof.raw_context) : Local.tree option Lwt.t
    =
  match raw with
  | Key (bytes : Bytes.t) ->
      Lwt.return (Some (Local.Tree.of_raw (`Value bytes)))
  | Cut -> Lwt.return None
  | Dir map ->
      let open Lwt_syntax in
      let add_to_tree tree (string, raw_context) =
        let* u = raw_context_to_tree raw_context in
        match u with
        | None -> Lwt.return tree
        | Some u -> Local.Tree.add_tree tree [string] u
      in
      let* dir =
        String.Map.bindings map
        |> List.fold_left_s
             add_to_tree
             (Tezos_context_memory.Context.make_empty_tree ())
      in
      if Local.Tree.is_empty dir then return_none else return_some dir

module type M = sig
  val proxy_dir_mem :
    Proxy.proxy_getter_input -> Local.key -> bool tzresult Lwt.t

  val proxy_get :
    Proxy.proxy_getter_input -> Local.key -> Local.tree option tzresult Lwt.t

  val proxy_mem : Proxy.proxy_getter_input -> Local.key -> bool tzresult Lwt.t
end

type proxy_m = (module M)

type proxy_builder =
  | Of_rpc of (Proxy_proto.proto_rpc -> proxy_m Lwt.t)
  | Of_data_dir of
      (Context_hash.t ->
      Tezos_protocol_environment.Proxy_delegate.t tzresult Lwt.t)

type rpc_context_args = {
  printer : Tezos_client_base.Client_context.printer option;
  proxy_builder : proxy_builder;
  rpc_context : Tezos_rpc.Context.generic;
  mode : Proxy.mode;
  chain : Tezos_shell_services.Block_services.chain;
  block : Tezos_shell_services.Block_services.block;
}

module StringMap = String.Map

let make_delegate (ctx : rpc_context_args)
    (proto_rpc : (module Proxy_proto.PROTO_RPC)) (hash : Context_hash.t) :
    Tezos_protocol_environment.Proxy_delegate.t tzresult Lwt.t =
  match ctx.proxy_builder with
  | Of_rpc f ->
      let open Lwt_result_syntax in
      let*! (module Initial_context) = f proto_rpc in
      let pgi : Proxy.proxy_getter_input =
        {
          rpc_context = (ctx.rpc_context :> Tezos_rpc.Context.simple);
          mode = ctx.mode;
          chain = ctx.chain;
          block = ctx.block;
        }
      in
      return
        (module struct
          let proxy_dir_mem = Initial_context.proxy_dir_mem pgi

          let proxy_get = Initial_context.proxy_get pgi

          let proxy_mem = Initial_context.proxy_mem pgi
        end : Tezos_protocol_environment.Proxy_delegate.T)
  | Of_data_dir f -> f hash

module Tree : Proxy.TREE with type t = Local.tree with type key = Local.key =
struct
  type t = Local.tree

  type key = Local.key

  let empty =
    Local.Tree.empty (Tezos_context_memory.Context.make_empty_context ())

  let get = Local.Tree.find_tree

  let add_leaf tree key raw_context : t Proxy.update Lwt.t =
    let open Lwt_syntax in
    let* tree_opt = raw_context_to_tree raw_context in
    let* updated_tree =
      match tree_opt with
      | None -> Lwt.return tree
      | Some sub_tree -> Local.Tree.add_tree tree key sub_tree
    in
    Lwt.return (Proxy.Value updated_tree)
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
    | _, [] | All, _ -> All
    | Partial map, k_hd :: k_tail -> (
        let sub_t_opt = StringMap.find_opt k_hd map in
        match sub_t_opt with
        | None -> Partial (StringMap.add k_hd (add empty k_tail) map)
        | Some (Partial _ as sub_t) ->
            Partial (StringMap.add k_hd (add sub_t k_tail) map)
        | Some All -> t)

  let rec find_opt (t : tree) (k : string list) : tree option =
    match (t, k) with
    | All, _ -> Some All
    | Partial _, [] -> None
    | Partial map, k_hd :: k_tail -> (
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

  let get key =
    let open Lwt_syntax in
    let* store = lazy_load_store () in
    T.get store key

  let do_rpc : Proxy.proxy_getter_input -> Local.key -> unit tzresult Lwt.t =
   fun pgi key ->
    let open Lwt_result_syntax in
    let* tree = X.do_rpc pgi key in
    let*! current_store = lazy_load_store () in
    (* Update cache with data obtained *)
    let*! updated = T.add_leaf current_store key tree in
    (match updated with Mutation -> () | Value cache' -> store := Some cache') ;
    return_unit
end

module Make (C : Proxy.CORE) (X : Proxy_proto.PROTO_RPC) : M = struct
  let requests = ref RequestsTree.empty

  let is_all k =
    match RequestsTree.find_opt !requests k with Some All -> true | _ -> false

  (** Handles the application of [X.split_key] to optimize queries. *)
  let do_rpc (pgi : Proxy.proxy_getter_input) (kind : kind)
      (requested_key : Local.key) : unit tzresult Lwt.t =
    let open Lwt_result_syntax in
    let key_to_get, split =
      match kind with
      | Mem ->
          (* If the value is not going to be used, don't request a parent *)
          (requested_key, false)
      | Get -> (
          match X.split_key pgi.mode requested_key with
          | None ->
              (* There's no splitting for this key *)
              (requested_key, false)
          | Some (prefix, _) ->
              (* Splitting triggers: a parent key will be requested *)
              (prefix, true))
    in
    let remember_request () =
      (* Remember request was done: map [key] to [All] in [!requests]
         (see [Proxy_getter.REQUESTS_TREE] mli for further details) *)
      requests := RequestsTree.add !requests key_to_get ;
      return_unit
    in
    (* [is_all] has been checked (by the caller: [generic_call])
       for the key received as parameter. Hence it only makes sense
       to check it if a parent key is being retrieved ('split' = true
       and hence 'key' here differs from the key received as parameter) *)
    if split && is_all key_to_get then return_unit
    else
      let*! () =
        if split then
          Events.(emit split_key_triggers (key_to_get, requested_key))
        else Lwt.return_unit
      in
      let*! r = C.do_rpc pgi key_to_get in
      match r with
      | Ok _ -> remember_request ()
      | Error _ when X.failure_is_permanent requested_key -> remember_request ()
      | Error err ->
          (* Don't remember the request, maybe it will succeed in the future *)
          Lwt.return_error err

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
    let open Lwt_result_syntax in
    let* () =
      if is_all key then
        (* This exact request was done already.
           So data was obtained already. Note that this does not imply
           that this function will return [Some] (maybe the node doesn't
           map this key). *)
        Lwt_result.ok @@ Events.(emit cache_hit (kind, key))
      else
        (* This exact request was NOT done already (either a longer request
           was done or no related request was done at all).
           An RPC MUST be done. *)
        let*! () = Events.(emit cache_miss (kind, key)) in
        do_rpc pgi kind key
    in
    Lwt_result.ok @@ C.get key

  let proxy_get pgi key = generic_call Get pgi key

  let proxy_dir_mem pgi key =
    let open Lwt_result_syntax in
    let* tree_opt = generic_call Mem pgi key in
    match tree_opt with
    | None -> return_false
    | Some tree -> (
        match Local.Tree.kind tree with
        | `Tree -> return_true
        | `Value -> return_false)

  let proxy_mem pgi key =
    let open Lwt_result_syntax in
    let* tree_opt = generic_call Mem pgi key in
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

  let raw_context_to_tree = raw_context_to_tree
end
