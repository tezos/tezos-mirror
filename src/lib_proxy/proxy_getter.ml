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
  | Tezos_shell_services.Block_services.Key _ | Cut ->
      0
  | Dir map ->
      List.fold_left (fun sz (_, v) -> sz + 1 + raw_context_size v) 0 map

let rec raw_context_to_tree
    (raw : Tezos_shell_services.Block_services.raw_context) :
    Proxy_context.M.tree option =
  match raw with
  | Key (bytes : Bytes.t) ->
      Some (`Value bytes)
  | Cut ->
      None
  | Dir pairs ->
      let f string_map (string, raw_context) =
        let nested = raw_context_to_tree raw_context in
        match nested with
        | None ->
            string_map
        | Some u ->
            TzString.Map.add string u string_map
      in
      let dir = List.fold_left f TzString.Map.empty pairs in
      if TzString.Map.is_empty dir then None else Some (`Tree dir)

module type M = sig
  val proxy_dir_mem :
    Proxy.proxy_getter_input -> Proxy_context.M.key -> bool tzresult Lwt.t

  val proxy_get :
    Proxy.proxy_getter_input ->
    Proxy_context.M.key ->
    Proxy_context.M.tree option tzresult Lwt.t

  val proxy_mem :
    Proxy.proxy_getter_input -> Proxy_context.M.key -> bool tzresult Lwt.t
end

module StringMap = TzString.Map

module Tree :
  Proxy.TREE
    with type t = Proxy_context.M.tree
    with type key = Proxy_context.M.key = struct
  type t = Proxy_context.M.tree

  type key = Proxy_context.M.key

  let empty = Lwt.return Proxy_context.M.empty

  let rec get (m : t) k =
    match (k, m) with
    | ([], m) ->
        Some m
    | (n :: k, `Tree m) -> (
      match StringMap.find_opt n m with Some res -> get res k | None -> None )
    | (_ :: _, `Value _) ->
        None

  let get m k = Lwt.return @@ get m k

  let rec comb (k : StringMap.key list) (v : Proxy_context.M.tree) :
      Proxy_context.M.tree =
    match k with
    | [] ->
        v
    | k_hd :: k_tail ->
        `Tree (StringMap.singleton k_hd (comb k_tail v))

  let rec set_leaf (m : Proxy_context.M.tree) (k : StringMap.key list)
      (v : Proxy_context.M.tree) =
    match k with
    | [] ->
        v
    | k_hd :: k_tail -> (
      match m with
      | `Value _ ->
          assert false
      | `Tree map ->
          let k_m =
            match StringMap.find_opt k_hd map with
            | None ->
                comb k_tail v
            | Some k_hd_tree ->
                set_leaf k_hd_tree k_tail v
          in
          `Tree (StringMap.add k_hd k_m map) )

  let set_leaf m k raw_context =
    let m' =
      match raw_context_to_tree raw_context with
      | None ->
          m
      | Some v ->
          set_leaf m k v
    in
    return @@ Proxy.Value m'
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
    | (_, []) | (All, _) ->
        All
    | (Partial map, k_hd :: k_tail) -> (
        let sub_t_opt = StringMap.find_opt k_hd map in
        match sub_t_opt with
        | None ->
            Partial (StringMap.add k_hd (add empty k_tail) map)
        | Some (Partial _ as sub_t) ->
            Partial (StringMap.add k_hd (add sub_t k_tail) map)
        | Some All ->
            t )

  let rec find_opt (t : tree) (k : string list) : tree option =
    match (t, k) with
    | (All, _) ->
        Some All
    | (Partial _, []) ->
        None
    | (Partial map, k_hd :: k_tail) -> (
        let sub_t_opt = StringMap.find_opt k_hd map in
        match sub_t_opt with
        | None ->
            None
        | Some All ->
            Some All
        | Some (Partial _ as sub_t) -> (
          match k_tail with [] -> Some sub_t | _ -> find_opt sub_t k_tail ) )
end

module StringSet = TzString.Set

module Make'
    (T : Proxy.TREE with type key = string list)
    (X : Proxy_proto.PROTO_RPC) : M = struct
  let cache = ref None

  let requests = ref RequestsTree.empty

  let pp_key k = String.concat ";" k

  let is_all k =
    match RequestsTree.find_opt !requests k with
    | Some All ->
        true
    | _ ->
        false

  type kind = Get | Mem

  let get_cache () =
    match !cache with
    | None ->
        T.empty
        >>= fun e ->
        cache := Some e ;
        Lwt.return e
    | Some e ->
        Lwt.return e

  (** Handles the application of [X.split_key]. Performs
      the RPC call and updates [cache] accordingly. *)
  let do_rpc (pgi : Proxy.proxy_getter_input) (kind : kind)
      (key : Proxy_context.M.key) : unit tzresult Lwt.t =
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
            (prefix, true) )
    in
    (* is_all has been checked (by the caller: generic_call)
       for the key received as parameter. Hence it only makes sense
       to check it if a parent key is being retrieved ('split' = true
       and hence 'key' here differs from the key received as parameter) *)
    if split && is_all key then return_unit
    else
      X.do_rpc pgi key
      >>=? fun tree ->
      (* Remember request was done: map key to [All] in !requests
         (see [REQUESTS_TREE]'s mli for further details) *)
      requests := RequestsTree.add !requests key ;
      get_cache ()
      >>= fun c ->
      (* Update cache with data obtained *)
      T.set_leaf c key tree
      >>=? fun updated ->
      ( match updated with
      | Mutation ->
          ()
      | Value cache' ->
          cache := Some cache' ) ;
      return_unit

  (* [generic_call] and [do_rpc] above go hand in hand. do_rpc takes
     care of performing the RPC call and updating [cache].
     generic_call calls [do_rpc] to make sure the cache is filled, and
     then queries the cache to return the desired value.
     Having them separate allows to avoid mixing the logic of
     [X.split_key] (confined to do_rpc) and the logic of getting
     the key's value. *)

  let generic_call :
      kind ->
      Proxy.proxy_getter_input ->
      Proxy_context.M.key ->
      Proxy_context.M.tree option tzresult Lwt.t =
   fun (kind : kind)
       (pgi : Proxy.proxy_getter_input)
       (key : Proxy_context.M.key) ->
    let pped_key = pp_key key in
    ( if is_all key then
      (* This exact request was done already.
         So data was obtained already. Note that this does not imply
         that this function will return Some (maybe the node doesn't
         map this key). *)
      Events.(emit cache_hit pped_key) >>= fun () -> return_unit
    else
      (* This exact request was NOT done already (either a longer request
         was done or no related request was done at all).
         An RPC MUST be done. *)
      Events.(emit cache_miss pped_key) >>= fun () -> do_rpc pgi kind key )
    >>=? fun () -> get_cache () >>= fun c -> T.get c key >>= return

  let proxy_get = generic_call Get

  let proxy_dir_mem pgi key =
    generic_call Mem pgi key
    >>=? fun tree_opt ->
    match tree_opt with
    | None ->
        return_false
    | Some (`Value _) ->
        return_false
    | Some (`Tree _) ->
        return_true

  let proxy_mem pgi key =
    generic_call Mem pgi key
    >>=? fun tree_opt ->
    match tree_opt with
    | None ->
        return_false
    | Some (`Value _) ->
        return_true
    | Some (`Tree _) ->
        return_false
end

module Make (X : Proxy_proto.PROTO_RPC) : M = Make' (Tree) (X)
