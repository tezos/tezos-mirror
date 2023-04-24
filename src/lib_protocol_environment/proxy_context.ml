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

module Local = Tezos_context_memory.Context

module M = struct
  type key = Local.key

  type value = Local.value

  type tree = Local.tree

  (* When the [proxy] option is [None], this instance of [M] should
     behave like [Memory_context]. *)
  type t = {proxy : Proxy_delegate.t option; local : Local.t}

  let empty = Tezos_context_memory.Context.make_empty_tree ()
end

module C = struct
  type key = M.key

  type value = M.value

  type t = M.t

  (* [tree] is the tree available under [/data/<path>]. *)
  type tree = {proxy : Proxy_delegate.t option; path : key; tree : Local.tree}

  (** Generic pretty printing functions *)
  let pp_key ppf key =
    Format.pp_print_list
      ~pp_sep:(fun ppf _ -> Format.fprintf ppf "; ")
      Format.pp_print_string
      ppf
      key

  module L = struct
    module S = Internal_event.Simple

    let section = ["proxy"; "context"]

    let proxy_context_missing =
      S.declare_1
        ~section
        ~name:"proxy_context_missing"
        ~msg:"delegating to proxy cache, because data misses for: {key}"
        ~pp1:pp_key
        ("key", Data_encoding.(Variable.list string))

    let delegation_error =
      S.declare_2
        ~section
        ~name:"delegation_error"
        ~msg:
          "{function} returned an error, ignoring it but this is bad: {trace}"
        ~pp2:pp_print_trace
        ("function", Data_encoding.string)
        ("trace", Error_monad.trace_encoding)
  end

  type elt = Key of value | Dir of Local.tree

  let elt t =
    let open Lwt_syntax in
    let+ o = Local.Tree.to_value t in
    match o with Some v -> Key v | None -> Dir t

  let raw_find (t : tree) k =
    let open Lwt_syntax in
    let* o = Local.Tree.find_tree t.tree k in
    match o with
    | Some _ -> Lwt.return o
    | None -> (
        let* () = L.(S.emit proxy_context_missing) k in
        match t.proxy with
        | None -> Lwt.return_none
        | Some (module ProxyDelegation) -> (
            let* r = ProxyDelegation.proxy_get (t.path @ k) in
            match r with
            | Error err ->
                let* () = L.(S.emit delegation_error ("get", err)) in
                Lwt.return_none
            | Ok x -> Lwt.return x))

  let raw_mem_aux kind (t : tree) k =
    let open Lwt_syntax in
    let* o = Local.Tree.find_tree t.tree k in
    let o = Option.map Local.Tree.kind o in
    match o with
    | Some `Value -> Lwt.return (kind = `Value)
    | Some `Tree -> Lwt.return (kind = `Tree)
    | None -> (
        match t.proxy with
        | None -> Lwt.return_false
        | Some (module ProxyDelegation) -> (
            let mem =
              match kind with
              | `Value -> ProxyDelegation.proxy_mem
              | `Tree -> ProxyDelegation.proxy_dir_mem
            in
            let* r = mem (t.path @ k) in
            match r with
            | Error err ->
                let msg =
                  match kind with `Value -> "mem" | `Tree -> "dir_mem"
                in
                let* () = L.(S.emit delegation_error (msg, err)) in
                Lwt.return_false
            | Ok x -> Lwt.return x))

  let raw_mem = raw_mem_aux `Value

  let raw_mem_tree = raw_mem_aux `Tree

  (* The tree under /data *)
  let data_tree (t : t) =
    let open Lwt_syntax in
    let+ o = Local.find_tree t.local [] in
    match o with
    | None -> {proxy = t.proxy; path = []; tree = Local.Tree.empty t.local}
    | Some tree -> {proxy = t.proxy; path = []; tree}

  let mem t k =
    let open Lwt_syntax in
    let* tree = data_tree t in
    raw_mem tree k

  let mem_tree t k =
    let open Lwt_syntax in
    let* tree = data_tree t in
    raw_mem_tree tree k

  let find t k =
    let open Lwt_syntax in
    let* tree = data_tree t in
    let* o = raw_find tree k in
    match o with
    | None -> Lwt.return_none
    | Some v -> (
        let+ k = elt v in
        match k with Key v -> Some v | _ -> None)

  let find_tree t k =
    let open Lwt_syntax in
    let* tree = data_tree t in
    let+ o = raw_find tree k in
    Option.map (fun tree -> {proxy = t.proxy; path = k; tree}) o

  let add_tree (t : t) k (v : tree) =
    let open Lwt_syntax in
    let+ local = Local.add_tree t.local k v.tree in
    if t.local == local then t else {t with local}

  let add (t : t) k v =
    let open Lwt_syntax in
    let+ local = Local.add t.local k v in
    if t.local == local then t else {t with local}

  let remove (t : t) k =
    let open Lwt_syntax in
    let+ local = Local.remove t.local k in
    if t.local == local then t else {t with local}

  let raw_list (t : tree) ?offset ?length k =
    let open Lwt_syntax in
    let+ ls = Local.Tree.list t.tree ?offset ?length k in
    List.fold_left
      (fun acc (k, tree) ->
        let v = {proxy = t.proxy; path = t.path @ [k]; tree} in
        (k, v) :: acc)
      []
      (List.rev ls)

  let list t ?offset ?length k =
    let open Lwt_syntax in
    let local_raw_list () =
      let* tree = data_tree t in
      raw_list tree ?offset ?length k
    in
    match t.proxy with
    | None -> local_raw_list ()
    | Some (module ProxyDelegation) -> (
        let* tree = ProxyDelegation.proxy_get k in
        match tree with
        | Ok v -> (
            match v with
            | Some tree ->
                (* [tree] is the value at [k], so we need to pass [] as the key
                   in the call to [raw_list]: *)
                raw_list {proxy = None; path = k; tree} ?offset ?length []
            | None -> return [])
        | Error err ->
            (* We are in trouble here. The delegate failed; but we can't
               forward the error to the caller, because this function is
               [Lwt.t], but not in [tzresult Lwt.t]. To keep track of the error,
               we log it and are left with deciding what to return. We could
               list on the local tree ([local_raw_list]) but it doesn't make
               much sense, because in production this tree is almost
               completely empty. That is why we return the default value, i.e. the
               empty list. It's not a perfect choice, but we prefer that than failing. *)
            let+ () = L.(S.emit delegation_error ("get", err)) in
            [])

  let length t k =
    let open Lwt_syntax in
    let local_raw_length () =
      let* t = data_tree t in
      Local.Tree.length t.tree k
    in
    match t.proxy with
    | None -> local_raw_length ()
    | Some (module ProxyDelegation) -> (
        let* tree = ProxyDelegation.proxy_get k in
        match tree with
        | Ok v -> (
            match v with
            | Some tree ->
                (* [tree] is the value at [k], so we need to pass [] as the key
                   in the call to [length]: *)
                Local.Tree.length tree []
            | None -> local_raw_length ())
        | Error err ->
            (* We are in trouble here. The delegate failed; but we can't
               forward the error to the caller, because this function is
               [Lwt.t], but not in [tzresult Lwt.t]. To keep track of the error,
               we log it and are left with deciding what to return. We could call
               [length] on the local tree ([local_raw_length]) but it doesn't make
               much sense, because in production this tree is almost
               completely empty. That is why we return the default value, i.e. zero.
               It's not a perfect choice, but we prefer that than failing. *)
            let+ () = L.(S.emit delegation_error ("get", err)) in
            0)

  let fold ?depth (t : t) root ~order ~init ~f =
    let open Lwt_syntax in
    (* Fold over the tree mapped by [root] *)
    let fold_root_tree (tree : M.tree) =
      Local.Tree.fold ?depth tree [] ~order ~init ~f:(fun k tree acc ->
          let tree = {proxy = t.proxy; path = root @ k; tree} in
          f k tree acc)
    in
    let local_raw_fold () =
      let* tr = find_tree t root in
      match tr with
      | None -> Lwt.return init
      | Some tr -> fold_root_tree tr.tree
    in
    match t.proxy with
    | None -> local_raw_fold ()
    | Some (module ProxyDelegation) -> (
        let* tree = ProxyDelegation.proxy_get root in
        match tree with
        | Ok None -> Lwt.return init
        | Ok (Some v) -> fold_root_tree v
        | Error err ->
            (* We are in trouble here. The delegate failed; but we can't
               forward the error to the caller, because this function is
               [Lwt.t], but not in [tzresult Lwt.t]. To keep track of the error,
               we log it and are left with deciding what to return. We could
               fold on the local tree ([local_raw_fold]) but it doesn't make
               much sense, because in production this tree is almost
               completely empty. That is why we return the default value.
               It's not a perfect choice, but we prefer that than failing. *)
            let+ () = L.(S.emit delegation_error ("get", err)) in
            init)

  let set_protocol (t : t) p =
    let open Lwt_syntax in
    let+ local = Local.add_protocol t.local p in
    {t with local}

  let get_protocol (t : t) = Local.get_protocol t.local

  let fork_test_chain c ~protocol:_ ~expiration:_ = Lwt.return c

  let set_hash_version (t : t) v =
    let open Lwt_result_syntax in
    let+ local = Local.set_hash_version t.local v in
    {t with local}

  let get_hash_version (t : t) = Local.get_hash_version t.local

  module Tree = struct
    let pp ppf t = Local.Tree.pp ppf t.tree

    let empty t = {proxy = None; path = []; tree = Local.Tree.empty t.M.local}

    let equal x y = Local.Tree.equal x.tree y.tree

    let hash x = Local.Tree.hash x.tree

    let is_empty t = Local.Tree.is_empty t.tree

    let add t k v =
      let open Lwt_syntax in
      let+ tree = Local.Tree.add t.tree k v in
      if tree == t.tree then t else {t with tree}

    let add_tree t k v =
      let open Lwt_syntax in
      let+ tree = Local.Tree.add_tree t.tree k v.tree in
      if tree == t.tree then t else {t with tree}

    let mem = raw_mem

    let mem_tree = raw_mem_tree

    let find t k =
      let open Lwt_syntax in
      let* o = raw_find t k in
      match o with
      | None -> Lwt.return_none
      | Some tree -> Local.Tree.to_value tree

    let find_tree t k =
      let open Lwt_syntax in
      let+ o = raw_find t k in
      match o with
      | None -> None
      | Some tree ->
          if k = [] then Some t
          else Some {proxy = t.proxy; path = t.path @ k; tree}

    let remove t k =
      let open Lwt_syntax in
      let+ tree = Local.Tree.remove t.tree k in
      if tree == t.tree then t else {t with tree}

    let length t k = Local.Tree.length t.tree k

    let fold ?depth (t : tree) k ~order ~init ~f =
      Local.Tree.fold ?depth t.tree k ~order ~init ~f:(fun k tree acc ->
          let tree = {proxy = t.proxy; path = t.path @ k; tree} in
          f k tree acc)

    let kind t = Local.Tree.kind t.tree

    let to_value t = Local.Tree.to_value t.tree

    let of_value t v =
      let open Lwt_syntax in
      let+ tree = Local.Tree.of_value t.M.local v in
      {proxy = t.proxy; path = []; tree}

    let list = raw_list

    let clear ?depth t = Local.Tree.clear ?depth t.tree

    let config t = Local.Tree.config t.tree
  end

  module Proof = Local.Proof

  let of_local tree = {proxy = None; path = []; tree}

  let map_f f tree =
    let open Lwt_syntax in
    let+ t, r = f (of_local tree) in
    (t.tree, r)

  let verify verifier proof f =
    let open Lwt_syntax in
    let+ r = verifier proof (map_f f) in
    match r with Ok (t, r) -> Ok (of_local t, r) | Error _ as e -> e

  let verify_tree_proof p f = verify Local.verify_tree_proof p f

  let verify_stream_proof p f = verify Local.verify_stream_proof p f

  let config t = Local.config t.M.local

  let equal_config = Local.equal_config
end

open Environment_context
include Environment_context.Register (C)

let proxy_impl_name = "proxy"

let empty proxy =
  let local = Tezos_context_memory.Context.make_empty_context () in
  let ctxt = M.{proxy; local} in
  Context.make
    ~ops
    ~ctxt
    ~kind:Context
    ~equality_witness
    ~impl_name:proxy_impl_name
