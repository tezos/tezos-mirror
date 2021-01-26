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

module Local = Tezos_storage_memory.Context

module M = struct
  type key = Local.key

  type value = Local.value

  type tree = [`Value of bytes | `Tree of tree TzString.Map.t]

  module type ProxyDelegate = sig
    val proxy_dir_mem : key -> bool tzresult Lwt.t

    val proxy_get : key -> tree option tzresult Lwt.t

    val proxy_mem : key -> bool tzresult Lwt.t
  end

  type proxy_delegate = (module ProxyDelegate)

  (* When the [proxy] option is [None], this instance of [M] should
     behave like [Memory_context]. *)
  type t = {proxy : proxy_delegate option; local : Local.t}

  let rec tree_size_aux acc = function
    | `Value _ ->
        acc + 1
    | `Tree t ->
        TzString.Map.fold (fun _ t acc -> tree_size_aux (acc + 1) t) t acc

  let tree_size = tree_size_aux 0

  let empty = `Tree TzString.Map.empty
end

module C = struct
  type key = M.key

  type value = M.value

  type t = M.t

  (* [root] is the root of the current subtree; [path] is the path
     from the underlying local store. *)
  type tree = {root : M.t; path : key}

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
        ~pp2:pp_print_error
        ("function", Data_encoding.string)
        ("trace", Error_monad.trace_encoding)
  end

  type elt = Key of value | Dir of Local.tree

  let elt t = match Local.Tree.kind t with `Value v -> Key v | `Tree -> Dir t

  let raw_find (t : tree) k =
    Local.find_tree t.root.local k
    >>= function
    | Some x ->
        Lwt.return_some x
    | None -> (
        L.(S.emit proxy_context_missing) k
        >>= fun () ->
        match t.root.proxy with
        | None ->
            Lwt.return_none
        | Some (module ProxyDelegation) -> (
            ProxyDelegation.proxy_get (t.path @ k)
            >>= function
            | Error err ->
                L.(S.emit delegation_error ("get", err))
                >>= fun () -> Lwt.return_none
            | Ok x ->
                Lwt.return (Option.map Local.Tree.of_raw x) ) )

  let raw_mem_aux kind (t : tree) k =
    Local.find_tree t.root.local k
    >|= Option.map Local.Tree.kind
    >>= function
    | Some (`Value _) ->
        Lwt.return (kind = `Value)
    | Some `Tree ->
        Lwt.return (kind = `Tree)
    | None -> (
      match t.root.proxy with
      | None ->
          Lwt.return_false
      | Some (module ProxyDelegation) -> (
          let mem =
            match kind with
            | `Value ->
                ProxyDelegation.proxy_mem
            | `Tree ->
                ProxyDelegation.proxy_dir_mem
          in
          mem (t.path @ k)
          >>= function
          | Error err ->
              let msg =
                match kind with `Value -> "mem" | `Tree -> "dir_mem"
              in
              L.(S.emit delegation_error (msg, err))
              >>= fun () -> Lwt.return_false
          | Ok x ->
              Lwt.return x ) )

  let raw_mem = raw_mem_aux `Value

  let raw_mem_tree = raw_mem_aux `Tree

  let root t = {root = t; path = []}

  let mem t k = raw_mem (root t) k

  let mem_tree t k = raw_mem_tree (root t) k

  let find t k =
    raw_find (root t) k
    >|= Option.map elt
    >|= function Some (Key v) -> Some v | _ -> None

  let find_tree t k = raw_find (root t) k

  let add_tree (t : t) k v =
    Local.add_tree t.local k v
    >|= fun local -> if t.local == local then t else {t with local}

  let add (t : t) k v =
    Local.add t.local k v
    >|= fun local -> if t.local == local then t else {t with local}

  let remove (t : t) k =
    Local.remove t.local k
    >|= fun local -> if t.local == local then t else {t with local}

  let copy ctxt ~from ~to_ =
    find_tree ctxt from
    >>= function
    | None ->
        Lwt.return_none
    | Some sub_tree ->
        add_tree ctxt to_ sub_tree >>= Lwt.return_some

  type key_or_dir = [`Key of key | `Dir of key]

  let fold ctxt root ~init ~f =
    find_tree ctxt root
    >>= function
    | None ->
        Lwt.return init
    | Some t ->
        Local.Tree.fold ~depth:(`Eq 1) t [] ~init ~f:(fun k t acc ->
            let k = root @ k in
            match Local.Tree.kind t with
            | `Value _ ->
                f (`Key k) acc
            | `Tree ->
                f (`Dir k) acc)

  let set_protocol (t : t) p =
    Local.add_protocol t.local p >|= fun local -> {t with local}

  let get_protocol (t : t) = Local.get_protocol t.local

  let fork_test_chain c ~protocol:_ ~expiration:_ = Lwt.return c
end

open Tezos_protocol_environment

type _ Context.kind += Proxy : M.t Context.kind

let ops = (module C : CONTEXT with type t = 'ctxt)

let empty proxy =
  let ctxt = M.{proxy; local = Local.empty} in
  Context.Context {ops; ctxt; kind = Proxy}

let set_delegate : M.proxy_delegate -> Context.t -> Context.t =
 fun proxy (Context.Context t) ->
  match t.kind with
  | Proxy ->
      let ctxt = {t.ctxt with proxy = Some proxy} in
      Context.Context {t with ctxt}
  | _ ->
      assert false
