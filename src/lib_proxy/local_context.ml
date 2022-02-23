(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018-2021 Tarides <contact@tarides.com>                     *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

include Tezos_context_memory.Context

let store_empty = empty

let shallow_of_tree repo tree =
  let h = Tree.hash tree in
  let hash =
    match Tree.kind tree with `Tree -> `Node h | `Value -> `Contents h
  in
  Tree.shallow repo hash

module Tree = struct
  include Tree

  (* Since irmin.2.8.0, [find_tree] raises [Dangling_hash] exceptions when
     called on shallow node.  The proxy starts with an empty in-memory store and
     does not have access to the on disk store. It calls distant nodes to resolve
     its shallow trees. So it is expected that it will call `find_tree` on shallow
     trees which will raise the expection. We catch that exception and return
     `None` to signal to the proxy that a shallow tree is not in its store. *)
  let find_tree t k =
    Lwt.catch
      (fun () -> find_tree t k)
      (function Context_dangling_hash _ -> Lwt.return_none | exn -> raise exn)

  let remove_dangling_hash t =
    let open Lwt_syntax in
    let kind = kind t in
    let* t = add t [] Bytes.empty in
    match kind with
    | `Value -> Lwt.return t
    | `Tree -> add_tree t [] (empty store_empty)

  (* Similarly [add_tree t] raises [Dangling_hash] exceptions when [t] is a
     shallow node or contents. When this occurs we replace [t] with the empty
     node or contents. *)
  let add_tree t k v =
    let open Lwt_syntax in
    Lwt.catch
      (fun () -> add_tree t k v)
      (function
        | Context_dangling_hash _ ->
            let* t = remove_dangling_hash t in
            add_tree t k v
        | exn -> raise exn)
end
