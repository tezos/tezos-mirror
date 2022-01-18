(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxheadalpha.com>                    *)
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

open Protocol.Tx_rollup_l2_storage_sig

(* Build a Tezos context with binary trees *)
module Store = struct
  open Tezos_context_encoding.Context

  module Conf : Irmin_pack.Conf.S = struct
    let entries = 2

    let stable_hash = 2

    let inode_child_order = `Seeded_hash
  end

  (* We could directly use a simpler encoding for commits
     instead of keeping the same as in the current context. *)
  include
    Irmin_pack_mem.Make (Node) (Commit) (Conf) (Metadata) (Contents) (Path)
      (Branch)
      (Hash)
end

module Irmin_storage :
  STORAGE
    with type t = Store.tree
     and type 'a m = ('a, Environment.Error_monad.error) result Lwt.t = struct
  type t = Store.tree

  type 'a m = ('a, Environment.Error_monad.error) result Lwt.t

  let path k = [Bytes.to_string k]

  let get store key =
    let open Lwt_syntax in
    let* res = Store.Tree.find store (path key) in
    return_ok res

  let set store key value =
    let open Lwt_syntax in
    let* store = Store.Tree.add store (path key) value in
    return_ok store

  module Syntax = struct
    include Lwt_result_syntax

    let fail : Environment.Error_monad.error -> 'a m =
     fun e -> Lwt.return (Error e)

    let catch (m : 'a m) k h =
      Lwt.bind m (function Ok x -> k x | Error e -> h e)

    let list_fold_left_m = List.fold_left_es
  end
end

let empty_storage : Irmin_storage.t = Store.Tree.empty ()
