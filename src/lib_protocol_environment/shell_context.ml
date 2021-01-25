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

open Tezos_protocol_environment
open Context

let ( >>= ) = Lwt.( >>= )

type _ Context.kind += Shell : Tezos_storage.Context.t Context.kind

module C = struct
  include Tezos_storage.Context

  let set = add

  let get = find

  let dir_mem = mem_tree

  let remove_rec = remove

  let set_protocol = add_protocol

  let copy ctxt ~from ~to_ =
    find_tree ctxt from
    >>= function
    | None ->
        Lwt.return_none
    | Some sub_tree ->
        add_tree ctxt to_ sub_tree >>= Lwt.return_some

  type key_or_dir = [`Key of key | `Dir of key]

  let fold t root ~init ~f =
    fold ~depth:(`Eq 1) t root ~init ~f:(fun k t acc ->
        let k = root @ k in
        match Tree.kind t with
        | `Value _ ->
            f (`Key k) acc
        | `Tree ->
            f (`Dir k) acc)
end

let ops = (module C : CONTEXT with type t = 'ctxt)

let checkout index context_hash =
  Tezos_storage.Context.checkout index context_hash
  >>= function
  | Some ctxt ->
      Lwt.return_some (Context.Context {ops; ctxt; kind = Shell})
  | None ->
      Lwt.return_none

let checkout_exn index context_hash =
  Tezos_storage.Context.checkout_exn index context_hash
  >>= fun ctxt -> Lwt.return (Context.Context {ops; ctxt; kind = Shell})

let wrap_disk_context ctxt = Context.Context {ops; ctxt; kind = Shell}

let unwrap_disk_context : t -> Tezos_storage.Context.t = function
  | Context.Context {ctxt; kind = Shell; _} ->
      ctxt
  | _ ->
      assert false
