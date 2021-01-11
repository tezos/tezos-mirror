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

module M = struct
  include Tezos_storage_memory.Context

  let set = add

  let get = find

  let dir_mem = mem_tree

  let remove_rec = remove

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

  let set_protocol = add_protocol

  let fork_test_chain c ~protocol:_ ~expiration:_ = Lwt.return c
end

open Tezos_protocol_environment

type t = M.t

type _ Context.kind += Memory : t Context.kind

let ops = (module M : CONTEXT with type t = 'ctxt)

let empty =
  let ctxt = M.empty in
  Context.Context {ops; ctxt; kind = Memory}

let project : Context.t -> t =
 fun (Context.Context {ctxt; kind; _} : Context.t) ->
  match kind with Memory -> ctxt | _ -> assert false

let inject : t -> Context.t =
 fun ctxt -> Context.Context {ops; ctxt; kind = Memory}

let encoding : Context.t Data_encoding.t =
  let open Data_encoding in
  conv project inject M.encoding
