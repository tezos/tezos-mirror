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

open Error_monad

module type CONTEXT = sig
  type t

  type key = string list

  type value = MBytes.t

  val mem : t -> key -> bool Lwt.t

  val dir_mem : t -> key -> bool Lwt.t

  val get : t -> key -> value option Lwt.t

  val set : t -> key -> value -> t Lwt.t

  val copy : t -> from:key -> to_:key -> t option Lwt.t

  val del : t -> key -> t Lwt.t

  val remove_rec : t -> key -> t Lwt.t

  val fold :
    t ->
    key ->
    init:'a ->
    f:([`Key of key | `Dir of key] -> 'a -> 'a Lwt.t) ->
    'a Lwt.t

  val set_protocol : t -> Protocol_hash.t -> t Lwt.t

  val fork_test_chain :
    t -> protocol:Protocol_hash.t -> expiration:Time.Protocol.t -> t Lwt.t
end

module Context = struct
  type key = string list

  type value = MBytes.t

  type 'ctxt ops = (module CONTEXT with type t = 'ctxt)

  type _ kind = ..

  type t = Context : {kind : 'a kind; ctxt : 'a; ops : 'a ops} -> t

  let mem (Context {ops = (module Ops); ctxt; _}) key = Ops.mem ctxt key

  let set (Context {ops = (module Ops) as ops; ctxt; kind}) key value =
    Ops.set ctxt key value
    >>= fun ctxt -> Lwt.return (Context {ops; ctxt; kind})

  let dir_mem (Context {ops = (module Ops); ctxt; _}) key =
    Ops.dir_mem ctxt key

  let get (Context {ops = (module Ops); ctxt; _}) key = Ops.get ctxt key

  let copy (Context {ops = (module Ops) as ops; ctxt; kind}) ~from ~to_ =
    Ops.copy ctxt ~from ~to_
    >>= function
    | Some ctxt ->
        Lwt.return_some (Context {ops; ctxt; kind})
    | None ->
        Lwt.return_none

  let del (Context {ops = (module Ops) as ops; ctxt; kind}) key =
    Ops.del ctxt key >>= fun ctxt -> Lwt.return (Context {ops; ctxt; kind})

  let remove_rec (Context {ops = (module Ops) as ops; ctxt; kind}) key =
    Ops.remove_rec ctxt key
    >>= fun ctxt -> Lwt.return (Context {ops; ctxt; kind})

  let fold (Context {ops = (module Ops); ctxt; _}) key ~init ~f =
    Ops.fold ctxt key ~init ~f

  let set_protocol (Context {ops = (module Ops) as ops; ctxt; kind})
      protocol_hash =
    Ops.set_protocol ctxt protocol_hash
    >>= fun ctxt -> Lwt.return (Context {ops; ctxt; kind})

  let fork_test_chain (Context {ops = (module Ops) as ops; ctxt; kind})
      ~protocol ~expiration =
    Ops.fork_test_chain ctxt ~protocol ~expiration
    >>= fun ctxt -> Lwt.return (Context {ops; ctxt; kind})
end

type validation_result = {
  context : Context.t;
  fitness : Fitness.t;
  message : string option;
  max_operations_ttl : int;
  last_allowed_fork_level : Int32.t;
}

type quota = {max_size : int; max_op : int option}

type rpc_context = {
  block_hash : Block_hash.t;
  block_header : Block_header.shell_header;
  context : Context.t;
}
