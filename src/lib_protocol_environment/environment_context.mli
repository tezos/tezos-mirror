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

module type CONTEXT = sig
  (** @inline *)
  include Environment_context_intf.S
end

module type VIEW = sig
  (** @inline *)
  include Environment_context_intf.VIEW
end

module type TREE = sig
  (** @inline *)
  include Environment_context_intf.TREE
end

module Equality_witness : sig
  type (_, _) eq = Refl : ('a, 'a) eq

  type 'a t

  val make : unit -> 'a t

  val eq : 'a t -> 'b t -> ('a, 'b) eq option

  val hash : 'a t -> int
end

module Context : sig
  type ('ctxt, 'tree) ops =
    (module CONTEXT with type t = 'ctxt and type tree = 'tree)

  type _ kind = private ..

  type ('a, 'b) equality_witness

  type t =
    | Context : {
        kind : 'a kind;
        impl_name : string;
        ctxt : 'a;
        ops : ('a, 'b) ops;
        equality_witness : ('a, 'b) equality_witness;
      }
        -> t

  include CONTEXT with type t := t
end

module Register (C : CONTEXT) : sig
  type _ Context.kind += Context : C.t Context.kind

  val equality_witness : (C.t, C.tree) Context.equality_witness

  val ops : (C.t, C.tree) Context.ops
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

val err_implementation_mismatch : expected:string -> got:string -> 'a
