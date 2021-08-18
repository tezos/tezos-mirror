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

let err_implementation_mismatch ~expected ~got =
  Fmt.invalid_arg
    "Context implementation mismatch: expecting %s, got %s"
    expected
    got

module type CONTEXT = Environment_context_intf.S

module type VIEW = Environment_context_intf.VIEW

module type TREE = Environment_context_intf.TREE

module Equality_witness : sig
  type (_, _) eq = Refl : ('a, 'a) eq

  type 'a t

  val make : unit -> 'a t

  val eq : 'a t -> 'b t -> ('a, 'b) eq option

  val hash : 'a t -> int
end = struct
  type (_, _) eq = Refl : ('a, 'a) eq

  type _ equality = ..

  module type Inst = sig
    type t

    type _ equality += Eq : t equality
  end

  type 'a t = (module Inst with type t = 'a)

  let make : type a. unit -> a t =
   fun () ->
    let module Inst = struct
      type t = a

      type _ equality += Eq : t equality
    end in
    (module Inst)

  let eq : type a b. a t -> b t -> (a, b) eq option =
   fun (module A) (module B) -> match A.Eq with B.Eq -> Some Refl | _ -> None

  let hash : type a. a t -> int = fun (module A) -> Hashtbl.hash A.Eq
end

module Context = struct
  type key = string list

  type value = Bytes.t

  type ('ctxt, 'tree) ops =
    (module CONTEXT with type t = 'ctxt and type tree = 'tree)

  type _ kind = ..

  type ('a, 'b) equality_witness = 'a Equality_witness.t * 'b Equality_witness.t

  let equality_witness () = (Equality_witness.make (), Equality_witness.make ())

  let equiv (a, b) (c, d) = (Equality_witness.eq a c, Equality_witness.eq b d)

  type t =
    | Context : {
        kind : 'a kind;
        impl_name : string;
        ctxt : 'a;
        ops : ('a, 'b) ops;
        equality_witness : ('a, 'b) equality_witness;
      }
        -> t

  let mem (Context {ops = (module Ops); ctxt; _}) key = Ops.mem ctxt key

  let add (Context ({ops = (module Ops); ctxt; _} as c)) key value =
    Ops.add ctxt key value >|= fun ctxt -> Context {c with ctxt}

  let find (Context {ops = (module Ops); ctxt; _}) key = Ops.find ctxt key

  let remove (Context ({ops = (module Ops); ctxt; _} as c)) key =
    Ops.remove ctxt key >|= fun ctxt -> Context {c with ctxt}

  (* trees *)
  type tree =
    | Tree : {
        ops : ('a, 'b) ops;
        impl_name : string;
        tree : 'b;
        equality_witness : ('a, 'b) equality_witness;
      }
        -> tree

  let mem_tree (Context {ops = (module Ops); ctxt; _}) key =
    Ops.mem_tree ctxt key

  let add_tree (Context ({ops = (module Ops); ctxt; _} as c)) key (Tree t) =
    match equiv c.equality_witness t.equality_witness with
    | (Some Refl, Some Refl) ->
        Ops.add_tree ctxt key t.tree >|= fun ctxt -> Context {c with ctxt}
    | _ -> err_implementation_mismatch ~expected:c.impl_name ~got:t.impl_name

  let find_tree
      (Context
        {ops = (module Ops) as ops; ctxt; equality_witness; impl_name; _}) key =
    Ops.find_tree ctxt key
    >|= Option.map (fun tree -> Tree {ops; tree; equality_witness; impl_name})

  let list
      (Context
        {ops = (module Ops) as ops; ctxt; equality_witness; impl_name; _})
      ?offset ?length key =
    Ops.list ctxt ?offset ?length key >|= fun ls ->
    List.fold_left
      (fun acc (k, tree) ->
        let v = Tree {ops; tree; equality_witness; impl_name} in
        (k, v) :: acc)
      []
      (List.rev ls)

  let fold ?depth
      (Context
        {ops = (module Ops) as ops; ctxt; equality_witness; impl_name; _}) key
      ~init ~f =
    Ops.fold ?depth ctxt key ~init ~f:(fun k v acc ->
        let v = Tree {ops; tree = v; equality_witness; impl_name} in
        f k v acc)

  (* Tree *)
  module Tree = struct
    let pp ppf (Tree {ops = (module Ops); tree; _}) = Ops.Tree.pp ppf tree

    let hash (Tree {ops = (module Ops); tree; _}) = Ops.Tree.hash tree

    let kind (Tree {ops = (module Ops); tree; _}) = Ops.Tree.kind tree

    let to_value (Tree {ops = (module Ops); tree; _}) = Ops.Tree.to_value tree

    let of_value
        (Context
          {ops = (module Ops) as ops; ctxt; equality_witness; impl_name; _}) v =
      Ops.Tree.of_value ctxt v >|= fun tree ->
      Tree {ops; tree; equality_witness; impl_name}

    let equal (Tree {ops = (module Ops); tree; equality_witness; _}) (Tree t) =
      match equiv equality_witness t.equality_witness with
      | (Some Refl, Some Refl) -> Ops.Tree.equal tree t.tree
      | _ -> false

    let empty
        (Context
          {ops = (module Ops) as ops; equality_witness; ctxt; impl_name; _}) =
      let empty = Ops.Tree.empty ctxt in
      Tree {ops; equality_witness; tree = empty; impl_name}

    let is_empty (Tree {ops = (module Ops); tree; _}) = Ops.Tree.is_empty tree

    let mem (Tree {ops = (module Ops); tree; _}) key = Ops.Tree.mem tree key

    let add (Tree ({ops = (module Ops); tree; _} as c)) key value =
      Ops.Tree.add tree key value >|= fun tree -> Tree {c with tree}

    let find (Tree {ops = (module Ops); tree; _}) key = Ops.Tree.find tree key

    let mem_tree (Tree {ops = (module Ops); tree; _}) key =
      Ops.Tree.mem_tree tree key

    let add_tree (Tree ({ops = (module Ops); _} as c)) key (Tree t) =
      match equiv c.equality_witness t.equality_witness with
      | (Some Refl, Some Refl) ->
          Ops.Tree.add_tree c.tree key t.tree >|= fun tree -> Tree {c with tree}
      | _ -> err_implementation_mismatch ~expected:c.impl_name ~got:t.impl_name

    let find_tree (Tree ({ops = (module Ops); tree; _} as c)) key =
      Ops.Tree.find_tree tree key
      >|= Option.map (fun tree -> Tree {c with tree})

    let remove (Tree ({ops = (module Ops); tree; _} as c)) key =
      Ops.Tree.remove tree key >|= fun tree -> Tree {c with tree}

    let list
        (Tree {ops = (module Ops) as ops; tree; equality_witness; impl_name})
        ?offset ?length key =
      Ops.Tree.list tree ?offset ?length key >|= fun ls ->
      List.fold_left
        (fun acc (k, tree) ->
          let v = Tree {ops; tree; equality_witness; impl_name} in
          (k, v) :: acc)
        []
        (List.rev ls)

    let fold ?depth
        (Tree
          {ops = (module Ops) as ops; tree = t; equality_witness; impl_name})
        key ~init ~f =
      Ops.Tree.fold ?depth t key ~init ~f:(fun k v acc ->
          let v = Tree {ops; tree = v; equality_witness; impl_name} in
          f k v acc)

    let clear ?depth (Tree {ops = (module Ops); tree; _}) =
      Ops.Tree.clear ?depth tree
  end

  (* misc *)

  let set_protocol (Context ({ops = (module Ops); ctxt; _} as c)) protocol_hash
      =
    Ops.set_protocol ctxt protocol_hash >|= fun ctxt -> Context {c with ctxt}

  let get_protocol (Context {ops = (module Ops); ctxt; _}) =
    Ops.get_protocol ctxt

  let fork_test_chain (Context ({ops = (module Ops); ctxt; _} as c)) ~protocol
      ~expiration =
    Ops.fork_test_chain ctxt ~protocol ~expiration >|= fun ctxt ->
    Context {c with ctxt}
end

module Register (C : CONTEXT) = struct
  type _ Context.kind += Context : C.t Context.kind

  let equality_witness : (C.t, C.tree) Context.equality_witness =
    Context.equality_witness ()

  let ops = (module C : CONTEXT with type t = 'ctxt and type tree = 'tree)
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
