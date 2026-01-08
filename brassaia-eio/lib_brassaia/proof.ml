(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open! Import
include Proof_intf

module Make (C : Type.S) (H : Type.S) = struct
  type contents = C.t [@@deriving brassaia]

  type hash = H.t [@@deriving brassaia]

  type kinded_hash = [`Contents of hash | `Node of hash]

  (* Custom Repr encoding to be coherent with the previous binary
     representation that included metadatas *)
  let kinded_hash_t =
    let open Type in
    variant "kinded_hash" (fun c n -> function
      | `Contents h -> c ((), h)
      | `Node h -> n h)
    |~ case1 "contents" (pair unit hash_t) (fun ((), h) -> `Contents h)
    |~ case1 "node" hash_t (fun h -> `Node h)
    |> sealv

  type 'a inode = {length : int; proofs : (int * 'a) list} [@@deriving brassaia]

  type 'a inode_extender = {length : int; segments : int list; proof : 'a}
  [@@deriving brassaia]

  type tree =
    | Contents of contents
    | Blinded_contents of hash
    | Node of (Path.step * tree) list
    | Blinded_node of hash
    | Inode of inode_tree inode
    | Extender of inode_tree inode_extender
  [@@deriving brassaia]

  and inode_tree =
    | Blinded_inode of hash
    | Inode_values of (Path.step * tree) list
    | Inode_tree of inode_tree inode
    | Inode_extender of inode_tree inode_extender
  [@@deriving brassaia]

  (* Custom Repr encoding to be coherent with the previous binary
     representation that included metadatas *)
  let tree_t =
    let open Type in
    variant
      "tree"
      (fun
        contents blinded_contents node blinded_node inode extender ->
        function
      | Contents c -> contents (c, ())
      | Blinded_contents h -> blinded_contents (h, ())
      | Node l -> node l
      | Blinded_node h -> blinded_node h
      | Inode i -> inode i
      | Extender e -> extender e)
    |~ case1 "contents" (pair contents_t unit) (fun (c, ()) -> Contents c)
    |~ case1 "blinded-contents" (pair hash_t unit) (fun (h, ()) ->
           Blinded_contents h)
    |~ case1 "node" (list (pair Path.step_t tree_t)) (fun h -> Node h)
    |~ case1 "blinded-node" hash_t (fun h -> Blinded_node h)
    |~ case1 "inode" (inode_t inode_tree_t) (fun i -> Inode i)
    |~ case1 "extender" (inode_extender_t inode_tree_t) (fun e -> Extender e)
    |> sealv

  type elt =
    | Contents of contents
    | Node of (Path.step * kinded_hash) list
    | Inode of hash inode
    | Inode_extender of hash inode_extender
  [@@deriving brassaia]

  type stream = elt Seq.t [@@deriving brassaia]

  type t = {before : kinded_hash; after : kinded_hash; state : tree}
  [@@deriving brassaia]

  let before t = t.before

  let after t = t.after

  let state t = t.state

  let init ~before ~after state = {after; before; state}
end

exception Bad_proof of {context : string}

let bad_proof_exn context = raise (Bad_proof {context})

module Env (Backend : Backend.S) = struct
  module H = Backend.Hash

  module Hashes = struct
    module Unsafe = Hashtbl.Make (struct
      type t = H.t

      let hash = H.short_hash

      let equal = Type.(unstage (equal H.t))
    end)

    type 'a t = {lock : Eio.Mutex.t; data : 'a Unsafe.t}

    let of_unsafe data = {lock = Eio.Mutex.create (); data}

    let create size = of_unsafe (Unsafe.create size)

    let mem {lock; data} k =
      Eio.Mutex.use_ro lock @@ fun () -> Unsafe.mem data k

    let find_opt {lock; data} k =
      Eio.Mutex.use_ro lock @@ fun () -> Unsafe.find_opt data k

    let add {lock; data} k v =
      Eio.Mutex.use_rw ~protect:true lock @@ fun () -> Unsafe.add data k v

    let replace {lock; data} k v =
      Eio.Mutex.use_rw ~protect:true lock @@ fun () -> Unsafe.replace data k v

    let of_seq s = of_unsafe (Unsafe.of_seq s)

    let of_list l = of_seq (List.to_seq l)

    let to_list t =
      Eio.Mutex.use_ro t.lock @@ fun () -> List.of_seq (Unsafe.to_seq t.data)

    let t elt_t = Type.map [%typ: (H.t * elt) list] of_list to_list
  end

  type mode = Produce | Serialise | Deserialise | Consume
  [@@deriving brassaia]

  module Set = struct
    type produce = {
      nodes : Backend.Node.Val.t Hashes.t;
      contents : Backend.Contents.Val.t Hashes.t;
    }
    [@@deriving brassaia]

    type deserialise = {
      nodes : Backend.Node_portable.t Hashes.t;
      contents : Backend.Contents.Val.t Hashes.t;
    }
    [@@deriving brassaia]

    type t =
      | Produce of produce
      | Serialise of produce
      | Deserialise of deserialise
      | Consume of deserialise
    [@@deriving brassaia]

    let producer () =
      Produce {contents = Hashes.create 13; nodes = Hashes.create 13}

    let deserialiser () =
      Deserialise {contents = Hashes.create 13; nodes = Hashes.create 13}
  end

  type v = Empty | Set of Set.t [@@deriving brassaia]

  type t = v Atomic.t

  let t = Type.map v_t Atomic.make Atomic.get

  let empty () : t = Atomic.make Empty

  let is_empty t = Atomic.get t = Empty

  let copy ~into t = Atomic.set into (Atomic.get t)

  type hash = H.t [@@deriving brassaia ~equal ~pp]

  let set_mode t mode =
    Atomic.set t
    @@
    match (Atomic.get t, mode) with
    | Empty, Produce -> Set Set.(producer ())
    | Empty, Deserialise -> Set Set.(deserialiser ())
    | Set (Produce set), Serialise -> Set Set.(Serialise set)
    | Set (Deserialise set), Consume -> Set Set.(Consume set)
    | _ -> assert false

  let with_consume f =
    let t = Atomic.make Empty in
    set_mode t Deserialise ;
    let stop_deserialise () = set_mode t Consume in
    let res = f t ~stop_deserialise in
    Atomic.set t Empty ;
    res

  let with_produce f =
    let t = Atomic.make Empty in
    set_mode t Produce ;
    let start_serialise () = set_mode t Serialise in
    let res = f t ~start_serialise in
    Atomic.set t Empty ;
    res

  module Contents_hash = Hash.Typed (H) (Backend.Contents.Val)

  let find_contents t h =
    match Atomic.get t with
    | Empty -> None
    | Set (Produce set) ->
        (* Sharing of contents is not strictly needed during this phase. It
           could be disabled. *)
        Hashes.find_opt set.contents h
    | Set (Serialise set) ->
        (* This is needed in order to differenciate between blinded contents
           from others. *)
        Hashes.find_opt set.contents h
    | Set (Deserialise _) ->
        (* This phase only fills the env, it should search for anything *)
        assert false
    | Set (Consume set) ->
        (* Use the Env to feed the values during consume *)
        Hashes.find_opt set.contents h

  let add_contents_from_store t h v =
    match Atomic.get t with
    | Empty -> ()
    | Set (Produce set) ->
        (* Registering in [set] for traversal during [Serialise]. *)
        assert (not (Hashes.mem set.contents h)) ;
        Hashes.add set.contents h v
    | Set (Serialise _) ->
        (* There shouldn't be new contents during this phase *)
        assert false
    | Set (Deserialise _) ->
        (* This phase has no repo pointer *)
        assert false
    | Set (Consume _) ->
        (* This phase has no repo pointer *)
        assert false

  let add_contents_from_proof t h v =
    match Atomic.get t with
    | Set (Deserialise set) ->
        (* Using [replace] because there could be several instances of this
           contents in the proof, we will not share as this is not strictly
           needed. *)
        Hashes.replace set.contents h v
    | Empty ->
        (* Happens during [hash_of_proof_state] *)
        ()
    | _ -> assert false

  let find_node t h =
    match Atomic.get t with
    | Empty -> None
    | Set (Produce set) ->
        (* This is needed in order to achieve sharing on inode's pointers. In
           other words, each node present in the [before] tree should have a
           single [P.Node.Val.t] representative that will witness all the lazy
           inode loadings. *)
        Hashes.find_opt set.nodes h
    | Set (Serialise set) ->
        (* This is needed in order to follow loaded paths in the [before]
           tree. *)
        Hashes.find_opt set.nodes h
    | Set (Deserialise _) ->
        (* This phase only fills the env, it should search for anything *)
        assert false
    | Set (Consume _) ->
        (* This phase looks for portable nodes *)
        None

  let find_pnode t h =
    match Atomic.get t with
    | Set (Consume set) ->
        (* [set] has been filled during deserialise. Using it to provide values
            during consume. *)
        Hashes.find_opt set.nodes h
    | _ -> None

  let add_node_from_store t h v =
    match Atomic.get t with
    | Empty -> v
    | Set (Produce set) ->
        (* Registering in [set] for sharing during [Produce] and traversal
           during [Serialise]. This assertion is guarenteed because
           [add_node_from_store] is guarded by a call to [find_node] in tree. *)
        assert (not (Hashes.mem set.nodes h)) ;
        Hashes.add set.nodes h v ;
        v
    | Set (Serialise _) ->
        (* There shouldn't be new nodes during this phase *)
        assert false
    | Set (Deserialise _) ->
        (* This phase has no repo pointer *)
        assert false
    | Set (Consume _) ->
        (* This phase has no repo pointer *)
        assert false

  let add_pnode_from_proof t h v =
    match Atomic.get t with
    | Set (Deserialise set) ->
        (* Using [replace] because there could be several instances of this
           node in the proof, we will not share as this is not strictly
           needed.
           All the occurences of this node in the proof are expected to have
           the same blinded/visible coverage (i.e. the same node proof). *)
        Hashes.replace set.nodes h v
    | Empty ->
        (* Happens during [hash_of_proof_state] *)
        ()
    | _ -> assert false
end
