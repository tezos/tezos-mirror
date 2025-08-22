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

open Tezos_context_encoding.Context
module Env = Env

module type DB = Irmin.Generic_key.S with module Schema = Schema

module Kinded_hash = struct
  let of_context_hash = function
    | `Value h -> `Contents (Hash.of_context_hash h, ())
    | `Node h -> `Node (Hash.of_context_hash h)

  let to_context_hash = function
    | `Contents (h, ()) -> `Value (Hash.to_context_hash h)
    | `Node h -> `Node (Hash.to_context_hash h)
end

type proof_version_expanded = {is_stream : bool; is_binary : bool}

let stream_mask = 0b1

let binary_mask = 0b10

let decode_proof_version v =
  let extract_bit v mask = (v land mask <> 0, v land lnot mask) in
  let is_stream, v = extract_bit v stream_mask in
  let is_binary, v = extract_bit v binary_mask in
  if v <> 0 then Error `Invalid_proof_version else Ok {is_stream; is_binary}

let encode_proof_version ~is_stream ~is_binary =
  (if is_stream then stream_mask else 0)
  lor if is_binary then binary_mask else 0

module Make_config (Conf : Conf) = struct
  let equal_config = Tezos_context_sigs.Config.equal

  let config _ =
    Tezos_context_sigs.Config.v
      ~entries:Conf.entries
      ~stable_hash:Conf.stable_hash
      ~inode_child_order:Conf.inode_child_order
end

type raw = [`Value of bytes | `Tree of raw String.Map.t]

let raw_encoding : raw Data_encoding.t =
  let open Data_encoding in
  mu "Tree.raw" (fun encoding ->
      let map_encoding =
        conv
          String.Map.bindings
          (fun bindings -> String.Map.of_seq (List.to_seq bindings))
          (list (tup2 string encoding))
      in
      union
        [
          case
            ~title:"tree"
            (Tag 0)
            map_encoding
            (function `Tree t -> Some t | `Value _ -> None)
            (fun t -> `Tree t);
          case
            ~title:"value"
            (Tag 1)
            bytes
            (function `Value v -> Some v | `Tree _ -> None)
            (fun v -> `Value v);
        ])

module Make_tree (Conf : Conf) (Store : DB) = struct
  include Store.Tree
  include Make_config (Conf)

  let pp = Irmin.Type.pp Store.tree_t

  let empty _ = Store.Tree.empty ()

  let equal = Irmin.Type.(unstage (equal Store.tree_t))

  let is_empty t = equal (Store.Tree.empty ()) t

  let hash t = Hash.to_context_hash (Store.Tree.hash t)

  let add t k v = Store.Tree.add t k v

  let kind t =
    match Store.Tree.destruct t with `Contents _ -> `Value | `Node _ -> `Tree

  let to_value t =
    let open Lwt_syntax in
    match Store.Tree.destruct t with
    | `Contents (c, _) ->
        let+ v = Store.Tree.Contents.force_exn c in
        Some v
    | `Node _ -> Lwt.return_none

  let of_value _ v = Store.Tree.add (Store.Tree.empty ()) [] v

  let fold ?depth t k ~(order : [`Sorted | `Undefined]) ~init ~f =
    let open Lwt_syntax in
    let* o = find_tree t k in
    match o with
    | None -> Lwt.return init
    | Some t ->
        let order =
          (order :> [`Random of Random.State.t | `Sorted | `Undefined])
        in
        Store.Tree.fold
          ?depth
          ~force:`True
          ~cache:false
          ~uniq:`False
          ~order
          ~tree:(fun k t acc ->
            match kind t with
            | `Value -> if k = [] then Lwt.return acc else f k t acc
            | `Tree -> f k t acc)
          t
          init

  type raw = [`Value of bytes | `Tree of raw String.Map.t]

  type concrete = Store.Tree.concrete

  let rec raw_of_concrete : type a. (raw -> a) -> concrete -> a =
   fun k -> function
    | `Tree l -> raw_of_node (fun l -> k (`Tree (String.Map.of_seq l))) l
    | `Contents (v, _) -> k (`Value v)

  and raw_of_node : type a.
      ((string * raw) Seq.t -> a) -> (string * concrete) list -> a =
   fun k -> function
    | [] -> k Seq.empty
    | (n, v) :: t ->
        raw_of_concrete
          (fun v -> raw_of_node (fun t -> k (fun () -> Seq.Cons ((n, v), t))) t)
          v

  let to_raw t =
    let open Lwt_syntax in
    let+ c = Store.Tree.to_concrete t in
    raw_of_concrete (fun t -> t) c

  let rec concrete_of_raw : type a. (concrete -> a) -> raw -> a =
   fun k -> function
    | `Tree l -> concrete_of_node (fun l -> k (`Tree l)) (String.Map.to_seq l)
    | `Value v -> k (`Contents (v, ()))

  and concrete_of_node : type a.
      ((string * concrete) list -> a) -> (string * raw) Seq.t -> a =
   fun k seq ->
    match seq () with
    | Nil -> k []
    | Cons ((n, v), t) ->
        concrete_of_raw
          (fun v -> concrete_of_node (fun t -> k ((n, v) :: t)) t)
          v

  let of_raw = concrete_of_raw Store.Tree.of_concrete

  let raw_encoding = raw_encoding

  (** [unshallow t] is the tree equivalent to [t] but with all subtrees evaluated,
    i.e. without "reference" nodes.
    This is done by calling `of_raw . to_raw`, which is *not* the identity function.
    TODO: find a more efficient way to do the same, maybe with `fold` *)
  let unshallow t =
    let open Lwt_syntax in
    let* r = to_raw t in
    return (of_raw r)

  type repo = Store.repo

  let make_repo =
    let prng_state = lazy (Random.State.make_self_init ()) in
    (* [irmin-pack] stores implicitly share instances according to a string
       argument (for persistent stores, this is the store's file path). To avoid
       having hidden global state, we generate a unique string each time. *)
    let random_store_name () =
      let prng_state = Lazy.force prng_state in
      String.init 64 (fun _ -> Char.chr (Random.State.int prng_state 256))
    in
    fun () -> Store.Repo.v @@ Irmin_pack.config @@ random_store_name ()

  let kinded_key t =
    match Store.Tree.key t with
    | (None | Some (`Node _)) as r -> r
    | Some (`Contents (v, ())) -> Some (`Value v)

  let is_shallow tree =
    match Store.Tree.inspect tree with
    | `Node `Key -> true
    | `Node (`Map | `Value | `Portable_dirty | `Pruned) | `Contents -> false

  let list tree ?offset ?length key =
    Store.Tree.list ~cache:true tree ?offset ?length key

  let length tree key = Store.Tree.length ~cache:true tree key

  exception Context_dangling_hash of string

  let find_tree tree key =
    Lwt.catch
      (fun () -> Store.Tree.find_tree tree key)
      (function
        | Store.Backend.Node.Val.Dangling_hash {context; hash}
        | Store.Tree.Dangling_hash {context; hash} ->
            let str =
              Fmt.str
                "%s encountered dangling hash %a"
                context
                (Irmin.Type.pp Hash.t)
                hash
            in
            raise (Context_dangling_hash str)
        | exn -> Lwt.reraise exn)

  let add_tree tree key value =
    Lwt.catch
      (fun () -> Store.Tree.add_tree tree key value)
      (function
        | Store.Backend.Node.Val.Dangling_hash {context; hash}
        | Store.Tree.Dangling_hash {context; hash} ->
            let str =
              Fmt.str
                "%s encountered dangling hash %a"
                context
                (Irmin.Type.pp Hash.t)
                hash
            in
            raise (Context_dangling_hash str)
        | exn -> Lwt.reraise exn)
end

module Proof_encoding = Tezos_context_merkle_proof_encoding

module Make_proof
    (Store : DB)
    (Store_conf : Tezos_context_encoding.Context.Conf) =
struct
  module DB_proof = Store.Tree.Proof

  module Proof = struct
    include Tezos_context_sigs.Context.Proof_types

    module State = struct
      let rec to_inode : type a b. (a -> b) -> a DB_proof.inode -> b inode =
       fun f {length; proofs} ->
        {length; proofs = List.map (fun (k, v) -> (k, f v)) proofs}

      and to_tree : DB_proof.tree -> tree = function
        | Contents (c, ()) -> Value c
        | Blinded_contents (h, ()) -> Blinded_value (Hash.to_context_hash h)
        | Node l -> Node (List.map (fun (k, v) -> (k, to_tree v)) l)
        | Blinded_node h -> Blinded_node (Hash.to_context_hash h)
        | Inode i -> Inode (to_inode to_inode_tree i)
        | Extender e -> Extender (to_inode_extender to_inode_tree e)

      and to_inode_extender : type a b.
          (a -> b) -> a DB_proof.inode_extender -> b inode_extender =
       fun f {length; segments = segment; proof} ->
        {length; segment; proof = f proof}

      and to_inode_tree : DB_proof.inode_tree -> inode_tree = function
        | Blinded_inode h -> Blinded_inode (Hash.to_context_hash h)
        | Inode_values l ->
            Inode_values (List.map (fun (k, v) -> (k, to_tree v)) l)
        | Inode_tree i -> Inode_tree (to_inode to_inode_tree i)
        | Inode_extender e -> Inode_extender (to_inode_extender to_inode_tree e)

      let rec of_inode : type a b. (a -> b) -> a inode -> b DB_proof.inode =
       fun f {length; proofs} ->
        {length; proofs = List.map (fun (k, v) -> (k, f v)) proofs}

      and of_tree : tree -> DB_proof.tree = function
        | Value c -> Contents (c, ())
        | Blinded_value h -> Blinded_contents (Hash.of_context_hash h, ())
        | Node l -> Node (List.map (fun (k, v) -> (k, of_tree v)) l)
        | Blinded_node h -> Blinded_node (Hash.of_context_hash h)
        | Inode i -> Inode (of_inode of_inode_tree i)
        | Extender e -> Extender (of_inode_extender of_inode_tree e)

      and of_inode_extender : type a b.
          (a -> b) -> a inode_extender -> b DB_proof.inode_extender =
       fun f {length; segment = segments; proof} ->
        {length; segments; proof = f proof}

      and of_inode_tree : inode_tree -> DB_proof.inode_tree = function
        | Blinded_inode h -> Blinded_inode (Hash.of_context_hash h)
        | Inode_values l ->
            Inode_values (List.map (fun (k, v) -> (k, of_tree v)) l)
        | Inode_tree i -> Inode_tree (of_inode of_inode_tree i)
        | Inode_extender e -> Inode_extender (of_inode_extender of_inode_tree e)

      let of_stream_elt : Stream.elt -> DB_proof.elt = function
        | Value c -> Contents c
        | Node l ->
            Node (List.map (fun (k, v) -> (k, Kinded_hash.of_context_hash v)) l)
        | Inode i -> Inode (of_inode Hash.of_context_hash i)
        | Inode_extender e ->
            Inode_extender (of_inode_extender Hash.of_context_hash e)

      let of_stream : stream -> DB_proof.stream = Seq.map of_stream_elt

      let to_stream_elt : DB_proof.elt -> Stream.elt = function
        | Contents c -> Value c
        | Node l ->
            Node (List.map (fun (k, v) -> (k, Kinded_hash.to_context_hash v)) l)
        | Inode i -> Inode (to_inode Hash.to_context_hash i)
        | Inode_extender e ->
            Inode_extender (to_inode_extender Hash.to_context_hash e)

      let to_stream : DB_proof.stream -> stream = Seq.map to_stream_elt
    end

    let is_binary =
      if Store_conf.entries = 2 then true
      else if Store_conf.entries = 32 then false
      else assert false

    let of_proof ~is_stream f p =
      let before = Kinded_hash.to_context_hash (DB_proof.before p) in
      let after = Kinded_hash.to_context_hash (DB_proof.after p) in
      let state = f (DB_proof.state p) in
      let version = encode_proof_version ~is_stream ~is_binary in
      {version; before; after; state}

    let to_proof f p =
      let before = Kinded_hash.of_context_hash p.before in
      let after = Kinded_hash.of_context_hash p.after in
      let state = f p.state in
      DB_proof.v ~before ~after state

    let to_tree = of_proof ~is_stream:false State.to_tree

    let of_tree = to_proof State.of_tree

    let to_stream = of_proof ~is_stream:true State.to_stream

    let of_stream = to_proof State.of_stream
  end

  let produce_tree_proof repo key f =
    let open Lwt_syntax in
    let key =
      match key with `Node n -> `Node n | `Value v -> `Contents (v, ())
    in
    let+ p, r = Store.Tree.produce_proof repo key f in
    (Proof.to_tree p, r)

  let verify_tree_proof proof f =
    let proof = Proof.of_tree proof in
    Store.Tree.verify_proof proof f

  let produce_stream_proof repo key f =
    let open Lwt_syntax in
    let key =
      match key with `Node n -> `Node n | `Value v -> `Contents (v, ())
    in
    let+ p, r = Store.Tree.produce_stream repo key f in
    (Proof.to_stream p, r)

  let verify_stream_proof proof f =
    let proof = Proof.of_stream proof in
    Store.Tree.verify_stream proof f
end

type error += Unsupported_context_hash_version of Context_hash.Version.t

let () =
  register_error_kind
    `Permanent
    ~id:"context_hash.unsupported_version"
    ~title:"Unsupported context hash version"
    ~description:"Unsupported context hash version."
    ~pp:(fun ppf version ->
      Format.fprintf
        ppf
        "@[Context hash version %a is not supported.@,\
         You might need to update the shell.@]"
        Context_hash.Version.pp
        version)
    Data_encoding.(obj1 (req "version" Context_hash.Version.encoding))
    (function
      | Unsupported_context_hash_version version -> Some version | _ -> None)
    (fun version -> Unsupported_context_hash_version version)
