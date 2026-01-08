(*
 * Copyright (c) 2013      Louis Gesbert     <louis.gesbert@ocamlpro.com>
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
include Node_intf

let src = Logs.Src.create "brassaia.node" ~doc:"Brassaia trees/nodes"

module Log = (val Logs.src_log src : Logs.LOG)

(* Add [merge] to a [Core] implementation. *)
module Of_core (S : Core) = struct
  include S
  (* Merges *)

  let all_contents t =
    let kvs = S.list t in
    List.fold_left
      (fun acc -> function k, `Contents c -> (k, c) :: acc | _ -> acc)
      [] kvs

  let all_succ t =
    let kvs = S.list t in
    List.fold_left
      (fun acc -> function k, `Node n -> (k, n) :: acc | _ -> acc)
      [] kvs

  let merge_contents merge_key =
    Merge.alist Path.step_t S.contents_key_t (fun _step -> merge_key)

  let merge_node merge_key =
    Merge.alist Path.step_t S.node_key_t (fun _step -> merge_key)

  (* FIXME: this is very broken; do the same thing as [Tree.merge]
     instead. *)
  let merge ~contents ~node =
    let explode t = (all_contents t, all_succ t) in
    let implode (contents, succ) =
      let xs = List.rev_map (fun (s, c) -> (s, `Contents c)) contents in
      let ys = List.rev_map (fun (s, n) -> (s, `Node n)) succ in
      S.of_list (xs @ ys)
    in
    let merge = Merge.pair (merge_contents contents) (merge_node node) in
    Merge.like S.t merge explode implode
end

module Brassaia_hash = Hash

(* A [Make] implementation providing the subset of [S] that can be implemented
   over abstract [key] types. *)
module Make_core
    (Hash : Hash.S)
    (Contents_key : Key.S with type hash = Hash.t)
    (Node_key : Key.S with type hash = Hash.t) =
struct
  type contents_key = Contents_key.t [@@deriving brassaia]

  let contents_key_encoding = Contents_key.encoding

  type node_key = Node_key.t [@@deriving brassaia]

  let node_key_encoding = Node_key.encoding

  type hash = Hash.t [@@deriving brassaia]

  let hash_encoding = Hash.encoding

  type 'key contents_entry = { name : Path.step; contents : 'key }
  [@@deriving brassaia]

  type 'key contents_m_entry = { name : Path.step; contents : 'key }

  (* Custom Repr encoding to be coherent with the previous binary
     representation that included metadatas *)
  let contents_m_entry_t key_t =
    let open Type in
    record "contents_m_entry" (fun () name contents -> { name; contents })
    |+ field "metadata" unit (fun _ -> ())
    |+ field "name" Path.step_t (fun cont -> cont.name)
    |+ field "contents" key_t (fun cont -> cont.contents)
    |> sealr

  module StepMap = struct
    include Map.Make (struct
      type t = Path.step [@@deriving brassaia ~compare]
    end)

    let key_encoding = Path.step_encoding
  end

  type 'h node_entry = { name : Path.step; node : 'h } [@@deriving brassaia]

  type entry =
    | Node of node_key node_entry
    | Contents of contents_key contents_entry
    | Contents_m of contents_key contents_m_entry
    (* Invariant: the [_hash] cases are only externally reachable via
       [Portable.of_node]. *)
    | Node_hash of Hash.t node_entry
    | Contents_hash of Hash.t contents_entry
    | Contents_m_hash of Hash.t contents_m_entry
  [@@deriving brassaia]

  let entry_encoding =
    Data_encoding.conv
      Type.(to_string entry_t)
      Type.(
        of_string_exn ~path:"lib_brassaia/node.ml/Make_core/entry_encoding"
          entry_t)
      Data_encoding.string

  type t = entry StepMap.t

  let encoding =
    Data_encoding.conv StepMap.bindings
      (fun l -> List.to_seq l |> StepMap.of_seq)
      Data_encoding.(list (tup2 StepMap.key_encoding entry_encoding))

  type value = [ `Node of node_key | `Contents of contents_key ]

  (* Custom Repr encoding to be coherent with the previous binary
     representation that included metadatas *)
  let value_t =
    let open Type in
    variant "value" (fun n c _ -> function
      | `Node h -> n h
      | `Contents h -> c (h, ()))
    |~ case1 "node" node_key_t (fun k -> `Node k)
    |~ case1 "contents" (pair contents_key_t unit) (fun (h, ()) -> `Contents h)
    |~ case1 "contents-x" (pair contents_key_t unit) (fun (h, ()) ->
           `Contents h)
    |> sealv

  let value_encoding =
    let open Data_encoding in
    union
      [
        case (Tag 1) ~title:"`Node" node_key_encoding
          (function `Node k -> Some k | _ -> None)
          (fun k -> `Node k);
        case (Tag 2) ~title:"`Contents"
          (tup2 contents_key_encoding unit)
          (function `Contents k -> Some (k, ()) | _ -> None)
          (fun (k, ()) -> `Contents k);
      ]

  type weak_value = [ `Contents of hash | `Node of hash ]

  let weak_value_encoding =
    let open Data_encoding in
    union
      [
        case (Tag 1) ~title:"`Node" hash_encoding
          (function `Node k -> Some k | _ -> None)
          (fun k -> `Node k);
        case (Tag 2) ~title:"`Contents" (tup2 hash_encoding unit)
          (function `Contents k -> Some (k, ()) | _ -> None)
          (fun (k, ()) -> `Contents k);
      ]

  let to_entry (k, (v : value)) =
    match v with
    | `Node h -> Node { name = k; node = h }
    | `Contents h -> Contents { name = k; contents = h }

  let inspect_nonportable_entry_exn : entry -> Path.step * value = function
    | Node n -> (n.name, `Node n.node)
    | Contents c -> (c.name, `Contents c.contents)
    | Contents_m c -> (c.name, `Contents c.contents)
    | Node_hash _ | Contents_hash _ | Contents_m_hash _ ->
        (* Not reachable after [Portable.of_node]. See invariant on {!entry}. *)
        assert false

  let step_of_entry : entry -> Path.step = function
    | Node { name; _ }
    | Node_hash { name; _ }
    | Contents { name; _ }
    | Contents_m { name; _ }
    | Contents_hash { name; _ }
    | Contents_m_hash { name; _ } ->
        name

  let weak_of_entry : entry -> Path.step * weak_value = function
    | Node n -> (n.name, `Node (Node_key.to_hash n.node))
    | Node_hash n -> (n.name, `Node n.node)
    | Contents c -> (c.name, `Contents (Contents_key.to_hash c.contents))
    | Contents_m c -> (c.name, `Contents (Contents_key.to_hash c.contents))
    | Contents_hash c -> (c.name, `Contents c.contents)
    | Contents_m_hash c -> (c.name, `Contents c.contents)

  let of_seq l =
    Seq.fold_left
      (fun acc x -> StepMap.add (fst x) (to_entry x) acc)
      StepMap.empty l

  let of_list l = of_seq (List.to_seq l)

  let seq_entries ~offset ?length (t : t) =
    let take seq = match length with None -> seq | Some n -> Seq.take n seq in
    StepMap.to_seq t |> Seq.drop offset |> take

  let seq ?(offset = 0) ?length ?cache:_ (t : t) =
    seq_entries ~offset ?length t
    |> Seq.map (fun (_, e) -> inspect_nonportable_entry_exn e)

  let list ?offset ?length ?cache:_ t = List.of_seq (seq ?offset ?length t)
  let find_entry ?cache:_ (t : t) s = StepMap.find_opt s t

  let find ?cache (t : t) s =
    Option.map
      (fun e -> snd (inspect_nonportable_entry_exn e))
      (find_entry ?cache t s)

  let empty = Fun.const StepMap.empty
  let is_empty e = StepMap.is_empty e
  let length e = StepMap.cardinal e
  let clear _ = ()
  let equal_entry_opt = Type.(unstage (equal (option entry_t)))

  let add_entry t k e =
    StepMap.update k
      (fun e' -> if equal_entry_opt (Some e) e' then e' else Some e)
      t

  let add t k v =
    let e = to_entry (k, v) in
    add_entry t k e

  let remove t k = StepMap.remove k t

  let of_entries es =
    List.to_seq es |> Seq.map (fun e -> (step_of_entry e, e)) |> StepMap.of_seq

  let entries e = List.rev_map (fun (_, e) -> e) (StepMap.bindings e)

  module Hash_preimage = struct
    type entry =
      | Node_hash of Hash.t node_entry
      | Contents_hash of Hash.t contents_entry
      | Contents_m_hash of Hash.t contents_m_entry
    [@@deriving brassaia]

    type t = entry list [@@deriving brassaia ~pre_hash]
    type t_not_prefixed = t [@@deriving brassaia ~pre_hash]

    let pre_hash = Type.(unstage (pre_hash t))

    (* Manually add a prefix to default nodes, in order to prevent hash
       collision between contents and nodes (see
       https://github.com/mirage/brassaia/issues/1304).

       Prefixing the contents is not enough to prevent the collision: the
       prehash of a node starts with the number of its children, which can
       coincide with the prefix of the content's prehash. *)
    let pre_hash x f =
      f "N";
      pre_hash x f
  end

  let pre_hash pre_hash t f =
    let entries : Hash_preimage.t =
      StepMap.to_seq t
      |> Seq.map (fun (_, v) ->
             match v with
             (* Weaken keys to hashes *)
             | Node { name; node } ->
                 Hash_preimage.Node_hash { name; node = Node_key.to_hash node }
             | Contents { name; contents } ->
                 Contents_hash
                   { name; contents = Contents_key.to_hash contents }
             | Contents_m { name; contents } ->
                 Contents_m_hash
                   { name; contents = Contents_key.to_hash contents }
             | Node_hash { name; node } -> Node_hash { name; node }
             | Contents_hash { name; contents } ->
                 Contents_hash { name; contents }
             | Contents_m_hash { name; contents } ->
                 Contents_m_hash { name; contents })
      |> Seq.fold_left (fun xs x -> x :: xs) []
    in
    pre_hash entries f

  let t =
    let pre_hash = pre_hash Hash_preimage.pre_hash in
    Type.map ~pre_hash Type.(list entry_t) of_entries entries

  let t_not_prefixed =
    let pre_hash = pre_hash Hash_preimage.pre_hash_t_not_prefixed in
    Type.map ~pre_hash Type.(list entry_t) of_entries entries

  let with_handler _ t = t

  let head_entries t =
    let l = seq_entries ~offset:0 t |> List.of_seq in
    `Node l

  let head t =
    let (`Node l) = head_entries t in
    let l = List.map (fun (_, e) -> inspect_nonportable_entry_exn e) l in
    `Node l

  module Ht =
    Brassaia_hash.Typed
      (Hash)
      (struct
        type nonrec t = t [@@deriving brassaia]

        let encoding = encoding
      end)

  let hash_exn ?force:_ = Ht.hash
end

module Portable = struct
  module Of_core (X : sig
    type hash

    include
      Core
        with type hash := hash
         and type contents_key = hash
         and type node_key = hash
  end) =
  struct
    include X

    let of_node t = t

    type proof =
      [ `Blinded of hash
      | `Values of (Path.step * value) list
      | `Inode of int * (int * proof) list ]
    [@@deriving brassaia]

    let to_proof (t : t) : proof = `Values (seq t |> List.of_seq)

    let of_proof ~depth (t : proof) =
      assert (depth = 0);
      match t with
      | `Blinded _ | `Inode _ -> None
      | `Values e -> Some (of_list e)
  end

  module Of_node (X : S) = struct
    include Of_core (X)
    include X
  end

  module type S = Portable
end

module Make_generic_key
    (Hash : Hash.S)
    (Contents_key : Key.S with type hash = Hash.t)
    (Node_key : Key.S with type hash = Hash.t) =
struct
  module Core = Make_core (Hash) (Contents_key) (Node_key)
  include Core
  include Of_core (Core)

  module Portable = struct
    module Core = struct
      include Core

      type contents_key = hash [@@deriving brassaia]

      let contents_key_encoding = hash_encoding

      type node_key = hash [@@deriving brassaia]

      let node_key_encoding = hash_encoding

      type value = weak_value

      (* Custom Repr encoding to be coherent with the previous binary
         representation that included metadatas *)
      let value_t =
        let open Type in
        variant "Portable.value" (fun c n -> function
          | `Contents h -> c (h, ())
          | `Node h -> n h)
        |~ case1 "contents" (pair hash_t unit) (fun (h, ()) -> `Contents h)
        |~ case1 "node" hash_t (fun h -> `Node h)
        |> sealv

      let value_encoding = weak_value_encoding

      let to_entry name = function
        | `Node node -> Node_hash { name; node }
        | `Contents contents -> Contents_hash { name; contents }

      let of_seq s =
        Seq.fold_left
          (fun acc (name, v) -> StepMap.add name (to_entry name v) acc)
          StepMap.empty s

      let of_list s = of_seq (List.to_seq s)

      let add t name v =
        let entry = to_entry name v in
        add_entry t name entry

      let find ?cache t s =
        Option.map (fun e -> snd (weak_of_entry e)) (find_entry ?cache t s)

      let seq ?(offset = 0) ?length ?cache:_ (t : t) =
        seq_entries ~offset ?length t |> Seq.map (fun (_, e) -> weak_of_entry e)

      let list ?offset ?length ?cache t =
        List.of_seq (seq ?offset ?length ?cache t)

      let head t =
        let (`Node l) = head_entries t in
        let l = List.map (fun (_, e) -> weak_of_entry e) l in
        `Node l
    end

    include Of_core (Core)
    include Portable.Of_core (Core)
  end

  exception Dangling_hash of { context : string; hash : hash }

  type nonrec hash = hash [@@deriving brassaia ~pp]

  let () =
    Printexc.register_printer (function
      | Dangling_hash { context; hash } ->
          Some (Fmt.str "%s: encountered dangling hash %a" context pp_hash hash)
      | _ -> None)
end

module Make_generic_key_v2
    (Hash : Hash.S)
    (Contents_key : Key.S with type hash = Hash.t)
    (Node_key : Key.S with type hash = Hash.t) =
struct
  include Make_generic_key (Hash) (Contents_key) (Node_key)

  let t = t_not_prefixed

  module Portable = struct
    include Portable

    let t = t_not_prefixed
  end
end

module Make (Hash : Hash.S) = struct
  module Key = Key.Of_hash (Hash)
  include Make_generic_key (Hash) (Key) (Key)
end

module Store_generic_key
    (C : Contents.Store)
    (S : Indexable.S)
    (H : Hash.S with type t = S.hash)
    (V :
      S_generic_key
        with type t = S.value
         and type contents_key = C.Key.t
         and type node_key = S.Key.t) =
struct
  module Val = struct
    include V

    type hash = H.t
  end

  module Contents = C
  module Key = S.Key
  module Hash = Hash.Typed (H) (Val)

  type 'a t = 'a C.t * 'a S.t
  type value = S.value
  type key = Key.t
  type hash = Hash.t

  let mem (_, t) = S.mem t
  let find (_, t) = S.find t
  let add (_, t) = S.add t
  let unsafe_add (_, t) = S.unsafe_add t
  let index (_, t) h = S.index t h
  let batch (c, s) f = C.batch c (fun n -> S.batch s (fun s -> f (n, s)))

  let close (c, s) =
    let* () = C.close c in
    let+ () = S.close s in
    ()

  let rec merge t =
    let merge_key =
      Merge.init [%typ: Key.t option] (fun ~old x y ->
          Merge.(f (merge t)) ~old x y)
    in
    let merge = Val.merge ~contents:C.(merge (fst t)) ~node:merge_key in
    let read = function
      | None -> Lwt.return (Val.empty ())
      | Some k -> ( find t k >|= function None -> Val.empty () | Some v -> v)
    in
    let add v =
      if Val.is_empty v then Lwt.return_none else add t v >>= Lwt.return_some
    in
    Merge.like_lwt [%typ: Key.t option] merge read add
end

module Generic_key = struct
  module type S = S_generic_key
  module type Maker = Maker_generic_key
  module type Core = Core

  module Make = Make_generic_key
  module Store = Store_generic_key
  module Make_v2 = Make_generic_key_v2
end

module Store
    (C : Contents.Store)
    (S : Content_addressable.S with type key = C.key)
    (H : Hash.S with type t = S.key)
    (V : S with type t = S.value and type hash = S.key) =
struct
  module S = Indexable.Of_content_addressable (H) (S)
  include Store_generic_key (C) (S) (H) (V)
end

module Graph (S : Store) = struct
  module Contents_key = S.Contents.Key

  type contents_key = Contents_key.t [@@deriving brassaia]

  let _contents_key_encoding = Contents_key.encoding

  type node_key = S.Key.t [@@deriving brassaia]

  let _node_key_encoding = S.Key.encoding

  type 'a t = 'a S.t
  type value = [ `Contents of contents_key | `Node of node_key ]

  let empty t = S.add t (S.Val.empty ())

  let list t n =
    [%log.debug "steps"];
    S.find t n >|= function None -> [] | Some n -> S.Val.list n

  module U = struct
    type t = unit [@@deriving brassaia]

    let encoding = Data_encoding.unit
  end

  module Graph = Object_graph.Make (Contents_key) (S.Key) (U) (U)

  let edges t =
    List.rev_map
      (function _, `Node n -> `Node n | _, `Contents c -> `Contents c)
      (S.Val.list t)

  let pp_key = Type.pp S.Key.t
  let pp_keys = Fmt.(Dump.list pp_key)
  let pp_path = Type.pp Path.t
  let equal_val = Type.(unstage (equal S.Val.t))

  let pred t = function
    | `Node k -> ( S.find t k >|= function None -> [] | Some v -> edges v)
    | _ -> Lwt.return_nil

  let closure t ~min ~max =
    [%log.debug "closure min=%a max=%a" pp_keys min pp_keys max];
    let min = List.rev_map (fun x -> `Node x) min in
    let max = List.rev_map (fun x -> `Node x) max in
    let+ g = Graph.closure ~pred:(pred t) ~min ~max () in
    List.fold_left
      (fun acc -> function `Node x -> x :: acc | _ -> acc)
      [] (Graph.vertex g)

  let ignore_lwt _ = Lwt.return_unit

  let iter t ~min ~max ?(node = ignore_lwt) ?(contents = ignore_lwt) ?edge
      ?(skip_node = fun _ -> Lwt.return_false)
      ?(skip_contents = fun _ -> Lwt.return_false) ?(rev = true) () =
    let min = List.rev_map (fun x -> `Node x) min in
    let max = List.rev_map (fun x -> `Node x) max in
    let node = function
      | `Node x -> node x
      | `Contents c -> contents c
      | `Branch _ | `Commit _ -> Lwt.return_unit
    in
    let edge =
      Option.map
        (fun edge n pred ->
          match (n, pred) with
          | `Node src, `Node dst -> edge src dst
          | _ -> Lwt.return_unit)
        edge
    in
    let skip = function
      | `Node x -> skip_node x
      | `Contents c -> skip_contents c
      | _ -> Lwt.return_false
    in
    Graph.iter ~pred:(pred t) ~min ~max ~node ?edge ~skip ~rev ()

  let init t xs = S.add t (S.Val.of_list xs)

  let find_step t node step =
    [%log.debug "contents %a" pp_key node];
    S.find t node >|= function None -> None | Some n -> S.Val.find n step

  let find t node path =
    [%log.debug "read_node_exn %a %a" pp_key node pp_path path];
    let rec aux node path =
      match Path.decons path with
      | None -> Lwt.return_some (`Node node)
      | Some (h, tl) -> (
          find_step t node h >>= function
          | (None | Some (`Contents _)) as x -> Lwt.return x
          | Some (`Node node) -> aux node tl)
    in
    aux node path

  let err_empty_path () = invalid_arg "Brassaia.node: empty path"

  let map_one t node f label =
    [%log.debug "map_one %a" Type.(pp Path.step_t) label];
    let old_key = S.Val.find node label in
    let* old_node =
      match old_key with
      | None | Some (`Contents _) -> Lwt.return (S.Val.empty ())
      | Some (`Node k) -> (
          S.find t k >|= function None -> S.Val.empty () | Some v -> v)
    in
    let* new_node = f old_node in
    if equal_val old_node new_node then Lwt.return node
    else if S.Val.is_empty new_node then
      let node = S.Val.remove node label in
      if S.Val.is_empty node then Lwt.return (S.Val.empty ())
      else Lwt.return node
    else
      let+ k = S.add t new_node in
      S.Val.add node label (`Node k)

  let map t node path f =
    [%log.debug "map %a %a" pp_key node pp_path path];
    let rec aux node path =
      match Path.decons path with
      | None -> Lwt.return (f node)
      | Some (h, tl) -> map_one t node (fun node -> aux node tl) h
    in
    let* node =
      S.find t node >|= function None -> S.Val.empty () | Some n -> n
    in
    aux node path >>= S.add t

  let add t node path n =
    [%log.debug "add %a %a" pp_key node pp_path path];
    match Path.rdecons path with
    | Some (path, file) -> map t node path (fun node -> S.Val.add node file n)
    | None -> (
        match n with
        | `Node n -> Lwt.return n
        | `Contents _ -> failwith "TODO: Node.add")

  let rdecons_exn path =
    match Path.rdecons path with
    | Some (l, t) -> (l, t)
    | None -> err_empty_path ()

  let remove t node path =
    let path, file = rdecons_exn path in
    map t node path (fun node -> S.Val.remove node file)

  let value_t = S.Val.value_t
end

module V1 (N : Generic_key.S) = struct
  module K (H : Type.S) = struct
    let h = Type.string_of `Int64

    type t = H.t [@@deriving brassaia ~to_bin_string ~of_bin_string]

    let size_of = Type.Size.using to_bin_string (Type.Size.t h)

    let encode_bin =
      let encode_bin = Type.(unstage (encode_bin h)) in
      fun e k -> encode_bin (to_bin_string e) k

    let decode_bin =
      let decode_bin = Type.(unstage (decode_bin h)) in
      fun buf pos_ref ->
        let v = decode_bin buf pos_ref in
        match of_bin_string v with
        | Ok v -> v
        | Error (`Msg e) -> Fmt.failwith "decode_bin: %s" e

    let t = Type.like t ~bin:(encode_bin, decode_bin, size_of)
    let encoding = H.encoding
  end

  module Node_key = K (struct
    type t = N.node_key

    let t = N.node_key_t
    let encoding = N.node_key_encoding
  end)

  module Contents_key = K (struct
    type t = N.contents_key

    let t = N.contents_key_t
    let encoding = N.contents_key_encoding
  end)

  type node_key = Node_key.t [@@deriving brassaia]

  let node_key_encoding = Node_key.encoding

  type contents_key = Contents_key.t [@@deriving brassaia]

  let contents_key_encoding = Contents_key.encoding

  type hash = N.hash [@@deriving brassaia]

  let hash_encoding = N.hash_encoding

  type value = N.value

  let value_encoding = N.value_encoding

  type t = { n : N.t; entries : (Path.step * value) list }

  let encoding =
    Data_encoding.(
      conv
        (fun { n; entries } -> (n, entries))
        (fun (n, entries) -> { n; entries })
        (obj2 (req "n" N.encoding)
           (req "entries" (list (tup2 Path.step_encoding value_encoding)))))

  exception Dangling_hash = N.Dangling_hash

  let import n = { n; entries = N.list n }
  let export t = t.n
  let with_handler _ t = t
  let hash_exn ?force t = N.hash_exn ?force t.n
  let head t = N.head t.n

  let of_seq entries =
    let n = N.of_seq entries in
    let entries = List.of_seq entries in
    { n; entries }

  let of_list entries =
    let n = N.of_list entries in
    { n; entries }

  let seq ?(offset = 0) ?length ?cache:_ t =
    let take seq = match length with None -> seq | Some n -> Seq.take n seq in
    List.to_seq t.entries |> Seq.drop offset |> take

  let list ?offset ?length ?cache t = List.of_seq (seq ?offset ?length ?cache t)
  let empty () = { n = N.empty (); entries = [] }
  let is_empty t = t.entries = []
  let length e = N.length e.n
  let clear _ = ()
  let find ?cache t k = N.find ?cache t.n k

  let add t k v =
    let n = N.add t.n k v in
    if t.n == n then t else { n; entries = N.list n }

  let remove t k =
    let n = N.remove t.n k in
    if t.n == n then t else { n; entries = N.list n }

  let v1_step = Type.string_of `Int64
  let step_to_bin_string = Type.(unstage (to_bin_string v1_step))
  let step_of_bin_string = Type.(unstage (of_bin_string v1_step))

  let step_t : Path.step Type.t =
    let to_string p = step_to_bin_string p in
    let of_string s =
      step_of_bin_string s |> function
      | Ok x -> x
      | Error (`Msg e) -> Fmt.failwith "Step.of_string: %s" e
    in
    Type.(map (string_of `Int64)) of_string to_string

  let value_t =
    let open Type in
    record "node" (fun contents _ node ->
        match (contents, node) with
        | Some c, None -> `Contents c
        | None, Some n -> `Node n
        | _ -> failwith "invalid node")
    |+ field "contents" (option Contents_key.t) (function
         | `Contents x -> Some x
         | _ -> None)
    |+ field "metadata" (option unit) (fun _ -> None)
    |+ field "node" (option Node_key.t) (function
         | `Node n -> Some n
         | _ -> None)
    |> sealr

  let t : t Type.t =
    Type.map Type.(list ~len:`Int64 (pair step_t value_t)) of_list list

  let merge ~contents ~node =
    let merge = N.merge ~contents ~node in
    let f ~old x y =
      let old = Merge.map_promise (fun old -> old.n) old in
      let+ r = Merge.f merge ~old x.n y.n in
      match r with Ok r -> Ok (import r) | Error e -> Error e
    in
    Merge.init t f
end
