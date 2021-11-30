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

module type DB = Irmin.Generic_key.S with module Schema = Schema

module Kinded_hash = struct
  let of_context_hash = function
    | `Value h -> `Contents (Hash.of_context_hash h, ())
    | `Node h -> `Node (Hash.of_context_hash h)

  let to_context_hash = function
    | `Contents (h, ()) -> `Value (Hash.to_context_hash h)
    | `Node h -> `Node (Hash.to_context_hash h)
end

module Make_tree (Store : DB) = struct
  include Store.Tree

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

  type raw = [`Value of bytes | `Tree of raw TzString.Map.t]

  type concrete = Store.Tree.concrete

  let rec raw_of_concrete : type a. (raw -> a) -> concrete -> a =
   fun k -> function
    | `Tree l -> raw_of_node (fun l -> k (`Tree (TzString.Map.of_seq l))) l
    | `Contents (v, _) -> k (`Value v)

  and raw_of_node :
      type a. ((string * raw) Seq.t -> a) -> (string * concrete) list -> a =
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
    | `Tree l -> concrete_of_node (fun l -> k (`Tree l)) (TzString.Map.to_seq l)
    | `Value v -> k (`Contents (v, ()))

  and concrete_of_node :
      type a. ((string * concrete) list -> a) -> (string * raw) Seq.t -> a =
   fun k seq ->
    match seq () with
    | Nil -> k []
    | Cons ((n, v), t) ->
        concrete_of_raw
          (fun v -> concrete_of_node (fun t -> k ((n, v) :: t)) t)
          v

  let of_raw = concrete_of_raw Store.Tree.of_concrete

  let raw_encoding : raw Data_encoding.t =
    let open Data_encoding in
    mu "Tree.raw" (fun encoding ->
        let map_encoding =
          conv
            TzString.Map.bindings
            (fun bindings -> TzString.Map.of_seq (List.to_seq bindings))
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

  let shallow repo kinded_hash =
    let kinded_hash =
      match kinded_hash with `Node n -> `Node n | `Value v -> `Contents (v, ())
    in
    Store.Tree.shallow repo kinded_hash

  let list tree ?offset ?length key =
    Store.Tree.list ~cache:true tree ?offset ?length key

  let length tree key = Store.Tree.length ~cache:true tree key

  exception Context_dangling_hash of string

  exception Dangling_hash = Store.Backend.Node.Val.Dangling_hash

  let find_tree tree key =
    Lwt.catch
      (fun () -> Store.Tree.find_tree tree key)
      (function
        | Dangling_hash {context; hash} ->
            let str =
              Fmt.str
                "%s encountered dangling hash %a"
                context
                (Irmin.Type.pp Hash.t)
                hash
            in
            raise (Context_dangling_hash str)
        | exn -> raise exn)

  let add_tree tree key value =
    Lwt.catch
      (fun () -> Store.Tree.add_tree tree key value)
      (function
        | Dangling_hash {context; hash} ->
            let str =
              Fmt.str
                "%s encountered dangling hash %a"
                context
                (Irmin.Type.pp Hash.t)
                hash
            in
            raise (Context_dangling_hash str)
        | exn -> raise exn)
end

module Proof_encoding_V1 = struct
  open Tezos_context_sigs.Context.Proof_types
  open Data_encoding

  let value_encoding : value Data_encoding.t = bytes

  let length_field = req "length" (conv Int64.of_int Int64.to_int int64)

  let step_encoding : step Data_encoding.t =
    (* Context path name.  [bytes] must be used for JSON since we have no
       charset specificaiton. *)
    conv
      Bytes.unsafe_of_string
      Bytes.unsafe_to_string
      (Bounded.bytes 255 (* 1 byte for the length *))

  let hash_encoding = Context_hash.encoding

  let index_encoding =
    (* Assumes uint8 covers [0..Conf.entries-1] *)
    assert (Tezos_context_encoding.Context.Conf.entries <= 256) ;
    uint8

  let segment_encoding =
    assert (Tezos_context_encoding.Context.Conf.entries <= 32) ;
    (* The segment int is in 5bits. *)
    (* Format

       * Required bytes = (n * 5 + 8) / 8
       * 10* is filled at the end of the bytes

       ex: Encoding of [aaaaa; bbbbb; ccccc; ddddd; eeeee; ..; zzzzz]

               |76543210|76543210|7654.. ..       |76543210|
               |aaaaabbb|bbcccccd|ddde.. ..        zzzzz100|

               |76543210|76543210|7654.. ..  43210|76543210|
               |aaaaabbb|bbcccccd|ddde.. ..  yzzzz|z1000000|

               |76543210|76543210|7654.. .. 543210|76543210|
               |aaaaabbb|bbcccccd|ddde.. .. yzzzzz|10000000|
    *)
    let encode is =
      let buf = Buffer.create 0 in
      let push c = Buffer.add_char buf @@ Char.chr c in
      let close c bit = push (c lor (1 lsl (7 - bit))) in
      let write c bit i =
        if bit < 3 then (c lor (i lsl (3 - bit)), bit + 5)
        else
          let i = i lsl (11 - bit) in
          push (c lor (i / 256)) ;
          (i land 255, bit - 3)
      in
      let rec f c bit = function
        | [] -> close c bit
        | i :: is ->
            let (c, bit) = write c bit i in
            f c bit is
      in
      f 0 0 is ;
      Buffer.to_bytes buf
    in
    let decode b =
      let open Tzresult_syntax in
      let error = Error "invalid 5bit list" in
      let* l =
        let sl = Bytes.length b in
        if sl = 0 then error
        else
          let c = Char.code @@ Bytes.get b (sl - 1) in
          let* last_bit =
            if c = 0 then error
            else
              let rec aux i =
                if c land (1 lsl i) = 0 then aux (i + 1) else i + 1
              in
              Ok (aux 0)
          in
          let bits = (sl * 8) - last_bit in
          if bits mod 5 = 0 then Ok (bits / 5) else error
      in
      let s = Bytes.to_seq b in
      let head s =
        (* This assertion won't fail even with malformed input. *)
        match s () with
        | Seq.Nil -> assert false
        | Seq.Cons (c, s) -> (Char.code c, s)
      in
      let rec read c rembit l s =
        if l = 0 then []
        else
          let (c, s, rembit) =
            if rembit >= 5 then (c, s, rembit)
            else
              let (c', s) = head s in
              ((c * 256) + c', s, rembit + 8)
          in
          let rembit = rembit - 5 in
          let i = c lsr rembit in
          let c = c land ((1 lsl rembit) - 1) in
          i :: read c rembit (l - 1) s
      in
      Ok (read 0 0 l s)
    in
    conv_with_guard encode decode bytes

  let inode_proofs_encoding a =
    (* When the number of proofs is large enough (>= Context.Conf.entries / 2),
       proofs are encoded as `array` instead of `list` for compactness. *)
    (* This encode assumes that proofs are ordered by its index. *)
    let entries = Tezos_context_encoding.Context.Conf.entries in
    let encode_type =
      if entries <= 2 then fun _ -> `List
      else fun v ->
        if Compare.List_length_with.(v >= entries / 2) then `Array else `List
    in
    union
      ~tag_size:`Uint8
      [
        case
          ~title:"sparse_proof"
          (Tag 0)
          (obj1
             (req
                "sparse_proof"
                (conv_with_guard
                   (fun v -> List.map (fun (i, d) -> (i, Some d)) v)
                   (fun v ->
                     List.fold_right_e
                       (fun (i, d) acc ->
                         match d with
                         | None -> Error "cannot decode ill-formed Merkle proof"
                         | Some d -> Ok ((i, d) :: acc))
                       v
                       [])
                   (list (tup2 index_encoding a)))))
          (fun v -> if encode_type v = `List then Some v else None)
          (fun v -> v);
        case
          ~title:"dense_proof"
          (Tag 1)
          (obj1 (req "dense_proof" (array ~max_length:entries a)))
          (fun v ->
            if encode_type v = `Array then (
              let arr = Array.make entries None in
              (* The `a` passed to this encoding will be
                 `option_inode_tree_encoding` and `option_inode_tree_encoding`,
                 both encode `option` tags with its variant tag,
                 thus this `option` wrapping won't increase the encoding size. *)
              List.iter (fun (i, d) -> arr.(i) <- Some d) v ;
              Some arr)
            else None)
          (fun v ->
            let res = ref [] in
            Array.iteri
              (fun i -> function None -> () | Some d -> res := (i, d) :: !res)
              v ;
            List.rev !res);
      ]

  let inode_encoding a =
    conv
      (fun {length; proofs} -> (length, proofs))
      (fun (length, proofs) -> {length; proofs})
    @@ obj2 length_field (req "proofs" (inode_proofs_encoding a))

  let inode_extender_encoding a =
    conv
      (fun {length; segment; proof} -> (length, segment, proof))
      (fun (length, segment, proof) -> {length; segment; proof})
    @@ obj3 length_field (req "segment" segment_encoding) (req "proof" a)

  (* data-encoding.0.4/test/mu.ml for building mutually recursive data_encodings *)
  let (_inode_tree_encoding, tree_encoding) =
    let unoptionize enc =
      conv_with_guard
        (fun v -> Some v)
        (function
          | Some v -> Ok v
          | None -> Error "cannot decode ill-formed Merkle proof")
        enc
    in
    let mu_option_inode_tree_encoding tree_encoding =
      mu "inode_tree" (fun option_inode_tree_encoding ->
          let inode_tree_encoding = unoptionize option_inode_tree_encoding in
          union
            [
              case
                ~title:"Blinded_inode"
                (Tag 0)
                (obj1 (req "blinded_inode" hash_encoding))
                (function Some (Blinded_inode h) -> Some h | _ -> None)
                (fun h -> Some (Blinded_inode h));
              case
                ~title:"Inode_values"
                (Tag 1)
                (obj1
                   (req
                      "inode_values"
                      (list (tup2 step_encoding tree_encoding))))
                (function Some (Inode_values xs) -> Some xs | _ -> None)
                (fun xs -> Some (Inode_values xs));
              case
                ~title:"Inode_tree"
                (Tag 2)
                (obj1
                   (req
                      "inode_tree"
                      (inode_encoding option_inode_tree_encoding)))
                (function Some (Inode_tree i) -> Some i | _ -> None)
                (fun i -> Some (Inode_tree i));
              case
                ~title:"Inode_extender"
                (Tag 3)
                (obj1
                   (req
                      "inode_extender"
                      (inode_extender_encoding inode_tree_encoding)))
                (function
                  | Some (Inode_extender i : inode_tree) -> Some i | _ -> None)
                (fun i : inode_tree option -> Some (Inode_extender i));
              case
                ~title:"None"
                (Tag 4)
                (obj1 (req "none" null))
                (function None -> Some () | Some _ -> None)
                (fun () -> None);
            ])
    in
    let mu_option_tree_encoding : tree option encoding =
      mu "tree_encoding" (fun option_tree_encoding ->
          let tree_encoding = unoptionize option_tree_encoding in
          let option_inode_tree_encoding =
            mu_option_inode_tree_encoding tree_encoding
          in
          let inode_tree_encoding = unoptionize option_inode_tree_encoding in
          union
            [
              case
                ~title:"Value"
                (Tag 0)
                (obj1 (req "value" value_encoding))
                (function Some (Value v : tree) -> Some v | _ -> None)
                (fun v -> Some (Value v));
              case
                ~title:"Blinded_value"
                (Tag 1)
                (obj1 (req "blinded_value" hash_encoding))
                (function Some (Blinded_value hash) -> Some hash | _ -> None)
                (fun hash -> Some (Blinded_value hash));
              case
                ~title:"Node"
                (Tag 2)
                (obj1 (req "node" (list (tup2 step_encoding tree_encoding))))
                (function Some (Node sts : tree) -> Some sts | _ -> None)
                (fun sts -> Some (Node sts));
              case
                ~title:"Blinded_node"
                (Tag 3)
                (obj1 (req "blinded_node" hash_encoding))
                (function Some (Blinded_node hash) -> Some hash | _ -> None)
                (fun hash -> Some (Blinded_node hash));
              case
                ~title:"Inode"
                (Tag 4)
                (obj1 (req "inode" (inode_encoding option_inode_tree_encoding)))
                (function Some (Inode i : tree) -> Some i | _ -> None)
                (fun i -> Some (Inode i));
              case
                ~title:"Extender"
                (Tag 5)
                (obj1
                   (req
                      "extender"
                      (inode_extender_encoding inode_tree_encoding)))
                (function Some (Extender i) -> Some i | _ -> None)
                (fun i -> Some (Extender i));
              case
                ~title:"None"
                (Tag 6)
                (obj1 (req "none" null))
                (function None -> Some () | Some _ -> None)
                (fun () -> None);
            ])
    in
    let tree_encoding = unoptionize mu_option_tree_encoding in
    let inode_tree_encoding =
      unoptionize @@ mu_option_inode_tree_encoding tree_encoding
    in
    (inode_tree_encoding, tree_encoding)

  let kinded_hash_encoding =
    union
      [
        case
          ~title:"Value"
          (Tag 0)
          (obj1 (req "value" hash_encoding))
          (function `Value ch -> Some ch | _ -> None)
          (fun ch -> `Value ch);
        case
          ~title:"Node"
          (Tag 1)
          (obj1 (req "node" hash_encoding))
          (function `Node ch -> Some ch | _ -> None)
          (fun ch -> `Node ch);
      ]

  let elt_encoding =
    let open Stream in
    union
      [
        case
          ~title:"Value"
          (Tag 0)
          (obj1 (req "value" value_encoding))
          (function Value v -> Some v | _ -> None)
          (fun v -> Value v);
        case
          ~title:"Node"
          (Tag 1)
          (obj1 (req "node" (list (tup2 step_encoding kinded_hash_encoding))))
          (function Node sks -> Some sks | _ -> None)
          (fun sks -> Node sks);
        case
          ~title:"Inode"
          (Tag 2)
          (* This option wrapping increases the encoding size.
             But stream encoding is basically larger than proof encoding,
             so I temporarily won't mind this increment. *)
          (obj1 (req "inode" (inode_encoding (option hash_encoding))))
          (function Inode hinode -> Some hinode | _ -> None)
          (fun hinode -> Inode hinode);
        case
          ~title:"Inode_extender"
          (Tag 3)
          (obj1 (req "inode_extender" (inode_extender_encoding hash_encoding)))
          (function Inode_extender e -> Some e | _ -> None)
          (fun e -> Inode_extender e);
      ]

  let stream_encoding = conv List.of_seq List.to_seq (list elt_encoding)

  let encoding a =
    conv
      (fun {version; before; after; state} -> (version, before, after, state))
      (fun (version, before, after, state) -> {version; before; after; state})
    @@ obj4
         (req "version" int16)
         (req "before" kinded_hash_encoding)
         (req "after" kinded_hash_encoding)
         (req "state" a)

  let tree_proof_encoding = encoding tree_encoding

  let stream_proof_encoding = encoding stream_encoding
end

module Make_proof (Store : DB) = struct
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

      and to_inode_extender :
          type a b. (a -> b) -> a DB_proof.inode_extender -> b inode_extender =
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

      and of_inode_extender :
          type a b. (a -> b) -> a inode_extender -> b DB_proof.inode_extender =
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

    let of_proof f p =
      let before = Kinded_hash.to_context_hash (DB_proof.before p) in
      let after = Kinded_hash.to_context_hash (DB_proof.after p) in
      let state = f (DB_proof.state p) in
      {version = 0; before; after; state}

    let to_proof f p =
      let before = Kinded_hash.of_context_hash p.before in
      let after = Kinded_hash.of_context_hash p.after in
      let state = f p.state in
      DB_proof.v ~before ~after state

    let to_tree = of_proof State.to_tree

    let of_tree = to_proof State.of_tree

    let to_stream = of_proof State.to_stream

    let of_stream = to_proof State.of_stream
  end

  let produce_tree_proof repo key f =
    let open Lwt_syntax in
    let key =
      match key with `Node n -> `Node n | `Value v -> `Contents (v, ())
    in
    let+ (p, r) = Store.Tree.produce_proof repo key f in
    (Proof.to_tree p, r)

  let verify_tree_proof proof f =
    let proof = Proof.of_tree proof in
    Store.Tree.verify_proof proof f

  let produce_stream_proof repo key f =
    let open Lwt_syntax in
    let key =
      match key with `Node n -> `Node n | `Value v -> `Contents (v, ())
    in
    let+ (p, r) = Store.Tree.produce_stream repo key f in
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
