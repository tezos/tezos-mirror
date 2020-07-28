(* The MIT License (MIT)
 *
 * Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE. *)

module Commitments : sig
  val init : Raw_context.t -> Storage.Sapling.id -> Raw_context.t Lwt.t

  val default_root : Sapling.Hash.t

  val get_root :
    Raw_context.t ->
    Storage.Sapling.id ->
    (Raw_context.t * Sapling.Hash.t) tzresult Lwt.t

  val add :
    Raw_context.t ->
    Storage.Sapling.id ->
    Sapling.Commitment.t ->
    int64 ->
    (Raw_context.t * int) tzresult Lwt.t

  val get_from :
    Raw_context.t ->
    Storage.Sapling.id ->
    int64 ->
    Sapling.Commitment.t list tzresult Lwt.t
end = struct
  module H = Sapling.Hash

  (** Incremental Merkle Tree
   *
   * A tree of height h contains 2^h leaves and h+1 levels of nodes with
   * leaves at level 0 and root at level h.
   *
   * The leaves are commitments and the tree it is treated as always filled
   * with a default value H.uncommitted. This allows to have proofs of
   * membership, or witnesses, of fixed size.
   *
   * All the nodes at the same level of an empty tree have the same hash,
   * which can be computed from the default value of the leaves. This is
   * stored in the [uncommitted] list.
   *
   * Any subtree filled with default values is represented by the Empty
   * constructor and given its height it's possible to compute its hash
   * using the [uncommitted] list.
   *
   * The leaves are indexed by their position [pos], ranging from 0 to 2^h.
   * The encoding of [pos] limits the possible size of the tree. In any
   * case the only valid height for the Sapling library is 32, so even if
   * the library encodes positions as uint64, they never exceed uint32.
   * Because support for unsigned integers is only added in OCaml 4.08,
   * this implementation uses int64.
   *
   * New leaves can be added exclusively in successive order, for this
   * reason the type t contains the position of the next position to fill.
   *
   * Given that elements are added and retrieved by position, it is
   * possible to use the binary representation of the position as an
   * indication of the path to follow in the binary tree. The predicate
   * [go_left pos h] returns true if the element at position [pos] is on the
   * left subtree of tree of height [h].
   *
   * Each node of the tree is indexed starting from the root at index 1,
   * followed by its left child at index 2, right child at index 3 and so on
   * until the last leaf at index 2^(depth+1)-1, or in terms of height
   * 2^(32 - height +1) -1.
   * The functions left and right return the index of the left and right child
   * of a node.
   *)

  let max_height = 32

  let assert_node node height =
    assert (
      let first_of_height = Int64.shift_left 1L (max_height - height) in
      let first_of_next_height = Int64.shift_left first_of_height 1 in
      Compare.Int64.(node >= first_of_height && node < first_of_next_height) )

  let assert_height height =
    assert (Compare.Int.(height >= 0 && height <= max_height))

  let default_root = H.uncommitted max_height

  let init = Storage.Sapling.commitments_init

  let get_root_height ctx id node height =
    assert_height height ;
    Storage.Sapling.Commitments.get_option (ctx, id) node
    >|=? function
    | (ctx, None) ->
        let hash = H.uncommitted height in
        (ctx, hash)
    | (ctx, Some hash) ->
        (ctx, hash)

  let left node = Int64.mul node 2L

  let right node = Int64.(add (mul node 2L) 1L)

  let go_left pos height =
    let open Int64 in
    let mask = shift_left 1L (height - 1) in
    equal (logand pos mask) 0L

  let rec insert ctx id node cm height pos =
    assert_node node height ;
    assert_height height ;
    assert (Sapling.Commitment.valid_position pos) ;
    let update_hash ctx =
      get_root_height ctx id (left node) (height - 1)
      >>=? fun (ctx, hash_left) ->
      get_root_height ctx id (right node) (height - 1)
      >|=? fun (ctx, hash_right) ->
      let h = H.merkle_hash ~height:(height - 1) hash_left hash_right in
      (ctx, h)
    in
    Storage.Sapling.Commitments.get_option (ctx, id) node
    >>=? function
    | (ctx, None) ->
        if Compare.Int.(height = 0) then
          Storage.Sapling.Commitments.init (ctx, id) node (H.of_commitment cm)
        else
          insert ctx id (left node) cm (height - 1) pos
          >>=? fun (ctx, size_insert) ->
          update_hash ctx
          >>=? fun (ctx, h) ->
          Storage.Sapling.Commitments.init (ctx, id) node h
          >|=? fun (ctx, size) -> (ctx, size + size_insert)
    | (ctx, Some _) ->
        assert (Compare.Int.(height > 0)) ;
        if go_left pos height then
          insert ctx id (left node) cm (height - 1) pos
          >>=? fun (ctx, size_insert) ->
          update_hash ctx
          >>=? fun (ctx, h) ->
          Storage.Sapling.Commitments.set (ctx, id) node h
          >|=? fun (ctx, size) -> (ctx, size + size_insert)
        else
          insert ctx id (right node) cm (height - 1) pos
          >>=? fun (ctx, size_insert) ->
          update_hash ctx
          >>=? fun (ctx, h) ->
          Storage.Sapling.Commitments.set (ctx, id) node h
          >|=? fun (ctx, size) -> (ctx, size + size_insert)

  let rec fold_from_height ctx id node ~pos ~f ~acc height =
    assert_node node height ;
    assert_height height ;
    assert (Sapling.Commitment.valid_position pos) ;
    Storage.Sapling.Commitments.get_option (ctx, id) node
    (* we don't count gas for this function, it is called only by RPC *)
    >>=? function
    | (_ctx, None) ->
        return acc
    | (_ctx, Some h) ->
        if Compare.Int.(height = 0) then return (f acc h)
        else if go_left pos height then
          fold_from_height ctx id (left node) ~pos ~f ~acc (height - 1)
          >>=? fun acc ->
          (* Setting pos to 0 folds on the whole right subtree *)
          fold_from_height ctx id (right node) ~pos:0L ~f ~acc (height - 1)
        else fold_from_height ctx id (right node) ~pos ~f ~acc (height - 1)

  let get_root ctx id = get_root_height ctx id 1L max_height

  (* a commitment should always be added together with a corresponding
     ciphertext in the same position *)
  let add ctx id cm pos = insert ctx id 1L cm max_height pos

  let get_from ctx id pos =
    fold_from_height
      ctx
      id
      1L
      ~pos
      ~f:(fun acc c -> H.to_commitment c :: acc)
      ~acc:[]
      max_height
    >|=? fun l -> List.rev l
end

module Ciphertexts = struct
  let init ctx id = Storage.Sapling.ciphertexts_init ctx id

  (* a ciphertext should always be added together with a corresponding
     commitment in the same position *)
  let add ctx id c pos = Storage.Sapling.Ciphertexts.init (ctx, id) pos c

  let get_from ctx id offset =
    let rec aux (ctx, acc) pos =
      Storage.Sapling.Ciphertexts.get_option (ctx, id) pos
      >>=? fun (ctx, c) ->
      match c with
      | None ->
          return (ctx, List.rev acc)
      | Some c ->
          aux (ctx, c :: acc) (Int64.succ pos)
    in
    aux (ctx, []) offset
end

(* Collection of nullifiers w/o duplicates, append-only. It has a dual
   implementation with a hash map for constant `mem` and with a ordered set to
   retrieve by position.  *)
module Nullifiers = struct
  let init = Storage.Sapling.nullifiers_init

  let size ctx id = Storage.Sapling.Nullifiers_size.get (ctx, id)

  let mem ctx id nf = Storage.Sapling.Nullifiers_hashed.mem (ctx, id) nf

  (* Allows for duplicates as they are already checked by verify_update before
     updating the state. *)
  let add_list ctx id nfs =
    size ctx id
    >>=? fun nf_start_pos ->
    fold_right_s
      (fun nf (ctx, pos, acc_size) ->
        Storage.Sapling.Nullifiers_hashed.init (ctx, id) nf
        >>=? fun (ctx, size) ->
        Storage.Sapling.Nullifiers_ordered.init (ctx, id) pos nf
        >|=? fun ctx -> (ctx, Int64.succ pos, Z.(add acc_size (of_int size))))
      nfs
      (ctx, nf_start_pos, Z.zero)
    >>=? fun (ctx, nf_end_pos, size) ->
    Storage.Sapling.Nullifiers_size.set (ctx, id) nf_end_pos
    >|=? fun ctx -> (ctx, size)

  let get_from ctx id offset =
    let rec aux acc pos =
      Storage.Sapling.Nullifiers_ordered.get_option (ctx, id) pos
      >>=? function
      | None ->
          return @@ List.rev acc
      | Some c ->
          aux (c :: acc) (Int64.succ pos)
    in
    aux [] offset
end

(** Bounded queue of roots. The full size is initialized with the default
    uncommitted root, that's why roots storage doesn't need to be carbonated.
    A maximum of one new root is added per protocol level.
    If multiple transactions for the same shielded pool are processed during the
    same contract call or several calls in the same block, only the last root
    will be stored.
    This property prevents transactions in the same block from depending on each
    other and guarantees that a transaction will be valid for a least two hours
    (hence the 120 size) after being forged. *)
module Roots = struct
  let size = 120l

  (* pos is the index of the last inserted element *)

  let get ctx id =
    Storage.Sapling.Roots_pos.get (ctx, id)
    >>=? fun pos -> Storage.Sapling.Roots.get (ctx, id) pos

  let init ctx id =
    let rec aux ctx pos =
      if Compare.Int32.(pos < 0l) then return ctx
      else
        Storage.Sapling.Roots.init (ctx, id) pos Commitments.default_root
        >>=? fun ctx -> aux ctx (Int32.pred pos)
    in
    aux ctx (Int32.pred size)
    >>=? fun ctx ->
    Storage.Sapling.Roots_pos.init (ctx, id) 0l
    >>=? fun ctx ->
    let level = (Raw_context.current_level ctx).level in
    Storage.Sapling.Roots_level.init (ctx, id) level

  let mem ctx id root =
    Storage.Sapling.Roots_pos.get (ctx, id)
    >>=? fun start_pos ->
    let rec aux pos =
      Storage.Sapling.Roots.get (ctx, id) pos
      >>=? fun hash ->
      if Compare.Int.(Sapling.Hash.compare hash root = 0) then return true
      else
        let pos = Int32.(pred pos) in
        let pos = if Compare.Int32.(pos < 0l) then Int32.pred size else pos in
        if Compare.Int32.(pos = start_pos) then return false else aux pos
    in
    aux start_pos

  (* allows duplicates *)
  let add ctx id root =
    Storage.Sapling.Roots_pos.get (ctx, id)
    >>=? fun pos ->
    let level = (Raw_context.current_level ctx).level in
    Storage.Sapling.Roots_level.get (ctx, id)
    >>=? fun stored_level ->
    if Raw_level_repr.(stored_level = level) then
      (* if there is another add during the same level, it will over-write on
         the same position  *)
      Storage.Sapling.Roots.init_set (ctx, id) pos root >|= ok
    else
      (* it's the first add for this level *)
      Storage.Sapling.Roots_level.set (ctx, id) level
      >>=? fun ctx ->
      let pos = Int32.rem (Int32.succ pos) size in
      Storage.Sapling.Roots_pos.set (ctx, id) pos
      >>=? fun ctx -> Storage.Sapling.Roots.init_set (ctx, id) pos root >|= ok
end

(** This type links the permanent state stored in the context at the specified
    id together with the ephemeral diff managed by the Michelson
    interpreter. After a successful execution the diff can be applied to update
    the state at id. The first time a state is created its id is None, one will
    be assigned after the first application. *)
type state = {
  id : Lazy_storage_kind.Sapling_state.Id.t option;
  diff : Sapling_repr.diff;
  memo_size : int;
}

let empty_diff =
  Sapling_repr.{commitments = []; ciphertexts = []; nullifiers = []}

let empty_state ?id ?(diff = empty_diff) ~memo_size () = {id; diff; memo_size}

(** Returns a state from an existing id. *)
let state_from_id ctxt ?(diff = empty_diff) id =
  Storage.Sapling.Memo_size.get (ctxt, id)
  >|=? fun memo_size -> ({id = Some id; diff; memo_size}, ctxt)

let rpc_arg = Storage.Sapling.rpc_arg

let get_memo_size ctx id = Storage.Sapling.Memo_size.get (ctx, id)

let init ctx id ~memo_size =
  Storage.Sapling.Memo_size.init_set (ctx, id) memo_size
  >>= fun ctx ->
  Storage.Sapling.Commitments_size.init_set (ctx, id) Int64.zero
  >>= fun ctx ->
  Commitments.init ctx id
  >>= fun ctx ->
  Nullifiers.init ctx id
  >>= fun ctx ->
  Roots.init ctx id >>=? fun ctx -> Ciphertexts.init ctx id >|= ok

(** Applies a diff to a state id stored in the context. Updates Commitments,
    Ciphertexts and Nullifiers using the diff and updates the Roots using the
    new Commitments tree. *)
let apply_diff ctx id diff =
  let open Sapling_repr in
  Storage.Sapling.Commitments_size.get (ctx, id)
  >>=? fun cm_start_pos ->
  fold_right_s
    (fun cm (ctx, pos, acc_size) ->
      Commitments.add ctx id cm pos
      >|=? fun (ctx, size) ->
      (ctx, Int64.succ pos, Z.add acc_size (Z.of_int size)))
    diff.commitments
    (ctx, cm_start_pos, Z.zero)
  >>=? fun (ctx, cm_end_pos, size) ->
  Storage.Sapling.Commitments_size.set (ctx, id) cm_end_pos
  >>=? fun ctx ->
  fold_right_s
    (fun cp (ctx, pos, acc_size) ->
      Ciphertexts.add ctx id cp pos
      >|=? fun (ctx, size) ->
      (ctx, Int64.succ pos, Z.add acc_size (Z.of_int size)))
    diff.ciphertexts
    (ctx, cm_start_pos, size)
  >>=? fun (ctx, ct_end_pos, size) ->
  assert (Compare.Int64.(cm_end_pos = ct_end_pos)) ;
  Nullifiers.add_list ctx id diff.nullifiers
  >>=? fun (ctx, size_nf) ->
  let size = Z.add size size_nf in
  match diff.commitments with
  | [] ->
      (* avoids adding dulicates to Roots *)
      return (ctx, size)
  | _ ->
      Commitments.get_root ctx id
      >>=? fun (ctx, root) -> Roots.add ctx id root >|=? fun ctx -> (ctx, size)

let add {id; diff; memo_size} cm cipher =
  assert (Compare.Int.(Sapling.Ciphertext.get_memo_size cipher = memo_size)) ;
  {
    id;
    diff =
      {
        diff with
        commitments = cm :: diff.commitments;
        ciphertexts = cipher :: diff.ciphertexts;
      };
    memo_size;
  }

let root_mem ctx {id} tested_root =
  match id with
  | Some id ->
      Roots.mem ctx id tested_root
  | None ->
      return
        Compare.Int.(
          Sapling.Hash.compare tested_root Commitments.default_root = 0)

(* to avoid a double spend we need to check the disk AND the diff *)
let nullifiers_mem ctx {id; diff} nf =
  let exists_in_diff =
    List.exists
      (fun v -> Compare.Int.(Sapling.Nullifier.compare nf v = 0))
      diff.nullifiers
  in
  if exists_in_diff then return (ctx, true)
  else
    match id with
    | None ->
        return (ctx, false)
    | Some id ->
        Nullifiers.mem ctx id nf

(* Allows for duplicates as they are already checked by verify_update before
   updating the state. *)
let nullifiers_add {id; diff; memo_size} nf =
  {id; diff = {diff with nullifiers = nf :: diff.nullifiers}; memo_size}

type root = Sapling.Hash.t

let root_encoding = Sapling.Hash.encoding

let get_diff ctx id ?(offset_commitment = 0L) ?(offset_nullifier = 0L) () =
  if
    not
      Sapling.Commitment.(
        valid_position offset_commitment && valid_position offset_nullifier)
  then failwith "Invalid argument."
  else
    Commitments.get_from ctx id offset_commitment
    >>=? fun commitments ->
    Roots.get ctx id
    >>=? fun root ->
    Nullifiers.get_from ctx id offset_nullifier
    >>=? fun nullifiers ->
    Ciphertexts.get_from ctx id offset_commitment
    (* we don't count gas for RPCs *)
    >|=? fun (_ctx, ciphertexts) ->
    (root, Sapling_repr.{commitments; ciphertexts; nullifiers})
