(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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

module type COMMITMENTS = sig
  val init : Raw_context.t -> Storage.Sapling.id -> Raw_context.t Lwt.t

  val default_root : Sapling.Hash.t

  val get_root :
    Raw_context.t ->
    Storage.Sapling.id ->
    (Raw_context.t * Sapling.Hash.t) tzresult Lwt.t

  val add :
    Raw_context.t ->
    Storage.Sapling.id ->
    Sapling.Commitment.t list ->
    int64 ->
    (Raw_context.t * int) tzresult Lwt.t

  val get_from :
    Raw_context.t ->
    Storage.Sapling.id ->
    int64 ->
    Sapling.Commitment.t list tzresult Lwt.t
end

module Commitments : COMMITMENTS = struct
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
   * The leaves are indexed by their position [pos], ranging from 0 to
   * (2^h)-1. The encoding of [pos] limits the possible size of the tree.
   * In any case the only valid height for the Sapling library is 32, so even
   * if the library encodes positions as uint64, they never exceed uint32.
   *
   * The tree is incremental in the sense that leaves cannot be modified but
   * only added and exclusively in successive positions.
   *
   * Given that elements are added and retrieved by position, it is possible
   * to use this information to efficiently navigate the tree.
   * Given a tree of height [h] and a position [pos], if pos < pow2 (h-1) only
   * the left subtree needs to be inspected recursively. Otherwise only the
   * right needs to be visited, decreasing [pos] by [pow2 (h-1)].
   *
   * In order to avoid storing the height for each subtree (or worse
   * recomputing it), each function with suffix `_height` expects the height
   * of the tree as parameter. These functions are only for internal use and
   * are later aliased by functions using the default height of a Sapling
   * incremental Merkle tree.
   *
   * Each node of the tree is indexed starting from the root at index 1,
   * followed by its left child at index 2, right child at index 3 and so on
   * until the last leaf at index 2^(depth+1)-1, or in terms of height
   * 2^(32 - height +1) -1.
   * The functions left and right return the index of the left and right child
   * of a node.
   *)

  let pow2 h = Int64.(shift_left 1L h)

  let max_height = 32

  let max_size = pow2 max_height

  let assert_node node height =
    assert (
      let first_of_height = pow2 (max_height - height) in
      let first_of_next_height = Int64.shift_left first_of_height 1 in
      Compare.Int64.(node >= first_of_height && node < first_of_next_height))

  let assert_height height =
    assert (Compare.Int.(height >= 0 && height <= max_height))

  let assert_pos pos height =
    assert (Compare.Int64.(pos >= 0L && pos <= pow2 height))

  let default_root = H.uncommitted ~height:max_height

  let init = Storage.Sapling.commitments_init

  let get_root_height ctx id node height =
    assert_node node height ;
    assert_height height ;
    Storage.Sapling.Commitments.find (ctx, id) node >|=? function
    | ctx, None ->
        let hash = H.uncommitted ~height in
        (ctx, hash)
    | ctx, Some hash -> (ctx, hash)

  let left node = Int64.mul node 2L

  let right node = Int64.(add (mul node 2L) 1L)

  (* Not tail-recursive *)
  let rec split_at n l =
    if Compare.Int64.(n = 0L) then ([], l)
    else
      match l with
      | [] -> ([], l)
      | x :: xs ->
          let l1, l2 = split_at Int64.(pred n) xs in
          (x :: l1, l2)

  (* [insert tree height pos cms] inserts the list of commitments
     [cms] in the tree [tree] of height [height] at the next position [pos].
     Returns the context, the size of the added storage, and the hash of the
     node. Not tail-recursive.
     Pre: incremental tree /\
          size tree + List.length cms <= pow2 height /\
          pos = size tree /\
     Post: incremental tree /\
           to_list (insert tree height pos cms) = to_list t @ cms *)
  let rec insert ctx id node height pos cms =
    assert_node node height ;
    assert_height height ;
    assert_pos pos height ;
    match (height, cms) with
    | _, [] ->
        get_root_height ctx id node height >|=? fun (ctx, h) -> (ctx, 0, h)
    | 0, [cm] ->
        let h = H.of_commitment cm in
        Storage.Sapling.Commitments.init (ctx, id) node h
        >|=? fun (ctx, size) -> (ctx, size, h)
    | _ ->
        let height = height - 1 in
        (if Compare.Int64.(pos < pow2 height) then
         let at = Int64.(sub (pow2 height) pos) in
         let cml, cmr = split_at at cms in
         insert ctx id (left node) height pos cml >>=? fun (ctx, size_l, hl) ->
         insert ctx id (right node) height 0L cmr >|=? fun (ctx, size_r, hr) ->
         (ctx, size_l + size_r, hl, hr)
        else
          get_root_height ctx id (left node) height >>=? fun (ctx, hl) ->
          let pos = Int64.(sub pos (pow2 height)) in
          insert ctx id (right node) height pos cms
          >|=? fun (ctx, size_r, hr) -> (ctx, size_r, hl, hr))
        >>=? fun (ctx, size_children, hl, hr) ->
        let h = H.merkle_hash ~height hl hr in
        Storage.Sapling.Commitments.add (ctx, id) node h
        >|=? fun (ctx, size, _existing) -> (ctx, size + size_children, h)

  let rec fold_from_height ctx id node ~pos ~f ~acc height =
    assert_node node height ;
    assert_height height ;
    assert_pos pos height ;
    Storage.Sapling.Commitments.find (ctx, id) node
    (* we don't count gas for this function, it is called only by RPC *)
    >>=?
    function
    | _ctx, None -> return acc
    | _ctx, Some h ->
        if Compare.Int.(height = 0) then return (f acc h)
        else
          let full = pow2 (height - 1) in
          if Compare.Int64.(pos < full) then
            fold_from_height ctx id (left node) ~pos ~f ~acc (height - 1)
            >>=? fun acc ->
            (* Setting pos to 0 folds on the whole right subtree *)
            fold_from_height ctx id (right node) ~pos:0L ~f ~acc (height - 1)
          else
            let pos = Int64.(sub pos full) in
            fold_from_height ctx id (right node) ~pos ~f ~acc (height - 1)

  let root_node = 1L

  let get_root ctx id = get_root_height ctx id root_node max_height

  (* Expects pos to be the next position to insert. Pos is also the number of
     inserted leaves.
     A commitment should always be added together with a corresponding
     ciphertext in the same position.
     [insert] is not tail-recursive so we put a hard limit on the size of the
     list of commitments. The use of [split_at] has O(n logn) complexity that is
     less relevant on a smaller list. *)
  let add ctx id cms pos =
    let l = List.length cms in
    assert (Compare.Int.(l <= 1000)) ;
    let n' = Int64.(add pos (of_int l)) in
    assert (Compare.Int64.(n' <= max_size)) ;
    insert ctx id root_node max_height pos cms >|=? fun (ctx, size, _h) ->
    (ctx, size)

  let get_from ctx id pos =
    fold_from_height
      ctx
      id
      root_node
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
      Storage.Sapling.Ciphertexts.find (ctx, id) pos >>=? fun (ctx, c) ->
      match c with
      | None -> return (ctx, List.rev acc)
      | Some c -> aux (ctx, c :: acc) (Int64.succ pos)
    in
    aux (ctx, []) offset
end

(* Collection of nullifiers w/o duplicates, append-only. It has a dual
   implementation with a hash map for constant `mem` and with a ordered set to
   retrieve by position. *)
module Nullifiers = struct
  let init = Storage.Sapling.nullifiers_init

  let size ctx id = Storage.Sapling.Nullifiers_size.get (ctx, id)

  let mem ctx id nf = Storage.Sapling.Nullifiers_hashed.mem (ctx, id) nf

  (* Allows for duplicates as they are already checked by verify_update before
     updating the state.
     Not tail-recursive so we put a hard limit on the size of the
     list of nullifiers. *)
  let add ctx id nfs =
    assert (Compare.Int.(List.compare_length_with nfs 1000 <= 0)) ;
    size ctx id >>=? fun nf_start_pos ->
    List.fold_right_es
      (fun nf (ctx, pos, acc_size) ->
        Storage.Sapling.Nullifiers_hashed.init (ctx, id) nf
        >>=? fun (ctx, size) ->
        Storage.Sapling.Nullifiers_ordered.init (ctx, id) pos nf >|=? fun ctx ->
        (ctx, Int64.succ pos, Z.add acc_size (Z.of_int size)))
      nfs
      (ctx, nf_start_pos, Z.zero)
    >>=? fun (ctx, nf_end_pos, size) ->
    Storage.Sapling.Nullifiers_size.update (ctx, id) nf_end_pos >|=? fun ctx ->
    (ctx, size)

  let get_from ctx id offset =
    let rec aux acc pos =
      Storage.Sapling.Nullifiers_ordered.find (ctx, id) pos >>=? function
      | None -> return @@ List.rev acc
      | Some c -> aux (c :: acc) (Int64.succ pos)
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
    Storage.Sapling.Roots_pos.get (ctx, id) >>=? fun pos ->
    Storage.Sapling.Roots.get (ctx, id) pos

  let init ctx id =
    let rec aux ctx pos =
      if Compare.Int32.(pos < 0l) then return ctx
      else
        Storage.Sapling.Roots.init (ctx, id) pos Commitments.default_root
        >>=? fun ctx -> aux ctx (Int32.pred pos)
    in
    aux ctx (Int32.pred size) >>=? fun ctx ->
    Storage.Sapling.Roots_pos.init (ctx, id) 0l >>=? fun ctx ->
    let level = (Raw_context.current_level ctx).level in
    Storage.Sapling.Roots_level.init (ctx, id) level

  let mem ctx id root =
    Storage.Sapling.Roots_pos.get (ctx, id) >>=? fun start_pos ->
    let rec aux pos =
      Storage.Sapling.Roots.get (ctx, id) pos >>=? fun hash ->
      if Compare.Int.(Sapling.Hash.compare hash root = 0) then return true
      else
        let pos = Int32.(pred pos) in
        let pos = if Compare.Int32.(pos < 0l) then Int32.pred size else pos in
        if Compare.Int32.(pos = start_pos) then return false else aux pos
    in
    aux start_pos

  (* allows duplicates *)
  let add ctx id root =
    Storage.Sapling.Roots_pos.get (ctx, id) >>=? fun pos ->
    let level = (Raw_context.current_level ctx).level in
    Storage.Sapling.Roots_level.get (ctx, id) >>=? fun stored_level ->
    if Raw_level_repr.(stored_level = level) then
      (* if there is another add during the same level, it will over-write on
         the same position *)
      Storage.Sapling.Roots.add (ctx, id) pos root >|= ok
    else
      (* it's the first add for this level *)
      (* TODO(samoht): why is it using [update] and not [init] then? *)
      Storage.Sapling.Roots_level.update (ctx, id) level >>=? fun ctx ->
      let pos = Int32.rem (Int32.succ pos) size in
      Storage.Sapling.Roots_pos.update (ctx, id) pos >>=? fun ctx ->
      Storage.Sapling.Roots.add (ctx, id) pos root >|= ok
end

(** This type links the permanent state stored in the context at the specified
    id together with the ephemeral diff managed by the Michelson
    interpreter. After a successful execution the diff can be applied to update
    the state at id. The first time a state is created its id is None, one will
    be assigned after the first application. *)
type state = {
  id : Lazy_storage_kind.Sapling_state.Id.t option;
  diff : Sapling_repr.diff;
  memo_size : Sapling_repr.Memo_size.t;
}

let empty_diff =
  Sapling_repr.{commitments_and_ciphertexts = []; nullifiers = []}

let empty_state ?id ~memo_size () = {id; diff = empty_diff; memo_size}

(** Returns a state from an existing id. *)
let state_from_id ctxt id =
  Storage.Sapling.Memo_size.get (ctxt, id) >|=? fun memo_size ->
  ({id = Some id; diff = empty_diff; memo_size}, ctxt)

let rpc_arg = Storage.Sapling.rpc_arg

let get_memo_size ctx id = Storage.Sapling.Memo_size.get (ctx, id)

let init ctx id ~memo_size =
  Storage.Sapling.Memo_size.add (ctx, id) memo_size >>= fun ctx ->
  Storage.Sapling.Commitments_size.add (ctx, id) Int64.zero >>= fun ctx ->
  Commitments.init ctx id >>= fun ctx ->
  Nullifiers.init ctx id >>= fun ctx ->
  Roots.init ctx id >>=? fun ctx -> Ciphertexts.init ctx id >|= ok

(* Gas costs for apply_diff. *)
let sapling_apply_diff_cost ~inputs ~outputs =
  let open Saturation_repr in
  add
    (safe_int 1_300_000)
    (add
       (scale_fast (mul_safe_of_int_exn 5_000) (safe_int inputs))
       (scale_fast (mul_safe_of_int_exn 55_000) (safe_int outputs)))

(** Applies a diff to a state id stored in the context. Updates Commitments,
    Ciphertexts and Nullifiers using the diff and updates the Roots using the
    new Commitments tree. *)
let apply_diff ctx id diff =
  let open Sapling_repr in
  let nb_commitments = List.length diff.commitments_and_ciphertexts in
  let nb_nullifiers = List.length diff.nullifiers in
  let sapling_cost =
    sapling_apply_diff_cost ~inputs:nb_nullifiers ~outputs:nb_commitments
  in
  Raw_context.consume_gas ctx sapling_cost >>?= fun ctx ->
  Storage.Sapling.Commitments_size.get (ctx, id) >>=? fun cm_start_pos ->
  let cms = List.rev_map fst diff.commitments_and_ciphertexts in
  Commitments.add ctx id cms cm_start_pos >>=? fun (ctx, size) ->
  Storage.Sapling.Commitments_size.update
    (ctx, id)
    (Int64.add cm_start_pos (Int64.of_int nb_commitments))
  >>=? fun ctx ->
  List.fold_right_es
    (fun (_cm, cp) (ctx, pos, acc_size) ->
      Ciphertexts.add ctx id cp pos >|=? fun (ctx, size) ->
      (ctx, Int64.succ pos, Z.add acc_size (Z.of_int size)))
    diff.commitments_and_ciphertexts
    (ctx, cm_start_pos, Z.of_int size)
  >>=? fun (ctx, _ct_end_pos, size) ->
  Nullifiers.add ctx id diff.nullifiers >>=? fun (ctx, size_nf) ->
  let size = Z.add size size_nf in
  match diff.commitments_and_ciphertexts with
  | [] ->
      (* avoids adding duplicates to Roots *)
      return (ctx, size)
  | _ :: _ ->
      Commitments.get_root ctx id >>=? fun (ctx, root) ->
      Roots.add ctx id root >|=? fun ctx -> (ctx, size)

let add {id; diff; memo_size} cm_cipher_list =
  assert (
    List.for_all
      (fun (_cm, cipher) ->
        Compare.Int.(Sapling.Ciphertext.get_memo_size cipher = memo_size))
      cm_cipher_list) ;
  {
    id;
    diff =
      {
        diff with
        commitments_and_ciphertexts =
          List.rev cm_cipher_list @ diff.commitments_and_ciphertexts;
      };
    memo_size;
  }

let root_mem ctx {id; _} tested_root =
  match id with
  | Some id -> Roots.mem ctx id tested_root
  | None ->
      return
        Compare.Int.(
          Sapling.Hash.compare tested_root Commitments.default_root = 0)

(* to avoid a double spend we need to check the disk AND the diff *)
let nullifiers_mem ctx {id; diff; _} nf =
  let exists_in_diff =
    List.exists
      (fun v -> Compare.Int.(Sapling.Nullifier.compare nf v = 0))
      diff.nullifiers
  in
  if exists_in_diff then return (ctx, true)
  else
    match id with
    | None -> return (ctx, false)
    | Some id -> Nullifiers.mem ctx id nf

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
    Commitments.get_from ctx id offset_commitment >>=? fun commitments ->
    Roots.get ctx id >>=? fun root ->
    Nullifiers.get_from ctx id offset_nullifier >>=? fun nullifiers ->
    Ciphertexts.get_from ctx id offset_commitment
    (* we don't count gas for RPCs *)
    >|=? fun (_ctx, ciphertexts) ->
    match List.combine ~when_different_lengths:() commitments ciphertexts with
    | Error () -> failwith "Invalid argument."
    | Ok commitments_and_ciphertexts ->
        (root, Sapling_repr.{commitments_and_ciphertexts; nullifiers})
