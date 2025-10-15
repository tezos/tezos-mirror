(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

open Kernel

type rule_set = {rule_patt : pattern; replacements : guarded_replacement list}

and guarded_replacement = {
  type_constraint : type_constraint;
  replacement : replacement list;
}

and type_constraint =
  | No_cnstrnt
  | Data_cnstrnt of {cnstrnt : Type.Base.t; fresh : int list}
  | Instr_cnstrnt of {
      cnstrnt : Inference.transformer;
      fresh : var list;
      fresh_stack : int list;
    }

and replacement =
  | Context_aware of (Mikhailsky.node -> Mikhailsky.node)
  | Context_blind of (unit -> Mikhailsky.node)

and pattern = Pattern of Patt.t | Root

and var = Plain of int | Cmp of int

let stack_repr = Inference.Stack_type None

let base_repr =
  Inference.Base_type {repr = None; comparable = Inference.Unconstrained}

let cmp_repr =
  Inference.Base_type {repr = None; comparable = Inference.Comparable}

let rec add_fresh_stack_variables vars =
  let open Inference.M in
  match vars with
  | [] -> return ()
  | fresh :: tl ->
      uf_lift (Uf.UF.add fresh) >>= fun () ->
      set_repr fresh stack_repr >>= fun () -> add_fresh_stack_variables tl

let rec add_fresh_data_variables vars =
  let open Inference.M in
  match vars with
  | [] -> return ()
  | fresh :: tl ->
      uf_lift (Uf.UF.add fresh) >>= fun () ->
      set_repr fresh base_repr >>= fun () -> add_fresh_data_variables tl

let rec add_fresh_variables vars plain_repr cmp_repr =
  let open Inference.M in
  match vars with
  | [] -> return ()
  | Plain fresh :: tl ->
      uf_lift (Uf.UF.add fresh) >>= fun () ->
      set_repr fresh plain_repr >>= fun () ->
      add_fresh_variables tl plain_repr cmp_repr
  | Cmp fresh :: tl ->
      uf_lift (Uf.UF.add fresh) >>= fun () ->
      set_repr fresh cmp_repr >>= fun () ->
      add_fresh_variables tl plain_repr cmp_repr

let evaluate_guard_monadic guard path =
  let open Inference.M in
  match guard with
  | No_cnstrnt -> return ()
  | Data_cnstrnt {cnstrnt = base_type_constraint; fresh} -> (
      add_fresh_data_variables fresh >>= fun () ->
      get_data_annot path >>= fun res_opt ->
      match res_opt with
      | None -> assert false
      | Some type_of_expr ->
          Inference.unify_base type_of_expr base_type_constraint >>= fun () ->
          Inference.instantiate_base type_of_expr >>= fun _ -> return ())
  | Instr_cnstrnt {cnstrnt = {bef = pre; aft = post}; fresh; fresh_stack} -> (
      (* Add base fresh type variables *)
      add_fresh_variables fresh base_repr cmp_repr
      >>= fun () ->
      add_fresh_stack_variables fresh_stack >>= fun () ->
      get_instr_annot path >>= fun res_opt ->
      match res_opt with
      | None -> assert false
      | Some {bef; aft} ->
          Inference.unify pre bef >>= fun () ->
          Inference.unify post aft >>= fun () ->
          Inference.instantiate bef >>= fun _bef ->
          Inference.instantiate aft >>= fun _aft -> return ())

let evaluate_guard typing guard path =
  try
    let _ = evaluate_guard_monadic guard path typing in
    true
  with Inference.Ill_typed_script _ -> false

let filter_matches typing guard matches =
  List.filter (evaluate_guard typing guard) matches

(* Provides a speedup but should better be done in the
   rewriting module (so that not only top matches are hash-consed). *)
let matches_with_hash_consing =
  let match_table : (int * int, Kernel.Path.t list) Hashtbl.t =
    Hashtbl.create 97
  in
  fun pattern term ->
    match pattern with
    | Root -> [Path.root]
    | Pattern patt -> (
        let key = (Kernel.Patt.uid patt, Mikhailsky.tag term) in
        match Hashtbl.find_opt match_table key with
        | None ->
            let res = Rewriter.all_matches patt term in
            Hashtbl.add match_table key res ;
            res
        | Some res -> res)

let matches_without_consing pattern term =
  match pattern with
  | Root -> [Path.root]
  | Pattern patt -> Rewriter.all_matches patt term

let rewriting (state : State_space.t) (rules : rule_set list) =
  List.fold_left
    (fun acc rule ->
      let matches = matches_without_consing rule.rule_patt state.term in
      List.fold_left
        (fun acc guarded_replacement ->
          let matches =
            filter_matches
              (Lazy.force state.typing)
              guarded_replacement.type_constraint
              matches
          in
          List.fold_left
            (fun acc replacement ->
              match replacement with
              | Context_blind term ->
                  List.fold_left
                    (fun acc path -> (path, term ()) :: acc)
                    acc
                    matches
              | Context_aware f ->
                  List.fold_left
                    (fun acc path ->
                      let term = Rewriter.get_subterm ~term:state.term ~path in
                      (path, f term) :: acc)
                    acc
                    matches)
            acc
            guarded_replacement.replacement)
        acc
        rule.replacements)
    []
    rules

module Instruction = struct
  (* ----------------------------------------------------------------------- *)
  (* Rule: replace instruction by hole. *)

  (* Matches instructions *)
  let match_any_instr =
    let open Patt in
    Pattern
      (focus
         (prim_pred
            (fun prim -> Mikhailsky_prim.kind prim = Mikhailsky_prim.Instr_kind)
            list_any))

  let replace_any_instr_by_hole =
    let replace_by_hole =
      {
        type_constraint = No_cnstrnt;
        replacement = [Context_blind (fun () -> Mikhailsky.instr_hole)];
      }
    in
    {rule_patt = match_any_instr; replacements = [replace_by_hole]}

  (* ----------------------------------------------------------------------- *)
  (* Rule: replace instruction hole by instruction satisfying typing
     constraints. *)

  (* Matches instruction holes *)
  let match_instr_hole =
    let open Patt in
    Pattern (focus (prim I_Hole list_any))

  let replacement ?(fresh = []) ?(fresh_stack = []) ~bef ~aft ~replacement () :
      guarded_replacement =
    {
      type_constraint = Instr_cnstrnt {cnstrnt = {bef; aft}; fresh; fresh_stack};
      replacement;
    }

  let instructions =
    let open Type in
    let module M = Mikhailsky in
    let module I = Inference in
    let alpha = ~-1 in
    let beta = ~-2 in
    let gamma = ~-3 in
    let delta = ~-4 in
    [
      replacement
        ~fresh_stack:[alpha]
        ~bef:(item bytes (stack_var alpha))
        ~aft:(item bytes (stack_var alpha))
        ~replacement:
          [
            Context_blind (fun () -> M.Instructions.blake2b);
            Context_blind (fun () -> M.Instructions.sha256);
            Context_blind (fun () -> M.Instructions.sha512);
          ]
        ();
      replacement
        ~fresh_stack:[alpha]
        ~bef:(item int (stack_var alpha))
        ~aft:(item bool (stack_var alpha))
        ~replacement:[Context_blind (fun () -> M.Instructions.gt)]
        ();
      replacement
        ~fresh_stack:[alpha]
        ~bef:(item int (stack_var alpha))
        ~aft:(item nat (stack_var alpha))
        ~replacement:[Context_blind (fun () -> M.Instructions.abs)]
        ();
      replacement
        ~fresh_stack:[alpha]
        ~bef:(item int (item int (stack_var alpha)))
        ~aft:(item int (stack_var alpha))
        ~replacement:
          [
            Context_blind (fun () -> M.Instructions.add M.int_ty M.int_ty);
            Context_blind (fun () -> M.Instructions.mul M.int_ty M.int_ty);
          ]
        ();
      replacement
        ~fresh:[Plain alpha; Plain beta]
        ~fresh_stack:[gamma]
        ~bef:(item (pair (var alpha) (var beta)) (stack_var gamma))
        ~aft:(item (var alpha) (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.car)]
        ();
      replacement
        ~fresh:[Plain alpha; Plain beta]
        ~fresh_stack:[gamma]
        ~bef:(item (pair (var alpha) (var beta)) (stack_var gamma))
        ~aft:(item (var beta) (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.cdr)]
        ();
      replacement
        ~fresh:[Cmp alpha]
        ~fresh_stack:[gamma]
        ~bef:(item (var alpha) (item (var alpha) (stack_var gamma)))
        ~aft:(item int (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.compare)]
        ();
      replacement
        ~fresh_stack:[gamma]
        ~bef:(item string (item string (stack_var gamma)))
        ~aft:(item string (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.concat)]
        ();
      replacement
        ~fresh:[Plain alpha]
        ~fresh_stack:[beta; gamma]
        ~bef:(item (var alpha) (stack_var beta))
        ~aft:(item (var alpha) (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.(dip hole))]
        ();
      replacement
        ~fresh:[Plain alpha]
        ~fresh_stack:[beta]
        ~bef:(item (var alpha) (stack_var beta))
        ~aft:(stack_var beta)
        ~replacement:[Context_blind (fun () -> M.Instructions.drop)]
        ();
      replacement
        ~fresh:[Plain alpha]
        ~fresh_stack:[beta]
        ~bef:(item (var alpha) (stack_var beta))
        ~aft:(item (var alpha) (item (var alpha) (stack_var beta)))
        ~replacement:[Context_blind (fun () -> M.Instructions.dup)]
        ();
      replacement
        ~fresh:[]
        ~fresh_stack:[alpha]
        ~bef:(stack_var alpha)
        ~aft:(item int (stack_var alpha))
        ~replacement:
          [
            (* TODO : push random integer? *)
            Context_blind
              (fun () -> M.Instructions.push M.int_ty (M.Data.integer 100));
          ]
        ();
      replacement
        ~fresh:[Plain alpha; Plain beta]
        ~fresh_stack:[gamma]
        ~bef:(item (var alpha) (item (var beta) (stack_var gamma)))
        ~aft:(item (var beta) (item (var alpha) (stack_var gamma)))
        ~replacement:[Context_blind (fun () -> M.Instructions.swap)]
        ();
      (* control *)
      replacement
        ~fresh_stack:[alpha]
        ~bef:(item bool (stack_var alpha))
        ~aft:(stack_var alpha)
        ~replacement:[Context_blind (fun () -> M.Instructions.(loop hole))]
        ();
      replacement
        ~fresh:[Plain alpha; Plain beta]
        ~fresh_stack:[gamma]
        ~bef:(item (or_ (var alpha) (var beta)) (stack_var gamma))
        ~aft:(item (var beta) (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.(loop_left hole))]
        ();
      replacement
        ~fresh:[Plain alpha]
        ~fresh_stack:[beta; gamma]
        ~bef:(item (option (var alpha)) (stack_var beta))
        ~aft:(stack_var gamma)
        ~replacement:
          [Context_blind (fun () -> M.Instructions.(if_none hole hole))]
        ();
      replacement
        ~fresh:[Plain alpha; Plain beta]
        ~fresh_stack:[gamma; delta]
        ~bef:(item (or_ (var alpha) (var beta)) (stack_var gamma))
        ~aft:(stack_var delta)
        ~replacement:
          [Context_blind (fun () -> M.Instructions.(if_left hole hole))]
        ();
      replacement
        ~fresh:[]
        ~fresh_stack:[alpha; beta]
        ~bef:(item bool (stack_var alpha))
        ~aft:(stack_var beta)
        ~replacement:[Context_blind (fun () -> M.Instructions.(if_ hole hole))]
        ();
      replacement
        ~fresh_stack:[alpha; beta]
        ~bef:(stack_var alpha)
        ~aft:(stack_var beta)
        ~replacement:
          [
            Context_blind
              (fun () -> M.seq [M.Instructions.hole; M.Instructions.hole]);
          ]
        ();
      replacement
        ~fresh:[Plain alpha; Plain beta]
        ~fresh_stack:[gamma]
        ~bef:(item (var alpha) (stack_var gamma))
        ~aft:(item (or_ (var alpha) (var beta)) (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.left)]
        ();
      replacement
        ~fresh:[Plain alpha; Plain beta]
        ~fresh_stack:[gamma]
        ~bef:(item (var beta) (stack_var gamma))
        ~aft:(item (or_ (var alpha) (var beta)) (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.right)]
        ();
      replacement
        ~fresh:[Plain alpha; Plain beta]
        ~fresh_stack:[gamma]
        ~bef:(stack_var gamma)
        ~aft:(item (lambda (var alpha) (var beta)) (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.(lambda [hole]))]
        ();
      replacement
        ~fresh:[Plain alpha; Plain beta]
        ~fresh_stack:[gamma]
        ~bef:(stack_var gamma)
        ~aft:(item (lambda (var alpha) (var beta)) (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.(lambda [hole]))]
        ();
      (* set/map/list*)
      replacement
        ~fresh:[Cmp alpha]
        ~fresh_stack:[gamma]
        ~bef:
          (item
             (var alpha)
             (item bool (item (set (var alpha)) (stack_var gamma))))
        ~aft:(item (set (var alpha)) (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.update_set)]
        ();
      replacement
        ~fresh:[Cmp alpha]
        ~fresh_stack:[gamma]
        ~bef:(stack_var gamma)
        ~aft:(item (set (var alpha)) (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.empty_set)]
        ();
      replacement
        ~fresh:[Cmp alpha]
        ~fresh_stack:[gamma]
        ~bef:(item (set (var alpha)) (stack_var gamma))
        ~aft:(stack_var gamma)
        ~replacement:
          [Context_blind (fun () -> M.Instructions.(iter_set [hole]))]
        ();
      replacement
        ~fresh:[Cmp alpha]
        ~fresh_stack:[gamma]
        ~bef:(item (var alpha) (item (set (var alpha)) (stack_var gamma)))
        ~aft:(item bool (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.mem_set)]
        ();
      replacement
        ~fresh:[Cmp alpha; Plain beta]
        ~fresh_stack:[gamma]
        ~bef:
          (item
             (var alpha)
             (item
                (option (var beta))
                (item (map (var alpha) (var beta)) (stack_var gamma))))
        ~aft:(item (map (var alpha) (var beta)) (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.update_map)]
        ();
      replacement
        ~fresh:[Cmp alpha; Plain beta]
        ~fresh_stack:[gamma]
        ~bef:(stack_var gamma)
        ~aft:(item (map (var alpha) (var beta)) (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.empty_map)]
        ();
      replacement
        ~fresh:[Cmp alpha; Plain beta]
        ~fresh_stack:[gamma]
        ~bef:(item (map (var alpha) (var beta)) (stack_var gamma))
        ~aft:(stack_var gamma)
        ~replacement:
          [Context_blind (fun () -> M.Instructions.(iter_map [hole]))]
        ();
      replacement
        ~fresh:[Cmp alpha; Plain beta; Plain delta]
        ~fresh_stack:[gamma]
        ~bef:(item (map (var alpha) (var beta)) (stack_var gamma))
        ~aft:(item (map (var alpha) (var delta)) (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.(map_map [hole]))]
        ();
      replacement
        ~fresh:[Cmp alpha; Plain beta]
        ~fresh_stack:[gamma]
        ~bef:
          (item
             (var alpha)
             (item (map (var alpha) (var beta)) (stack_var gamma)))
        ~aft:(item bool (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.mem_map)]
        ();
      replacement
        ~fresh:[Cmp alpha; Plain beta]
        ~fresh_stack:[gamma]
        ~bef:
          (item
             (var alpha)
             (item (map (var alpha) (var beta)) (stack_var gamma)))
        ~aft:(item (option (var beta)) (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.get_map)]
        ();
      (* lists *)
      replacement
        ~fresh:[Plain alpha]
        ~fresh_stack:[gamma]
        ~bef:(stack_var gamma)
        ~aft:(item (list (var alpha)) (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.nil)]
        ();
      replacement
        ~fresh:[Plain alpha]
        ~fresh_stack:[gamma]
        ~bef:(item (var alpha) (item (list (var alpha)) (stack_var gamma)))
        ~aft:(item (list (var alpha)) (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.cons)]
        ();
      replacement
        ~fresh:[Plain alpha]
        ~fresh_stack:[gamma]
        ~bef:(item (list (var alpha)) (stack_var gamma))
        ~aft:(stack_var gamma)
        ~replacement:
          [Context_blind (fun () -> M.Instructions.(iter_list [hole]))]
        ();
      replacement
        ~fresh:[Plain alpha; Plain beta]
        ~fresh_stack:[gamma]
        ~bef:(item (list (var alpha)) (stack_var gamma))
        ~aft:(item (list (var beta)) (stack_var gamma))
        ~replacement:
          [Context_blind (fun () -> M.Instructions.(map_list [hole]))]
        ();
      (* sizes *)
      replacement
        ~fresh:[Cmp alpha]
        ~fresh_stack:[gamma]
        ~bef:(item (set (var alpha)) (stack_var gamma))
        ~aft:(item nat (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.size_set)]
        ();
      replacement
        ~fresh:[Cmp alpha; Plain beta]
        ~fresh_stack:[gamma]
        ~bef:(item (map (var alpha) (var beta)) (stack_var gamma))
        ~aft:(item nat (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.size_map)]
        ();
      replacement
        ~fresh:[Plain alpha]
        ~fresh_stack:[gamma]
        ~bef:(item (list (var alpha)) (stack_var gamma))
        ~aft:(item nat (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.size_list)]
        ();
      replacement
        ~fresh:[]
        ~fresh_stack:[gamma]
        ~bef:(item string (stack_var gamma))
        ~aft:(item nat (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.size_string)]
        ();
      replacement
        ~fresh:[]
        ~fresh_stack:[gamma]
        ~bef:(item bytes (stack_var gamma))
        ~aft:(item nat (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.size_bytes)]
        ();
      (* pack/unpack *)
      replacement
        ~fresh:[Plain alpha]
        ~fresh_stack:[gamma]
        ~bef:(item (var alpha) (stack_var gamma))
        ~aft:(item bytes (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.pack)]
        ();
      replacement
        ~fresh:[Plain alpha]
        ~fresh_stack:[gamma]
        ~bef:(item bytes (stack_var gamma))
        ~aft:(item (option (var alpha)) (stack_var gamma))
        ~replacement:[Context_blind (fun () -> M.Instructions.unpack)]
        ();
    ]

  let rules =
    [
      replace_any_instr_by_hole;
      {rule_patt = match_instr_hole; replacements = instructions};
    ]
end

module Data_rewrite_leaves
    (Michelson_base : Michelson_samplers_base.S)
    (Crypto_samplers : Crypto_samplers.Finite_key_pool_S) =
struct
  let hole_patt =
    let open Patt in
    prim_pred (fun prim -> prim = D_Hole) list_empty

  (* Matches a data hole *)
  let match_hole =
    let open Patt in
    Pattern (focus hole_patt)

  (* Matches an integer literal *)
  let match_int =
    let open Patt in
    Pattern (focus (prim_pred (fun prim -> prim = A_Int) list_any))

  (* Matches a list literal *)
  let match_list =
    let open Patt in
    Pattern (focus (prim_pred (fun prim -> prim = A_List) list_any))

  (* Matches a set literal *)
  let match_set =
    let open Patt in
    Pattern (focus (prim_pred (fun prim -> prim = A_Set) list_any))

  (* Matches a map literal *)
  let match_map =
    let open Patt in
    Pattern (focus (prim_pred (fun prim -> prim = A_Map) list_any))

  (* Matches a timestamp literal *)
  let match_timestamp =
    let open Patt in
    Pattern (focus (prim_pred (fun prim -> prim = A_Timestamp) list_any))

  (* Matches a mutez literal *)
  let match_mutez =
    let open Patt in
    Pattern (focus (prim_pred (fun prim -> prim = A_Mutez) list_any))

  (* Matches a key_hash literal *)
  let match_key_hash =
    let open Patt in
    Pattern (focus (prim_pred (fun prim -> prim = A_Key_hash) list_any))

  let match_int_mutez_timestamp_key_hash_key_or_none =
    let open Patt in
    Pattern
      (focus
         (prim_pred
            (function
              | A_Int | A_Nat | A_Mutez | A_Timestamp | A_Key_hash | A_Key
              | D_None ->
                  true
              | _ -> false)
            list_any))

  (* Matches an empty list, set or map literal *)
  let match_empty_list_set_or_map =
    let open Patt in
    Pattern
      (focus
         (prim_pred
            (function A_List | A_Set | A_Map -> true | _ -> false)
            (list_cons (seq list_empty) list_empty)))

  (* Matches a pair containing two holes*)
  let match_empty_pair =
    let open Patt in
    Pattern
      (focus
         (prim_pred
            (fun prim -> prim = D_Pair)
            (list_cons hole_patt (list_cons hole_patt list_empty))))

  (* Match a Some, Left or Right containing a hole *)
  let match_empty_some_left_or_right =
    let open Patt in
    Pattern
      (focus
         (prim_pred
            (function D_Left | D_Right | D_Some -> true | _ -> false)
            (list_cons hole_patt list_empty)))

  (* Match a None constructor *)
  let match_none =
    let open Patt in
    Pattern (focus (prim_pred (fun prim -> prim = D_None) list_empty))

  (* rules *)

  (* fresh type variables *)
  let alpha, beta = (-1, -2)

  let replacement ~fresh ~typ ~replacement =
    {
      type_constraint = Data_cnstrnt {cnstrnt = typ; fresh};
      replacement = [Context_blind (fun () -> replacement)];
    }

  let replacement_gen ~fresh ~typ ~replacement =
    {
      type_constraint = Data_cnstrnt {cnstrnt = typ; fresh};
      replacement = [Context_blind replacement];
    }

  let fill_in_hole rng_state =
    let replace_by_singleton_list =
      replacement
        ~fresh:[alpha]
        ~typ:Type.(list (var alpha))
        ~replacement:Mikhailsky.Data.(list [hole])
    in
    let replace_by_empty_pair =
      replacement
        ~fresh:[alpha; beta]
        ~typ:Type.(pair (var alpha) (var beta))
        ~replacement:Mikhailsky.Data.(pair hole hole)
    in
    let replace_by_singleton_set =
      replacement
        ~fresh:[alpha]
        ~typ:Type.(set (var alpha))
        ~replacement:Mikhailsky.Data.(set [hole])
    in
    let replace_by_singleton_map =
      replacement
        ~fresh:[alpha; beta]
        ~typ:Type.(map (var alpha) (var beta))
        ~replacement:Mikhailsky.Data.(map [map_elt hole hole])
    in
    let replace_by_random_int rng_state =
      let type_constraint = Data_cnstrnt {cnstrnt = Type.int; fresh = []} in
      let replacement =
        Context_blind
          (fun () ->
            Mikhailsky.Data.big_integer
              (Protocol.Script_int.to_zint (Michelson_base.int rng_state)))
      in
      {type_constraint; replacement = [replacement]}
    in
    let replace_by_left =
      replacement
        ~fresh:[alpha; beta]
        ~typ:Type.(or_ (var alpha) (var beta))
        ~replacement:Mikhailsky.Data.(left hole)
    in
    let replace_by_right =
      replacement
        ~fresh:[alpha; beta]
        ~typ:Type.(or_ (var alpha) (var beta))
        ~replacement:Mikhailsky.Data.(right hole)
    in
    let replace_by_some =
      replacement
        ~fresh:[alpha]
        ~typ:Type.(option (var alpha))
        ~replacement:Mikhailsky.Data.(some hole)
    in
    let replace_by_none =
      replacement
        ~fresh:[alpha]
        ~typ:Type.(option (var alpha))
        ~replacement:Mikhailsky.Data.none
    in
    let replace_by_mutez rng_state =
      replacement_gen ~fresh:[] ~typ:Type.mutez ~replacement:(fun () ->
          Mikhailsky.Data.mutez (Michelson_base.tez rng_state))
    in
    let replace_by_key_hash rng_state =
      replacement_gen ~fresh:[] ~typ:Type.key_hash ~replacement:(fun () ->
          Mikhailsky.Data.key_hash (Crypto_samplers.pkh rng_state))
    in
    let replace_by_key rng_state =
      replacement_gen ~fresh:[] ~typ:Type.key ~replacement:(fun () ->
          Mikhailsky.Data.key (Crypto_samplers.pk rng_state))
    in
    {
      rule_patt = match_hole;
      replacements =
        [
          replace_by_singleton_list;
          replace_by_empty_pair;
          replace_by_singleton_set;
          replace_by_singleton_map;
          replace_by_random_int rng_state;
          replace_by_left;
          replace_by_right;
          replace_by_some;
          replace_by_none;
          replace_by_mutez rng_state;
          replace_by_key_hash rng_state;
          replace_by_key rng_state;
        ];
    }

  let kill_empty_pair =
    {
      rule_patt = match_empty_pair;
      replacements =
        [
          {
            type_constraint = No_cnstrnt;
            replacement = [Context_blind (fun () -> Mikhailsky.Data.hole)];
          };
        ];
    }

  let kill_int_mutez_timestamp_key_hash_none =
    {
      rule_patt = match_int_mutez_timestamp_key_hash_key_or_none;
      replacements =
        [
          {
            type_constraint = No_cnstrnt;
            replacement = [Context_blind (fun () -> Mikhailsky.Data.hole)];
          };
        ];
    }

  let kill_empty_list_set_or_map =
    {
      rule_patt = match_empty_list_set_or_map;
      replacements =
        [
          {
            type_constraint = No_cnstrnt;
            replacement = [Context_blind (fun () -> Mikhailsky.Data.hole)];
          };
        ];
    }

  let kill_empty_some_left_or_right =
    {
      rule_patt = match_empty_some_left_or_right;
      replacements =
        [
          {
            type_constraint = No_cnstrnt;
            replacement = [Context_blind (fun () -> Mikhailsky.Data.hole)];
          };
        ];
    }

  let modify_set =
    let grow_ungrow_set =
      {
        type_constraint = No_cnstrnt;
        replacement =
          [
            Context_aware
              (fun set ->
                match set with
                | Micheline.Prim (_, A_Set, [Micheline.Seq (_, elements)], _) ->
                    Mikhailsky.Data.(set (hole :: elements))
                | _ -> assert false);
            Context_aware
              (fun set ->
                match set with
                | Micheline.Prim (_, A_Set, [Micheline.Seq (_, elements)], _)
                  -> (
                    match elements with
                    | [] -> Mikhailsky.Data.hole
                    | _ :: tl -> Mikhailsky.Data.set tl)
                | _ -> assert false);
          ];
      }
    in
    {rule_patt = match_set; replacements = [grow_ungrow_set]}

  let modify_map =
    let grow_ungrow_map =
      {
        type_constraint = No_cnstrnt;
        replacement =
          [
            Context_aware
              (fun set ->
                match set with
                | Micheline.Prim (_, A_Map, [Micheline.Seq (_, elements)], _) ->
                    Mikhailsky.Data.(map (map_elt hole hole :: elements))
                | _ -> assert false);
            Context_aware
              (fun set ->
                match set with
                | Micheline.Prim (_, A_Map, [Micheline.Seq (_, elements)], _)
                  -> (
                    match elements with
                    | [] -> Mikhailsky.Data.hole
                    | _ :: tl -> Mikhailsky.Data.map tl)
                | _ -> assert false);
          ];
      }
    in
    {rule_patt = match_map; replacements = [grow_ungrow_map]}

  let modify_list =
    let grow_ungrow_list =
      {
        type_constraint = No_cnstrnt;
        replacement =
          [
            Context_aware
              (fun list ->
                match list with
                | Micheline.Prim (_, A_List, [Micheline.Seq (_, terms)], _) ->
                    Mikhailsky.Data.(list (hole :: terms))
                | _ -> assert false);
            Context_aware
              (fun list ->
                match list with
                | Micheline.Prim (_, A_List, [Micheline.Seq (_, terms)], _) -> (
                    match terms with
                    | [] -> Mikhailsky.Data.hole
                    | _ :: tl -> Mikhailsky.Data.list tl)
                | _ -> assert false);
          ];
      }
    in
    {rule_patt = match_list; replacements = [grow_ungrow_list]}

  let rules rng_state =
    [
      fill_in_hole rng_state;
      kill_empty_pair;
      kill_empty_list_set_or_map;
      kill_empty_some_left_or_right;
      kill_int_mutez_timestamp_key_hash_none;
      modify_list;
      modify_set;
      modify_map;
    ]
end

module Data
    (Michelson_base : Michelson_samplers_base.S)
    (Crypto_samplers : Crypto_samplers.Finite_key_pool_S) =
struct
  let match_data_node =
    let open Patt in
    Pattern
      (focus
         (prim_pred
            (function
              | Mikhailsky_prim.D_Elt | D_Hole -> false
              | D_False | D_Left | D_None | D_Pair | D_Right | D_Some | D_True
              | D_Unit | A_Int | A_Nat | A_Set | A_List | A_Map | A_Key_hash
              | A_Mutez | A_Timestamp | A_Key ->
                  true
              | _ -> false)
            list_any))

  let match_list =
    let open Patt in
    Pattern (focus (prim_pred (fun prim -> prim = A_List) list_any))

  let match_data_hole =
    let open Patt in
    Pattern (focus (prim_pred (fun prim -> prim = D_Hole) list_any))

  let replace_by_hole =
    let replace_by_hole =
      {
        type_constraint = No_cnstrnt;
        replacement = [Context_blind (fun () -> Mikhailsky.Data.hole)];
      }
    in
    {rule_patt = match_data_node; replacements = [replace_by_hole]}

  let pack_root =
    let replacement =
      [
        Context_aware (fun node -> Mikhailsky.Data.list [node]);
        Context_aware (fun node -> Mikhailsky.Data.(pair node hole));
        Context_aware (fun node -> Mikhailsky.Data.(pair hole node));
      ]
    in
    let guarded_replacements = [{type_constraint = No_cnstrnt; replacement}] in
    {rule_patt = Root; replacements = guarded_replacements}

  module Data_rewrite_leaves_rules =
    Data_rewrite_leaves (Michelson_base) (Crypto_samplers)

  let rules rng_state =
    [
      Data_rewrite_leaves_rules.fill_in_hole rng_state;
      replace_by_hole;
      Data_rewrite_leaves_rules.modify_list;
      Data_rewrite_leaves_rules.modify_map;
      Data_rewrite_leaves_rules.modify_set;
    ]
end
