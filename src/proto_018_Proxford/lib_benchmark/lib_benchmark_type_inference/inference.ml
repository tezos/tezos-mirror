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

open Micheline
module UF = Uf.UF

(* The domain of comparability:
 *
 *            Comparable      Not_comparable
 *                   ^         ^
 *                    \       /
 *                     \     /
 *                     Unconstrained
 *
 * The higher we go, the more information we have.
 * This domain admits all glbs but not all lubs.
 *)

type comparability = Comparable | Not_comparable | Unconstrained

let pp_comparability fmtr (cmp : comparability) =
  match cmp with
  | Comparable -> Format.fprintf fmtr "Comparable"
  | Not_comparable -> Format.fprintf fmtr "Not_comparable"
  | Unconstrained -> Format.fprintf fmtr "Unconstrained"

let sup_comparability (c1 : comparability) (c2 : comparability) =
  match (c1, c2) with
  | Unconstrained, c | c, Unconstrained -> Some c
  | Comparable, Comparable -> Some Comparable
  | Not_comparable, Not_comparable -> Some Not_comparable
  | Comparable, Not_comparable | Not_comparable, Comparable -> None

type michelson_type =
  | Base_type of {repr : Type.Base.t option; comparable : comparability}
  | Stack_type of Type.Stack.t option

type transformer = {bef : Type.Stack.t; aft : Type.Stack.t}

let michelson_type_to_string (x : michelson_type) =
  match x with
  | Base_type {repr = None; comparable} ->
      Format.asprintf "?::[%a]" pp_comparability comparable
  | Base_type {repr = Some ty; comparable} ->
      Format.asprintf "%a::[%a]" Type.Base.pp ty pp_comparability comparable
  | Stack_type None -> "<?>"
  | Stack_type (Some sty) -> Format.asprintf "%a" Type.Stack.pp sty

(* ------------------------------------------------------------------------- *)
(* Typechecking errors *)

type inference_error =
  (* | Expected_data_with_ground_type of Mikhailsky.Path.t * Mikhailsky.node *)
  | Unhandled_micheline of Mikhailsky.Path.t * Mikhailsky.node
  | Expected_micheline_prim
  | Unsatisfiable_comparability_constraint of comparability_error_witness
  | Base_types_incompatible of Type.Base.t * Type.Base.t
  | Stack_types_incompatible of Type.Stack.t * Type.Stack.t
  | Badly_typed_arithmetic of Mikhailsky_prim.prim * Type.Base.t * Type.Base.t
  | Ill_formed_arithmetic of Mikhailsky.Path.t * Mikhailsky.node
  | Cyclic_stack_type
  | Cyclic_base_type
  | Invalid_ast of string option * Mikhailsky.Path.t * Mikhailsky.node

and comparability_error_witness =
  | Comparability_error_types of michelson_type * michelson_type
  | Comparability_error_tags of Type.Base.t * comparability * comparability

let pp_inference_error fmtr (err : inference_error) =
  match err with
  (* | Expected_data_with_ground_type (path, node) ->
   *     let path = Mikhailsky.Path.to_string path in
   *     let node = Mikhailsky.to_string node in
   *     Format.fprintf fmtr "Expected data with ground type: %s at path %s" node path *)
  | Unhandled_micheline (path, node) ->
      let path = Mikhailsky.Path.to_string path in
      let node = Mikhailsky.to_string node in
      Format.fprintf fmtr "Unhandled micheline: %s at path %s" node path
  | Expected_micheline_prim ->
      Format.fprintf fmtr "%s" "Expected_micheline_prim"
  | Unsatisfiable_comparability_constraint
      (Comparability_error_types (ty1, ty2)) ->
      let ty1 = michelson_type_to_string ty1 in
      let ty2 = michelson_type_to_string ty2 in
      Format.fprintf
        fmtr
        "Unsatisfiable comparability constraint: %s # %s"
        ty1
        ty2
  | Unsatisfiable_comparability_constraint
      (Comparability_error_tags (ty, cmp1, cmp2)) ->
      Format.fprintf
        fmtr
        "Unsatisfiable comparability constraint: %a :: %a # %a"
        Type.Base.pp
        ty
        pp_comparability
        cmp1
        pp_comparability
        cmp2
  | Base_types_incompatible (ty1, ty2) ->
      Format.fprintf
        fmtr
        "Base types incompatible: %a %a"
        Type.Base.pp
        ty1
        Type.Base.pp
        ty2
  | Stack_types_incompatible (sty1, sty2) ->
      Format.fprintf
        fmtr
        "Stack types incompatible: %a %a"
        Type.Stack.pp
        sty1
        Type.Stack.pp
        sty2
  | Badly_typed_arithmetic (prim, ty1, ty2) ->
      Format.fprintf
        fmtr
        "Badly typed arithmetic: %a(%a, %a)"
        Mikhailsky_prim.pp
        prim
        Type.Base.pp
        ty1
        Type.Base.pp
        ty2
  | Ill_formed_arithmetic (path, node) ->
      let path = Mikhailsky.Path.to_string path in
      let node = Mikhailsky.to_string node in
      Format.fprintf fmtr "Ill formed arithmetic: %s at path %s" node path
  | Cyclic_stack_type -> Format.fprintf fmtr "Cyclic stack type"
  | Cyclic_base_type -> Format.fprintf fmtr "Cyclic base type"
  | Invalid_ast (msg_opt, path, node) -> (
      let path = Mikhailsky.Path.to_string path in
      let node = Mikhailsky.to_string node in
      match msg_opt with
      | None -> Format.fprintf fmtr "Invalid ast: %s at path %s" node path
      | Some msg ->
          Format.fprintf fmtr "Invalid ast: %s at path %s (%s)" node path msg)

exception Ill_typed_script of inference_error

let unsatisfiable_comparability ty cmp1 cmp2 =
  raise
    (Ill_typed_script
       (Unsatisfiable_comparability_constraint
          (Comparability_error_tags (ty, cmp1, cmp2))))

let invalid_ast ?msg path node =
  raise (Ill_typed_script (Invalid_ast (msg, path, node)))

let () =
  Printexc.register_printer (fun exn ->
      match exn with
      | Ill_typed_script error ->
          Some (Format.asprintf "%a" pp_inference_error error)
      | _ -> None)

(* ------------------------------------------------------------------------- *)

module Repr_store =
  Stores.Map
    (Int_map)
    (struct
      type key = int

      type value = michelson_type

      let key_to_string = string_of_int

      let value_to_string = michelson_type_to_string
    end)

module Repr_sm = Monads.Make_state_monad (Repr_store)
module Path_map = Map.Make (Mikhailsky.Path)

module Annot_instr_store =
  Stores.Map
    (Path_map)
    (struct
      type key = Mikhailsky.Path.t

      type value = transformer

      let key_to_string = Mikhailsky.Path.to_string

      let value_to_string {bef; aft} =
        Format.asprintf "%a => %a" Type.Stack.pp bef Type.Stack.pp aft
    end)

module Annot_instr_sm = Monads.Make_state_monad (Annot_instr_store)

module Annot_data_store =
  Stores.Map
    (Path_map)
    (struct
      type key = Mikhailsky.Path.t

      type value = Type.Base.t

      let key_to_string = Mikhailsky.Path.to_string

      let value_to_string ty = Format.asprintf "%a" Type.Base.pp ty
    end)

module Annot_data_sm = Monads.Make_state_monad (Annot_data_store)

type state = {
  uf : UF.M.state;
  repr : Repr_sm.state;
  annot_instr : Annot_instr_sm.state;
  annot_data : Annot_data_sm.state;
}

module M = struct
  type 'a t = state -> 'a * state

  let empty : unit -> state =
   fun () ->
    {
      uf = UF.M.empty ();
      repr = Repr_sm.empty ();
      annot_instr = Annot_instr_sm.empty ();
      annot_data = Annot_data_sm.empty ();
    }

  let ( >>= ) m f s =
    let x, s = m s in
    f x s
    [@@inline]

  let return x s = (x, s)

  (* let run m = fst (m (empty ())) *)

  let uf_lift : 'a UF.M.t -> 'a t =
   fun computation state ->
    let res, uf = computation state.uf in
    (res, {state with uf})
   [@@inline]

  let repr_lift : 'a Repr_sm.t -> 'a t =
   fun computation state ->
    let res, repr = computation state.repr in
    (res, {state with repr})
   [@@inline]

  let annot_instr_lift : 'a Annot_instr_sm.t -> 'a t =
   fun computation state ->
    let res, annot_instr = computation state.annot_instr in
    (res, {state with annot_instr})
   [@@inline]

  let annot_data_lift : 'a Annot_data_sm.t -> 'a t =
   fun computation state ->
    let res, annot_data = computation state.annot_data in
    (res, {state with annot_data})
   [@@inline]

  let set_repr k v = repr_lift (Repr_sm.set k v) [@@inline]

  let get_repr_exn k =
    repr_lift (Repr_sm.get k) >>= function
    | None -> Stdlib.failwith "get_repr_exn"
    | Some res -> return res
    [@@inline]

  let set_instr_annot k v = annot_instr_lift (Annot_instr_sm.set k v) [@@inline]

  let get_instr_annot k = annot_instr_lift (Annot_instr_sm.get k) [@@inline]

  let set_data_annot k v = annot_data_lift (Annot_data_sm.set k v) [@@inline]

  let get_data_annot k = annot_data_lift (Annot_data_sm.get k) [@@inline]

  let get_state state = (state, state)
end

module S = Set.Make (Int)

let rec instantiate (encountered : S.t) (stack_ty : Type.Stack.t) :
    Type.Stack.t M.t =
  let open Type.Stack in
  let open M in
  if S.mem stack_ty.tag encountered then
    raise (Ill_typed_script Cyclic_stack_type)
  else
    let encountered = S.add stack_ty.tag encountered in
    match stack_ty.node with
    | Empty_t -> return stack_ty
    | Stack_var_t x -> (
        uf_lift (UF.find x) >>= fun root ->
        get_repr_exn root >>= function
        | Stack_type None -> return (Type.stack_var root)
        | Stack_type (Some ty) -> instantiate encountered ty
        | _ -> assert false)
    | Item_t (head, tail) ->
        instantiate_base S.empty head >>= fun head ->
        instantiate encountered tail >>= fun tail ->
        return (Type.item head tail)

and instantiate_base (encountered : S.t) (ty : Type.Base.t) : Type.Base.t M.t =
  let open Type.Base in
  let open M in
  if S.mem ty.tag encountered then raise (Ill_typed_script Cyclic_base_type)
  else
    let encountered = S.add ty.tag encountered in
    match ty.node with
    | Unit_t | Int_t | Nat_t | Bool_t | String_t | Bytes_t | Key_hash_t | Key_t
    | Timestamp_t | Mutez_t ->
        return ty
    | Option_t ty ->
        instantiate_base encountered ty >>= fun ty -> return (Type.option ty)
    | List_t ty ->
        instantiate_base encountered ty >>= fun ty -> return (Type.list ty)
    | Set_t ty ->
        instantiate_base encountered ty >>= fun ty -> return (Type.set ty)
    | Map_t (kty, vty) ->
        instantiate_base encountered kty >>= fun kty ->
        instantiate_base encountered vty >>= fun vty ->
        return (Type.map kty vty)
    | Pair_t (lty, rty) ->
        instantiate_base encountered lty >>= fun lty ->
        instantiate_base encountered rty >>= fun rty ->
        return (Type.pair lty rty)
    | Or_t (lty, rty) ->
        instantiate_base encountered lty >>= fun lty ->
        instantiate_base encountered rty >>= fun rty ->
        return (Type.or_ lty rty)
    | Lambda_t (dom, range) ->
        instantiate_base encountered dom >>= fun dom ->
        instantiate_base encountered range >>= fun range ->
        return (Type.lambda dom range)
    | Var_t x -> (
        uf_lift (UF.find x) >>= fun root ->
        get_repr_exn root >>= function
        | Base_type {repr = None; _} -> return (Type.var root)
        | Base_type {repr = Some ty; _} -> instantiate_base encountered ty
        | _ -> assert false)

let instantiate_base base_ty = instantiate_base S.empty base_ty

let instantiate stack_ty = instantiate S.empty stack_ty

let rec unify (x : Type.Stack.t) (y : Type.Stack.t) : unit M.t =
  let open Type.Stack in
  let open M in
  let unify_single_stack v x =
    (match Type.Stack.vars x with
    | None -> return ()
    | Some v' ->
        if v = v' then raise (Ill_typed_script Cyclic_stack_type) else return ())
    >>= fun () ->
    M.uf_lift (UF.find v) >>= fun root ->
    get_repr_exn root >>= fun repr ->
    merge_reprs (Stack_type (Some x)) repr >>= fun repr -> set_repr root repr
  in
  if x.tag = y.tag then return ()
  else
    match (x.node, y.node) with
    | Empty_t, Empty_t -> return ()
    | Stack_var_t x, Stack_var_t y ->
        M.uf_lift (UF.find x) >>= fun root_x ->
        M.uf_lift (UF.find y) >>= fun root_y ->
        get_repr_exn root_x >>= fun repr_x ->
        get_repr_exn root_y >>= fun repr_y ->
        M.uf_lift (UF.union x y) >>= fun root ->
        merge_reprs repr_x repr_y >>= fun repr -> set_repr root repr
    | Stack_var_t v, _ -> unify_single_stack v y
    | _, Stack_var_t v -> unify_single_stack v x
    | Item_t (ty1, tail1), Item_t (ty2, tail2) ->
        unify_base ty1 ty2 >>= fun () ->
        unify tail1 tail2 >>= fun () -> return ()
    | _ -> raise (Ill_typed_script (Stack_types_incompatible (x, y)))

and unify_base (x : Type.Base.t) (y : Type.Base.t) : unit M.t =
  let open Type.Base in
  let open M in
  let unify_single_var v x =
    (if List.mem v (Type.Base.vars x) then
     raise (Ill_typed_script Cyclic_base_type)
    else return ())
    >>= fun () ->
    M.uf_lift (UF.find v) >>= fun root ->
    get_repr_exn root >>= fun repr ->
    get_comparability x >>= fun comparable ->
    merge_reprs (Base_type {repr = Some x; comparable}) repr >>= fun repr ->
    set_repr root repr
  in
  if x.tag = y.tag then return ()
  else
    match (x.node, y.node) with
    | Unit_t, Unit_t
    | Int_t, Int_t
    | Nat_t, Nat_t
    | Bool_t, Bool_t
    | String_t, String_t
    | Bytes_t, Bytes_t
    | Key_hash_t, Key_hash_t
    | Timestamp_t, Timestamp_t
    | Mutez_t, Mutez_t
    | Key_t, Key_t ->
        return ()
    | Option_t x, Option_t y -> unify_base x y
    | List_t x, List_t y -> unify_base x y
    | Set_t x, Set_t y -> unify_base x y
    | Map_t (kx, vx), Map_t (ky, vy) ->
        unify_base kx ky >>= fun () -> unify_base vx vy
    | Pair_t (x, x'), Pair_t (y, y') ->
        unify_base x y >>= fun () -> unify_base x' y'
    | Or_t (x, x'), Or_t (y, y') ->
        unify_base x y >>= fun () -> unify_base x' y'
    | Lambda_t (x, x'), Lambda_t (y, y') ->
        unify_base x y >>= fun () -> unify_base x' y'
    | Var_t x, Var_t y ->
        M.uf_lift (UF.find x) >>= fun root_x ->
        M.uf_lift (UF.find y) >>= fun root_y ->
        get_repr_exn root_x >>= fun repr_x ->
        get_repr_exn root_y >>= fun repr_y ->
        M.uf_lift (UF.union x y) >>= fun root ->
        merge_reprs repr_x repr_y >>= fun repr -> set_repr root repr
    | Var_t v, _ -> unify_single_var v y
    | _, Var_t v -> unify_single_var v x
    | _ ->
        instantiate_base x >>= fun x ->
        instantiate_base y >>= fun y ->
        raise (Ill_typed_script (Base_types_incompatible (x, y)))

and merge_reprs (repr1 : michelson_type) (repr2 : michelson_type) :
    michelson_type M.t =
  let open M in
  match (repr1, repr2) with
  | (Stack_type None as repr), Stack_type None
  | (Stack_type (Some _) as repr), Stack_type None
  | Stack_type None, (Stack_type (Some _) as repr) ->
      return repr
  | (Stack_type (Some sty1) as repr), Stack_type (Some sty2) ->
      unify sty1 sty2 >>= fun () -> return repr
  | ( Base_type {repr = opt1; comparable = cmp1},
      Base_type {repr = opt2; comparable = cmp2} ) -> (
      let comparable_opt = sup_comparability cmp1 cmp2 in
      match comparable_opt with
      | None ->
          raise
            (Ill_typed_script
               (Unsatisfiable_comparability_constraint
                  (Comparability_error_types (repr1, repr2))))
      | Some comparable -> (
          match (opt1, opt2) with
          | None, None -> return (Base_type {repr = None; comparable})
          | (Some ty as repr), None ->
              assert_comparability comparable ty >>= fun () ->
              return (Base_type {repr; comparable})
          | None, (Some ty as repr) ->
              assert_comparability comparable ty >>= fun () ->
              return (Base_type {repr; comparable})
          | Some ty1, Some ty2 ->
              unify_base ty1 ty2 >>= fun () ->
              assert_comparability comparable ty1 >>= fun () ->
              assert_comparability comparable ty2 >>= fun () ->
              return (Base_type {repr = opt1; comparable})))
  | _ -> assert false

and assert_comparability comparable ty =
  assert_comparability_aux comparable ty []

and assert_comparability_aux lower_bound (ty : Type.Base.t)
    (encountered : int list) : unit M.t =
  let open M in
  if List.mem ty.tag encountered then raise (Ill_typed_script Cyclic_base_type)
  else
    let encountered = ty.tag :: encountered in
    match ty.node with
    | Var_t v -> (
        uf_lift (UF.find v) >>= fun root ->
        get_repr_exn root >>= fun repr ->
        match repr with
        | Base_type {repr = None; comparable} -> (
            match sup_comparability comparable lower_bound with
            | None -> unsatisfiable_comparability ty comparable lower_bound
            | Some comparable ->
                set_repr root (Base_type {repr = None; comparable}))
        | Base_type {repr = Some ty; comparable} -> (
            match sup_comparability comparable lower_bound with
            | None -> unsatisfiable_comparability ty comparable lower_bound
            | Some comparable ->
                assert_comparability_aux lower_bound ty encountered
                >>= fun () ->
                set_repr root (Base_type {repr = Some ty; comparable}))
        | Stack_type _ -> assert false)
    | List_t _ | Set_t _ | Map_t _ | Lambda_t _ | Key_t -> (
        match lower_bound with
        | Unconstrained | Not_comparable -> return ()
        | Comparable -> unsatisfiable_comparability ty Unconstrained lower_bound
        )
    | Unit_t | Int_t | Nat_t | Bool_t | String_t | Bytes_t | Key_hash_t
    | Timestamp_t | Mutez_t ->
        (* if not (le_comparability lower_bound Comparable) then
         *   unsatisfiable_comparability ty Comparable lower_bound
         * else *)
        return ()
    | Option_t ty -> (
        match lower_bound with
        | Comparable -> assert_comparability_aux Comparable ty encountered
        | Not_comparable | Unconstrained -> return ())
    | Pair_t (l, r) -> (
        match lower_bound with
        | Comparable ->
            assert_comparability_aux Comparable l encountered >>= fun () ->
            assert_comparability_aux Comparable r encountered
        | Unconstrained | Not_comparable -> return ())
    | Or_t (l, r) -> (
        match lower_bound with
        | Comparable ->
            assert_comparability_aux Comparable l encountered >>= fun () ->
            assert_comparability_aux Comparable r encountered
        | Unconstrained | Not_comparable -> return ())

and get_comparability (ty : Type.Base.t) : comparability M.t =
  let open M in
  match ty.node with
  | Var_t v -> (
      get_repr_exn v >>= fun repr ->
      match repr with
      | Stack_type _ -> assert false
      | Base_type {comparable; _} -> return comparable)
  | Unit_t | Int_t | Nat_t | Bool_t | String_t | Bytes_t | Key_hash_t
  | Timestamp_t | Mutez_t ->
      return Comparable
  | List_t _ | Set_t _ | Map_t _ | Lambda_t _ | Key_t -> return Not_comparable
  | Option_t ty -> get_comparability ty
  | Or_t (lt, rt) | Pair_t (lt, rt) -> (
      get_comparability lt >>= fun lc ->
      get_comparability rt >>= fun rc ->
      match (lc, rc) with
      | Comparable, Comparable -> return Comparable
      | _ -> return Unconstrained)

let fresh =
  let x = ref ~-1 in
  fun () ->
    incr x ;
    !x

let exists_stack : unit -> Type.Stack.t M.t =
  let open M in
  fun () ->
    let fresh = fresh () in
    uf_lift (UF.add fresh) >>= fun () ->
    set_repr fresh (Stack_type None) >>= fun () -> return (Type.stack_var fresh)

let exists : unit -> Type.Base.t M.t =
  let open M in
  fun () ->
    let fresh = fresh () in
    uf_lift (UF.add fresh) >>= fun () ->
    set_repr fresh (Base_type {repr = None; comparable = Unconstrained})
    >>= fun () -> return (Type.var fresh)

let exists_cmp : unit -> Type.Base.t M.t =
  let open M in
  fun () ->
    let fresh = fresh () in
    uf_lift (UF.add fresh) >>= fun () ->
    set_repr fresh (Base_type {repr = None; comparable = Comparable})
    >>= fun () -> return (Type.var fresh)

(* Adapted from [script_ir_translator] *)
let parse_uint30 n : int =
  let max_uint30 = 0x3fffffff in
  match n with
  | Micheline.Int (_, n')
    when Compare.Z.(Z.zero <= n') && Compare.Z.(n' <= Z.of_int max_uint30) ->
      Z.to_int n'
  | _ -> assert false

(* encodes the per-instruction relationship between input and output types
   of binary arithmetic operations. *)
let arith_type (instr : Mikhailsky_prim.prim) (ty1 : Type.Base.t)
    (ty2 : Type.Base.t) : Type.Base.t option =
  match (instr, ty1.node, ty2.node) with
  | (I_ADD | I_MUL), Int_t, Int_t
  | (I_ADD | I_MUL), Int_t, Nat_t
  | (I_ADD | I_MUL), Nat_t, Int_t ->
      Some Type.int
  | (I_ADD | I_MUL), Nat_t, Nat_t -> Some Type.nat
  | I_SUB, Int_t, Int_t
  | I_SUB, Int_t, Nat_t
  | I_SUB, Nat_t, Int_t
  | I_SUB, Nat_t, Nat_t
  | I_SUB, Timestamp_t, Timestamp_t ->
      Some Type.int
  | I_EDIV, Int_t, Int_t
  | I_EDIV, Int_t, Nat_t
  | I_EDIV, Nat_t, Int_t
  | I_EDIV, Nat_t, Nat_t ->
      Some Type.(option (pair nat nat))
  (* Timestamp *)
  | I_ADD, Timestamp_t, Int_t
  | I_ADD, Int_t, Timestamp_t
  | I_SUB, Timestamp_t, Int_t ->
      Some Type.timestamp
  (* Mutez *)
  | I_ADD, Mutez_t, Mutez_t
  | I_SUB, Mutez_t, Mutez_t
  | I_MUL, Mutez_t, Nat_t
  | I_MUL, Nat_t, Mutez_t ->
      Some Type.mutez
  | I_EDIV, Mutez_t, Nat_t -> Some Type.(option (pair mutez mutez))
  | I_EDIV, Mutez_t, Mutez_t -> Some Type.(option (pair nat mutez))
  | _ -> None

let rec generate_constraints (path : Mikhailsky.Path.t) (node : Mikhailsky.node)
    (bef : Type.Stack.t) (aft : Type.Stack.t) : unit M.t =
  let open M in
  set_instr_annot path {bef; aft} >>= fun () ->
  match node with
  | Int (_, _) ->
      assert false (* Ints should always be guarded by annotations *)
  | String (_, _) | Bytes (_, _) ->
      raise (Ill_typed_script Expected_micheline_prim)
  (* Hole *)
  | Prim (_, I_Hole, [], _) -> return ()
  (* Stack ops - simple cases *)
  | Prim (_loc, I_DROP, [], _annot) ->
      exists () >>= fun top -> unify bef (Type.item top aft)
  | Prim (_loc, I_DROP, [n], _annot) ->
      let n = parse_uint30 n in
      generate_constraints_dropn n bef aft
  | Prim (_loc, I_DUP, [], _annot) ->
      exists () >>= fun top ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item top rest) >>= fun () ->
      unify aft Type.(item top (item top rest))
  | Prim (_loc, I_SWAP, [], _annot) ->
      exists () >>= fun a ->
      exists () >>= fun b ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item a (item b rest)) >>= fun () ->
      unify aft Type.(item b (item a rest))
  | Prim (_loc, I_PUSH, [t; d], _annot) ->
      let ty =
        Mikhailsky.parse_ty
          ~allow_big_map:false
          ~allow_operation:false
          ~allow_contract:false
          t
      in
      generate_constraints_data (Mikhailsky.Path.at_index 1 path) d ty
      >>= fun () ->
      (* assert_data_has_ground_type (Mikhailsky.Path.at_index 1 path) d ty >>= fun () -> *)
      unify aft Type.(item ty bef)
  | Prim (_loc, I_UNIT, [], _annot) -> unify aft Type.(item unit bef)
  | Prim (_loc, I_DIP, [code], _annot) ->
      exists () >>= fun top ->
      exists_stack () >>= fun bef_rest ->
      exists_stack () >>= fun aft_rest ->
      unify bef Type.(item top bef_rest) >>= fun () ->
      unify aft Type.(item top aft_rest) >>= fun () ->
      generate_constraints
        (Mikhailsky.Path.at_index 0 path)
        code
        bef_rest
        aft_rest
  (* TODO: DIGn, etc *)
  (* Option-related instructions *)
  | Prim (_, I_SOME, [], _) ->
      exists () >>= fun top ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item top rest) >>= fun () ->
      unify aft Type.(item (option top) rest)
  | Prim (_, I_NONE, [t], _) ->
      let ty =
        Mikhailsky.parse_ty
          ~allow_big_map:true
          ~allow_operation:true
          ~allow_contract:true
          t
      in
      unify aft Type.(item (option ty) bef)
  | Prim (_, I_IF_NONE, [bt; bf], _) ->
      exists () >>= fun a ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item (option a) rest) >>= fun () ->
      generate_constraints (Mikhailsky.Path.at_index 0 path) bt rest aft
      >>= fun () ->
      generate_constraints
        (Mikhailsky.Path.at_index 1 path)
        bf
        Type.(item a rest)
        aft
  (* bool-based control flow *)
  | Prim (_, I_IF, [bt; bf], _) ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item bool rest) >>= fun () ->
      generate_constraints (Mikhailsky.Path.at_index 0 path) bt rest aft
      >>= fun () ->
      generate_constraints (Mikhailsky.Path.at_index 1 path) bf rest aft
  | Prim (_, I_LOOP, [body], _) ->
      unify bef Type.(item bool aft) >>= fun () ->
      generate_constraints (Mikhailsky.Path.at_index 0 path) body aft bef
  (* Boolean binops *)
  | Prim (_, I_AND, [], _) | Prim (_, I_OR, [], _) | Prim (_, I_XOR, [], _) ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item bool (item bool rest)) >>= fun () ->
      unify aft Type.(item bool rest)
  (* Arithmetic *)
  | Prim (_, ((I_ADD | I_SUB | I_MUL | I_EDIV) as instr), [ty1; ty2], _) -> (
      let ty1 =
        Mikhailsky.parse_ty
          ~allow_big_map:false
          ~allow_operation:false
          ~allow_contract:false
          ty1
      in
      let ty2 =
        Mikhailsky.parse_ty
          ~allow_big_map:false
          ~allow_operation:false
          ~allow_contract:false
          ty2
      in
      match arith_type instr ty1 ty2 with
      | None ->
          raise (Ill_typed_script (Badly_typed_arithmetic (instr, ty1, ty2)))
      | Some ret ->
          exists_stack () >>= fun rest ->
          unify bef Type.(item ty1 (item ty2 rest)) >>= fun () ->
          unify aft Type.(item ret rest))
  | Prim (_, (I_ADD | I_SUB | I_MUL | I_EDIV), _, _) ->
      raise (Ill_typed_script (Ill_formed_arithmetic (path, node)))
  | Prim (_, I_COMPARE, [], _) ->
      exists_cmp () >>= fun a ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item a (item a rest)) >>= fun () ->
      unify aft Type.(item int rest)
  | Prim (_, I_ABS, [], _) ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item int rest) >>= fun () ->
      unify aft Type.(item nat rest)
  | Prim (_, I_GT, [], _) ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item int rest) >>= fun () ->
      unify aft Type.(item bool rest)
  (* Strings/bytes *)
  | Prim (_, I_CONCAT, [], _) ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item string (item string rest)) >>= fun () ->
      unify aft Type.(item string rest)
  | Prim (_, I_SIZE_STRING, [], _) ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item string rest) >>= fun () ->
      unify aft Type.(item nat rest)
  | Prim (_, I_SIZE_BYTES, [], _) ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item bytes rest) >>= fun () ->
      unify aft Type.(item nat rest)
  (* Crypto *)
  | Prim (_, I_SHA256, [], _)
  | Prim (_, I_SHA512, [], _)
  | Prim (_, I_BLAKE2B, [], _) ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item bytes rest) >>= fun () ->
      unify aft Type.(item bytes rest)
  | Prim (_, I_HASH_KEY, [], _) ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item key rest) >>= fun () ->
      unify aft Type.(item key_hash rest)
  (* sets *)
  | Prim (_, I_EMPTY_SET, [], _) ->
      exists_cmp () >>= fun cmpty -> unify aft Type.(item (set cmpty) bef)
  | Prim (_, I_UPDATE_SET, [], _) ->
      exists_cmp () >>= fun cty ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item cty (item bool (item (set cty) rest))) >>= fun () ->
      unify aft Type.(item (set cty) rest)
  | Prim (_, I_SIZE_SET, [], _) ->
      exists_cmp () >>= fun cmpty ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item (set cmpty) rest) >>= fun () ->
      unify aft Type.(item nat rest)
  | Prim (_, I_ITER_SET, [code], _) ->
      exists_cmp () >>= fun cmpty ->
      unify bef Type.(item (set cmpty) aft) >>= fun () ->
      generate_constraints
        (Mikhailsky.Path.at_index 0 path)
        code
        Type.(item cmpty aft)
        aft
  | Prim (_, I_MEM_SET, [], _) ->
      exists_cmp () >>= fun cmpty ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item cmpty (item (set cmpty) rest)) >>= fun () ->
      unify aft Type.(item bool rest)
  (* maps *)
  | Prim (_, I_EMPTY_MAP, [], _) ->
      exists_cmp () >>= fun kty ->
      exists () >>= fun vty -> unify aft Type.(item (map kty vty) bef)
  | Prim (_, I_UPDATE_MAP, [], _) ->
      exists_cmp () >>= fun kty ->
      exists () >>= fun vty ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item kty (item (option vty) (item (map kty vty) rest)))
      >>= fun () -> unify aft Type.(item (map kty vty) rest)
  | Prim (_, I_SIZE_MAP, [], _) ->
      exists_cmp () >>= fun kty ->
      exists () >>= fun vty ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item (map kty vty) rest) >>= fun () ->
      unify aft Type.(item nat rest)
  | Prim (_, I_ITER_MAP, [code], _) ->
      exists_cmp () >>= fun kty ->
      exists () >>= fun vty ->
      unify bef Type.(item (map kty vty) aft) >>= fun () ->
      generate_constraints
        (Mikhailsky.Path.at_index 0 path)
        code
        Type.(item (pair kty vty) aft)
        aft
  | Prim (_, I_MAP_MAP, [code], _) ->
      exists_cmp () >>= fun kty ->
      exists () >>= fun vty1 ->
      exists () >>= fun vty2 ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item (map kty vty1) rest) >>= fun () ->
      unify aft Type.(item (map kty vty2) rest) >>= fun () ->
      generate_constraints
        (Mikhailsky.Path.at_index 0 path)
        code
        Type.(item (pair kty vty1) rest)
        Type.(item vty2 rest)
  | Prim (_, I_MEM_MAP, [], _) ->
      exists_cmp () >>= fun kty ->
      exists () >>= fun vty ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item kty (item (map kty vty) rest)) >>= fun () ->
      unify aft Type.(item bool rest)
  | Prim (_, I_GET_MAP, [], _) ->
      exists_cmp () >>= fun kty ->
      exists () >>= fun vty ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item kty (item (map kty vty) rest)) >>= fun () ->
      unify aft Type.(item (option vty) rest)
  (* Pairs *)
  | Prim (_, I_PAIR, [], _) ->
      exists () >>= fun a ->
      exists () >>= fun b ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item a (item b rest)) >>= fun () ->
      unify aft Type.(item (pair a b) rest)
  | Prim (_, I_CAR, [], _) ->
      exists () >>= fun a ->
      exists () >>= fun b ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item (pair a b) rest) >>= fun () ->
      unify aft Type.(item a rest)
  | Prim (_, I_CDR, [], _) ->
      exists () >>= fun a ->
      exists () >>= fun b ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item (pair a b) rest) >>= fun () ->
      unify aft Type.(item b rest)
  (* Ors *)
  | Prim (_, I_LEFT, [], _) ->
      exists () >>= fun lt ->
      exists () >>= fun rt ->
      exists_stack () >>= fun rest ->
      unify bef (Type.item lt rest) >>= fun () ->
      unify aft Type.(item (or_ lt rt) rest) >>= fun res -> return res
  | Prim (_, I_RIGHT, [], _) ->
      exists () >>= fun lt ->
      exists () >>= fun rt ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item rt rest) >>= fun () ->
      unify aft Type.(item (or_ lt rt) rest)
  | Prim (_, (I_LEFT | I_RIGHT), _ :: _, _) ->
      invalid_ast ~msg:__LOC__ path node
  | Prim (_, I_LOOP_LEFT, [body], _) ->
      exists () >>= fun l ->
      exists () >>= fun r ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item (or_ l r) rest) >>= fun () ->
      unify aft Type.(item r rest) >>= fun () ->
      generate_constraints
        (Mikhailsky.Path.at_index 0 path)
        body
        Type.(item l rest)
        bef
  | Prim (_, I_IF_LEFT, [bt; bf], _) ->
      exists () >>= fun a ->
      exists () >>= fun b ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item (or_ a b) rest) >>= fun () ->
      generate_constraints
        (Mikhailsky.Path.at_index 0 path)
        bt
        (Type.item a rest)
        aft
      >>= fun () ->
      generate_constraints
        (Mikhailsky.Path.at_index 1 path)
        bf
        (Type.item b rest)
        aft
  (* lambdas *)
  | Prim (_, I_LAMBDA, [code], _) ->
      exists () >>= fun dom ->
      exists () >>= fun range ->
      unify aft Type.(item (lambda dom range) bef) >>= fun () ->
      generate_constraints
        (Mikhailsky.Path.at_index 0 path)
        code
        Type.(item dom empty)
        Type.(item range empty)
  | Prim (_, I_LAMBDA, _, _) -> invalid_ast ~msg:__LOC__ path node
  | Prim (_, I_APPLY, [], _) ->
      exists () >>= fun a ->
      exists () >>= fun b ->
      exists () >>= fun ret ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item a (item (lambda (pair a b) ret) rest)) >>= fun () ->
      unify aft Type.(item (lambda b ret) rest)
  | Prim (_, I_EXEC, [], _) ->
      exists () >>= fun a ->
      exists () >>= fun ret ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item a (item (lambda a ret) rest)) >>= fun () ->
      unify aft Type.(item ret rest)
  (* lists *)
  | Prim (_, I_NIL, [], _) ->
      exists () >>= fun a -> unify aft Type.(item (list a) bef)
  | Prim (_, I_CONS, [], _) ->
      exists () >>= fun a ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item a (item (list a) rest)) >>= fun () ->
      unify aft Type.(item (list a) rest)
  | Prim (_, I_SIZE_LIST, [], _) ->
      exists () >>= fun ty ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item (list ty) rest) >>= fun () ->
      unify aft Type.(item nat rest)
  | Prim (_, I_ITER_LIST, [code], _) ->
      exists () >>= fun ty ->
      unify bef Type.(item (list ty) aft) >>= fun () ->
      generate_constraints
        (Mikhailsky.Path.at_index 0 path)
        code
        Type.(item ty aft)
        aft
  | Prim (_, I_MAP_LIST, [code], _) ->
      exists () >>= fun ty1 ->
      exists () >>= fun ty2 ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item (list ty1) rest) >>= fun () ->
      unify aft Type.(item (list ty2) rest) >>= fun () ->
      generate_constraints
        (Mikhailsky.Path.at_index 0 path)
        code
        Type.(item ty1 rest)
        Type.(item ty2 rest)
  (* pack/unpack*)
  | Prim (_, I_PACK, [], _) ->
      exists () >>= fun ty ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item ty rest) >>= fun () ->
      unify aft Type.(item bytes rest)
  | Prim (_, I_UNPACK, [], _) ->
      exists () >>= fun ty ->
      exists_stack () >>= fun rest ->
      unify bef Type.(item bytes rest) >>= fun () ->
      unify aft Type.(item (option ty) rest)
  (* Others *)
  | Seq (_, []) -> unify bef aft
  | Seq (_, [single]) ->
      generate_constraints (Mikhailsky.Path.at_index 0 path) single bef aft
  | Seq (_, instrs) -> generate_constraints_seq path 0 instrs bef aft
  | _ -> raise (Ill_typed_script (Unhandled_micheline (path, node)))

and generate_constraints_seq path index instrs bef aft =
  let open M in
  match instrs with
  | [] -> assert false
  | [single] ->
      generate_constraints (Mikhailsky.Path.at_index index path) single bef aft
  | hd :: tl ->
      exists_stack () >>= fun stack_ty ->
      generate_constraints (Mikhailsky.Path.at_index index path) hd bef stack_ty
      >>= fun () -> generate_constraints_seq path (index + 1) tl stack_ty aft

and generate_constraints_data (path : Mikhailsky.Path.t)
    (node : Mikhailsky.node) (ty : Type.Base.t) : unit M.t =
  let open M in
  set_data_annot path ty >>= fun () ->
  match node with
  | Prim (_, D_Hole, [], _) -> return ()
  | Prim (_, D_Unit, [], _) -> unify_base ty Type.unit
  | Prim (_, D_True, [], _) | Prim (_, D_False, [], _) ->
      unify_base ty Type.bool
  | String _ -> unify_base ty Type.string
  | Bytes _ -> unify_base ty Type.bytes
  | Prim (_, D_Pair, [vl; vr], _) ->
      exists () >>= fun lty ->
      exists () >>= fun rty ->
      generate_constraints_data (Mikhailsky.Path.at_index 0 path) vl lty
      >>= fun () ->
      generate_constraints_data (Mikhailsky.Path.at_index 1 path) vr rty
      >>= fun () -> unify_base ty (Type.pair lty rty)
  | Prim (_, D_Left, [term], _) ->
      exists () >>= fun lty ->
      exists () >>= fun rty ->
      generate_constraints_data (Mikhailsky.Path.at_index 0 path) term lty
      >>= fun () -> unify_base ty (Type.or_ lty rty)
  | Prim (_, D_Right, [term], _) ->
      exists () >>= fun lty ->
      exists () >>= fun rty ->
      generate_constraints_data (Mikhailsky.Path.at_index 0 path) term rty
      >>= fun () -> unify_base ty (Type.or_ lty rty)
  | Prim (_, D_None, [], _) ->
      exists () >>= fun elt_ty -> unify_base ty (Type.option elt_ty)
  | Prim (_, D_Some, [v], _) ->
      exists () >>= fun elt_ty ->
      generate_constraints_data (Mikhailsky.Path.at_index 0 path) v elt_ty
      >>= fun () -> unify_base ty (Type.option elt_ty)
  | Prim (_, A_Int, [Int (_, _)], _) -> unify_base ty Type.int
  | Prim (_, A_Nat, [Int (_, _)], _) -> unify_base ty Type.nat
  | Prim (_, A_Timestamp, [Int (_, _)], _) -> unify_base ty Type.timestamp
  | Prim (_, A_Mutez, [Int (_, _)], _) -> unify_base ty Type.mutez
  | Prim (_, A_Key_hash, [Bytes (_, _)], _) -> unify_base ty Type.key_hash
  | Prim (_, A_Key, [Bytes (_, _)], _) -> unify_base ty Type.key
  | Prim (_, A_List, [Seq (_, subterms)], _) ->
      exists () >>= fun elt_ty ->
      unify_base ty Type.(list elt_ty) >>= fun () ->
      (* path' accounts for the fact that the Seq is hidden under an annot. *)
      let path' = Mikhailsky.Path.at_index 0 path in
      generate_constraints_data_list path' 0 subterms elt_ty
  | Prim (_, A_Set, [Seq (_, subterms)], _) ->
      exists_cmp () >>= fun elt_ty ->
      unify_base ty Type.(set elt_ty) >>= fun () ->
      (* path' accounts for the fact that the Seq is hidden under an annot. *)
      let path' = Mikhailsky.Path.at_index 0 path in
      generate_constraints_data_set path' 0 subterms elt_ty
  | Prim (_, A_Map, [Seq (_, subterms)], _) ->
      exists_cmp () >>= fun k_ty ->
      exists () >>= fun v_ty ->
      unify_base ty Type.(map k_ty v_ty) >>= fun () ->
      (* path' accounts for the fact that the Seq is hidden under an annot. *)
      let path' = Mikhailsky.Path.at_index 0 path in
      generate_constraints_data_map path' 0 subterms k_ty v_ty
  | Prim (_, A_Lambda, [(Seq (_, _) as node)], _) ->
      exists () >>= fun dom ->
      exists () >>= fun range ->
      unify_base ty Type.(lambda dom range) >>= fun () ->
      let path' = Mikhailsky.Path.at_index 0 path in
      let bef = Type.(item dom empty) in
      let aft = Type.(item range empty) in
      generate_constraints path' node bef aft
  | Prim (_, (A_Int | A_Nat | A_List), _, _) ->
      invalid_ast ~msg:__LOC__ path node
  | Int _
  (* Ints should always be guarded by annotations *)
  | Seq (_, _)
  (* Lists, sets, maps, lambdas, should always be guarded by annotations *)
  | _ ->
      invalid_ast ~msg:__LOC__ path node

(* raise (Ill_typed_script (Invalid_ast (path, node))) *)
and generate_constraints_data_list path index data ty =
  let open M in
  match data with
  | [] -> return ()
  | hd :: tl ->
      let hd_path = Mikhailsky.Path.at_index index path in
      generate_constraints_data hd_path hd ty >>= fun () ->
      generate_constraints_data_list path (index + 1) tl ty

and generate_constraints_data_set path index data ty =
  let open M in
  match data with
  | [] -> return ()
  | hd :: tl ->
      let hd_path = Mikhailsky.Path.at_index index path in
      generate_constraints_data hd_path hd ty >>= fun () ->
      generate_constraints_data_list path (index + 1) tl ty

and generate_constraints_data_map path index data k_ty v_ty =
  let open M in
  match data with
  | [] -> return ()
  | elt :: tl -> (
      let elt_path = Mikhailsky.Path.at_index index path in
      match elt with
      | Prim (_, D_Elt, [k; v], _) ->
          let k_path = Mikhailsky.Path.at_index 0 elt_path in
          generate_constraints_data k_path k k_ty >>= fun () ->
          let v_path = Mikhailsky.Path.at_index 1 elt_path in
          generate_constraints_data v_path v v_ty >>= fun () ->
          generate_constraints_data_map path (index + 1) tl k_ty v_ty
      | _ -> invalid_ast ~msg:__LOC__ elt_path elt)

and generate_constraints_dropn n bef aft =
  let open M in
  if n = 0 then unify bef aft
  else
    exists () >>= fun top ->
    generate_constraints_dropn (n - 1) bef (Type.item top aft)

let infer_with_state (node : Mikhailsky.node) :
    (Type.Stack.t * Type.Stack.t) * state =
  let open M in
  ( exists_stack () >>= fun bef ->
    exists_stack () >>= fun aft ->
    generate_constraints Mikhailsky.Path.root node bef aft >>= fun () ->
    instantiate bef >>= fun bef ->
    instantiate aft >>= fun aft -> return (bef, aft) )
    (M.empty ())

let infer (node : Mikhailsky.node) : Type.Stack.t * Type.Stack.t =
  fst (infer_with_state node)

let infer_data_with_state (node : Mikhailsky.node) : Type.Base.t * state =
  let open M in
  ( exists () >>= fun ty ->
    generate_constraints_data Mikhailsky.Path.root node ty >>= fun () ->
    instantiate_base ty >>= fun ty -> return ty )
    (M.empty ())

let infer_data (node : Mikhailsky.node) : Type.Base.t =
  fst (infer_data_with_state node)
