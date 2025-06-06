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

exception Cannot_get_type of Mikhailsky.node * Kernel.Path.t

exception Unexpected_stack_type of string

exception Unexpected_base_type

let unparse_type = Mikhailsky.map_var (fun _ -> Mikhailsky.prim T_unit [] [])

let project_top (aft : Type.Stack.t) =
  match aft.node with
  | Type.Stack.Empty_t -> raise (Unexpected_stack_type "empty")
  | Type.Stack.Stack_var_t _ -> raise (Unexpected_stack_type "var")
  | Type.Stack.Item_t (top, _) -> top

let project_or (aft : Type.Stack.t) =
  let top = project_top aft in
  match top.node with
  | Type.Base.Or_t (l, r) -> (l, r)
  | _ -> raise Unexpected_base_type

let project_lambda (aft : Type.Stack.t) =
  let top = project_top aft in
  match top.node with
  | Type.Base.Lambda_t (dom, range) -> (dom, range)
  | _ -> raise Unexpected_base_type

let project_list (aft : Type.Stack.t) =
  let top = project_top aft in
  match top.node with
  | Type.Base.List_t t -> t
  | _ -> raise Unexpected_base_type

let project_set (aft : Type.Stack.t) =
  let top = project_top aft in
  match top.node with Type.Base.Set_t t -> t | _ -> raise Unexpected_base_type

let project_map (aft : Type.Stack.t) =
  let top = project_top aft in
  match top.node with
  | Type.Base.Map_t (k, v) -> (k, v)
  | _ -> raise Unexpected_base_type

let project_option (aft : Type.Stack.t) =
  let top = project_top aft in
  match top.node with
  | Type.Base.Option_t t -> t
  | _ -> raise Unexpected_base_type

let rec convert_raw : Mikhailsky.node -> (int, 'a) Micheline.node =
 fun node ->
  match node with
  | Micheline.Int (_, i) -> Micheline.Int (0, i)
  | Micheline.Prim (_, head, subterms, annots) ->
      let head = Mikhailsky_prim.to_michelson head in
      Micheline.Prim (0, head, List.map convert_raw subterms, annots)
  | Micheline.String (_, s) -> Micheline.String (0, s)
  | Micheline.Bytes (_, b) -> Micheline.Bytes (0, b)
  | Micheline.Seq (_, subterms) ->
      Micheline.Seq (0, List.map convert_raw subterms)

(* We assume that the term has been completed. *)
let rec convert :
    Mikhailsky.node -> Kernel.Path.t -> (int, 'a) Micheline.node Inference.M.t =
 fun node path ->
  let open Inference.M in
  match node with
  | Micheline.Int (_, i) -> return (Micheline.Int (0, i))
  | Micheline.String (_, s) -> return (Micheline.String (0, s))
  | Micheline.Bytes (_, b) -> return (Micheline.Bytes (0, b))
  (* Remove annotations *)
  | Micheline.Prim (_, prim, [term], _)
    when Mikhailsky_prim.kind prim = Annot_kind ->
      let path = Kernel.Path.at_index 0 path in
      convert term path
  (* Fail on holes *)
  | Micheline.Prim (_, I_Hole, _, _) | Micheline.Prim (_, D_Hole, _, _) ->
      raise Mikhailsky.Term_contains_holes
  (* Add type information to or injections *)
  | Micheline.Prim (_, I_LEFT, [], annots) -> (
      get_instr_annot path >>= fun ty_opt ->
      match ty_opt with
      | None -> raise (Cannot_get_type (node, path))
      | Some {aft; _} ->
          Inference.instantiate aft >>= fun aft ->
          let _, r = project_or aft in
          Inference.instantiate_base r >>= fun r ->
          Autocomp.replace_vars r >>= fun r ->
          let r = unparse_type r in
          let head = Mikhailsky_prim.to_michelson I_LEFT in
          return (Micheline.Prim (0, head, [convert_raw r], annots)))
  | Micheline.Prim (_, I_RIGHT, [], annots) -> (
      get_instr_annot path >>= fun ty_opt ->
      match ty_opt with
      | None -> raise (Cannot_get_type (node, path))
      | Some {aft; _} ->
          Inference.instantiate aft >>= fun aft ->
          let l, _ = project_or aft in
          Inference.instantiate_base l >>= fun l ->
          Autocomp.replace_vars l >>= fun l ->
          let l = unparse_type l in
          let head = Mikhailsky_prim.to_michelson I_RIGHT in
          return (Micheline.Prim (0, head, [convert_raw l], annots)))
  | Micheline.Prim (_, (I_LEFT | I_RIGHT), _, _) ->
      raise Mikhailsky.Ill_formed_mikhailsky
  (* Add type information for lambdas *)
  | Micheline.Prim (_, I_LAMBDA, [code], annots) -> (
      convert code (Kernel.Path.at_index 0 path) >>= fun code ->
      get_instr_annot path >>= fun ty_opt ->
      match ty_opt with
      | None -> raise (Cannot_get_type (node, path))
      | Some {aft; _} ->
          Inference.instantiate aft >>= fun aft ->
          let dom, range = project_lambda aft in
          Inference.instantiate_base dom >>= fun dom ->
          Autocomp.replace_vars dom >>= fun dom ->
          Inference.instantiate_base range >>= fun range ->
          Autocomp.replace_vars range >>= fun range ->
          let dom = unparse_type dom in
          let range = unparse_type range in
          let head = Mikhailsky_prim.to_michelson I_LAMBDA in
          return
            (Micheline.Prim
               (0, head, [convert_raw dom; convert_raw range; code], annots)))
  (* Add type information for empty_set, empty_map *)
  | Micheline.Prim (_, I_EMPTY_SET, [], annots) -> (
      get_instr_annot path >>= fun ty_opt ->
      match ty_opt with
      | None -> raise (Cannot_get_type (node, path))
      | Some {aft; _} ->
          Inference.instantiate aft >>= fun aft ->
          let elt = project_set aft in
          Inference.instantiate_base elt >>= fun elt ->
          Autocomp.replace_vars elt >>= fun elt ->
          let elt = unparse_type elt in
          let head = Mikhailsky_prim.to_michelson I_EMPTY_SET in
          return (Micheline.Prim (0, head, [convert_raw elt], annots)))
  | Micheline.Prim (_, I_EMPTY_MAP, [], annots) -> (
      get_instr_annot path >>= fun ty_opt ->
      match ty_opt with
      | None -> raise (Cannot_get_type (node, path))
      | Some {aft; _} ->
          Inference.instantiate aft >>= fun aft ->
          let k, v = project_map aft in
          Inference.instantiate_base k >>= fun k ->
          Autocomp.replace_vars k >>= fun k ->
          Inference.instantiate_base v >>= fun v ->
          Autocomp.replace_vars v >>= fun v ->
          let k = convert_raw (unparse_type k) in
          let v = convert_raw (unparse_type v) in
          let head = Mikhailsky_prim.to_michelson I_EMPTY_MAP in
          return (Micheline.Prim (0, head, [k; v], annots)))
  (* Add type information for UNPACK *)
  | Micheline.Prim (_, I_UNPACK, [], annots) -> (
      get_instr_annot path >>= fun ty_opt ->
      match ty_opt with
      | None -> raise (Cannot_get_type (node, path))
      | Some {aft; _} ->
          Inference.instantiate aft >>= fun aft ->
          let elt = project_option aft in
          Inference.instantiate_base elt >>= fun elt ->
          Autocomp.replace_vars elt >>= fun elt ->
          let elt = unparse_type elt in
          let head = Mikhailsky_prim.to_michelson I_UNPACK in
          return (Micheline.Prim (0, head, [convert_raw elt], annots)))
  (* Add type information for NIL *)
  | Micheline.Prim (_, I_NIL, [], annots) -> (
      get_instr_annot path >>= fun ty_opt ->
      match ty_opt with
      | None -> raise (Cannot_get_type (node, path))
      | Some {aft; _} ->
          Inference.instantiate aft >>= fun aft ->
          let elt = project_list aft in
          Inference.instantiate_base elt >>= fun elt ->
          Autocomp.replace_vars elt >>= fun elt ->
          let elt = unparse_type elt in
          let head = Mikhailsky_prim.to_michelson I_NIL in
          return (Micheline.Prim (0, head, [convert_raw elt], annots)))
  | Micheline.Prim (_, I_NIL, _, _) -> raise Mikhailsky.Ill_formed_mikhailsky
  (* Project out type information from arithmetic ops *)
  | Prim (_, ((I_ADD | I_SUB | I_MUL | I_EDIV) as instr), [_ty1; _ty2], annots)
    ->
      let head = Mikhailsky_prim.to_michelson instr in
      return (Micheline.Prim (0, head, [], annots))
  | Prim (_, (I_ADD | I_SUB | I_MUL | I_EDIV), _, _) ->
      raise Mikhailsky.Ill_formed_mikhailsky
  (* Base case *)
  | Micheline.Prim (_, head, subterms, annots) ->
      let head = Mikhailsky_prim.to_michelson head in
      convert_list path 0 subterms [] >>= fun subterms ->
      return (Micheline.Prim (0, head, subterms, annots))
  | Micheline.Seq (_, subterms) ->
      convert_list path 0 subterms [] >>= fun subterms ->
      return (Micheline.Seq (0, subterms))

and convert_list path i subterms acc =
  let open Inference.M in
  match subterms with
  | [] -> return (List.rev acc)
  | subterm :: tl ->
      let path' = Kernel.Path.at_index i path in
      convert subterm path' >>= fun term ->
      convert_list path (i + 1) tl (term :: acc)

let convert node state = fst (convert node Kernel.Path.root state)
