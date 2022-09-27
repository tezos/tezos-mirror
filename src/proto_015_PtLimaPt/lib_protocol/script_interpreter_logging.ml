(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Alpha_context
open Script_typed_ir

type kinstr_rewritek = {
  apply :
    'b 'u 'r 'f.
    ('b, 'u) stack_ty -> ('b, 'u, 'r, 'f) kinstr -> ('b, 'u, 'r, 'f) kinstr;
}
[@@ocaml.unboxed]

(* An existential wrapper around failed [kinstr], whose final stack type
   is hidden as it is irrelevant. *)
type ('a, 's) failed_kinstr_cast = {cast : 'b 'u. ('a, 's, 'b, 'u) kinstr}
[@@ocaml.unboxed]

(* This is a view on a deconstructed [kinstr]. Its type parameters refer to
   the type of the viewed [kinstr], while existentials inside describe types of
   [kinstr]'s components. The [reconstruct] field in each record stores a
   function which reconstructs the original instruction from its components. *)
type ('a, 's, 'r, 'f) ex_split_kinstr =
  | Ex_split_kinstr : {
      cont_init_stack : ('b, 'u) stack_ty;
      continuation : ('b, 'u, 'r, 'f) kinstr;
      reconstruct : ('b, 'u, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr;
    }
      -> ('a, 's, 'r, 'f) ex_split_kinstr
  | Ex_split_log : {
      stack : ('a, 's) stack_ty;
      continuation : ('a, 's, 'r, 'f) kinstr;
      reconstruct : ('a, 's, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr;
    }
      -> ('a, 's, 'r, 'f) ex_split_kinstr
  | Ex_split_loop_may_fail : {
      body_init_stack : ('b, 'u) stack_ty;
      body : ('b, 'u, 'r, 'f) kinstr;
      cont_init_stack : ('c, 'v) stack_ty;
      continuation : ('c, 'v, 't, 'g) kinstr;
      reconstruct :
        ('b, 'u, 'r, 'f) kinstr ->
        ('c, 'v, 't, 'g) kinstr ->
        ('a, 's, 't, 'g) kinstr;
    }
      -> ('a, 's, 't, 'g) ex_split_kinstr
  | Ex_split_loop_may_not_fail : {
      body_init_stack : ('b, 'u) stack_ty;
      body : ('b, 'u, 'r, 'f) kinstr;
      continuation : ('c, 'v, 't, 'g) kinstr;
      aft_body_stack_transform :
        ('r, 'f) stack_ty -> ('c, 'v) stack_ty tzresult;
      reconstruct :
        ('b, 'u, 'r, 'f) kinstr ->
        ('c, 'v, 't, 'g) kinstr ->
        ('a, 's, 't, 'g) kinstr;
    }
      -> ('a, 's, 't, 'g) ex_split_kinstr
  | Ex_split_if : {
      left_init_stack : ('b, 'u) stack_ty;
      left_branch : ('b, 'u, 'r, 'f) kinstr;
      right_init_stack : ('c, 'v) stack_ty;
      right_branch : ('c, 'v, 'r, 'f) kinstr;
      continuation : ('r, 'f, 't, 'g) kinstr;
      reconstruct :
        ('b, 'u, 'r, 'f) kinstr ->
        ('c, 'v, 'r, 'f) kinstr ->
        ('r, 'f, 't, 'g) kinstr ->
        ('a, 's, 't, 'g) kinstr;
    }
      -> ('a, 's, 't, 'g) ex_split_kinstr
  | Ex_split_halt : Script.location -> ('a, 's, 'a, 's) ex_split_kinstr
  | Ex_split_failwith : {
      location : Script.location;
      arg_ty : ('a, _) ty;
      cast : ('a, 's) failed_kinstr_cast;
    }
      -> ('a, 's, 'r, 'f) ex_split_kinstr

type ('r, 'f) ex_init_stack_ty =
  | Ex_init_stack_ty :
      ('a, 's) stack_ty * ('a, 's, 'r, 'f) kinstr
      -> ('r, 'f) ex_init_stack_ty

let rec stack_prefix_preservation_witness_split_input :
    type a s b t c u d v.
    (b, t, c, u, a, s, d, v) stack_prefix_preservation_witness ->
    (a, s) stack_ty ->
    (b, t) stack_ty =
 fun w s ->
  match (w, s) with
  | KPrefix (_, _, w), Item_t (_, s) ->
      stack_prefix_preservation_witness_split_input w s
  | KRest, s -> s

let rec stack_prefix_preservation_witness_split_output :
    type a s b t c u d v.
    (b, t, c, u, a, s, d, v) stack_prefix_preservation_witness ->
    (c, u) stack_ty ->
    (d, v) stack_ty =
 fun w s ->
  match (w, s) with
  | KPrefix (_, a, w), s ->
      Item_t (a, stack_prefix_preservation_witness_split_output w s)
  | KRest, s -> s

(* We apply this function to optional type information which must be present
   if functions from this module were called. Use with care. *)
let assert_some = function None -> assert false | Some x -> x

let kinstr_split :
    type a s r f.
    (a, s) stack_ty ->
    (a, s, r, f) kinstr ->
    (a, s, r, f) ex_split_kinstr tzresult =
 fun s i ->
  let dummy = Micheline.dummy_location in
  match (i, s) with
  | IDrop (loc, k), Item_t (_a, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IDrop (loc, k));
           }
  | IDup (loc, k), Item_t (a, s) ->
      let s = Item_t (a, Item_t (a, s)) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IDup (loc, k));
           }
  | ISwap (loc, k), Item_t (a, Item_t (b, s)) ->
      let s = Item_t (b, Item_t (a, s)) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISwap (loc, k));
           }
  | IConst (loc, a, x, k), s ->
      let s = Item_t (a, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IConst (loc, a, x, k));
           }
  | ICons_pair (loc, k), Item_t (a, Item_t (b, s)) ->
      pair_t dummy a b >|? fun (Ty_ex_c c) ->
      let s = Item_t (c, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> ICons_pair (loc, k));
        }
  | ICar (loc, k), Item_t (Pair_t (a, _b, _meta, _), s) ->
      let s = Item_t (a, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ICar (loc, k));
           }
  | ICdr (loc, k), Item_t (Pair_t (_a, b, _meta, _), s) ->
      let s = Item_t (b, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ICdr (loc, k));
           }
  | IUnpair (loc, k), Item_t (Pair_t (a, b, _meta, _), s) ->
      let s = Item_t (a, Item_t (b, s)) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IUnpair (loc, k));
           }
  | ICons_some (loc, k), Item_t (a, s) ->
      option_t dummy a >|? fun o ->
      let s = Item_t (o, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> ICons_some (loc, k));
        }
  | ICons_none (loc, a, k), s ->
      option_t dummy a >|? fun o ->
      let s = Item_t (o, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> ICons_none (loc, a, k));
        }
  | ( IIf_none {loc; branch_if_none; branch_if_some; k},
      Item_t (Option_t (a, _meta, _), s) ) ->
      ok
      @@ Ex_split_if
           {
             left_init_stack = s;
             left_branch = branch_if_none;
             right_init_stack = Item_t (a, s);
             right_branch = branch_if_some;
             continuation = k;
             reconstruct =
               (fun branch_if_none branch_if_some k ->
                 IIf_none {loc; branch_if_none; branch_if_some; k});
           }
  | IOpt_map {loc; body; k}, Item_t (Option_t (a, _meta, _), s) ->
      ok
      @@ Ex_split_loop_may_not_fail
           {
             body_init_stack = Item_t (a, s);
             body;
             continuation = k;
             aft_body_stack_transform =
               (function
               | Item_t (b, s) -> option_t dummy b >|? fun o -> Item_t (o, s));
             reconstruct = (fun body k -> IOpt_map {loc; body; k});
           }
  | ICons_left (loc, b, k), Item_t (a, s) ->
      union_t dummy a b >|? fun (Ty_ex_c c) ->
      let s = Item_t (c, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> ICons_left (loc, b, k));
        }
  | ICons_right (loc, a, k), Item_t (b, s) ->
      union_t dummy a b >|? fun (Ty_ex_c c) ->
      let s = Item_t (c, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> ICons_right (loc, a, k));
        }
  | ( IIf_left {loc; branch_if_left; branch_if_right; k},
      Item_t (Union_t (a, b, _meta, _), s) ) ->
      ok
      @@ Ex_split_if
           {
             left_init_stack = Item_t (a, s);
             left_branch = branch_if_left;
             right_init_stack = Item_t (b, s);
             right_branch = branch_if_right;
             continuation = k;
             reconstruct =
               (fun branch_if_left branch_if_right k ->
                 IIf_left {loc; branch_if_left; branch_if_right; k});
           }
  | ICons_list (loc, k), Item_t (_a, Item_t (l, s)) ->
      let s = Item_t (l, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ICons_list (loc, k));
           }
  | INil (loc, a, k), s ->
      list_t dummy a >|? fun l ->
      let s = Item_t (l, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> INil (loc, a, k));
        }
  | ( IIf_cons {loc; branch_if_cons; branch_if_nil; k},
      Item_t ((List_t (a, _meta) as l), s) ) ->
      ok
      @@ Ex_split_if
           {
             left_init_stack = Item_t (a, Item_t (l, s));
             left_branch = branch_if_cons;
             right_init_stack = s;
             right_branch = branch_if_nil;
             continuation = k;
             reconstruct =
               (fun branch_if_cons branch_if_nil k ->
                 IIf_cons {loc; branch_if_cons; branch_if_nil; k});
           }
  | IList_map (loc, body, ty, k), Item_t (List_t (a, _meta), s) ->
      let s = Item_t (a, s) in
      ok
      @@ Ex_split_loop_may_not_fail
           {
             body_init_stack = s;
             body;
             continuation = k;
             aft_body_stack_transform =
               (function
               | Item_t (b, s) -> list_t dummy b >|? fun l -> Item_t (l, s));
             reconstruct = (fun body k -> IList_map (loc, body, ty, k));
           }
  | IList_iter (loc, ty, body, k), Item_t (List_t (a, _meta), s) ->
      ok
      @@ Ex_split_loop_may_fail
           {
             body_init_stack = Item_t (a, s);
             body;
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun body k -> IList_iter (loc, ty, body, k));
           }
  | IList_size (loc, k), Item_t (_l, s) ->
      let s = Item_t (nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IList_size (loc, k));
           }
  | IEmpty_set (loc, a, k), s ->
      set_t dummy a >|? fun b ->
      let s = Item_t (b, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IEmpty_set (loc, a, k));
        }
  | ISet_iter (loc, a, body, k), Item_t (_b, s) ->
      ok
      @@ Ex_split_loop_may_fail
           {
             body_init_stack = Item_t (assert_some a, s);
             body;
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun body k -> ISet_iter (loc, a, body, k));
           }
  | ISet_mem (loc, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISet_mem (loc, k));
           }
  | ISet_update (loc, k), Item_t (_, Item_t (_, s)) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISet_update (loc, k));
           }
  | ISet_size (loc, k), Item_t (_, s) ->
      let s = Item_t (nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISet_size (loc, k));
           }
  | IEmpty_map (loc, cty, vty, k), s ->
      map_t dummy cty (assert_some vty) >|? fun m ->
      let s = Item_t (m, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IEmpty_map (loc, cty, vty, k));
        }
  | IMap_map (loc, ty, body, k), Item_t (Map_t (kty, vty, _meta), s) ->
      let (Map_t (key_ty, _, _)) = assert_some ty in
      pair_t dummy key_ty vty >|? fun (Ty_ex_c p) ->
      Ex_split_loop_may_not_fail
        {
          body_init_stack = Item_t (p, s);
          body;
          continuation = k;
          aft_body_stack_transform =
            (fun (Item_t (b, s)) ->
              map_t dummy kty b >|? fun m -> Item_t (m, s));
          reconstruct = (fun body k -> IMap_map (loc, ty, body, k));
        }
  | IMap_iter (loc, kvty, body, k), Item_t (_, stack) ->
      ok
      @@ Ex_split_loop_may_fail
           {
             body_init_stack = Item_t (assert_some kvty, stack);
             body;
             cont_init_stack = stack;
             continuation = k;
             reconstruct = (fun body k -> IMap_iter (loc, kvty, body, k));
           }
  | IMap_mem (loc, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMap_mem (loc, k));
           }
  | IMap_get (loc, k), Item_t (_, Item_t (Map_t (_kty, vty, _meta), s)) ->
      option_t dummy vty >|? fun o ->
      let s = Item_t (o, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IMap_get (loc, k));
        }
  | IMap_update (loc, k), Item_t (_, Item_t (_, s)) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMap_update (loc, k));
           }
  | IMap_get_and_update (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMap_get_and_update (loc, k));
           }
  | IMap_size (loc, k), Item_t (_, s) ->
      let s = Item_t (nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMap_size (loc, k));
           }
  | IEmpty_big_map (loc, cty, ty, k), s ->
      big_map_t dummy cty ty >|? fun b ->
      let s = Item_t (b, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IEmpty_big_map (loc, cty, ty, k));
        }
  | IBig_map_mem (loc, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IBig_map_mem (loc, k));
           }
  | IBig_map_get (loc, k), Item_t (_, Item_t (Big_map_t (_kty, vty, _meta), s))
    ->
      option_t dummy vty >|? fun o ->
      let s = Item_t (o, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IBig_map_get (loc, k));
        }
  | IBig_map_update (loc, k), Item_t (_, Item_t (_, s)) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IBig_map_update (loc, k));
           }
  | IBig_map_get_and_update (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IBig_map_get_and_update (loc, k));
           }
  | IConcat_string (loc, k), Item_t (_, s) ->
      let s = Item_t (string_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IConcat_string (loc, k));
           }
  | IConcat_string_pair (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IConcat_string_pair (loc, k));
           }
  | ISlice_string (loc, k), Item_t (_, Item_t (_, Item_t (_, s))) ->
      let s = Item_t (option_string_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISlice_string (loc, k));
           }
  | IString_size (loc, k), Item_t (_, s) ->
      let s = Item_t (nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IString_size (loc, k));
           }
  | IConcat_bytes (loc, k), Item_t (_, s) ->
      let s = Item_t (bytes_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IConcat_bytes (loc, k));
           }
  | IConcat_bytes_pair (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IConcat_bytes_pair (loc, k));
           }
  | ISlice_bytes (loc, k), Item_t (_, Item_t (_, Item_t (_, s))) ->
      let s = Item_t (option_bytes_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISlice_bytes (loc, k));
           }
  | IBytes_size (loc, k), Item_t (_, s) ->
      let s = Item_t (nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IBytes_size (loc, k));
           }
  | IAdd_seconds_to_timestamp (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAdd_seconds_to_timestamp (loc, k));
           }
  | IAdd_timestamp_to_seconds (loc, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (timestamp_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAdd_timestamp_to_seconds (loc, k));
           }
  | ISub_timestamp_seconds (loc, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (timestamp_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISub_timestamp_seconds (loc, k));
           }
  | IDiff_timestamps (loc, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (int_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IDiff_timestamps (loc, k));
           }
  | IAdd_tez (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAdd_tez (loc, k));
           }
  | ISub_tez (loc, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (option_mutez_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISub_tez (loc, k));
           }
  | ISub_tez_legacy (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISub_tez_legacy (loc, k));
           }
  | IMul_teznat (loc, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (mutez_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMul_teznat (loc, k));
           }
  | IMul_nattez (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMul_nattez (loc, k));
           }
  | IEdiv_teznat (loc, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (option_pair_mutez_mutez_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IEdiv_teznat (loc, k));
           }
  | IEdiv_tez (loc, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (option_pair_nat_mutez_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IEdiv_tez (loc, k));
           }
  | IOr (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IOr (loc, k));
           }
  | IAnd (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAnd (loc, k));
           }
  | IXor (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IXor (loc, k));
           }
  | INot (loc, k), s ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> INot (loc, k));
           }
  | IIs_nat (loc, k), Item_t (_, s) ->
      let s = Item_t (option_nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IIs_nat (loc, k));
           }
  | INeg (loc, k), Item_t (_, s) ->
      let s = Item_t (int_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> INeg (loc, k));
           }
  | IAbs_int (loc, k), Item_t (_, s) ->
      let s = Item_t (nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAbs_int (loc, k));
           }
  | IInt_nat (loc, k), Item_t (_, s) ->
      let s = Item_t (int_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IInt_nat (loc, k));
           }
  | IAdd_int (loc, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (int_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAdd_int (loc, k));
           }
  | IAdd_nat (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAdd_nat (loc, k));
           }
  | ISub_int (loc, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (int_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISub_int (loc, k));
           }
  | IMul_int (loc, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (int_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMul_int (loc, k));
           }
  | IMul_nat (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMul_nat (loc, k));
           }
  | IEdiv_int (loc, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (option_pair_int_nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IEdiv_int (loc, k));
           }
  | IEdiv_nat (loc, k), Item_t (_, Item_t (a, s)) ->
      pair_t dummy a nat_t >>? fun (Ty_ex_c p) ->
      option_t dummy p >|? fun o ->
      let s = Item_t (o, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IEdiv_nat (loc, k));
        }
  | ILsl_nat (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ILsl_nat (loc, k));
           }
  | ILsr_nat (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ILsr_nat (loc, k));
           }
  | IOr_nat (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IOr_nat (loc, k));
           }
  | IAnd_nat (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAnd_nat (loc, k));
           }
  | IAnd_int_nat (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAnd_int_nat (loc, k));
           }
  | IXor_nat (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IXor_nat (loc, k));
           }
  | INot_int (loc, k), Item_t (_, s) ->
      let s = Item_t (int_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> INot_int (loc, k));
           }
  | IIf {loc; branch_if_true; branch_if_false; k}, Item_t (_, s) ->
      ok
      @@ Ex_split_if
           {
             left_init_stack = s;
             left_branch = branch_if_true;
             right_init_stack = s;
             right_branch = branch_if_false;
             continuation = k;
             reconstruct =
               (fun branch_if_true branch_if_false k ->
                 IIf {loc; branch_if_true; branch_if_false; k});
           }
  | ILoop (loc, body, k), Item_t (_, s) ->
      ok
      @@ Ex_split_loop_may_fail
           {
             body_init_stack = s;
             body;
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun body k -> ILoop (loc, body, k));
           }
  | ILoop_left (loc, kl, kr), Item_t (Union_t (a, b, _meta, _), s) ->
      ok
      @@ Ex_split_loop_may_fail
           {
             body_init_stack = Item_t (a, s);
             body = kl;
             cont_init_stack = Item_t (b, s);
             continuation = kr;
             reconstruct = (fun kl kr -> ILoop_left (loc, kl, kr));
           }
  | IDip (loc, body, ty, k), Item_t (a, s) ->
      ok
      @@ Ex_split_loop_may_not_fail
           {
             body_init_stack = s;
             body;
             continuation = k;
             aft_body_stack_transform = (fun s -> ok @@ Item_t (a, s));
             reconstruct = (fun body k -> IDip (loc, body, ty, k));
           }
  | IExec (loc, sty, k), Item_t (_, Item_t (Lambda_t (_, b, _meta), s)) ->
      let s = Item_t (b, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IExec (loc, sty, k));
           }
  | ( IApply (loc, ty, k),
      Item_t (_, Item_t (Lambda_t (Pair_t (_, a, _, _), b, _), s)) ) ->
      lambda_t dummy a b >|? fun l ->
      let s = Item_t (l, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IApply (loc, ty, k));
        }
  | ILambda (loc, (Lam (desc, _) as l), k), s ->
      let (Item_t (a, Bot_t)) = desc.kbef in
      let (Item_t (b, Bot_t)) = desc.kaft in
      lambda_t dummy a b >|? fun lam ->
      let s = Item_t (lam, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> ILambda (loc, l, k));
        }
  | ILambda (loc, (LamRec (desc, _) as l), k), s ->
      let (Item_t (a, Item_t (Lambda_t _, Bot_t))) = desc.kbef in
      let (Item_t (b, Bot_t)) = desc.kaft in
      lambda_t dummy a b >|? fun lam ->
      let s = Item_t (lam, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> ILambda (loc, l, k));
        }
  | IFailwith (location, arg_ty), _ ->
      ok
      @@ Ex_split_failwith
           {location; arg_ty; cast = {cast = IFailwith (location, arg_ty)}}
  | ICompare (loc, ty, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (int_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ICompare (loc, ty, k));
           }
  | IEq (loc, k), Item_t (_, s) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IEq (loc, k));
           }
  | INeq (loc, k), Item_t (_, s) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> INeq (loc, k));
           }
  | ILt (loc, k), Item_t (_, s) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ILt (loc, k));
           }
  | IGt (loc, k), Item_t (_, s) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IGt (loc, k));
           }
  | ILe (loc, k), Item_t (_, s) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ILe (loc, k));
           }
  | IGe (loc, k), Item_t (_, s) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IGe (loc, k));
           }
  | IAddress (loc, k), Item_t (_, s) ->
      let s = Item_t (address_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAddress (loc, k));
           }
  | IContract (loc, ty, code, k), Item_t (_, s) ->
      contract_t dummy ty >>? fun c ->
      option_t dummy c >|? fun o ->
      let s = Item_t (o, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IContract (loc, ty, code, k));
        }
  | ITransfer_tokens (loc, k), Item_t (_, Item_t (_, Item_t (_, s))) ->
      let s = Item_t (operation_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ITransfer_tokens (loc, k));
           }
  | ( IView (loc, (View_signature {output_ty; _} as view_signature), sty, k),
      Item_t (_, Item_t (_, s)) ) ->
      option_t dummy output_ty >|? fun b ->
      let s = Item_t (b, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IView (loc, view_signature, sty, k));
        }
  | IImplicit_account (loc, k), Item_t (_, s) ->
      let s = Item_t (contract_unit_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IImplicit_account (loc, k));
           }
  | ( ICreate_contract {loc; storage_type; code; k},
      Item_t (_, Item_t (_, Item_t (_, s))) ) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = Item_t (operation_t, Item_t (address_t, s));
             continuation = k;
             reconstruct =
               (fun k -> ICreate_contract {loc; storage_type; code; k});
           }
  | ISet_delegate (loc, k), Item_t (_, s) ->
      let s = Item_t (operation_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISet_delegate (loc, k));
           }
  | INow (loc, k), s ->
      let s = Item_t (timestamp_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> INow (loc, k));
           }
  | IBalance (loc, k), s ->
      let s = Item_t (mutez_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IBalance (loc, k));
           }
  | ILevel (loc, k), s ->
      let s = Item_t (nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ILevel (loc, k));
           }
  | ICheck_signature (loc, k), Item_t (_, Item_t (_, Item_t (_, s))) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ICheck_signature (loc, k));
           }
  | IHash_key (loc, k), Item_t (_, s) ->
      let s = Item_t (key_hash_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IHash_key (loc, k));
           }
  | IPack (loc, ty, k), Item_t (_, s) ->
      let s = Item_t (bytes_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IPack (loc, ty, k));
           }
  | IUnpack (loc, ty, k), Item_t (_, s) ->
      option_t dummy ty >|? fun o ->
      let s = Item_t (o, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IUnpack (loc, ty, k));
        }
  | IBlake2b (loc, k), s ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IBlake2b (loc, k));
           }
  | ISha256 (loc, k), s ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISha256 (loc, k));
           }
  | ISha512 (loc, k), s ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISha512 (loc, k));
           }
  | ISource (loc, k), s ->
      let s = Item_t (address_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISource (loc, k));
           }
  | ISender (loc, k), s ->
      let s = Item_t (address_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISender (loc, k));
           }
  | ISelf (loc, ty, ep, k), s ->
      contract_t dummy ty >|? fun c ->
      let s = Item_t (c, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> ISelf (loc, ty, ep, k));
        }
  | ISelf_address (loc, k), s ->
      let s = Item_t (address_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISelf_address (loc, k));
           }
  | IAmount (loc, k), s ->
      let s = Item_t (mutez_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAmount (loc, k));
           }
  | ISapling_empty_state (loc, memo_size, k), s ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = Item_t (sapling_state_t ~memo_size, s);
             continuation = k;
             reconstruct = (fun k -> ISapling_empty_state (loc, memo_size, k));
           }
  | ISapling_verify_update_deprecated (loc, k), Item_t (_, Item_t (state_ty, s))
    ->
      pair_t dummy int_t state_ty >>? fun (Ty_ex_c pair_ty) ->
      option_t dummy pair_ty >|? fun ty ->
      Ex_split_kinstr
        {
          cont_init_stack = Item_t (ty, s);
          continuation = k;
          reconstruct = (fun k -> ISapling_verify_update_deprecated (loc, k));
        }
  | ISapling_verify_update (loc, k), Item_t (_, Item_t (state_ty, s)) ->
      pair_t dummy int_t state_ty >>? fun (Ty_ex_c int_state_ty) ->
      pair_t dummy bytes_t int_state_ty >>? fun (Ty_ex_c pair_ty) ->
      option_t dummy pair_ty >|? fun ty ->
      let s = Item_t (ty, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> ISapling_verify_update (loc, k));
        }
  | IDig (loc, n, p, k), s ->
      let (Item_t (b, s)) = stack_prefix_preservation_witness_split_input p s in
      let s = stack_prefix_preservation_witness_split_output p s in
      let s = Item_t (b, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IDig (loc, n, p, k));
           }
  | IDug (loc, n, p, k), Item_t (a, s) ->
      let s = stack_prefix_preservation_witness_split_input p s in
      let s = Item_t (a, s) in
      let s = stack_prefix_preservation_witness_split_output p s in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IDug (loc, n, p, k));
           }
  | IDipn (loc, n, p, k1, k2), s ->
      ok
      @@ Ex_split_loop_may_not_fail
           {
             body_init_stack = stack_prefix_preservation_witness_split_input p s;
             body = k1;
             continuation = k2;
             aft_body_stack_transform =
               (fun s ->
                 ok @@ stack_prefix_preservation_witness_split_output p s);
             reconstruct = (fun k1 k2 -> IDipn (loc, n, p, k1, k2));
           }
  | IDropn (loc, n, p, k), s ->
      let s = stack_prefix_preservation_witness_split_input p s in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IDropn (loc, n, p, k));
           }
  | IChainId (loc, k), s ->
      let s = Item_t (chain_id_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IChainId (loc, k));
           }
  | INever location, Item_t (arg_ty, _) ->
      ok
      @@ Ex_split_failwith {location; arg_ty; cast = {cast = INever location}}
  | IVoting_power (loc, k), Item_t (_, s) ->
      let s = Item_t (nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IVoting_power (loc, k));
           }
  | ITotal_voting_power (loc, k), s ->
      let s = Item_t (nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ITotal_voting_power (loc, k));
           }
  | IKeccak (loc, k), s ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IKeccak (loc, k));
           }
  | ISha3 (loc, k), s ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISha3 (loc, k));
           }
  | IAdd_bls12_381_g1 (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAdd_bls12_381_g1 (loc, k));
           }
  | IAdd_bls12_381_g2 (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAdd_bls12_381_g2 (loc, k));
           }
  | IAdd_bls12_381_fr (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAdd_bls12_381_fr (loc, k));
           }
  | IMul_bls12_381_g1 (loc, k), Item_t (g1, Item_t (_, s)) ->
      let s = Item_t (g1, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMul_bls12_381_g1 (loc, k));
           }
  | IMul_bls12_381_g2 (loc, k), Item_t (g2, Item_t (_, s)) ->
      let s = Item_t (g2, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMul_bls12_381_g2 (loc, k));
           }
  | IMul_bls12_381_fr (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMul_bls12_381_fr (loc, k));
           }
  | IMul_bls12_381_z_fr (loc, k), Item_t (fr, Item_t (_, s)) ->
      let s = Item_t (fr, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMul_bls12_381_z_fr (loc, k));
           }
  | IMul_bls12_381_fr_z (loc, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMul_bls12_381_fr_z (loc, k));
           }
  | IInt_bls12_381_fr (loc, k), Item_t (_, s) ->
      let s = Item_t (int_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IInt_bls12_381_fr (loc, k));
           }
  | INeg_bls12_381_g1 (loc, k), s ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> INeg_bls12_381_g1 (loc, k));
           }
  | INeg_bls12_381_g2 (loc, k), s ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> INeg_bls12_381_g2 (loc, k));
           }
  | INeg_bls12_381_fr (loc, k), s ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> INeg_bls12_381_fr (loc, k));
           }
  | IPairing_check_bls12_381 (loc, k), Item_t (_, s) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IPairing_check_bls12_381 (loc, k));
           }
  | IComb (loc, n, p, k), s ->
      let rec aux :
          type a b s c d t.
          (a, b * s) stack_ty ->
          (a, b, s, c, d, t) comb_gadt_witness ->
          (c, d * t) stack_ty tzresult =
       fun s w ->
        match (w, s) with
        | Comb_one, s -> ok s
        | Comb_succ w, Item_t (a, s) ->
            aux s w >>? fun (Item_t (c, t)) ->
            pair_t dummy a c >|? fun (Ty_ex_c p) -> Item_t (p, t)
      in
      aux s p >|? fun s ->
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IComb (loc, n, p, k));
        }
  | IUncomb (loc, n, p, k), s ->
      let rec aux :
          type a b s c d t.
          (a, b * s) stack_ty ->
          (a, b, s, c, d, t) uncomb_gadt_witness ->
          (c, d * t) stack_ty =
       fun s w ->
        match (w, s) with
        | Uncomb_one, s -> s
        | Uncomb_succ w, Item_t (Pair_t (a, b, _meta, _), s) ->
            let s = aux (Item_t (b, s)) w in
            Item_t (a, s)
      in
      let s = aux s p in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IUncomb (loc, n, p, k));
           }
  | IComb_get (loc, n, p, k), Item_t (c, s) ->
      let rec aux :
          type c cc a. (c, cc) ty -> (c, a) comb_get_gadt_witness -> a ty_ex_c =
       fun c w ->
        match (w, c) with
        | Comb_get_zero, c -> Ty_ex_c c
        | Comb_get_one, Pair_t (hd, _tl, _meta, _) -> Ty_ex_c hd
        | Comb_get_plus_two w, Pair_t (_hd, tl, _meta, _) -> aux tl w
      in
      let s =
        let (Ty_ex_c ty) = aux c p in
        Item_t (ty, s)
      in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IComb_get (loc, n, p, k));
           }
  | IComb_set (loc, n, p, k), Item_t (a, Item_t (b, s)) ->
      let rec aux :
          type a b c ca cb.
          (a, ca) ty ->
          (b, cb) ty ->
          (a, b, c) comb_set_gadt_witness ->
          c ty_ex_c tzresult =
       fun a b w ->
        match (w, b) with
        | Comb_set_zero, _ -> ok (Ty_ex_c a)
        | Comb_set_one, Pair_t (_hd, tl, _meta, _) -> pair_t dummy a tl
        | Comb_set_plus_two w, Pair_t (hd, tl, _meta, _) ->
            aux a tl w >>? fun (Ty_ex_c c) -> pair_t dummy hd c
      in
      aux a b p >|? fun (Ty_ex_c c) ->
      let s = Item_t (c, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IComb_set (loc, n, p, k));
        }
  | IDup_n (loc, n, p, k), s ->
      let rec aux :
          type a b s t.
          (a, b * s) stack_ty -> (a, b, s, t) dup_n_gadt_witness -> t ty_ex_c =
       fun s w ->
        match (w, s) with
        | Dup_n_succ w, Item_t (_, s) -> aux s w
        | Dup_n_zero, Item_t (a, _) -> Ty_ex_c a
      in
      let s =
        let (Ty_ex_c ty) = aux s p in
        Item_t (ty, s)
      in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IDup_n (loc, n, p, k));
           }
  | ITicket (loc, cty, k), Item_t (_, Item_t (_, s)) ->
      ticket_t dummy (assert_some cty) >>? option_t loc >|? fun t ->
      let s = Item_t (t, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> ITicket (loc, cty, k));
        }
  | ITicket_deprecated (loc, cty, k), Item_t (_, Item_t (_, s)) ->
      ticket_t dummy (assert_some cty) >|? fun t ->
      let s = Item_t (t, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> ITicket_deprecated (loc, cty, k));
        }
  | IRead_ticket (loc, a, k), s ->
      pair_t dummy (assert_some a) nat_t >>? fun (Ty_ex_c p) ->
      pair_t dummy address_t p >|? fun (Ty_ex_c t) ->
      let s = Item_t (t, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IRead_ticket (loc, a, k));
        }
  | ISplit_ticket (loc, k), Item_t (t, Item_t (_, s)) ->
      pair_t dummy t t >>? fun (Ty_ex_c p) ->
      option_t dummy p >|? fun o ->
      let s = Item_t (o, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> ISplit_ticket (loc, k));
        }
  | IJoin_tickets (loc, ty, k), Item_t (Pair_t (t, _t, _meta, _), s) ->
      option_t dummy t >|? fun o ->
      let s = Item_t (o, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IJoin_tickets (loc, ty, k));
        }
  | IOpen_chest (loc, k), Item_t (_, Item_t (_, Item_t (_, s))) ->
      let s = Item_t (union_bytes_bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IOpen_chest (loc, k));
           }
  | IMin_block_time (loc, k), s ->
      let s = Item_t (nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMin_block_time (loc, k));
           }
  | IEmit {loc; ty; unparsed_ty; tag; k}, Item_t (_, s) ->
      let s = Item_t (operation_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IEmit {loc; ty; unparsed_ty; tag; k});
           }
  | IEmit _, Bot_t -> .
  | IHalt loc, _s -> ok @@ Ex_split_halt loc
  | ILog (loc, _stack_ty, event, logger, continuation), stack ->
      ok
      @@ Ex_split_log
           {
             stack;
             continuation;
             reconstruct = (fun k -> ILog (loc, s, event, logger, k));
           }

let rec kinstr_final_stack_type :
    type a s r f.
    (a, s) stack_ty -> (a, s, r, f) kinstr -> (r, f) stack_ty option tzresult =
 fun s i ->
  kinstr_split s i >>? function
  | Ex_split_kinstr {cont_init_stack; continuation; _} ->
      kinstr_final_stack_type cont_init_stack continuation
  | Ex_split_log {stack; continuation; _} ->
      kinstr_final_stack_type stack continuation
  | Ex_split_loop_may_fail {cont_init_stack; continuation; _} ->
      kinstr_final_stack_type cont_init_stack continuation
  | Ex_split_loop_may_not_fail
      {body_init_stack; body; continuation; aft_body_stack_transform; _} -> (
      kinstr_final_stack_type body_init_stack body >>? function
      | Some after_body ->
          aft_body_stack_transform after_body >>? fun before_k ->
          kinstr_final_stack_type before_k continuation
      | None -> ok None)
  | Ex_split_if
      {
        left_init_stack;
        left_branch;
        right_init_stack;
        right_branch;
        continuation;
        _;
      } -> (
      kinstr_final_stack_type left_init_stack left_branch >>? function
      | Some after_branch_a ->
          kinstr_final_stack_type after_branch_a continuation
      | None -> (
          kinstr_final_stack_type right_init_stack right_branch >>? function
          | Some after_branch_b ->
              kinstr_final_stack_type after_branch_b continuation
          | None -> ok None))
  | Ex_split_halt _ -> ok @@ Some s
  | Ex_split_failwith {cast = {cast = _}; _} -> ok None

let rec branched_final_stack_type :
    type r f. (r, f) ex_init_stack_ty list -> (r, f) stack_ty option tzresult =
  function
  | [] -> ok None
  | Ex_init_stack_ty (init_sty, branch) :: bs -> (
      kinstr_final_stack_type init_sty branch >>? function
      | Some _ as sty -> ok sty
      | None -> branched_final_stack_type bs)

let kinstr_rewritek :
    type a s r f.
    (a, s) stack_ty ->
    (a, s, r, f) kinstr ->
    kinstr_rewritek ->
    (a, s, r, f) kinstr tzresult =
 fun s i f ->
  kinstr_split s i >>? function
  | Ex_split_kinstr {cont_init_stack; continuation; reconstruct} ->
      ok @@ reconstruct (f.apply cont_init_stack continuation)
  | Ex_split_log {continuation; reconstruct; _} ->
      ok @@ reconstruct continuation
  | Ex_split_loop_may_fail
      {body_init_stack; body; cont_init_stack; continuation; reconstruct} ->
      ok
      @@ reconstruct
           (f.apply body_init_stack body)
           (f.apply cont_init_stack continuation)
  | Ex_split_loop_may_not_fail
      {
        body_init_stack;
        body;
        continuation;
        aft_body_stack_transform;
        reconstruct;
      } ->
      (kinstr_final_stack_type body_init_stack body >>? function
       | Some after_body ->
           aft_body_stack_transform after_body >|? fun before_k ->
           f.apply before_k continuation
       | None -> ok continuation)
      >|? fun k -> reconstruct (f.apply body_init_stack body) k
  | Ex_split_if
      {
        left_init_stack;
        left_branch;
        right_init_stack;
        right_branch;
        continuation;
        reconstruct;
      } ->
      (kinstr_final_stack_type left_init_stack left_branch >>? function
       | Some after_left_branch -> ok @@ f.apply after_left_branch continuation
       | None -> (
           kinstr_final_stack_type right_init_stack right_branch >>? function
           | Some after_right_branch ->
               ok @@ f.apply after_right_branch continuation
           | None -> ok continuation))
      >|? fun k ->
      reconstruct
        (f.apply left_init_stack left_branch)
        (f.apply right_init_stack right_branch)
        k
  | Ex_split_halt loc -> ok @@ IHalt loc
  | Ex_split_failwith {location; arg_ty; _} -> ok @@ IFailwith (location, arg_ty)

let log_entry logger ctxt gas k sty accu stack =
  let ctxt = Local_gas_counter.update_context gas ctxt in
  logger.log_entry k ctxt (kinstr_location k) sty (accu, stack)

let log_exit logger ctxt gas loc_prev k sty accu stack =
  let _loc = kinstr_location k in
  let ctxt = Local_gas_counter.update_context gas ctxt in
  logger.log_exit k ctxt loc_prev sty (accu, stack)

let log_control logger ks = logger.log_control ks

(* [log_kinstr logger i] emits an instruction to instrument the
   execution of [i] with [logger]. *)
let log_kinstr logger sty i = ILog (kinstr_location i, sty, LogEntry, logger, i)

(* [log_next_kinstr logger i] instruments the next instruction of [i]
   with the [logger].

   Notice that the instrumentation breaks the sharing of continuations
   that is normally enforced between branches of conditionals. This
   has a performance cost. Anyway, the instrumentation allocates many
   new [ILog] instructions and [KLog] continuations which makes
   the execution of instrumented code significantly slower than
   non-instrumented code. "Zero-cost logging" means that the normal
   non-instrumented execution is not impacted by the ability to
   instrument it, not that the logging itself has no cost.
*)
let log_next_kinstr logger sty i =
  let apply sty k =
    ILog
      ( kinstr_location k,
        sty,
        LogExit (kinstr_location i),
        logger,
        log_kinstr logger sty k )
  in
  kinstr_rewritek sty i {apply}

let instrument_cont :
    type a b c d.
    logger ->
    (a, b) stack_ty ->
    (a, b, c, d) continuation ->
    (a, b, c, d) continuation =
 fun logger sty -> function KLog _ as k -> k | k -> KLog (k, sty, logger)

let log_next_continuation :
    type a b c d.
    logger ->
    (a, b) stack_ty ->
    (a, b, c, d) continuation ->
    (a, b, c, d) continuation tzresult =
 fun logger stack_ty cont ->
  let enable_log sty ki = log_kinstr logger sty ki in
  match cont with
  | KCons (ki, k) -> (
      let ki' = enable_log stack_ty ki in
      kinstr_final_stack_type stack_ty ki >|? function
      | None -> KCons (ki', k)
      | Some sty -> KCons (ki', instrument_cont logger sty k))
  | KLoop_in (ki, k) ->
      let (Item_t (Bool_t, sty)) = stack_ty in
      ok @@ KLoop_in (enable_log sty ki, instrument_cont logger sty k)
  | KReturn (stack, sty, k) ->
      let k' = instrument_cont logger (assert_some sty) k in
      ok @@ KReturn (stack, sty, k')
  | KLoop_in_left (ki, k) ->
      let (Item_t (Union_t (a_ty, b_ty, _, _), rest)) = stack_ty in
      let ki' = enable_log (Item_t (a_ty, rest)) ki in
      let k' = instrument_cont logger (Item_t (b_ty, rest)) k in
      ok @@ KLoop_in_left (ki', k')
  | KUndip (x, ty, k) ->
      let k' = instrument_cont logger (Item_t (assert_some ty, stack_ty)) k in
      ok @@ KUndip (x, ty, k')
  | KIter (body, xty, xs, k) ->
      let body' = enable_log (Item_t (assert_some xty, stack_ty)) body in
      let k' = instrument_cont logger stack_ty k in
      ok @@ KIter (body', xty, xs, k')
  | KList_enter_body (body, xs, ys, ty, len, k) ->
      let k' = instrument_cont logger (Item_t (assert_some ty, stack_ty)) k in
      ok @@ KList_enter_body (body, xs, ys, ty, len, k')
  | KList_exit_body (body, xs, ys, ty, len, k) ->
      let (Item_t (_, sty)) = stack_ty in
      let k' = instrument_cont logger (Item_t (assert_some ty, sty)) k in
      ok @@ KList_exit_body (body, xs, ys, ty, len, k')
  | KMap_enter_body (body, xs, ys, ty, k) ->
      let k' = instrument_cont logger (Item_t (assert_some ty, stack_ty)) k in
      ok @@ KMap_enter_body (body, xs, ys, ty, k')
  | KMap_exit_body (body, xs, ys, yk, ty, k) ->
      let (Item_t (_, sty)) = stack_ty in
      let k' = instrument_cont logger (Item_t (assert_some ty, sty)) k in
      ok @@ KMap_exit_body (body, xs, ys, yk, ty, k')
  | KMap_head (_, _)
  | KView_exit (_, _)
  | KLog _ (* This case should never happen. *) | KNil ->
      ok cont

let rec dipn_stack_ty :
    type a s e z c u d w.
    (a, s, e, z, c, u, d, w) stack_prefix_preservation_witness ->
    (c, u) stack_ty ->
    (a, s) stack_ty =
 fun witness stack ->
  match (witness, stack) with
  | KPrefix (_, _, witness'), Item_t (_, sty) -> dipn_stack_ty witness' sty
  | KRest, sty -> sty
