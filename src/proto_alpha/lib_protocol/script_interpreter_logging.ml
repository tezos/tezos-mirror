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
  | Ex_split_halt : ('a, 's) kinfo -> ('a, 's, 'a, 's) ex_split_kinstr
  | Ex_split_failwith : {
      kinfo : ('a, 's) kinfo;
      location : Script.location;
      arg_ty : ('a, _) ty;
      cast : ('a, 's) failed_kinstr_cast;
    }
      -> ('a, 's, 'r, 'f) ex_split_kinstr

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

let kinstr_split :
    type a s r f.
    (a, s) stack_ty ->
    (a, s, r, f) kinstr ->
    (a, s, r, f) ex_split_kinstr tzresult =
 fun s i ->
  let loc = Micheline.dummy_location in
  match (i, s) with
  | IDrop (kinfo, k), Item_t (_a, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IDrop (kinfo, k));
           }
  | IDup (kinfo, k), Item_t (a, s) ->
      let s = Item_t (a, Item_t (a, s)) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IDup (kinfo, k));
           }
  | ISwap (kinfo, k), Item_t (a, Item_t (b, s)) ->
      let s = Item_t (b, Item_t (a, s)) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISwap (kinfo, k));
           }
  | IConst (kinfo, a, x, k), s ->
      let s = Item_t (a, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IConst (kinfo, a, x, k));
           }
  | ICons_pair (kinfo, k), Item_t (a, Item_t (b, s)) ->
      pair_t loc a b >|? fun (Ty_ex_c c) ->
      let s = Item_t (c, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> ICons_pair (kinfo, k));
        }
  | ICar (kinfo, k), Item_t (Pair_t (a, _b, _meta, _), s) ->
      let s = Item_t (a, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ICar (kinfo, k));
           }
  | ICdr (kinfo, k), Item_t (Pair_t (_a, b, _meta, _), s) ->
      let s = Item_t (b, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ICdr (kinfo, k));
           }
  | IUnpair (kinfo, k), Item_t (Pair_t (a, b, _meta, _), s) ->
      let s = Item_t (a, Item_t (b, s)) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IUnpair (kinfo, k));
           }
  | ICons_some (kinfo, k), Item_t (a, s) ->
      option_t loc a >|? fun o ->
      let s = Item_t (o, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> ICons_some (kinfo, k));
        }
  | ICons_none (kinfo, a, k), s ->
      option_t loc a >|? fun o ->
      let s = Item_t (o, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> ICons_none (kinfo, a, k));
        }
  | ( IIf_none {kinfo; branch_if_none; branch_if_some; k},
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
                 IIf_none {kinfo; branch_if_none; branch_if_some; k});
           }
  | IOpt_map {kinfo; body; k}, Item_t (Option_t (a, _meta, _), s) ->
      ok
      @@ Ex_split_loop_may_not_fail
           {
             body_init_stack = Item_t (a, s);
             body;
             continuation = k;
             aft_body_stack_transform =
               (function
               | Item_t (b, s) -> option_t loc b >|? fun o -> Item_t (o, s));
             reconstruct = (fun body k -> IOpt_map {kinfo; body; k});
           }
  | ICons_left (kinfo, b, k), Item_t (a, s) ->
      union_t loc a b >|? fun (Ty_ex_c c) ->
      let s = Item_t (c, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> ICons_left (kinfo, b, k));
        }
  | ICons_right (kinfo, a, k), Item_t (b, s) ->
      union_t loc a b >|? fun (Ty_ex_c c) ->
      let s = Item_t (c, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> ICons_right (kinfo, a, k));
        }
  | ( IIf_left {kinfo; branch_if_left; branch_if_right; k},
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
                 IIf_left {kinfo; branch_if_left; branch_if_right; k});
           }
  | ICons_list (kinfo, k), Item_t (_a, Item_t (l, s)) ->
      let s = Item_t (l, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ICons_list (kinfo, k));
           }
  | INil (kinfo, a, k), s ->
      list_t loc a >|? fun l ->
      let s = Item_t (l, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> INil (kinfo, a, k));
        }
  | ( IIf_cons {kinfo; branch_if_cons; branch_if_nil; k},
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
                 IIf_cons {kinfo; branch_if_cons; branch_if_nil; k});
           }
  | IList_map (kinfo, body, k), Item_t (List_t (a, _meta), s) ->
      let s = Item_t (a, s) in
      ok
      @@ Ex_split_loop_may_not_fail
           {
             body_init_stack = s;
             body;
             continuation = k;
             aft_body_stack_transform =
               (function
               | Item_t (b, s) -> list_t loc b >|? fun l -> Item_t (l, s));
             reconstruct = (fun body k -> IList_map (kinfo, body, k));
           }
  | IList_iter (kinfo, ty, body, k), Item_t (List_t (a, _meta), s) ->
      ok
      @@ Ex_split_loop_may_fail
           {
             body_init_stack = Item_t (a, s);
             body;
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun body k -> IList_iter (kinfo, ty, body, k));
           }
  | IList_size (kinfo, k), Item_t (_l, s) ->
      let s = Item_t (nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IList_size (kinfo, k));
           }
  | IEmpty_set (kinfo, a, k), s ->
      set_t loc a >|? fun b ->
      let s = Item_t (b, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IEmpty_set (kinfo, a, k));
        }
  | ISet_iter (kinfo, a, body, k), Item_t (_b, s) ->
      ok
      @@ Ex_split_loop_may_fail
           {
             body_init_stack = Item_t (a, s);
             body;
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun body k -> ISet_iter (kinfo, a, body, k));
           }
  | ISet_mem (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISet_mem (kinfo, k));
           }
  | ISet_update (kinfo, k), Item_t (_, Item_t (_, s)) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISet_update (kinfo, k));
           }
  | ISet_size (kinfo, k), Item_t (_, s) ->
      let s = Item_t (nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISet_size (kinfo, k));
           }
  | IEmpty_map (kinfo, cty, vty, k), s ->
      map_t loc cty vty >|? fun m ->
      let s = Item_t (m, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IEmpty_map (kinfo, cty, vty, k));
        }
  | IMap_map (kinfo, key_ty, body, k), Item_t (Map_t (kty, vty, _meta), s) ->
      pair_t loc key_ty vty >|? fun (Ty_ex_c p) ->
      Ex_split_loop_may_not_fail
        {
          body_init_stack = Item_t (p, s);
          body;
          continuation = k;
          aft_body_stack_transform =
            (fun (Item_t (b, s)) -> map_t loc kty b >|? fun m -> Item_t (m, s));
          reconstruct = (fun body k -> IMap_map (kinfo, key_ty, body, k));
        }
  | IMap_iter (kinfo, kvty, body, k), Item_t (_, stack) ->
      ok
      @@ Ex_split_loop_may_fail
           {
             body_init_stack = Item_t (kvty, stack);
             body;
             cont_init_stack = stack;
             continuation = k;
             reconstruct = (fun body k -> IMap_iter (kinfo, kvty, body, k));
           }
  | IMap_mem (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMap_mem (kinfo, k));
           }
  | IMap_get (kinfo, k), Item_t (_, Item_t (Map_t (_kty, vty, _meta), s)) ->
      option_t loc vty >|? fun o ->
      let s = Item_t (o, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IMap_get (kinfo, k));
        }
  | IMap_update (kinfo, k), Item_t (_, Item_t (_, s)) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMap_update (kinfo, k));
           }
  | IMap_get_and_update (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMap_get_and_update (kinfo, k));
           }
  | IMap_size (kinfo, k), Item_t (_, s) ->
      let s = Item_t (nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMap_size (kinfo, k));
           }
  | IEmpty_big_map (kinfo, cty, ty, k), s ->
      big_map_t loc cty ty >|? fun b ->
      let s = Item_t (b, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IEmpty_big_map (kinfo, cty, ty, k));
        }
  | IBig_map_mem (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IBig_map_mem (kinfo, k));
           }
  | IBig_map_get (kinfo, k), Item_t (_, Item_t (Big_map_t (_kty, vty, _meta), s))
    ->
      option_t loc vty >|? fun o ->
      let s = Item_t (o, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IBig_map_get (kinfo, k));
        }
  | IBig_map_update (kinfo, k), Item_t (_, Item_t (_, s)) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IBig_map_update (kinfo, k));
           }
  | IBig_map_get_and_update (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IBig_map_get_and_update (kinfo, k));
           }
  | IConcat_string (kinfo, k), Item_t (_, s) ->
      let s = Item_t (string_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IConcat_string (kinfo, k));
           }
  | IConcat_string_pair (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IConcat_string_pair (kinfo, k));
           }
  | ISlice_string (kinfo, k), Item_t (_, Item_t (_, Item_t (_, s))) ->
      let s = Item_t (option_string_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISlice_string (kinfo, k));
           }
  | IString_size (kinfo, k), Item_t (_, s) ->
      let s = Item_t (nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IString_size (kinfo, k));
           }
  | IConcat_bytes (kinfo, k), Item_t (_, s) ->
      let s = Item_t (bytes_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IConcat_bytes (kinfo, k));
           }
  | IConcat_bytes_pair (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IConcat_bytes_pair (kinfo, k));
           }
  | ISlice_bytes (kinfo, k), Item_t (_, Item_t (_, Item_t (_, s))) ->
      let s = Item_t (option_bytes_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISlice_bytes (kinfo, k));
           }
  | IBytes_size (kinfo, k), Item_t (_, s) ->
      let s = Item_t (nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IBytes_size (kinfo, k));
           }
  | IAdd_seconds_to_timestamp (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAdd_seconds_to_timestamp (kinfo, k));
           }
  | IAdd_timestamp_to_seconds (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (timestamp_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAdd_timestamp_to_seconds (kinfo, k));
           }
  | ISub_timestamp_seconds (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (timestamp_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISub_timestamp_seconds (kinfo, k));
           }
  | IDiff_timestamps (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (int_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IDiff_timestamps (kinfo, k));
           }
  | IAdd_tez (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAdd_tez (kinfo, k));
           }
  | ISub_tez (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (option_mutez_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISub_tez (kinfo, k));
           }
  | ISub_tez_legacy (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISub_tez_legacy (kinfo, k));
           }
  | IMul_teznat (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (mutez_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMul_teznat (kinfo, k));
           }
  | IMul_nattez (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMul_nattez (kinfo, k));
           }
  | IEdiv_teznat (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (option_pair_mutez_mutez_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IEdiv_teznat (kinfo, k));
           }
  | IEdiv_tez (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (option_pair_nat_mutez_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IEdiv_tez (kinfo, k));
           }
  | IOr (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IOr (kinfo, k));
           }
  | IAnd (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAnd (kinfo, k));
           }
  | IXor (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IXor (kinfo, k));
           }
  | INot (kinfo, k), s ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> INot (kinfo, k));
           }
  | IIs_nat (kinfo, k), Item_t (_, s) ->
      let s = Item_t (option_nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IIs_nat (kinfo, k));
           }
  | INeg (kinfo, k), Item_t (_, s) ->
      let s = Item_t (int_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> INeg (kinfo, k));
           }
  | IAbs_int (kinfo, k), Item_t (_, s) ->
      let s = Item_t (nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAbs_int (kinfo, k));
           }
  | IInt_nat (kinfo, k), Item_t (_, s) ->
      let s = Item_t (int_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IInt_nat (kinfo, k));
           }
  | IAdd_int (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (int_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAdd_int (kinfo, k));
           }
  | IAdd_nat (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAdd_nat (kinfo, k));
           }
  | ISub_int (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (int_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISub_int (kinfo, k));
           }
  | IMul_int (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (int_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMul_int (kinfo, k));
           }
  | IMul_nat (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMul_nat (kinfo, k));
           }
  | IEdiv_int (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (option_pair_int_nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IEdiv_int (kinfo, k));
           }
  | IEdiv_nat (kinfo, k), Item_t (_, Item_t (a, s)) ->
      pair_t loc a nat_t >>? fun (Ty_ex_c p) ->
      option_t loc p >|? fun o ->
      let s = Item_t (o, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IEdiv_nat (kinfo, k));
        }
  | ILsl_nat (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ILsl_nat (kinfo, k));
           }
  | ILsr_nat (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ILsr_nat (kinfo, k));
           }
  | IOr_nat (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IOr_nat (kinfo, k));
           }
  | IAnd_nat (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAnd_nat (kinfo, k));
           }
  | IAnd_int_nat (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAnd_int_nat (kinfo, k));
           }
  | IXor_nat (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IXor_nat (kinfo, k));
           }
  | INot_int (kinfo, k), Item_t (_, s) ->
      let s = Item_t (int_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> INot_int (kinfo, k));
           }
  | IIf {kinfo; branch_if_true; branch_if_false; k}, Item_t (_, s) ->
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
                 IIf {kinfo; branch_if_true; branch_if_false; k});
           }
  | ILoop (kinfo, body, k), Item_t (_, s) ->
      ok
      @@ Ex_split_loop_may_fail
           {
             body_init_stack = s;
             body;
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun body k -> ILoop (kinfo, body, k));
           }
  | ILoop_left (kinfo, kl, kr), Item_t (Union_t (a, b, _meta, _), s) ->
      ok
      @@ Ex_split_loop_may_fail
           {
             body_init_stack = Item_t (a, s);
             body = kl;
             cont_init_stack = Item_t (b, s);
             continuation = kr;
             reconstruct = (fun kl kr -> ILoop_left (kinfo, kl, kr));
           }
  | IDip (kinfo, body, k), Item_t (a, s) ->
      ok
      @@ Ex_split_loop_may_not_fail
           {
             body_init_stack = s;
             body;
             continuation = k;
             aft_body_stack_transform = (fun s -> ok @@ Item_t (a, s));
             reconstruct = (fun body k -> IDip (kinfo, body, k));
           }
  | IExec (kinfo, k), Item_t (_, Item_t (Lambda_t (_, b, _meta), s)) ->
      let s = Item_t (b, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IExec (kinfo, k));
           }
  | ( IApply (kinfo, ty, k),
      Item_t (_, Item_t (Lambda_t (Pair_t (_, a, _, _), b, _), s)) ) ->
      lambda_t loc a b >|? fun l ->
      let s = Item_t (l, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IApply (kinfo, ty, k));
        }
  | ILambda (kinfo, l, k), s ->
      let (Lam (desc, _)) = l in
      let (Item_t (a, Bot_t)) = desc.kbef in
      let (Item_t (b, Bot_t)) = desc.kaft in
      lambda_t loc a b >|? fun lam ->
      let s = Item_t (lam, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> ILambda (kinfo, l, k));
        }
  | IFailwith (kinfo, location, arg_ty), _ ->
      ok
      @@ Ex_split_failwith
           {
             kinfo;
             location;
             arg_ty;
             cast = {cast = IFailwith (kinfo, location, arg_ty)};
           }
  | ICompare (kinfo, ty, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (int_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ICompare (kinfo, ty, k));
           }
  | IEq (kinfo, k), Item_t (_, s) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IEq (kinfo, k));
           }
  | INeq (kinfo, k), Item_t (_, s) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> INeq (kinfo, k));
           }
  | ILt (kinfo, k), Item_t (_, s) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ILt (kinfo, k));
           }
  | IGt (kinfo, k), Item_t (_, s) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IGt (kinfo, k));
           }
  | ILe (kinfo, k), Item_t (_, s) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ILe (kinfo, k));
           }
  | IGe (kinfo, k), Item_t (_, s) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IGe (kinfo, k));
           }
  | IAddress (kinfo, k), Item_t (_, s) ->
      let s = Item_t (address_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAddress (kinfo, k));
           }
  | IContract (kinfo, ty, code, k), Item_t (_, s) ->
      contract_t loc ty >>? fun c ->
      option_t loc c >|? fun o ->
      let s = Item_t (o, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IContract (kinfo, ty, code, k));
        }
  | ITransfer_tokens (kinfo, k), Item_t (_, Item_t (_, Item_t (_, s))) ->
      let s = Item_t (operation_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ITransfer_tokens (kinfo, k));
           }
  | ( IView (kinfo, (View_signature {output_ty; _} as view_signature), k),
      Item_t (_, Item_t (_, s)) ) ->
      option_t loc output_ty >|? fun b ->
      let s = Item_t (b, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IView (kinfo, view_signature, k));
        }
  | IImplicit_account (kinfo, k), Item_t (_, s) ->
      let s = Item_t (contract_unit_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IImplicit_account (kinfo, k));
           }
  | ( ICreate_contract {kinfo; storage_type; code; k},
      Item_t (_, Item_t (_, Item_t (_, s))) ) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = Item_t (operation_t, Item_t (address_t, s));
             continuation = k;
             reconstruct =
               (fun k -> ICreate_contract {kinfo; storage_type; code; k});
           }
  | ISet_delegate (kinfo, k), Item_t (_, s) ->
      let s = Item_t (operation_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISet_delegate (kinfo, k));
           }
  | INow (kinfo, k), s ->
      let s = Item_t (timestamp_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> INow (kinfo, k));
           }
  | IBalance (kinfo, k), s ->
      let s = Item_t (mutez_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IBalance (kinfo, k));
           }
  | ILevel (kinfo, k), s ->
      let s = Item_t (nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ILevel (kinfo, k));
           }
  | ICheck_signature (kinfo, k), Item_t (_, Item_t (_, Item_t (_, s))) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ICheck_signature (kinfo, k));
           }
  | IHash_key (kinfo, k), Item_t (_, s) ->
      let s = Item_t (key_hash_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IHash_key (kinfo, k));
           }
  | IPack (kinfo, ty, k), Item_t (_, s) ->
      let s = Item_t (bytes_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IPack (kinfo, ty, k));
           }
  | IUnpack (kinfo, ty, k), Item_t (_, s) ->
      option_t loc ty >|? fun o ->
      let s = Item_t (o, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IUnpack (kinfo, ty, k));
        }
  | IBlake2b (kinfo, k), s ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IBlake2b (kinfo, k));
           }
  | ISha256 (kinfo, k), s ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISha256 (kinfo, k));
           }
  | ISha512 (kinfo, k), s ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISha512 (kinfo, k));
           }
  | ISource (kinfo, k), s ->
      let s = Item_t (address_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISource (kinfo, k));
           }
  | ISender (kinfo, k), s ->
      let s = Item_t (address_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISender (kinfo, k));
           }
  | ISelf (kinfo, ty, ep, k), s ->
      contract_t loc ty >|? fun c ->
      let s = Item_t (c, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> ISelf (kinfo, ty, ep, k));
        }
  | ISelf_address (kinfo, k), s ->
      let s = Item_t (address_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISelf_address (kinfo, k));
           }
  | IAmount (kinfo, k), s ->
      let s = Item_t (mutez_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAmount (kinfo, k));
           }
  | ISapling_empty_state (kinfo, memo_size, k), s ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = Item_t (sapling_state_t ~memo_size, s);
             continuation = k;
             reconstruct = (fun k -> ISapling_empty_state (kinfo, memo_size, k));
           }
  | ( ISapling_verify_update_deprecated (kinfo, k),
      Item_t (_, Item_t (state_ty, s)) ) ->
      pair_t loc int_t state_ty >>? fun (Ty_ex_c pair_ty) ->
      option_t loc pair_ty >|? fun ty ->
      Ex_split_kinstr
        {
          cont_init_stack = Item_t (ty, s);
          continuation = k;
          reconstruct = (fun k -> ISapling_verify_update_deprecated (kinfo, k));
        }
  | ISapling_verify_update (kinfo, k), Item_t (_, Item_t (state_ty, s)) ->
      pair_t loc int_t state_ty >>? fun (Ty_ex_c int_state_ty) ->
      pair_t loc bytes_t int_state_ty >>? fun (Ty_ex_c pair_ty) ->
      option_t loc pair_ty >|? fun ty ->
      let s = Item_t (ty, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> ISapling_verify_update (kinfo, k));
        }
  | IDig (kinfo, n, p, k), s ->
      let (Item_t (b, s)) = stack_prefix_preservation_witness_split_input p s in
      let s = stack_prefix_preservation_witness_split_output p s in
      let s = Item_t (b, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IDig (kinfo, n, p, k));
           }
  | IDug (kinfo, n, p, k), Item_t (a, s) ->
      let s = stack_prefix_preservation_witness_split_input p s in
      let s = Item_t (a, s) in
      let s = stack_prefix_preservation_witness_split_output p s in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IDug (kinfo, n, p, k));
           }
  | IDipn (kinfo, n, p, k1, k2), s ->
      ok
      @@ Ex_split_loop_may_not_fail
           {
             body_init_stack = stack_prefix_preservation_witness_split_input p s;
             body = k1;
             continuation = k2;
             aft_body_stack_transform =
               (fun s ->
                 ok @@ stack_prefix_preservation_witness_split_output p s);
             reconstruct = (fun k1 k2 -> IDipn (kinfo, n, p, k1, k2));
           }
  | IDropn (kinfo, n, p, k), s ->
      let s = stack_prefix_preservation_witness_split_input p s in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IDropn (kinfo, n, p, k));
           }
  | IChainId (kinfo, k), s ->
      let s = Item_t (chain_id_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IChainId (kinfo, k));
           }
  | INever kinfo, Item_t (arg_ty, _) ->
      ok
      @@ Ex_split_failwith
           {kinfo; location = loc; arg_ty; cast = {cast = INever kinfo}}
  | IVoting_power (kinfo, k), Item_t (_, s) ->
      let s = Item_t (nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IVoting_power (kinfo, k));
           }
  | ITotal_voting_power (kinfo, k), s ->
      let s = Item_t (nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ITotal_voting_power (kinfo, k));
           }
  | IKeccak (kinfo, k), s ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IKeccak (kinfo, k));
           }
  | ISha3 (kinfo, k), s ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> ISha3 (kinfo, k));
           }
  | IAdd_bls12_381_g1 (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAdd_bls12_381_g1 (kinfo, k));
           }
  | IAdd_bls12_381_g2 (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAdd_bls12_381_g2 (kinfo, k));
           }
  | IAdd_bls12_381_fr (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IAdd_bls12_381_fr (kinfo, k));
           }
  | IMul_bls12_381_g1 (kinfo, k), Item_t (g1, Item_t (_, s)) ->
      let s = Item_t (g1, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMul_bls12_381_g1 (kinfo, k));
           }
  | IMul_bls12_381_g2 (kinfo, k), Item_t (g2, Item_t (_, s)) ->
      let s = Item_t (g2, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMul_bls12_381_g2 (kinfo, k));
           }
  | IMul_bls12_381_fr (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMul_bls12_381_fr (kinfo, k));
           }
  | IMul_bls12_381_z_fr (kinfo, k), Item_t (fr, Item_t (_, s)) ->
      let s = Item_t (fr, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMul_bls12_381_z_fr (kinfo, k));
           }
  | IMul_bls12_381_fr_z (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMul_bls12_381_fr_z (kinfo, k));
           }
  | IInt_bls12_381_fr (kinfo, k), Item_t (_, s) ->
      let s = Item_t (int_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IInt_bls12_381_fr (kinfo, k));
           }
  | INeg_bls12_381_g1 (kinfo, k), s ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> INeg_bls12_381_g1 (kinfo, k));
           }
  | INeg_bls12_381_g2 (kinfo, k), s ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> INeg_bls12_381_g2 (kinfo, k));
           }
  | INeg_bls12_381_fr (kinfo, k), s ->
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> INeg_bls12_381_fr (kinfo, k));
           }
  | IPairing_check_bls12_381 (kinfo, k), Item_t (_, s) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IPairing_check_bls12_381 (kinfo, k));
           }
  | IComb (kinfo, n, p, k), s ->
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
            pair_t loc a c >|? fun (Ty_ex_c p) -> Item_t (p, t)
      in
      aux s p >|? fun s ->
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IComb (kinfo, n, p, k));
        }
  | IUncomb (kinfo, n, p, k), s ->
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
             reconstruct = (fun k -> IUncomb (kinfo, n, p, k));
           }
  | IComb_get (kinfo, n, p, k), Item_t (c, s) ->
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
             reconstruct = (fun k -> IComb_get (kinfo, n, p, k));
           }
  | IComb_set (kinfo, n, p, k), Item_t (a, Item_t (b, s)) ->
      let rec aux :
          type a b c ca cb.
          (a, ca) ty ->
          (b, cb) ty ->
          (a, b, c) comb_set_gadt_witness ->
          c ty_ex_c tzresult =
       fun a b w ->
        match (w, b) with
        | Comb_set_zero, _ -> ok (Ty_ex_c a)
        | Comb_set_one, Pair_t (_hd, tl, _meta, _) -> pair_t loc a tl
        | Comb_set_plus_two w, Pair_t (hd, tl, _meta, _) ->
            aux a tl w >>? fun (Ty_ex_c c) -> pair_t loc hd c
      in
      aux a b p >|? fun (Ty_ex_c c) ->
      let s = Item_t (c, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IComb_set (kinfo, n, p, k));
        }
  | IDup_n (kinfo, n, p, k), s ->
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
             reconstruct = (fun k -> IDup_n (kinfo, n, p, k));
           }
  | ITicket (kinfo, cty, k), Item_t (_, Item_t (_, s)) ->
      ticket_t loc cty >|? fun t ->
      let s = Item_t (t, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> ITicket (kinfo, cty, k));
        }
  | IRead_ticket (kinfo, a, k), s ->
      pair_t loc a nat_t >>? fun (Ty_ex_c p) ->
      pair_t loc address_t p >|? fun (Ty_ex_c t) ->
      let s = Item_t (t, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IRead_ticket (kinfo, a, k));
        }
  | ISplit_ticket (kinfo, k), Item_t (t, Item_t (_, s)) ->
      pair_t loc t t >>? fun (Ty_ex_c p) ->
      option_t loc p >|? fun o ->
      let s = Item_t (o, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> ISplit_ticket (kinfo, k));
        }
  | IJoin_tickets (kinfo, ty, k), Item_t (Pair_t (t, _t, _meta, _), s) ->
      option_t loc t >|? fun o ->
      let s = Item_t (o, s) in
      Ex_split_kinstr
        {
          cont_init_stack = s;
          continuation = k;
          reconstruct = (fun k -> IJoin_tickets (kinfo, ty, k));
        }
  | IOpen_chest (kinfo, k), Item_t (_, Item_t (_, Item_t (_, s))) ->
      let s = Item_t (union_bytes_bool_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IOpen_chest (kinfo, k));
           }
  | IMin_block_time (kinfo, k), s ->
      let s = Item_t (nat_t, s) in
      ok
      @@ Ex_split_kinstr
           {
             cont_init_stack = s;
             continuation = k;
             reconstruct = (fun k -> IMin_block_time (kinfo, k));
           }
  | IHalt kinfo, _s -> ok @@ Ex_split_halt kinfo
  | ILog (kinfo, _stack_ty, event, logger, continuation), stack ->
      ok
      @@ Ex_split_log
           {
             stack;
             continuation;
             reconstruct = (fun k -> ILog (kinfo, s, event, logger, k));
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
  | Ex_split_halt kinfo -> ok @@ IHalt kinfo
  | Ex_split_failwith {kinfo; location; arg_ty; _} ->
      ok @@ IFailwith (kinfo, location, arg_ty)

let log_entry logger ctxt gas k sty accu stack =
  let kinfo = kinfo_of_kinstr k in
  let ctxt = Local_gas_counter.update_context gas ctxt in
  logger.log_entry k ctxt kinfo.iloc sty (accu, stack)

let log_exit logger ctxt gas kinfo_prev k sty accu stack =
  let _kinfo = kinfo_of_kinstr k in
  let ctxt = Local_gas_counter.update_context gas ctxt in
  logger.log_exit k ctxt kinfo_prev.iloc sty (accu, stack)

let log_control logger ks = logger.log_control ks

let get_log = function
  | None -> Lwt.return (Ok None)
  | Some logger -> logger.get_log ()
  [@@ocaml.inline always]

(* [log_kinstr logger i] emits an instruction to instrument the
   execution of [i] with [logger]. *)
let log_kinstr logger sty i = ILog (kinfo_of_kinstr i, sty, LogEntry, logger, i)

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
      ( kinfo_of_kinstr k,
        sty,
        LogExit (kinfo_of_kinstr i),
        logger,
        log_kinstr logger sty k )
  in
  kinstr_rewritek sty i {apply}
