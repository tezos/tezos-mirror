(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 DaiLambda, Inc. <contact@dailambda,jp>                 *)
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

open Protocol
open Environment
open Error_monad
open Alpha_context
open Script_typed_ir

module Stack_utils = struct
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

  (** An existential container for an instruction paired with its
    initial stack type. This is used internally to pack together
    execution branches with different initial stack types but
    the same final stack type (which we want to compute). *)
  type ('r, 'f) ex_init_stack_ty =
    | Ex_init_stack_ty :
        ('a, 's) stack_ty * ('a, 's, 'r, 'f) kinstr
        -> ('r, 'f) ex_init_stack_ty

  let rec stack_prefix_preservation_witness_split_input : type a s b t c u d v.
      (b, t, c, u, a, s, d, v) stack_prefix_preservation_witness ->
      (a, s) stack_ty ->
      (b, t) stack_ty =
   fun w s ->
    match (w, s) with
    | KPrefix (_, _, w), Item_t (_, s) ->
        stack_prefix_preservation_witness_split_input w s
    | KRest, s -> s

  let rec stack_prefix_preservation_witness_split_output : type a s b t c u d v.
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

  let kinstr_split : type a s r f.
      (a, s) stack_ty ->
      (a, s, r, f) kinstr ->
      (a, s, r, f) ex_split_kinstr tzresult =
    let open Result_syntax in
    fun s i ->
      let dummy = Micheline.dummy_location in
      match (i, s) with
      | IDrop (loc, k), Item_t (_a, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IDrop (loc, k));
               }
      | IDup (loc, k), Item_t (a, s) ->
          let s = Item_t (a, Item_t (a, s)) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IDup (loc, k));
               }
      | ISwap (loc, k), Item_t (a, Item_t (b, s)) ->
          let s = Item_t (b, Item_t (a, s)) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ISwap (loc, k));
               }
      | IPush (loc, a, x, k), s ->
          let s = Item_t (a, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IPush (loc, a, x, k));
               }
      | IUnit (loc, k), s ->
          let s = Item_t (unit_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IUnit (loc, k));
               }
      | ICons_pair (loc, k), Item_t (a, Item_t (b, s)) ->
          let+ (Ty_ex_c c) = pair_t dummy a b in
          let s = Item_t (c, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> ICons_pair (loc, k));
            }
      | ICar (loc, k), Item_t (Pair_t (a, _b, _meta, _), s) ->
          let s = Item_t (a, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ICar (loc, k));
               }
      | ICdr (loc, k), Item_t (Pair_t (_a, b, _meta, _), s) ->
          let s = Item_t (b, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ICdr (loc, k));
               }
      | IUnpair (loc, k), Item_t (Pair_t (a, b, _meta, _), s) ->
          let s = Item_t (a, Item_t (b, s)) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IUnpair (loc, k));
               }
      | ICons_some (loc, k), Item_t (a, s) ->
          let+ o = option_t dummy a in
          let s = Item_t (o, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> ICons_some (loc, k));
            }
      | ICons_none (loc, a, k), s ->
          let+ o = option_t dummy a in
          let s = Item_t (o, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> ICons_none (loc, a, k));
            }
      | ( IIf_none {loc; branch_if_none; branch_if_some; k},
          Item_t (Option_t (a, _meta, _), s) ) ->
          return
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
          return
          @@ Ex_split_loop_may_not_fail
               {
                 body_init_stack = Item_t (a, s);
                 body;
                 continuation = k;
                 aft_body_stack_transform =
                   (function
                   | Item_t (b, s) ->
                       let+ o = option_t dummy b in
                       Item_t (o, s));
                 reconstruct = (fun body k -> IOpt_map {loc; body; k});
               }
      | ICons_left (loc, b, k), Item_t (a, s) ->
          let+ (Ty_ex_c c) = or_t dummy a b in
          let s = Item_t (c, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> ICons_left (loc, b, k));
            }
      | ICons_right (loc, a, k), Item_t (b, s) ->
          let+ (Ty_ex_c c) = or_t dummy a b in
          let s = Item_t (c, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> ICons_right (loc, a, k));
            }
      | ( IIf_left {loc; branch_if_left; branch_if_right; k},
          Item_t (Or_t (a, b, _meta, _), s) ) ->
          return
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
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ICons_list (loc, k));
               }
      | INil (loc, a, k), s ->
          let+ l = list_t dummy a in
          let s = Item_t (l, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> INil (loc, a, k));
            }
      | ( IIf_cons {loc; branch_if_cons; branch_if_nil; k},
          Item_t ((List_t (a, _meta) as l), s) ) ->
          return
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
          return
          @@ Ex_split_loop_may_not_fail
               {
                 body_init_stack = s;
                 body;
                 continuation = k;
                 aft_body_stack_transform =
                   (function
                   | Item_t (b, s) ->
                       let+ l = list_t dummy b in
                       Item_t (l, s));
                 reconstruct = (fun body k -> IList_map (loc, body, ty, k));
               }
      | IList_iter (loc, ty, body, k), Item_t (List_t (a, _meta), s) ->
          return
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
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IList_size (loc, k));
               }
      | IEmpty_set (loc, a, k), s ->
          let+ b = set_t dummy a in
          let s = Item_t (b, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> IEmpty_set (loc, a, k));
            }
      | ISet_iter (loc, a, body, k), Item_t (_b, s) ->
          return
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
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ISet_mem (loc, k));
               }
      | ISet_update (loc, k), Item_t (_, Item_t (_, s)) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ISet_update (loc, k));
               }
      | ISet_size (loc, k), Item_t (_, s) ->
          let s = Item_t (nat_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ISet_size (loc, k));
               }
      | IEmpty_map (loc, cty, vty, k), s ->
          let+ m = map_t dummy cty (assert_some vty) in
          let s = Item_t (m, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> IEmpty_map (loc, cty, vty, k));
            }
      | IMap_map (loc, ty, body, k), Item_t (Map_t (kty, vty, _meta), s) ->
          let (Map_t (key_ty, _, _)) = assert_some ty in
          let+ (Ty_ex_c p) = pair_t dummy key_ty vty in
          Ex_split_loop_may_not_fail
            {
              body_init_stack = Item_t (p, s);
              body;
              continuation = k;
              aft_body_stack_transform =
                (fun (Item_t (b, s)) ->
                  let+ m = map_t dummy kty b in
                  Item_t (m, s));
              reconstruct = (fun body k -> IMap_map (loc, ty, body, k));
            }
      | IMap_iter (loc, kvty, body, k), Item_t (_, stack) ->
          return
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
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IMap_mem (loc, k));
               }
      | IMap_get (loc, k), Item_t (_, Item_t (Map_t (_kty, vty, _meta), s)) ->
          let+ o = option_t dummy vty in
          let s = Item_t (o, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> IMap_get (loc, k));
            }
      | IMap_update (loc, k), Item_t (_, Item_t (_, s)) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IMap_update (loc, k));
               }
      | IMap_get_and_update (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IMap_get_and_update (loc, k));
               }
      | IMap_size (loc, k), Item_t (_, s) ->
          let s = Item_t (nat_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IMap_size (loc, k));
               }
      | IEmpty_big_map (loc, cty, ty, k), s ->
          let+ b = big_map_t dummy cty ty in
          let s = Item_t (b, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> IEmpty_big_map (loc, cty, ty, k));
            }
      | IBig_map_mem (loc, k), Item_t (_, Item_t (_, s)) ->
          let s = Item_t (bool_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IBig_map_mem (loc, k));
               }
      | ( IBig_map_get (loc, k),
          Item_t (_, Item_t (Big_map_t (_kty, vty, _meta), s)) ) ->
          let+ o = option_t dummy vty in
          let s = Item_t (o, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> IBig_map_get (loc, k));
            }
      | IBig_map_update (loc, k), Item_t (_, Item_t (_, s)) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IBig_map_update (loc, k));
               }
      | IBig_map_get_and_update (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IBig_map_get_and_update (loc, k));
               }
      | IConcat_string (loc, k), Item_t (_, s) ->
          let s = Item_t (string_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IConcat_string (loc, k));
               }
      | IConcat_string_pair (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IConcat_string_pair (loc, k));
               }
      | ISlice_string (loc, k), Item_t (_, Item_t (_, Item_t (_, s))) ->
          let s = Item_t (option_string_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ISlice_string (loc, k));
               }
      | IString_size (loc, k), Item_t (_, s) ->
          let s = Item_t (nat_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IString_size (loc, k));
               }
      | IConcat_bytes (loc, k), Item_t (_, s) ->
          let s = Item_t (bytes_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IConcat_bytes (loc, k));
               }
      | IConcat_bytes_pair (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IConcat_bytes_pair (loc, k));
               }
      | ISlice_bytes (loc, k), Item_t (_, Item_t (_, Item_t (_, s))) ->
          let s = Item_t (option_bytes_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ISlice_bytes (loc, k));
               }
      | IBytes_size (loc, k), Item_t (_, s) ->
          let s = Item_t (nat_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IBytes_size (loc, k));
               }
      | ILsl_bytes (loc, k), Item_t (_, Item_t (_, s)) ->
          let s = Item_t (bytes_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ILsl_bytes (loc, k));
               }
      | ILsr_bytes (loc, k), Item_t (_, Item_t (_, s)) ->
          let s = Item_t (bytes_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ILsr_bytes (loc, k));
               }
      | IOr_bytes (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IOr_bytes (loc, k));
               }
      | IAnd_bytes (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IAnd_bytes (loc, k));
               }
      | IXor_bytes (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IXor_bytes (loc, k));
               }
      | INot_bytes (loc, k), Item_t (_, s) ->
          let s = Item_t (bytes_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> INot_bytes (loc, k));
               }
      | IBytes_nat (loc, k), Item_t (_, s) ->
          let s = Item_t (bytes_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IBytes_nat (loc, k));
               }
      | INat_bytes (loc, k), Item_t (_, s) ->
          let s = Item_t (nat_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> INat_bytes (loc, k));
               }
      | IBytes_int (loc, k), Item_t (_, s) ->
          let s = Item_t (bytes_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IBytes_int (loc, k));
               }
      | IInt_bytes (loc, k), Item_t (_, s) ->
          let s = Item_t (int_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IInt_bytes (loc, k));
               }
      | IAdd_seconds_to_timestamp (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IAdd_seconds_to_timestamp (loc, k));
               }
      | IAdd_timestamp_to_seconds (loc, k), Item_t (_, Item_t (_, s)) ->
          let s = Item_t (timestamp_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IAdd_timestamp_to_seconds (loc, k));
               }
      | ISub_timestamp_seconds (loc, k), Item_t (_, Item_t (_, s)) ->
          let s = Item_t (timestamp_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ISub_timestamp_seconds (loc, k));
               }
      | IDiff_timestamps (loc, k), Item_t (_, Item_t (_, s)) ->
          let s = Item_t (int_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IDiff_timestamps (loc, k));
               }
      | IAdd_tez (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IAdd_tez (loc, k));
               }
      | ISub_tez (loc, k), Item_t (_, Item_t (_, s)) ->
          let s = Item_t (option_mutez_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ISub_tez (loc, k));
               }
      | ISub_tez_legacy (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ISub_tez_legacy (loc, k));
               }
      | IMul_teznat (loc, k), Item_t (_, Item_t (_, s)) ->
          let s = Item_t (mutez_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IMul_teznat (loc, k));
               }
      | IMul_nattez (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IMul_nattez (loc, k));
               }
      | IEdiv_teznat (loc, k), Item_t (_, Item_t (_, s)) ->
          let s = Item_t (option_pair_mutez_mutez_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IEdiv_teznat (loc, k));
               }
      | IEdiv_tez (loc, k), Item_t (_, Item_t (_, s)) ->
          let s = Item_t (option_pair_nat_mutez_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IEdiv_tez (loc, k));
               }
      | IOr (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IOr (loc, k));
               }
      | IAnd (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IAnd (loc, k));
               }
      | IXor (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IXor (loc, k));
               }
      | INot (loc, k), s ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> INot (loc, k));
               }
      | IIs_nat (loc, k), Item_t (_, s) ->
          let s = Item_t (option_nat_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IIs_nat (loc, k));
               }
      | INeg (loc, k), Item_t (_, s) ->
          let s = Item_t (int_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> INeg (loc, k));
               }
      | IAbs_int (loc, k), Item_t (_, s) ->
          let s = Item_t (nat_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IAbs_int (loc, k));
               }
      | IInt_nat (loc, k), Item_t (_, s) ->
          let s = Item_t (int_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IInt_nat (loc, k));
               }
      | IAdd_int (loc, k), Item_t (_, Item_t (_, s)) ->
          let s = Item_t (int_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IAdd_int (loc, k));
               }
      | IAdd_nat (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IAdd_nat (loc, k));
               }
      | ISub_int (loc, k), Item_t (_, Item_t (_, s)) ->
          let s = Item_t (int_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ISub_int (loc, k));
               }
      | IMul_int (loc, k), Item_t (_, Item_t (_, s)) ->
          let s = Item_t (int_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IMul_int (loc, k));
               }
      | IMul_nat (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IMul_nat (loc, k));
               }
      | IEdiv_int (loc, k), Item_t (_, Item_t (_, s)) ->
          let s = Item_t (option_pair_int_nat_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IEdiv_int (loc, k));
               }
      | IEdiv_nat (loc, k), Item_t (_, Item_t (a, s)) ->
          let* (Ty_ex_c p) = pair_t dummy a nat_t in
          let+ o = option_t dummy p in
          let s = Item_t (o, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> IEdiv_nat (loc, k));
            }
      | ILsl_nat (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ILsl_nat (loc, k));
               }
      | ILsr_nat (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ILsr_nat (loc, k));
               }
      | IOr_nat (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IOr_nat (loc, k));
               }
      | IAnd_nat (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IAnd_nat (loc, k));
               }
      | IAnd_int_nat (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IAnd_int_nat (loc, k));
               }
      | IXor_nat (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IXor_nat (loc, k));
               }
      | INot_int (loc, k), Item_t (_, s) ->
          let s = Item_t (int_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> INot_int (loc, k));
               }
      | IIf {loc; branch_if_true; branch_if_false; k}, Item_t (_, s) ->
          return
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
          return
          @@ Ex_split_loop_may_fail
               {
                 body_init_stack = s;
                 body;
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun body k -> ILoop (loc, body, k));
               }
      | ILoop_left (loc, kl, kr), Item_t (Or_t (a, b, _meta, _), s) ->
          return
          @@ Ex_split_loop_may_fail
               {
                 body_init_stack = Item_t (a, s);
                 body = kl;
                 cont_init_stack = Item_t (b, s);
                 continuation = kr;
                 reconstruct = (fun kl kr -> ILoop_left (loc, kl, kr));
               }
      | IDip (loc, body, ty, k), Item_t (a, s) ->
          return
          @@ Ex_split_loop_may_not_fail
               {
                 body_init_stack = s;
                 body;
                 continuation = k;
                 aft_body_stack_transform = (fun s -> return (Item_t (a, s)));
                 reconstruct = (fun body k -> IDip (loc, body, ty, k));
               }
      | IExec (loc, sty, k), Item_t (_, Item_t (Lambda_t (_, b, _meta), s)) ->
          let s = Item_t (b, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IExec (loc, sty, k));
               }
      | ( IApply (loc, ty, k),
          Item_t (_, Item_t (Lambda_t (Pair_t (_, a, _, _), b, _), s)) ) ->
          let+ l = lambda_t dummy a b in
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
          let+ lam = lambda_t dummy a b in
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
          let+ lam = lambda_t dummy a b in
          let s = Item_t (lam, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> ILambda (loc, l, k));
            }
      | IFailwith (location, arg_ty), _ ->
          return
          @@ Ex_split_failwith
               {location; arg_ty; cast = {cast = IFailwith (location, arg_ty)}}
      | ICompare (loc, ty, k), Item_t (_, Item_t (_, s)) ->
          let s = Item_t (int_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ICompare (loc, ty, k));
               }
      | IEq (loc, k), Item_t (_, s) ->
          let s = Item_t (bool_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IEq (loc, k));
               }
      | INeq (loc, k), Item_t (_, s) ->
          let s = Item_t (bool_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> INeq (loc, k));
               }
      | ILt (loc, k), Item_t (_, s) ->
          let s = Item_t (bool_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ILt (loc, k));
               }
      | IGt (loc, k), Item_t (_, s) ->
          let s = Item_t (bool_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IGt (loc, k));
               }
      | ILe (loc, k), Item_t (_, s) ->
          let s = Item_t (bool_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ILe (loc, k));
               }
      | IGe (loc, k), Item_t (_, s) ->
          let s = Item_t (bool_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IGe (loc, k));
               }
      | IAddress (loc, k), Item_t (_, s) ->
          let s = Item_t (address_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IAddress (loc, k));
               }
      | IContract (loc, ty, code, k), Item_t (_, s) ->
          let* c = contract_t dummy ty in
          let+ o = option_t dummy c in
          let s = Item_t (o, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> IContract (loc, ty, code, k));
            }
      | ITransfer_tokens (loc, k), Item_t (_, Item_t (_, Item_t (_, s))) ->
          let s = Item_t (operation_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ITransfer_tokens (loc, k));
               }
      | ( IView (loc, (View_signature {output_ty; _} as view_signature), sty, k),
          Item_t (_, Item_t (_, s)) ) ->
          let+ b = option_t dummy output_ty in
          let s = Item_t (b, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> IView (loc, view_signature, sty, k));
            }
      | IImplicit_account (loc, k), Item_t (_, s) ->
          let s = Item_t (contract_unit_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IImplicit_account (loc, k));
               }
      | IIs_implicit_account (loc, k), Item_t (_, s) ->
          let s = Item_t (key_hash_option_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IIs_implicit_account (loc, k));
               }
      | IIndex_address (loc, k), Item_t (_, s) ->
          let s = Item_t (nat_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IIndex_address (loc, k));
               }
      | IGet_address_index (loc, k), Item_t (_, s) ->
          let* b = option_t dummy nat_t in
          let s = Item_t (b, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IGet_address_index (loc, k));
               }
      | ( ICreate_contract {loc; storage_type; code; k},
          Item_t (_, Item_t (_, Item_t (_, s))) ) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = Item_t (operation_t, Item_t (address_t, s));
                 continuation = k;
                 reconstruct =
                   (fun k -> ICreate_contract {loc; storage_type; code; k});
               }
      | ISet_delegate (loc, k), Item_t (_, s) ->
          let s = Item_t (operation_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ISet_delegate (loc, k));
               }
      | INow (loc, k), s ->
          let s = Item_t (timestamp_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> INow (loc, k));
               }
      | IBalance (loc, k), s ->
          let s = Item_t (mutez_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IBalance (loc, k));
               }
      | ILevel (loc, k), s ->
          let s = Item_t (nat_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ILevel (loc, k));
               }
      | ICheck_signature (loc, k), Item_t (_, Item_t (_, Item_t (_, s))) ->
          let s = Item_t (bool_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ICheck_signature (loc, k));
               }
      | IHash_key (loc, k), Item_t (_, s) ->
          let s = Item_t (key_hash_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IHash_key (loc, k));
               }
      | IPack (loc, ty, k), Item_t (_, s) ->
          let s = Item_t (bytes_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IPack (loc, ty, k));
               }
      | IUnpack (loc, ty, k), Item_t (_, s) ->
          let+ o = option_t dummy ty in
          let s = Item_t (o, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> IUnpack (loc, ty, k));
            }
      | IBlake2b (loc, k), s ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IBlake2b (loc, k));
               }
      | ISha256 (loc, k), s ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ISha256 (loc, k));
               }
      | ISha512 (loc, k), s ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ISha512 (loc, k));
               }
      | ISource (loc, k), s ->
          let s = Item_t (address_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ISource (loc, k));
               }
      | ISender (loc, k), s ->
          let s = Item_t (address_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ISender (loc, k));
               }
      | ISelf (loc, ty, ep, k), s ->
          let+ c = contract_t dummy ty in
          let s = Item_t (c, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> ISelf (loc, ty, ep, k));
            }
      | ISelf_address (loc, k), s ->
          let s = Item_t (address_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ISelf_address (loc, k));
               }
      | IAmount (loc, k), s ->
          let s = Item_t (mutez_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IAmount (loc, k));
               }
      | ISapling_empty_state (loc, memo_size, k), s ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = Item_t (sapling_state_t ~memo_size, s);
                 continuation = k;
                 reconstruct =
                   (fun k -> ISapling_empty_state (loc, memo_size, k));
               }
      | ( ISapling_verify_update_deprecated (loc, k),
          Item_t (_, Item_t (state_ty, s)) ) ->
          let* (Ty_ex_c pair_ty) = pair_t dummy int_t state_ty in
          let+ ty = option_t dummy pair_ty in
          Ex_split_kinstr
            {
              cont_init_stack = Item_t (ty, s);
              continuation = k;
              reconstruct =
                (fun k -> ISapling_verify_update_deprecated (loc, k));
            }
      | ISapling_verify_update (loc, k), Item_t (_, Item_t (state_ty, s)) ->
          let* (Ty_ex_c int_state_ty) = pair_t dummy int_t state_ty in
          let* (Ty_ex_c pair_ty) = pair_t dummy bytes_t int_state_ty in
          let+ ty = option_t dummy pair_ty in
          let s = Item_t (ty, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> ISapling_verify_update (loc, k));
            }
      | IDig (loc, n, p, k), s ->
          let (Item_t (b, s)) =
            stack_prefix_preservation_witness_split_input p s
          in
          let s = stack_prefix_preservation_witness_split_output p s in
          let s = Item_t (b, s) in
          return
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
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IDug (loc, n, p, k));
               }
      | IDipn (loc, n, p, k1, k2), s ->
          return
          @@ Ex_split_loop_may_not_fail
               {
                 body_init_stack =
                   stack_prefix_preservation_witness_split_input p s;
                 body = k1;
                 continuation = k2;
                 aft_body_stack_transform =
                   (fun s ->
                     return
                     @@ stack_prefix_preservation_witness_split_output p s);
                 reconstruct = (fun k1 k2 -> IDipn (loc, n, p, k1, k2));
               }
      | IDropn (loc, n, p, k), s ->
          let s = stack_prefix_preservation_witness_split_input p s in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IDropn (loc, n, p, k));
               }
      | IChainId (loc, k), s ->
          let s = Item_t (chain_id_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IChainId (loc, k));
               }
      | INever location, Item_t (arg_ty, _) ->
          return
          @@ Ex_split_failwith
               {location; arg_ty; cast = {cast = INever location}}
      | IVoting_power (loc, k), Item_t (_, s) ->
          let s = Item_t (nat_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IVoting_power (loc, k));
               }
      | ITotal_voting_power (loc, k), s ->
          let s = Item_t (nat_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ITotal_voting_power (loc, k));
               }
      | IKeccak (loc, k), s ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IKeccak (loc, k));
               }
      | ISha3 (loc, k), s ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> ISha3 (loc, k));
               }
      | IAdd_bls12_381_g1 (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IAdd_bls12_381_g1 (loc, k));
               }
      | IAdd_bls12_381_g2 (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IAdd_bls12_381_g2 (loc, k));
               }
      | IAdd_bls12_381_fr (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IAdd_bls12_381_fr (loc, k));
               }
      | IMul_bls12_381_g1 (loc, k), Item_t (g1, Item_t (_, s)) ->
          let s = Item_t (g1, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IMul_bls12_381_g1 (loc, k));
               }
      | IMul_bls12_381_g2 (loc, k), Item_t (g2, Item_t (_, s)) ->
          let s = Item_t (g2, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IMul_bls12_381_g2 (loc, k));
               }
      | IMul_bls12_381_fr (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IMul_bls12_381_fr (loc, k));
               }
      | IMul_bls12_381_z_fr (loc, k), Item_t (fr, Item_t (_, s)) ->
          let s = Item_t (fr, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IMul_bls12_381_z_fr (loc, k));
               }
      | IMul_bls12_381_fr_z (loc, k), Item_t (_, s) ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IMul_bls12_381_fr_z (loc, k));
               }
      | IInt_bls12_381_fr (loc, k), Item_t (_, s) ->
          let s = Item_t (int_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IInt_bls12_381_fr (loc, k));
               }
      | INeg_bls12_381_g1 (loc, k), s ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> INeg_bls12_381_g1 (loc, k));
               }
      | INeg_bls12_381_g2 (loc, k), s ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> INeg_bls12_381_g2 (loc, k));
               }
      | INeg_bls12_381_fr (loc, k), s ->
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> INeg_bls12_381_fr (loc, k));
               }
      | IPairing_check_bls12_381 (loc, k), Item_t (_, s) ->
          let s = Item_t (bool_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IPairing_check_bls12_381 (loc, k));
               }
      | IComb (loc, n, p, k), s ->
          let rec aux : type a b s c d t.
              (a, b * s) stack_ty ->
              (a, b, s, c, d, t) comb_gadt_witness ->
              (c, d * t) stack_ty tzresult =
           fun s w ->
            match (w, s) with
            | Comb_one, s -> return s
            | Comb_succ w, Item_t (a, s) ->
                let* (Item_t (c, t)) = aux s w in
                let+ (Ty_ex_c p) = pair_t dummy a c in
                Item_t (p, t)
          in
          let+ s = aux s p in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> IComb (loc, n, p, k));
            }
      | IUncomb (loc, n, p, k), s ->
          let rec aux : type a b s c d t.
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
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IUncomb (loc, n, p, k));
               }
      | IComb_get (loc, n, p, k), Item_t (c, s) ->
          let rec aux : type c cc a.
              (c, cc) ty -> (c, a) comb_get_gadt_witness -> a ty_ex_c =
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
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IComb_get (loc, n, p, k));
               }
      | IComb_set (loc, n, p, k), Item_t (a, Item_t (b, s)) ->
          let rec aux : type a b c ca cb.
              (a, ca) ty ->
              (b, cb) ty ->
              (a, b, c) comb_set_gadt_witness ->
              c ty_ex_c tzresult =
           fun a b w ->
            match (w, b) with
            | Comb_set_zero, _ -> return (Ty_ex_c a)
            | Comb_set_one, Pair_t (_hd, tl, _meta, _) -> pair_t dummy a tl
            | Comb_set_plus_two w, Pair_t (hd, tl, _meta, _) ->
                let* (Ty_ex_c c) = aux a tl w in
                pair_t dummy hd c
          in
          let+ (Ty_ex_c c) = aux a b p in
          let s = Item_t (c, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> IComb_set (loc, n, p, k));
            }
      | IDup_n (loc, n, p, k), s ->
          let rec aux : type a b s t.
              (a, b * s) stack_ty ->
              (a, b, s, t) dup_n_gadt_witness ->
              t ty_ex_c =
           fun s w ->
            match (w, s) with
            | Dup_n_succ w, Item_t (_, s) -> aux s w
            | Dup_n_zero, Item_t (a, _) -> Ty_ex_c a
          in
          let s =
            let (Ty_ex_c ty) = aux s p in
            Item_t (ty, s)
          in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IDup_n (loc, n, p, k));
               }
      | ITicket (loc, cty, k), Item_t (_, Item_t (_, s)) ->
          let* ty = ticket_t dummy (assert_some cty) in
          let+ t = option_t loc ty in
          let s = Item_t (t, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> ITicket (loc, cty, k));
            }
      | ITicket_deprecated (loc, cty, k), Item_t (_, Item_t (_, s)) ->
          let+ t = ticket_t dummy (assert_some cty) in
          let s = Item_t (t, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> ITicket_deprecated (loc, cty, k));
            }
      | IRead_ticket (loc, a, k), s ->
          let* (Ty_ex_c p) = pair_t dummy (assert_some a) nat_t in
          let+ (Ty_ex_c t) = pair_t dummy address_t p in
          let s = Item_t (t, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> IRead_ticket (loc, a, k));
            }
      | ISplit_ticket (loc, k), Item_t (t, Item_t (_, s)) ->
          let* (Ty_ex_c p) = pair_t dummy t t in
          let+ o = option_t dummy p in
          let s = Item_t (o, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> ISplit_ticket (loc, k));
            }
      | IJoin_tickets (loc, ty, k), Item_t (Pair_t (t, _t, _meta, _), s) ->
          let+ o = option_t dummy t in
          let s = Item_t (o, s) in
          Ex_split_kinstr
            {
              cont_init_stack = s;
              continuation = k;
              reconstruct = (fun k -> IJoin_tickets (loc, ty, k));
            }
      | IOpen_chest (loc, k), Item_t (_, Item_t (_, Item_t (_, s))) ->
          let s = Item_t (option_bytes_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IOpen_chest (loc, k));
               }
      | IMin_block_time (loc, k), s ->
          let s = Item_t (nat_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IMin_block_time (loc, k));
               }
      | IEmit {loc; ty; unparsed_ty; tag; k}, Item_t (_, s) ->
          let s = Item_t (operation_t, s) in
          return
          @@ Ex_split_kinstr
               {
                 cont_init_stack = s;
                 continuation = k;
                 reconstruct = (fun k -> IEmit {loc; ty; unparsed_ty; tag; k});
               }
      | IEmit _, Bot_t -> .
      | IHalt loc, _s -> return @@ Ex_split_halt loc
      | ILog (loc, _stack_ty, event, logger, continuation), stack ->
          return
          @@ Ex_split_log
               {
                 stack;
                 continuation;
                 reconstruct = (fun k -> ILog (loc, s, event, logger, k));
               }

  (* [kinstr_final_stack_type sty instr] computes the stack type after
     [instr] has been executed, assuming [sty] is the type of the stack
     prior to execution. For the rare instructions which can return stacks
     of any type ([FAILWITH] and [NEVER]), this function returns [None]. *)
  let rec kinstr_final_stack_type : type a s r f.
      (a, s) stack_ty -> (a, s, r, f) kinstr -> (r, f) stack_ty option tzresult
      =
    let open Result_syntax in
    fun s i ->
      let* ex_split_kinstr = kinstr_split s i in
      match ex_split_kinstr with
      | Ex_split_kinstr {cont_init_stack; continuation; _} ->
          kinstr_final_stack_type cont_init_stack continuation
      | Ex_split_log {stack; continuation; _} ->
          kinstr_final_stack_type stack continuation
      | Ex_split_loop_may_fail {cont_init_stack; continuation; _} ->
          kinstr_final_stack_type cont_init_stack continuation
      | Ex_split_loop_may_not_fail
          {body_init_stack; body; continuation; aft_body_stack_transform; _}
        -> (
          let* sty = kinstr_final_stack_type body_init_stack body in
          match sty with
          | Some after_body ->
              let* before_k = aft_body_stack_transform after_body in
              kinstr_final_stack_type before_k continuation
          | None -> return_none)
      | Ex_split_if
          {
            left_init_stack;
            left_branch;
            right_init_stack;
            right_branch;
            continuation;
            _;
          } -> (
          let* sty = kinstr_final_stack_type left_init_stack left_branch in
          match sty with
          | Some after_branch_a ->
              kinstr_final_stack_type after_branch_a continuation
          | None -> (
              let* sty =
                kinstr_final_stack_type right_init_stack right_branch
              in
              match sty with
              | Some after_branch_b ->
                  kinstr_final_stack_type after_branch_b continuation
              | None -> return_none))
      | Ex_split_halt _ -> return_some s
      | Ex_split_failwith {cast = {cast = _}; _} -> return_none

  (* The same as [kinstr_final_stack_type], but selects from multiple
     possible execution branches. If the first instr ends with FAILWITH,
     it will try the next and so on. Note that all instructions must
     result in the same stack type. *)
  let rec branched_final_stack_type : type r f.
      (r, f) ex_init_stack_ty list -> (r, f) stack_ty option tzresult =
    let open Result_syntax in
    function
    | [] -> return_none
    | Ex_init_stack_ty (init_sty, branch) :: bs -> (
        let* sty = kinstr_final_stack_type init_sty branch in
        match sty with
        | Some _ as sty -> return sty
        | None -> branched_final_stack_type bs)

  let kinstr_rewritek : type a s r f.
      (a, s) stack_ty ->
      (a, s, r, f) kinstr ->
      kinstr_rewritek ->
      (a, s, r, f) kinstr tzresult =
    let open Result_syntax in
    fun s i f ->
      let* ex_split_kinstr = kinstr_split s i in
      match ex_split_kinstr with
      | Ex_split_kinstr {cont_init_stack; continuation; reconstruct} ->
          return @@ reconstruct (f.apply cont_init_stack continuation)
      | Ex_split_log {continuation; reconstruct; _} ->
          return @@ reconstruct continuation
      | Ex_split_loop_may_fail
          {body_init_stack; body; cont_init_stack; continuation; reconstruct} ->
          return
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
          let+ k =
            let* sty = kinstr_final_stack_type body_init_stack body in
            match sty with
            | Some after_body ->
                let+ before_k = aft_body_stack_transform after_body in
                f.apply before_k continuation
            | None -> return continuation
          in
          reconstruct (f.apply body_init_stack body) k
      | Ex_split_if
          {
            left_init_stack;
            left_branch;
            right_init_stack;
            right_branch;
            continuation;
            reconstruct;
          } ->
          let+ k =
            let* sty = kinstr_final_stack_type left_init_stack left_branch in
            match sty with
            | Some after_left_branch ->
                return @@ f.apply after_left_branch continuation
            | None -> (
                let* sty =
                  kinstr_final_stack_type right_init_stack right_branch
                in
                match sty with
                | Some after_right_branch ->
                    return @@ f.apply after_right_branch continuation
                | None -> return continuation)
          in
          reconstruct
            (f.apply left_init_stack left_branch)
            (f.apply right_init_stack right_branch)
            k
      | Ex_split_halt loc -> return @@ IHalt loc
      | Ex_split_failwith {location; arg_ty; _} ->
          return @@ IFailwith (location, arg_ty)

  (** [instrument_cont logger sty] creates a function instrumenting
    continuations starting from the stack type described by [sty].
    Instrumentation consists in wrapping inner continuations in
    [KLog] continuation so that logging continues. *)
  let instrument_cont : type a b c d.
      logger ->
      (a, b) stack_ty ->
      (a, b, c, d) continuation ->
      (a, b, c, d) continuation =
   fun logger sty -> function KLog _ as k -> k | k -> KLog (k, sty, logger)
end

module type Logger_base = sig
  val log_interp : ('a, 's, 'b, 'f, 'c, 'u) logging_function

  val log_entry : ('a, 's, 'b, 'f, 'a, 's) logging_function

  val log_control : ('a, 's, 'b, 'f) continuation -> unit

  val log_exit : ('a, 's, 'b, 'f, 'c, 'u) logging_function

  val get_log : unit -> execution_trace option tzresult Lwt.t
end

module Logger (Base : Logger_base) = struct
  open Stack_utils
  open Local_gas_counter
  open Script_interpreter_defs
  open Script_interpreter.Internals.Raw

  (** [log_entry ctxt gas instr sty accu stack] simply calls the
      [Base.log_entry] function with the appropriate arguments. *)
  let log_entry ctxt gas k sty accu stack =
    let ctxt = Local_gas_counter.update_context gas ctxt in
    Base.log_entry k ctxt (kinstr_location k) sty (accu, stack)

  (** [log_exit ctxt gas loc instr sty accu stack] simply calls the
      [Base.log_exit] function with the appropriate arguments. *)
  let log_exit ctxt gas loc_prev k sty accu stack =
    let ctxt = Local_gas_counter.update_context gas ctxt in
    Base.log_exit k ctxt loc_prev sty (accu, stack)

  (** [log_control continuation] simply calls the [Base.log_control]
      function with the appropriate arguments. *)
  let log_control ks = Base.log_control ks

  (** [log_kinstr logger sty instr] returns [instr] prefixed by an
      [ILog] instruction to log the first instruction in [instr]. *)
  let log_kinstr logger sty i =
    ILog (kinstr_location i, sty, LogEntry, logger, i)

  (* [log_next_kinstr logger i] instruments the next instruction of [i]
     with [ILog] instructions to make sure it will be logged.
     This instrumentation has a performance cost, but importantly, it is
     only ever paid when logging is enabled. Otherwise, the possibility
     to instrument the script is costless.

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

  (** [log_next_continuation logger sty cont] instruments the next
    continuation in [cont] with [KLog] continuations to ensure
    logging.

    This instrumentation has a performance cost, but importantly, it
    is only ever paid when logging is enabled. Otherwise, the
    possibility to instrument the script is costless. *)
  let log_next_continuation : type a b c d.
      logger ->
      (a, b) stack_ty ->
      (a, b, c, d) continuation ->
      (a, b, c, d) continuation tzresult =
    let open Result_syntax in
    fun logger stack_ty cont ->
      let enable_log sty ki = log_kinstr logger sty ki in
      match cont with
      | KCons (ki, k) -> (
          let ki' = enable_log stack_ty ki in
          let+ sty = kinstr_final_stack_type stack_ty ki in
          match sty with
          | None -> KCons (ki', k)
          | Some sty -> KCons (ki', instrument_cont logger sty k))
      | KLoop_in (ki, k) ->
          let (Item_t (Bool_t, sty)) = stack_ty in
          return @@ KLoop_in (enable_log sty ki, instrument_cont logger sty k)
      | KReturn (stack, sty, k) ->
          let k' = instrument_cont logger (assert_some sty) k in
          return @@ KReturn (stack, sty, k')
      | KLoop_in_left (ki, k) ->
          let (Item_t (Or_t (a_ty, b_ty, _, _), rest)) = stack_ty in
          let ki' = enable_log (Item_t (a_ty, rest)) ki in
          let k' = instrument_cont logger (Item_t (b_ty, rest)) k in
          return @@ KLoop_in_left (ki', k')
      | KUndip (x, ty, k) ->
          let k' =
            instrument_cont logger (Item_t (assert_some ty, stack_ty)) k
          in
          return @@ KUndip (x, ty, k')
      | KIter (body, xty, xs, k) ->
          let body' = enable_log (Item_t (assert_some xty, stack_ty)) body in
          let k' = instrument_cont logger stack_ty k in
          return @@ KIter (body', xty, xs, k')
      | KList_enter_body (body, xs, ys, ty, len, k) ->
          let k' =
            instrument_cont logger (Item_t (assert_some ty, stack_ty)) k
          in
          return @@ KList_enter_body (body, xs, ys, ty, len, k')
      | KList_exit_body (body, xs, ys, ty, len, k) ->
          let (Item_t (_, sty)) = stack_ty in
          let k' = instrument_cont logger (Item_t (assert_some ty, sty)) k in
          return @@ KList_exit_body (body, xs, ys, ty, len, k')
      | KMap_enter_body (body, xs, ys, ty, k) ->
          let k' =
            instrument_cont logger (Item_t (assert_some ty, stack_ty)) k
          in
          return @@ KMap_enter_body (body, xs, ys, ty, k')
      | KMap_exit_body (body, xs, ys, yk, ty, k) ->
          let (Item_t (_, sty)) = stack_ty in
          let k' = instrument_cont logger (Item_t (assert_some ty, sty)) k in
          return @@ KMap_exit_body (body, xs, ys, yk, ty, k')
      | KMap_head (_, _)
      | KView_exit (_, _)
      | KLog _ (* This case should never happen. *) | KNil ->
          return cont

  (*

    Zero-cost logging
    =================

  *)

  (*

     The following functions insert a logging instruction to continue
     the logging process in the next execution steps.

     There is a special treatment of instructions that generate fresh
     continuations: we pass a constructor as argument to their
     evaluation rules so that they can instrument these fresh
     continuations by themselves.

     This on-the-fly instrumentation of the execution allows zero-cost
     logging since logging instructions are only introduced if an
     initial logging continuation is pushed in the initial continuation
     that starts the evaluation.

  *)
  let ilog : type a s b t r f.
      logger -> logging_event -> (a, s) stack_ty -> (a, s, b, t, r, f) step_type
      =
    let open Lwt_result_syntax in
    fun logger event sty ((ctxt, _) as g) old_gas k ks accu stack ->
      (match (k, event) with
      | ILog _, LogEntry -> ()
      | _, LogEntry -> log_entry ctxt old_gas k sty accu stack
      | _, LogExit prev_loc -> log_exit ctxt old_gas prev_loc k sty accu stack) ;
      let*? k = log_next_kinstr logger sty k in
      (* We need to match on instructions that create continuations so
         that we can instrument those continuations with [KLog] (see
         comment above).  For functions that don't do this, we simply call
         [step], as they don't require any special treatment. *)
      match consume_instr old_gas k accu stack with
      | None -> tzfail Gas.Operation_quota_exceeded
      | Some gas -> (
          match k with
          | IIf_none {branch_if_none; branch_if_some; k; _} -> (
              let (Item_t (Option_t (ty, _, _), rest)) = sty in
              let*? sty_opt =
                branched_final_stack_type
                  [
                    Ex_init_stack_ty (rest, branch_if_none);
                    Ex_init_stack_ty (Item_t (ty, rest), branch_if_some);
                  ]
              in
              let ks' =
                match sty_opt with
                | None -> KCons (k, ks)
                | Some sty' -> instrument_cont logger sty' @@ KCons (k, ks)
              in
              match accu with
              | None ->
                  let accu, stack = stack in
                  (step [@ocaml.tailcall]) g gas branch_if_none ks' accu stack
              | Some v ->
                  (step [@ocaml.tailcall]) g gas branch_if_some ks' v stack)
          | IOpt_map {body; k; loc = _} -> (
              match accu with
              | None -> (step [@ocaml.tailcall]) g gas k ks None stack
              | Some v ->
                  let (Item_t (Option_t (ty, _, _), rest)) = sty in
                  let bsty = Item_t (ty, rest) in
                  let kmap_head = KMap_head (Option.some, KCons (k, ks)) in
                  let*? sty_opt = kinstr_final_stack_type bsty body in
                  let ks' =
                    match sty_opt with
                    | None -> kmap_head
                    | Some sty' -> instrument_cont logger sty' kmap_head
                  in
                  (step [@ocaml.tailcall]) g gas body ks' v stack)
          | IIf_left {branch_if_left; branch_if_right; k; _} -> (
              let (Item_t (Or_t (lty, rty, _, _), rest)) = sty in
              let*? sty_opt =
                branched_final_stack_type
                  [
                    Ex_init_stack_ty (Item_t (lty, rest), branch_if_left);
                    Ex_init_stack_ty (Item_t (rty, rest), branch_if_right);
                  ]
              in
              let k' =
                match sty_opt with
                | None -> KCons (k, ks)
                | Some sty' -> instrument_cont logger sty' @@ KCons (k, ks)
              in
              match accu with
              | L v -> (step [@ocaml.tailcall]) g gas branch_if_left k' v stack
              | R v -> (step [@ocaml.tailcall]) g gas branch_if_right k' v stack
              )
          | IIf_cons {branch_if_cons; branch_if_nil; k; _} -> (
              let (Item_t ((List_t (elty, _) as lty), rest)) = sty in
              let*? sty' =
                branched_final_stack_type
                  [
                    Ex_init_stack_ty (rest, branch_if_nil);
                    Ex_init_stack_ty
                      (Item_t (elty, Item_t (lty, rest)), branch_if_cons);
                  ]
              in
              let k' =
                match sty' with
                | None -> KCons (k, ks)
                | Some sty' -> instrument_cont logger sty' @@ KCons (k, ks)
              in
              match Script_list.uncons accu with
              | None ->
                  let accu, stack = stack in
                  (step [@ocaml.tailcall]) g gas branch_if_nil k' accu stack
              | Some (hd, tl) ->
                  (step [@ocaml.tailcall]) g gas branch_if_cons k' hd (tl, stack)
              )
          | IList_map (_, body, ty, k) ->
              let (Item_t (_, sty')) = sty in
              let instrument = instrument_cont logger sty' in
              (ilist_map [@ocaml.tailcall])
                instrument
                g
                gas
                body
                k
                ks
                ty
                accu
                stack
          | IList_iter (_, ty, body, k) ->
              let (Item_t (_, sty')) = sty in
              let instrument = instrument_cont logger sty' in
              (ilist_iter [@ocaml.tailcall])
                instrument
                g
                gas
                body
                ty
                k
                ks
                accu
                stack
          | ISet_iter (_, ty, body, k) ->
              let (Item_t (_, rest)) = sty in
              let instrument = instrument_cont logger rest in
              (iset_iter [@ocaml.tailcall])
                instrument
                g
                gas
                body
                ty
                k
                ks
                accu
                stack
          | IMap_map (_, ty, body, k) ->
              let (Item_t (_, rest)) = sty in
              let instrument = instrument_cont logger rest in
              (imap_map [@ocaml.tailcall])
                instrument
                g
                gas
                body
                k
                ks
                ty
                accu
                stack
          | IMap_iter (_, kvty, body, k) ->
              let (Item_t (_, rest)) = sty in
              let instrument = instrument_cont logger rest in
              (imap_iter [@ocaml.tailcall])
                instrument
                g
                gas
                body
                kvty
                k
                ks
                accu
                stack
          | IMul_teznat (loc, k) ->
              (imul_teznat [@ocaml.tailcall])
                (Some logger)
                g
                gas
                loc
                k
                ks
                accu
                stack
          | IMul_nattez (loc, k) ->
              (imul_nattez [@ocaml.tailcall])
                (Some logger)
                g
                gas
                loc
                k
                ks
                accu
                stack
          | ILsl_nat (loc, k) ->
              (ilsl_nat [@ocaml.tailcall])
                (Some logger)
                g
                gas
                loc
                k
                ks
                accu
                stack
          | ILsr_nat (loc, k) ->
              (ilsr_nat [@ocaml.tailcall])
                (Some logger)
                g
                gas
                loc
                k
                ks
                accu
                stack
          | IIf {branch_if_true; branch_if_false; k; _} ->
              let (Item_t (Bool_t, rest)) = sty in
              let*? sty' =
                branched_final_stack_type
                  [
                    Ex_init_stack_ty (rest, branch_if_true);
                    Ex_init_stack_ty (rest, branch_if_false);
                  ]
              in
              let k' =
                match sty' with
                | None -> KCons (k, ks)
                | Some sty' -> instrument_cont logger sty' @@ KCons (k, ks)
              in
              let res, stack = stack in
              if accu then
                (step [@ocaml.tailcall]) g gas branch_if_true k' res stack
              else (step [@ocaml.tailcall]) g gas branch_if_false k' res stack
          | ILoop (_, body, k) ->
              let ks =
                instrument_cont logger sty @@ KLoop_in (body, KCons (k, ks))
              in
              (next [@ocaml.tailcall]) g gas ks accu stack
          | ILoop_left (_, bl, br) ->
              let ks =
                instrument_cont logger sty @@ KLoop_in_left (bl, KCons (br, ks))
              in
              (next [@ocaml.tailcall]) g gas ks accu stack
          | IDip (_, b, ty, k) ->
              let (Item_t (_, rest)) = sty in
              let*? rest' = kinstr_final_stack_type rest b in
              let ign = accu in
              let ks =
                match rest' with
                | None -> KUndip (ign, ty, KCons (k, ks))
                | Some rest' ->
                    instrument_cont
                      logger
                      rest'
                      (KUndip (ign, ty, KCons (k, ks)))
              in
              let accu, stack = stack in
              (step [@ocaml.tailcall]) g gas b ks accu stack
          | IExec (_, stack_ty, k) ->
              let (Item_t (_, Item_t (Lambda_t (_, ret, _), _))) = sty in
              let sty' = Item_t (ret, Bot_t) in
              let instrument = instrument_cont logger sty' in
              iexec instrument (Some logger) g gas stack_ty k ks accu stack
          | IFailwith (kloc, tv) ->
              let {ifailwith} = ifailwith in
              (ifailwith [@ocaml.tailcall]) (Some logger) g gas kloc tv accu
          | IDipn (_, _n, n', b, k) ->
              let accu, stack, ks = kundip n' accu stack (KCons (k, ks)) in
              (step [@ocaml.tailcall]) g gas b ks accu stack
          | IView
              (_, (View_signature {output_ty; _} as view_signature), stack_ty, k)
            ->
              let sty' = Item_t (output_ty, Bot_t) in
              let instrument = instrument_cont logger sty' in
              (iview [@ocaml.tailcall])
                instrument
                g
                gas
                view_signature
                stack_ty
                k
                ks
                accu
                stack
          | _ -> (step [@ocaml.tailcall]) g old_gas k ks accu stack)
  [@@inline]

  let klog : type a s r f.
      logger ->
      outdated_context * step_constants ->
      local_gas_counter ->
      (a, s) stack_ty ->
      (a, s, r, f) continuation ->
      (a, s, r, f) continuation ->
      a ->
      s ->
      (r * f * outdated_context * local_gas_counter) tzresult Lwt.t =
    let open Lwt_result_syntax in
    fun logger g old_gas stack_ty k0 ks accu stack ->
      let ty_for_logging_unsafe = function
        (* This function is only called when logging is enabled.  If
           that's the case, the elaborator must have been called with
           [logging_enabled] option, which ensures that this will not be
           [None]. Realistically, it can happen that the [logging_enabled]
           option was omitted, resulting in a crash here. But this is
           acceptable, because logging is never enabled during block
           validation, so the layer 1 is safe. *)
        | None -> assert false
        | Some ty -> ty
      in
      (match ks with KLog _ -> () | _ -> log_control ks) ;
      match consume_control old_gas ks with
      | None -> tzfail Gas.Operation_quota_exceeded
      | Some gas -> (
          let*? continuation = log_next_continuation logger stack_ty ks in
          match continuation with
          | KCons (ki, k) -> (step [@ocaml.tailcall]) g gas ki k accu stack
          | KLoop_in (ki, k) ->
              (kloop_in [@ocaml.tailcall]) g gas k0 ki k accu stack
          | KReturn (_, _, _) as k ->
              (next [@ocaml.tailcall]) g old_gas k accu stack
          | KLoop_in_left (ki, k) ->
              (kloop_in_left [@ocaml.tailcall]) g gas k0 ki k accu stack
          | KUndip (_, _, _) as k ->
              (next [@ocaml.tailcall]) g old_gas k accu stack
          | KIter (body, xty, xs, k) ->
              let instrument = instrument_cont logger stack_ty in
              (kiter [@ocaml.tailcall])
                instrument
                g
                gas
                body
                xty
                xs
                k
                accu
                stack
          | KList_enter_body (body, xs, ys, ty_opt, len, k) ->
              let instrument =
                let ty = ty_for_logging_unsafe ty_opt in
                let (List_t (vty, _)) = ty in
                let sty = Item_t (vty, stack_ty) in
                instrument_cont logger sty
              in
              (klist_enter [@ocaml.tailcall])
                instrument
                g
                gas
                body
                xs
                ys
                ty_opt
                len
                k
                accu
                stack
          | KList_exit_body (body, xs, ys, ty_opt, len, k) ->
              let (Item_t (_, rest)) = stack_ty in
              let instrument = instrument_cont logger rest in
              (klist_exit [@ocaml.tailcall])
                instrument
                g
                gas
                body
                xs
                ys
                ty_opt
                len
                k
                accu
                stack
          | KMap_enter_body (body, xs, ys, ty_opt, k) ->
              let instrument =
                let ty = ty_for_logging_unsafe ty_opt in
                let (Map_t (_, vty, _)) = ty in
                let sty = Item_t (vty, stack_ty) in
                instrument_cont logger sty
              in
              (kmap_enter [@ocaml.tailcall])
                instrument
                g
                gas
                body
                xs
                ty_opt
                ys
                k
                accu
                stack
          | KMap_exit_body (body, xs, ys, yk, ty_opt, k) ->
              let (Item_t (_, rest)) = stack_ty in
              let instrument = instrument_cont logger rest in
              (kmap_exit [@ocaml.tailcall])
                instrument
                g
                gas
                body
                xs
                ty_opt
                ys
                yk
                k
                accu
                stack
          | KMap_head (f, k) -> (next [@ocaml.tailcall]) g gas k (f accu) stack
          | KView_exit (scs, k) ->
              (next [@ocaml.tailcall]) (fst g, scs) gas k accu stack
          | KLog _ as k ->
              (* This case should never happen. *)
              (next [@ocaml.tailcall]) g old_gas k accu stack
          | KNil as k -> (next [@ocaml.tailcall]) g old_gas k accu stack)
  [@@inline]
end

let make (module Base : Logger_base) =
  let module Logger = Logger (Base) in
  let open Logger in
  let open Base in
  {log_interp; get_log; log_kinstr; klog; ilog}
