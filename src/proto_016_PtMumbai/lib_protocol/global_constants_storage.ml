(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <team@marigold.dev>                           *)
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
open Michelson_v1_primitives

(*

   See [expand] for an example.

   TODO: https://gitlab.com/tezos/tezos/-/issues/1609
   Move function to lib_micheline.

   On our next opportunity to update the environment, we
   should move this function to lib_micheline.

*)
let bottom_up_fold_cps initial_accumulator node initial_k f =
  let rec traverse_node accu node k =
    f accu node @@ fun accu node ->
    match node with
    | String _ | Int _ | Bytes _ -> k accu node
    | Prim (loc, prim, args, annot) ->
        (traverse_nodes [@ocaml.tailcall]) accu args (fun accu args ->
            f accu (Prim (loc, prim, args, annot)) k)
    | Seq (loc, elts) ->
        (traverse_nodes [@ocaml.tailcall]) accu elts (fun accu elts ->
            f accu (Seq (loc, elts)) k)
  and traverse_nodes accu nodes k =
    match nodes with
    | [] -> k accu []
    | node :: nodes ->
        (traverse_node [@ocaml.tailcall]) accu node (fun accu node ->
            (traverse_nodes [@ocaml.tailcall]) accu nodes (fun accu nodes ->
                k accu (node :: nodes)))
  in
  traverse_node initial_accumulator node initial_k

module Gas_costs = Global_constants_costs
module Expr_hash_map = Map.Make (Script_expr_hash)

type error += Expression_too_deep

type error += Expression_already_registered

type error += Badly_formed_constant_expression

type error += Nonexistent_global

type error += Expression_too_large

let () =
  let description =
    "Attempted to register an expression that, after fully expanding all \
     referenced global constants, would result in too many levels of nesting."
  in
  register_error_kind
    `Branch
    ~id:"Expression_too_deep"
    ~title:"Expression too deep"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Expression_too_deep -> Some () | _ -> None)
    (fun () -> Expression_too_deep) ;
  let description =
    "Attempted to register an expression as global constant that has already \
     been registered."
  in
  register_error_kind
    `Branch
    ~id:"Expression_already_registered"
    ~title:"Expression already registered"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Expression_already_registered -> Some () | _ -> None)
    (fun () -> Expression_already_registered) ;
  let description =
    "Found a badly formed constant expression. The 'constant' primitive must \
     always be followed by a string of the hash of the expression it points \
     to."
  in
  register_error_kind
    `Branch
    ~id:"Badly_formed_constant_expression"
    ~title:"Badly formed constant expression"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Badly_formed_constant_expression -> Some () | _ -> None)
    (fun () -> Badly_formed_constant_expression) ;
  let description =
    "No registered global was found at the given hash in storage."
  in
  register_error_kind
    `Branch
    ~id:"Nonexistent_global"
    ~title:"Tried to look up nonexistent global"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Nonexistent_global -> Some () | _ -> None)
    (fun () -> Nonexistent_global) ;
  let description =
    "Encountered an expression that, after expanding all constants, is larger \
     than the expression size limit."
  in
  register_error_kind
    `Branch
    ~id:"Expression_too_large"
    ~title:"Expression too large"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Expression_too_large -> Some () | _ -> None)
    (fun () -> Expression_too_large)

let get context hash =
  Storage.Global_constants.Map.find context hash >>=? fun (context, value) ->
  match value with
  | None -> tzfail Nonexistent_global
  | Some value -> return (context, value)

let expr_to_address_in_context context expr =
  let lexpr = Script_repr.lazy_expr expr in
  Raw_context.consume_gas context @@ Script_repr.force_bytes_cost lexpr
  >>? fun context ->
  Script_repr.force_bytes lexpr >>? fun b ->
  Raw_context.consume_gas context @@ Gas_costs.expr_to_address_in_context_cost b
  >|? fun context -> (context, Script_expr_hash.hash_bytes [b])

let node_too_large node =
  let node_size = Script_repr.Micheline_size.of_node node in
  let nodes = Saturation_repr.to_int node_size.nodes in
  let string_bytes = Saturation_repr.to_int node_size.string_bytes in
  let z_bytes = Saturation_repr.to_int node_size.z_bytes in
  Compare.Int.(
    nodes > Constants_repr.max_micheline_node_count
    || string_bytes + z_bytes > Constants_repr.max_micheline_bytes_limit)

let expand_node context node =
  (* We charge for traversing the top-level node at the beginning.
     Inside the loop, we charge for traversing each new constant
     that gets expanded. *)
  Raw_context.consume_gas
    context
    (Gas_costs.expand_no_constants_branch_cost node)
  >>?= fun context ->
  bottom_up_fold_cps
    (* We carry a Boolean representing whether we
       had to do any expansions or not. *)
    (context, Expr_hash_map.empty, false)
    node
    (fun (context, _, did_expansion) node ->
      return (context, node, did_expansion))
    (fun (context, map, did_expansion) node k ->
      match node with
      | Prim (_, H_constant, args, annot) -> (
          (* Charge for validating the b58check hash. *)
          Raw_context.consume_gas context Gas_costs.expand_constants_branch_cost
          >>?= fun context ->
          match (args, annot) with
          (* A constant Prim should always have a single String argument,
              being a properly formatted hash. *)
          | [String (_, address)], [] -> (
              match Script_expr_hash.of_b58check_opt address with
              | None -> tzfail Badly_formed_constant_expression
              | Some hash -> (
                  match Expr_hash_map.find hash map with
                  | Some node ->
                      (* Charge traversing the newly retrieved node *)
                      Raw_context.consume_gas
                        context
                        (Gas_costs.expand_no_constants_branch_cost node)
                      >>?= fun context -> k (context, map, true) node
                  | None ->
                      get context hash >>=? fun (context, expr) ->
                      (* Charge traversing the newly retrieved node *)
                      let node = root expr in
                      Raw_context.consume_gas
                        context
                        (Gas_costs.expand_no_constants_branch_cost node)
                      >>?= fun context ->
                      k (context, Expr_hash_map.add hash node map, true) node))
          | _ -> tzfail Badly_formed_constant_expression)
      | Int _ | String _ | Bytes _ | Prim _ | Seq _ ->
          k (context, map, did_expansion) node)
  >>=? fun (context, node, did_expansion) ->
  if did_expansion then
    (* Gas charged during expansion is at least proportional to the size of the
       resulting node so the execution time of [node_too_large] is already
       covered. *)
    if node_too_large node then tzfail Expression_too_large
    else return (context, node)
  else return (context, node)

let expand context expr =
  expand_node context (root expr) >|=? fun (context, node) ->
  (context, strip_locations node)

(** Computes the maximum depth of a Micheline node. Fails
    with [Expression_too_deep] if greater than
    [max_allowed_global_constant_depth].*)
let check_depth node =
  let rec advance node depth k =
    if Compare.Int.(depth > Constants_repr.max_allowed_global_constant_depth)
    then error Expression_too_deep
    else
      match node with
      | Int _ | String _ | Bytes _ | Prim (_, _, [], _) | Seq (_, []) ->
          (k [@tailcall]) (depth + 1)
      | Prim (loc, _, hd :: tl, _) | Seq (loc, hd :: tl) ->
          (advance [@tailcall]) hd (depth + 1) (fun dhd ->
              (advance [@tailcall])
                (* Because [depth] doesn't care about the content
                   of the expression, we can safely throw away information
                   about primitives and replace them with the [Seq] constructor.*)
                (Seq (loc, tl))
                depth
                (fun dtl -> (k [@tailcall]) (Compare.Int.max dhd dtl)))
  in
  advance node 0 (fun x -> Ok x)

let register context value =
  (* To calculate the total depth, we first expand all constants
     in the expression. This may fail with [Expression_too_large].

     Though the stored expression is the unexpanded version.
  *)
  expand_node context (root value) >>=? fun (context, node) ->
  (* We do not need to carbonate [check_depth]. [expand_node] and
     [Storage.Global_constants.Map.init] are already carbonated
     with gas at least proportional to the size of the expanded node
     and the computation cost of [check_depth] is of the same order. *)
  check_depth node >>?= fun (_depth : int) ->
  expr_to_address_in_context context value >>?= fun (context, key) ->
  trace Expression_already_registered
  @@ Storage.Global_constants.Map.init context key value
  >|=? fun (context, size) -> (context, key, Z.of_int size)

module Internal_for_tests = struct
  let node_too_large = node_too_large

  let bottom_up_fold_cps = bottom_up_fold_cps

  let expr_to_address_in_context = expr_to_address_in_context
end
