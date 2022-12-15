(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022 Trili Tech <contact@trili.tech>                        *)
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

(* Overview:

   This mli is organized into roughly three parts:

   1. A set of new types prefixed with "ex_"
   Michelson is encoded in a GADT that preserves certain properties about its
   type system. If you haven't read about GADT's, check out the relevant section
   in the Tezos docs:
   https://tezos.gitlab.io/developer/gadt.html#generalized-algebraic-data-types-gadts

   The idea is that type representing a Michelson type, ['a ty], is parameterized
   by a type 'a. But that 'a can't be just _any_ type; it must be valid according
   to the definition of ['a ty]. Thus, if I give you a value of type ['a ty],
   all you know is that "there exists some 'a such that 'a ty exists". You must be
   careful not to accidentally quantify 'a universally, that is "for all 'a,
   'a ty exists", otherwise you'll get an annoying error about 'a trying to escape
   it's scope. We do this by hiding 'a in an existential type. This is what
    ex_comparable_ty, ex_ty, ex_stack_ty, etc. do.

   2. A set of functions dealing with high-level Michelson types:
   This module also provides functions for interacting with the list, map,
   set, and big_map Michelson types.

   3. A set of functions for parsing and typechecking Michelson.
   Finally, and what you likely came for, the module provides many functions prefixed
   with "parse_" that convert untyped Micheline (which is essentially S-expressions
   with a few primitive atom types) into the GADT encoding well-typed Michelson. Likewise
   there is a number of functions prefixed "unparse_" that do the reverse. These functions
   consume gas, and thus are parameterized by an [Alpha_context.t].

   The variety of functions reflects the variety of things one might want to parse,
   from [parse_data] for arbitrary Micheline expressions to [parse_contract_data] for
   well-formed Michelson contracts.
*)

(** {1 Michelson Existential Witness types} *)
open Alpha_context

open Script_typed_ir
open Script_tc_errors

type ('ta, 'tb) eq = Eq : ('same, 'same) eq

type ex_comparable_ty =
  | Ex_comparable_ty : 'a Script_typed_ir.comparable_ty -> ex_comparable_ty

type ex_parameter_ty_and_entrypoints =
  | Ex_parameter_ty_and_entrypoints : {
      arg_type : ('a, _) Script_typed_ir.ty;
      entrypoints : 'a Script_typed_ir.entrypoints;
    }
      -> ex_parameter_ty_and_entrypoints

type ex_stack_ty =
  | Ex_stack_ty : ('a, 's) Script_typed_ir.stack_ty -> ex_stack_ty

type ex_script = Ex_script : ('a, 'b) Script_typed_ir.script -> ex_script

type toplevel = {
  code_field : Script.node;
  arg_type : Script.node;
  storage_type : Script.node;
  views : Script_typed_ir.view_map;
}

type ('arg, 'storage) code =
  | Code : {
      code :
        ( ('arg, 'storage) Script_typed_ir.pair,
          ( Script_typed_ir.operation Script_list.t,
            'storage )
          Script_typed_ir.pair )
        Script_typed_ir.lambda;
      arg_type : ('arg, _) Script_typed_ir.ty;
      storage_type : ('storage, _) Script_typed_ir.ty;
      views : Script_typed_ir.view_map;
      entrypoints : 'arg Script_typed_ir.entrypoints;
      code_size : Cache_memory_helpers.sint;
          (** This is an over-approximation of the value size in memory, in
         bytes, of the contract's static part, that is its source
         code. This includes the code of the contract as well as the code
         of the views. The storage size is not taken into account by this
         field as it has a dynamic size. *)
    }
      -> ('arg, 'storage) code

type ex_code = Ex_code : ('a, 'c) code -> ex_code

type 'storage typed_view =
  | Typed_view : {
      input_ty : ('input, _) Script_typed_ir.ty;
      output_ty : ('output, _) Script_typed_ir.ty;
      kinstr :
        ( 'input * 'storage,
          Script_typed_ir.end_of_stack,
          'output,
          Script_typed_ir.end_of_stack )
        Script_typed_ir.kinstr;
      original_code_expr : Script.node;
    }
      -> 'storage typed_view

type 'storage typed_view_map =
  (Script_string.t, 'storage typed_view) Script_typed_ir.map

type ('a, 's, 'b, 'u) cinstr = {
  apply :
    'r 'f.
    ('b, 'u, 'r, 'f) Script_typed_ir.kinstr ->
    ('a, 's, 'r, 'f) Script_typed_ir.kinstr;
}
[@@ocaml.unboxed]

type ('a, 's, 'b, 'u) descr = {
  loc : Script.location;
  bef : ('a, 's) Script_typed_ir.stack_ty;
  aft : ('b, 'u) Script_typed_ir.stack_ty;
  instr : ('a, 's, 'b, 'u) cinstr;
}

type tc_context = Script_tc_context.t

type ('a, 's) judgement =
  | Typed : ('a, 's, 'b, 'u) descr -> ('a, 's) judgement
  | Failed : {
      descr : 'b 'u. ('b, 'u) Script_typed_ir.stack_ty -> ('a, 's, 'b, 'u) descr;
    }
      -> ('a, 's) judgement

val close_descr :
  ('a, 'b, 'c, 'd) descr -> ('a, 'b, 'c, 'd) Script_typed_ir.kdescr

(* ---- Lists, Sets and Maps ----------------------------------------------- *)

(** {2 High-level Michelson Data Types} *)
val ty_eq :
  error_details:(Script.location, 'error_trace) error_details ->
  ('a, 'ac) Script_typed_ir.ty ->
  ('b, 'bc) Script_typed_ir.ty ->
  ( (('a, 'ac) Script_typed_ir.ty, ('b, 'bc) Script_typed_ir.ty) eq,
    'error_trace )
  Gas_monad.t

(** {3 Parsing and Typechecking Michelson} *)
val parse_comparable_data :
  ?type_logger:Script_ir_translator_config.type_logger ->
  context ->
  'a Script_typed_ir.comparable_ty ->
  Script.node ->
  ('a * context) tzresult Lwt.t

(* Parsing a Micheline node data into an IR-typed data. *)
val parse_data :
  elab_conf:Script_ir_translator_config.elab_config ->
  context ->
  allow_forged:bool ->
  ('a, _) Script_typed_ir.ty ->
  Script.node ->
  ('a * context) tzresult Lwt.t

(* Unparsing an IR-typed data back into a Micheline node data *)
val unparse_data :
  context ->
  Script_ir_unparser.unparsing_mode ->
  ('a, _) Script_typed_ir.ty ->
  'a ->
  (Script.expr * context) tzresult Lwt.t

val unparse_code :
  context ->
  Script_ir_unparser.unparsing_mode ->
  Script.node ->
  (Script.expr * context) tzresult Lwt.t

(** For benchmarking purpose, we also export versions of the unparsing
    functions which don't call location stripping. These functions are
    not carbonated and should not be called directly from the protocol. *)
module Internal_for_benchmarking : sig
  val unparse_data :
    context ->
    stack_depth:int ->
    Script_ir_unparser.unparsing_mode ->
    ('a, 'ac) ty ->
    'a ->
    (Script.node * context) tzresult Lwt.t

  val unparse_code :
    context ->
    stack_depth:int ->
    Script_ir_unparser.unparsing_mode ->
    Script.node ->
    (Script.node * context) tzresult Lwt.t
end

val parse_instr :
  elab_conf:Script_ir_translator_config.elab_config ->
  tc_context ->
  context ->
  Script.node ->
  ('a, 's) Script_typed_ir.stack_ty ->
  (('a, 's) judgement * context) tzresult Lwt.t

(**
  [parse_ty] specialized for the right-hand side part of a big map type, i.e.
  the `value` in `big_map key value`.
*)
val parse_big_map_value_ty :
  context -> legacy:bool -> Script.node -> (ex_ty * context) tzresult

val parse_packable_ty :
  context -> legacy:bool -> Script.node -> (ex_ty * context) tzresult

val parse_passable_ty :
  context -> legacy:bool -> Script.node -> (ex_ty * context) tzresult

val parse_comparable_ty :
  context -> Script.node -> (ex_comparable_ty * context) tzresult

val parse_parameter_ty_and_entrypoints :
  context ->
  legacy:bool ->
  Script.node ->
  (ex_parameter_ty_and_entrypoints * context) tzresult

val parse_view_input_ty :
  context ->
  stack_depth:int ->
  legacy:bool ->
  Script.node ->
  (ex_ty * context) tzresult

val parse_view_output_ty :
  context ->
  stack_depth:int ->
  legacy:bool ->
  Script.node ->
  (ex_ty * context) tzresult

val parse_view :
  elab_conf:Script_ir_translator_config.elab_config ->
  context ->
  ('storage, _) Script_typed_ir.ty ->
  Script_typed_ir.view ->
  ('storage typed_view * context) tzresult Lwt.t

val parse_views :
  elab_conf:Script_ir_translator_config.elab_config ->
  context ->
  ('storage, _) Script_typed_ir.ty ->
  Script_typed_ir.view_map ->
  ('storage typed_view_map * context) tzresult Lwt.t

(**
  [parse_ty] allowing big_map values, operations, contract and tickets.
*)
val parse_any_ty :
  context -> legacy:bool -> Script.node -> (ex_ty * context) tzresult

(** We expose [parse_ty] for convenience to external tools. Please use
    specialized versions such as [parse_packable_ty], [parse_passable_ty],
    [parse_comparable_ty], or [parse_big_map_value_ty] if possible. *)
val parse_ty :
  context ->
  legacy:bool ->
  allow_lazy_storage:bool ->
  allow_operation:bool ->
  allow_contract:bool ->
  allow_ticket:bool ->
  Script.node ->
  (ex_ty * context) tzresult

val parse_toplevel :
  context -> legacy:bool -> Script.expr -> (toplevel * context) tzresult Lwt.t

(** High-level function to typecheck a Michelson script. This function is not
    used for validating operations but only for the [typecheck_code] RPC.

    If [show_types] is set to [true], details of the typechecking are returned
    in the [type_map], otherwise the returned [type_map] is empty. *)
val typecheck_code :
  legacy:bool ->
  show_types:bool ->
  context ->
  Script.expr ->
  (type_map * context) tzresult Lwt.t

val parse_code :
  elab_conf:Script_ir_translator_config.elab_config ->
  context ->
  code:Script.lazy_expr ->
  (ex_code * context) tzresult Lwt.t

val parse_storage :
  elab_conf:Script_ir_translator_config.elab_config ->
  context ->
  allow_forged:bool ->
  ('storage, _) Script_typed_ir.ty ->
  storage:Script.lazy_expr ->
  ('storage * context) tzresult Lwt.t

(** Combines [parse_code] and [parse_storage] *)
val parse_script :
  elab_conf:Script_ir_translator_config.elab_config ->
  context ->
  allow_forged_in_storage:bool ->
  Script.t ->
  (ex_script * context) tzresult Lwt.t

(* Gas accounting may not be perfect in this function, as it is only called by RPCs. *)
val parse_and_unparse_script_unaccounted :
  context ->
  legacy:bool ->
  allow_forged_in_storage:bool ->
  Script_ir_unparser.unparsing_mode ->
  normalize_types:bool ->
  Script.t ->
  (Script.t * context) tzresult Lwt.t

val parse_contract_data :
  context ->
  Script.location ->
  ('a, _) Script_typed_ir.ty ->
  Destination.t ->
  entrypoint:Entrypoint.t ->
  (context * 'a Script_typed_ir.typed_contract) tzresult Lwt.t

val parse_contract_for_script :
  context ->
  Script.location ->
  ('a, _) Script_typed_ir.ty ->
  Destination.t ->
  entrypoint:Entrypoint.t ->
  (context * 'a Script_typed_ir.typed_contract option) tzresult Lwt.t

(** ['a ex_ty_cstr] is like [ex_ty], but also adds to the existential a function
    used to reconstruct a value of type ['a] from the internal type of the
    existential. Typically, it will be used to go from the type of an
    entry-point to the full type of a contract. *)
type 'a ex_ty_cstr =
  | Ex_ty_cstr : {
      ty : ('b, _) Script_typed_ir.ty;
      construct : 'b -> 'a;
      original_type_expr : Script.node;
    }
      -> 'a ex_ty_cstr

val find_entrypoint :
  error_details:(_, 'error_trace) error_details ->
  ('t, _) Script_typed_ir.ty ->
  't Script_typed_ir.entrypoints ->
  Entrypoint.t ->
  ('t ex_ty_cstr, 'error_trace) Gas_monad.t

val list_entrypoints_uncarbonated :
  ('t, _) Script_typed_ir.ty ->
  't Script_typed_ir.entrypoints ->
  Michelson_v1_primitives.prim list list
  * (ex_ty * Script.node) Entrypoint.Map.t

val pack_data :
  context ->
  ('a, _) Script_typed_ir.ty ->
  'a ->
  (bytes * context) tzresult Lwt.t

val hash_comparable_data :
  context ->
  'a Script_typed_ir.comparable_ty ->
  'a ->
  (Script_expr_hash.t * context) tzresult Lwt.t

val hash_data :
  context ->
  ('a, _) Script_typed_ir.ty ->
  'a ->
  (Script_expr_hash.t * context) tzresult Lwt.t

type lazy_storage_ids

val no_lazy_storage_id : lazy_storage_ids

(** Traverse the given type, producing a {!lazy_storage_ids} for
    use with {!extract_lazy_storage_diff}.
 *)
val collect_lazy_storage :
  context ->
  ('a, _) Script_typed_ir.ty ->
  'a ->
  (lazy_storage_ids * context) tzresult

val list_of_big_map_ids : lazy_storage_ids -> Big_map.Id.t list

(** Produce a lazy storage diff, containing in-memory writes to
    lazy data structures such as big_maps yet to be committed.

    The resulting diff can be committed to the underlying storage
    (context) using [Lazy_storage_diff.apply].

 @param to_duplicate
    Lazy data structure reference produced via {!collect_lazy_storage}
    that can not be reused. Typically collected via traversing
    the parameters to a smart contract.
 @param to_update
    Lazy data structure reference produced via {!collect_lazy_storage}
    that can be reused. Typically collected via traversing the previous
    storage of a smart contract.
 *)
val extract_lazy_storage_diff :
  context ->
  Script_ir_unparser.unparsing_mode ->
  temporary:bool ->
  to_duplicate:lazy_storage_ids ->
  to_update:lazy_storage_ids ->
  ('a, _) Script_typed_ir.ty ->
  'a ->
  ('a * Lazy_storage.diffs option * context) tzresult Lwt.t

(* return [None] if none or more than one found *)
val get_single_sapling_state :
  context ->
  ('a, _) Script_typed_ir.ty ->
  'a ->
  (Sapling.Id.t option * context) tzresult

(** [code_size ctxt code views] returns an overapproximation of the size of
    the in-memory representation of [code] and [views] in bytes in the
    context [ctxt]. *)
val code_size :
  context ->
  ('a, 'b) Script_typed_ir.lambda ->
  Script_typed_ir.view_map ->
  (Cache_memory_helpers.sint * context) tzresult

(** [script_size script] returns an overapproximation of the size of
    the in-memory representation of [script] in bytes as well as the cost
    associated to computing that overapproximation. *)
val script_size : ex_script -> int * Gas_limit_repr.cost
