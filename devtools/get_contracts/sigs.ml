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

module Time = Time.Protocol

module type MAIN = sig
  val main :
    output_dir:string ->
    Tezos_protocol_environment.Context.t ->
    head:Tezos_store.Store.Block.t ->
    unit tzresult Lwt.t
end

module type PROTOCOL = sig
  val hash : Protocol_hash.t

  module Context : sig
    type t

    val prepare :
      level:Int32.t ->
      predecessor_timestamp:Time.t ->
      timestamp:Time.t ->
      Tezos_protocol_environment.Context.t ->
      t tzresult Lwt.t
  end

  type context = Context.t

  module Script : sig
    type prim

    type expr = prim Micheline.canonical

    type lazy_expr = expr Data_encoding.lazy_t

    type node = (Micheline.canonical_location, prim) Micheline.node

    val expr_encoding : expr Data_encoding.t

    val lazy_expr : expr -> lazy_expr

    val decode_and_costs : lazy_expr -> (expr * int * int) tzresult

    val print_expr : Format.formatter -> expr -> unit

    module Hash : sig
      type t

      val compare : t -> t -> int

      val to_b58check : t -> string

      val hash_bytes : ?key:bytes -> bytes list -> t
    end
  end

  module Contract : sig
    type repr

    val is_implicit : repr -> bool

    val pp : Format.formatter -> repr -> unit

    val get_code :
      context -> repr -> (context * Script.lazy_expr) tzresult Lwt.t

    val get_storage :
      context -> repr -> (context * Script.lazy_expr) tzresult Lwt.t

    val fold : context -> init:'a -> f:(repr -> 'a -> 'a Lwt.t) -> 'a Lwt.t
  end

  module Translator : sig
    type toplevel

    type ('a, 'b) ty

    type ex_ty = Ex_ty : ('a, 'b) ty -> ex_ty

    type ex_code

    val actual_code_size : ex_code -> int

    val expected_code_size : ex_code -> int

    val parse_ty :
      context ->
      allow_lazy_storage:bool ->
      allow_operation:bool ->
      allow_contract:bool ->
      allow_ticket:bool ->
      Script.node ->
      (ex_ty * int) tzresult

    val unparse_ty : context -> ex_ty -> Script.node tzresult

    val parse_toplevel :
      context -> Script.expr -> (toplevel * int) tzresult Lwt.t

    val parse_code : context -> Script.lazy_expr -> ex_code tzresult Lwt.t

    val parse_data :
      context ->
      allow_forged:bool ->
      ('a, 'b) ty ->
      Script.node ->
      ('a * int) tzresult Lwt.t

    val unparse_data_cost : context -> ('a, 'b) ty -> 'a -> int tzresult Lwt.t
  end

  module Storage : sig
    type big_map_id

    val id_to_z : big_map_id -> Z.t

    val list_values :
      ?offset:int ->
      ?length:int ->
      context * big_map_id ->
      (context * Script.expr list) tzresult Lwt.t

    val get : context -> big_map_id -> Script.expr tzresult Lwt.t

    val fold :
      context -> init:'a -> f:(big_map_id -> 'a -> 'a Lwt.t) -> 'a Lwt.t
  end

  module Lambda : sig
    type ex_lambda

    type ex_ty_lambdas

    val lam_node : ex_lambda -> Script.node

    val collect_lambda_tys : Translator.ex_ty -> ex_ty_lambdas option

    val fold_ex_ty_lambdas :
      ctxt:context ->
      expr:Script.node ->
      f:('a -> Script.node -> ex_lambda list -> 'a) ->
      acc:'a ->
      ex_ty_lambdas ->
      'a Lwt.t
  end

  module Global_constants : sig
    val expand : context -> Script.expr -> (context * Script.expr) Lwt.t
  end

  val code_storage_type : Translator.toplevel -> Script.node

  val is_unpack : Script.prim -> bool
end
