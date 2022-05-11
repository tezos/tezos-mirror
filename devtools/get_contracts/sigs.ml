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

module type PROTOCOL = sig
  module Error_monad : sig
    type 'a tzresult
  end

  module Alpha_context : sig
    type t

    type context = t

    module Fitness : sig
      type t
    end

    module Script : sig
      type prim

      type expr = prim Micheline.canonical

      type lazy_expr = expr Data_encoding.lazy_t

      type node = (Micheline.canonical_location, prim) Micheline.node

      val expr_encoding : expr Data_encoding.t
    end

    module Timestamp : sig
      type time
    end
  end

  module Contract_repr : sig
    type t

    val is_implicit : t -> Signature.Public_key_hash.t option

    val pp : Format.formatter -> t -> unit
  end

  module Lazy_storage_kind : sig
    module Big_map : sig
      module Id : sig
        type t

        val unparse_to_z : t -> Z.t
      end
    end
  end

  module Raw_context : sig
    type t

    val prepare :
      level:Int32.t ->
      predecessor_timestamp:Time.t ->
      timestamp:Time.t ->
      fitness:Fitness.t ->
      Environment_context.Context.t ->
      t Error_monad.tzresult Lwt.t

    val recover : t -> Environment_context.Context.t
  end

  module Script_expr_hash : sig
    type t

    val compare : t -> t -> int

    val to_b58check : t -> string

    val hash_bytes : ?key:bytes -> bytes list -> t
  end

  module Script_typed_ir : sig
    type ('k, 'v) map

    type ('arg, 'ret) lambda

    type 'a ty
  end

  module Script_ir_translator : sig
    open Alpha_context

    type toplevel

    type ex_ty = Ex_ty : 'a Script_typed_ir.ty -> ex_ty

    type type_logger

    val parse_ty :
      context ->
      legacy:bool ->
      allow_lazy_storage:bool ->
      allow_operation:bool ->
      allow_contract:bool ->
      allow_ticket:bool ->
      Script.node ->
      (ex_ty * context) Error_monad.tzresult

    val parse_data :
      ?type_logger:type_logger ->
      context ->
      legacy:bool ->
      allow_forged:bool ->
      'a Script_typed_ir.ty ->
      Script.node ->
      ('a * context) Error_monad.tzresult Lwt.t

    val unparse_ty :
      context ->
      'a Script_typed_ir.ty ->
      (Script.node * context) Error_monad.tzresult

    val parse_toplevel :
      context ->
      legacy:bool ->
      Script.expr ->
      (toplevel * context) Error_monad.tzresult Lwt.t
  end

  module Client : sig
    module Michelson_v1_printer : sig
      val print_expr : Format.formatter -> Alpha_context.Script.expr -> unit
    end
  end

  module Storage : sig
    module Big_map : sig
      type id = Lazy_storage_kind.Big_map.Id.t

      module Contents : sig
        val list_values :
          ?offset:int ->
          ?length:int ->
          Raw_context.t * id ->
          (Raw_context.t * Alpha_context.Script.expr list) Error_monad.tzresult
          Lwt.t
      end

      module Value_type : sig
        val get :
          Raw_context.t ->
          id ->
          Alpha_context.Script.expr Error_monad.tzresult Lwt.t
      end

      val fold :
        Raw_context.t -> init:'a -> f:(id -> 'a -> 'a Lwt.t) -> 'a Lwt.t
    end

    module Contract : sig
      module Code : sig
        val get :
          Raw_context.t ->
          Contract_repr.t ->
          (Raw_context.t * Alpha_context.Script.lazy_expr) Error_monad.tzresult
          Lwt.t
      end

      module Storage : sig
        val get :
          Raw_context.t ->
          Contract_repr.t ->
          (Raw_context.t * Alpha_context.Script.lazy_expr) Error_monad.tzresult
          Lwt.t
      end

      val fold :
        Raw_context.t ->
        init:'a ->
        f:(Contract_repr.t -> 'a -> 'a Lwt.t) ->
        'a Lwt.t
    end
  end

  module Unparse_types : sig
    type ex_lambda =
      | Ex_lambda :
          ('a, 'b) Script_typed_ir.lambda Script_typed_ir.ty
          * ('a, 'b) Script_typed_ir.lambda
          -> ex_lambda

    val collect_lambda_tys :
      'a Script_typed_ir.ty -> ('a -> ex_lambda list) list

    val collect_lambda_tys_map :
      'tv Script_typed_ir.ty ->
      (('tk, 'tv) Script_typed_ir.map -> ex_lambda list) list
  end

  val code_storage_type :
    Script_ir_translator.toplevel -> Alpha_context.Script.node

  val is_unpack : Alpha_context.Script.prim -> bool

  val lam_node : (_, _) Script_typed_ir.lambda -> Alpha_context.Script.node

  val wrap_tzresult : 'a Error_monad.tzresult -> 'a tzresult
end
