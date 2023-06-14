(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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
open Alpha_context
open Expr_common

module Parameter = struct
  (* // =============================================================================
   * // Entrypoints
   * // ============================================================================= *)

  (* Note: in the lqt_fa12 contract, [value] is a nat. Hence, it
     should always be positive *)
  type approve = {spender : Contract.t; value : Z.t}

  type mintOrBurn = {quantity : Z.t; target : Contract.t}

  (* Note: this wrapper does not implement a reprensentation for the
     entrypoints transfer, getAllowance, getBalance, getTotalSupply,
     as they are not used as of yet. *)
  type t = Approve of approve | MintOrBurn of mintOrBurn

  let approve p =
    assert (Z.lt Z.zero p.value || Z.equal Z.zero p.value) ;
    Approve p

  let mintOrBurn p = MintOrBurn p

  let approve_to_string {spender; value} =
    Format.asprintf
      "{ spender: %a; value: %a }"
      Contract.pp
      spender
      Z.pp_print
      value

  let mint_or_burn_to_string {quantity; target} =
    Format.asprintf
      "{ quantity: %a; target: %a }"
      Z.pp_print
      quantity
      Contract.pp
      target

  let to_string : t -> string = function
    | Approve p -> Format.asprintf "Approve %s" (approve_to_string p)
    | MintOrBurn p -> Format.asprintf "MintOrBurn %s" (mint_or_burn_to_string p)

  let entrypoint_of_parameter : t -> Entrypoint.t = function
    | Approve _ -> Entrypoint.of_string_strict_exn "approve"
    | MintOrBurn _ -> Entrypoint.of_string_strict_exn "mintOrBurn"

  let pp fmt s = Format.fprintf fmt "%s" (to_string s)

  let eq s s' = s = s'

  let to_expr_rooted :
      loc:'a ->
      t ->
      ('a, Michelson_v1_primitives.prim) Tezos_micheline.Micheline.node =
   fun ~loc -> function
    | MintOrBurn {quantity; target} ->
        comb ~loc [int ~loc quantity; address_string ~loc target]
    | Approve {spender; value} ->
        comb ~loc [address_string ~loc spender; int ~loc value]

  let to_expr :
      loc:'a ->
      t ->
      ('a, Michelson_v1_primitives.prim) Tezos_micheline.Micheline.node =
   fun ~loc p ->
    let rooted = to_expr_rooted ~loc p in
    match p with
    | MintOrBurn _ -> right ~loc @@ left ~loc rooted
    | Approve _ -> left ~loc @@ left ~loc @@ left ~loc rooted

  let to_michelson_string e =
    let e = to_expr ~loc:0 e in
    Format.asprintf
      "%a"
      Michelson_v1_printer.print_expr
      (Micheline.strip_locations e)
end

(* // =============================================================================
 * // Storage
 * // ============================================================================= *)

module Storage = struct
  let pp_big_map_id fmt v = Z.pp_print fmt (Big_map.Id.unparse_to_z v)

  type t = {
    tokens : Big_map.Id.t;
    allowances : Big_map.Id.t;
    admin : Contract.t;
    totalSupply : Z.t;
  }

  let pp {tokens; allowances; admin; totalSupply} =
    Format.asprintf
      "{ tokens: %a; allowances: %a; admin: %a; totalSupply: %a}"
      Z.pp_print
      (Big_map.Id.unparse_to_z tokens)
      Z.pp_print
      (Big_map.Id.unparse_to_z allowances)
      Contract.pp
      admin
      Z.pp_print
      totalSupply

  let null : t =
    {
      tokens = Big_map.Id.parse_z Z.zero;
      allowances = Big_map.Id.parse_z Z.one;
      admin = Contract.Implicit Signature.Public_key_hash.zero;
      totalSupply = Z.zero;
    }

  let eq s s' = s = s'

  let to_expr :
      loc:'a ->
      t ->
      ('a, Michelson_v1_primitives.prim) Tezos_micheline.Micheline.node =
   fun ~loc {tokens; allowances; admin; totalSupply} ->
    comb
      ~loc
      [
        big_map_id ~loc tokens;
        big_map_id ~loc allowances;
        address_string ~loc admin;
        int ~loc totalSupply;
      ]

  let to_michelson_string e =
    let e = to_expr ~loc:0 e in
    Format.asprintf
      "%a"
      Michelson_v1_printer.print_expr
      (Micheline.strip_locations e)

  type exn += Invalid_storage_expr of string

  (** Note: parses a storage unparsed in readable mode (as
     e.g. returned by [Alpha_services.Contract.storage]), so that
     contracts are represented by strings.  *)
  let of_expr_exn :
      ('a, Michelson_v1_primitives.prim) Tezos_micheline.Micheline.node -> t =
    function
    | Tezos_micheline.Micheline.Prim
        ( _,
          Script.D_Pair,
          [
            Tezos_micheline.Micheline.Int (_, tokens);
            Tezos_micheline.Micheline.Int (_, allowances);
            Tezos_micheline.Micheline.String (_, admin);
            Tezos_micheline.Micheline.Int (_, totalSupply);
          ],
          [] ) ->
        let tokens = Big_map.Id.parse_z tokens in
        let allowances = Big_map.Id.parse_z allowances in
        let admin = address_of_string_exn admin in
        {tokens; allowances; admin; totalSupply}
    | e ->
        let canonical = Micheline.strip_locations e in
        let msg =
          Format.asprintf
            "Not a valid LQT_FA1.2 storage: %s /// %a"
            (try
               Michelson_v1_printer.micheline_string_of_expression
                 ~zero_loc:true
                 canonical
             with Z.Overflow ->
               "Cannot represent as micheline due to overflowing Z -> int")
            Michelson_v1_printer.print_expr
            canonical
        in
        raise (Invalid_storage_expr msg)

  let get (ctxt : Context.t) ~(contract : Contract.t) : t tzresult Lwt.t =
    match contract with
    | Implicit _ ->
        invalid_arg "Lqt_fa12_repr.Storage.get called on implicit account"
    | Originated c ->
        Context.Contract.storage ctxt c >|=? Micheline.root >|=? of_expr_exn

  let get_alpha_context (ctxt : Context.t) : Alpha_context.t tzresult Lwt.t =
    (match ctxt with
    | B b ->
        (* can perhaps be retrieved through Raw_context.prepare ? *)
        Incremental.begin_construction b
    | I i -> return i)
    >|=? Incremental.alpha_ctxt

  let getBalance_opt (ctxt : Context.t) ~(contract : Contract.t)
      (owner : Script_typed_ir.address) =
    get ctxt ~contract >>=? fun storage ->
    let tokens = storage.tokens in
    get_alpha_context ctxt >>=? fun ctxt ->
    Script_ir_translator.hash_data ctxt Script_typed_ir.address_t owner
    >|= Environment.wrap_tzresult
    >>=? fun (address_hash, ctxt) ->
    Big_map.get_opt ctxt tokens address_hash >|= Environment.wrap_tzresult
    >>=? function
    | _, Some canonical -> (
        match Tezos_micheline.Micheline.root canonical with
        | Tezos_micheline.Micheline.Int (_, amount) -> return @@ Some amount
        | _ -> assert false)
    | _, None -> return @@ None

  let getBalance (ctxt : Context.t) ~(contract : Contract.t)
      (owner : Script_typed_ir.address) =
    getBalance_opt ctxt ~contract owner >|=? Option.value ~default:Z.zero
end

let transaction (ctxt : Context.t) ~(contract : Contract.t) ~(src : Contract.t)
    ?(amount = Tez.zero) (parameters : Parameter.t) =
  let entrypoint = Parameter.entrypoint_of_parameter parameters in
  let rooted_param_lazy =
    parameters
    |> Parameter.to_expr_rooted ~loc:0
    |> Micheline.strip_locations |> Alpha_context.Script.lazy_expr
  in
  Op.transaction
    ctxt
    src
    contract
    amount
    ~entrypoint
    ~parameters:rooted_param_lazy
