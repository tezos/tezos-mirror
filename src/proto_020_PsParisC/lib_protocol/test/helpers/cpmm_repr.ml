(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* // =============================================================================
 * // Storage
 * // ============================================================================= *)

module Storage = struct
  type t = {
    tokenPool : Z.t;
    xtzPool : Tez.t;
    lqtTotal : Z.t;
    tokenAddress : Contract_hash.t;
    lqtAddress : Contract_hash.t;
  }

  let zero : t =
    {
      tokenPool = Z.zero;
      xtzPool = Tez.zero;
      lqtTotal = Z.zero;
      tokenAddress = Contract_hash.zero;
      lqtAddress = Contract_hash.zero;
    }

  let to_string {tokenPool; xtzPool; lqtTotal; tokenAddress; lqtAddress} =
    Format.asprintf
      "{tokenPool : %a; xtzPool : %s; lqtTotal : %a; tokenAddress : %s; \
       lqtAddress : %s;}"
      Z.pp_print
      tokenPool
      (Int64.to_string @@ Tez.to_mutez xtzPool)
      Z.pp_print
      lqtTotal
      (Contract_hash.to_b58check tokenAddress)
      (Contract_hash.to_b58check lqtAddress)

  let pp fmt s = Format.fprintf fmt "%s" (to_string s)

  let eq s s' = s = s'

  let to_expr :
      loc:'a ->
      t ->
      ('a, Michelson_v1_primitives.prim) Tezos_micheline.Micheline.node =
   fun ~loc {tokenPool; xtzPool; lqtTotal; tokenAddress; lqtAddress} ->
    comb
      ~loc
      [
        int ~loc tokenPool;
        mutez ~loc xtzPool;
        int ~loc lqtTotal;
        address_string ~loc (Contract.Originated tokenAddress);
        address_string ~loc (Contract.Originated lqtAddress);
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
            Tezos_micheline.Micheline.Int (_, tokenPool);
            Tezos_micheline.Micheline.Int (_, xtzPool);
            Tezos_micheline.Micheline.Int (_, lqtTotal);
            Tezos_micheline.Micheline.String (_, tokenAddress);
            Tezos_micheline.Micheline.String (_, lqtAddress);
          ],
          [] ) ->
        let xtzPool = Tez.of_mutez_exn (Z.to_int64 xtzPool) in
        let tokenAddress = originated_of_string_exn tokenAddress in
        let lqtAddress = originated_of_string_exn lqtAddress in
        {tokenPool; xtzPool; lqtTotal; tokenAddress; lqtAddress}
    | e ->
        let canonical = Micheline.strip_locations e in
        let msg =
          Format.asprintf
            "Not a valid CPMM storage: %s /// %a"
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
    let open Lwt_result_syntax in
    match contract with
    | Implicit _ ->
        invalid_arg "Cpmm_repr.Storage.get called on implicit account"
    | Originated c ->
        let+ expr = Context.Contract.storage ctxt c in
        Micheline.root expr |> of_expr_exn

  let of_tuple (tokenPool, xtzPool, lqtTotal, tokenAddress, lqtAddress) =
    {tokenPool; xtzPool; lqtTotal; tokenAddress; lqtAddress}

  let to_tuple {tokenPool; xtzPool; lqtTotal; tokenAddress; lqtAddress} =
    (tokenPool, xtzPool, lqtTotal, tokenAddress, lqtAddress)

  let valid {tokenPool; xtzPool; lqtTotal; _} =
    tokenPool > Z.zero && lqtTotal > Z.zero && Tez.(xtzPool > Tez.zero)
end

module Parameter = struct
  (* // =============================================================================
   * // Entrypoints
   * // ============================================================================= *)

  type add_liquidity = {
    owner : Contract.t;
    minLqtMinted : Z.t;
    maxTokensDeposited : Z.t;
    deadline : Script_timestamp.t;
  }

  type remove_liquidity = {
    to_ : Contract.t;
    (* recipient of the liquidity redemption *)
    lqtBurned : Z.t;
    (* amount of lqt owned by sender to burn *)
    minXtzWithdrawn : Tez.t;
    (* minimum amount of Tez.t to withdraw *)
    minTokensWithdrawn : Z.t;
    (* minimum amount of tokens to withdraw *)
    deadline : Script_timestamp.t;
        (* the time before which the request must be completed *)
  }

  type token_to_token = {
    outputDexterContract : Contract.t;
    minTokensBought : Z.t;
    to_ : Contract.t;
    tokensSold : Z.t;
    deadline : Script_timestamp.t;
  }

  type token_to_xtz = {
    to_ : Contract.t;
    tokensSold : Z.t;
    minXtzBought : Tez.t;
    deadline : Script_timestamp.t;
  }

  type xtz_to_token = {
    to_ : Contract.t;
    minTokensBought : Z.t;
    deadline : Script_timestamp.t;
  }

  type t =
    | AddLiquidity of add_liquidity
    | Default of unit
    | RemoveLiquidity of remove_liquidity
    | TokenToToken of token_to_token
    | TokenToXtz of token_to_xtz
    | XtzToToken of xtz_to_token

  let addLiquidity p = AddLiquidity p

  let default p = Default p

  let removeLiquidity p = RemoveLiquidity p

  let tokenToToken p = TokenToToken p

  let tokenToXtz p = TokenToXtz p

  let xtzToToken p = XtzToToken p

  let add_liquidity_to_string : add_liquidity -> string =
   fun {owner; minLqtMinted; maxTokensDeposited; deadline} ->
    Format.asprintf
      "{owner : %s; minLqtMinted : %a; maxTokensDeposited : %a; deadline : %s }"
      (Contract.to_b58check owner)
      Z.pp_print
      minLqtMinted
      Z.pp_print
      maxTokensDeposited
      (Script_timestamp.to_string deadline)

  let remove_liquidity_to_string : remove_liquidity -> string =
   fun {to_; lqtBurned; minXtzWithdrawn; minTokensWithdrawn; deadline} ->
    Format.asprintf
      "{owner : %s; lqtBurned : %a; minXtzWithdrawn : %s; minTokensWithdrawn : \
       %a; deadline : %s }"
      (Contract.to_b58check to_)
      Z.pp_print
      lqtBurned
      (Int64.to_string @@ Tez.to_mutez minXtzWithdrawn)
      Z.pp_print
      minTokensWithdrawn
      (Script_timestamp.to_string deadline)

  let token_to_token_to_string : token_to_token -> string =
   fun {outputDexterContract; minTokensBought; to_; tokensSold; deadline} ->
    Format.asprintf
      "{outputDexterContract : %s; minTokensBought : %a; to_ : %s; tokensSold \
       : %a; deadline : %s }"
      (Contract.to_b58check outputDexterContract)
      Z.pp_print
      minTokensBought
      (Contract.to_b58check to_)
      Z.pp_print
      tokensSold
      (Script_timestamp.to_string deadline)

  let token_to_xtz_to_string : token_to_xtz -> string =
   fun {to_; tokensSold; minXtzBought; deadline} ->
    Format.asprintf
      "{to_ : %s; tokensSold : %a; minXtzBought : %s; deadline : %s }"
      (Contract.to_b58check to_)
      Z.pp_print
      tokensSold
      (Int64.to_string @@ Tez.to_mutez minXtzBought)
      (Script_timestamp.to_string deadline)

  let xtz_to_token_to_string : xtz_to_token -> string =
   fun {to_; minTokensBought; deadline} ->
    Format.asprintf
      "{to_ : %s; minTokensBought : %a; deadline : %s }"
      (Contract.to_b58check to_)
      Z.pp_print
      minTokensBought
      (Script_timestamp.to_string deadline)

  let to_string : t -> string = function
    | AddLiquidity p ->
        Format.asprintf "AddLiquidity %s" (add_liquidity_to_string p)
    | Default () -> "Default ()"
    | RemoveLiquidity p ->
        Format.asprintf "RemoveLiquidity %s" (remove_liquidity_to_string p)
    | TokenToToken p ->
        Format.asprintf "TokenToToken (%s)" (token_to_token_to_string p)
    | TokenToXtz p ->
        Format.asprintf "TokenToXtz (%s)" (token_to_xtz_to_string p)
    | XtzToToken p ->
        Format.asprintf "XtzToToken (%s)" (xtz_to_token_to_string p)

  let entrypoint_of_parameter : t -> Entrypoint.t = function
    | AddLiquidity _ -> Entrypoint.of_string_strict_exn "addLiquidity"
    | Default _ -> Entrypoint.default
    | RemoveLiquidity _ -> Entrypoint.of_string_strict_exn "removeLiquidity"
    | TokenToToken _ -> Entrypoint.of_string_strict_exn "tokenToToken"
    | TokenToXtz _ -> Entrypoint.of_string_strict_exn "tokenToXtz"
    | XtzToToken _ -> Entrypoint.of_string_strict_exn "xtzToToken"

  let pp fmt s = Format.fprintf fmt "%s" (to_string s)

  let eq s s' = s = s'

  let to_expr_rooted :
      loc:'a ->
      t ->
      ('a, Michelson_v1_primitives.prim) Tezos_micheline.Micheline.node =
   fun ~loc -> function
    | AddLiquidity {owner; minLqtMinted; maxTokensDeposited; deadline} ->
        comb
          ~loc
          [
            address_string ~loc owner;
            int ~loc minLqtMinted;
            int ~loc maxTokensDeposited;
            timestamp ~loc deadline;
          ]
    | Default () -> unit ~loc ()
    | RemoveLiquidity
        {to_; lqtBurned; minXtzWithdrawn; minTokensWithdrawn; deadline} ->
        comb
          ~loc
          [
            address_string ~loc to_;
            int ~loc lqtBurned;
            mutez ~loc minXtzWithdrawn;
            int ~loc minTokensWithdrawn;
            timestamp ~loc deadline;
          ]
    | TokenToToken
        {outputDexterContract; minTokensBought; to_; tokensSold; deadline} ->
        comb
          ~loc
          [
            address_string ~loc outputDexterContract;
            int ~loc minTokensBought;
            address_string ~loc to_;
            int ~loc tokensSold;
            timestamp ~loc deadline;
          ]
    | TokenToXtz {to_; tokensSold; minXtzBought; deadline} ->
        comb
          ~loc
          [
            address_string ~loc to_;
            int ~loc tokensSold;
            mutez ~loc minXtzBought;
            timestamp ~loc deadline;
          ]
    | XtzToToken {to_; minTokensBought; deadline} ->
        comb
          ~loc
          [
            address_string ~loc to_;
            int ~loc minTokensBought;
            timestamp ~loc deadline;
          ]

  let to_expr :
      loc:'a ->
      t ->
      ('a, Michelson_v1_primitives.prim) Tezos_micheline.Micheline.node =
   fun ~loc p ->
    let rooted = to_expr_rooted ~loc p in
    match p with
    | AddLiquidity _ -> left ~loc @@ left ~loc @@ left ~loc rooted
    | Default () -> left ~loc @@ left ~loc @@ right ~loc rooted
    | RemoveLiquidity _ -> left ~loc @@ right ~loc @@ left ~loc rooted
    | TokenToToken _ -> left ~loc @@ right ~loc @@ right ~loc rooted
    | TokenToXtz _ -> right ~loc @@ left ~loc rooted
    | XtzToToken _ -> right ~loc @@ right ~loc rooted

  let to_michelson_string e =
    let e = to_expr ~loc:0 e in
    Format.asprintf
      "%a"
      Michelson_v1_printer.print_expr
      (Micheline.strip_locations e)
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
