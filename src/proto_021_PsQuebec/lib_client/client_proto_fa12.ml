(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Protocol_client_context
open Protocol
open Alpha_context
open Tezos_micheline

type error += Contract_has_no_script of Contract_hash.t

type error += Contract_has_no_storage of Contract.t

type error += Entrypoint_mismatch of string * (Script.expr * Script.expr) option

type error += Action_unwrapping_error of string * Script.expr

type error += Not_a_viewable_entrypoint of Entrypoint.t

type error += Not_an_entrypoint of Script.expr

type error += Not_enough_balance of Z.t * Z.t

type error += Not_enough_allowance of Z.t * Z.t

type error += Unsafe_allowance_change of Z.t

type error += Unexpected_error of Script.location * Script.expr

let entrypoint_mismatch_explanation ppf (name, ty) =
  match ty with
  | None -> Format.fprintf ppf "Entrypoint %s is missing" name
  | Some (ty, expected) ->
      Format.fprintf
        ppf
        "Entrypoint \"%s\" has type @[%a@], but should have type @[%a@]"
        name
        Michelson_v1_printer.print_expr
        ty
        Michelson_v1_printer.print_expr
        expected

let () =
  register_error_kind
    `Permanent
    ~id:"fa12ContractHasNoScript"
    ~title:"The given contract is not a smart contract"
    ~description:"An FA1.2 command has referenced a scriptless contract."
    ~pp:(fun ppf contract ->
      Format.fprintf
        ppf
        "Contract %a is not a smart contract, it has no script."
        Contract_hash.pp
        contract)
    Data_encoding.(obj1 (req "contract" Contract.originated_encoding))
    (function Contract_has_no_script c -> Some c | _ -> None)
    (fun c -> Contract_has_no_script c) ;
  register_error_kind
    `Permanent
    ~id:"fa12ContractHasNoStorage"
    ~title:"The given contract has no storage"
    ~description:
      "An FA1.2 command made a call on a contract that has no storage."
    ~pp:(fun ppf contract ->
      Format.fprintf ppf "Contract %a has no storage." Contract.pp contract)
    Data_encoding.(obj1 (req "contract" Contract.encoding))
    (function Contract_has_no_storage c -> Some c | _ -> None)
    (fun c -> Contract_has_no_storage c) ;
  register_error_kind
    `Permanent
    ~id:"entrypointMismatch"
    ~title:"The given contract does not implement the FA1.2 interface"
    ~description:
      "An FA1.2 command has referenced a smart contract whose script does not \
       implement at least one FA1.2 entrypoint, or with an incompatible type. \
       See TZIP-7 \
       (https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-7/tzip-7.md) \
       for documentation on FA1.2."
    ~pp:(fun ppf (name, ty) ->
      Format.fprintf
        ppf
        "Not a supported FA1.2 contract.@\n%a."
        entrypoint_mismatch_explanation
        (name, ty))
    Data_encoding.(
      obj2
        (req "name" string)
        (req "type" (option (tup2 Script.expr_encoding Script.expr_encoding))))
    (function Entrypoint_mismatch (n, t) -> Some (n, t) | _ -> None)
    (fun (n, t) -> Entrypoint_mismatch (n, t)) ;
  register_error_kind
    `Permanent
    ~id:"actionUnwrappingError"
    ~title:"The argument is not for an FA1.2 parameter"
    ~description:
      "The argument's type does not correspond to that of the corresponding \
       FA1.2 entrypoint."
    ~pp:(fun ppf (entrypoint, expr) ->
      Format.fprintf
        ppf
        "Not a supported FA1.2 entrypoint argument.@\nEntrypoint: %s@\n%a."
        entrypoint
        Michelson_v1_printer.print_expr
        expr)
    Data_encoding.(
      obj2 (req "entrypoint" string) (req "expr" Script.expr_encoding))
    (function Action_unwrapping_error (s, e) -> Some (s, e) | _ -> None)
    (fun (s, e) -> Action_unwrapping_error (s, e)) ;
  register_error_kind
    `Permanent
    ~id:"notAViewableEntrypoint"
    ~title:"The entrypoint is not viewable"
    ~description:
      "A transaction made a call on an entrypoint expecting it to implement \
       the 'view' type."
    ~pp:(fun ppf entrypoint ->
      Format.fprintf
        ppf
        "Entrypoint %a is not viewable."
        Entrypoint.pp
        entrypoint)
    Data_encoding.(obj1 (req "entrypoint" Entrypoint.simple_encoding))
    (function Not_a_viewable_entrypoint e -> Some e | _ -> None)
    (fun e -> Not_a_viewable_entrypoint e) ;
  register_error_kind
    `Permanent
    ~id:"notAnEntrypoint"
    ~title:"The expression is not for an entrypoint"
    ~description:
      "The parameter value of the contract call refers to a non-existing \
       entrypoint."
    ~pp:(fun ppf param ->
      Format.fprintf
        ppf
        "Not a parameter for an entrypoint.@\n%a."
        Michelson_v1_printer.print_expr
        param)
    Data_encoding.(obj1 (req "param" Script.expr_encoding))
    (function Not_an_entrypoint e -> Some e | _ -> None)
    (fun e -> Not_an_entrypoint e) ;
  register_error_kind
    `Permanent
    ~id:"notEnoughBalance"
    ~title:"The sender does not have enough balance"
    ~description:
      "An FA1.2 transfer failed because the sender does not have enough \
       balance."
    ~pp:(fun ppf (required, present) ->
      Format.fprintf
        ppf
        "Not enough balance.@\nRequired: %a.@\nPresent: %a."
        Z.pp_print
        required
        Z.pp_print
        present)
    Data_encoding.(obj2 (req "present" n) (req "required" n))
    (function Not_enough_balance (p, r) -> Some (p, r) | _ -> None)
    (fun (p, r) -> Not_enough_balance (p, r)) ;
  register_error_kind
    `Permanent
    ~id:"notEnoughAllowance"
    ~title:"The sender does not have enough allowance"
    ~description:
      "An FA1.2 transfer failed because the receiver does not have enough \
       allowance to ask for a transfer from the sender."
    ~pp:(fun ppf (required, present) ->
      Format.fprintf
        ppf
        "Not enough allowance.@\nRequired: %a.@\nPresent: %a."
        Z.pp_print
        required
        Z.pp_print
        present)
    Data_encoding.(obj2 (req "present" n) (req "required" n))
    (function Not_enough_allowance (p, r) -> Some (p, r) | _ -> None)
    (fun (p, r) -> Not_enough_allowance (p, r)) ;
  register_error_kind
    `Permanent
    ~id:"unsafeAllowanceChange"
    ~title:"The allowance change is unsafe"
    ~description:
      "An FA1.2 non-zero allowance change failed because the current allowance \
       is non-zero. For more explanation on why such allowance change is \
       unsafe, please look at TZIP-7 \
       (https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-7/tzip-7.md#approve)."
    ~pp:(fun ppf previous ->
      Format.fprintf
        ppf
        "Unsafe allowance change@\nPrevious: %a."
        Z.pp_print
        previous)
    Data_encoding.(obj1 (req "previous" n))
    (function Unsafe_allowance_change p -> Some p | _ -> None)
    (fun p -> Unsafe_allowance_change p) ;
  register_error_kind
    `Permanent
    ~id:"fa12UnexpectedError"
    ~title:"Unexpected error during FA1.2 contract interpretation"
    ~description:
      "An unexpected Michelson error was reached during the interpretation of \
       an FA1.2 contract."
    ~pp:(fun ppf (loc, expr) ->
      Format.fprintf
        ppf
        "An unexpected error was reached at location %d with: %a."
        loc
        Michelson_v1_printer.print_expr
        expr)
    Data_encoding.(
      obj2
        (req "location" Script.location_encoding)
        (req "value" Script.expr_encoding))
    (function Unexpected_error (loc, expr) -> Some (loc, expr) | _ -> None)
    (fun (loc, expr) -> Unexpected_error (loc, expr))

let callback_encoding =
  Data_encoding.(
    conv
      (fun (c, e) -> (c, Option.value ~default:"" e))
      (fun (c, e) -> (c, if String.equal e "" then None else Some e))
      (tup2 Contract.encoding Variable.string))

(** Michelson combinators *)

let pair ~loc a b = Micheline.Prim (loc, Script.D_Pair, [a; b], [])

let nat ~loc i = Micheline.Int (loc, i)

let unit ~loc = Micheline.Prim (loc, Script.D_Unit, [], [])

let bytes ~loc b = Micheline.Bytes (loc, b)

let address ~loc addr =
  bytes ~loc (Data_encoding.Binary.to_bytes_exn Contract.encoding addr)

let callback ~loc ?entrypoint addr =
  bytes
    ~loc
    (Data_encoding.Binary.to_bytes_exn callback_encoding (addr, entrypoint))

(** Types *)

(** Michelson type combinators: produce a Michelson node of the
   expected type, and a function to check another node is
   syntactically equivalent. *)
type type_eq_combinator = Script.node * (Script.node -> bool)

(** [t_pair ~loc l] takes a list of types and respective equivalence
   check functions, and returns a type of n-ary pair of such types and
   a function checking syntactical equivalence with another node. *)
let t_pair ~loc l : type_eq_combinator =
  let values, are_ty = List.split l in
  let is_pair p =
    match p with
    | Micheline.Prim (_, Script.T_pair, l, _) -> (
        let res =
          List.for_all2
            ~when_different_lengths:()
            (fun is_ty v -> is_ty v)
            are_ty
            l
        in
        match res with Ok b -> b | Error () -> false)
    | _ -> false
  in
  (Micheline.Prim (loc, Script.T_pair, values, []), is_pair)

(** [t_unit ~loc] returns a Micheline node for the `unit` type, and
   a function checking another node is syntactically equivalent. *)
let t_unit ~loc : type_eq_combinator =
  let is_unit p =
    match p with Micheline.Prim (_, Script.T_unit, [], _) -> true | _ -> false
  in
  (Micheline.Prim (loc, Script.T_unit, [], []), is_unit)

(** [t_nat ~loc] returns a Micheline node for the `nat` type, and
   a function checking another node is syntactically equivalent. *)
let t_nat ~loc : type_eq_combinator =
  let is_nat p =
    match p with Micheline.Prim (_, Script.T_nat, [], _) -> true | _ -> false
  in
  (Micheline.Prim (loc, Script.T_nat, [], []), is_nat)

(** [t_address ~loc] returns a Micheline node for the `address`
   type, and a function checking another node is syntactically
   equivalent. *)
let t_address ~loc : type_eq_combinator =
  let is_address p =
    match p with
    | Micheline.Prim (_, Script.T_address, [], _) -> true
    | _ -> false
  in
  (Micheline.Prim (loc, Script.T_address, [], []), is_address)

(** [t_contract ~loc (c, is_c)] takes a node representing a Michelson
   type and its own syntactical equivalence checker, and returns a
   Micheline node for the type `contract c`, and a function checking
   another node is syntactically equivalent. *)
let t_contract ~loc (a, is_a) : type_eq_combinator =
  let is_contract c =
    match c with
    | Micheline.Prim (_, Script.T_contract, [a], _) -> is_a a
    | _ -> false
  in
  (Micheline.Prim (loc, Script.T_contract, [a], []), is_contract)

(** [t_view ~loc a b] takes two node [a] and [b] and their syntactical
   equivalence checking functions, and returns a Micheline node for
   the `view a b` type, and a function checking another node is
   syntactically equivalent. The view type is defined by
   [TZIP4](https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-4/tzip-4.md).
   *)
let t_view ~loc a b : type_eq_combinator = t_pair ~loc [a; t_contract ~loc b]

(** * Actions *)

(** Corresponds to
   [TZIP7](https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-7/tzip-7.md)
   entrypoints. *)

(** A callback from a view can be on a specific entrypoint of the
   contract, or the default one if not specified. *)
type callback_contract = Contract.t * string option

type action =
  | Transfer of Contract.t * Contract.t * Z.t
  | Approve of Contract.t * Z.t
  | Get_allowance of Contract.t * Contract.t * callback_contract
  | Get_balance of Contract.t * callback_contract
  | Get_total_supply of callback_contract

let print_callback_contract ppf (c, etp) =
  Format.fprintf
    ppf
    "%a%s"
    Contract.pp
    c
    (match etp with None | Some "" -> "" | Some etp -> "%" ^ etp)

let print_action ppf = function
  | Transfer (src, dst, amount) ->
      Format.fprintf
        ppf
        "Transfer (%a, %a, %a)"
        Contract.pp
        src
        Contract.pp
        dst
        Z.pp_print
        amount
  | Approve (addr, amount) ->
      Format.fprintf ppf "Approve (%a, %a)" Contract.pp addr Z.pp_print amount
  | Get_allowance (src, dst, callback) ->
      Format.fprintf
        ppf
        "Get_allowance (%a, %a, %a)"
        Contract.pp
        src
        Contract.pp
        dst
        print_callback_contract
        callback
  | Get_balance (addr, callback) ->
      Format.fprintf
        ppf
        "Get_balance (%a, %a)"
        Contract.pp
        addr
        print_callback_contract
        callback
  | Get_total_supply callback ->
      Format.fprintf
        ppf
        "Get_total_supply (%a)"
        print_callback_contract
        callback

let transfer_encoding =
  Data_encoding.(
    case
      ~title:"transfer"
      (Tag 0)
      (obj3
         (req "transfer_source" Contract.encoding)
         (req "transfer_destination" Contract.encoding)
         (req "transfer_amount" n))
      (function
        | Transfer (src, dst, amount) -> Some (src, dst, amount) | _ -> None)
      (fun (src, dst, amount) -> Transfer (src, dst, amount)))

let approve_encoding =
  Data_encoding.(
    case
      ~title:"approve"
      (Tag 1)
      (obj2 (req "approve_address" Contract.encoding) (req "approve_amount" n))
      (function Approve (addr, amount) -> Some (addr, amount) | _ -> None)
      (fun (addr, amount) -> Approve (addr, amount)))

let getBalance_encoding =
  Data_encoding.(
    case
      ~title:"getBalance"
      (Tag 2)
      (obj2
         (req "getBalance_address" Contract.encoding)
         (req "getBalance_callback" callback_encoding))
      (function
        | Get_balance (addr, callback) -> Some (addr, callback) | _ -> None)
      (fun (addr, callback) -> Get_balance (addr, callback)))

let getAllowance_encoding =
  Data_encoding.(
    case
      ~title:"getAllowance"
      (Tag 3)
      (obj3
         (req "getAllowance_source" Contract.encoding)
         (req "getAllowance_destination" Contract.encoding)
         (req "getAllowance_callback" callback_encoding))
      (function
        | Get_allowance (src, dst, callback) -> Some (src, dst, callback)
        | _ -> None)
      (fun (src, dst, callback) -> Get_allowance (src, dst, callback)))

let getTotalSupply_encoding =
  Data_encoding.(
    case
      ~title:"getTotalSupply"
      (Tag 4)
      (obj1 (req "getTotalSupply_callback" callback_encoding))
      (function Get_total_supply callback -> Some callback | _ -> None)
      (fun callback -> Get_total_supply callback))

let action_encoding =
  Data_encoding.union
    [
      transfer_encoding;
      approve_encoding;
      getBalance_encoding;
      getAllowance_encoding;
      getTotalSupply_encoding;
    ]

let transfer_type ~loc =
  t_pair ~loc [t_address ~loc; t_address ~loc; t_nat ~loc]

let approve_type ~loc = t_pair ~loc [t_address ~loc; t_nat ~loc]

let getAllowance_type ~loc =
  t_view ~loc (t_pair ~loc [t_address ~loc; t_address ~loc]) (t_nat ~loc)

let getBalance_type ~loc = t_view ~loc (t_address ~loc) (t_nat ~loc)

let getTotalSupply_type ~loc = t_view ~loc (t_unit ~loc) (t_nat ~loc)

let standard_entrypoints =
  let loc = -1 in
  [
    ("transfer", transfer_type ~loc);
    ("approve", approve_type ~loc);
    ("getAllowance", getAllowance_type ~loc);
    ("getBalance", getBalance_type ~loc);
    ("getTotalSupply", getTotalSupply_type ~loc);
  ]

let view_input ~loc action =
  match action with
  | Get_allowance (source, destination, _) ->
      pair ~loc (address ~loc source) (address ~loc destination)
  | Get_balance (addr, _) -> address ~loc addr
  | Get_total_supply _ -> unit ~loc
  | _ -> unit ~loc

let action_to_expr ~loc action =
  match action with
  | Transfer (source, destination, amount) ->
      pair
        ~loc
        (address ~loc source)
        (pair ~loc (address ~loc destination) (nat ~loc amount))
  | Approve (addr, amount) -> pair ~loc (address ~loc addr) (nat ~loc amount)
  | Get_allowance (_, _, (cb, entrypoint)) ->
      let input = view_input ~loc action in
      pair ~loc input (callback ~loc ?entrypoint cb)
  | Get_balance (_, (cb, entrypoint)) ->
      let input = view_input ~loc action in
      pair ~loc input (callback ~loc ?entrypoint cb)
  | Get_total_supply (cb, entrypoint) ->
      let input = view_input ~loc action in
      pair ~loc input (callback ~loc ?entrypoint cb)

let parse_address error =
  let open Result_syntax in
  function
  | Micheline.Bytes (_, b) ->
      return @@ Data_encoding.Binary.of_bytes_exn Contract.encoding b
  | String (_, s) -> (
      match Contract.of_b58check s with Ok c -> return c | Error _ -> error ())
  | _ -> error ()

let parse_callback error expr =
  let of_b58_check (c, entrypoint) =
    match Contract.of_b58check c with
    | Ok c -> Ok (c, entrypoint)
    | Error _ -> error ()
  in
  match expr with
  | Micheline.Bytes (_, b) -> (
      match Data_encoding.Binary.of_bytes callback_encoding b with
      | Ok (c, entrypoint) -> Ok (c, entrypoint)
      | Error _ -> error ())
  | String (_, s) -> (
      match String.index_opt s '%' with
      | None -> of_b58_check (s, None)
      | Some pos -> (
          let len = String.length s - pos - 1 in
          let name = String.sub s (pos + 1) len in
          match (String.sub s 0 pos, name) with
          | addr, "default" -> of_b58_check (addr, None)
          | addr, name -> of_b58_check (addr, Some name)))
  | _ -> error ()

let action_of_expr ~entrypoint expr =
  let open Result_syntax in
  let open Micheline in
  let error () =
    tzfail
      (Action_unwrapping_error (entrypoint, Micheline.strip_locations expr))
  in
  match (entrypoint, expr) with
  (* Transfer operation before comb pairs. *)
  | ( "transfer",
      Prim
        ( _,
          Script.D_Pair,
          [
            ((Bytes (_, _) | String (_, _)) as source);
            Prim
              ( _,
                Script.D_Pair,
                [
                  ((Bytes (_, _) | String (_, _)) as destination);
                  Int (_, amount);
                ],
                _ );
          ],
          _ ) )
  (* Transfer operation since Edo comb pairs are now directly interpreted as a
     tuple of 3 elements instead of a pair inside a pair. *)
  | ( "transfer",
      Prim
        ( _,
          Script.D_Pair,
          [
            ((Bytes (_, _) | String (_, _)) as source);
            ((Bytes (_, _) | String (_, _)) as destination);
            Int (_, amount);
          ],
          _ ) ) ->
      let* source = parse_address error source in
      let* destination = parse_address error destination in
      return (Transfer (source, destination, amount))
  | ( "approve",
      Prim
        ( _,
          Script.D_Pair,
          [((Bytes (_, _) | String (_, _)) as addr); Int (_, amount)],
          _ ) ) ->
      let* addr = parse_address error addr in
      return (Approve (addr, amount))
  | ( "getBalance",
      Prim
        ( _,
          Script.D_Pair,
          [
            ((Bytes (_, _) | String (_, _)) as addr);
            ((Bytes (_, _) | String (_, _)) as cb);
          ],
          _ ) ) ->
      let* addr = parse_address error addr in
      let* callback = parse_callback error cb in
      return (Get_balance (addr, callback))
  | ( "getAllowance",
      Prim
        ( _,
          Script.D_Pair,
          [
            Prim
              ( _,
                Script.D_Pair,
                [
                  ((Bytes (_, _) | String (_, _)) as source);
                  ((Bytes (_, _) | String (_, _)) as destination);
                ],
                _ );
            ((Bytes (_, _) | String (_, _)) as contract);
          ],
          _ ) ) ->
      let* source = parse_address error source in
      let* destination = parse_address error destination in
      let* callback = parse_callback error contract in
      return (Get_allowance (source, destination, callback))
  | ( "getTotalSupply",
      Prim
        ( _,
          Script.D_Pair,
          [
            Prim (_, Script.D_Unit, [], _);
            ((Bytes (_, _) | String (_, _)) as contract);
          ],
          _ ) ) ->
      let* callback = parse_callback error contract in
      return (Get_total_supply callback)
  | _ -> error ()

let find_entrypoint_in_annot error annots expr =
  match List.find_opt (fun annot -> annot.[0] = '%') annots with
  | Some entrypoint ->
      action_of_expr
        ~entrypoint:(String.sub entrypoint 1 (String.length entrypoint - 1))
        expr
  | None -> error ()

let derive_action expr t_param =
  let error () =
    Result_syntax.tzfail (Not_an_entrypoint (Micheline.strip_locations expr))
  in
  let rec derive expr t_param =
    match (expr, t_param) with
    | ( Micheline.Prim (_, Script.D_Left, [left], _),
        Micheline.Prim (_, Script.T_or, [t_left; _], _) ) ->
        derive left t_left
    | ( Micheline.Prim (_, Script.D_Right, [right], _),
        Micheline.Prim (_, Script.T_or, [_; t_right], _) ) ->
        derive right t_right
    | _, Micheline.Prim (_, _, _, annots) ->
        find_entrypoint_in_annot error annots expr
    | _ -> error ()
  in
  derive expr t_param

let extract_parameter contract =
  let open Result_syntax in
  function
  | Micheline.Seq (_, l) -> (
      List.filter_map
        (function
          | Micheline.Prim (_, Script.K_parameter, [param], _) -> Some param
          | _ -> None)
        l
      |> function
      | param :: _ -> return param
      | _ -> tzfail (Contract_has_no_script contract))
  | _ -> tzfail (Contract_has_no_script contract)

let get_contract_parameter cctxt ~chain ~block contract =
  let open Lwt_result_syntax in
  let* script_opt =
    Client_proto_context.get_script
      cctxt
      ~chain
      ~block
      contract
      ~unparsing_mode:Optimized
      ~normalize_types:true
  in
  match script_opt with
  | None -> tzfail (Contract_has_no_script contract)
  | Some {code; _} -> (
      match Script_repr.force_decode code with
      | Error _ -> tzfail (Contract_has_no_script contract)
      | Ok code -> Lwt.return (extract_parameter contract (Micheline.root code))
      )

let convert_wrapped_parameter_into_action cctxt ~chain ~block contract param =
  let open Lwt_result_syntax in
  let* parameter = get_contract_parameter cctxt ~chain ~block contract in
  Lwt.return (derive_action param parameter)

let check_entrypoint entrypoints (name, (expected_ty, check)) =
  let open Result_syntax in
  match List.assoc_opt ~equal:String.equal name entrypoints with
  | None -> tzfail (Entrypoint_mismatch (name, None))
  | Some ty ->
      if not (check (Micheline.root ty)) then
        tzfail
          (Entrypoint_mismatch
             (name, Some (ty, Micheline.strip_locations expected_ty)))
      else return_unit

let action_to_entrypoint =
  let transfer = Entrypoint.of_string_strict_exn "transfer" in
  let approve = Entrypoint.of_string_strict_exn "approve" in
  let get_allowance = Entrypoint.of_string_strict_exn "getAllowance" in
  let get_balance = Entrypoint.of_string_strict_exn "getBalance" in
  let get_total_supply = Entrypoint.of_string_strict_exn "getTotalSupply" in
  function
  | Transfer (_, _, _) -> transfer
  | Approve (_, _) -> approve
  | Get_allowance (_, _, _) -> get_allowance
  | Get_balance (_, _) -> get_balance
  | Get_total_supply _ -> get_total_supply

let contract_has_fa12_interface :
    #Protocol_client_context.rpc_context ->
    chain:Shell_services.chain ->
    block:Shell_services.block ->
    contract:Contract_hash.t ->
    unit ->
    unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  fun cctxt ~chain ~block ~contract () ->
    let* entrypoints =
      Michelson_v1_entrypoints.list_contract_entrypoints
        cctxt
        ~chain
        ~block
        ~contract
        ~normalize_types:true
    in
    let*? () =
      List.iter_e (check_entrypoint entrypoints) standard_entrypoints
    in
    return_unit

let translate_action_to_argument action =
  let entrypoint = action_to_entrypoint action in
  let expr =
    Micheline.strip_locations (action_to_expr ~loc:() action)
    |> Script.lazy_expr
  in
  (entrypoint, expr)

let parse_error =
  let open Micheline in
  function
  | ( "NotEnoughBalance",
      Prim (_, Script.D_Pair, [Int (_, required); Int (_, present)], _) ) ->
      Some (Not_enough_balance (required, present))
  | ( "NotEnoughAllowance",
      Prim (_, Script.D_Pair, [Int (_, required); Int (_, present)], _) ) ->
      Some (Not_enough_allowance (required, present))
  | "UnsafeAllowanceChange", Int (_, previous) ->
      Some (Unsafe_allowance_change previous)
  | _ -> None

let extract_error trace =
  let open Micheline in
  TzTrace.fold
    (fun _ error ->
      match error with
      | Environment.Ecoproto_error (Script_interpreter.Reject (loc, param, _))
        -> (
          match root param with
          | Prim (_, Script.D_Pair, [String (_, error); res], _) ->
              parse_error (error, res)
          | _ -> Some (Unexpected_error (loc, param)))
      | _ -> None)
    None
    trace

let call_contract (cctxt : #Protocol_client_context.full) ~chain ~block
    ?confirmations ?dry_run ?verbose_signing ?branch ~source ~src_pk ~src_sk
    ~contract ~action ~tez_amount ?fee ?gas_limit ?safety_guard ?storage_limit
    ?counter ~fee_parameter () =
  let open Lwt_result_syntax in
  let* () = contract_has_fa12_interface cctxt ~chain ~block ~contract () in
  let entrypoint, parameters = translate_action_to_argument action in
  let*! result =
    Client_proto_context.transfer_with_script
      cctxt
      ~chain
      ~block
      ?confirmations
      ?dry_run
      ?branch
      ~source
      ~src_pk
      ~src_sk
      ~destination:(Originated contract)
      ~parameters
      ~amount:tez_amount
      ~entrypoint
      ?fee
      ?gas_limit
      ?safety_guard
      ?storage_limit
      ?counter
      ~fee_parameter
      ?verbose_signing
      ()
  in
  match result with
  | Ok res -> return res
  | Error trace -> (
      match extract_error trace with
      | None -> Lwt.return (Error trace)
      | Some error -> tzfail error)

type token_transfer = {
  token_contract : string;
  destination : string;
  amount : Z.t;
  tez_amount : string option;
  fee : string option;
  gas_limit : Gas.Arith.integral option;
  storage_limit : Z.t option;
}

let token_transfer_encoding =
  let open Data_encoding in
  conv
    (fun {
           token_contract;
           destination;
           amount;
           tez_amount;
           fee;
           gas_limit;
           storage_limit;
         } ->
      ( token_contract,
        destination,
        amount,
        tez_amount,
        fee,
        gas_limit,
        storage_limit ))
    (fun ( token_contract,
           destination,
           amount,
           tez_amount,
           fee,
           gas_limit,
           storage_limit ) ->
      {
        token_contract;
        destination;
        amount;
        tez_amount;
        fee;
        gas_limit;
        storage_limit;
      })
    (obj7
       (req "token_contract" string)
       (req "destination" string)
       (req "amount" z)
       (opt "tez-amount" string)
       (opt "fee" string)
       (opt "gas-limit" Gas.Arith.n_integral_encoding)
       (opt "storage-limit" z))

let tez_of_string_exn index field s =
  match Tez.of_string s with
  | Some t -> Ok t
  | None ->
      error_with
        "Invalid %s notation at entry %i, field \"%s\": %s"
        Operation_result.tez_sym
        index
        field
        s

let tez_of_opt_string_exn index field s =
  Option.map_e (tez_of_string_exn index field) s

let build_transaction_operation ?(tez_amount = Tez.zero) ?fee ?gas_limit
    ?storage_limit token action =
  let entrypoint = action_to_entrypoint action in
  let parameters =
    Script.lazy_expr (Micheline.strip_locations (action_to_expr ~loc:() action))
  in
  let operation =
    Transaction
      {amount = tez_amount; parameters; destination = token; entrypoint}
  in
  Injection.prepare_manager_operation
    ~fee:(Limit.of_option fee)
    ~gas_limit:(Limit.of_option gas_limit)
    ~storage_limit:(Limit.of_option storage_limit)
    operation

let prepare_single_token_transfer cctxt ?default_fee ?default_gas_limit
    ?default_storage_limit ~chain ~block src index transfer =
  let open Lwt_result_syntax in
  let* token =
    Client_proto_contracts.Originated_contract_alias.find_destination
      cctxt
      transfer.token_contract
  in
  let* () =
    contract_has_fa12_interface cctxt ~chain ~block ~contract:token ()
  in
  let* dest =
    Client_proto_contracts.Contract_alias.find_destination
      cctxt
      transfer.destination
  in
  let*? tez_amount =
    tez_of_opt_string_exn index "tez_amount" transfer.tez_amount
  in
  let*? transfer_fee = tez_of_opt_string_exn index "fee" transfer.fee in
  let fee = Option.either transfer_fee default_fee in
  let gas_limit = Option.either transfer.gas_limit default_gas_limit in
  let storage_limit =
    Option.either transfer.storage_limit default_storage_limit
  in
  let action = Transfer (src, dest, transfer.amount) in
  let operation =
    build_transaction_operation
      ?tez_amount
      ?fee
      ?gas_limit
      ?storage_limit
      (Originated token)
      action
  in
  return (Annotated_manager_operation.Annotated_manager_operation operation)

let inject_token_transfer_batch (cctxt : #Protocol_client_context.full) ~chain
    ~block ?confirmations ?dry_run ?verbose_signing ~sender ~source ~src_pk
    ~src_sk ~token_transfers ~fee_parameter ?counter ?default_fee
    ?default_gas_limit ?default_storage_limit ?safety_guard () =
  let open Lwt_result_syntax in
  let* contents =
    List.mapi_ep
      (prepare_single_token_transfer
         cctxt
         ?default_fee
         ?default_gas_limit
         ?default_storage_limit
         ~chain
         ~block
         sender)
      token_transfers
  in
  let (Manager_list contents) =
    Annotated_manager_operation.manager_of_list contents
  in
  let*! result =
    Injection.inject_manager_operation
      cctxt
      ~chain
      ~block
      ?confirmations
      ?dry_run
      ?verbose_signing
      ~source
      ~fee:(Limit.of_option default_fee)
      ~gas_limit:(Limit.of_option default_gas_limit)
      ~storage_limit:(Limit.of_option default_storage_limit)
      ?safety_guard
      ?counter
      ~src_pk
      ~src_sk
      ~fee_parameter
      contents
  in
  match result with
  | Ok _ -> return_unit
  | Error trace -> (
      match extract_error trace with
      | None -> Lwt.return (Error trace)
      | Some error -> tzfail error)

let is_viewable_action action =
  let open Lwt_result_syntax in
  match action with
  | Get_balance (_, _) | Get_allowance (_, _, _) | Get_total_supply _ ->
      return_unit
  | _ -> tzfail (Not_a_viewable_entrypoint (action_to_entrypoint action))

let run_view_action (cctxt : #Protocol_client_context.full) ~chain ~block
    ~sender ~contract ~action ~payer ~gas ~unparsing_mode () =
  let open Lwt_result_syntax in
  let* () = is_viewable_action action in
  let* () = contract_has_fa12_interface cctxt ~chain ~block ~contract () in
  let entrypoint = action_to_entrypoint action in
  let input = Micheline.strip_locations (view_input ~loc:() action) in
  let* chain_id = Chain_services.chain_id cctxt ~chain () in
  Plugin.RPC.Scripts.run_tzip4_view
    cctxt
    (chain, block)
    ~contract
    ~input
    ~chain_id
    ~sender
    ~payer
    ~gas
    ~entrypoint
    ~unparsing_mode
    ~now:None
    ~level:None
    ~other_contracts:None
    ~extra_big_maps:None

let () =
  Data_encoding.(
    Registration.register
    @@ def (Protocol.name ^ ".fa1.2.token_transfer") token_transfer_encoding)
