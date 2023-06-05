(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021-2022 Trili Tech, <contact@trili.tech>                  *)
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

let error ~loc v f =
  match v with
  | Error err when List.exists f err -> return_unit
  | Ok _ -> failwith "Unexpected successful result (%s)" loc
  | Error err -> failwith "@[Unexpected error (%s): %a@]" loc pp_print_trace err

let test_error_encodings e =
  let module E = Environment.Error_monad in
  ignore (E.pp Format.str_formatter e) ;
  let e' = E.json_of_error e |> E.error_of_json in
  assert (e = e')

let proto_error ~loc v f =
  error ~loc v (function
      | Environment.Ecoproto_error err ->
          test_error_encodings err ;
          f err
      | _ -> false)

let proto_error_with_info ?(error_info_field = `Title) ~loc v
    expected_error_info =
  let info err =
    let i = Error_monad.find_info_of_error (Environment.wrap_tzerror err) in
    match error_info_field with
    | `Title -> i.title
    | `Id -> i.id
    | `Description -> i.description
    | `Message -> Format.asprintf "%a" Environment.Error_monad.pp err
  in
  proto_error ~loc v (function err ->
      Format.printf
        "@[<v 4>THE ERROR IS: %s@,EXPECTED: %s@]@."
        (info err)
        expected_error_info ;
      let info = info err in
      String.equal info expected_error_info)

let equal ~loc (cmp : 'a -> 'a -> bool) msg pp a b =
  if not (cmp a b) then
    failwith "@[@[[%s]@] - @[%s : %a is not equal to %a@]@]" loc msg pp a pp b
  else return_unit

let leq ~loc (cmp : 'a -> 'a -> int) msg pp a b =
  if cmp a b > 0 then
    failwith
      "@[@[[%s]@] - @[%s : %a is not less or equal to %a@]@]"
      loc
      msg
      pp
      a
      pp
      b
  else return_unit

let lt ~loc (cmp : 'a -> 'a -> int) msg pp a b =
  if cmp a b >= 0 then
    failwith "@[@[[%s]@] - @[%s : %a is not less than %a@]@]" loc msg pp a pp b
  else return_unit

let not_equal ~loc (cmp : 'a -> 'a -> bool) msg pp a b =
  if cmp a b then
    failwith "@[@[[%s]@] - @[%s : %a is equal to %a@]@]" loc msg pp a pp b
  else return_unit

module Int32 = struct
  include Int32

  let pp pp v = Format.pp_print_int pp (Int32.to_int v)
end

module Int64 = struct
  include Int64

  let pp pp v = Format.pp_print_int pp (Int64.to_int v)
end

(* char *)
let equal_char ~loc a b =
  equal ~loc Char.equal "Characters aren't equal" Format.pp_print_char a b

(* int *)
let equal_int ~loc (a : int) (b : int) =
  equal ~loc Int.equal "Integers aren't equal" Format.pp_print_int a b

let not_equal_int ~loc (a : int) (b : int) =
  not_equal ~loc Int.equal "Integers are equal" Format.pp_print_int a b

let leq_int ~loc (a : int) (b : int) =
  leq ~loc Compare.Int.compare "Integer comparison" Format.pp_print_int a b

(* int32 *)
let equal_int32 ~loc (a : int32) (b : int32) =
  equal ~loc Int32.equal "Int32 aren't equal" Int32.pp a b

let leq_int32 ~loc (a : int32) (b : int32) =
  leq ~loc Compare.Int32.compare "Int32 comparison" Int32.pp a b

let lt_int32 ~loc (a : int32) (b : int32) =
  lt ~loc Compare.Int32.compare "Int32 comparison" Int32.pp a b

(* int64 *)
let equal_int64 ~loc (a : int64) (b : int64) =
  equal ~loc Compare.Int64.( = ) "Int64 aren't equal" Int64.pp a b

let not_equal_int64 ~loc (a : int64) (b : int64) =
  not_equal ~loc Int64.equal "Int64 are equal" Int64.pp a b

let leq_int64 ~loc (a : int64) (b : int64) =
  leq ~loc Compare.Int64.compare "Int64 comparison" Int64.pp a b

let lt_int64 ~loc (a : int64) (b : int64) =
  lt ~loc Compare.Int64.compare "Int64 comparison" Int64.pp a b

let equal_z ~loc (a : Z.t) (b : Z.t) =
  equal ~loc Compare.Z.( = ) "Z are not equal" Z.pp_print a b

(* bool *)
let equal_bool ~loc (a : bool) (b : bool) =
  equal ~loc Bool.equal "Booleans aren't equal" Format.pp_print_bool a b

let not_equal_bool ~loc (a : bool) (b : bool) =
  not_equal ~loc Bool.equal "Booleans are equal" Format.pp_print_bool a b

(* string *)
let equal_string ~loc (a : string) (b : string) =
  equal ~loc String.equal "Strings aren't equal" Format.pp_print_string a b

let not_equal_string ~loc (a : string) (b : string) =
  not_equal ~loc String.equal "Strings are equal" Format.pp_print_string a b

(* tez *)
let equal_tez ~loc (a : Alpha_context.Tez.t) (b : Alpha_context.Tez.t) =
  let open Alpha_context in
  equal ~loc Tez.( = ) "Tez aren't equal" Tez.pp a b

let not_equal_tez ~loc (a : Alpha_context.Tez.t) (b : Alpha_context.Tez.t) =
  let open Alpha_context in
  not_equal ~loc Tez.( = ) "Tez are equal" Tez.pp a b

(* pkh *)
let equal_pkh ~loc (a : Signature.Public_key_hash.t)
    (b : Signature.Public_key_hash.t) =
  let module PKH = Signature.Public_key_hash in
  equal ~loc PKH.equal "Public key hashes aren't equal" PKH.pp a b

let not_equal_pkh ~loc (a : Signature.Public_key_hash.t)
    (b : Signature.Public_key_hash.t) =
  let module PKH = Signature.Public_key_hash in
  not_equal ~loc PKH.equal "Public key hashes are equal" PKH.pp a b

(* protocol hash *)
let equal_protocol_hash ~loc (a : Protocol_hash.t) (b : Protocol_hash.t) =
  equal
    ~loc
    Protocol_hash.equal
    "Protocol hashes aren't equal"
    Protocol_hash.pp
    a
    b

let not_equal_protocol_hash ~loc (a : Protocol_hash.t) (b : Protocol_hash.t) =
  not_equal
    ~loc
    Protocol_hash.equal
    "Protocol hashes are equal"
    Protocol_hash.pp
    a
    b

let get_some ~loc = function
  | Some x -> return x
  | None -> failwith "Unexpected None (%s)" loc

let is_none ~loc ~pp = function
  | Some x -> failwith "Unexpected (Some %a) (%s)" pp x loc
  | None -> return_unit

let equal_result ~loc ~pp_ok ~pp_error eq_ok eq_error a b =
  equal
    ~loc
    (Result.equal ~ok:eq_ok ~error:eq_error)
    "Results are not equal"
    (Format.pp_print_result ~ok:pp_ok ~error:pp_error)
    a
    b

let is_error ~loc ~pp = function
  | Ok x -> failwith "Unexpected (Ok %a) (%s)" pp x loc
  | Error _ -> return_unit

let get_ok ~__LOC__ = function
  | Ok r -> return r
  | Error err ->
      failwith "@[Unexpected error (%s): %a@]" __LOC__ pp_print_trace err

open Context

(* Some asserts for account operations *)

let contract_property_is property ~loc b contract expected =
  property b contract >>=? fun balance -> equal_tez ~loc balance expected

(** [balance_is b c amount] checks that the current balance [b] of contract [c]
    is [amount].
*)
let balance_is = contract_property_is Contract.balance

(** [frozen_bonds_is b c amount] checks that the current frozen bonds of
    contract [c] is [amount].
*)
let frozen_bonds_is = contract_property_is Contract.frozen_bonds

let balance_or_frozen_bonds_was_operated ~is_balance ~operand ~loc b contract
    old_balance amount =
  operand old_balance amount |> Environment.wrap_tzresult >>?= fun expected ->
  let f = if is_balance then balance_is else frozen_bonds_is in
  f ~loc b contract expected

(** [balance_was_credited ~loc ctxt contract old_balance amount] checks
    that [contract]'s balance was credited [amount] tez in comparison to
    [old_balance].
*)
let balance_was_credited =
  balance_or_frozen_bonds_was_operated
    ~is_balance:true
    ~operand:Alpha_context.Tez.( +? )

(** [balance_was_credited ~loc ctxt contract old_balance amount] checks
    that [contract]'s balance was debited [amount] tez in comparison to
    [old_balance].
*)
let balance_was_debited =
  balance_or_frozen_bonds_was_operated
    ~is_balance:true
    ~operand:Alpha_context.Tez.( -? )

(** [frozen_bonds_was_credited ~loc ctxt contract old_balance amount] checks
    that [contract]'s frozen bonds was credited [amount] tez in comparison to
    [old_balance].
*)
let frozen_bonds_was_credited =
  balance_or_frozen_bonds_was_operated
    ~is_balance:false
    ~operand:Alpha_context.Tez.( +? )

(** [frozen_bonds_was_credited ~loc ctxt contract old_balance amount] checks
    that [contract]'s frozen bonds was credited [amount] tez in comparison to
    [old_balance].
*)
let frozen_bonds_was_debited =
  balance_or_frozen_bonds_was_operated
    ~is_balance:false
    ~operand:Alpha_context.Tez.( -? )

let pp_print_list pp out xs =
  let list_pp fmt =
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@.") fmt
  in
  Format.fprintf out "[%a]" (list_pp pp) xs

let assert_equal_list ~loc eq msg pp =
  equal ~loc (List.equal eq) msg (pp_print_list pp)

let to_json_string encoding x =
  x
  |> Data_encoding.Json.construct encoding
  |> Format.asprintf "\n%a\n" Data_encoding.Json.pp

let equal_with_encoding ~loc encoding a b =
  equal_string ~loc (to_json_string encoding a) (to_json_string encoding b)

let not_equal_with_encoding ~loc encoding a b =
  not_equal_string ~loc (to_json_string encoding a) (to_json_string encoding b)
