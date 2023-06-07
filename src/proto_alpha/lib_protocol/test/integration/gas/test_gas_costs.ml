(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** Testing
    -------
    Component:  Protocol (gas costs)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/integration/gas/main.exe \
                  -- --file test_gas_costs.ml
    Subject:    Gas costs
                Current limitations: for maps, sets & compare, we only test
                integer comparable keys.
*)

open Protocol
module S = Saturation_repr

let dummy_list = Script_list.(cons 42 empty)

let forty_two = Script_int.of_int 42

let forty_two_n = Script_int.abs forty_two

let dummy_set =
  let open Script_set in
  update forty_two true (empty Script_typed_ir.int_t)

let dummy_map =
  let open Script_map in
  update forty_two (Some forty_two) (empty Script_typed_ir.int_t)

let dummy_timestamp = Script_timestamp.of_zint (Z.of_int 42)

let dummy_pk =
  Signature.Public_key.of_b58check_exn
    "edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU"

let dummy_bytes = Bytes.of_string "dummy"

let dummy_string =
  match Script_string.of_string "dummy" with
  | Ok s -> s
  | Error _ -> assert false

let dummy_ty = Script_typed_ir.never_t

let free = ["balance"; "bool"; "parsing_unit"; "unparsing_unit"]

(* /!\ The compiler will only complain if costs are _removed_ /!\*)
let all_interpreter_costs =
  let open Michelson_v1_gas.Cost_of.Interpreter in
  [
    ("drop", drop);
    ("dup", dup);
    ("swap", swap);
    ("cons_some", cons_some);
    ("cons_none", cons_none);
    ("if_none", if_none);
    ("cons_pair", cons_pair);
    ("car", car);
    ("cdr", cdr);
    ("cons_left", cons_left);
    ("cons_right", cons_right);
    ("if_left", if_left);
    ("cons_list", cons_list);
    ("nil", nil);
    ("if_cons", if_cons);
    ("list_map", list_map dummy_list);
    ("list_size", list_size);
    ("list_iter", list_iter dummy_list);
    ("empty_set", empty_set);
    ("set_iter", set_iter dummy_set);
    ("set_mem", set_mem forty_two dummy_set);
    ("set_update", set_update forty_two dummy_set);
    ("set_size", set_size);
    ("empty_map", empty_map);
    ("map_map", map_map dummy_map);
    ("map_iter", map_iter dummy_map);
    ("map_mem", map_mem forty_two dummy_map);
    ("map_get", map_get forty_two dummy_map);
    ("map_update", map_update forty_two dummy_map);
    ("map_size", map_size);
    ("add_seconds_timestamp", add_seconds_timestamp forty_two dummy_timestamp);
    ("sub_timestamp_seconds", sub_timestamp_seconds dummy_timestamp forty_two);
    ("diff_timestamps", diff_timestamps dummy_timestamp dummy_timestamp);
    ("concat_string_pair", concat_string_pair dummy_string dummy_string);
    ("slice_string", slice_string dummy_string);
    ("string_size", string_size);
    ("concat_bytes_pair", concat_bytes_pair dummy_bytes dummy_bytes);
    ("slice_bytes", slice_bytes dummy_bytes);
    ("bytes_size", bytes_size);
    ("add_tez", add_tez);
    ("sub_tez", sub_tez);
    ("mul_teznat", mul_teznat);
    ("bool_or", bool_or);
    ("bool_and", bool_and);
    ("bool_xor", bool_xor);
    ("bool_not", bool_not);
    ("is_nat", is_nat);
    ("abs_int", abs_int forty_two);
    ("int_nat", int_nat);
    ("neg", neg forty_two);
    ("add_int", add_int forty_two forty_two);
    ("sub_int", sub_int forty_two forty_two);
    ("mul_int", mul_int forty_two forty_two);
    ("ediv_teznat", ediv_teznat Alpha_context.Tez.fifty_cents forty_two);
    ("ediv_tez", ediv_tez);
    ("ediv_int", ediv_int forty_two (Script_int.of_int 1));
    ("eq", eq);
    ("lsl_nat", lsl_nat forty_two);
    ("lsr_nat", lsr_nat forty_two);
    ("or_nat", or_nat forty_two forty_two);
    ("and_nat", and_nat forty_two forty_two);
    ("xor_nat", xor_nat forty_two forty_two);
    ("not_int", not_int forty_two);
    ("if_", if_);
    ("loop", loop);
    ("loop_left", loop_left);
    ("dip", dip);
    ("check_signature", check_signature dummy_pk dummy_bytes);
    ("blake2b", blake2b dummy_bytes);
    ("sha256", sha256 dummy_bytes);
    ("sha512", sha512 dummy_bytes);
    ("dign", dign 42);
    ("dugn", dugn 42);
    ("dipn", dipn 42);
    ("dropn", dropn 42);
    ("neq", neq);
    ("compare", compare Script_typed_ir.int_t forty_two forty_two);
    ( "concat_string_precheck",
      concat_string_precheck Script_list.(cons "42" empty) );
    ("concat_string", concat_string (S.safe_int 42));
    ("concat_bytes", concat_bytes (S.safe_int 42));
    ("exec", exec);
    ("apply_rec", apply ~rec_flag:true);
    ("apply", apply ~rec_flag:false);
    ("lambda", lambda);
    ("address", address);
    ("contract", contract);
    ("transfer_tokens", transfer_tokens);
    ("implicit_account", implicit_account);
    ("create_contract", create_contract);
    ("set_delegate", set_delegate);
    (* balance is free *)
    ("balance", balance);
    ("level", level);
    ("now", now);
    ("hash_key", hash_key dummy_pk);
    ("source", source);
    ("sender", sender);
    ("self", self);
    ("self_address", self_address);
    ("amount", amount);
    ("chain_id", chain_id);
    ("unpack_failed", unpack_failed "dummy");
  ]

(* /!\ The compiler will only complain if costs are _removed_ /!\*)
let all_parsing_costs =
  let open Michelson_v1_gas.Cost_of.Typechecking in
  [
    ("public_key_optimized", public_key_optimized);
    ("public_key_readable", public_key_readable);
    ("key_hash_optimized", key_hash_optimized);
    ("key_hash_readable", key_hash_readable);
    ("signature_optimized", signature_optimized);
    ("signature_readable", signature_readable);
    ("chain_id_optimized", chain_id_optimized);
    ("chain_id_readable", chain_id_readable);
    ("address_optimized", address_optimized);
    ("contract_optimized", contract_optimized);
    ("contract_readable", contract_readable);
    ("check_printable", check_printable "dummy");
    ("merge_cycle", merge_cycle);
    ("parse_type_cycle", parse_type_cycle);
    ("parse_instr_cycle", parse_instr_cycle);
    ("parse_data_cycle", parse_data_cycle);
    ("bool", bool);
    ("parsing_unit", unit);
    ("timestamp_readable", timestamp_readable "dummy");
    ("contract_exists", contract_exists);
    ("proof_argument", proof_argument 42);
  ]

(* /!\ The compiler will only complain if costs are _removed_ /!\*)
let all_unparsing_costs =
  let open Michelson_v1_gas.Cost_of.Unparsing in
  [
    ("public_key_optimized", public_key_optimized);
    ("public_key_readable", public_key_readable);
    ("key_hash_optimized", key_hash_optimized);
    ("key_hash_readable", key_hash_readable);
    ("signature_optimized", signature_optimized);
    ("signature_readable", signature_readable);
    ("chain_id_optimized", chain_id_optimized);
    ("chain_id_readable", chain_id_readable);
    ("timestamp_readable", timestamp_readable);
    ("address_optimized", address_optimized);
    ("contract_optimized", contract_optimized);
    ("contract_readable", contract_readable);
    ("unparse_type", unparse_type dummy_ty);
    ("unparse_instr_cycle", unparse_instr_cycle);
    ("unparse_data_cycle", unparse_data_cycle);
    ("unparsing_unit", unit);
    ("operation", operation dummy_bytes);
  ]

(* /!\ The compiler will only complain if costs are _removed_ /!\*)
let all_io_costs =
  let open Storage_costs in
  [
    ("read_access 0 0", read_access ~path_length:0 ~read_bytes:0);
    ("read_access 1 0", read_access ~path_length:1 ~read_bytes:0);
    ("read_access 0 1", read_access ~path_length:0 ~read_bytes:1);
    ("read_access 1 1", read_access ~path_length:1 ~read_bytes:1);
    ("write_access 0", write_access ~written_bytes:0);
    ("write_access 1", write_access ~written_bytes:1);
  ]

(* Here we're using knowledge of the internal representation of costs to
   cast them to S ... *)
let cast_cost_to_s (c : Alpha_context.Gas.cost) : _ S.t =
  Data_encoding.Binary.to_bytes_exn Alpha_context.Gas.cost_encoding c
  |> Data_encoding.Binary.of_bytes_exn S.n_encoding

(** Checks that all costs are positive values. *)
let test_cost_reprs_are_all_positive list () =
  List.iter_es
    (fun (cost_name, cost) ->
      if S.(cost > S.zero) then return_unit
      else if S.equal cost S.zero && List.mem ~equal:String.equal cost_name free
      then return_unit
      else
        fail
          (Exn
             (Failure (Format.asprintf "Gas cost test \"%s\" failed" cost_name))))
    list

(** Checks that all costs are positive values. *)
let test_costs_are_all_positive list () =
  let list =
    List.map (fun (cost_name, cost) -> (cost_name, cast_cost_to_s cost)) list
  in
  test_cost_reprs_are_all_positive list ()

let tests =
  [
    Tztest.tztest
      "Positivity of interpreter costs"
      `Quick
      (test_costs_are_all_positive all_interpreter_costs);
    Tztest.tztest
      "Positivity of typechecking costs"
      `Quick
      (test_costs_are_all_positive all_parsing_costs);
    Tztest.tztest
      "Positivity of unparsing costs"
      `Quick
      (test_costs_are_all_positive all_unparsing_costs);
    Tztest.tztest
      "Positivity of io costs"
      `Quick
      (test_cost_reprs_are_all_positive all_io_costs);
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("gas cost functions", tests)]
  |> Lwt_main.run
