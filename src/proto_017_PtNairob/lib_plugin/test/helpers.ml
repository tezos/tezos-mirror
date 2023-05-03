(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** One-element list containing the Tezt tag of the local protocol,
    e.g. "alpha", "nairobi", "mumbai", etc. *)
let proto_tags = Alcotezt_utils.is_proto_test __FILE__

(** Register a plugin test, with file-specific tags and prefix in title. *)
let register_test ~__FILE__ ~file_title ~file_tags ~title ~additional_tags =
  Test.register
    ~__FILE__
    ~title:(sf "%s/plugin: %s: %s" Protocol.name file_title title)
    ~tags:(proto_tags @ ("plugin" :: (file_tags @ additional_tags)))

(** Generator for a packed operation preceded by its hash. *)
let oph_and_op_gen = QCheck2.Gen.map snd Operation_generator.generate_operation

(** Generator for a packed non-manager operation. *)
let non_manager_operation_gen =
  Operation_generator.generate_non_manager_operation

(** Generator for a packed manager operation. *)
let manager_operation_gen =
  let open QCheck2.Gen in
  let* batch_size = int_range 1 Operation_generator.max_batch_size in
  Operation_generator.generate_manager_operation batch_size

(** Generator for a packed manager operation with the specified
    total fee and gas limit. *)
let manager_op_with_fee_and_gas_gen ~fee_in_mutez ~gas =
  let open Alpha_context in
  let open QCheck2.Gen in
  let rec set_fee_and_gas :
      type kind. _ -> _ -> kind contents_list -> kind contents_list t =
   fun desired_total_fee desired_total_gas -> function
    | Single (Manager_operation data) ->
        let fee = Tez.of_mutez_exn (Int64.of_int desired_total_fee) in
        let gas_limit = Gas.Arith.integral_of_int_exn desired_total_gas in
        return (Single (Manager_operation {data with fee; gas_limit}))
    | Cons (Manager_operation data, tail) ->
        let* local_fee =
          (* We generate some corner cases where some individual
             operations in the batch have zero fees. *)
          let* r = frequencyl [(7, `Random); (2, `Zero); (1, `All)] in
          match r with
          | `Random -> int_range 0 desired_total_fee
          | `Zero -> return 0
          | `All -> return desired_total_fee
        in
        let* local_gas = int_range 0 desired_total_gas in
        let fee = Tez.of_mutez_exn (Int64.of_int local_fee) in
        let gas_limit = Gas.Arith.integral_of_int_exn local_gas in
        let* tail =
          set_fee_and_gas
            (desired_total_fee - local_fee)
            (desired_total_gas - local_gas)
            tail
        in
        return (Cons (Manager_operation {data with fee; gas_limit}, tail))
    | Single _ ->
        (* This function is only called on a manager operation. *) assert false
  in
  (* Generate a random manager operation. *)
  let* batch_size = int_range 1 Operation_generator.max_batch_size in
  let* op = Operation_generator.generate_manager_operation batch_size in
  (* Modify its fee and gas to match the [fee_in_mutez] and [gas] inputs. *)
  let {shell = _; protocol_data = Operation_data protocol_data} = op in
  let* contents = set_fee_and_gas fee_in_mutez gas protocol_data.contents in
  let protocol_data = {protocol_data with contents} in
  let op = {op with protocol_data = Operation_data protocol_data} in
  return (Operation.hash_packed op, op)

(** Generate a packed manager operation with the specified total fee
    and gas limit. *)
let generate_manager_op_with_fee_and_gas ~fee_in_mutez ~gas =
  QCheck2.Gen.generate1 (manager_op_with_fee_and_gas_gen ~fee_in_mutez ~gas)
