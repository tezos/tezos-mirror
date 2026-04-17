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
  let rec set_fee_and_gas : type kind.
      _ -> _ -> kind contents_list -> kind contents_list t =
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

(** Change the total fee of the packed operation [op] to [fee] (in mutez).
    Also change its source to [source] if the argument is provided.

    Precondition: [op] must be a manager operation. *)
let set_fee_and_source fee ?source op =
  let open Alpha_context in
  let open QCheck2.Gen in
  let rec set_fee_contents_list_gen : type kind.
      int64 -> kind contents_list -> kind contents_list t =
   fun desired_total_fee (* in mutez *) -> function
     | Single (Manager_operation data) ->
         let fee = Tez.of_mutez_exn desired_total_fee in
         let contents =
           match source with
           | Some source -> Manager_operation {data with fee; source}
           | None -> Manager_operation {data with fee}
         in
         return (Single contents)
     | Cons (Manager_operation data, tail) ->
         let* local_fee =
           (* We generate some corner cases where some individual
             operations in the batch have zero fees. *)
           let* r = frequencyl [(7, `Random); (2, `Zero); (1, `All)] in
           match r with
           | `Random ->
               let* n = int_range 0 (Int64.to_int desired_total_fee) in
               return (Int64.of_int n)
           | `Zero -> return 0L
           | `All -> return desired_total_fee
         in
         let fee = Tez.of_mutez_exn local_fee in
         let contents =
           match source with
           | Some source -> Manager_operation {data with fee; source}
           | None -> Manager_operation {data with fee}
         in
         let* tail =
           set_fee_contents_list_gen
             (Int64.sub desired_total_fee local_fee)
             tail
         in
         return (Cons (contents, tail))
     | Single _ -> (* see precondition: manager operation *) assert false
  in
  let {shell = _; protocol_data = Operation_data data} = op in
  let contents = generate1 (set_fee_contents_list_gen fee data.contents) in
  {op with protocol_data = Operation_data {data with contents}}

let set_fee fee op = set_fee_and_source fee op

(** Return an [Operation_hash.t] that is distinct from [different_from]. *)
let different_oph ~different_from =
  if Operation_hash.(different_from = zero) then (
    let new_hash = Operation_hash.hash_string ["1"] in
    assert (Operation_hash.(new_hash <> zero)) ;
    new_hash)
  else Operation_hash.zero

(** List helpers *)

let rec iter_neighbors f = function
  | [] | [_] -> ()
  | x :: (y :: _ as l) ->
      f x y ;
      iter_neighbors f l

let iter2_exn f l1 l2 =
  match List.iter2 ~when_different_lengths:() f l1 l2 with
  | Ok () -> ()
  | Error () ->
      Test.fail
        ~__LOC__
        "Lists have respective lengths %d and %d."
        (List.length l1)
        (List.length l2)
