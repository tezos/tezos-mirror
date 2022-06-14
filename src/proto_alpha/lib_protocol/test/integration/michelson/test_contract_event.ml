(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
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
open Lwt_result_syntax

(** Testing
    -------
    Component:  Protocol (event logging)
    Invocation: dune exec \
                src/proto_alpha/lib_protocol/test/integration/michelson/main.exe \
                -- test '^event logging$'
    Subject:  This module tests that the event logs can be written to the receipt
              in correct order and expected format.
*)

let wrap m = m >|= Environment.wrap_tzresult

(** Parse a Michelson contract from string. *)
let originate_contract file storage src b =
  let load_file f =
    let ic = open_in f in
    let res = really_input_string ic (in_channel_length ic) in
    close_in ic ;
    res
  in
  let contract_string = load_file file in
  let code = Expr.toplevel_from_string contract_string in
  let storage = Expr.from_string storage in
  let script =
    Alpha_context.Script.{code = lazy_expr code; storage = lazy_expr storage}
  in
  let* operation, dst =
    Op.contract_origination (B b) src ~fee:(Test_tez.of_int 10) ~script
  in
  let* incr = Incremental.begin_construction b in
  let* incr = Incremental.add_operation incr operation in
  let+ b = Incremental.finalize_block incr in
  (dst, b)

(** Run emit.tz and assert that both the order of events and data content are correct *)
let contract_test () =
  let* b, src = Context.init1 ~consensus_threshold:0 () in
  let* dst, b = originate_contract "contracts/emit.tz" "Unit" src b in
  let fee = Test_tez.of_int 10 in
  let parameters = Script.unit_parameter in
  let* operation =
    Op.transaction ~fee ~parameters (B b) src dst (Test_tez.of_int 0)
  in
  let* incr = Incremental.begin_construction b in
  let* incr = Incremental.add_operation incr operation in
  match Incremental.rev_tickets incr with
  | [
   Operation_metadata
     {
       contents =
         Single_result
           (Manager_operation_result
             {
               internal_operation_results =
                 [
                   Internal_manager_operation_result
                     ( {
                         operation =
                           Transaction
                             {
                               entrypoint = tag1;
                               parameters = data1;
                               amount = amount1;
                               destination = Destination.Event addr1;
                             };
                         _;
                       },
                       Applied
                         (ITransaction_result (Transaction_to_event_result _))
                     );
                   Internal_manager_operation_result
                     ( {
                         operation =
                           Transaction
                             {
                               entrypoint = tag2;
                               parameters = data2;
                               amount = amount2;
                               destination = Destination.Event addr2;
                             };
                         _;
                       },
                       Applied
                         (ITransaction_result (Transaction_to_event_result _))
                     );
                 ];
               _;
             });
     };
  ] ->
      let ctxt = Gas.set_unlimited (Incremental.alpha_ctxt incr) in
      let* data1, ctxt =
        Lwt.return @@ Environment.wrap_tzresult
        @@ Script.force_decode_in_context
             ~consume_deserialization_gas:When_needed
             ctxt
             data1
      in
      let* data2, _ =
        Lwt.return @@ Environment.wrap_tzresult
        @@ Script.force_decode_in_context
             ~consume_deserialization_gas:When_needed
             ctxt
             data2
      in
      let open Micheline in
      ((match root data1 with
       | Prim (_, D_Right, [String (_, "right")], _) -> ()
       | _ -> assert false) ;

       match root data2 with
       | Prim (_, D_Left, [Int (_, n)], _) -> assert (Z.to_int n = 2)
       | _ -> assert false) ;
      let addr1 = Contract_event.to_b58check addr1 in
      let addr2 = Contract_event.to_b58check addr2 in
      assert (Entrypoint.to_string tag1 = "tag1") ;
      assert (Entrypoint.to_string tag2 = "tag2") ;
      assert (addr1 = "ev12m5E1yW14mc9rsrcdGAWVfDSdmRGuctykrVU55bHZBGv9kmdhW") ;
      assert (addr2 = "ev12m5E1yW14mc9rsrcdGAWVfDSdmRGuctykrVU55bHZBGv9kmdhW") ;
      assert (Tez.(amount1 = zero)) ;
      assert (Tez.(amount2 = zero)) ;
      return_unit
  | _ -> assert false

let tests =
  [
    Tztest.tztest
      "contract emits event with correct data in proper order"
      `Quick
      contract_test;
  ]
