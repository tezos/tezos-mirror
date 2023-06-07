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

(* Testing
   -------
   Component:    Validation components
   Invocation:   dune exec tezt/tests/main.exe -- --file "op_validation.ml"
   Subject:      Checks the validation of operations
*)

(** This test checks that the 1M restriction is enforced, with and
    without the [--disable-mempool-precheck] argument (note: since Lima,
    this argument does nothing). *)
let check_validate_1m_restriction_node =
  Protocol.register_test
    ~__FILE__
    ~supports:(Protocol.From_protocol 14)
    ~title:"Check 1M restriction with and without precheck in the plugin"
    ~tags:["1m"; "manager"; "plugin"; "restriction"]
  @@ fun protocol ->
  let inject_two_manager_operations_and_check_error ~disable_operations_precheck
      =
    Log.info
      "Initialize a client %s operation precheck in the plugin."
      (if disable_operations_precheck then "without" else "with") ;
    let* _node, client =
      Client.init_with_protocol
        ~nodes_args:
          ((if disable_operations_precheck then
            [Node.Disable_operations_precheck]
           else [])
          @ [Synchronisation_threshold 0])
        ~protocol
        `Client
        ()
    in

    Log.info "Inject a first transfer." ;
    let op1 =
      Operation.Manager.make (Operation.Manager.transfer ~amount:1 ())
    in
    let* (`OpHash _s) = Operation.Manager.inject [op1] client in

    let error = Operation.conflict_error in
    Log.info
      "Inject a second transfer with the same manager and check that the \
       injection fails with the following message:\n\
       %s"
      (show_rex error) ;
    let op2 =
      Operation.Manager.make (Operation.Manager.transfer ~amount:2 ())
    in
    let* (`OpHash _) =
      Operation.Manager.inject ~error ~request:`Inject [op2] client
    in
    unit
  in

  let* () =
    inject_two_manager_operations_and_check_error
      ~disable_operations_precheck:false
  in
  inject_two_manager_operations_and_check_error
    ~disable_operations_precheck:true

let register ~protocols = check_validate_1m_restriction_node protocols
