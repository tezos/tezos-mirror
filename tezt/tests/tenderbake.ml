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

(* Testing
   -------
   Component:    Tenderbake
   Invocation:   dune exec tezt/tests/main.exe -- --file tenderbake.ml
   Subject:      Basic test for Tenderbake and related newly added API components
*)

(* ------------------------------------------------------------------------- *)
(* Typedefs *)

let transfer_data =
  (Constant.bootstrap1.alias, Tez.one, Constant.bootstrap2.alias)

let baker = Constant.bootstrap5.alias

let default_overrides =
  [
    (* ensure that blocks must be endorsed *) (["consensus_threshold"], Some "6");
  ]

let init ?(overrides = default_overrides) protocol =
  let* sandbox_node = Node.init [Synchronisation_threshold 0; Private_mode] in
  let sandbox_endpoint = Client.Node sandbox_node in
  let* sandbox_client = Client.init ~endpoint:sandbox_endpoint () in
  let* parameter_file =
    let base = Either.Right (protocol, None) in
    Protocol.write_parameter_file ~base overrides
  in
  let* () =
    (* activate in the past - let timestamp_delay be the default value of 1 year *)
    Client.activate_protocol ~protocol sandbox_client ~parameter_file
  in
  Log.info "Activated protocol." ;
  return
  @@ ( Tezos_crypto.Protocol_hash.of_b58check_exn (Protocol.hash protocol),
       sandbox_endpoint,
       sandbox_client )

let bootstrap_accounts = List.tl Constant.all_secret_keys

let endorsers =
  [
    Constant.bootstrap1.alias;
    Constant.bootstrap2.alias;
    Constant.bootstrap3.alias;
    Constant.bootstrap4.alias;
    Constant.bootstrap5.alias;
  ]

let endorse endpoint protocol client endorsers =
  Client.endorse_for ~endpoint ~protocol ~key:endorsers ~force:true client

let preendorse endpoint protocol client preendorsers =
  Client.preendorse_for ~endpoint ~protocol ~key:preendorsers ~force:true client

let test_bake_two =
  Protocol.register_test
    ~__FILE__
    ~title:"Tenderbake transfer - baking 2"
    ~tags:["baking"; "tenderbake"]
  @@ fun protocol ->
  let* (_proto_hash, endpoint, client) = init protocol in
  let end_idx = List.length bootstrap_accounts in
  let rec loop i =
    if i = end_idx then Lwt.return_unit
    else
      let baker =
        [
          (List.nth bootstrap_accounts i).alias;
          (List.nth bootstrap_accounts ((i + 3) mod end_idx)).alias;
        ]
      in
      let amount = Tez.of_int (i + 1) in
      let giver = (List.nth bootstrap_accounts ((i + 1) mod end_idx)).alias in
      let receiver =
        (List.nth bootstrap_accounts ((i + 2) mod end_idx)).alias
      in
      Log.info "Phase %d" i ;
      let* () = Client.transfer ~amount ~giver ~receiver client in
      let* () = Client.bake_for ~endpoint ~protocol ~keys:baker client in
      loop (i + 1)
  in
  loop 0

let test_low_level_commands =
  Protocol.register_test
    ~__FILE__
    ~title:"Tenderbake low level commands"
    ~tags:["propose"; "endorse"; "preendorse"; "tenderbake"; "low_level"]
  @@ fun protocol ->
  let* (_proto_hash, endpoint, client) = init protocol in
  Log.info "Doing a propose -> preendorse -> endorse cycle" ;
  let proposer = endorsers in
  let preendorsers = endorsers in
  let* () =
    Client.transfer
      ~amount:(Tez.of_int 3)
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      client
  in
  let* () = Client.propose_for client ~protocol ~endpoint ~key:proposer in
  let* () = preendorse endpoint protocol client preendorsers in
  let* () = endorse endpoint protocol client endorsers in
  let* () =
    Client.transfer
      ~amount:(Tez.of_int 2)
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      client
  in
  let* () = Client.propose_for client ~protocol ~endpoint ~key:proposer in
  let* () = preendorse endpoint protocol client preendorsers in
  let* () = endorse endpoint protocol client endorsers in
  let* () = Client.propose_for client ~protocol ~endpoint ~key:proposer in
  Lwt.return_unit

let test_repropose =
  Protocol.register_test
    ~__FILE__
    ~title:"Tenderbake low level repropose"
    ~tags:
      [
        "propose";
        "endorse";
        "preendorse";
        "tenderbake";
        "low_level";
        "repropose";
      ]
  @@ fun protocol ->
  let* (_proto_hash, endpoint, client) = init protocol in
  Log.info "Doing a propose -> preendorse -> endorse cycle" ;
  let proposer = endorsers in
  let preendorsers = endorsers in
  let* () =
    Client.transfer
      ~amount:(Tez.of_int 3)
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      client
  in
  let* () = Client.propose_for client ~protocol ~endpoint ~key:proposer in
  let* () = preendorse endpoint protocol client preendorsers in
  let* () = endorse endpoint protocol client endorsers in
  let* () =
    Client.transfer
      ~amount:(Tez.of_int 2)
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      client
  in
  let* () = Client.propose_for client ~protocol ~endpoint ~key:proposer in
  let sleeping_time = 5. in
  Log.debug "Waiting %.0fs so that the previous round ends" sleeping_time ;
  let* () = Lwt_unix.sleep sleeping_time in
  let* () = Client.propose_for client ~protocol ~endpoint ~key:proposer in
  Lwt.return_unit

let register ~protocols =
  test_bake_two ~protocols ;
  test_low_level_commands ~protocols ;
  test_repropose ~protocols
