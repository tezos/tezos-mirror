(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Adaptive Issuance
   Invocation:   dune exec tezt/tests/main.exe -- --file adaptive_issuance.ml
   Subject:      Basic test for Adaptive Issuance and related newly added API components
*)

(* ------------------------------------------------------------------------- *)
let blocks_per_cycle = 4

let nonce_revelation_threshold = 2

module Helpers = struct
  let level_type : RPC.level Check.typ =
    Check.convert
      (fun RPC.
             {level; level_position; cycle; cycle_position; expected_commitment} ->
        (level, level_position, cycle, cycle_position, expected_commitment))
      Check.(tuple5 int int int int bool)

  let get_current_level client =
    Client.RPC.call client @@ RPC.get_chain_block_helper_current_level ()

  let check_current_level client expected_level =
    let* level = get_current_level client in
    Check.((level = expected_level) level_type)
      ~error_msg:"expected current_period = %R, got %L" ;
    unit

  let exec_n_cycles f ?keys n client =
    let* current_level = get_current_level client in
    let current_level = current_level.level in
    let nb_baked_blocks_in_cycle = current_level mod blocks_per_cycle in
    let nb_blocks_to_bake = (n * blocks_per_cycle) - nb_baked_blocks_in_cycle in
    let rec loop n =
      if n = 0 then unit
      else
        let* () = f (nb_blocks_to_bake - n) ?keys client in
        loop (n - 1)
    in
    Log.info
      "Bake %d cycle(s) (from level %d to %d)"
      n
      current_level
      (current_level + nb_blocks_to_bake) ;
    loop nb_blocks_to_bake
end

let default_overrides =
  [
    (* Shorter cycles *)
    (["blocks_per_cycle"], `Int blocks_per_cycle);
    (["nonce_revelation_threshold"], `Int nonce_revelation_threshold);
    (* Activate adaptive issuance feature vote *)
    (["adaptive_issuance_activation_vote_enable"], `Bool true);
    (* Make adaptive issuance activation faster *)
    (["adaptive_issuance_launch_ema_threshold"], `Int 1);
  ]

let bootstrap_accounts = Constant.all_secret_keys

let launch_ema_threshold client =
  let* json =
    Client.RPC.call client @@ RPC.get_chain_block_context_constants ()
  in
  Lwt.return
  @@ JSON.(json |-> "adaptive_issuance_launch_ema_threshold" |> as_int)

let init ?(overrides = default_overrides) protocol =
  let* sandbox_node = Node.init [Synchronisation_threshold 0; Private_mode] in
  let sandbox_endpoint = Client.Node sandbox_node in
  let* sandbox_client = Client.init ~endpoint:sandbox_endpoint () in
  let* parameter_file =
    let base = Either.Right (protocol, None) in
    Protocol.write_parameter_file ~base overrides
  in
  let* () = Client.activate_protocol ~protocol sandbox_client ~parameter_file in
  Log.info "Activated protocol." ;
  return
  @@ ( Tezos_crypto.Hashed.Protocol_hash.of_b58check_exn (Protocol.hash protocol),
       sandbox_endpoint,
       sandbox_client,
       sandbox_node )

let activate_ai protocol sandbox_client sandbox_endpoint =
  let* ema_threshold = launch_ema_threshold sandbox_client in
  Log.info "EMA threshold: %d" ema_threshold ;
  (* Feature vote has not passed so AI should not activate and <launch_cycle> should be null *)
  let* launch_cycle =
    Client.RPC.call sandbox_client
    @@ RPC.get_chain_block_context_adaptive_issuance_launch_cycle ()
  in
  assert (JSON.is_null launch_cycle) ;
  (* Make delegate vote for AI activation*)
  let bake _i ?keys client =
    Client.bake_for
      ~endpoint:sandbox_endpoint
      ~protocol
      ?keys
      client
      ~ai_vote:On
  in
  (* The vote should have pass during the first cycle *)
  let* () =
    Helpers.exec_n_cycles
      bake
      1
      ~keys:(List.map (fun x -> x.Account.alias) bootstrap_accounts)
      sandbox_client
  in

  (* Now AI should activate and <launch_cycle> should be set *)
  let* launch_cycle =
    Client.RPC.call sandbox_client
    @@ RPC.get_chain_block_context_adaptive_issuance_launch_cycle ()
  in
  Log.info "AI will activate in cycle %d" (JSON.as_int launch_cycle) ;

  (* Wait for <launch_cycle> to have AI fully activated *)
  Helpers.exec_n_cycles bake (JSON.as_int launch_cycle) sandbox_client

(** This test starts from a protocol with AI feature flag enabled. It
    tests the correct activation of AI features behind the
    feature vote. *)
let test_AI_activation =
  Protocol.register_test
    ~__FILE__
    ~title:"AI Activation - test AI activation after feature vote"
    ~tags:["adaptive_issuance"]
  @@ fun protocol ->
  let* _proto_hash, endpoint, client, _node = init protocol in
  let stake = Client.spawn_stake ~wait:"1" (Tez.of_int 1) "bootstrap2" client in
  let* () =
    Process.check_error
      ~msg:
        (rex
           "Manual staking operations are forbidden because staking is \
            currently automated.")
      stake
  in

  let* _ = activate_ai protocol client endpoint in

  (* Make sure AI is activated by trying to explictly stake with one delegate *)
  let stake = Client.spawn_stake (Tez.of_int 1) "bootstrap2" client in
  let* () =
    Client.bake_for_and_wait
      ~endpoint
      ~protocol
      ~keys:(List.map (fun x -> x.Account.alias) bootstrap_accounts)
      client
      ~ai_vote:On
  in
  let* () = Process.check stake in
  Lwt.return_unit

let register ~protocols = test_AI_activation protocols
